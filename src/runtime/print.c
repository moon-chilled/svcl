/* code for low-level debugging/diagnostic output */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

/*
 * FIXME:
 *   Some of the code in here is deeply broken, depending on guessing
 *   already out-of-date values instead of getting them from sbcl.h.
 */

#include <stdio.h>
#include <string.h>

#include "sbcl.h"
#include "print.h"
#include "runtime.h"
#include "code.h"
#include "gc-internal.h"
#include "gc-private.h"
#include "genesis/gc-tables.h"
#include "thread.h"              /* genesis/primitive-objects.h needs this */
#include <errno.h>
#include <stdlib.h>
#include <inttypes.h>
#include <setjmp.h>

struct dyndebug_config dyndebug_config = {
    QSHOW == 2,
};

void
dyndebug_init()
{
#define DYNDEBUG_NFLAGS (sizeof(struct dyndebug_config) / sizeof(int))
#define dyndebug_init1(lowercase, uppercase)                    \
    do {                                                        \
        int *ptr = &dyndebug_config.dyndebug_##lowercase;       \
        ptrs[n] = ptr;                                          \
        names[n] = #lowercase;                                  \
        char *val = getenv("SBCL_DYNDEBUG__" uppercase);        \
        *ptr = val && strlen(val);                              \
        n++;                                                    \
    } while (0)
    int n = 0;
    char *names[DYNDEBUG_NFLAGS];
    int *ptrs[DYNDEBUG_NFLAGS];

    dyndebug_init1(fshow,          "FSHOW");
    dyndebug_init1(gencgc_verbose, "GENCGC_VERBOSE");
    dyndebug_init1(safepoints,     "SAFEPOINTS");
    dyndebug_init1(seh,            "SEH");
    dyndebug_init1(misc,           "MISC");
    dyndebug_init1(pagefaults,     "PAGEFAULTS");
    dyndebug_init1(io,             "IO");
    dyndebug_init1(runtime_link,   "RUNTIME_LINK");

    int n_output_flags = n;
    dyndebug_init1(backtrace_when_lost, "BACKTRACE_WHEN_LOST");
    dyndebug_init1(sleep_when_lost,     "SLEEP_WHEN_LOST");

    if (n != DYNDEBUG_NFLAGS)
        fprintf(stderr, "Bug in dyndebug_init\n");

    char *featurelist = getenv("SBCL_DYNDEBUG");
    if (featurelist) {
        int err = 0;
        featurelist = strdup(featurelist);
        char *ptr = featurelist;
        for (;;) {
            char *token = strtok(ptr, " ");
            if (!token) break;
            int i;
            if (!strcmp(token, "all"))
                for (i = 0; i < n_output_flags; i++)
                    *ptrs[i] = 1;
            else {
                for (i = 0; i < (int)DYNDEBUG_NFLAGS; i++)
                    if (!strcmp(token, names[i])) {
                        *ptrs[i] = 1;
                        break;
                    }
                if (i == DYNDEBUG_NFLAGS) {
                    fprintf(stderr, "No such dyndebug flag: `%s'\n", token);
                    err = 1;
                }
            }
            ptr = 0;
        }
        free(featurelist);
        if (err) {
            fprintf(stderr, "Valid flags are:\n");
            fprintf(stderr, "  all  ;enables all of the following:\n");
            int i;
            for (i = 0; i < (int)DYNDEBUG_NFLAGS; i++) {
                if (i == n_output_flags)
                    fprintf(stderr, "Additional options:\n");
                fprintf(stderr, "  %s\n", names[i]);
            }
        }
    }
#if defined(LISP_FEATURE_GENCGC)
    if (dyndebug_config.dyndebug_gencgc_verbose) {
        gencgc_verbose = 1;
    }
#endif

#undef dyndebug_init1
#undef DYNDEBUG_NFLAGS
}

#include "monitor.h"
#include "vars.h"
#include "os.h"
#ifdef LISP_FEATURE_GENCGC
#include "gencgc-alloc-region.h" /* genesis/thread.h needs this */
#endif
#include "genesis/static-symbols.h"
#include "genesis/primitive-objects.h"
#include "genesis/static-symbols.h"
#include "genesis/tagnames.h"

static int max_lines = 20, cur_lines = 0;
static int max_depth = 5, brief_depth = 2, cur_depth = 0;
static int max_length = 5;
static boolean dont_descend = 0, skip_newline = 0;
static int cur_clock = 0;

static void print_obj(char *prefix, lispobj obj);

#define NEWLINE_OR_RETURN if (continue_p(1)) newline(NULL); else return;

static void indent(int in)
{
    static char *spaces = "                                                                ";

    while (in > 64) {
        fputs(spaces, stdout);
        in -= 64;
    }
    if (in != 0)
        fputs(spaces + 64 - in, stdout);
}

static jmp_buf ldb_print_nlx;
static boolean continue_p(boolean newline)
{
    char buffer[256];

    if (cur_depth >= max_depth || dont_descend)
        return 0;

    if (newline) {
        if (skip_newline)
            skip_newline = 0;
        else
            putchar('\n');

        if (cur_lines >= max_lines) {
            printf("More? [y] ");
            fflush(stdout);

            if (fgets(buffer, sizeof(buffer), stdin)) {
                if (buffer[0] == 'n' || buffer[0] == 'N')
                    longjmp(ldb_print_nlx, 1);
                else
                    cur_lines = 0;
            } else {
                printf("\nUnable to read response, assuming y.\n");
                cur_lines = 0;
            }
        }
    }

    return 1;
}

static void newline(char *label)
{
    cur_lines++;
    if (label != NULL)
        fputs(label, stdout);
    putchar('\t');
    indent(cur_depth * 2);
}


static void print_unknown(lispobj obj)
{
  printf("unknown object: %p", (void *)obj);
}

#ifdef PRIdPTR
# define OBJ_FMTd PRIdPTR
#else
# error "Your inttypes.h is lame"
#endif

static void brief_fixnum(lispobj obj)
{
    /* KLUDGE: Rather than update the tables in print_obj(), we
       declare all fixnum-or-unknown tags to be fixnums and sort it
       out here with a guard clause. */
    if (!fixnump(obj)) return print_unknown(obj);
    printf("%"OBJ_FMTd, fixnum_value(obj));
}

static void print_fixnum(lispobj obj)
{
    /* KLUDGE: Rather than update the tables in print_obj(), we
       declare all fixnum-or-unknown tags to be fixnums and sort it
       out here with a guard clause. */
    if (!fixnump(obj)) return print_unknown(obj);
    printf(": %"OBJ_FMTd, fixnum_value(obj));
}

static void brief_otherimm(lispobj obj)
{
    int type, c;
    char * charname = 0;

    type = header_widetag(obj);
    switch (type) {
        case CHARACTER_WIDETAG:
            c = obj>>8; // no mask. show whatever's there
            printf("#\\");
            switch (c) {
                case '\0': charname = "Nul"; break;
                case '\n': charname = "Newline"; break;
                case '\b': charname = "Backspace"; break;
                case '\177': charname = "Delete"; break;
                default:
                  if (c < 32) printf("^%c", c+64);
                  else printf(c < 128 ? "%c" : "U+%X", c);
            }
            if (charname)
                fputs(charname, stdout);
            break;

        case UNBOUND_MARKER_WIDETAG:
            printf("<unbound marker>");
            break;

        default:
            printf("%s", widetag_names[type >> 2]);
            break;
    }
}

static void print_otherimm(lispobj obj)
{
    printf(", %s", widetag_names[header_widetag(obj) >> 2]);

    switch (header_widetag(obj)) {
    case CHARACTER_WIDETAG:
        printf(": ");
        brief_otherimm(obj);
        break;

    case SAP_WIDETAG:
    case UNBOUND_MARKER_WIDETAG:
        break;

    default:
        printf(": data=%"OBJ_FMTX, (obj>>8));
        break;
    }
}

static void brief_list(lispobj obj)
{
    int space = 0;
    int length = 0;

    if (obj == NIL)
        printf("NIL");
    else {
        putchar('(');
        while (listp(obj)) {
            if (space)
                putchar(' ');
            if (++length >= max_length) {
                printf("...");
                obj = NIL;
                break;
            }
            print_obj("", CONS(obj)->car);
            obj = CONS(obj)->cdr;
            space = 1;
            if (obj == NIL)
                break;
        }
        if (obj != NIL) {
            printf(" . ");
            print_obj("", obj);
        }
        putchar(')');
    }
}

void print_list_car_ptrs(lispobj obj, FILE* f)
{
    char sep = '(';
    int len = 0;
    if (obj == NIL) { fprintf(f, "NIL"); return; }
    do {
        if (++len > 20) { fprintf(f, "...)"); return; }
        fprintf(f, "%c%p", sep, (void*)CONS(obj)->car);
        obj = CONS(obj)->cdr;
        sep = ' ';
    } while (listp(obj) && obj != NIL);
    if (obj != NIL) fprintf(f, " . %p", (void*)obj);
    putc(')', f);
}


static void print_list(lispobj obj)
{
    if (obj == NIL) {
        printf(" (NIL)");
    } else {
        print_obj("car: ", CONS(obj)->car);
        print_obj("cdr: ", CONS(obj)->cdr);
    }
}

// takes native pointer as input
char * simple_base_stringize(struct vector * string)
{
  if (widetag_of(&string->header) == SIMPLE_BASE_STRING_WIDETAG)
      return (char*)string->data;
  int length = vector_len(string);
  char * newstring = malloc(length+1);
  uint32_t * data = (uint32_t*)string->data;
  int i;
  for(i=0;i<length;++i)
      newstring[i] = data[i] < 128 ? data[i] : '?';
  newstring[length] = 0;
  return newstring;
}

static void brief_struct(lispobj obj)
{
    struct instance *instance = INSTANCE(obj);
    extern struct vector * instance_classoid_name(lispobj*);
    struct vector * classoid_name;
    classoid_name = instance_classoid_name((lispobj*)instance);
    lispobj layout = instance_layout((lispobj*)instance);
    if ( classoid_name ) {
        char * namestring = simple_base_stringize(classoid_name);
        printf("#<ptr to %"OBJ_FMTX" %s instance>", layout, namestring);
        if ( namestring != (char*)classoid_name->data )
            free(namestring);
    } else {
        printf("#<ptr to %"OBJ_FMTX" instance>", layout);
    }
}

#include "genesis/layout.h"
#include "genesis/defstruct-description.h"
#include "genesis/defstruct-slot-description.h"
static boolean tagged_slot_p(struct layout *layout, int slot_index)
{
    // Since we're doing this scan, we could return the name
    // and exact raw type.
#ifdef LISP_FEATURE_METASPACE
    struct wrapper *wrapper = (void*)(layout->friend-INSTANCE_POINTER_LOWTAG);
    if (instancep(wrapper->_info)) {
        struct defstruct_description* dd = (void*)(wrapper->_info-INSTANCE_POINTER_LOWTAG);
#else
    if (instancep(layout->_info)) {
        struct defstruct_description* dd = (void*)(layout->_info-INSTANCE_POINTER_LOWTAG);
#endif
        lispobj slots = dd->slots;
        for ( ; slots != NIL ; slots = CONS(slots)->cdr ) {
            struct defstruct_slot_description* dsd =
                (void*)(CONS(slots)->car-INSTANCE_POINTER_LOWTAG);
            if ((fixnum_value(dsd->bits) >> DSD_INDEX_SHIFT) == slot_index)
                return (fixnum_value(dsd->bits) & DSD_RAW_TYPE_MASK) == 0;
        }
    }
    return 0;
}

static void print_struct(lispobj obj)
{
    struct instance *instance = INSTANCE(obj);
    short int i;
    char buffer[16];
    lispobj layout = instance_layout(native_pointer(obj));
    print_obj("type: ", layout);
    for (i=INSTANCE_DATA_START; i<instance_length(instance->header); i++) {
        sprintf(buffer, "slot %d: ", i);
        if (layout && tagged_slot_p(LAYOUT(layout), i)) {
            print_obj(buffer, instance->slots[i]);
        } else {
            newline(NULL);
            printf("\n\t    %s0x%"OBJ_FMTX" [raw]", buffer, instance->slots[i]);
        }
    }
}

void show_lstring(struct vector * string, int quotes, FILE *s)
{
  int ucs4_p = 0;
  int i, len = vector_len(string);

#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
  if (widetag_of(&string->header) == SIMPLE_CHARACTER_STRING_WIDETAG) {
      ucs4_p = 1;
      if (quotes)
          putc('u', s); /* an arbitrary notational convention */
  }
#endif
  if (quotes) putc('"', s);
  for (i=0 ; i<len ; i++) {
      // hopefully the compiler will optimize out the ucs4_p test
      // when the runtime is built without Unicode support
      int ch;
      if (ucs4_p)
          ch = i[(uint32_t*)string->data];
      else
          ch = i[(char*)string->data];
      if (ch >= 32 && ch < 127) {
          if (quotes && (ch == '"' || ch == '\\'))
              putc('\\', s);
          putc(ch, s);
      } else {
          fprintf(s, ch > 0xffff ? "\\U%08X" :
                     ch > 0xff ? "\\u%04X" : "\\x%02X", ch);
      }
  }
  if (quotes) putc('"', s);
}

static void brief_fun_or_otherptr(lispobj obj)
{
    lispobj *ptr, header;
    int type;
    struct symbol *symbol;

    ptr = native_pointer(obj);
    header = *ptr;
    type = header_widetag(header);
    switch (type) {
        case SYMBOL_WIDETAG:
            symbol = (struct symbol *)ptr;
            lispobj package = symbol_package(symbol);
            if (package == NIL)
                printf("#:");
            show_lstring(symbol_name(symbol), 0, stdout);
            break;

        case SIMPLE_BASE_STRING_WIDETAG:
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
        case SIMPLE_CHARACTER_STRING_WIDETAG:
#endif
            show_lstring((struct vector*)ptr, 1, stdout);
            break;

        default:
            printf("#<ptr to ");
            brief_otherimm(header);
            if (type == FDEFN_WIDETAG) {  // Try to print name, if a symbol
                // FIXME: more address validity checks perhaps?
                lispobj name = ((struct fdefn*)ptr)->name;
                if (lowtag_of(name) == OTHER_POINTER_LOWTAG
                    && widetag_of(native_pointer(name)) == SYMBOL_WIDETAG) {
                  printf(" for ");
                  struct vector* str = symbol_name(SYMBOL(name));
                  safely_show_lstring(str, 0, stdout);
                }
            }
            putchar('>');
    }
}

static void print_slots(char **slots, int count, lispobj *ptr)
{
    while (count-- > 0) {
        if (*slots) {
            // kludge for encoded slots
            lispobj word = *ptr;
            char* slot_name = *slots;
            if (N_WORD_BYTES == 8 && !strcmp(slot_name, "boxed_size: ")) word = word & 0xFFFFFFFF;
#ifdef LISP_FEATURE_COMPACT_SYMBOL
            else if (!strcmp(slot_name, "name: ")) word = decode_symbol_name(word);
#endif
            print_obj(slot_name, word);
            slots++;
        } else {
            print_obj("???: ", *ptr);
        }
        ptr++;
    }
}

lispobj symbol_function(struct symbol* symbol)
{
    if (symbol->fdefn) return FDEFN(symbol->fdefn)->fun;
    return NIL;
}

static void print_fun_or_otherptr(lispobj obj)
{
    lispobj *ptr;
    unsigned long header;
    int count, type, index;
    char buffer[16];

    ptr = native_pointer(obj);
    if (ptr == NULL) {
        printf(" (NULL Pointer)");
        return;
    }

    header = *ptr++;
    count = HeaderValue(header);
    type = header_widetag(header);

    print_obj("header: ", header);
    if (!other_immediate_lowtag_p(header)) {
        NEWLINE_OR_RETURN;
        printf("(invalid header object)");
        return;
    }

    switch (type) {
    case BIGNUM_WIDETAG:
        count &= 0x7fffff;
        ptr += count;
        NEWLINE_OR_RETURN;
        printf("0x");
        while (count-- > 0)
            printf(
#if N_WORD_BITS == 32
                   "%08lx%s",
#else
                   "%016lx%s",
#endif
                   (unsigned long) *--ptr, (count?"_":""));
        break;

    case RATIO_WIDETAG:
        print_slots(ratio_slots, count, ptr);
        break;

    case COMPLEX_WIDETAG:
        print_slots(complex_slots, count, ptr);
        break;

    case SYMBOL_WIDETAG:
        // Only 1 byte of a symbol header conveys its size.
        // The other bytes may be freely used by the backend.
        print_slots(symbol_slots, count & 0xFF, ptr);
        struct symbol* sym = (void*)(ptr - 1);
        if (symbol_function(sym) != NIL) print_obj("fun: ", symbol_function(sym));
#ifdef LISP_FEATURE_SB_THREAD
        int tlsindex = tls_index_of(sym);
        struct thread*th = get_sb_vm_thread();
        if (th != 0 && tlsindex != 0) {
            lispobj v = *(lispobj*)(tlsindex + (char*)th);
            print_obj("tlsval: ", v);
        }
#endif
#ifdef LISP_FEATURE_COMPACT_SYMBOL
        // print_obj doesn't understand raw words, so make it a fixnum
        int pkgid = symbol_package_id(sym) << N_FIXNUM_TAG_BITS;
        print_obj("package_id: ", pkgid);
#endif
        break;

#if N_WORD_BITS == 32
    case SINGLE_FLOAT_WIDETAG:
        NEWLINE_OR_RETURN;
        printf("%g", ((struct single_float *)native_pointer(obj))->value);
        break;
#endif
    case DOUBLE_FLOAT_WIDETAG:
        NEWLINE_OR_RETURN;
        printf("%g", ((struct double_float *)native_pointer(obj))->value);
        break;

#ifdef LONG_FLOAT_WIDETAG
    case LONG_FLOAT_WIDETAG:
        NEWLINE_OR_RETURN;
        printf("%Lg", ((struct long_float *)native_pointer(obj))->value);
        break;
#endif

#ifdef COMPLEX_SINGLE_FLOAT_WIDETAG
    case COMPLEX_SINGLE_FLOAT_WIDETAG:
        NEWLINE_OR_RETURN;
#ifdef LISP_FEATURE_64_BIT
        printf("%g", ((struct complex_single_float *)native_pointer(obj))->data.data[0]);
#else
        printf("%g", ((struct complex_single_float *)native_pointer(obj))->real);
#endif
        NEWLINE_OR_RETURN;
#ifdef LISP_FEATURE_64_BIT
        printf("%g", ((struct complex_single_float *)native_pointer(obj))->data.data[1]);
#else
        printf("%g", ((struct complex_single_float *)native_pointer(obj))->imag);
#endif
        break;
#endif

#ifdef COMPLEX_DOUBLE_FLOAT_WIDETAG
    case COMPLEX_DOUBLE_FLOAT_WIDETAG:
        NEWLINE_OR_RETURN;
        printf("%g", ((struct complex_double_float *)native_pointer(obj))->real);
        NEWLINE_OR_RETURN;
        printf("%g", ((struct complex_double_float *)native_pointer(obj))->imag);
        break;
#endif

#ifdef COMPLEX_LONG_FLOAT_WIDETAG
    case COMPLEX_LONG_FLOAT_WIDETAG:
        NEWLINE_OR_RETURN;
        printf("%Lg", ((struct complex_long_float *)native_pointer(obj))->real);
        NEWLINE_OR_RETURN;
        printf("%Lg", ((struct complex_long_float *)native_pointer(obj))->imag);
        break;
#endif

    case SIMPLE_BASE_STRING_WIDETAG:
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
    case SIMPLE_CHARACTER_STRING_WIDETAG:
#endif
        NEWLINE_OR_RETURN;
        show_lstring((struct vector*)native_pointer(obj), 1, stdout);
        break;

    case SIMPLE_VECTOR_WIDETAG:
        NEWLINE_OR_RETURN;
        {
        long length = vector_len(VECTOR(obj));
        printf("length = %ld", length);
        ptr++;
        index = 0;
        while (length-- > 0) {
            sprintf(buffer, "%d: ", index++);
            print_obj(buffer, *ptr++);
        }
        }
        break;

    case SIMPLE_BIT_VECTOR_WIDETAG:
        NEWLINE_OR_RETURN;
        {
        long length = vector_len(VECTOR(obj));
        printf("length = %ld : ", length);
        int bits_to_print = (length < N_WORD_BITS) ? length : N_WORD_BITS;
        uword_t word = ptr[1];
        int i;
        for(i=0; i<bits_to_print; ++i) {
            putchar((word & 1) ? '1' : '0');
            if ((i%8)==7) putchar('_');
            word >>= 1;
        }
        if(bits_to_print < length) printf("...");
        printf("\n");
        }
        break;

    case CODE_HEADER_WIDETAG:
        // ptr was already bumped up
        count = code_header_words((struct code*)(ptr-1));
        for_each_simple_fun(fun_index, fun, (struct code*)(ptr-1), 0, {
            sprintf(buffer, "f[%d]: ", fun_index);
            print_obj(buffer, make_lispobj(fun,FUN_POINTER_LOWTAG));
        });
        print_slots(code_slots, count-1, ptr);
        break;

    case SIMPLE_FUN_WIDETAG:
        print_obj("code: ", fun_code_tagged(ptr-1));
        print_slots(simple_fun_slots,
                    sizeof simple_fun_slots/sizeof(char*)-1, ptr);
        break;

#ifdef RETURN_PC_WIDETAG
    case RETURN_PC_WIDETAG:
        print_obj("code: ", obj - (count * 4));
        break;
#endif

    case CLOSURE_WIDETAG:
        print_slots(closure_slots,
                    count & SHORT_HEADER_MAX_WORDS, ptr);
        break;

    case FUNCALLABLE_INSTANCE_WIDETAG:
        print_slots(funcallable_instance_slots,
                    count & SHORT_HEADER_MAX_WORDS, ptr);
        break;

    case VALUE_CELL_WIDETAG:
        print_slots(value_cell_slots, 1, ptr);
        break;

    case SAP_WIDETAG:
        NEWLINE_OR_RETURN;
        printf("%p", (void*)*ptr);
        break;

    case WEAK_POINTER_WIDETAG:
        print_slots(weak_pointer_slots, 1, ptr);
        break;

    case CHARACTER_WIDETAG:
    case UNBOUND_MARKER_WIDETAG:
        NEWLINE_OR_RETURN;
        printf("pointer to an immediate?");
        break;

    case FDEFN_WIDETAG:
        print_slots(fdefn_slots, 2, ptr);
        print_obj("entry: ", decode_fdefn_rawfun((struct fdefn*)(ptr-1)));
        break;

    // Make some vectors printable from C, for when all hell breaks lose
    case SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG:
        NEWLINE_OR_RETURN;
        {
        long length = vector_len(VECTOR(obj));
        uint32_t * data = (uint32_t*)(ptr + 1);
        long i;
        printf("#(");
        for (i=0; i<length; ++i) {
            printf("%s%d", i>0?" ":"", data[i]);
            if(i==255 && length>256) { printf(" ..."); break; }
        }
        printf(")");
        }
        break;
    default:
        NEWLINE_OR_RETURN;
        if (specialized_vector_widetag_p(type))
            printf("length = %"OBJ_FMTd, vector_len(VECTOR(obj)));
        else
            printf("Unknown header object?");
        break;
    }
}

static void print_obj(char *prefix, lispobj obj)
{
#include "genesis/print.inc"
    int type = lowtag_of(obj);
    struct var *var = lookup_by_obj(obj);
    char buffer[256];
    boolean verbose = cur_depth < brief_depth;

    if (!continue_p(verbose))
        return;

    if (var != NULL && var_clock(var) == cur_clock)
        dont_descend = 1;

    if (var == NULL && is_lisp_pointer(obj))
        var = define_var(NULL, obj, 0);

    if (var != NULL)
        var_setclock(var, cur_clock);

    void (**fns)(lispobj) = NULL;
    cur_depth++;
    if (verbose) {
        if (var != NULL) {
            sprintf(buffer, "$%s=", var_name(var));
            newline(buffer);
        }
        else
            newline(NULL);
        printf("%s0x%08lx: ", prefix, (unsigned long) obj);
        if (cur_depth < brief_depth) {
            fputs(lowtag_names[type], stdout);
            fns = print_fns;
        }
        else
            fns = brief_fns;
    }
    else {
        if (dont_descend)
            printf("$%s", var_name(var));
        else {
            if (var != NULL)
                printf("$%s=", var_name(var));
            fns = brief_fns;
        }
    }
    if (!fns)
        ;
    else if (is_lisp_pointer(obj) && !gc_managed_addr_p(obj))
        printf("(bad-address)");
    else
        (*fns[type])(obj);
    cur_depth--;
    dont_descend = 0;
}

void reset_printer()
{
    cur_clock++;
    cur_lines = 0;
    dont_descend = 0;
}

void print(lispobj obj)
{
    skip_newline = 1;
    cur_depth = 0;
    max_depth = 5;
    max_lines = 20;

    if (!setjmp(ldb_print_nlx))
        print_obj("", obj);

    putchar('\n');
}

void brief_print(lispobj obj)
{
    skip_newline = 1;
    cur_depth = 0;
    max_depth = 1;
    max_lines = 5000;
    cur_lines = 0;

    print_obj("", obj);
    putchar('\n');
}

// The following accessors, which take a valid native pointer as input
// and return a Lisp string, are designed to be foolproof during GC,
// hence all the forwarding checks.

#include "forwarding-ptr.h"
struct vector * symbol_name(struct symbol* sym)
{
  if (forwarding_pointer_p((lispobj*)sym))
    sym = (void*)native_pointer(forwarding_pointer_value((lispobj*)sym));
  lispobj name = sym->name;
  if (lowtag_of(name) != OTHER_POINTER_LOWTAG) return NULL;
  lispobj string = decode_symbol_name(name);
  return VECTOR(follow_maybe_fp(string));
}
struct vector * classoid_name(lispobj * classoid)
{
  if (forwarding_pointer_p(classoid))
      classoid = native_pointer(forwarding_pointer_value(classoid));
  // Classoids are named by symbols even though a CLASS name is arbitrary (theoretically)
  lispobj sym = classoid[CLASSOID_NAME_WORDINDEX];
  return lowtag_of(sym) != OTHER_POINTER_LOWTAG ? NULL : symbol_name(SYMBOL(sym));
}
struct vector * layout_classoid_name(lispobj * layout)
{
#ifdef LISP_FEATURE_METASPACE
  // layout can't be forwarded, but wrapper could be
  lispobj* wrapper = native_pointer(((struct layout*)layout)->friend);
  if (forwarding_pointer_p(wrapper))
      wrapper = native_pointer(forwarding_pointer_value(wrapper));
  lispobj classoid = ((struct wrapper*)wrapper)->classoid;
#else
  if (forwarding_pointer_p(layout))
      layout = native_pointer(forwarding_pointer_value(layout));
  lispobj classoid = ((struct layout*)layout)->classoid;
#endif
  return instancep(classoid) ? classoid_name(native_pointer(classoid)) : NULL;
}
struct vector * instance_classoid_name(lispobj * instance)
{
  if (forwarding_pointer_p(instance))
      instance = native_pointer(forwarding_pointer_value(instance));
  lispobj layout = instance_layout(instance);
  return instancep(layout) ? layout_classoid_name(native_pointer(layout)) : NULL;
}
void safely_show_lstring(struct vector * string, int quotes, FILE *s)
{
  extern void show_lstring(struct vector*, int, FILE*);
  if (forwarding_pointer_p((lispobj*)string))
      string = (struct vector*)forwarding_pointer_value((lispobj*)string);
  if (
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
      header_widetag(string->header) == SIMPLE_CHARACTER_STRING_WIDETAG ||
#endif
      header_widetag(string->header) == SIMPLE_BASE_STRING_WIDETAG)
    show_lstring(string, quotes, s);
  else {
    fprintf(s, "#<[widetag=%02X]>", header_widetag(string->header));
  }
}
