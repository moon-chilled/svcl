;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

;;; The list below contains the name of every symbol whose function might be
;;; called from a non-compiled format string. This is important only for SBCL
;;; as host lisp built prior to git revision 6d743d78d9ba840b.
;;; For such hosts, after giving all host packages a new name, we must ASAP
;;; reestablish the function bindings of these "missing" symbols.
;;;
;;; * PRINT-SYMBOL-WITH-PREFIX was added in rev ff218a24fbd70b2e (2007-04-29)
;;;   and later exported from SB-EXT with no explanation.
;;;   It was occasionally invoked via SB-IMPL: which it no longer is.
;;;
;;; * SB-IMPL:PRINT-TYPE and SB-IMPL:PRINT-TYPE-SPECIFIER were first added
;;;   in rev c1b03a36ec4439c8 (2016-01-09). Newer code always uses them
;;;   from SB-IMPL but older code used them from SB-EXT as well.
;;;
;;; The other 3 functions mentioned below have been stable
;;; in that they are always used from SB-IMPL.
;;;
;;; This list must contain names of functions used by *ANY* past revision
;;; even if such function does not appear in current code.  The list does
;;; not need additions to it for future changes though. i.e. this is final.
;;;
(defparameter *host-format-function-names*
  '("PRINT-SYMBOL-WITH-PREFIX"
    "PRINT-TYPE"
    "PRINT-TYPE-SPECIFIER"
    "FORMAT-MILLISECONDS"
    "FORMAT-MICROSECONDS"
    "PRINT-DEPRECATION-REPLACEMENTS"))
(defparameter *host-format-functions* nil)

;;; Rename all host packages, but unhide the host's format functions
;;; for hosts that do not support renaming of internal packages.
;;; Consider all situations in which functions are called via ~// directives:
;;; - The host calls ~/PRINT-TYPE/ for itself (i.e in its compiler) while
;;;   running make-host-1 because "our" code (being compiled) is wrong.
;;;   Barring a host bug (such as was fixed in rev e6fd2a9635e4),
;;;   the format string referencing this function would spell the name
;;;   using a "dash" package. We restore the format functions into the
;;;   expected package below.
;;; - The cross-compiler wants to call a ~// function while executing,
;;;   maybe even from a macro. The format control string should spell the
;;;   function as a "bang" package. If it didn't, the string is wrong.
;;; - The target wants to call a ~// function.
;;;   That's conceptually the easiest to deal with - we've already ensured
;;;   that cross-compiled format strings dump their symbols correctly.
;;;
#+sbcl
(defun hide-host-packages ()
  ;; Rename
  (sb-ext:without-package-locks
   (dolist (pkg (list-all-packages))
     (let ((name (package-name pkg)))
       (unless (or (member name '("KEYWORD" "COMMON-LISP"  "COMMON-LISP-USER"
                                  "XC-STRICT-CL" "SB-COLD" "SB-XC")
                           :test #'string=)
                   #+swank
                   (search "SWANK" name))
         ;; This also removes nicknames SEQUENCE and SB-C-CALL.
         (rename-package pkg (concatenate 'string "HOST-" name)))))))

#+sbcl
(defun unhide-host-format-funs ()
  ;; Restore operation
  (unless (find-symbol "FMT-CONTROL" "HOST-SB-FORMAT")
    (format t "~&; Restoring format control functions~%")
    ;; Copy the definitions from SB-EXT and SB-IMPL as needed.
    (dolist (symbol-name *host-format-function-names*)
      (dolist (package-name '("SB-EXT" "SB-IMPL"))
        (multiple-value-bind (host-symbol access)
            (find-symbol symbol-name (concatenate 'string "HOST-" package-name))
          (when (and access (fboundp host-symbol))
            (let ((original-fun (fdefinition host-symbol)))
              (push (list* host-symbol original-fun package-name)
                    *host-format-functions*)
              (setf (fdefinition (intern symbol-name package-name))
                    original-fun)))))))

  nil)

#-sbcl
(progn (defun hide-host-packages ())
       (defun unhide-host-format-funs ()))

(compile 'hide-host-packages)
(compile 'unhide-host-format-funs)

;;; Macro invoked from 'src/code/early-extensions' to avoid clobbering
;;; host functions that are potentially called from format strings.
(export 'preserving-host-function)
(defmacro preserving-host-function (defun)
  (let ((name (second defun)))
    `(progn
       ;; Assume that this function got a definition from set-up-cold-packages
       ;; and prevent a possible redefinition warning.
       (fmakunbound ',name)
       ,defun
       (restore-host-function ',(string name)))))

(defun restore-host-function (name)
  (declare (ignorable name))
  #+sbcl
  (let ((host-fun (second (assoc name *host-format-functions* :test #'string=))))
    (when host-fun
      (if (string= name "PRINT-TYPE")
          (let* ((our-fun (fdefinition (intern name "SB-IMPL")))
                 (combined
                  (lambda (stream object &rest rest)
                    (let ((ours (typep object (find-symbol "CTYPE" "SB-KERNEL"))))
                      (apply (if ours our-fun host-fun) stream object rest)))))
            (dolist (entry *host-format-functions*)
              (when (string= (car entry) 'print-type)
                (let ((symbol (intern name (cddr entry))))
                  ;; The parallelized build performs this twice: once from
                  ;; interpreted load, again from compilation.
                  ;; So don't wrap more than once.
                  (unless (sb-impl::closurep (fdefinition symbol))
                    (setf (fdefinition symbol) combined))))))
          (dolist (entry *host-format-functions*)
            (when (string= (car entry) name)
              (setf (fdefinition (intern name (cddr entry))) host-fun)))))))

;;; an entry in the table which describes the non-standard part (i.e. not
;;; CL/CL-USER/KEYWORD) of the package structure of the SBCL system
;;;
;;; We make no attempt to be fully general; our table doesn't need to be
;;; able to express features which we don't happen to use.
(export '(genesis))
(defstruct package-data
  ;; a string designator for the package name
  (name (error "missing PACKAGE-DATA-NAME datum"))
  ;; a doc string
  (documentation (error "missing PACKAGE-DOCUMENATION datum"))
  ;; a list of nicknames
  nicknames
  ;; a list of string designators for shadowing symbols
  shadow
  ;; a list of string designators for exported symbols which'll be set
  ;; up at package creation time.
  export
  ;; a list of sublists describing imports. Each sublist has the format as an
  ;; IMPORT-FROM list in DEFPACKAGE: the first element is the name of the
  ;; package to import from, and the remaining elements are the names of
  ;; symbols to import.
  import-from
  ;; a list of string designators for package names of other packages
  ;; which this package uses
  use)

;;; A symbol in the "shadows" list ALWAYS refers to the symbol
;;; in SB-XC when unqualified. Each symbol uncrosses to itself.
;;; I'm taking the stance that since we don't seem to have any calls to
;;; PLUSP or MINUSP involving target floatnums, we don't need to provide
;;; alternate symbols.
(defparameter *shadows*
  '("FLOAT" "SHORT-FLOAT" "SINGLE-FLOAT" "DOUBLE-FLOAT" "LONG-FLOAT"
    "REAL" "COMPLEX" "NUMBER"
    ;; "RATIONAL" is here for the same reason are the preceding:
    ;; we don't want to mess up all tests of the form (IF (EQ X 'RATIONAL) ...)
    ;; or worry about the package of the symbol we're testing (since identity matters).
    ;; But we also need to logically shadow #'RATIONAL which would not be legal
    ;; if it refers to the standard CL:RATIONAL symbol,
    ;; but we need not to harm the standard type specifier '(RATIONAL).
    "RATIONAL"
    ;; Since we're shadowing the types, it also makes sense to shadow
    ;; the predicates lest it be confusing to have to write SB-XC:
    ;; in front of them but not in front of the type.
    ;; RATIONALP isn't here because its behavior is unchanged.
    "FLOATP" "REALP" "COMPLEXP" "NUMBERP"
    "COERCE" "EXP" "EXPT" "LOG" "SIGNUM" "IMAGPART" "REALPART"
    "ZEROP" "ABS" "SIGNUM" "FLOAT-SIGN"
    "CEILING" "FLOOR" "ROUND" "TRUNCATE" "MOD" "REM"
    ;; We always want irrational functions to use target floats.
    "ACOS" "ACOSH" "ASIN" "ASINH" "ATAN" "ATANH"  "CIS" "CONJUGATE"
    "COS" "COSH"  "FCEILING" "FFLOOR" "FROUND" "FTRUNCATE"
    "PHASE" "RATIONALIZE" "SIN" "SINH" "SQRT" "TAN" "TANH"
    ;;
    "SXHASH" ; must package-qualify if you mean CL:SXHASH
    ;;
    "BYTE" "BYTE-POSITION" "BYTE-SIZE"
    "DPB" "LDB" "LDB-TEST"
    "DEPOSIT-FIELD" "MASK-FIELD"
    ;;
    ;; the constants (except for T and NIL which have
    ;; a specially hacked correspondence between
    ;; cross-compilation host Lisp and target Lisp)

    ;; We include these here since there is no reason to reference
    ;; host constants.
    "ARRAY-DIMENSION-LIMIT"
    "ARRAY-RANK-LIMIT"
    "ARRAY-TOTAL-SIZE-LIMIT"
    "BOOLE-1"
    "BOOLE-2"
    "BOOLE-AND"
    "BOOLE-ANDC1"
    "BOOLE-ANDC2"
    "BOOLE-C1"
    "BOOLE-C2"
    "BOOLE-CLR"
    "BOOLE-EQV"
    "BOOLE-IOR"
    "BOOLE-NAND"
    "BOOLE-NOR"
    "BOOLE-ORC1"
    "BOOLE-ORC2"
    "BOOLE-SET"
    "BOOLE-XOR"
    "CALL-ARGUMENTS-LIMIT"
    "CHAR-CODE-LIMIT"
    "DOUBLE-FLOAT-EPSILON"
    "DOUBLE-FLOAT-NEGATIVE-EPSILON"
    "INTERNAL-TIME-UNITS-PER-SECOND"
    "LAMBDA-LIST-KEYWORDS"
    "LAMBDA-PARAMETERS-LIMIT"
    "LEAST-NEGATIVE-DOUBLE-FLOAT"
    "LEAST-NEGATIVE-LONG-FLOAT"
    "LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT"
    "LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT"
    "LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT"
    "LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT"
    "LEAST-NEGATIVE-SHORT-FLOAT"
    "LEAST-NEGATIVE-SINGLE-FLOAT"
    "LEAST-POSITIVE-DOUBLE-FLOAT"
    "LEAST-POSITIVE-LONG-FLOAT"
    "LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT"
    "LEAST-POSITIVE-NORMALIZED-LONG-FLOAT"
    "LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT"
    "LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT"
    "LEAST-POSITIVE-SHORT-FLOAT"
    "LEAST-POSITIVE-SINGLE-FLOAT"
    "LONG-FLOAT-EPSILON"
    "LONG-FLOAT-NEGATIVE-EPSILON"
    "MOST-NEGATIVE-DOUBLE-FLOAT"
    "MOST-NEGATIVE-FIXNUM"
    "MOST-NEGATIVE-LONG-FLOAT"
    "MOST-NEGATIVE-SHORT-FLOAT"
    "MOST-NEGATIVE-SINGLE-FLOAT"
    "MOST-POSITIVE-DOUBLE-FLOAT"
    "MOST-POSITIVE-FIXNUM"
    "MOST-POSITIVE-LONG-FLOAT"
    "MOST-POSITIVE-SHORT-FLOAT"
    "MOST-POSITIVE-SINGLE-FLOAT"
    "MULTIPLE-VALUES-LIMIT"
    "PI"
    "SHORT-FLOAT-EPSILON"
    "SHORT-FLOAT-NEGATIVE-EPSILON"
    "SINGLE-FLOAT-EPSILON"
    "SINGLE-FLOAT-NEGATIVE-EPSILON"

    ;; The cross-compiler itself shouldn't really need to use the host
    ;; versions of these in target code except in exceptional cases.
    "CHAR-CODE"
    "CODE-CHAR"
    "COMPILE-FILE"
    "COMPILE-FILE-PATHNAME"
    "*COMPILE-FILE-PATHNAME*"
    "*COMPILE-FILE-TRUENAME*"
    "*COMPILE-PRINT*"
    "*COMPILE-VERBOSE*"
    "COMPILER-MACRO-FUNCTION"
    "CONSTANTP"
    "GET-SETF-EXPANSION"
    "GENSYM"
    "*GENSYM-COUNTER*"
    "LISP-IMPLEMENTATION-TYPE" "LISP-IMPLEMENTATION-VERSION"
    "MACRO-FUNCTION"
    "MACROEXPAND" "MACROEXPAND-1" "*MACROEXPAND-HOOK*"
    "MAKE-LOAD-FORM"
    "MAKE-LOAD-FORM-SAVING-SLOTS"
    "PROCLAIM"
    "SPECIAL-OPERATOR-P"
    "SUBTYPEP"
    "UPGRADED-ARRAY-ELEMENT-TYPE"
    "UPGRADED-COMPLEX-PART-TYPE"
    "WITH-COMPILATION-UNIT"
    ))

;;; A symbol in the "dual personality" list refers to the symbol in CL unless
;;; package-prefixed with SB-XC:.  The main reason for not putting these
;;; in the *shadows* list is that it's not worth trying to handle, or inefficient
;;; to handle the general case of +,-,*,/ and the comparators.
;;; i.e. the code compiled in make-host-1 by the host compiler would be
;;; that much less efficient by always having to use the intercepted function)
;;; We're also not handling 1+ or 1- or INCF, DECF.
;;; It's unlikely that a host floating-pointer value could sneak through
;;; to one of the un-intercepted functions given the prohibition against
;;; using floating-point literals and that almost all other functions
;;; are intercepted. Granted there are some roundabout ways to spell a
;;; floating-point number that can not be detected, such as:
;;;   (* 50 (hash-table-rehash-threshold (make-hash-table)))
;;; because we are not intercepting '*.
(defparameter *dual-personality-math-symbols*
  '("+" "-" "*" "/" "=" "/=" "<" "<=" ">" ">=" "MIN" "MAX"
    ;; We've gotten along quite well without an alter-ego for FIXNUM,
    ;; but now some s-expressions mentioning the type FIXNUM are fed
    ;; to the host for evaluation, and a type-checking host (such as we are)
    ;; croaks if an argument exceeds the host's notion of fixnum.
    ;; Get around it by changing those uses of fixnum to SB-XC:FIXNUM.
    "FIXNUM"
    ))

;;; When playing such tricky package games, it's best to have the symbols that
;;; are visible by default, i.e. XC-STRICT-CL:NAME, have no definition,
;;; and the expressly qualified (with SB-XC:) symbols have definitions.
;;; This idiom makes you pick one or the other of CL:THING or SB-XC:THING,
;;; and not ever just get one at random.
;;; In fact I especially don't like the magic byte specifier hacks. It would be
;;; safer and clearer to have no definition associated with the symbols that you
;;; see by default, so that using them by accident fails.
(defparameter *undefineds*
  '("SYMBOL-PACKAGE"
    "PACKAGE-NAME"
    ;; Float decoding: don't want to see these used either.
    "DECODE-FLOAT" "INTEGER-DECODE-FLOAT"
    "FLOAT-DIGITS" "FLOAT-PRECISION" "FLOAT-RADIX"
    "SCALE-FLOAT"))

;; The running-in-the-host-Lisp Python cross-compiler defines its
;; own versions of a number of functions which should not overwrite
;; host-Lisp functions. Instead we put them in a special package.
;;
;; The common theme of the functions, macros, constants, and so
;; forth in this package is that they run in the host and affect the
;; compilation of the target.
;;
(let ((package-name "SB-XC"))
  (dolist (name (append *undefineds* *dual-personality-math-symbols*))
    ;; FIXME: this triggers some pathological behavior in our implementation
    ;; of EXPORT. For each symbol, we're adding it to the internals, then
    ;; removing that to add to the externals, each time shrinking the hashset
    ;; of internals back to nothing. Is there way to not do that?
    (export (intern name package-name) package-name))
  (dolist (name '("*READ-DEFAULT-FLOAT-FORMAT*"
                  "ARRAY-ELEMENT-TYPE"
                  "DEFMACRO" "DEFSTRUCT" "DEFTYPE"
                  "MAKE-ARRAY"
                  "SIMPLE-VECTOR"
                  "TYPEP"
                  ))
    (export (intern name package-name) package-name)))

(defun count-symbols (pkg)
  (let ((n 0))
    (do-external-symbols (s pkg n)
      (declare (ignorable s))
      (incf n))))

;;; Build a new package that exports a not-necessarily-strict subset of
;;; what the host CL exports. This deals with hosts that have too many
;;; symbols exported from CL.
(let ((cl-model-package (make-package "XC-STRICT-CL" :use nil)))
  (flet ((new-external (x package &aux (s (intern x package)))
           (export s package)
           s)
         (reexport (x)
           (import x cl-model-package)
           (export x cl-model-package)))
    (reexport (list nil))
    (dolist (string (read-from-file "^common-lisp-exports.lisp-expr"))
      (unless (string= string "NIL") ; already done
        (cond ((member string *undefineds* :test #'string=)
               (new-external string cl-model-package))
              ((find string *shadows* :test #'string=)
               (reexport (new-external string "SB-XC")))
              ((find-symbol string "CL")
               (reexport (find-symbol string "CL")))
              (t
               (warn "No symbol named ~S in host CL package!" string)
               (new-external string cl-model-package)))))))

;;; Snapshot so that we can ascertain in genesis that nothing new got interned
;;; in the standardized packages.
(defun compute-cl-package-symbol-counts ()
  (mapcar (lambda (x) (cons x (count-symbols x)))
          '("XC-STRICT-CL" "SB-XC")))

(defvar *package-symbol-counts* (compute-cl-package-symbol-counts))
(defun check-no-new-cl-symbols ()
  (assert (equal *package-symbol-counts* (compute-cl-package-symbol-counts))))

(defun create-target-packages (package-data-list)
  (hide-host-packages)

  ;; Build all packages that we need, and initialize them as far as we
  ;; can without referring to any other packages.
  (dolist (package-data package-data-list)
    (let ((package (make-package (package-data-name package-data)
                                 :use nil
                                 :nicknames (package-data-nicknames package-data))))
      (dolist (string (package-data-shadow package-data))
        (shadow string package))
      (setf (documentation package t) (package-data-documentation package-data))))
  ;; Now that all packages exist, we can set up package-package
  ;; references.
  (dolist (package-data package-data-list)
    (use-package (substitute "XC-STRICT-CL" "CL"
                             (package-data-use package-data)
                             :test 'string=)
                 (package-data-name package-data))
    ;; Note: Unlike plain-old DEFPACKAGE, this IMPORT-FROM does
    ;; potentially intern NAME into the FROM-PACKAGE to make forward
    ;; references work.
    (dolist (sublist (package-data-import-from package-data))
      (let* ((from-package (first sublist))
             (from-package (if (string= from-package "CL")
                               "XC-STRICT-CL"
                               from-package)))
        (import (mapcar (lambda (name) (intern name from-package))
                        (rest sublist))
                (package-data-name package-data))))
    (dolist (string (package-data-export package-data))
      (export (intern string (package-data-name package-data))
              (package-data-name package-data))))

  (unhide-host-format-funs))

(export '*undefined-fun-allowlist*)
(defvar *undefined-fun-allowlist* (make-hash-table :test 'equal))

(defparameter *package-data-list* '())

;;; Like DEFPACKAGE, but does some special stuff to make forward
;;; references work.
(defmacro defpackage* (name &rest options)
  (let ((flattened-options '()))
    (dolist (option options)
      (destructuring-bind (kind . args) option
        (ecase kind
          ((:use :export :shadow :nicknames)
           (let ((existing-option (assoc kind flattened-options)))
             (if existing-option
                 (setf (second (second existing-option))
                       (append (second (second existing-option)) args))
                 (push (list kind (list 'quote args)) flattened-options))))
          ((:import-from)
           (let ((existing-option (assoc kind flattened-options)))
             (if existing-option
                 (push args (second (second existing-option)))
                 (push (list kind (list 'quote (list args))) flattened-options))))
          ((:documentation)
           (push option flattened-options)))))
    `(push (make-package-data :name ',name ,@(mapcan #'identity flattened-options))
           *package-data-list*)))

(let ((*readtable* *xc-readtable*))
  (load (find-bootstrap-file "^exports.lisp"))
  (create-target-packages *package-data-list*))

(defun read-undefined-fun-allowlist ()
  (with-open-file (data (find-bootstrap-file "^undefined-fun-allowlist.lisp-expr"))
    (let ((*readtable* *xc-readtable*))
      (dolist (name (apply #'append (read data)))
        (setf (gethash name *undefined-fun-allowlist*) t)))))

(defvar *asm-package-use-list*
  '("SB-ASSEM" "SB-DISASSEM"
    "SB-INT" "SB-EXT" "SB-KERNEL" "SB-VM"
    "SB-SYS" ; for SAP accessors
    ;; Dependence of the assembler on the compiler feels a bit backwards,
    ;; but assembly needs TN-SC, TN-OFFSET, etc. because the compiler
    ;; doesn't speak the assembler's language. Rather vice-versa.
    "SB-C"))
(defun make-assembler-package (pkg-name)
  (when (find-package pkg-name)
    (delete-package pkg-name))
  (let ((pkg (make-package pkg-name
                           :use (cons "XC-STRICT-CL" (cddr *asm-package-use-list*)))))
    ;; Both SB-ASSEM and SB-DISASSEM export these two symbols.
    ;; Neither is shadowing-imported. If you need one, package-qualify it.
    (shadow '("SEGMENT" "MAKE-SEGMENT") pkg)
    (use-package '("SB-ASSEM" "SB-DISASSEM") pkg)
    pkg))

;; Each backend should have a different package for its instruction set
;; so that they can co-exist.
(make-assembler-package (backend-asm-package-name))

;;; Not all things shown by this are actually unused. Some get removed
;;; by the tree-shaker as intended.
#+nil
(defun show-unused-exports (&aux nonexistent uninteresting)
  (dolist (entry *package-data-list*)
    (let ((pkg (find-package (package-data-name entry))))
      (dolist (string (mapcan (lambda (x) (if (stringp x) (list x) x))
                              (package-data-export entry)))
        (unless (or (string= string "!" :end1 1) (string= string "*!" :end1 2))
          (let ((s (find-symbol string pkg)))
            (cond ((not s)
                   (push (cons pkg string) nonexistent))
                  ((and (not (boundp s))
                        (not (sb-kernel:symbol-%info s))
                        (not (gethash s sb-c::*backend-parsed-vops*)))
                   (push s uninteresting))))))))
  (format t "~&Nonexistent:~%")
  (dolist (x nonexistent)
    (format t "  ~a ~a~%" (package-name (car x)) (cdr x)))
  (format t "~&Possibly uninteresting:~%")
  ;; FIXME: prints some things that it shouldn't as "uninteresting"
  ;; including but not limited to:
  ;;   - alien struct slot names
  ;;   - catch tag names (e.g. 'TOPLEVEL-CATCHER)
  ;;   - declarations
  ;;   - restart names
  ;;   - object-not-<type>-error
  ;;   - markers such as SB-SYS:MACRO (in lexenvs)
  (dolist (x uninteresting)
    (format t "  ~s~%" x)))
