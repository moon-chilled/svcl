;;;; This file contains some parameterizations of various VM
;;;; attributes for the x86. This file is separate from other stuff so
;;;; that it can be compiled and loaded earlier.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defconstant sb-assem:assem-scheduler-p nil)
(defconstant sb-assem:+inst-alignment-bytes+ 1)

(defconstant +backend-fasl-file-implementation+ :x86-64)
(defconstant-eqx +fixup-kinds+ #(:abs32 :rel32 :absolute) #'equalp)

;;; This size is supposed to indicate something about the actual granularity
;;; at which you can map memory.  We just hardwire it, but that may or may not
;;; be necessary any more.
(defconstant +backend-page-bytes+ #+win32 65536 #-win32 32768)

;;; The size in bytes of GENCGC pages. A page is the smallest amount of memory
;;; that a thread can claim for a thread-local region, and also determines
;;; the granularity at which we can find the start of a sequence of objects.
(defconstant gencgc-page-bytes 32768)
;;; The divisor relative to page-bytes which computes the granularity
;;; at which writes to old generations are logged.
(defconstant cards-per-page 32)
;;; The minimum size of new allocation regions.  While it doesn't
;;; currently make a lot of sense to have a card size lower than
;;; the alloc granularity, it will, once we are smarter about finding
;;; the start of objects.
(defconstant gencgc-alloc-granularity 0)
;;; The minimum size at which we release address ranges to the OS.
;;; This must be a multiple of the OS page size.
(defconstant gencgc-release-granularity +backend-page-bytes+)
;;; The card size for immobile/low space
(defconstant immobile-card-bytes 4096)

;;; ### Note: we simultaneously use ``word'' to mean a 32 bit quantity
;;; and a 16 bit quantity depending on context. This is because Intel
;;; insists on calling 16 bit things words and 32 bit things
;;; double-words (or dwords). Therefore, in the instruction definition
;;; and register specs, we use the Intel convention. But whenever we
;;; are talking about stuff the rest of the lisp system might be
;;; interested in, we use ``word'' to mean the size of a descriptor
;;; object, which is 64 bits.

;;;; machine architecture parameters

;;; the number of bits per word, where a word holds one lisp descriptor
(defconstant n-word-bits 64)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 64)

;;; from AMD64 Architecture manual
(defconstant float-invalid-trap-bit       (ash 1 0))
(defconstant float-denormal-trap-bit       (ash 1 1))
(defconstant float-divide-by-zero-trap-bit (ash 1 2))
(defconstant float-overflow-trap-bit       (ash 1 3))
(defconstant float-underflow-trap-bit      (ash 1 4))
(defconstant float-inexact-trap-bit       (ash 1 5))

(defconstant float-round-to-nearest  0)
(defconstant float-round-to-negative 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-zero     3)

(defconstant-eqx float-rounding-mode     (byte 2 13) #'equalp)
(defconstant-eqx float-sticky-bits       (byte 6  0) #'equalp)
(defconstant-eqx float-traps-byte        (byte 6  7) #'equalp)
(defconstant-eqx float-exceptions-byte   (byte 6  0) #'equalp)
(defconstant float-fast-bit 0) ; no fast mode on x86-64

;;;; description of the target address space

;;; where to put the different spaces.

;;; Currently the read-only and static spaces must be located in low
;;; memory (certainly under the 4GB limit, very probably under 2GB
;;; limit). This is due to the inability of using immediate values of
;;; more than 32 bits (31 bits if you take sign extension into
;;; account) in any other instructions except MOV. Removing this limit
;;; would be possible, but probably not worth the time and code bloat
;;; it would cause. -- JES, 2005-12-11

#+(or linux darwin)
(!gencgc-space-setup #x50000000
                     :read-only-space-size #+metaspace #.(* 2 1024 1024)
                                           #-metaspace 0
                     :fixedobj-space-size #.(* 40 1024 1024)
                     :text-space-size #.(* 130 1024 1024)
                     :dynamic-space-start #x1000000000)

;;; The default dynamic space size is lower on OpenBSD to allow SBCL to
;;; run under the default 512M data size limit.

#-(or linux darwin)
(!gencgc-space-setup #x20000000
                     :read-only-space-size 0
                     :dynamic-space-start #x1000000000
                     #+openbsd :dynamic-space-size #+openbsd #x1bcf0000)

(defconstant alien-linkage-table-growth-direction :up)
(defconstant alien-linkage-table-entry-size 16)


(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  single-step-around-trap
  single-step-before-trap
  undefined-function-trap
  invalid-arg-count-trap
  memory-fault-emulation-trap
  #+sb-safepoint global-safepoint-trap
  #+sb-safepoint csp-safepoint-trap
  uninitialized-load-trap
  ;; ERROR-TRAP has to be numerically highest.
  ;; The various internal errors are numbered from here upward.
  error-trap)

;;;; static symbols

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols. That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
;;; we could profitably keep these in registers on x86-64 now we have
;;; r8-r15 as well
;;;     Note these spaces grow from low to high addresses.
(defvar *binding-stack-pointer*)

;;; Bit indices into *CPU-FEATURE-BITS*
(defconstant cpu-has-ymm-registers   0)
(defconstant cpu-has-popcnt          1)

(defconstant-eqx +static-symbols+
 `#(,@+common-static-symbols+
    #+(and immobile-space (not sb-thread)) function-layout
    #-sb-thread *alien-stack-pointer*    ; a thread slot if #+sb-thread
     ;; interrupt handling
    #-sb-thread *pseudo-atomic-bits*     ; ditto
    #-sb-thread *binding-stack-pointer* ; ditto
    ;; Since the text space and alien linkage table might both get relocated on startup
    ;; under #+immobile-space, an alien callback wrapper can't wire in the address
    ;; of a word that holds the C function pointer to callback_wrapper_trampoline.
    ;; (There is no register that points to a known address when entering the callback)
    ;; A static symbol works well for this, and is sensible considering that
    ;; the assembled wrappers also reside in static space.
    #+(and sb-thread immobile-space) callback-wrapper-trampoline
    *cpu-feature-bits*)
  #'equalp)

(defconstant-eqx +static-fdefns+
    `#(ensure-symbol-hash sb-impl::install-hash-table-lock update-object-layout
       ,@common-static-fdefns)
  #'equalp)

#+sb-simd-pack
(progn
  (defconstant-eqx +simd-pack-element-types+
      #(single-float double-float
        (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32) (unsigned-byte 64)
        (signed-byte 8) (signed-byte 16) (signed-byte 32) (signed-byte 64))
    #'equalp)
  (defconstant sb-kernel::+simd-pack-wild+
    (ldb (byte (length +simd-pack-element-types+) 0) -1))
  (defconstant-eqx +simd-pack-128-primtypes+
      #(simd-pack-single simd-pack-double
        simd-pack-ub8 simd-pack-ub16 simd-pack-ub32 simd-pack-ub64
        simd-pack-sb8 simd-pack-sb16 simd-pack-sb32 simd-pack-sb64)
    #'equalp)
  (defconstant-eqx +simd-pack-256-primtypes+
      #(simd-pack-256-single simd-pack-256-double
        simd-pack-256-ub8 simd-pack-256-ub16 simd-pack-256-ub32 simd-pack-256-ub64
        simd-pack-256-sb8 simd-pack-256-sb16 simd-pack-256-sb32 simd-pack-256-sb64)
    #'equalp)
  (defconstant-eqx +simd-pack-512-primtypes+
      #(simd-pack-512-single simd-pack-512-double
        simd-pack-512-ub8 simd-pack-512-ub16 simd-pack-512-ub32 simd-pack-512-ub64
        simd-pack-512-sb8 simd-pack-512-sb16 simd-pack-512-sb32 simd-pack-512-sb64)
    #'equalp))

(defconstant undefined-fdefn-header
  ;; This constant is constructed as follows: Take the INT opcode
  ;; plus the undefined-fun trap byte, then the bytes of the 'disp' field
  ;; of the JMP instruction that would overwrite the INT instruction.
  ;;   INT3 <trap-code> = CC **
  ;;   JMP [RIP+16]     = FF 25 10 00 00 00 00
  ;; When assigning a function we'll change the first two bytes to 0xFF 0x25.
  ;; The 'disp' field will aready be correct.
  (logior (ash undefined-function-trap 8)
          (+ #x100000 (or #+int4-breakpoints #xCE #xCC))))
