;;;; utility functions and macros needed by the back end to generate
;;;; code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Make a fixnum out of NUM. (I.e. shift by two bits if it will fit.)
(defun fixnumize (num)
  (if (fixnump num)
      (ash num n-fixnum-tag-bits)
      (error "~W is too big for a fixnum." num)))

(declaim (inline tn-byte-offset))
(defun tn-byte-offset (tn)
  (ash (tn-offset tn) word-shift))

;;; Determining whether a constant offset fits in an addressing mode.
#+(or x86 x86-64)
(defun foldable-constant-offset-p (element-size lowtag data-offset offset)
  (if (< element-size n-byte-bits)
      nil
      (multiple-value-bind (min max)
          (displacement-bounds lowtag element-size data-offset)
        (<= min offset max))))


;;;; routines for dealing with static symbols

(defun static-symbol-p (symbol)
  (or (null symbol)
      (and (find symbol +static-symbols+) t)))

;;; the byte offset of the static symbol SYMBOL
(defun static-symbol-offset (symbol)
  (if symbol
      (let ((posn (position symbol +static-symbols+)))
        (unless posn (error "~S is not a static symbol." symbol))
        (+ (* posn (pad-data-block symbol-size))
           (pad-data-block (1- symbol-size))
           other-pointer-lowtag
           (- list-pointer-lowtag)))
      0))

(symbol-macrolet ((alien-linkage-table-space-end
                   (+ alien-linkage-table-space-start alien-linkage-table-space-size)))
;;; the address of the linkage table entry for table index I.
(defun alien-linkage-table-entry-address (i)
  (ecase alien-linkage-table-growth-direction
    (:up   (+ (* i alien-linkage-table-entry-size) alien-linkage-table-space-start))
    (:down (- alien-linkage-table-space-end (* (1+ i) alien-linkage-table-entry-size)))))

#-sb-xc-host
(defun alien-linkage-table-index-from-address (addr)
  (ecase alien-linkage-table-growth-direction
    (:up
     (floor (- addr alien-linkage-table-space-start) alien-linkage-table-entry-size))
    (:down
     (1- (floor (- alien-linkage-table-space-end addr) alien-linkage-table-space-end)))))
)

(defconstant-eqx +all-static-fdefns+
    #.(concatenate 'vector +c-callable-fdefns+ +static-fdefns+) #'equalp)

;;; Return the (byte) offset from NIL to the start of the fdefn object
;;; for the static function NAME.
(defun static-fdefn-offset (name)
  (let ((static-fun-index (position name +all-static-fdefns+)))
    (and static-fun-index
         (+ (* (length +static-symbols+) (pad-data-block symbol-size))
            (pad-data-block (1- symbol-size))
            (- list-pointer-lowtag)
            (* static-fun-index (pad-data-block fdefn-size))
            other-pointer-lowtag))))

;;; Return absolute address of the 'fun' slot in static fdefn NAME.
(defun static-fdefn-fun-addr (name)
  (+ nil-value
     (static-fdefn-offset name)
     (- other-pointer-lowtag)
     (ash fdefn-fun-slot word-shift)))

;;; Return the (byte) offset from NIL to the raw-addr slot of the
;;; fdefn object for the static function NAME.
(defun static-fun-offset (name)
  (+ (static-fdefn-offset name)
     (- other-pointer-lowtag)
     (* fdefn-raw-addr-slot n-word-bytes)))

;;; Various error-code generating helpers
(defvar *adjustable-vectors*)

(defmacro with-adjustable-vector ((var) &rest body)
  `(let ((,var (or (pop *adjustable-vectors*)
                   (make-array 16
                               :element-type '(unsigned-byte 8)
                               :fill-pointer 0
                               :adjustable t))))
     ;; Don't declare the length - if it gets adjusted and pushed back
     ;; onto the freelist, it's anyone's guess whether it was expanded.
     ;; This code was wrong for >12 years, so nobody must have needed
     ;; more than 16 elements. Maybe we should make it nonadjustable?
     (declare (type (vector (unsigned-byte 8)) ,var))
     (setf (fill-pointer ,var) 0)
     ;; No UNWIND-PROTECT here - semantics are unaffected by nonlocal exit,
     ;; and this macro is about speeding up the compiler, not slowing it down.
     ;; GC will clean up any debris, and since the vector does not point
     ;; to anything, even an accidental promotion to a higher generation
     ;; will not cause transitive garbage retention.
     (prog1 (progn ,@body)
       (push ,var *adjustable-vectors*))))

;;;; interfaces to IR2 conversion

;;; Return a wired TN describing the N'th full call argument passing
;;; location.
(defun standard-arg-location (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-wired-tn *backend-t-primitive-type* descriptor-reg-sc-number
                     (nth n *register-arg-offsets*))
      (make-wired-tn *backend-t-primitive-type* control-stack-sc-number n)))

;;; Same as above but marks stack locations as :arg-pass
(defun standard-call-arg-location (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-wired-tn *backend-t-primitive-type* descriptor-reg-sc-number
                     (nth n *register-arg-offsets*))
      (let ((tn
              (make-wired-tn *backend-t-primitive-type* control-stack-sc-number n)))
        (setf (tn-kind tn) :arg-pass)
        tn)))

(defun standard-arg-location-sc (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-sc+offset descriptor-reg-sc-number
                      (nth n *register-arg-offsets*))
      (make-sc+offset control-stack-sc-number n)))

(defstruct fixed-call-args-state
  (descriptors -1 :type fixnum)
  #-c-stack-is-control-stack
  (non-descriptors -1 :type fixnum)
  (float -1 :type fixnum))

(declaim (#+sb-xc-host special
          #-sb-xc-host sb-ext:global
          *float-regs* *descriptor-args*
          #-c-stack-is-control-stack *non-descriptor-args*))

(defun fixed-call-arg-location (type state)
  (let* ((primtype (primitive-type type))
         (sc (find descriptor-reg-sc-number (sb-c::primitive-type-scs primtype) :test-not #'eql)))
    (case (primitive-type-name primtype)
      ((double-float single-float)
       (make-wired-tn primtype
                      sc
                      (elt *float-regs* (incf (fixed-call-args-state-float state)))))
      ((unsigned-byte-64 signed-byte-64)
       (make-wired-tn primtype
                      sc
                      (elt #-c-stack-is-control-stack *non-descriptor-args*
                           #+c-stack-is-control-stack *descriptor-args*
                           (incf (#-c-stack-is-control-stack fixed-call-args-state-non-descriptors
                                  #+c-stack-is-control-stack fixed-call-args-state-descriptors
                                  state)))))
      (t
       (make-wired-tn primtype
                      descriptor-reg-sc-number
                      (elt *descriptor-args* (incf (fixed-call-args-state-descriptors state))))))))

;;; Make a TN to hold the number-stack frame pointer.  This is allocated
;;; once per component, and is component-live.
(defun make-nfp-tn ()
  #+c-stack-is-control-stack
  (make-restricted-tn *fixnum-primitive-type* ignore-me-sc-number)
  #-c-stack-is-control-stack
  (component-live-tn
   (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nfp-offset)))

;;; Make an environment-live stack TN for saving the SP for NLX entry.
(defun make-nlx-sp-tn (env)
  (environment-live-tn
   (make-representation-tn *fixnum-primitive-type* any-reg-sc-number)
   env))

#-x86-64
(defun make-stack-pointer-tn (&optional nargs)
  (declare (ignore nargs))
  (make-normal-tn *fixnum-primitive-type*))

(defun make-number-stack-pointer-tn ()
  #+c-stack-is-control-stack
  (make-restricted-tn *fixnum-primitive-type* ignore-me-sc-number)
  #-c-stack-is-control-stack
  (make-normal-tn *fixnum-primitive-type*))

;;; Return a list of TNs that can be used to represent an unknown-values
;;; continuation within a function.
(defun make-unknown-values-locations (&optional unused-count unused-sp)
  (declare (ignorable unused-count unused-sp))
  (list (cond #+(or arm64 x86-64)
              ;; needs support from receive-unknown-values, push-values, %more-arg-values, values-list,
              ;; nlx-entry-multiple
              (unused-sp
               (sb-c::make-unused-tn))
              (t
               (make-stack-pointer-tn)))
        (cond #+(or arm64 x86-64)
              (unused-count
               (sb-c::make-unused-tn))
              (t
               (make-normal-tn *fixnum-primitive-type*)))))

(defun error-call (vop error-code &rest values)
  "Cause an error.  ERROR-CODE is the error to cause."
  (emit-error-break vop error-trap (error-number-or-lose error-code) values))

(defun cerror-call (vop error-code &rest values)
  "Cause a continuable error.  ERROR-CODE is the error to cause."
  (emit-error-break vop cerror-trap (error-number-or-lose error-code) values))

#+sb-safepoint
(define-vop (insert-safepoint)
  (:policy :fast-safe)
  (:translate sb-kernel::gc-safepoint)
  (:generator 0
    (emit-safepoint)))

;;; Does the TN definitely hold *any* of the 4 pointer types
(defun pointer-tn-ref-p (tn-ref)
  (and (sc-is (tn-ref-tn tn-ref) descriptor-reg)
       (not (types-equal-or-intersect
             (tn-ref-type tn-ref)
             (specifier-type '(or fixnum
                               #+64-bit single-float
                               character))))))

;;; Does the TN definitely hold any of the 3 non-list pointer types
(defun headered-object-pointer-tn-ref-p (tn-ref)
  (and (pointer-tn-ref-p tn-ref)
       (not (types-equal-or-intersect (tn-ref-type tn-ref)
                                      (specifier-type 'list)))))

;;; Does the TN definitely hold an OTHER pointer?
;;; If the operation next to be performed on TN is a widetag test,
;;; then NIL is ok as the input. Indicate this by specifying PERMIT-NIL.
;;; With rare exception it should always be permitted, though not on ppc64
;;; where it would never be. The safe default is NIL.
(defun other-pointer-tn-ref-p (tn-ref &optional permit-nil)
  (and (sc-is (tn-ref-tn tn-ref) descriptor-reg)
       (not (types-equal-or-intersect
             (tn-ref-type tn-ref)
             (if permit-nil
                 (specifier-type '(or fixnum
                                   #+64-bit single-float
                                   function
                                   cons
                                   instance
                                   character))
                 (specifier-type '(or fixnum
                                   #+64-bit single-float
                                   function
                                   list
                                   instance
                                   character)))))))

(defun fixnum-or-other-pointer-tn-ref-p (tn-ref &optional permit-nil)
  (and (sc-is (tn-ref-tn tn-ref) descriptor-reg)
       (not (types-equal-or-intersect
             (tn-ref-type tn-ref)
             (specifier-type (if permit-nil
                                 '(or #+64-bit single-float
                                   function
                                   cons
                                   instance
                                   character)
                                 '(or #+64-bit single-float
                                   function
                                   list
                                   instance
                                   character)))))))

(defun not-nil-tn-ref-p (tn-ref)
  (not (types-equal-or-intersect (tn-ref-type tn-ref)
                                 (specifier-type '(eql nil)))))

(defun instance-tn-ref-p (tn-ref)
  (csubtypep (tn-ref-type tn-ref) (specifier-type 'instance)))

(defun stack-consed-p (object)
  (let ((write (sb-c::tn-writes object))) ; list of write refs
    (when (or (not write)    ; grrrr, the only write is from a LOAD tn
                                        ; and we don't know the corresponding normal TN?
              (tn-ref-next write))      ; can't determine if > 1 write
      (return-from stack-consed-p nil))
    (let ((vop (tn-ref-vop write)))
      (when (not vop)                   ; wat?
        (return-from stack-consed-p nil))
      (when (eq (vop-name vop) 'allocate-vector-on-stack)
        (return-from stack-consed-p t))
      (when (or (and (eq (vop-name vop) 'fixed-alloc) ; do we still need this case?
                     (fifth (vop-codegen-info vop))) ; STACK-ALLOCATE-P
                (eq (vop-name vop) 'sb-c::fixed-alloc-to-stack))
        (return-from stack-consed-p t))
      ;; Should we try to detect a stack-consed LIST also?
      ;; I don't think that will work.
      ;; (And is there anything else interesting to try?)
      (unless (member (vop-name vop) '(splat-word splat-small splat-any))
        (return-from stack-consed-p nil))
      (let* ((splat-input (vop-args vop))
             (splat-input-source
               (tn-ref-vop (sb-c::tn-writes (tn-ref-tn splat-input)))))
        ;; How in the heck can there NOT be a vop??? Well, sometimes there isn't.
        (when (and splat-input-source
                   (eq (vop-name splat-input-source)
                       'allocate-vector-on-stack))
          (return-from stack-consed-p t)))))
  nil)

;;; Just gathering some data to see where we can improve
(define-load-time-global *store-barriers-potentially-emitted* 0)
(define-load-time-global *store-barriers-emitted* 0)

(defun require-gc-store-barrier-p (object value-tn-ref value-tn)
  (incf *store-barriers-potentially-emitted*)
  ;; If OBJECT is stack-allocated, elide the barrier
  (when (stack-consed-p object)
    (return-from require-gc-store-barrier-p nil))
  (flet ((potential-heap-pointer-p (tn tn-ref)
           (when (sc-is tn any-reg) ; must be fixnum
             (return-from potential-heap-pointer-p nil))
           ;; If stack-allocated, elide the barrier
           (when (stack-consed-p tn)
             (return-from potential-heap-pointer-p nil))
           ;; If immediate non-pointer, elide the barrier
           (when (sc-is tn immediate)
             (let ((value (tn-value tn)))
               (when (sb-xc:typep value '(or character sb-xc:fixnum boolean
                                             #+64-bit single-float))
                 (return-from potential-heap-pointer-p nil))))
           (when (sb-c::unbound-marker-tn-p tn)
             (return-from potential-heap-pointer-p nil))
           ;; And elide for things like (OR FIXNUM NULL)
           (let ((type (tn-ref-type tn-ref)))
             (when (csubtypep type (specifier-type '(or character sb-xc:fixnum boolean
                                                        #+64-bit single-float)))
               (return-from potential-heap-pointer-p nil)))
           t))
    (cond (value-tn
           (unless (eq (tn-ref-tn value-tn-ref) value-tn)
             (aver (eq (tn-ref-load-tn value-tn-ref) value-tn)))
           (unless (potential-heap-pointer-p value-tn value-tn-ref)
             (return-from require-gc-store-barrier-p nil)))
          (value-tn-ref ; a list of refs linked through TN-REF-ACROSS
           ;; (presumably from INSTANCE-SET-MULTIPLE)
           (let ((any-pointer
                  (do ((ref value-tn-ref (tn-ref-across ref)))
                      ((null ref))
                    (when (potential-heap-pointer-p (tn-ref-tn ref) ref)
                      (return t)))))
             (unless any-pointer
               (return-from require-gc-store-barrier-p nil))))))
  (incf *store-barriers-emitted*)
  t)

(defun vop-nth-arg (n vop)
  (let ((ref (vop-args vop)))
    (dotimes (i n ref) (setq ref (tn-ref-across ref)))))

(defun length-field-shift (widetag)
  (if (= widetag instance-widetag)
      instance-length-shift
      n-widetag-bits))

(defconstant array-rank-mask 255)
;;; Rank is encoded as a (UNSIGNED-BYTE 8) minus one.
;;; Initialization of simple rank 1 array header words is completely unaffected-
;;; they store 0 for the rank, which is the correct encoding for 1.
;;; The encoding 1 means 2, encoding 2 means 3, and so on.
;;; Decoding is just an addition and bitwise AND.
(defun encode-array-rank (rank)
  (declare (type (unsigned-byte 8) rank))
  (logand (1- rank) array-rank-mask))

(defun compute-object-header (nwords widetag-or-metadata)
  (let* ((widetag (if (typep widetag-or-metadata '(or wrapper defstruct-description))
                      instance-widetag
                      widetag-or-metadata))
         (array-header-p
          (or (= widetag simple-array-widetag)
              (>= widetag complex-base-string-widetag))))
    (logior (if array-header-p
                (let ((rank (- nwords array-dimensions-offset)))
                  (ash (encode-array-rank rank) array-rank-position))
                (case widetag
                  (#.fdefn-widetag 0)
                  (t (ash (1- nwords) (length-field-shift widetag)))))
            widetag)))

;;; Convert # of "big digits" (= words, sometimes called "limbs") to a header value.
(defmacro bignum-header-for-length (n)
  (logior (ash n n-widetag-bits) bignum-widetag))

(defmacro id-bits-offset ()
  (let ((slot (get-dsd-index layout sb-kernel::id-word0)))
    (ash (+ sb-vm:instance-slots-offset slot) sb-vm:word-shift)))

;;; I'd like the division-by-constant-integer optimization to work
;;; during cross-compilation, but the algorithm to compute the magic
;;; parameters is expressed in C, not Lisp. I need to translate it.
#-sb-xc-host
(defun sb-c:compute-udiv32-magic (divisor)
  (with-alien ((mag (struct magu
                            (m unsigned-int)
                            (a int)
                            (s int)))
               (compute-udiv-magic32 (function void int (* (struct magu)))
                                     :extern))
    (alien-funcall compute-udiv-magic32 divisor (addr mag))
    (values (slot mag 'm) (slot mag 'a) (slot mag 's))))

;;; "Algorithm 2: Algorithm to select the number of fractional bits and the scaled
;;; approximate reciprocal in the case of unsigned integers."
;;; from https://r-libre.teluq.ca/1633/1/Faster_Remainder_of_the_Division_by_a_Constant.pdf
;;; See also https://github.com/bmkessler/fastdiv for that coded in Go.
;;; D = divisor
;;; N = number of bits of precision in numerator
;;; FRACTION-BITS is what you want, or :VARIABLE for the smallest
;;;
;;; Note that for 32 fraction bits, the divisor can *not* use all 32 bits of precision.
;;; It can only have about 27 or 28 significant bits. This function will figure it out.
(defun compute-fastrem-coefficient (d n fraction-bits)
  (multiple-value-bind (smallest-f c)
      (flet ((is-pow2 (n)
               (declare (unsigned-byte n))
               (let ((l (integer-length n)))
                 (= n (ash 1 (1- l))))))
        (if (is-pow2 d)
            (values (1- (integer-length d)) 1)
            (loop for L from 0
                  do (let* ((F (+ N L))
                            (2^F (expt 2 F)))
                       (when (<= d (+ (mod 2^F d) (expt 2 L)))
                         (let ((c (ceiling (expt 2 F) d)))
                           (return (values F c))))))))
    (cond ((eq fraction-bits :variable) ; return the smallest F
           (values c smallest-f))
          (t
           ;; Otherwise hardwire F to 32 so the algorithm can use :DWORD
           ;; register moves to perform the shifting and masking.
           ;; But make sure the smallest-f is not more than 32, or else
           ;; this can't work.
           (when (> smallest-f fraction-bits)
             (error "Need ~D fraction bits for divisor ~D and ~D bit dividend"
                    smallest-f d n))
           (values (ceiling (expt 2 fraction-bits) d) fraction-bits)))))

