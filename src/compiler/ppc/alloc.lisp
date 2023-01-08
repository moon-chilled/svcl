;;;; allocation VOPs for the PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; Storage allocation:

;;; This is the main mechanism for allocating memory in the lisp heap.
;;;
;;; The allocated space is stored in RESULT-TN with the lowtag LOWTAG
;;; applied.  The amount of space to be allocated is SIZE bytes (which
;;; must be a multiple of the lisp object size).
;;;
;;; On other platforms (Non-PPC), if STACK-P is given, then allocation
;;; occurs on the control stack (for dynamic-extent).  In this case,
;;; you MUST also specify NODE, so that the appropriate compiler
;;; policy can be used, and TEMP-TN, which is needed for work-space.
;;; TEMP-TN MUST be a non-descriptor reg. FIXME: This is not yet
;;; implemented on PPC. We should implement this and replace the
;;; inline stack-based allocation that presently occurs in the
;;; VOPs. The stack-p argument is ignored on PPC.
(defun allocation (type size lowtag result-tn &key stack-p node temp-tn flag-tn)
  (declare (ignore stack-p node))
  (binding*  ((cons-region-p (or #+use-cons-region (eq type 'list)))
              ((region-base-tn field-offset)
               #-sb-thread
               (values null-tn (- (if cons-region-p cons-region mixed-region) nil-value))
               #+sb-thread
               (values thread-base-tn
                       (ash (if cons-region-p thread-cons-tlab-slot thread-mixed-tlab-slot)
                            word-shift)))
              (imm-size (typep size '(unsigned-byte 15))))

    (unless imm-size ; Make temp-tn be the size
      (if (numberp size)
          (inst lr temp-tn size)
          (move temp-tn size)))

    (inst lwz result-tn region-base-tn field-offset)
    (inst lwz flag-tn region-base-tn (+ field-offset n-word-bytes)) ; region->end_addr

    ;; CAUTION: The C code depends on the exact order of
    ;; instructions here.  In particular, immediately before the
    ;; TW instruction must be an ADD or ADDI instruction, so it
    ;; can figure out the size of the desired allocation, and
    ;; storing the new base pointer back to the allocation region
    ;; must take one instruction.
    (without-scheduling ()
      ;; Make result-tn point at the end of the object, to
      ;; figure out if we overflowed the current region.
      (if imm-size
          (inst addi result-tn result-tn size)
          (inst add result-tn result-tn temp-tn))

      ;; result-tn points to the new end of the region.  Did we go past
      ;; the actual end of the region?  If so, we need a full alloc.
      ;; The C code depends on this exact form of instruction.  If
      ;; either changes, you have to change the other appropriately!
      ;; These two trap instructions are behaviorally the same,
      ;; but the encoding of the TO field informs the runtime
      ;; whether the allocation is a list or general object.
      (if (eq type 'list)
          (inst tw :llt flag-tn result-tn)  ; trap if region.end < new_freeptr
          (inst tw :lgt result-tn flag-tn)) ; trap if new_freeptr > region.end
      (inst stw result-tn region-base-tn field-offset))

    ;; Execution resumes here if the trap fires.
    ;; At this point, result-tn points at the end of the object.
    ;; Adjust to point to the beginning.
    (cond (imm-size
           (inst addi result-tn result-tn (+ (- size) lowtag)))
          (t
           (inst sub result-tn result-tn temp-tn)
           ;; Set the lowtag appropriately
           (inst ori result-tn result-tn lowtag)))))

(defun align-csp (temp)
  ;; is used for stack allocation of dynamic-extent objects
  (storew null-tn csp-tn 0 0) ; store a known-good value (don't want wild pointers below CSP)
  (inst addi temp csp-tn lowtag-mask)
  (inst clrrwi csp-tn temp n-lowtag-bits))

;;;; LIST and LIST*
(define-vop (list)
  (:args (things :more t :scs (any-reg descriptor-reg zero null control-stack)))
  (:temporary (:scs (descriptor-reg)) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result)
              res)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:temporary (:scs (non-descriptor-reg)) alloc-temp)
  (:info star cons-cells)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 0
    (macrolet ((maybe-load (tn)
                  (once-only ((tn tn))
                    `(sc-case ,tn
                       ((any-reg descriptor-reg zero null)
                        ,tn)
                       (control-stack
                        (load-stack-tn temp ,tn)
                        temp)))))
      (let ((dx-p (node-stack-allocate-p node))
            (alloc (* (pad-data-block cons-size) cons-cells)))
        (pseudo-atomic (pa-flag :sync nil :elide-if dx-p)
                 (if dx-p
                     (progn
                       (align-csp res)
                       (inst ori res csp-tn list-pointer-lowtag)
                       (inst addi csp-tn csp-tn alloc))
                     (allocation 'list alloc list-pointer-lowtag res
                                 :temp-tn alloc-temp
                                 :flag-tn pa-flag))
                 (move ptr res)
                 (dotimes (i (1- cons-cells))
                   (storew (maybe-load (tn-ref-tn things)) ptr
                           cons-car-slot list-pointer-lowtag)
                   (setf things (tn-ref-across things))
                   (inst addi ptr ptr (pad-data-block cons-size))
                   (storew ptr ptr
                           (- cons-cdr-slot cons-size)
                           list-pointer-lowtag))
                 (storew (maybe-load (tn-ref-tn things)) ptr
                         cons-car-slot list-pointer-lowtag)
                 (storew (if star
                             (maybe-load (tn-ref-tn (tn-ref-across things)))
                             null-tn)
                         ptr cons-cdr-slot list-pointer-lowtag))
        (move result res)))))

;;;; Special purpose inline allocators.

(define-vop (make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (with-fixed-allocation (result pa-flag temp fdefn-widetag fdefn-size)
      (load-asm-rtn-addr temp 'undefined-tramp)
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length stack-allocate-p)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (pseudo-atomic (pa-flag :elide-if stack-allocate-p)
        (if stack-allocate-p
            (progn
              (align-csp result)
              (inst ori result csp-tn fun-pointer-lowtag)
              (inst addi csp-tn csp-tn alloc-size)
              (inst lr temp (logior (ash (1- size) n-widetag-bits) closure-widetag)))
            (progn
              (allocation nil (pad-data-block size) fun-pointer-lowtag result
                          :temp-tn temp :flag-tn pa-flag)
              (inst lr temp (logior (ash (1- size) n-widetag-bits) closure-widetag))))
        (storew temp result 0 fun-pointer-lowtag)
        (storew function result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
;;;
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:info stack-allocate-p)
  (:ignore stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation (result pa-flag temp value-cell-widetag value-cell-size)
      (storew value result value-cell-value-slot other-pointer-lowtag))))



;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst li result unbound-marker-widetag)))

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (load-asm-rtn-addr result 'funcallable-instance-tramp)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:generator 4
    (with-fixed-allocation (result pa-flag temp type words
                                   :lowtag lowtag
                                   :stack-allocate-p stack-allocate-p)
      )))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:generator 6
    (inst addi bytes extra (* (1+ words) n-word-bytes))
    (inst slwi header bytes (- (length-field-shift type) n-fixnum-tag-bits))
    ;; The specified EXTRA value is the exact value placed in the header
    ;; as the word count when allocating code.
    (cond ((= type code-header-widetag)
           (inst addi header header type))
          (t
           (inst addi header header (+ (ash -2 (length-field-shift type)) type))
           (inst clrrwi bytes bytes n-lowtag-bits)))
    #+bignum-assertions
    (when (= type bignum-widetag)
      (inst slwi bytes bytes 1)) ; use 2x the space
    (pseudo-atomic (pa-flag)
      (allocation nil bytes lowtag result :temp-tn temp :flag-tn pa-flag)
      (storew header result 0 lowtag))))
