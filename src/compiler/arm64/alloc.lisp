;;;; allocation VOPs for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-vop (list)
  (:args (things :more t :scs (descriptor-reg any-reg control-stack constant immediate)))
  (:temporary (:scs (descriptor-reg)) ptr temp)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result)
              res)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:info star cons-cells)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:vop-var vop)
  (:generator 0
    (let ((alloc (* (pad-data-block cons-size) cons-cells))
          (dx (node-stack-allocate-p node))
          (prev-constant))
     (macrolet ((maybe-load (tn &optional (temp 'temp))
                  (once-only ((tn tn))
                    `(sc-case ,tn
                       ((any-reg descriptor-reg)
                        ,tn)
                       ((immediate constant)
                        (cond ((eql (tn-value ,tn) 0)
                               zr-tn)
                              ((or (eql prev-constant (tn-value ,tn))
                                   (progn
                                     (setf prev-constant (tn-value ,tn))
                                     nil))
                               temp)
                              ((sc-is ,tn constant)
                               (load-constant vop ,tn ,temp)
                               ,temp)
                              (t
                               (load-immediate vop ,tn ,temp)
                               ,temp)))
                       (control-stack
                        (setf prev-constant nil)
                        (load-stack-tn ,temp ,tn)
                        ,temp)))))
       (pseudo-atomic (lr :sync nil :elide-if dx)
         (allocation 'list alloc list-pointer-lowtag res
                     :flag-tn lr
                     :stack-allocate-p dx)
         (cond ((= cons-cells 1)
                (inst stp (maybe-load (tn-ref-tn things))
                      (if star
                          (maybe-load (tn-ref-tn (tn-ref-across things)) ptr)
                          null-tn)
                      (@ tmp-tn)))
               (t
                (move ptr res)
                (dotimes (i (1- cons-cells))
                  (storew (maybe-load (tn-ref-tn things)) ptr
                      cons-car-slot list-pointer-lowtag)
                  (setf things (tn-ref-across things))
                  (inst add ptr ptr (pad-data-block cons-size))
                  (storew ptr ptr
                      (- cons-cdr-slot cons-size)
                      list-pointer-lowtag))
                (storew (maybe-load (tn-ref-tn things)) ptr
                    cons-car-slot list-pointer-lowtag)
                (storew (if star
                            (maybe-load (tn-ref-tn (tn-ref-across things)))
                            null-tn)
                    ptr cons-cdr-slot list-pointer-lowtag))))
       (move result res)))))

;;;; Special purpose inline allocators.

(define-vop (make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:sc non-descriptor-reg) temp)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (with-fixed-allocation (result lr fdefn-widetag fdefn-size)
      (load-inline-constant temp '(:fixup undefined-tramp :assembly-routine))
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))

(macrolet
    ((frob (name labelp)
       `(define-vop (,name)
          ,@(unless labelp
              '((:args (function :to :save :scs (descriptor-reg)))))
          (:info ,@(when labelp '(label)) length stack-allocate-p)
          (:temporary (:scs (non-descriptor-reg)) temp)
          (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
          (:results (result :scs (descriptor-reg)))
          (:generator 10
            (let* ((size (+ length closure-info-offset))
                   (alloc-size (pad-data-block size)))
              (pseudo-atomic (lr :elide-if stack-allocate-p)
                (allocation nil alloc-size fun-pointer-lowtag result
                            :flag-tn lr
                            :stack-allocate-p stack-allocate-p)
                (load-immediate-word temp
                                     (logior (ash (1- size) n-widetag-bits) closure-widetag))
                ,(cond (labelp
                        `(progn
                           (inst adr lr label (ash simple-fun-insts-offset word-shift))
                           (storew-pair temp 0 lr closure-fun-slot tmp-tn)))
                       (t
                        `(progn
                           (inst add lr function (- (* simple-fun-insts-offset n-word-bytes) fun-pointer-lowtag))
                           (storew-pair temp 0 lr closure-fun-slot tmp-tn))))))))))
  (frob make-closure nil)
  (frob make-closure-from-label t))

;;; The compiler likes to be able to directly make value cells.
;;;
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:info stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation (result lr value-cell-widetag
                            value-cell-size :stack-allocate-p stack-allocate-p
                                            :store-type-code nil)
      (storew-pair lr 0 value value-cell-value-slot tmp-tn))))

;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst mov result unbound-marker-widetag)))

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (load-inline-constant result '(:fixup funcallable-instance-tramp :assembly-routine))))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:generator 4
    (with-fixed-allocation (result lr type words
                            :lowtag lowtag
                            :stack-allocate-p stack-allocate-p))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg) :from :argument) bytes)
  (:temporary (:sc non-descriptor-reg) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:generator 6
    ;; Build the object header, assuming that the header was in WORDS
    ;; but should not be in the header
    (inst lsl bytes extra (- word-shift n-fixnum-tag-bits))
    (inst add bytes bytes (add-sub-immediate (* (1- words) n-word-bytes)))
    (inst lsl header bytes (- (length-field-shift type) word-shift))
    (inst add header header type)
    ;; Add the object header to the allocation size and round up to
    ;; the allocation granularity
    (inst add bytes bytes (* 2 n-word-bytes))
    (inst and bytes bytes (bic-mask lowtag-mask))
    ;; Allocate the object and set its header
    (pseudo-atomic (lr)
      (allocation nil bytes lowtag result :flag-tn lr)
      (storew header result 0 lowtag))))
