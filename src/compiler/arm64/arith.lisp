;;;; the VM definition arithmetic VOPs for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; unary operations.

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe))

(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 64) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

(define-vop (unsigned-unop fast-safe-arith-op)
  (:args (x :scs (unsigned-reg)))
  (:results (res :scs (unsigned-reg)))
  (:note "inline (unsigned-byte 64) arithmetic")
  (:arg-types unsigned-num)
  (:result-types unsigned-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst neg res x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst neg res x)))

(define-vop (fast-negate/unsigned signed-unop)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:translate %negate)
  (:generator 3
    (inst neg res x)))

(define-vop (fast-negate/signed-unsigned signed-unop)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate %negate)
  (:generator 3
    (inst neg res x)))

(define-modular-fun %negate-mod64 (x) %negate :untagged nil 64)
(define-vop (%negate-mod64)
  (:translate %negate-mod64)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 3
    (inst neg r x)))

(define-modular-fun %negate-modfx (x) %negate :tagged t #.n-fixnum-bits)
(define-vop (%negate-modfx fast-negate/fixnum)
  (:translate %negate-modfx))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num)
  (:translate lognot)
  (:generator 1
    (inst eor res x (lognot n-fixnum-tag-bits))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 2
    (inst mvn res x)))


;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic"))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :scs (any-reg)))
  (:info y)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :scs (unsigned-reg)))
  (:info y)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:info y)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic"))

(defun bic-encode-immediate (x)
  (encode-logical-immediate (bic-mask x)))

(defun bic-fixnum-encode-immediate (x)
  (and (fixnump x)
       (encode-logical-immediate (bic-mask (fixnumize x)))))

(defmacro define-binop (translate untagged-penalty op
                        &key
                             (constant-test 'encode-logical-immediate)
                             (constant-fixnum-test 'fixnum-encode-logical-immediate)
                             swap
                             (constant-op op)
                             (constant-transform 'identity))
  `(progn
     (define-vop (,(symbolicate 'fast- translate '/fixnum=>fixnum)
                  fast-fixnum-binop)
       (:translate ,translate)
       (:generator 2
                   ,(if swap
                        `(inst ,op r y x)
                        `(inst ,op r x y))))
     (define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
                    fast-fixnum-binop-c)
       ,(if swap
            `(:arg-types (:constant (satisfies ,constant-fixnum-test))
                         tagged-num)
            `(:arg-types tagged-num
                         (:constant (satisfies ,constant-fixnum-test))))
         (:translate ,translate)
         (:generator 1
                     (inst ,constant-op r x (,constant-transform (fixnumize y)))))
     (define-vop (,(symbolicate 'fast- translate '/signed=>signed)
                  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
                   ,(if swap
                        `(inst ,op r y x)
                        `(inst ,op r x y))))
     (define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
                   fast-signed-binop-c)
       (:translate ,translate)
       ,(if swap
            `(:arg-types (:constant (satisfies ,constant-test))
                         signed-num)
            `(:arg-types signed-num
                         (:constant (satisfies ,constant-test))))
       (:generator ,untagged-penalty
                   (inst ,constant-op r x (,constant-transform y))))
     (define-vop (,(symbolicate 'fast- translate '/unsigned=>unsigned)
                  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
                   ,(if swap
                        `(inst ,op r y x)
                        `(inst ,op r x y))))
     (define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned)
                   fast-unsigned-binop-c)
       (:translate ,translate)
       ,(if swap
            `(:arg-types (:constant (satisfies ,constant-test))
                         unsigned-num)
            `(:arg-types unsigned-num
                         (:constant (satisfies ,constant-test))))
       (:generator ,untagged-penalty
                   (inst ,constant-op r x (,constant-transform y))))))

(define-binop + 4 add :constant-test add-sub-immediate-p :constant-fixnum-test fixnum-add-sub-immediate-p)
(define-binop - 4 sub :constant-test add-sub-immediate-p :constant-fixnum-test fixnum-add-sub-immediate-p)
(define-binop logand 2 and)
(define-binop logior 2 orr)
(define-binop logxor 2 eor)

(define-vop (fast--/unsigned=>signed fast-unsigned-binop)
  (:translate -)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 5
    (inst sub r x y)))

(define-binop logandc1 2 bic :swap t
  :constant-test bic-encode-immediate
  :constant-fixnum-test bic-fixnum-encode-immediate
  :constant-op and
  :constant-transform bic-mask)
(define-binop logandc2 2 bic
  :constant-test bic-encode-immediate
  :constant-fixnum-test bic-fixnum-encode-immediate
  :constant-op and
  :constant-transform bic-mask)

;; (define-binop logorc1 2 orn :swap t
;;   :constant-test bic-encode-immediate
;;   :constant-fixnum-test bic-fixnum-encode-immediate
;;   :constant-op orr
;;   :constant-transform bic-mask)
;; (define-binop logorc2 2 orn
;;   :constant-test bic-encode-immediate
;;   :constant-fixnum-test bic-fixnum-encode-immediate
;;   :constant-op orr
;;   :constant-transform bic-mask)

(define-vop (fast-logior-unsigned-signed=>signed fast-safe-arith-op)
  (:args (x :scs (unsigned-reg))
         (y :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 1)))
  (:result-types signed-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:translate logior)
  (:generator 3
    (inst orr r x y)))

(define-vop (fast-logior-signed-unsigned=>signed fast-safe-arith-op)
  (:args (x :scs (signed-reg))
         (y :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (r :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:translate logior)
  (:generator 3
    (inst orr r x y)))

;;; Multiplication

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:args (x :scs (any-reg))
         (y :scs (signed-reg immediate))) ;; one operand needs to be untagged
  (:translate *)
  (:generator 2
    (let (value)
      (cond ((and (sc-is y immediate)
                  (= (logcount (abs (setf value (tn-value y)))) 1))
             (let ((shift (1- (integer-length (abs value)))))
               (if (minusp value)
                   (inst neg r (lsl x shift))
                   (inst lsl r x shift))))
            ((and (typep value '(integer 1))
                  (= (logcount (1- value)) 1))
             (inst add r x (lsl x (1- (integer-length (1- value))))))
            ((and (typep value '(integer * -1))
                  (= (logcount (- 1 value)) 1))
             (inst sub r x (lsl x (1- (integer-length (- 1 value))))))
            (t
             (when value
               (load-immediate-word tmp-tn value)
               (setf y tmp-tn))
             (inst mul r x y))))))

(define-vop (fast-*/signed=>signed fast-*/fixnum=>fixnum)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:variant-cost 3))

(define-vop (fast-*/unsigned=>unsigned fast-*/fixnum=>fixnum)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:variant-cost 3))

;;; Division
(define-vop (fast-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg) :to :result)
         (y :scs (signed-reg) :to :result))
  (:arg-types signed-num signed-num)
  (:arg-refs nil y-ref)
  (:results (quo :scs (signed-reg) :from :eval)
            (rem :scs (signed-reg) :from :eval))
  (:optional-results rem)
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (when (types-equal-or-intersect (tn-ref-type y-ref)
                                    (specifier-type '(eql 0)))
      (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
        (inst cbz y zero)))
    (inst sdiv quo x y)
    (unless (eq (tn-kind rem) :unused)
     (inst msub rem quo y x))))

(define-vop (fast-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :to :result))
  (:arg-types unsigned-num unsigned-num)
  (:arg-refs nil y-ref)
  (:results (quo :scs (unsigned-reg) :from :eval)
            (rem :scs (unsigned-reg) :from :eval))
  (:optional-results rem)
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (when (types-equal-or-intersect (tn-ref-type y-ref)
                                    (specifier-type '(eql 0)))
      (let ((zero (generate-error-code vop 'division-by-zero-error x y)))
        (inst cbz y zero)))
    (inst udiv quo x y)
    (unless (eq (tn-kind rem) :unused)
      (inst msub rem quo y x))))

;;;
(define-vop (fast-lognor/fixnum=>fixnum fast-fixnum-binop)
  (:translate lognor)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:generator 3
    (inst orr r x y)
    (inst eor r r (lognot fixnum-tag-mask))))

(define-vop (fast-logand/signed-unsigned=>unsigned fast-logand/unsigned=>unsigned)
  (:args (x :scs (signed-reg))
         (y :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:translate logand))

(defun logical-immediate-or-word-mask (x)
  (and (integerp x)
       (or (encode-logical-immediate x)
           (= x most-positive-word))))

(define-vop (fast-logand-c/signed-unsigned=>unsigned fast-logand-c/unsigned=>unsigned)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (satisfies logical-immediate-or-word-mask)))
  (:translate logand)
  (:generator 2
    (if (= y most-positive-word)
        (move r x)
        (inst and r x y))))

(define-source-transform logeqv (&rest args)
  (if (oddp (length args))
      `(logxor ,@args)
      `(lognot (logxor ,@args))))
(define-source-transform logorc1 (x y)
  `(logior (lognot ,x) ,y))
(define-source-transform logorc2 (x y)
  `(logior ,x (lognot ,y)))

;;; Shifting

(define-vop (fast-ash-right-c/fixnum=>fixnum)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (any-reg)))
  (:info amount)
  (:arg-types tagged-num (:constant (integer * -1)))
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  (:temporary (:sc unsigned-reg :target result) temp)
  (:note "inline ASH")
  (:generator 1
    (inst asr temp number (min (- amount) 63))
    (inst and result temp (bic-mask fixnum-tag-mask))))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg)))
  (:info amount)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline ASH")
  (:generator 3
    (cond ((< -64 amount 64)
           (if (plusp amount)
               (inst lsl result number amount)
               (inst lsr result number (- amount))))
          (t
           (inst mov result 0)))))

(define-vop (fast-ash-c/signed=>signed)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg)))
  (:info amount)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline ASH")
  (:generator 3
    (cond ((< -64 amount 64)
           (if (plusp amount)
               (inst lsl result number amount)
               (inst asr result number (- amount))))
          ((= amount 64)
           (inst mov result 0))
          (t
           (inst asr result number 63)))))

(define-vop (fast-ash/signed/unsigned)
  (:note "inline ASH")
  (:args (number)
         (amount))
  (:results (result))
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) temp)
  (:arg-refs nil amount-ref)
  (:variant-vars variant)
  (:generator 5
    (inst subs temp amount zr-tn)
    (inst b :ge LEFT)
    (inst neg temp temp)
    (cond ((csubtypep (tn-ref-type amount-ref)
                      (specifier-type `(integer -63 *)))
           (ecase variant
             (:signed (inst asr result number temp))
             (:unsigned (inst lsr result number temp))))
          (t
           (inst cmp temp n-word-bits)
           (ecase variant
             (:signed
              ;; Only the first 6 bits count for shifts.
              ;; This sets all bits to 1 if AMOUNT is larger than 63,
              ;; cutting the amount to 63.
              (inst csinv temp temp zr-tn :lo)
              (inst asr result number temp))
             (:unsigned
              (inst csel result number zr-tn :lo)
              (inst lsr result result temp)))))

    (inst b END)
    LEFT
    (cond ((csubtypep (tn-ref-type amount-ref)
                      (specifier-type `(integer * 63)))
           (inst lsl result number temp))
          (t
           (inst cmp temp n-word-bits)
           (inst csel result number zr-tn :lo)
           (inst lsl result result temp)))
    END))

(define-vop (fast-ash-modfx/signed/unsigned=>fixnum)
  (:note "inline ASH")
  (:translate ash-modfx)
  (:args (number :scs (signed-reg unsigned-reg) :to :save)
         (amount :scs (signed-reg) :to :save :target temp))
  (:arg-types (:or signed-num unsigned-num) signed-num)
  (:results (result :scs (any-reg)))
  (:arg-refs nil amount-ref)
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) temp temp-result)
  (:generator 5
    (inst subs temp amount zr-tn)
    (inst b :ge LEFT)
    (inst neg temp temp)
    (cond ((csubtypep (tn-ref-type amount-ref)
                      (specifier-type `(integer -63 *)))
           (sc-case number
             (signed-reg (inst asr temp-result number temp))
             (unsigned-reg (inst lsr temp-result number temp))))
          (t
           (inst cmp temp n-word-bits)
           (sc-case number
             (signed-reg
              ;; Only the first 6 bits count for shifts.
              ;; This sets all bits to 1 if AMOUNT is larger than 63,
              ;; cutting the amount to 63.
              (inst csinv temp temp zr-tn :lo)
              (inst asr temp-result number temp))
             (unsigned-reg
              (inst csel temp-result number zr-tn :lo)
              (inst lsr temp-result temp-result temp)))))

    (inst b END)
    LEFT
    (cond ((csubtypep (tn-ref-type amount-ref)
                      (specifier-type `(integer * 63)))
           (inst lsl temp-result number temp))
          (t
           (inst cmp temp n-word-bits)
           (inst csel temp-result number zr-tn :lo)
           (inst lsl temp-result temp-result temp)))
    END
    (inst lsl result temp-result n-fixnum-tag-bits)))

(define-vop (fast-ash/signed=>signed fast-ash/signed/unsigned)
  (:args (number :scs (signed-reg) :to :save)
         (amount :scs (signed-reg) :to :save :target temp))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:translate ash)
  (:variant :signed))

(define-vop (fast-ash/unsigned=>unsigned fast-ash/signed/unsigned)
  (:args (number :scs (unsigned-reg) :to :save)
         (amount :scs (signed-reg) :to :save))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:variant :unsigned))

(macrolet ((def (name name-c sc-type type result-type cost)
             `(progn
                (define-vop (,name)
                  (:note "inline ASH")
                  (:translate ash)
                  (:args (number :scs (,sc-type))
                         (amount :scs (signed-reg unsigned-reg)))
                  ;; For modular variants
                  (:variant-vars cut)
                  (:arg-types ,type positive-fixnum)
                  (:arg-refs nil amount-ref)
                  (:results (result :scs (,result-type)))
                  (:result-types ,type)
                  (:policy :fast-safe)
                  (:generator ,cost
                    (cond ((and cut
                                (not (csubtypep (tn-ref-type amount-ref)
                                                (specifier-type `(mod ,n-word-bits)))))
                           (inst cmp amount n-word-bits)
                           (cond ((location= amount result)
                                  (inst csel tmp-tn number zr-tn :lo)
                                  (inst lsl result tmp-tn amount))
                                 (t
                                  (inst csel result number zr-tn :lo)
                                  (inst lsl result result amount))))
                          (t
                           (inst lsl result number amount)))))
                (define-vop (,name-c)
                  (:note "inline ASH")
                  (:translate ash)
                  (:args (number :scs (,sc-type)))
                  (:info amount)
                  (:arg-types ,type (:constant unsigned-byte))
                  (:results (result :scs (,result-type)))
                  (:result-types ,type)
                  (:policy :fast-safe)
                  (:generator ,(1- cost)
                              (if (< amount 64)
                                  (inst lsl result number amount)
                                  (inst mov result 0)))))))
  ;; FIXME: There's the opportunity for a sneaky optimization here, I
  ;; think: a FAST-ASH-LEFT-C/FIXNUM=>SIGNED vop.  -- CSR, 2003-09-03
  (def fast-ash-left/fixnum=>fixnum fast-ash-left-c/fixnum=>fixnum any-reg tagged-num any-reg 2)
  (def fast-ash-left/signed=>signed fast-ash-left-c/signed=>signed signed-reg signed-num signed-reg 3)
  (def fast-ash-left/unsigned=>unsigned fast-ash-left-c/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

(define-vop (fast-%ash/right/unsigned)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg))
         (amount :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:generator 4
     (inst lsr result number amount)))

(define-vop (fast-%ash/right/signed)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg))
         (amount :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (result :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:generator 4
    (inst asr result number amount)))

(define-vop (fast-%ash/right/fixnum)
  (:translate %ash/right)
  (:policy :fast-safe)
  (:args (number :scs (any-reg))
         (amount :scs (unsigned-reg) :target temp))
  (:arg-types tagged-num unsigned-num)
  (:results (result :scs (any-reg) :from (:argument 0)))
  (:result-types tagged-num)
  (:temporary (:sc unsigned-reg :target result) temp)
  (:generator 3
    (inst asr temp number amount)
    (inst and result temp (bic-mask fixnum-tag-mask))))

(define-vop (fast-ash-left-modfx/fixnum=>fixnum
             fast-ash-left/fixnum=>fixnum)
  (:variant t)
  (:translate ash-left-modfx))

(define-vop (fast-ash-left-modfx-c/fixnum=>fixnum
             fast-ash-left-c/fixnum=>fixnum)
  (:translate ash-left-modfx))

(define-vop (fast-ash-left-mod64-c/fixnum=>fixnum
             fast-ash-left-c/fixnum=>fixnum)
  (:translate ash-left-mod64))

(define-vop (fast-ash-left-mod64/fixnum=>fixnum
             fast-ash-left/fixnum=>fixnum)
  (:variant t)
  (:translate ash-left-mod64))

(define-vop (fast-ash-left-mod64/unsigned=>unsigned
             fast-ash-left/unsigned=>unsigned)
  (:variant t)
  (:translate ash-left-mod64))

(define-vop (fast-ash-left-mod64-c/unsigned=>unsigned
             fast-ash-left-c/unsigned=>unsigned)
  (:translate ash-left-mod64))

(define-vop (fast-ash-left-mod64-c/unsigned=>unsigned
             fast-ash-c/unsigned=>unsigned)
  (:translate ash-left-mod64))

(define-vop (fast-ash-modfx/signed=>signed
             fast-ash/signed=>signed)
  (:translate ash-modfx))

(define-vop (fast-ash-mod64/signed=>unsigned
             fast-ash/signed=>signed)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash-mod64))

(define-vop (fast-ash-mod64/unsigned=>unsigned
             fast-ash/unsigned=>unsigned)
  (:translate ash-mod64))

;;; Only the lower 6 bits of the shift amount are significant.
(macrolet ((define (translate operation)
             `(define-vop ()
                (:translate ,translate)
                (:note ,(string translate))
                (:policy :fast-safe)
                (:args (num :scs (unsigned-reg))
                       (amount :scs (signed-reg)))
                (:arg-types unsigned-num tagged-num)
                (:results (r :scs (unsigned-reg)))
                (:result-types unsigned-num)
                (:generator 1
                  (inst ,operation r num amount)))))
  (define shift-towards-start lsr)
  (define shift-towards-end   lsl))

(define-vop (signed-byte-64-len)
  (:translate integer-length)
  (:note "inline (signed-byte 64) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target temp))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
  (:generator 30
    (inst cmp arg 0)
    (inst csinv temp arg arg :ge)
    (inst clz temp temp)
    (inst mov res (fixnumize 64))
    (inst sub res res (lsl temp n-fixnum-tag-bits))))

(define-vop (unsigned-byte-64-len)
  (:translate integer-length)
  (:note "inline (unsigned-byte 64) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg) :target temp))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
  (:generator 29
    (inst clz temp arg)
    (inst mov res (fixnumize 64))
    (inst sub res res (lsl temp n-fixnum-tag-bits))))


(define-vop (unsigned-byte-64-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 64) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg any-reg)))
  (:arg-types (:or unsigned-num positive-fixnum))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (double-reg)) v)
  (:variant-vars signed)
  (:generator 29
    (when signed
      (sc-case arg
        (any-reg
         ;; Don't invert the tag bit
         (inst asr res arg n-fixnum-tag-bits)
         (setf arg res))
        (t))
      ;; Invert when negative
      (inst eor res arg (asr arg 63))
      (setf arg res))
    (inst fmov v arg)
    (inst cnt v v :8b)
    ;; GCC uses (inst addv v :b v :8b)
    ;; but clang uses:
    (inst uaddlv v :h v :8b)
    (inst fmov res v)))

(define-vop (signed-byte-64-count unsigned-byte-64-count)
    (:note "inline (signed-byte 64) logcount")
    (:args (arg :scs (signed-reg any-reg)))
    (:arg-types (:or signed-num fixnum))
    (:variant t)
    (:variant-cost 30))

(defknown %%ldb (integer unsigned-byte unsigned-byte) unsigned-byte
  (movable foldable flushable always-translatable))

(defknown %%dpb (integer unsigned-byte unsigned-byte integer) integer
  (movable foldable flushable always-translatable))

;;; Constant folding
(defun %%ldb (integer size posn)
  (%ldb size posn integer))

(deftransform %%ldb ((integer size posn) (unsigned-byte t (constant-arg (integer #.n-word-bits))) *
                     :important nil)
  0)

(deftransform %%ldb ((integer size posn) ((integer * -1) t (constant-arg (integer #.n-word-bits))) *
                     :important nil)
  1)

(defun %%dpb (newbyte size posn integer)
  (%dpb newbyte size posn integer))

(define-vop (ldb-c/fixnum)
  (:translate %%ldb)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num
              (:constant integer) (:constant integer))
  (:info size posn)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 2
    (cond ((<= (+ posn size) n-fixnum-bits)
           (inst ubfm res x (1+ posn) (+ posn size)))
          ((= size 1)
           (inst lsr res x n-fixnum-bits))
          (t
           ;; Can't constrain two constant args to avoid this VOP and
           ;; go to the signed variant, so do it manually.
           (inst asr res x (1+ posn))
           (inst and res res (ash most-positive-word (- size sb-vm:n-word-bits)))))))

(define-vop (ldb-c)
  (:translate %%ldb)
  (:args (x :scs (unsigned-reg signed-reg)))
  (:arg-types (:or unsigned-num signed-num)
              (:constant integer) (:constant integer))
  (:info size posn)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (if (and (>= (+ posn size) n-word-bits)
             (= size 1))
        (inst lsr res x (1- n-word-bits))
        (inst ubfm res x posn (+ posn size -1)))))

(define-vop (dpb-c/fixnum)
  (:translate %%dpb)
  (:args (x :scs (signed-reg) :to :save)
         (y :scs (any-reg)))
  (:arg-types signed-num
              (:constant integer) (:constant integer)
              tagged-num)
  (:info size posn)
  (:results (res :scs (any-reg)))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:generator 2
    (move res y)
    (inst bfm res x (- (1- n-word-bits) posn) (1- size))))

(define-vop (dpb-c/signed)
  (:translate %%dpb)
  (:args (x :scs (signed-reg) :to :save)
         (y :scs (signed-reg)))
  (:arg-types signed-num
              (:constant integer) (:constant integer)
              signed-num)
  (:info size posn)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:generator 3
    (move res y)
    (inst bfm res x (if (= posn 0)
                        0
                        (- n-word-bits posn)) (1- size))))

(define-vop (dpb-c/unsigned)
  (:translate %%dpb)
  (:args (x :scs (unsigned-reg) :to :save)
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num
              (:constant integer) (:constant integer)
              unsigned-num)
  (:info size posn)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 3
    (move res y)
    (inst bfm res x (if (= posn 0)
                        0
                        (- n-word-bits posn)) (1- size))))

;;; Modular functions
(define-modular-fun lognot-mod64 (x) lognot :untagged nil 64)
(define-vop (lognot-mod64/unsigned=>unsigned)
  (:translate lognot-mod64)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (inst mvn res x)))

(defmacro define-mod-binop ((name prototype) function)
  `(define-vop (,name ,prototype)
     (:args (x :scs (unsigned-reg signed-reg))
            (y :scs (unsigned-reg signed-reg)))
     (:arg-types untagged-num untagged-num)
     (:results (r :scs (unsigned-reg signed-reg) :from (:argument 0)))
     (:result-types unsigned-num)
     (:translate ,function)))

(defmacro define-mod-binop-c ((name prototype) function)
  `(define-vop (,name ,prototype)
     (:args (x :scs (unsigned-reg signed-reg)))
     (:info y)
     (:arg-types untagged-num (:constant (satisfies add-sub-immediate-p)))
     (:results (r :scs (unsigned-reg signed-reg) :from (:argument 0)))
     (:result-types unsigned-num)
     (:translate ,function)))

(macrolet ((def (name -c-p)
             (let ((fun64   (symbolicate name "-MOD64"))
                   (funfx   (symbolicate name "-MODFX"))
                   (vopu    (symbolicate "FAST-" name "/UNSIGNED=>UNSIGNED"))
                   (vopcu   (symbolicate "FAST-" name "-C/UNSIGNED=>UNSIGNED"))
                   (vopf    (symbolicate "FAST-" name "/FIXNUM=>FIXNUM"))
                   (vopcf   (symbolicate "FAST-" name "-C/FIXNUM=>FIXNUM"))
                   (vop64u  (symbolicate "FAST-" name "-MOD64/WORD=>UNSIGNED"))
                   (vop64f  (symbolicate "FAST-" name "-MOD64/FIXNUM=>FIXNUM"))
                   (vop64cu (symbolicate "FAST-" name "-MOD64-C/WORD=>UNSIGNED"))
                   (vopfxf  (symbolicate "FAST-" name "-MODFX/FIXNUM=>FIXNUM"))
                   (vopfxcf (symbolicate "FAST-" name "-MODFX-C/FIXNUM=>FIXNUM")))
               `(progn
                  (define-modular-fun ,fun64 (x y) ,name :untagged nil 64)
                  (define-modular-fun ,funfx (x y) ,name :tagged t ,n-fixnum-bits)
                  (define-mod-binop (,vop64u ,vopu) ,fun64)
                  (define-vop (,vop64f ,vopf) (:translate ,fun64))
                  (define-vop (,vopfxf ,vopf) (:translate ,funfx))
                  ,@(when -c-p
                      `((define-mod-binop-c (,vop64cu ,vopcu) ,fun64)
                        (define-vop (,vopfxcf ,vopcf) (:translate ,funfx))))))))
  (def + t)
  (def - t)
  (def * nil))

;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional :eq)
  (:policy :fast-safe))

(defun fixnum-abs-add-sub-immediate-p (n)
  (fixnum-add-sub-immediate-p (abs n)))

(defun abs-add-sub-immediate-p (n)
  ;; In the cross-compiler, stack traces such as the following can occur:
  ;; 0: (HOST-SB-KERNEL:%NEGATE #.(MAKE-DOUBLE-FLOAT #x7FEFFFFF #xFFFFFFFF))
  ;; 1: (SB-VM::ABS-ADD-SUB-IMMEDIATE-P #.(MAKE-DOUBLE-FLOAT #x7FEFFFFF #xFFFFFFFF))
  ;; 2: ((LABELS RECURSE :IN CROSS-TYPEP) #.(MAKE-DOUBLE-FLOAT #x7FEFFFFF #xFFFFFFFF)
  ;;     #<HAIRY-TYPE (SATISFIES SB-VM::ABS-ADD-SUB-IMMEDIATE-P)>)
  ;; 3: (CTYPEP #.(MAKE-DOUBLE-FLOAT #x7FEFFFFF #xFFFFFFFF)
  ;;     #<HAIRY-TYPE (SATISFIES SB-VM::ABS-ADD-SUB-IMMEDIATE-P)>)
  ;; 4: (SB-C::CHECK-ARG-TYPE #<SB-C::LVAR 1 {1008A71AD3}>
  ;;     #<CONSTANT-TYPE (CONSTANT-ARG (SATISFIES SB-VM::ABS-ADD-SUB-IMMEDIATE-P))> 2)
  ;; The target compiler will avoid using this predicate, for the wrong reason.
  ;; It avoids it because it thinks it's not foldable.
  ;; If it did call it, then it would get a bogus (but permissible) answer, namely:
  ;;  (ctypep 0 (specifier-type '(satisfies abs-add-sub-immediate-p))) => NIL,NIL
  (and (integerp n)
       (or (add-sub-immediate-p (abs n))
           (add-sub-immediate-p (ldb (byte n-word-bits 0) (- n))))))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 64) comparison"))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 64) comparison"))

(defmacro define-conditional-vop (tran signed unsigned &optional addend addend-signed addend-unsigned)
  `(progn
     ,@(loop for (suffix cost signed-p) in
             '((/fixnum 4 t)
               (/signed 6 t)
               (/unsigned 6 nil))
             collect
             `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)" tran suffix))
                           ,(intern (format nil "~:@(FAST-CONDITIONAL~A~)" suffix)))
                (:translate ,tran)
                (:conditional ,(if signed-p signed unsigned))
                (:generator ,cost
                  (inst cmp x y))))

     (define-vop (,(symbolicate "FAST-IF-" tran "-INTEGER/C") )
       (:translate ,tran)
       (:args (x :scs (any-reg signed-reg unsigned-reg)))
       (:arg-types (:or tagged-num signed-num unsigned-num)
                   (:constant (or signed-word word)))
       (:info y)
       (:vop-var vop)
       (:policy :fast-safe)
       ,(if (eq signed unsigned)
            `(:conditional ,signed)
            `(:conditional
              :after-sc-selection
              (flet ((try (y)
                       (let ((y (if (sc-is x any-reg)
                                    (fixnumize y)
                                    y)))
                         (flet ((try (constant)
                                  (add-sub-immediate-p constant)))
                           (or (try y)
                               (try (ldb (byte 64 0) (- y))))))))
                (cond ((and (not (try y))
                            (try (+ y ,addend)))
                       (setf (car (vop-codegen-info vop))
                             (+ y ,addend))
                       (if (sc-is x unsigned-reg)
                           ,addend-unsigned
                           ,addend-signed))
                      (t
                       (if (sc-is x unsigned-reg)
                           ,unsigned
                           ,signed))))))
       (:generator 2
         (let ((y (if (sc-is x any-reg)
                      (fixnumize y)
                      y)))
           (flet ((try (constant negate)
                    (when (add-sub-immediate-p constant)
                      (if negate
                          (inst cmn x constant)
                          (inst cmp x constant))
                      t)))
             (or (try y nil)
                 (try (ldb (byte 64 0) (- y)) t)
                 (inst cmp x (load-immediate-word tmp-tn y)))))))))

(define-conditional-vop < :lt :lo -1 :le :ls)
(define-conditional-vop > :gt :hi 1 :ge :hs)
(define-conditional-vop eql :eq :eq)

(define-vop (<-unsigned-signed)
  (:translate <)
  (:args (unsigned :scs (unsigned-reg))
         (signed :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:conditional :lo)
  (:policy :fast-safe)
  (:generator 7
    (inst cmp signed 0)
    (inst ccmp unsigned signed :ge #b0010)))

(define-vop (>-unsigned-signed <-unsigned-signed)
  (:translate >)
  (:conditional :hi))

(define-vop (<-signed-unsigned)
  (:translate <)
  (:args (signed :scs (signed-reg))
         (unsigned :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:conditional :lo)
  (:policy :fast-safe)
  (:generator 7
    (inst cmp signed 0)
    (inst ccmp signed unsigned :ge #b0000)))

(define-vop (>-signed-unsigned <-signed-unsigned)
  (:translate >)
  (:conditional :hi))

(define-vop (eql-unsigned-signed)
  (:translate eql)
  (:args (unsigned :scs (unsigned-reg))
         (signed :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:conditional :eq)
  (:policy :fast-safe)
  (:generator 7
    (inst cmp signed 0)
    (inst ccmp unsigned signed :ge #b0000)))

(define-vop (eql-signed-unsigned eql-unsigned-signed)
  (:args (signed :scs (signed-reg))
         (unsigned :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num))

(define-vop (eq-unsigned-signed eql-unsigned-signed)
  (:variant-cost 6)
  (:translate eq))

(define-vop (eq-signed-unsigned eql-signed-unsigned)
  (:variant-cost 6)
  (:translate eq))

(define-vop (generic-eql/fixnum fast-if-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (generic-eql-c/fixnum fast-if-eql-integer/c)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (satisfies fixnum-add-sub-immediate-p)))
  (:variant-cost 6))

(macrolet ((define-logtest-vops ()
             `(progn
                ,@(loop for suffix in '(/fixnum /signed /unsigned)
                        for cost in '(4 6 6)
                        collect
                        `(define-vop (,(symbolicate "FAST-LOGTEST" suffix)
                                      ,(symbolicate "FAST-CONDITIONAL" suffix))
                           (:translate logtest)
                           (:conditional :ne)
                           (:generator ,cost
                             (inst tst x y)))))))
  (define-logtest-vops))

(define-vop (fast-logtest-c)
  (:translate logtest)
  (:args (x :scs (any-reg signed-reg unsigned-reg)))
  (:arg-types (:or tagged-num signed-num unsigned-num)
              (:constant (or signed-word word)))
  (:info y)
  (:policy :fast-safe)
  (:conditional :ne)
  (:generator 2
    (block nil
      (if (= y most-positive-word)
          (inst cmp x 0)
          (let ((y (if (sc-is x any-reg)
                       (cond ((fixnump y)
                              (fixnumize y))
                             ((let ((y (ldb (byte 64 0) y)))
                                ;; Only a negative fixnum will match that bit,
                                ;; so if the next bit is also 1 it can be shifted left.
                                (when (and (= (ldb (byte 1 62) y) 1)
                                           (encode-logical-immediate
                                            (ldb (byte 64 0) (ash y 1))))
                                  (return (inst tst x (ldb (byte 64 0) (ash y 1)))))))
                             (t
                              (return
                                (inst tst (load-immediate-word tmp-tn y)
                                      (asr x n-fixnum-tag-bits)))))
                       y)))
            (if (encode-logical-immediate y)
                (inst tst x y)
                (inst tst x (load-immediate-word tmp-tn y))))))))

(define-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))

(defknown %logbitp (integer unsigned-byte) boolean
  (movable foldable flushable always-translatable))

;;; For constant folding
(defun %logbitp (integer index)
  (logbitp index integer))

(define-vop ()
  (:translate %logbitp)
  (:policy :fast-safe)
  (:args (x :scs (any-reg signed-reg unsigned-reg)))
  (:info y)
  (:arg-types (:or tagged-num signed-num unsigned-num) (:constant (mod #.n-word-bits)))
  (:conditional :ne)
  (:generator 2
    (inst tst x (ash 1 (min (if (sc-is x any-reg)
                                (+ y n-fixnum-tag-bits)
                                y)
                         (1- n-word-bits))))))

;; Specialised mask-signed-field VOPs.
(define-vop (mask-signed-field-word/c)
  (:translate sb-c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (signed-reg unsigned-reg)))
  (:arg-types (:constant (integer 0 64)) untagged-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:info width)
  (:generator 3
    (aver (/= width 0))
    (cond ((= width 64)
           (move r x))
          (t
           (inst sbfm r x 0 (1- width))))))

(define-vop (mask-signed-field-bignum/c)
  (:translate sb-c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:arg-types (:constant (integer 0 64)) bignum)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:info width)
  (:generator 4
    (aver (/= width 0))
    (loadw r x bignum-digits-offset other-pointer-lowtag)
    (inst sbfm r r 0 (1- width))))

(define-vop (mask-signed-field-fixnum)
  (:translate sb-c::mask-signed-field)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target r))
  (:arg-types (:constant (eql #.n-fixnum-bits)) t)
  (:results (r :scs (any-reg)))
  (:result-types fixnum)
  (:info width)
  (:ignore width)
  (:generator 5
    (move r x)
    (inst tbz r 0 DONE)
    (loadw tmp-tn r bignum-digits-offset other-pointer-lowtag)
    (inst lsl r tmp-tn (- n-word-bits n-fixnum-bits))
    DONE))

(define-vop (logand-word-mask)
  (:translate logand)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:arg-types t (:constant (member #.most-positive-word
                                   #.(ash most-positive-word -1))))
  (:results (r :scs (unsigned-reg)))
  (:info mask)
  (:result-types unsigned-num)
  (:generator 10
    (inst tbnz x 0 BIGNUM)
    (if (= mask most-positive-word)
        (inst asr r x n-fixnum-tag-bits)
        (inst lsr r x n-fixnum-tag-bits))
    (inst b DONE)
    BIGNUM
    (loadw r x bignum-digits-offset other-pointer-lowtag)
    (unless (= mask most-positive-word)
      (inst ubfm r r 0 (- n-word-bits 2)))
    DONE))

;;;; Bignum stuff.

(define-vop (bignum-length get-header-data)
  (:translate sb-bignum:%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate sb-bignum:%bignum-set-length)
  (:policy :fast-safe))

(define-full-reffer bignum-ref * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb-bignum:%bignum-ref)

(define-full-setter bignum-set * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb-bignum:%bignum-set)

(define-partial-reffer sb-c::%half-bignum-ref bignum
  :word nil bignum-digits-offset other-pointer-lowtag (unsigned-reg signed-reg)
  positive-fixnum
  sb-bignum:%half-bignum-ref)

(define-partial-setter sb-c::%half-bignum-set bignum
  :word bignum-digits-offset other-pointer-lowtag (unsigned-reg signed-reg)
  positive-fixnum
  (setf sb-bignum:%half-bignum-ref))


(define-vop (digit-0-or-plus)
  (:translate sb-bignum:%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:conditional)
  (:info target not-p)
  (:generator 2
    (inst cmp digit 0)
    (inst b (if not-p :lt :ge) target)))

(define-vop (add-w/carry)
  (:translate sb-bignum:%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (unsigned-reg any-reg immediate)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
            (carry :scs (unsigned-reg) :from :eval))
  (:optional-results carry)
  (:result-types unsigned-num positive-fixnum)
  (:generator 3
    (cond ((and (sc-is c immediate)
                (zerop (tn-value c)))
           (inst adds result a b))
          (t
           (inst cmp c 1)
           (inst adcs result a b)))
    (unless (eq (tn-kind carry) :unused)
      (inst cset carry :cs))))

(define-vop (sub-w/borrow)
  (:translate sb-bignum:%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
         (b :scs (unsigned-reg))
         (c :scs (unsigned-reg any-reg immediate)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
            (borrow :scs (unsigned-reg) :from :eval))
  (:optional-results borrow)
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (cond ((and (sc-is c immediate)
                (eql (tn-value c) 1))
           (inst subs result a b))
          (t
           (inst cmp c 1)
           (inst sbcs result a b)))

    (unless (eq (tn-kind borrow) :unused)
      (inst cset borrow :cs))))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :to :result)
         (carry-in :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:argument 2))
            (lo :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num unsigned-num)
  (:generator 2
    (inst mul lo x y)
    (inst adds lo lo carry-in)
    (inst umulh hi x y)
    (inst adc hi hi zr-tn)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate sb-bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :target lo)
         (prev :scs (unsigned-reg) :to :result)
         (carry-in :scs (unsigned-reg) :to :result))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from :eval)
            (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 9
    (inst umulh hi x y)
    (inst mul lo x y)
    (inst adds lo lo prev)
    (inst adc hi hi zr-tn)
    (inst adds lo lo carry-in)
    (inst adc hi hi zr-tn)))

(define-vop (bignum-mult)
  (:translate sb-bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (unsigned-reg) :to :result))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from :eval)
            (lo :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num unsigned-num)
  (:generator 1
    (inst umulh hi x y)
    (inst mul lo x y)))

(define-vop (mulhi)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 20
    (inst umulh hi x y)))

(define-vop (mulhi/fx)
  (:translate %multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (any-reg))
         (y :scs (unsigned-reg)))
  (:arg-types positive-fixnum unsigned-num)
  (:temporary (:sc unsigned-reg) temp)
  (:results (hi :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 15
    (inst umulh temp x y)
    (inst and hi temp (bic-mask fixnum-tag-mask))))

(define-vop ()
  (:translate %signed-multiply-high)
  (:policy :fast-safe)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (hi :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 20
    (inst smulh hi x y)))

(define-vop (bignum-lognot lognot-mod64/unsigned=>unsigned)
  (:translate sb-bignum:%lognot))

(define-vop (bignum-floor)
  (:translate sb-bignum:%bigfloor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg) :target rem)
         (div-low :scs (unsigned-reg) :target quo)
         (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg) :from (:argument 1))
            (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 300
    (move rem div-high)
    (move quo div-low)
    (dotimes (i 65)
      (assemble ()
        (inst cmp rem divisor)
        (inst b :cc CC)
        (inst sub rem rem divisor)
        CC
        (inst adcs quo quo quo)
        (unless (= i 64)
          (inst adc rem rem rem))))))

(define-vop (half-bignum-floor bignum-floor)
  (:translate sb-bignum:%half-bigfloor)
  (:args (div-high :scs (unsigned-reg) :to :save)
         (div-low :scs (unsigned-reg) :target x)
         (divisor :scs (unsigned-reg) :to (:result 1)))
  (:temporary (:sc unsigned-reg :from (:argument 1)) x)
  (:generator 30
    (move x div-low)
    (inst bfm x div-high 32 31)
    (inst udiv quo x divisor)
    (inst msub rem quo divisor x)))

(define-vop (signify-digit)
  (:translate sb-bignum:%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (if (sc-is res any-reg)
        (inst lsl res digit n-fixnum-tag-bits)
        (inst mov res digit))))

(define-vop (digit-ashr)
  (:translate sb-bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
         (count :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst asr result digit (sc-case count
                             (immediate
                              (tn-value count))
                             (t count)))))

(define-vop (digit-lshr digit-ashr)
  (:translate sb-bignum:%digit-logical-shift-right)
  (:generator 1
    (inst lsr result digit count)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb-bignum:%ashl)
  (:generator 1
    (inst lsl result digit count)))

(define-vop ()
  (:translate sb-c::fixnum*)
  (:args (x :scs (any-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types tagged-num tagged-num (:constant t))
  (:info type)
  (:temporary (:sc signed-reg) high)
  (:results (r :scs (any-reg) :from :load))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::mul-overflow-error r high)))
      (let ((value (and (sc-is y immediate)
                        (tn-value y))))
        (cond ((and value
                    (plusp value)
                    (= (logcount value) 1))
               (let ((shift (1- (integer-length value))))
                 (inst lsl r x shift)
                 (inst asr high x (- 64 shift))))
              (t
               (when value
                 (load-immediate-word high value)
                 (setf y high))
               (inst mul r x y)
               (inst smulh high x y))))
      (inst cmp high (asr r 63))
      (inst b :ne error))))

(define-vop ()
  (:translate sb-c::signed*)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num (:constant t))
  (:info type)
  (:temporary (:sc signed-reg) high)
  (:results (r :scs (signed-reg) :from :load))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::mul-overflow-error r high)))
      (let ((value (and (sc-is y immediate)
                        (tn-value y))))
        (cond ((and value
                    (plusp value)
                    (= (logcount value) 1))
               (let ((shift (1- (integer-length value))))
                 (inst lsl r x shift)
                 (inst asr high x (- 64 shift))))
              (t
               (when value
                 (load-immediate-word high value)
                 (setf y high))
               (inst mul r x y)
               (inst smulh high x y))))
      (inst cmp high (asr r 63))
      (inst b :ne error))))

(define-vop ()
  (:translate sb-c::unsigned*)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num (:constant t))
  (:info type)
  (:temporary (:sc unsigned-reg) high)
  (:results (r :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::mul-overflow-error r high)))
      (let ((value (and (sc-is y immediate)
                        (tn-value y))))

        (cond ((and value
                    (plusp value)
                    (= (logcount value) 1))
               (let ((shift (1- (integer-length value))))
                 (inst lsl r x shift)
                 (inst lsr high x (- n-word-bits shift))))
              (t
               (when (sc-is y immediate)
                 (load-immediate-word high value)
                 (setf y high))
               (inst mul r x y)
               (inst umulh high x y))))
      (inst cbnz high error))))

(define-vop (*/signed=>integer)
  (:translate *)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num)
  (:temporary (:sc signed-reg) high low)
  (:temporary (:sc signed-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 10
    (let ((value (and (sc-is y immediate)
                      (tn-value y))))
      (cond ((and value
                  (plusp value)
                  (= (logcount value) 1))
             (let ((shift (1- (integer-length value))))
               (inst lsl low x shift)
               (inst asr high x (- 64 shift))))
            (t
             (when value
               (load-immediate-word high value)
               (setf y high))
             (inst mul low x y)
             (inst smulh high x y))))
    (inst mov header (bignum-header-for-length 2))
    (inst cmp high (asr low 63))
    (inst b :ne allocate)
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    #+bignum-assertions
    (inst mov high 0)
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (*/unsigned=>integer)
  (:translate *)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg) high low)
  (:temporary (:sc unsigned-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 12
    (let ((value (and (sc-is y immediate)
                        (tn-value y))))
        (cond ((and value
                    (plusp value)
                    (= (logcount value) 1))
               (let ((shift (1- (integer-length value))))
                 (inst lsl low x shift)
                 (inst lsr high x (- n-word-bits shift))))
              (t
               (when (sc-is y immediate)
                 (load-immediate-word high value)
                 (setf y high))
               (inst mul low x y)
               (inst umulh high x y))))
    (inst mov header (bignum-header-for-length 3))
    (inst tbnz high 63 allocate)
    (inst mov header (bignum-header-for-length 2))
    (inst cbnz high allocate)
    (inst tbnz low 63 allocate)
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (+/signed=>integer)
  (:translate +)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num)
  (:temporary (:sc signed-reg) high low)
  (:temporary (:sc signed-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 10
    (if (sc-is y immediate)
        (let ((y (tn-value y)))
          (if (minusp y)
              (inst subs low x (add-sub-immediate (- y) high))
              (inst adds low x (add-sub-immediate y high))))
        (inst adds low x y))
    (inst csetm high :cs)
    (inst mov header (bignum-header-for-length 2))
    (inst b :vs allocate)
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (-/signed=>integer)
  (:translate -)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num)
  (:temporary (:sc signed-reg) high low)
  (:temporary (:sc signed-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 10
    (if (sc-is y immediate)
        (let ((y (tn-value y)))
          (if (minusp y)
              (inst adds low x (add-sub-immediate (- y) high))
              (inst subs low x (add-sub-immediate y high))))
        (inst subs low x y))
    (inst csetm high :cs)
    (inst mov header (bignum-header-for-length 2))
    (inst b :vs allocate)
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (+/unsigned=>integer)
  (:translate +)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg) high low)
  (:temporary (:sc unsigned-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 10
    (inst adds low x (if (sc-is y immediate)
                         (add-sub-immediate (tn-value y) high)
                         y))
    (inst mov header (bignum-header-for-length 2))
    (inst cset high :cs)
    (inst b :cs allocate)
    (inst b :mi allocate)
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop (-/unsigned=>integer)
  (:translate -)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg) high low)
  (:temporary (:sc unsigned-reg :from (:argument 2)) header)
  (:temporary (:scs (non-descriptor-reg) :offset lr-offset) lr)
  (:results (r :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 10
    (inst subs low x (if (sc-is y immediate)
                         (add-sub-immediate (tn-value y) high)
                         y))
    (inst mov header (bignum-header-for-length 2))
    (inst csetm high :cc)
    (inst b :cc negative)
    (inst b :mi allocate)
    (inst b positive)
    negative
    (inst b :pl allocate)
    positive
    (inst adds r low low)
    (inst b :vc done)
    (inst mov header (bignum-header-for-length 1))
    allocate
    (with-fixed-allocation
        (r lr nil (+ 2 bignum-digits-offset))
      (storew-pair header 0 low bignum-digits-offset tmp-tn)
      (storew high tmp-tn 2))
    DONE))

(define-vop ()
  (:translate sb-c::signed+)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num (:constant t))
  (:info type)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (if (sc-is y immediate)
          (let ((y (tn-value y)))
            (if (minusp y)
                (inst subs r x (add-sub-immediate (- y)))
                (inst adds r x (add-sub-immediate y))))
          (inst adds r x y))
      (inst b :vs error))))

(define-vop ()
  (:translate sb-c::signed-)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg immediate)))
  (:arg-types signed-num signed-num (:constant t))
  (:info type)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (if (sc-is y immediate)
          (let ((y (tn-value y)))
            (if (minusp y)
                (inst adds r x (add-sub-immediate (- y)))
                (inst subs r x (add-sub-immediate y))))
          (inst subs r x y))
      (inst b :vs error))))

(define-vop ()
  (:translate sb-c::unsigned+)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num (:constant t))
  (:info type)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (when (sc-is y immediate)
        (setf y (add-sub-immediate (tn-value y))))
      (inst adds r x y)
      (inst b :cs error))))

(define-vop ()
  (:translate sb-c::unsigned-)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num unsigned-num (:constant t))
  (:info type)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (when (sc-is y immediate)
        (setf y (add-sub-immediate (tn-value y))))
      (inst subs r x y)
      (inst b :cc error))))

(define-vop ()
  (:translate sb-c::unsigned+signed)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (signed-reg immediate)))
  (:arg-types unsigned-num signed-num (:constant t))
  (:info type)
  (:results (r :scs (unsigned-reg)))
  (:temporary (:scs (unsigned-reg) :from (:argument 2)) temp)
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (if (sc-is y immediate)
          (assemble ()
            (let ((value (tn-value y)))
              (setf y (add-sub-immediate (abs value)))
              (cond ((plusp value)
                     (inst adds r x y)
                     (inst b :cs error))
                    (t
                     (inst subs r x y)
                     (inst b :cc error)))))
          (assemble ()
            (when (sc-is y immediate)
              (setf y (add-sub-immediate (tn-value y))))
            (inst tbnz y 63 neg)
            (inst adds r x y)
            (inst b :cs error)
            (inst b done)
            neg
            (inst neg temp y)
            (inst subs r x temp)
            (inst b :cc error)
            done)))))

(define-vop ()
  (:translate sb-c::unsigned-signed)
  (:args (x :scs (unsigned-reg) :to :result)
         (y :scs (signed-reg immediate)))
  (:arg-types unsigned-num signed-num (:constant t))
  (:info type)
  (:results (r :scs (unsigned-reg)))
  (:temporary (:scs (unsigned-reg) :from (:argument 2)) temp)
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let* ((*location-context* (unless (eq type 'fixnum)
                                 type))
           (error (generate-error-code vop 'sb-kernel::add-sub-overflow-error r)))
      (if (sc-is y immediate)
          (assemble ()
            (let ((value (tn-value y)))
              (setf y (add-sub-immediate (abs value)))
              (cond ((minusp value)
                     (inst adds r x y)
                     (inst b :cs error))
                    (t
                     (inst subs r x y)
                     (inst b :cc error)))))
          (assemble ()
            (when (sc-is y immediate)
              (setf y (add-sub-immediate (tn-value y))))
            (inst tbz y 63 pos)
            (inst neg temp y)
            (inst adds r x temp)
            (inst b :cs error)
            (inst b done)
            pos
            (inst subs r x y)
            (inst b :cc error)
            done)))))

(define-vop (signum-signed signed-unop)
  (:args (x :scs (signed-reg any-reg) :target res))
  (:translate signum)
  (:generator 4
    (inst cmp x 0)
    (inst cset tmp-tn :ne)
    (inst csinv res tmp-tn zr-tn :ge)))

(define-vop (signum-unsigned unsigned-unop)
  (:args (x :scs (unsigned-reg any-reg) :target res))
  (:translate signum)
  (:generator 3
    (inst cmp x 0)
    (inst cset res :ne)))

(define-vop ()
  (:translate fastrem-32)
  (:policy :fast-safe)
  (:args (dividend :scs (unsigned-reg))
         (c :scs (unsigned-reg))
         (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (remainder :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 10
    (inst mul temp dividend c)
    (inst and temp temp #xFFFFFFFF) ; drop the high 32 bits, keep the low 32 bits
    (inst mul temp temp divisor)
    (inst lsr remainder temp 32))) ; take the high 32 bits
(define-vop ()
  (:translate fastrem-64)
  (:policy :fast-safe)
  (:args (dividend :scs (unsigned-reg))
         (c :scs (unsigned-reg))
         (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (remainder :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 10
    (inst mul temp dividend c) ; want only the low 64 bits
    (inst umulh remainder temp divisor))) ; want only the high 64 bits
