;;;; predicate functions (EQUAL and friends, and type predicates)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; miscellaneous non-primitive predicates

(declaim (inline streamp))
(defun streamp (stream)
  (typep stream 'stream))

;;; These would only be called from %%TYPEP given a built-in-classoid.
;;; Ordinarily TYPEP on either one would be transformed.
(defun sb-kernel::file-stream-p (x) (typep x 'file-stream))
(defun sb-kernel::string-stream-p (x) (typep x 'string-stream))

;;; various (VECTOR FOO) type predicates, not implemented as simple
;;; widetag tests
(macrolet
    ((def ()
       `(progn
          ,@(loop for (name spec) in *vector-without-complex-typecode-infos*
                  collect `(defun ,name (x)
                             (or (typep x '(simple-array ,spec (*)))
                                 (and (complex-vector-p x)
                                      (do ((data (%array-data x) (%array-data data)))
                                          ((not (array-header-p data)) (typep data '(simple-array ,spec (*))))))))))))
  (def))

;;; Is X an extended sequence?
(declaim (maybe-inline extended-sequence-p))
(defun extended-sequence-p (sb-c::object) ; name the argugment as required by transform
  (macrolet ((transform ()
               (sb-c::transform-instance-typep (find-classoid 'sequence))))
    (transform)))

;;; Is X a SEQUENCE?  Harder than just (OR VECTOR LIST)
(defun sequencep (x)
  (declare (inline extended-sequence-p))
  (or (listp x) (vectorp x) (extended-sequence-p x)))

;;;; primitive predicates. These must be supported directly by the
;;;; compiler.

(defun not (object)
  "Return T if X is NIL, otherwise return NIL."
  (not object))

;;; All the primitive type predicate wrappers share a parallel form..
(macrolet ((def-type-predicate-wrapper (pred)
             (let* ((name (symbol-name pred))
                    (stem (string-left-trim "%" (string-right-trim "P-" name)))
                    (article (if (position (schar name 0) "AEIOU") "an" "a")))
               `(defun ,pred (object)
                  ;; Document the standardized predicates and not the internal ones.
                  ,@(when (eql (sb-xc:symbol-package pred) *cl-package*)
                      (list (format nil
                                    "Return true if OBJECT is ~A ~A, and NIL otherwise."
                                    article
                                    stem)))
                  ;; (falling through to low-level implementation)
                  (,pred object)))))
  (def-type-predicate-wrapper array-header-p)
  (def-type-predicate-wrapper simple-array-header-p)
  (def-type-predicate-wrapper arrayp)
  (def-type-predicate-wrapper atom)
  ;; Testing for BASE-CHAR-P is usually redundant on #-sb-unicode,
  ;; remove it there completely so that #-sb-unicode build will
  ;; break when it's used.
  #+sb-unicode (def-type-predicate-wrapper base-char-p)
  (def-type-predicate-wrapper base-string-p)
  #+sb-unicode (def-type-predicate-wrapper character-string-p)
  (def-type-predicate-wrapper bignump)
  (def-type-predicate-wrapper bit-vector-p)
  (def-type-predicate-wrapper characterp)
  (def-type-predicate-wrapper code-component-p)
  (def-type-predicate-wrapper consp)
  (def-type-predicate-wrapper compiled-function-p)
  (def-type-predicate-wrapper complexp)
  (def-type-predicate-wrapper complex-double-float-p)
  (def-type-predicate-wrapper complex-float-p)
  #+long-float (def-type-predicate-wrapper complex-long-float-p)
  (def-type-predicate-wrapper complex-rational-p)
  (def-type-predicate-wrapper complex-single-float-p)
  ;; (COMPLEX-VECTOR-P is not included here since it's awkward to express
  ;; the type it tests for in the Common Lisp type system, and since it's
  ;; only used in the implementation of a few specialized things.)
  (def-type-predicate-wrapper double-float-p)
  (def-type-predicate-wrapper fdefn-p)
  (def-type-predicate-wrapper fixnump)
  (def-type-predicate-wrapper floatp)
  (def-type-predicate-wrapper functionp)
  ;; SIMPLE-FUN-P is needed for constant folding in early warm load,
  ;; and its absence would be obscured by the fact that
  ;; CONSTANT-FUNCTION-CALL-P allows the call to fail.
  (def-type-predicate-wrapper closurep)
  (def-type-predicate-wrapper simple-fun-p)
  (def-type-predicate-wrapper integerp)
  (def-type-predicate-wrapper listp)
  (def-type-predicate-wrapper long-float-p)
  #-(or x86 x86-64 arm64 riscv) (def-type-predicate-wrapper lra-p)
  (def-type-predicate-wrapper null)
  (def-type-predicate-wrapper numberp)
  (def-type-predicate-wrapper rationalp)
  (def-type-predicate-wrapper ratiop)
  (def-type-predicate-wrapper realp)
  (def-type-predicate-wrapper short-float-p)
  (def-type-predicate-wrapper single-float-p)
  #+sb-simd-pack (def-type-predicate-wrapper simd-pack-p)
  #+sb-simd-pack-256 (def-type-predicate-wrapper simd-pack-256-p)
  #+sb-simd-pack-512 (def-type-predicate-wrapper simd-pack-512-p)
  (def-type-predicate-wrapper %instancep)
  (def-type-predicate-wrapper funcallable-instance-p)
  (def-type-predicate-wrapper symbolp)
  ;; The interpreter needs this because it assumes that any type spec
  ;; in SB-C::*BACKEND-TYPE-PREDICATES* has a callable predicate.
  (def-type-predicate-wrapper non-null-symbol-p)
  (def-type-predicate-wrapper %other-pointer-p)
  (def-type-predicate-wrapper system-area-pointer-p)
  (def-type-predicate-wrapper unbound-marker-p)
  (def-type-predicate-wrapper weak-pointer-p)

  (sb-c::when-vop-existsp (:translate signed-byte-8-p)
    (def-type-predicate-wrapper signed-byte-8-p))
  (sb-c::when-vop-existsp (:translate signed-byte-16-p)
    (def-type-predicate-wrapper signed-byte-16-p))
  (sb-c::when-vop-existsp (:translate signed-byte-32-p)
    (def-type-predicate-wrapper signed-byte-32-p))
  #-64-bit
  (def-type-predicate-wrapper unsigned-byte-32-p)
  #+64-bit
  (progn
    (def-type-predicate-wrapper unsigned-byte-64-p)
    (def-type-predicate-wrapper signed-byte-64-p))
  ;; Specialized array types
  (macrolet ((saetp-defs ()
               `(progn
                  ,@(map 'list
                         (lambda (saetp)
                           `(def-type-predicate-wrapper
                                ,(symbolicate (sb-vm:saetp-primitive-type-name saetp) "-P")))
                         sb-vm:*specialized-array-element-type-properties*))))
    (saetp-defs))
  ;; Other array types
  (def-type-predicate-wrapper simple-array-p)
  (def-type-predicate-wrapper simple-rank-1-array-*-p)
  (def-type-predicate-wrapper simple-string-p)
  (def-type-predicate-wrapper stringp)
  (def-type-predicate-wrapper vectorp))

(sb-c::when-vop-existsp (:translate fixnum-mod-p)
  (defun fixnum-mod-p (x limit)
    (and (fixnump x)
         (<= 0 x limit))))

(sb-c::when-vop-existsp (:translate car-eq-if-listp)
  (defun car-eq-if-listp (value object)
    (car-eq-if-listp value object)))


;;; Return the specifier for the type of object. This is not simply
;;; (TYPE-SPECIFIER (CTYPE-OF OBJECT)) because CTYPE-OF has different
;;; goals than TYPE-OF. In particular, speed is more important than
;;; precision here, and it is not permitted to return member types,
;;; negation, union, or intersection types.
(defun type-of (object)
  "Return the type of OBJECT."
  (declare (explicit-check))
  ;; We have special logic for everything except arrays.
  ;; Arrays use CTYPE-OF and then convert the answer to a specifier.
  (typecase object
    (fixnum
     (cond
       ((<= 0 object 1) 'bit)
       ((< object 0) 'fixnum)
       (t `(integer 0 ,most-positive-fixnum))))
    (integer
     (if (>= object 0)
         `(integer ,(1+ most-positive-fixnum))
         'bignum))
    (character
     (typecase object
       (standard-char 'standard-char)
       (base-char 'base-char)
       (extended-char 'extended-char)))
    ;; We "have to" (or have chosen to) pick off KEYWORD and BOOLEAN,
    ;; so we may as well have a branch that returns early for any SYMBOL
    ;; rather than falling into the CLASSOID-based test. But then since we
    ;; do that, we also have to pick off NIL so that it doesn't say SYMBOL.
    (symbol
     (cond ((eq object t) 'boolean)
           ((eq object nil) 'null)
           ((eq (sb-xc:symbol-package object) *keyword-package*) 'keyword)
           (t 'symbol)))
    (array
     (let ((etype (sb-vm::array-element-ctype object)))
       ;; Obviously :COMPLEXP is known to be T or NIL, but it's not allowed to
       ;; return (NOT SIMPLE-ARRAY), so use :MAYBE in lieu of T.
       (type-specifier
        (make-array-type (array-dimensions object)
                         :complexp (if (typep object 'simple-array) nil :maybe)
                         :element-type etype
                         :specialized-element-type etype))))
    ((or complex #+sb-simd-pack simd-pack #+sb-simd-pack-256 simd-pack-256 #+sb-simd-pack-512 simd-pack-512)
     (type-specifier (ctype-of object)))
    (simple-fun 'compiled-function)
    (t
     #+metaspace ; WRAPPER-OF can't be called on layoutless objects.
     (unless (logtest (get-lisp-obj-address (%instanceoid-layout object))
                      sb-vm:widetag-mask)
       ;; [fun-]instances momentarily have no layout in any code interrupted
       ;; just after allocating and before assigning slots.
       ;; OUTPUT-UGLY-OBJECT has a similar precaution as this.
       (return-from type-of (if (functionp object) 'funcallable-instance 'instance)))
     (let ((wrapper (wrapper-of object)))
       #-metaspace ; already checked for a good layout if metaspace
       (when (= (get-lisp-obj-address wrapper) 0)
         (return-from type-of
           (if (functionp object) 'funcallable-instance 'instance)))
       (let* ((classoid (wrapper-classoid wrapper))
              (name (classoid-name classoid)))
         ;; FIXME: should the first test be (not (or (%instancep) (%funcallable-instance-p)))?
         ;; God forbid anyone makes anonymous classes of generic functions.
         (cond ((not (%instancep object))
                name)
               ((eq name 'sb-alien-internals:alien-value)
                `(alien ,(sb-alien-internals:unparse-alien-type
                          (sb-alien-internals:alien-value-type object))))
               (t
                (let ((pname (classoid-proper-name classoid)))
                  (if (classoid-p pname)
                      (classoid-pcl-class pname)
                      pname)))))))))

;;;; equality predicates

;;; This is real simple, 'cause the compiler takes care of it.
(defun eq (obj1 obj2)
  "Return T if OBJ1 and OBJ2 are the same object, otherwise NIL."
  (eq obj1 obj2))
;;; and this too, but it's only needed for backends on which
;;; IR1 might potentially transform EQL into %EQL/INTEGER.
#+integer-eql-vop
(defun %eql/integer (obj1 obj2)
  ;; This is just for constant folding, there's no real need to transform
  ;; into the %EQL/INTEGER VOP. But it's confusing if it becomes identical to
  ;; EQL, and then due to ICF we find that #'EQL => #<FUNCTION %EQL/INTEGER>.
  ;; Type declarations don't suffice because we don't know which arg is an integer
  ;; (if not both). We could ensure selection of the vop by using %PRIMITIVE.
  ;; Anyway it suffices to disable type checking and pretend its the always
  ;; the first arg that's an integer, but that won't work on the host because
  ;; it might enforce the type since we can't portably unenforce after declaring.
  (declare (optimize (sb-c::type-check 0)))
  (eql (the integer obj1) obj2))

(declaim (inline %eql))
(defun %eql (obj1 obj2)
  "Return T if OBJ1 and OBJ2 represent the same object, otherwise NIL."
  #+x86-64 (eql obj1 obj2) ; vop fully implements all cases of EQL
  #-x86-64 ; else this is the only full implementation of EQL
  (or (eq obj1 obj2)
      (if (or (typep obj2 'fixnum)
              (not (typep obj2 'number)))
          nil
          (macrolet ((foo (&rest stuff)
                       `(typecase obj2
                          ,@(mapcar (lambda (foo)
                                      (let ((type (car foo))
                                            (fn (cadr foo)))
                                        `(,type
                                          (and (typep obj1 ',type)
                                               (,fn obj1 obj2)))))
                                    stuff))))
            (foo
             (single-float eql)
             (double-float eql)
             #+long-float
             (long-float eql)
             (bignum
              #-integer-eql-vop (lambda (x y) (zerop (bignum-compare x y)))
              #+integer-eql-vop eql) ; will become %eql/integer
             (ratio
              (lambda (x y)
                (and (eql (numerator x) (numerator y))
                     (eql (denominator x) (denominator y)))))
             ((complex single-float)
              (lambda (x y)
                (and (eql (realpart x) (realpart y))
                     (eql (imagpart x) (imagpart y)))))
             ((complex double-float)
              (lambda (x y)
                (and (eql (realpart x) (realpart y))
                     (eql (imagpart x) (imagpart y)))))
             ((complex rational)
              (lambda (x y)
                (and (eql (realpart x) (realpart y))
                     (eql (imagpart x) (imagpart y))))))))))

(defun eql (x y)
  ;; On x86-64, EQL is just an interpreter stub for a vop.
  ;; For others it's a call to the implementation of generic EQL.
  (#+x86-64 eql #-x86-64 %eql x y))

(defun bit-vector-= (x y)
  (declare (type bit-vector x y))
  (cond ((eq x y))
        ((and (simple-bit-vector-p x)
              (simple-bit-vector-p y))
         (bit-vector-= x y))            ; DEFTRANSFORM
        (t
         (and (= (length x) (length y))
              (with-array-data ((x x) (start-x) (end-x) :force-inline t
                                                        :check-fill-pointer t)
                (with-array-data ((y y) (start-y) (end-y) :force-inline t
                                                          :check-fill-pointer t)
                  (declare (ignore end-y))
                  (loop for x-i fixnum from start-x below end-x
                        for y-i fixnum from start-y
                        always (or (= (sbit x x-i)
                                      (sbit y y-i))))))))))

(defun equal (x y)
  "Return T if X and Y are EQL or if they are structured components whose
elements are EQUAL. Strings and bit-vectors are EQUAL if they are the same
length and have identical components. Other arrays must be EQ to be EQUAL."
  ;; Non-tail self-recursion implemented with a local auxiliary function
  ;; is a lot faster than doing it the straightforward way (at least
  ;; on x86oids) due to calling convention differences. -- JES, 2005-12-30
  (labels ((equal-aux (x y)
             (cond ((%eql x y)
                    t)
                   ((consp x)
                    (and (consp y)
                         (equal-aux (car x) (car y))
                         (equal-aux (cdr x) (cdr y))))
                   ((stringp x)
                    (and (stringp y) (string= x y)))
                   ;; We could remove this case by ensuring that MAKE-PATHNAME,
                   ;; PARSE-NAMESTRING, MERGE-PATHNAME, etc look in a weak hash-based
                   ;; thing first for an EQUAL pathname, ensuring that if two pathnames
                   ;; are EQUAL then they are EQ. That would elide this test at the
                   ;; expense of pathname construction which seems like a good tradeoff.
                   ((pathnamep x)
                    (and (pathnamep y) (pathname= x y)))
                   ((bit-vector-p x)
                    (and (bit-vector-p y)
                         (bit-vector-= x y)))
                   (t nil))))
    ;; Use MAYBE-INLINE to get the inline expansion only once (instead
    ;; of 200 times with INLINE). -- JES, 2005-12-30
    (declare (maybe-inline equal-aux))
    (equal-aux x y)))

;;; Like EQUAL, but any two gensyms whose names are STRING= are equalish.
(defun fun-names-equalish (x y)
  (named-let recurse ((x x) (y y))
    (cond ((eql x y) t) ; not performance-critical: don't inline %EQL here
          ((consp x) (and (consp y)
                          (recurse (car x) (car y))
                          (recurse (cdr x) (cdr y))))
          ((and (symbolp x) (not (sb-xc:symbol-package x)))
           (and (symbolp y) (not (sb-xc:symbol-package y)) (string= x y)))
          (t
           (equal x y)))))

;;; EQUALP comparison of HASH-TABLE values
;;; Can be called only if both X and Y are definitely hash-tables.
(defun hash-table-equalp (x y)
  (declare (type hash-table x y) (explicit-check))
  (or (eq x y)
      (and (eql (hash-table-count x) (hash-table-count y))
           (eql (hash-table-test x) (hash-table-test y))
           (block comparison-of-entries
             (maphash (lambda (key x-value)
                        (multiple-value-bind (y-value y-value-p)
                            (gethash key y)
                          (unless (and y-value-p (equalp x-value y-value))
                            (return-from comparison-of-entries nil))))
                      x)
             t))))

(macrolet ((slot-ref-equalp ()
             `(let ((x-el (%instance-ref x i))
                    (y-el (%instance-ref y i)))
                (or (eq x-el y-el) (equalp x-el y-el)))))
(defun instance-equalp (x y)
  (declare (optimize (safety 0)))
  (loop for i downfrom (1- (%instance-length x)) to sb-vm:instance-data-start
        always (slot-ref-equalp)))
(defun instance-equalp* (comparators x y)
  (declare (optimize (safety 0))
           (simple-vector comparators)
           (type instance x y))
  ;; See remark at the source code for %TARGET-DEFSTRUCT
  ;; explaining how to use the vector of comparators.
  (loop for i downfrom (1- (%instance-length x)) to sb-vm:instance-data-start
        for test = (data-vector-ref comparators (- i sb-vm:instance-data-start))
        always (cond ((eql test 0) (slot-ref-equalp))
                     ((functionp test) (funcall test i x y))
                     (t)))))

(macrolet ((numericp (v)
             (let ((widetags
                    (map 'list #'sb-vm:saetp-typecode
                         (remove-if (lambda (x)
                                      (not (typep (sb-vm:saetp-ctype x) 'numeric-type)))
                                    sb-vm:*specialized-array-element-type-properties*))))
               `(%other-pointer-subtype-p ,v ',widetags)))
           (compare-loop (typespec)
             `(let ((x (truly-the (simple-array ,typespec 1) x))
                    (y (truly-the (simple-array ,typespec 1) y)))
                (loop for x-i fixnum from start-x below end-x
                      for y-i fixnum from start-y
                      always (= (aref x x-i) (aref y y-i))))))
(defun array-equalp (a b)
  (flet
      ((data-vector-compare (x y start-x end-x start-y)
         (declare (index start-x end-x start-y)
                  (optimize (sb-c:insert-array-bounds-checks 0)))
         (let ((xtag (%other-pointer-widetag (truly-the (simple-array * 1) x)))
               (ytag (%other-pointer-widetag (truly-the (simple-array * 1) y))))
           (case (if (= xtag ytag) xtag 0)
             (#.sb-vm:simple-vector-widetag
              (let ((x (truly-the simple-vector x))
                    (y (truly-the simple-vector y)))
                (loop for x-i fixnum from start-x below end-x
                      for y-i fixnum from start-y
                      always (let ((a (svref x x-i)) (b (svref y y-i)))
                               (or (eq a b) (equalp a b))))))
             ;; Special-case the important array types that would cause consing in aref.
             ;; Though (UNSIGNED-BYTE 62) and (UNSIGNED-BYTE 63) arrays exist,
             ;; I highly doubt that they occur anywhere except in contrived code
             ;; that does nothing but test that those types exist. i.e. They are
             ;; beyond worthless, and are frankly wasteful of space in array dispatch
             ;; scenarios, or to put it mildy: I disagree with their existence per se.
             #+64-bit (#.sb-vm:simple-array-unsigned-byte-64-widetag
                       (compare-loop (unsigned-byte 64)))
             #+64-bit (#.sb-vm:simple-array-signed-byte-64-widetag
                       (compare-loop (signed-byte 64)))
             #-64-bit (#.sb-vm:simple-array-unsigned-byte-32-widetag
                       (compare-loop (unsigned-byte 32)))
             #-64-bit (#.sb-vm:simple-array-signed-byte-32-widetag
                       (compare-loop (signed-byte 32)))
             ;; SINGLE-FLOAT wouldn't cons on 64-bit, but it should be treated
             ;; no less efficiently than DOUBLE-FLOAT.
             (#.sb-vm:simple-array-single-float-widetag
              (compare-loop single-float))
             (#.sb-vm:simple-array-double-float-widetag
              (compare-loop double-float))
             (#.sb-vm:simple-array-complex-single-float-widetag
              (compare-loop (complex single-float)))
             (#.sb-vm:simple-array-complex-double-float-widetag
              (compare-loop (complex double-float)))
             (t
              (let* ((reffers %%data-vector-reffers%%)
                     (getter-x (truly-the function (svref reffers xtag)))
                     (getter-y (truly-the function (svref reffers ytag))))
                ;; The arrays won't both be strings, because EQUALP has a case for that.
                ;; If they're both numeric, use = as the test.
                (if (and (numericp x) (numericp y))
                    (loop for x-i fixnum from start-x below end-x
                          for y-i fixnum from start-y
                          always (= (funcall getter-x x x-i) (funcall getter-y y y-i)))
                    ;; Everything else
                    (loop for x-i fixnum from start-x below end-x
                          for y-i fixnum from start-y
                          for x-el = (funcall getter-x x x-i)
                          for y-el = (funcall getter-y y y-i)
                          always (or (eq x-el y-el)
                                     (equalp x-el y-el))))))))))
    (if (vectorp (truly-the array a))
        (and (vectorp (truly-the array b))
             (= (length a) (length b))
             (with-array-data ((x a) (start-x) (end-x)
                               :force-inline t :check-fill-pointer t)
               (with-array-data ((y b) (start-y) (end-y)
                                 :force-inline t :check-fill-pointer t)
                 (declare (ignore end-y))
                 (data-vector-compare x y start-x end-x start-y))))
        (let ((rank (array-rank (truly-the array a))))
          (and (= rank (array-rank (truly-the array b)))
               (dotimes (axis rank t)
                 (unless (= (%array-dimension a axis)
                            (%array-dimension b axis))
                   (return nil)))
               (with-array-data ((x a) (start-x) (end-x)
                                 :force-inline t :array-header-p t)
                 (with-array-data ((y b) (start-y) (end-y)
                                   :force-inline t :array-header-p t)
                   (declare (ignore end-y))
                   (data-vector-compare x y start-x end-x start-y)))))))))

(defun equalp (x y)
  "Just like EQUAL, but more liberal in several respects.
  Numbers may be of different types, as long as the values are identical
  after coercion. Characters may differ in alphabetic case. Vectors and
  arrays must have identical dimensions and EQUALP elements, but may differ
  in their type restriction. The elements of structured components
  are compared with EQUALP."
  (cond ((eq x y) t)
        ((characterp x) (and (characterp y) (char-equal x y)))
        ((numberp x) (and (numberp y) (= x y)))
        ((consp x)
         (and (consp y)
              (equalp (car x) (car y))
              (equalp (cdr x) (cdr y))))
        ((%instancep x)
         (and (%instancep y)
              (let ((layout (%instance-layout x)))
                (and (logtest (logior +structure-layout-flag+ +pathname-layout-flag+)
                              (layout-flags layout))
                     (eq (%instance-layout y) layout)
                     (funcall (wrapper-equalp-impl (layout-friend layout))
                              x y)))))
        ((arrayp x)
         (and (arrayp y)
              ;; string-equal is nearly 2x the speed of array-equalp for comparing strings
              (cond ((and (stringp x) (stringp y)) (string-equal x y))
                    ((and (bit-vector-p x) (bit-vector-p y)) (bit-vector-= x y))
                    (t (array-equalp x y)))))
        (t nil)))

#-sb-show ;; I don't know why these tests crash with #+sb-show
(let ((test-cases `(($0.0 $-0.0 t)
                    ($0.0 $1.0 nil)
                    ;; There is no cross-compiler #C reader macro.
                    ;; SB-XC:COMPLEX does not want uncanonical input, i.e. imagpart
                    ;; of rational 0 which downgrades the result to just an integer.
                    (1 ,(complex $1.0 $0.0) t)
                    (,(complex 0 1) ,(complex $0.0 $1.0) t)
                    ;; 11/10 is unequal to real 1.1 due to roundoff error.
                    ;; COMPLEX here is a red herring
                    (,(complex $1.1 $0.0) 11/10 nil)
                    ("Hello" "hello" t)
                    ("Hello" #(#\h #\E #\l #\l #\o) t)
                    ("Hello" "goodbye" nil))))
  (dolist (test-case test-cases)
    (destructuring-bind (x y expected-result) test-case
      (let* ((result (equalp x y))
             (bresult (if result 1 0))
             (expected-bresult (if expected-result 1 0)))
        (unless (= bresult expected-bresult)
          ;; If a test fails, there's a chance of getting into a recursive error here
          ;; because, among other things, *BASE-CHAR-NAME-ALIST* has not been filled in,
          ;; so then you're get into an error printing#(#\h #\E #\l #\l #\o).
          ;; Hopefully the write-string calls will work though.
          (progn
            (write-string "test failed: ")
            (write (get-lisp-obj-address x) :base 16 :radix t)
            (write-char #\space)
            (write (get-lisp-obj-address y) :base 16 :radix t)
            (terpri))
          (error "failed test (EQUALP ~S ~S)" x y))))))
