;;;; This file contains the definition of non-CLASS types (e.g.
;;;; subtypes of interesting BUILT-IN-CLASSes) and the interfaces to
;;;; the type system. Common Lisp type specifiers are parsed into a
;;;; somewhat canonical internal type representation that supports
;;;; type union, intersection, etc.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(!begin-collecting-cold-init-forms)

;;; ### Remaining incorrectnesses:
;;;
;;; There are all sorts of nasty problems with open bounds on FLOAT
;;; types (and probably FLOAT types in general.)

;;; This condition is signalled whenever we make a UNKNOWN-TYPE so that
;;; compiler warnings can be emitted as appropriate.
(define-condition parse-unknown-type (condition)
  ((specifier :reader parse-unknown-type-specifier :initarg :specifier))
  (:default-initargs
   :specifier (missing-arg)))

;;; This condition is signalled whenever we encounter a type (DEFTYPE,
;;; structure, condition, class) that has been marked as deprecated.
(define-condition parse-deprecated-type (condition)
  ((specifier :reader parse-deprecated-type-specifier :initarg :specifier))
  (:default-initargs
   :specifier (missing-arg)))

;;; For-effect-only variant of CHECK-DEPRECATED-THING for
;;; type-specifiers that descends into compound type-specifiers.
(defun sb-impl::%check-deprecated-type (type-specifier)
  (let ((seen '()))
    ;; KLUDGE: we have to use SPECIFIER-TYPE to sanely traverse
    ;; TYPE-SPECIFIER and detect references to deprecated types. But
    ;; then we may have to drop its cache to get the
    ;; PARSE-DEPRECATED-TYPE condition when TYPE-SPECIFIER is parsed
    ;; again later.
    ;;
    ;; Proper fix would be a
    ;;
    ;;   walk-type function type-specifier
    ;;
    ;; mechanism that could drive VALUES-SPECIFIER-TYPE but also
    ;; things like this function.
    (block nil
      (handler-bind
          ((parse-deprecated-type
             (lambda (condition)
               (let ((type-specifier (parse-deprecated-type-specifier condition)))
                 (aver (symbolp type-specifier))
                 (unless (memq type-specifier seen)
                   (push type-specifier seen)
                   (check-deprecated-thing 'type type-specifier)))))
           ((or error parse-unknown-type)
             (lambda (condition)
               (declare (ignore condition))
               (return))))
        (specifier-type type-specifier)))))

(defun check-slot-type-specifier (specifier slot-name context)
  ;; This signals an error for malformed type specifiers and
  ;; deprecation warnings for deprecated types but does nothing for
  ;; unknown types.
  (with-current-source-form (specifier)
    (handler-case
        (and (specifier-type specifier)
             (sb-impl::%check-deprecated-type specifier))
      (parse-unknown-type (c)
        (when (typep specifier '(cons (eql quote)))
          (signal c)))
      (error (condition)
        (destructuring-bind (operator . class-name) context
          (sb-c:compiler-warn "Invalid :TYPE for slot ~S in ~S ~S: ~A."
                              slot-name operator class-name condition))))))

(defun maybe-reparse-specifier (type)
  (when (unknown-type-p type)
    (let* ((spec (unknown-type-specifier type))
           (name (if (consp spec)
                     (car spec)
                     spec)))
      (when (info :type :kind name)
        (let ((new-type (specifier-type spec)))
          (unless (unknown-type-p new-type)
            new-type))))))

;;; Evil macro.
(defmacro maybe-reparse-specifier! (type)
  (aver (symbolp type))
  (with-unique-names (new-type)
    `(let ((,new-type (maybe-reparse-specifier ,type)))
       (when ,new-type
         (setf ,type ,new-type)
         t))))

;;; These functions are used as method for types which need a complex
;;; subtypep method to handle some superclasses, but cover a subtree
;;; of the type graph (i.e. there is no simple way for any other type
;;; class to be a subtype.) There are always still complex ways,
;;; namely UNION and MEMBER types, so we must give TYPE1's method a
;;; chance to run, instead of immediately returning NIL, T.
(defun delegate-complex-subtypep-arg2 (type1 type2)
  (let ((subtypep-arg1
         (type-class-complex-subtypep-arg1 (type-class type1))))
    (if subtypep-arg1
        (funcall subtypep-arg1 type1 type2)
        (values nil t))))
(defun delegate-complex-intersection2 (type1 type2)
  (let ((method (type-class-complex-intersection2 (type-class type1))))
    (if (and method (not (eq method #'delegate-complex-intersection2)))
        (funcall method type2 type1)
        (hierarchical-intersection2 type1 type2))))

(defun map-type (function ctype)
  (declare (type (or ctype null) ctype)
           (dynamic-extent function))
  (named-let %map ((type ctype))
    (funcall function type)
    (typecase type
      (compound-type
       (mapc #'%map (compound-type-types type)))
      (negation-type (%map (negation-type-type type)))
      (cons-type
       (%map (cons-type-car-type type))
       (%map (cons-type-cdr-type type)))
      (array-type
       (%map (array-type-element-type type)))
      (args-type
       (mapc #'%map (args-type-required type))
       (mapc #'%map (args-type-optional type))
       (when (args-type-rest type)
         (%map (args-type-rest type)))
       (mapc (lambda (x) (%map (key-info-type x)))
             (args-type-keywords type))
       (when (fun-type-p type)
         (%map (fun-type-returns type))))))
  nil)

(defun contains-unknown-type-p (ctype)
  (map-type (lambda (type)
              (when (unknown-type-p type)
                (return-from contains-unknown-type-p t)))
            ctype))

(defun contains-hairy-type-p (ctype)
  (map-type (lambda (type)
              (when (hairy-type-p type)
                (return-from contains-hairy-type-p t)))
            ctype))

(defun replace-hairy-type (type)
  (if (contains-hairy-type-p type)
      (typecase type
        (hairy-type *universal-type*)
        (intersection-type (%type-intersection
                            (mapcar #'replace-hairy-type (intersection-type-types type))))
        (union-type (%type-union
                     (mapcar #'replace-hairy-type (union-type-types type))))
        (negation-type
         (let ((new (replace-hairy-type (negation-type-type type))))
           (if (eq new *universal-type*)
               new
               (type-negation new))))
        (t
         *universal-type*))
      type))

;; Similar to (NOT CONTAINS-UNKNOWN-TYPE-P), but report that (SATISFIES F)
;; is not a testable type unless F is currently bound.
(defun testable-type-p (ctype)
  (map-type
   (lambda (ctype)
     (typecase ctype
       (unknown-type
        (return-from testable-type-p nil)) ; must precede HAIRY because an unknown is HAIRY
       (hairy-type
        (let ((spec (hairy-type-specifier ctype)))
          ;; Anything other than (SATISFIES ...) is testable
          ;; because there's no reason to suppose that it isn't.
          (unless (or (neq (car spec) 'satisfies) (fboundp (cadr spec)))
            (return-from testable-type-p nil))))))
   ctype)
  t)

;;; This is used by !DEFINE-SUPERCLASSES to define the SUBTYPE-ARG1
;;; method. INFO is a list of conses
;;;   (SUPERCLASS-CLASS . {GUARD-TYPE-SPECIFIER | NIL}).
(defun has-superclasses-complex-subtypep-arg1 (type1 type2 info)
  ;; If TYPE2 might be concealing something related to our class
  ;; hierarchy
  (cond ((type-might-contain-other-types-p type2)
         ;; too confusing, gotta punt
         (values nil nil))
        ((fun-designator-type-p type1)
         (values nil t))
        (t
         ;; ordinary case expected by old CMU CL code, where the taxonomy
         ;; of TYPE2's representation accurately reflects the taxonomy of
         ;; the underlying set
         (values
          ;; FIXME: This old CMU CL code probably deserves a comment
          ;; explaining to us mere mortals how it works...
          (and (sb-xc:typep type2 'classoid)
               (dolist (x info nil)
                 (let ((guard (cdr x)))
                   (when (or (not guard)
                             (csubtypep type1 (if (%instancep guard)
                                                  guard
                                                  (setf (cdr x)
                                                        (specifier-type guard)))))
                     (return
                       (or (eq type2 (car x))
                           (let ((inherits (wrapper-inherits
                                            (classoid-wrapper (car x)))))
                             (dotimes (i (length inherits) nil)
                               (when (eq type2 (wrapper-classoid (svref inherits i)))
                                 (return t))))))))))
          t))))

;;; This function takes a list of specs, each of the form
;;;    (SUPERCLASS-NAME &OPTIONAL GUARD).
;;; Consider one spec (with no guard): any instance of the named
;;; TYPE-CLASS is also a subtype of the named superclass and of any of
;;; its superclasses. If there are multiple specs, then some will have
;;; guards. We choose the first spec whose guard is a supertype of
;;; TYPE1 and use its superclass. In effect, a sequence of guards
;;;    G0, G1, G2
;;; is actually
;;;    G0,(and G1 (not G0)), (and G2 (not (or G0 G1))).
;;;
;;; WHEN controls when the forms are executed.
(defmacro !define-superclasses (type-class-name specs progn-oid)
  (let ((defun-name (symbolicate type-class-name "-COMPLEX-SUBTYPEP-ARG1")))
    `(progn
       (defun ,defun-name (type1 type2)
         (has-superclasses-complex-subtypep-arg1
          type1 type2
          (load-time-value
           (list ,@(mapcar (lambda (spec)
                             (destructuring-bind (super &optional guard) spec
                               `(cons (find-classoid ',super) ',guard)))
                           specs)) #-sb-xc-host t)))
       (,progn-oid
        (let ((type-class (type-class-or-lose ',type-class-name)))
         (setf (type-class-complex-subtypep-arg1 type-class) #',defun-name)
         (setf (type-class-complex-subtypep-arg2 type-class)
               #'delegate-complex-subtypep-arg2)
         (setf (type-class-complex-intersection2 type-class)
               #'delegate-complex-intersection2))))))

;;;; FUNCTION and VALUES types
;;;;
;;;; Pretty much all of the general type operations are illegal on
;;;; VALUES types, since we can't discriminate using them, do
;;;; SUBTYPEP, etc. FUNCTION types are acceptable to the normal type
;;;; operations, but are generally considered to be equivalent to
;;;; FUNCTION. These really aren't true types in any type theoretic
;;;; sense, but we still parse them into CTYPE structures for two
;;;; reasons:

;;;; -- Parsing and unparsing work the same way, and indeed we can't
;;;;    tell whether a type is a function or values type without
;;;;    parsing it.
;;;; -- Many of the places that can be annotated with real types can
;;;;    also be annotated with function or values types.

(define-type-class values :enumerable nil :might-contain-other-types nil)

(defun-cached (make-values-type-cached
               :hash-bits 8
               :hash-function
               (lambda (req opt rest allowp)
                 (logxor (type-list-cache-hash req)
                         (type-list-cache-hash opt)
                          (if rest
                              (type-hash-value rest)
                              42)
                          ;; Results (logand #xFF (sxhash t/nil))
                          ;; hardcoded to avoid relying on the xc host.
                          ;; [but (logand (sxhash nil) #xff) => 2
                          ;;  for me, so the code and comment disagree,
                          ;;  but not in a way that matters.]
                          (if allowp
                              194
                              11))))
    ((required list-elts-eq) ; always compare ctype instances by EQ
     (optional list-elts-eq)
     (rest eq)
     (allowp eq))
  (%make-values-type :required required
                     :optional optional
                     :rest rest
                     :allowp allowp))

(defun make-values-type (&key required optional rest allowp)
  (multiple-value-bind (required optional rest)
      (canonicalize-args-type-args required optional rest)
    (cond ((and (null required)
                (null optional)
                (eq rest *universal-type*))
           *wild-type*)
          ((memq *empty-type* required)
           *empty-type*)
          (t (make-values-type-cached required optional
                                      rest allowp)))))

(define-type-method (values :simple-subtypep :complex-subtypep-arg1)
                     (type1 type2)
  (declare (ignore type2))
  ;; FIXME: should be TYPE-ERROR, here and in next method
  (error "SUBTYPEP is illegal on this type:~%  ~S" (type-specifier type1)))

(define-type-method (values :complex-subtypep-arg2)
                     (type1 type2)
  (declare (ignore type1))
  (error "SUBTYPEP is illegal on this type:~%  ~S" (type-specifier type2)))

(define-type-method (values :negate) (type)
  (error "NOT VALUES too confusing on ~S" (type-specifier type)))

(define-type-method (values :unparse) (type)
  (cons 'values
        (let ((unparsed (unparse-args-types type)))
          (if (or (values-type-optional type)
                  (values-type-rest type)
                  (values-type-allowp type))
              unparsed
              (nconc unparsed '(&optional))))))

;;; Return true if LIST1 and LIST2 have the same elements in the same
;;; positions according to TYPE=. We return NIL, NIL if there is an
;;; uncertain comparison.
(defun type=-list (list1 list2)
  (declare (list list1 list2))
  (do ((types1 list1 (cdr types1))
       (types2 list2 (cdr types2)))
      ((or (null types1) (null types2))
       (if (or types1 types2)
           (values nil t)
           (values t t)))
    (multiple-value-bind (val win)
        (type= (first types1) (first types2))
      (unless win
        (return (values nil nil)))
      (unless val
        (return (values nil t))))))

(define-type-method (values :simple-=) (type1 type2)
  (type=-args type1 type2))

;;; a flag that we can bind to cause complex function types to be
;;; unparsed as FUNCTION. This is useful when we want a type that we
;;; can pass to TYPEP.
(defvar *unparse-fun-type-simplify* nil)

(define-type-class function :enumerable nil :might-contain-other-types nil)

(define-type-method (function :negate) (type) (make-negation-type type))

(define-type-method (function :unparse) (type)
  (let ((name (if (fun-designator-type-p type)
                  'function-designator
                  'function)))
    (cond (*unparse-fun-type-simplify*
           name)
          (t
           (list name
                 (if (fun-type-wild-args type)
                     '*
                     (unparse-args-types type))
                 (type-specifier
                  (fun-type-returns type)))))))

;;; The meaning of this is a little confused. On the one hand, all
;;; function objects are represented the same way regardless of the
;;; arglists and return values, and apps don't get to ask things like
;;; (TYPEP #'FOO (FUNCTION (FIXNUM) *)) in any meaningful way. On the
;;; other hand, Python wants to reason about function types. So...
(define-type-method (function :simple-subtypep) (type1 type2)
  (cond ((and (fun-designator-type-p type1)
              (not (fun-designator-type-p type2)))
         (values nil t))
        ((type= type1 type2)
         ;; Since the following doesn't handle &rest or &key at least
         ;; pick out equal types.
         (values t t))
        (t
         (flet ((fun-type-simple-p (type)
                  (not (or (fun-type-rest type)
                           (fun-type-keyp type))))
                (every-csubtypep (types1 types2)
                  (loop
                    for a1 in types1
                    for a2 in types2
                    do (multiple-value-bind (res sure-p)
                           (csubtypep a1 a2)
                         (unless res (return (values res sure-p))))
                    finally (return (values t t)))))
           (and/type (values-subtypep (fun-type-returns type1)
                                      (fun-type-returns type2))
                     (cond ((fun-type-wild-args type2) (values t t))
                           ((fun-type-wild-args type1)
                            (cond ((fun-type-keyp type2) (values nil nil))
                                  ((not (fun-type-rest type2)) (values nil t))
                                  ((not (null (fun-type-required type2)))
                                   (values nil t))
                                  (t (and/type (type= *universal-type*
                                                      (fun-type-rest type2))
                                               (every/type #'type=
                                                           *universal-type*
                                                           (fun-type-optional
                                                            type2))))))
                           ((not (and (fun-type-simple-p type1)
                                      (fun-type-simple-p type2)))
                            (values nil nil))
                           (t (multiple-value-bind (min1 max1) (fun-type-nargs type1)
                                (multiple-value-bind (min2 max2) (fun-type-nargs type2)
                                  (cond ((or (> max1 max2) (< min1 min2))
                                         (values nil t))
                                        ((and (= min1 min2) (= max1 max2))
                                         (and/type (every-csubtypep
                                                    (fun-type-required type1)
                                                    (fun-type-required type2))
                                                   (every-csubtypep
                                                    (fun-type-optional type1)
                                                    (fun-type-optional type2))))
                                        (t (every-csubtypep
                                            (concatenate 'list
                                                         (fun-type-required type1)
                                                         (fun-type-optional type1))
                                            (concatenate 'list
                                                         (fun-type-required type2)
                                                         (fun-type-optional type2))))))))))))))

(!define-superclasses function ((function)) !cold-init-forms)

;;; The union or intersection of two FUNCTION types is FUNCTION.
(define-type-method (function :simple-union2) (type1 type2)
  (if (or (fun-designator-type-p type1)
          (fun-designator-type-p type2))
      (specifier-type 'function-designator)
      (specifier-type 'function)))

(define-type-method (function :simple-intersection2) (type1 type2)
  (let ((ftype (specifier-type 'function)))
    (cond ((eq type1 ftype) type2)
          ((eq type2 ftype) type1)
          (t (let ((rtype (values-type-intersection (fun-type-returns type1)
                                                    (fun-type-returns type2)))
                   (designator
                     (and (fun-designator-type-p type1)
                          (fun-designator-type-p type2))))
               (flet ((change-returns (ftype rtype)
                        (declare (type fun-type ftype) (type ctype rtype))
                        (make-fun-type :required (fun-type-required ftype)
                                       :optional (fun-type-optional ftype)
                                       :keyp (fun-type-keyp ftype)
                                       :keywords (fun-type-keywords ftype)
                                       :allowp (fun-type-allowp ftype)
                                       :returns rtype
                                       :designator designator)))
                 (cond
                   ((fun-type-wild-args type1)
                    (if (fun-type-wild-args type2)
                        (make-fun-type :wild-args t
                                       :returns rtype
                                       :designator designator)
                        (change-returns type2 rtype)))
                   ((fun-type-wild-args type2)
                    (change-returns type1 rtype))
                   (t (multiple-value-bind (req opt rest)
                          (args-type-op type1 type2 #'type-intersection #'max)
                        (make-fun-type :required req
                                       :optional opt
                                       :rest rest
                                       ;; FIXME: :keys
                                       :allowp (and (fun-type-allowp type1)
                                                    (fun-type-allowp type2))
                                       :returns rtype
                                       :designator designator))))))))))

;;; The union or intersection of a subclass of FUNCTION with a
;;; FUNCTION type is somewhat complicated.
(define-type-method (function :complex-intersection2) (type1 type2)
  (cond
    ((and (fun-designator-type-p type2)
          (or (csubtypep type1 (specifier-type 'symbol))
              (csubtypep type1 (specifier-type 'function))))
     type1)
    ((type= type1 (specifier-type 'function)) type2)
    ((csubtypep type1 (specifier-type 'function)) nil)
    (t :call-other-method)))
(define-type-method (function :complex-union2) (type1 type2)
  (declare (ignore type2))
  ;; TYPE2 is a FUNCTION type.  If TYPE1 is a classoid type naming
  ;; FUNCTION, then it is the union of the two; otherwise, there is no
  ;; special union.
  (cond
    ((type= type1 (specifier-type 'function)) type1)
    (t nil)))

(define-type-method (function :simple-=) (type1 type2)
  (if (or (and (fun-designator-type-p type1)
               (not (fun-designator-type-p type2)))
          (and (not (fun-designator-type-p type1))
               (fun-designator-type-p type2)))
      (values nil t)
      (macrolet ((compare (comparator field)
                   (let ((reader (symbolicate '#:fun-type- field)))
                     `(,comparator (,reader type1) (,reader type2)))))
        (and/type (compare type= returns)
                  (cond ((neq (fun-type-wild-args type1) (fun-type-wild-args type2))
                         (values nil t))
                        ((eq (fun-type-wild-args type1) t)
                         (values t t))
                        (t (type=-args type1 type2)))))))

#+sb-xc-host
(defvar *interned-fun-types*
  (flet ((fun-type (n)
           (!make-interned-fun-type (pack-interned-ctype-bits 'function)
                                    (make-list n :initial-element *universal-type*)
                                    nil nil nil nil nil nil *wild-type*)))
    (vector (fun-type 0) (fun-type 1) (fun-type 2) (fun-type 3))))

(defun make-fun-type (&key required optional rest
                           keyp keywords allowp
                           wild-args returns
                           designator)
  (let ((rest (if (eq rest *empty-type*) nil rest))
        (n (length required)))
    (cond (designator
           (make-fun-designator-type required optional rest keyp keywords
                                     allowp wild-args returns))
          ((and
            (<= n 3)
            (not optional) (not rest) (not keyp)
            (not keywords) (not allowp) (not wild-args)
            (eq returns *wild-type*)
            (not (find *universal-type* required :test #'neq)))
           (svref (literal-ctype-vector *interned-fun-types*) n))
          (t
           (%make-fun-type required optional rest keyp keywords
                           allowp wild-args returns)))))

;; This seems to be used only by cltl2, and within 'cross-type',
;; where it is never used, which makes sense, since pretty much we
;; never want this object, but instead the classoid FUNCTION
;; if we know nothing about a function's signature.
;; Maybe this should not exist unless cltl2 is loaded???
(define-load-time-global *universal-fun-type*
  (make-fun-type :wild-args t :returns *wild-type*))

(define-type-class constant :inherits values)

(define-type-method (constant :negate) (type)
  (error "NOT CONSTANT too confusing on ~S" (type-specifier type)))

(define-type-method (constant :unparse) (type)
  `(constant-arg ,(type-specifier (constant-type-type type))))

(define-type-method (constant :simple-=) (type1 type2)
  (type= (constant-type-type type1) (constant-type-type type2)))

(def-type-translator constant-arg ((:context context) type)
  (make-constant-type :type (single-value-specifier-type type context)))

(defun canonicalize-args-type-args (required optional rest &optional keyp)
  (when (eq rest *empty-type*)
    ;; or vice-versa?
    (setq rest nil))
  (loop with last-not-rest = nil
        for i from 0
        for opt in optional
        do (cond ((eq opt *empty-type*)
                  (return (values required (subseq optional 0 i) rest)))
                 ((and (not keyp) (neq opt rest))
                  (setq last-not-rest i)))
        finally (return (values required
                                (cond (keyp
                                       optional)
                                      (last-not-rest
                                       (subseq optional 0 (1+ last-not-rest))))
                                rest))))

;;; CONTEXT is the cookie passed down from the outermost surrounding call
;;; of BASIC-PARSE-TYPE. INNER-CONTEXT-KIND is an indicator of whether
;;; we are currently parsing a FUNCTION or a VALUES compound type specifier.
;;; If the entire LAMBDA-LISTY-THING is *, we do not call this function at all.
;;; If an element of it is *, that constitutes an error, as is clear
;;; for VALUES: "The symbol * may not be among the value-types."
;;;  http://www.lispworks.com/documentation/HyperSpec/Body/t_values.htm
;;; and the FUNCTION compound type, for which the grammar is:
;;;   function [arg-typespec [value-typespec]]
;;;   arg-typespec::= (typespec* [&optional typespec*] [&rest typespec];[&key (keyword typespec)*])
;;;   typespec --- a type specifier.
;;; where the glossary says: "type specifier: n. an expression that denotes a type."
;;; which of course * does not denote, and is made all the more clear by the fact
;;; that the AND, OR, and NOT combinators explicitly preclude * as an element.
(defun parse-args-types (context lambda-listy-thing inner-context-kind)
  (multiple-value-bind (llks required optional rest keys)
      (parse-lambda-list
       lambda-listy-thing
       :context inner-context-kind
       :accept (ecase inner-context-kind
                 (:values-type (lambda-list-keyword-mask '(&optional &rest)))
                 (:function-type (lambda-list-keyword-mask
                                  '(&optional &rest &key &allow-other-keys))))
       :silent t)
   (labels ((parse-list (list) (mapcar #'parse-one list))
            (parse-one (x)
              (specifier-type x context
                              (case inner-context-kind
                                (:function-type 'function)
                                (t 'values)))))
    (let ((required (parse-list required))
          (optional (parse-list optional))
          (rest (when rest (parse-one (car rest))))
          (keywords
           (collect ((key-info))
             (dolist (key keys)
               (unless (proper-list-of-length-p key 2)
                 (error "Keyword type description is not a two-list: ~S." key))
               (let ((kwd (first key)))
                 (when (find kwd (key-info) :key #'key-info-name)
                   (error (sb-format:tokens
                           "~@<repeated keyword ~S in lambda list: ~2I~_~
                            ~/sb-impl:print-lambda-list/~:>")
                          kwd lambda-listy-thing))
                 (key-info
                  (make-key-info
                   ;; MAKE-KEY-INFO will complain if KWD is not a symbol.
                   ;; That's good enough - we don't need an extra check here.
                   :name kwd
                   :type (single-value-specifier-type (second key) context)))))
             (key-info))))
      (multiple-value-bind (required optional rest)
          (canonicalize-args-type-args required optional rest
                                       (ll-kwds-keyp llks))
        (values llks required optional rest keywords))))))

;;; Return the lambda-list-like type specification corresponding
;;; to an ARGS-TYPE.
(declaim (ftype (function (args-type) list) unparse-args-types))
(defun unparse-args-types (type)
  (collect ((result))

    (dolist (arg (args-type-required type))
      (result (type-specifier arg)))

    (when (args-type-optional type)
      (result '&optional)
      (dolist (arg (args-type-optional type))
        (result (type-specifier arg))))

    (when (args-type-rest type)
      (result '&rest)
      (result (type-specifier (args-type-rest type))))

    (when (args-type-keyp type)
      (result '&key)
      (dolist (key (args-type-keywords type))
        (result (list (key-info-name key)
                      (type-specifier (key-info-type key))))))

    (when (args-type-allowp type)
      (result '&allow-other-keys))

    (result)))

(defun translate-fun-type (context args result
                           &key designator)
  (let ((result (coerce-to-values (basic-parse-typespec result context))))
    (cond ((neq args '*)
           (multiple-value-bind (llks required optional rest keywords)
               (parse-args-types context args :function-type)
             (if (and (null required)
                      (null optional)
                      (eq rest *universal-type*)
                      (not (ll-kwds-keyp llks)))
                 (if (eq result *wild-type*)
                     (specifier-type 'function)
                     (make-fun-type :wild-args t :returns result
                                    :designator designator))
                 (make-fun-type :required required
                                :optional optional
                                :rest rest
                                :keyp (ll-kwds-keyp llks)
                                :keywords keywords
                                :allowp (ll-kwds-allowp llks)
                                :returns result
                                :designator designator))))
          ((eq result *wild-type*)
           (if designator
               ;; Do not put 'FUNCTION-DESIGNATOR here!
               ;; (Since this is the parser for FUNCTION-DESIGNATOR)
               (specifier-type '(or function symbol))
               (specifier-type 'function)))
          (t
           (make-fun-type :wild-args t :returns result
                          :designator designator)))))

(def-type-translator function ((:context context)
                                &optional (args '*) (result '*))
  (translate-fun-type context args result))

(def-type-translator function-designator ((:context context)
                                &optional (args '*) (result '*))
  (translate-fun-type context args result :designator t))

(def-type-translator values :list ((:context context) &rest values)
  (multiple-value-bind (llks required optional rest)
      (parse-args-types context values :values-type)
    (if (plusp llks)
        (make-values-type :required required :optional optional :rest rest)
        (make-short-values-type required))))

;;;; VALUES types interfaces
;;;;
;;;; We provide a few special operations that can be meaningfully used
;;;; on VALUES types (as well as on any other type).

;;; Return the minimum number of values possibly matching VALUES type
;;; TYPE.
(defun values-type-min-value-count (type)
  (etypecase type
    (named-type
     (ecase (named-type-name type)
       ((t *) 0)
       ((nil) 0)))
    (values-type
     (length (values-type-required type)))))

;;; Return the maximum number of values possibly matching VALUES type
;;; TYPE.
(defun values-type-max-value-count (type)
  (etypecase type
    (named-type
     (ecase (named-type-name type)
       ((t *) call-arguments-limit)
       ((nil) 0)))
    (values-type
     (if (values-type-rest type)
         call-arguments-limit
         (+ (length (values-type-optional type))
            (length (values-type-required type)))))))

(defun values-type-may-be-single-value-p (type)
  (<= (values-type-min-value-count type)
      1
      (values-type-max-value-count type)))

;;; VALUES type with a single value.
(defun type-single-value-p (type)
  (and (values-type-p type)
       (not (values-type-rest type))
       (null (values-type-optional type))
       (singleton-p (values-type-required type))))

;;; Return the type of the first value indicated by TYPE. This is used
;;; by people who don't want to have to deal with VALUES types.
(defun single-value-type (type)
  (declare (type ctype type))
  (cond ((eq type *wild-type*)
         *universal-type*)
        ((eq type *empty-type*)
         *empty-type*)
        ((not (values-type-p type))
         type)
        ((car (args-type-required type)))
        (t (type-union (specifier-type 'null)
                       (or (car (args-type-optional type))
                           (args-type-rest type)
                           (specifier-type 'null))))))

;;; Return the minimum number of arguments that a function can be
;;; called with, and the maximum number or NIL. If not a function
;;; type, return NIL, NIL.
(defun fun-type-nargs (type)
  (declare (type ctype type))
  (if (and (fun-type-p type) (not (fun-type-wild-args type)))
      (let ((fixed (length (args-type-required type))))
        (if (or (args-type-rest type)
                (args-type-keyp type)
                (args-type-allowp type))
            (values fixed nil)
            (values fixed (+ fixed (length (args-type-optional type))))))
      (values nil nil)))

;;; Determine whether TYPE corresponds to a definite number of values.
;;; The first value is a list of the types for each value, and the
;;; second value is the number of values. If the number of values is
;;; not fixed, then return NIL and :UNKNOWN.
(defun values-types (type)
  (declare (type ctype type))
  (cond ((or (eq type *wild-type*) (eq type *empty-type*))
         (values nil :unknown))
        ((or (args-type-optional type)
             (args-type-rest type))
         (values nil :unknown))
        (t
         (let ((req (args-type-required type)))
           (values req (length req))))))

;;; Return two values:
;;; 1. A list of all the positional (fixed and optional) types.
;;; 2. The &REST type (if any). If no &REST, then the DEFAULT-TYPE.
(defun values-type-types (type &optional (default-type *empty-type*))
  (declare (type ctype type))
  (if (eq type *wild-type*)
      (values nil *universal-type*)
      (values (append (args-type-required type)
                      (args-type-optional type))
              (or (args-type-rest type)
                  default-type))))

;;; types of values in (the <type> (values o_1 ... o_n))
(defun values-type-out (type count)
  (declare (type ctype type) (type unsigned-byte count))
  (if (eq type *wild-type*)
      (make-list count :initial-element *universal-type*)
      (collect ((res))
        (flet ((process-types (types)
                 (loop for type in types
                       while (plusp count)
                       do (decf count)
                       do (res type))))
          (process-types (values-type-required type))
          (process-types (values-type-optional type))
          (let ((rest (values-type-rest type)))
            (when rest
              (loop repeat count
                    do (res rest)))))
        (res))))

;;; types of variable in (m-v-bind (v_1 ... v_n) (the <type> ...
(defun values-type-in (type count)
  (declare (type ctype type) (type unsigned-byte count))
  (if (eq type *wild-type*)
      (make-list count :initial-element *universal-type*)
      (collect ((res))
        (let ((null-type (specifier-type 'null)))
          (loop for type in (values-type-required type)
             while (plusp count)
             do (decf count)
             do (res type))
          (loop for type in (values-type-optional type)
             while (plusp count)
             do (decf count)
             do (res (type-union type null-type)))
          (when (plusp count)
            (loop with rest = (acond ((values-type-rest type)
                                      (type-union it null-type))
                                     (t null-type))
               repeat count
               do (res rest))))
        (res))))

;;; Return a list of OPERATION applied to the types in TYPES1 and
;;; TYPES2, padding with REST2 as needed. TYPES1 must not be shorter
;;; than TYPES2. The second value is T if OPERATION always returned a
;;; true second value.
(defun fixed-values-op (types1 types2 rest2 operation)
  (declare (list types1 types2) (type ctype rest2) (type function operation))
  (let ((exact t))
    (values (mapcar (lambda (t1 t2)
                      (multiple-value-bind (res win)
                          (funcall operation t1 t2)
                        (unless win
                          (setq exact nil))
                        res))
                    types1
                    (append types2
                            (make-list (- (length types1) (length types2))
                                       :initial-element rest2)))
            exact)))

;;; If TYPE isn't a values type, then make it into one.
(defun-cached (%coerce-to-values :hash-bits 8 :hash-function #'type-hash-value)
    ((type eq))
  (cond ((multiple-value-bind (res sure)
             (csubtypep (specifier-type 'null) type)
           (and (not res) sure))
         ;; FIXME: What should we do with (NOT SURE)?
         (make-values-type :required (list type) :rest *universal-type*))
        (t
         (make-values-type :optional (list type) :rest *universal-type*))))

(defun coerce-to-values (type)
  (declare (type ctype type))
  (cond ((or (eq type *universal-type*)
             (eq type *wild-type*))
         *wild-type*)
        ((values-type-p type)
         type)
        (t (%coerce-to-values type))))

;;; Return type, corresponding to ANSI short form of VALUES type
;;; specifier.
(defun make-short-values-type (types)
  (declare (list types))
  (let ((last-required (position-if
                        (lambda (type)
                          (not/type (csubtypep (specifier-type 'null) type)))
                        types
                        :from-end t)))
    (if last-required
        (make-values-type :required (subseq types 0 (1+ last-required))
                          :optional (subseq types (1+ last-required))
                          :rest *universal-type*)
        (make-values-type :optional types :rest *universal-type*))))

(defun make-single-value-type (type)
  (make-values-type :required (list type)))

;;; Do the specified OPERATION on TYPE1 and TYPE2, which may be any
;;; type, including VALUES types. With VALUES types such as:
;;;    (VALUES a0 a1)
;;;    (VALUES b0 b1)
;;; we compute the more useful result
;;;    (VALUES (<operation> a0 b0) (<operation> a1 b1))
;;; rather than the precise result
;;;    (<operation> (values a0 a1) (values b0 b1))
;;; This has the virtue of always keeping the VALUES type specifier
;;; outermost, and retains all of the information that is really
;;; useful for static type analysis. We want to know what is always
;;; true of each value independently. It is worthless to know that if
;;; the first value is B0 then the second will be B1.
;;;
;;; If the VALUES count signatures differ, then we produce a result with
;;; the required VALUE count chosen by NREQ when applied to the number
;;; of required values in TYPE1 and TYPE2. Any &KEY values become
;;; &REST T (anyone who uses keyword values deserves to lose.)
;;;
;;; The second value is true if the result is definitely empty or if
;;; OPERATION returned true as its second value each time we called
;;; it. Since we approximate the intersection of VALUES types, the
;;; second value being true doesn't mean the result is exact.
(defun args-type-op (type1 type2 operation nreq)
  (declare (type ctype type1 type2)
           (type function operation nreq))
  (when (eq type1 type2)
    (values type1 t))
  (multiple-value-bind (types1 rest1)
      (values-type-types type1)
    (multiple-value-bind (types2 rest2)
        (values-type-types type2)
      (multiple-value-bind (rest rest-exact)
          (funcall operation rest1 rest2)
        (multiple-value-bind (res res-exact)
            (if (< (length types1) (length types2))
                (fixed-values-op types2 types1 rest1 operation)
                (fixed-values-op types1 types2 rest2 operation))
          (let* ((req (funcall nreq
                               (length (args-type-required type1))
                               (length (args-type-required type2))))
                 (required (subseq res 0 req))
                 (opt (subseq res req)))
            (values required opt rest
                    (and rest-exact res-exact))))))))

(defun values-type-op (type1 type2 operation nreq)
  (multiple-value-bind (required optional rest exactp)
      (args-type-op type1 type2 operation nreq)
    (values (make-values-type :required required
                              :optional optional
                              :rest rest)
            exactp)))

(defun compare-key-args (type1 type2)
  (let ((keys1 (args-type-keywords type1))
        (keys2 (args-type-keywords type2)))
    (and (= (length keys1) (length keys2))
         (eq (args-type-allowp type1)
             (args-type-allowp type2))
         (loop for key1 in keys1
               for match = (find (key-info-name key1)
                                 keys2 :key #'key-info-name)
               always (and match
                           (type= (key-info-type key1)
                                  (key-info-type match)))))))

(defun type=-args (type1 type2)
  (macrolet ((compare (comparator field)
               (let ((reader (symbolicate '#:args-type- field)))
                 `(,comparator (,reader type1) (,reader type2)))))
    (and/type
     (cond ((null (args-type-rest type1))
            (values (null (args-type-rest type2)) t))
           ((null (args-type-rest type2))
            (values nil t))
           (t
            (compare type= rest)))
     (and/type (and/type (compare type=-list required)
                         (compare type=-list optional))
               (if (or (args-type-keyp type1) (args-type-keyp type2))
                   (values (compare-key-args type1 type2) t)
                   (values t t))))))

;;; Do a union or intersection operation on types that might be values
;;; types. The result is optimized for utility rather than exactness,
;;; but it is guaranteed that it will be no smaller (more restrictive)
;;; than the precise result.
;;;
;;; The return convention seems to be analogous to
;;; TYPES-EQUAL-OR-INTERSECT. -- WHN 19990910.
(defun-cached (values-type-union :hash-function #'type-cache-hash
                                 :hash-bits 8)
    ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type1 *wild-type*) (eq type2 *wild-type*)) *wild-type*)
        ((eq type1 *empty-type*) type2)
        ((eq type2 *empty-type*) type1)
        (t
         (values (values-type-op type1 type2 #'type-union #'min)))))

(defun-cached (values-type-intersection :hash-function #'type-cache-hash
                                        :hash-bits 8)
    ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((eq type1 *wild-type*)
         (coerce-to-values type2))
        ((or (eq type2 *wild-type*) (eq type2 *universal-type*))
         type1)
        ((or (eq type1 *empty-type*) (eq type2 *empty-type*))
         *empty-type*)
        ((and (not (values-type-p type2))
              (values-type-required type1))
         (let ((req1 (values-type-required type1)))
           (make-values-type :required (cons (type-intersection (first req1) type2)
                                             (rest req1))
                             :optional (values-type-optional type1)
                             :rest (values-type-rest type1)
                             :allowp (values-type-allowp type1))))
        (t
         (values (values-type-op type1 (coerce-to-values type2)
                                 #'type-intersection
                                 #'max)))))

;;; This is like TYPES-EQUAL-OR-INTERSECT, except that it sort of
;;; works on VALUES types. Note that due to the semantics of
;;; VALUES-TYPE-INTERSECTION, this might return (VALUES T T) when
;;; there isn't really any intersection.
(defun values-types-equal-or-intersect (type1 type2)
  (cond ((or (eq type1 *empty-type*) (eq type2 *empty-type*))
         (values t t))
        ((or (eq type1 *wild-type*) (eq type2 *wild-type*))
         (values t t))
        (t
         (let ((res (values-type-intersection type1 type2)))
           (values (not (eq res *empty-type*))
                   t)))))

;;; a SUBTYPEP-like operation that can be used on any types, including
;;; VALUES types
(defun-cached (values-subtypep :hash-function #'type-cache-hash
                               :hash-bits 8
                               :values 2)
    ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type2 *wild-type*) (eq type2 *universal-type*)
             (eq type1 *empty-type*))
         (values t t))
        ((eq type1 *wild-type*)
         (values (eq type2 *wild-type*) t))
        ((or (eq type2 *empty-type*)
             (not (values-types-equal-or-intersect type1 type2)))
         (values nil t))
        ((and (not (values-type-p type2))
              (values-type-required type1))
         (csubtypep (first (values-type-required type1))
                    type2))
        (t (setq type2 (coerce-to-values type2))
           (multiple-value-bind (types1 rest1) (values-type-types type1)
             (multiple-value-bind (types2 rest2) (values-type-types type2)
               (cond ((< (length (values-type-required type1))
                         (length (values-type-required type2)))
                      (values nil t))
                     ((< (length types1) (length types2))
                      (values nil nil))
                     (t
                      (do ((t1 types1 (rest t1))
                           (t2 types2 (rest t2)))
                          ((null t2)
                           (loop named loop
                                 for type in t1
                                 do (multiple-value-bind (res win)
                                        (csubtypep type rest2)
                                      (unless win
                                        (return (values nil nil)))
                                      (unless res
                                        (return (values nil t)))))
                           (csubtypep rest1 rest2))
                        (multiple-value-bind (res win-p)
                            (csubtypep (first t1) (first t2))
                          (unless win-p
                            (return (values nil nil)))
                          (unless res
                            (return (values nil t))))))))))))

;;;; type method interfaces

;;; like SUBTYPEP, only works on CTYPE structures
(defun-cached (csubtypep :hash-function #'type-cache-hash
                         :hash-bits 10
                         :memoizer memoize
                         :values 2)
              ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type1 type2)
             (eq type1 *empty-type*)
             (eq type2 *universal-type*))
         (values t t))
        #+nil
        ((eq type1 *universal-type*)
         (values nil t))
        (t
         (memoize
          (!invoke-type-method :simple-subtypep :complex-subtypep-arg2
                               type1 type2
                               :complex-arg1 :complex-subtypep-arg1)))))

;;; Just parse the type specifiers and call CSUBTYPE.
;;; Well, not "just" - Despite memoization of parsing and CSUBTYPEP,
;;; it's nonetheless better to test EQUAL first, which is ~10x faster
;;; in the positive case, and insigificant in the negative.
;;; The specifiers might not be legal type specifiers,
;;; but we're not obligated to police that:
;;;   "This version eliminates the requirement to signal an error."
;;; http://www.lispworks.com/documentation/HyperSpec/Issues/iss335_w.htm
;;; (Status: Passed, as amended, Jun89 X3J13)
;;;
;;; Also, inferring from the version of the text that was obsoleted
;;; - which while it has no direct impact on the final requirement,
;;; implies something about what would have been legal -
;;;   "SUBTYPEP must always return values T T in the case where the two
;;;    type specifiers (or their expansions) are EQUAL."
;;; i.e. though it is not longer technically a MUST, it suggests that EQUAL is
;;; in fact a valid implementation, at least where it computes T.
(defun subtypep (type1 type2 &optional environment)
  "Return two values indicating the relationship between type1 and type2.
  If values are T and T, type1 definitely is a subtype of type2.
  If values are NIL and T, type1 definitely is not a subtype of type2.
  If values are NIL and NIL, it couldn't be determined."
  (declare (type lexenv-designator environment) (ignore environment))
  (declare (explicit-check))
  (if (equal type1 type2)
      (values t t)
      (csubtypep (specifier-type type1) (specifier-type type2))))

(declaim (inline ctype-eq-comparable))
(defun ctype-eq-comparable (ctype)
  (logtest (type-hash-value ctype) +type-admits-type=-optimization+))

(defun ctype-interned-p (ctype)
  (logtest (type-hash-value ctype) +type-internedp+))

(declaim (start-block))

;;; If two types are definitely equivalent, return true. The second
;;; value indicates whether the first value is definitely correct.
;;; This should only fail in the presence of HAIRY types.
(defun-cached (type= :hash-function #'type-cache-hash
                     :hash-bits 11
                     :memoizer memoize
                     :values 2)
              ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((eq type1 type2)
         (values t t))
        ;; If args are not EQ, but both allow TYPE= optimization,
        ;; and at least one is interned, then return no and certainty.
        ;; Most of the interned CTYPEs admit this optimization,
        ;; NUMERIC and MEMBER types do as well.
        ((and (logtest +type-internedp+
                       (logior (type-hash-value type1) (type-hash-value type2)))
              (logtest +type-admits-type=-optimization+
                       (logand (type-hash-value type1) (type-hash-value type2))))
         (values nil t))
        (t
         (memoize (!invoke-type-method :simple-= :complex-= type1 type2)))))

;;; Not exactly the negation of TYPE=, since when the relationship is
;;; uncertain, we still return NIL, NIL. This is useful in cases where
;;; the conservative assumption is =.
(defun type/= (type1 type2)
  (declare (type ctype type1 type2))
  (multiple-value-bind (res win) (type= type1 type2)
    (if win
        (values (not res) t)
        (values nil nil))))

(declaim (end-block))

;;; the type method dispatch case of TYPE-UNION2
(defun %type-union2 (type1 type2)
  ;; As in %TYPE-INTERSECTION2, it seems to be a good idea to give
  ;; both argument orders a chance at COMPLEX-INTERSECTION2. Unlike
  ;; %TYPE-INTERSECTION2, though, I don't have a specific case which
  ;; demonstrates this is actually necessary. Also unlike
  ;; %TYPE-INTERSECTION2, there seems to be no need to distinguish
  ;; between not finding a method and having a method return NIL.
  (flet ((1way (x y)
           (!invoke-type-method :simple-union2 :complex-union2
                                x y
                                :default nil)))
    (declare (inline 1way))
    (or (1way type1 type2)
        (1way type2 type1))))

;;; Find a type which includes both types. Any inexactness is
;;; represented by the fuzzy element types; we return a single value
;;; that is precise to the best of our knowledge. This result is
;;; simplified into the canonical form, thus is not a UNION-TYPE
;;; unless we find no other way to represent the result.
(defun-cached (type-union2 :hash-function #'type-cache-hash
                           :hash-bits 11
                           :memoizer memoize)
              ((type1 eq) (type2 eq))
  ;; KLUDGE: This was generated from TYPE-INTERSECTION2 by Ye Olde Cut And
  ;; Paste technique of programming. If it stays around (as opposed to
  ;; e.g. fading away in favor of some CLOS solution) the shared logic
  ;; should probably become shared code. -- WHN 2001-03-16
  (declare (type ctype type1 type2))
  (let ((t2 nil))
    (if (eq type1 type2)
        type1
        (memoize
         (cond
          ;; CSUBTYPEP for array-types answers questions about the
          ;; specialized type, yet for union we want to take the
          ;; expressed type in account too.
          ((and (not (and (array-type-p type1) (array-type-p type2)))
                (or (setf t2 (csubtypep type1 type2))
                    (csubtypep type2 type1)))
           (if t2 type2 type1))
         ((or (union-type-p type1)
              (union-type-p type2))
          ;; Unions of UNION-TYPE should have the UNION-TYPE-TYPES
          ;; values broken out and united separately. The full TYPE-UNION
          ;; function knows how to do this, so let it handle it.
          (type-union type1 type2))
         (t
          ;; the ordinary case: we dispatch to type methods
          (%type-union2 type1 type2)))))))

;;; the type method dispatch case of TYPE-INTERSECTION2
(defun %type-intersection2 (type1 type2)
  ;; We want to give both argument orders a chance at
  ;; COMPLEX-INTERSECTION2. Without that, the old CMU CL type
  ;; methods could give noncommutative results, e.g.
  ;;   (TYPE-INTERSECTION2 *EMPTY-TYPE* SOME-HAIRY-TYPE)
  ;;     => NIL, NIL
  ;;   (TYPE-INTERSECTION2 SOME-HAIRY-TYPE *EMPTY-TYPE*)
  ;;     => #<NAMED-TYPE NIL>, T
  ;; We also need to distinguish between the case where we found a
  ;; type method, and it returned NIL, and the case where we fell
  ;; through without finding any type method. An example of the first
  ;; case is the intersection of a HAIRY-TYPE with some ordinary type.
  ;; An example of the second case is the intersection of two
  ;; completely-unrelated types, e.g. CONS and NUMBER, or SYMBOL and
  ;; ARRAY.
  ;;
  ;; (Why yes, CLOS probably *would* be nicer..)
  (flet ((1way (x y)
           (!invoke-type-method :simple-intersection2 :complex-intersection2
                                x y
                                :default :call-other-method)))
    (declare (inline 1way))
    (let ((xy (1way type1 type2)))
      (or (and (not (eql xy :call-other-method)) xy)
          (let ((yx (1way type2 type1)))
            (or (and (not (eql yx :call-other-method)) yx)
                (cond ((and (eql xy :call-other-method)
                            (eql yx :call-other-method))
                       *empty-type*)
                      (t
                       nil))))))))

(defun-cached (type-intersection2 :hash-function #'type-cache-hash
                                  :hash-bits 11
                                  :memoizer memoize
                                  :values 1)
              ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (if (eq type1 type2)
         ;; FIXME: For some reason, this doesn't catch e.g. type1 =
         ;; type2 = (SPECIFIER-TYPE
         ;; 'SOME-UNKNOWN-TYPE). Investigate. - CSR, 2002-04-10
      type1
      (memoize
       (cond
        ((or (intersection-type-p type1)
             (intersection-type-p type2))
         ;; Intersections of INTERSECTION-TYPE should have the
         ;; INTERSECTION-TYPE-TYPES values broken out and intersected
         ;; separately. The full TYPE-INTERSECTION function knows how
         ;; to do that, so let it handle it.
         (type-intersection type1 type2))
        (t
         ;; the ordinary case: we dispatch to type methods
         (%type-intersection2 type1 type2))))))

;;; Return as restrictive and simple a type as we can discover that is
;;; no more restrictive than the intersection of TYPE1 and TYPE2. At
;;; worst, we arbitrarily return one of the arguments as the first
;;; value (trying not to return a hairy type).
(defun type-approx-intersection2 (type1 type2)
  (cond ((type-intersection2 type1 type2))
        ((hairy-type-p type1) type2)
        (t type1)))

;;; a test useful for checking whether a derived type matches a
;;; declared type
;;;
;;; The first value is true unless the types don't intersect and
;;; aren't equal. The second value is true if the first value is
;;; definitely correct. NIL is considered to intersect with any type.
;;; If T is a subtype of either type, then we also return T, T. This
;;; way we recognize that hairy types might intersect with T.
;;;
;;; Well now given the statement above that this is "useful for ..."
;;; a particular thing, I see how treating *empty-type* magically could
;;; be useful, however given all the _other_ calls to this function within
;;; this file, it seems suboptimal, because logically it is wrong.
(defun types-equal-or-intersect (type1 type2)
  (declare (type ctype type1 type2))
  (if (or (eq type1 *empty-type*) (eq type2 *empty-type*))
      (values t t)
      (let ((intersection2 (type-intersection2 type1 type2)))
        (cond ((not intersection2)
               (if (or (csubtypep *universal-type* type1)
                       (csubtypep *universal-type* type2))
                   (values t t)
                   (values t nil)))
              ((eq intersection2 *empty-type*) (values nil t))
              (t (values t t))))))

;;; Return a Common Lisp type specifier corresponding to the TYPE
;;; object.
(defun type-specifier (type)
  (declare (type ctype type))
  (funcall (type-class-unparse (type-class type)) type))


;;; Return the type structure corresponding to a type specifier.
;;;
;;; Note: VALUES-SPECIFIER-TYPE-CACHE-CLEAR must be called whenever a
;;; type is defined (or redefined).
;;;
;;; As I understand things, :FORTHCOMING-DEFCLASS-TYPE behaves contrarily
;;; to the CLHS intent, which is to make the type known to the compiler.
;;; If we compile in one file:
;;;  (DEFCLASS FRUITBAT () ())
;;;  (DEFUN FRUITBATP (X) (TYPEP X 'FRUITBAT))
;;; we see that it emits a call to %TYPEP with the symbol FRUITBAT as its
;;; argument, whereas it should involve CLASSOID-CELL-TYPEP and LAYOUT-OF,
;;; which (correctly) signals an error if the class were not defined by the
;;; time of the call. Delayed re-parsing of FRUITBAT into any random specifier
;;; at call time is wrong.
;;;
;;; FIXME: symbols which are :PRIMITIVE are inconsistently accepted as singleton
;;; lists. e.g. (BIT) and (ATOM) are considered legal, but (FIXNUM) and
;;; (CHARACTER) are not. It has to do with whether the primitive is actually
;;; a DEFTYPE. The CLHS glossary implies that the singleton is *always* legal.
;;;  "For every atomic type specifier, x, there is an _equivalent_ [my emphasis]
;;;   compound type specifier with no arguments supplied, (x)."
;;; By that same reasonining, is (x) accepted if x names a class?
;;;

;;; The xc host uses an ordinary hash table for memoization.
#+sb-xc-host
(let ((table (make-hash-table :test 'equal)))
  (defun !values-specifier-type-memo-wrapper (thunk specifier)
    (multiple-value-bind (type yesp) (gethash specifier table)
      (if yesp
          type
          (setf (gethash specifier table) (funcall thunk)))))
  (defun values-specifier-type-cache-clear ()
    (clrhash table)))
;;; This cache is sized extremely generously, which has payoff
;;; elsewhere: it improves the TYPE= and CSUBTYPEP functions,
;;; since EQ types are an immediate win.
;;; EQL isn't the best comparator, but EQUAL would be wrong
;;; because EQL specifiers must not use a weaker comparison.
;;; This means that we won't match things like (INTEGER (0) 4) to an existing
;;; entry unless it is EQ.  This is probably not a disaster.
#-sb-xc-host
(sb-impl::!define-hash-cache values-specifier-type
  ((orig list-elements-eql)) ()
   :hash-function #'sxhash :hash-bits 10)

(declaim (inline make-type-context))
(defstruct (type-context
            (:constructor make-type-context
                          (spec &optional proto-classoid (options 0)))
            (:copier nil)
            (:predicate nil))
  (spec nil :read-only t)
  (proto-classoid nil :read-only t)
  (options 0 :type fixnum))
(defconstant +type-parse-cache-inhibit+  1)
(defconstant +type-parse-signal-inhibit+ 2)
(defmacro type-context-cacheable (x)
  `(not (logtest (type-context-options ,x) +type-parse-cache-inhibit+)))

;;; Maintain a table of symbols designating unknown types that have any references
;;; to them, making it easy to inquire whether such things exist. This is at a lower
;;; layer than the parser cache - it's a cache of the constructor itself - so we'll
;;; sitll signal that an unknown specifier is unknown on each reparse of the same.
;;; But as long as any reference enlivens the relevant CTYPE, we'll return that object.
(defglobal **unknown-type-atoms**
    ;; This table is specified as unsynchronized because we need to wrap the lock
    ;; around a read/modify/write. GETHASH and PUTHASH can't do that themselves.
    (sb-impl::make-system-hash-table :test 'eq :weakness :value :synchronized nil))

#-sb-xc-host
(progn (declaim (inline class-classoid))
       (defun class-classoid (class)
         (wrapper-classoid (sb-pcl::class-wrapper class))))

;;; Parsing of type specifiers comes in many variations:
;;;  SINGLE-VALUE-SPECIFIER-TYPE:
;;;    disallow VALUES even if single value, but allow *
;;;  SPECIFIER-TYPE:
;;;    disallow (VALUES ...) even if single value, and disallow *
;;;  VALUES-SPECIFIER-TYPE:
;;;    allow VALUES, disallow *
;;; TYPE-OR-NIL-IF-UNKNOWN:
;;;    like SPECIFIER-TYPE, but return NIL if contains unknown
;;; all the above are funneled through BASIC-PARSE-TYPESPEC.

;;; The recursive %PARSE-TYPE function is used for nested invocations
;;; of type spec parsing, passing the outermost context through on each call.
;;; Callers should use the BASIC-PARSE-TYPESPEC interface.

;;; Hint for when you bork this and/or bork the :UNPARSE methods - do:
;;; (remove-method #'print-object (find-method #'print-object nil
;;;    (list (find-class 'ctype) (find-class 't))))
;;; so that 'backtrace' doesn't encounter an infinite chain of errors.

(macrolet ((fail (spec)
             `(error "bad thing to be a type specifier: ~/sb-impl:print-type-specifier/"
                     ,spec)))
(defun %parse-type (spec context)
  (declare (type type-context context))
  (prog* ((head (if (listp spec) (car spec) spec))
          (builtin (if (symbolp head)
                       (info :type :builtin head)
                       (return (fail spec)))))
    (when (deprecated-thing-p 'type head)
      (setf (type-context-options context)
            (logior (type-context-options context) +type-parse-cache-inhibit+))
      (signal 'parse-deprecated-type :specifier spec))
    (when (atom spec)
      ;; If spec is non-atomic, the :BUILTIN value is inapplicable.
      ;; There used to be compound builtins, but not any more.
      (when builtin (return builtin))
      ;; Any spec that apparently refers to a defstruct form
      ;; that's being macroexpanded should refer to that type.
      (awhen (type-context-proto-classoid context)
        (when (eq (classoid-name it) spec) (return it)))
      (case (info :type :kind spec)
       (:instance (return (find-classoid spec)))
       (:forthcoming-defclass-type (go unknown))))
    ;; Expansion brings up an interesting question - should the cache
    ;; contain entries for intermediary types? Say A -> B -> REAL.
    ;; As it stands, we cache the ctype corresponding to A but not B.
    (awhen (info :type :expander head)
      (when (listp it) ; The function translates directly to a CTYPE.
        (return (or (funcall (car it) context spec) (fail spec))))
      ;; The function produces a type expression.
      (let ((expansion (funcall it (ensure-list spec))))
        (return (if (typep expansion 'instance)
                    (basic-parse-typespec expansion context)
                    (%parse-type expansion context)))))
    ;; If the spec is (X ...) and X has neither a translator
    ;; nor expander, and is a builtin, such as FIXNUM, fail now.
    ;; But - see FIXME at top - it would be consistent with
    ;; DEFTYPE to reject spec only if not a singleton.
    (when builtin (return (fail spec)))
    ;; SPEC has a legal form, so return an unknown type.
    (unless (logtest (type-context-options context) +type-parse-signal-inhibit+)
      (signal 'parse-unknown-type :specifier spec))
  UNKNOWN
    (setf (type-context-options context)
          (logior (type-context-options context) +type-parse-cache-inhibit+))
    (return (if (atom spec)
                (let ((table **unknown-type-atoms**))
                  (with-system-mutex ((hash-table-lock table))
                    (or (gethash spec table)
                        (progn #+sb-xc-host
                               (when cl:*compile-print*
                                 (format t "~&; NEW UNKNOWN-TYPE ~S~%" spec))
                               (setf (gethash spec table)
                                     (make-unknown-type :specifier spec))))))
                (make-unknown-type :specifier spec)))))

;;; BASIC-PARSE-TYPESPEC can grok some simple cases that involve turning an object
;;; used as a type specifier into an internalized type object (which might be
;;; the selfsame object, in the case of a CLASSOID).
;;;
;;; FIXME: nothing in the spec precludes calling TYPEP on a type specifier composed
;;; of dynamic-extent lists. Such a specifier must be uncacheable.
;;; Worse- the MEMBER type can have problems with the items per se, because the parse
;;; retains the items. There is no _practical_ reason to have such a type, as the atoms
;;; for which MEMBER tends to be used (symbol, number) can't be DX-allocated.
;;; Nonetheless, memoizing arbitrary user-supplied data is not careful enough.
(defun basic-parse-typespec (type-specifier context)
  (declare (type type-context context))
  (when (typep type-specifier 'instance)
    ;; An instance never needs the type parser cache, because it almost always
    ;; represents itself or a slot in itself.
    (flet ((classoid-to-ctype (classoid)
             ;; A few classoids have translations,
             ;; e.g. the classoid CONS is a CONS-TYPE.
             ;; Hmm, perhaps this should signal PARSE-UNKNOWN-TYPE
             ;; if CLASSOID is an instance of UNDEFINED-CLASSOID ?
             ;; Can that happen?
             (or (and (built-in-classoid-p classoid)
                      (built-in-classoid-translation classoid))
                 classoid)))
      (return-from basic-parse-typespec
       (cond ((classoid-p type-specifier) (classoid-to-ctype type-specifier))
             ;; Avoid TYPEP on SB-MOP:EQL-SPECIALIZER and CLASS because
             ;; the fake metaobjects do not allow type analysis, and
             ;; would cause a compiler error as it tries to decide
             ;; whether any clause of this COND subsumes another.
             ;; Moreover, we don't require the host to support MOP.
             #-sb-xc-host
             ((sb-pcl::classp type-specifier)
              ;; A CLOS class is translated to its CLASSOID, or the classoid's translation.
              (classoid-to-ctype (sb-pcl::class-classoid type-specifier)))
             #-sb-xc-host
             ((sb-pcl::eql-specializer-p type-specifier)
              ;; EQL specializers are are seldom used and not 100% portable,
              ;; though they are part of the AMOP.
              ;; See https://sourceforge.net/p/sbcl/mailman/message/11217378/
              ;; We implement the notion that an EQL-SPECIALIZER has-a CTYPE.
              ;; You might think that a cleverer way would be to say that
              ;; EQL-SPECIALIZER is-a CTYPE, i.e. incorporating EQL-SPECIALIZER
              ;; objects into the type machinary. Well, that's a problem -
              ;; it would mess up admissibility of the TYPE= optimization.
              ;; We don't want to create another way of representing
              ;; the type NULL = (MEMBER NIL), for example.
              (sb-pcl::eql-specializer-to-ctype type-specifier))
             ((wrapper-p type-specifier)
              (wrapper-classoid type-specifier))
             (t (fail type-specifier))))))
  (when (atom type-specifier)
    ;; Try to bypass the cache, which avoids using a cache line for standard
    ;; atomic specifiers. This is a trade-off- cache seek might be faster,
    ;; but this solves the problem that a full call to (TYPEP #\A 'FIXNUM)
    ;; consed a cache line every time the cache missed on FIXNUM (etc).
    (awhen (info :type :builtin type-specifier)
        (return-from basic-parse-typespec it)))

  ;; If CONTEXT was non-cacheable as supplied, the cache is bypassed
  ;; for any nested lookup, and we don't insert the result.
  (if (not (type-context-cacheable context))
      (%parse-type (uncross type-specifier) context)
      ;; Otherwise, try for a cache hit first, and usually update the cache.
      (!values-specifier-type-memo-wrapper
       (lambda ()
         (let ((answer (%parse-type (uncross type-specifier) context)))
           (if (type-context-cacheable context)
               answer
                 ;; Lookup was cacheable, but result isn't.
                 ;; Non-caching ensures that we see every occurrence of an unknown
                 ;; type no matter how deeply nested it is in the expression.
                 ;; e.g. (OR UNKNOWN-FOO CONS) and (OR INTEGER UNKNOWN-FOO)
                 ;; should both signal the PARSE-UNKNOWN condition, which would
                 ;; not happen if the first cached UNKNOWN-FOO.

                 ;; During make-host-2 I'm seeing the types &OPTIONAL-AND-&KEY-IN-LAMBDA-LIST,
                 ;; SIMPLE-ERROR, DISASSEM-STATE as non-cacheable,
                 ;; and much, much more during make-target-2.
                 ;; The condition types are obvious, because we mention them before
                 ;; defining them.
                 ;; DISASSEM-STATE comes from building **TYPE-SPEC-INTERR-SYMBOLS**
                 ;; where we have a fixed list of types which get assigned single-byte
                 ;; error codes.
               (progn
                   #+nil
                   (unless (type-context-cacheable context)
                     (format t "~&non-cacheable: ~S ~%" type-specifier))
                   (return-from basic-parse-typespec answer)))))
       type-specifier)))
) ; end MACROLET

;;; This takes no CONTEXT (which implies lack of recursion) because
;;; you can't reasonably place a VALUES type inside another type.
(defun values-specifier-type (type-specifier)
  ;; This catches uses of literal '* where it shouldn't appear, but it
  ;; accidentally lets other uses slip through. We'd have to catch '*
  ;; post-type-expansion to be more strict, but it isn't very important.
  (cond ((eq type-specifier '*)
         (warn "* is not permitted as a type specifier")
         *universal-type*)
        (t
         (dx-let ((context (make-type-context type-specifier)))
           (basic-parse-typespec type-specifier context)))))

;;; This is like VALUES-SPECIFIER-TYPE, except that we guarantee to
;;; never return a VALUES type.
;;; CONTEXT is either an instance of TYPE-CONTEXT or NIL.
;;; SUBCONTEXT is a symbol denoting the head of the current expression, or NIL.
(defun specifier-type (type-specifier &optional context subcontext)
  (let* ((ctype
           (if context
               (basic-parse-typespec type-specifier context)
               (dx-let ((context (make-type-context type-specifier)))
                 (basic-parse-typespec type-specifier context))))
         (wildp (eq ctype *wild-type*)))
    ;; We have to see how it was spelled to give an intelligent message.
    ;; If it's instance of VALUES-TYPE, then it was spelled as VALUES
    ;; whereas if it isn't, the user either spelled it as (VALUES) or *.
    ;; The case where this heuristic doesn't work is a DEFTYPE that expands
    ;; to *, but that's not worth worrying about.
    (cond ((or (values-type-p ctype)
               (and wildp (consp type-specifier)))
           (error "VALUES type illegal in this context:~% ~
               ~/sb-impl:print-type-specifier/"
                  type-specifier))
          (wildp
           (when context
             (setf (type-context-options context)
                   (logior (type-context-options context)
                           +type-parse-cache-inhibit+)))
           (if subcontext
               (warn "* is not permitted as an argument to the ~S type specifier"
                     subcontext)
               (warn "* is not permitted as a type specifier~@[ in the context ~S~]"
                     ;; If the entire surrounding context is * then there's not much
                     ;; else to say. Otherwise, show the original expression.
                     (when (and context (neq (type-context-spec context) '*))
                       (type-context-spec context))))
           *universal-type*)
          (t
           ctype))))

(defun single-value-specifier-type (x &optional context)
  (if (eq x '*)
      *universal-type*
      (specifier-type x context)))

;;; When cross-compiling SPECIFIER-TYPE with a quoted argument,
;;; it can be rendered as a literal object unless it mentions
;;; certain classoids.
;;;
;;; This is important for type system initialization.
;;;
;;; After the target is built, we remove this transform, both because calls
;;; to SPECIFIER-TYPE do not arise organically through user code,
;;; and because it is possible that user changes to types could make parsing
;;; return a different thing, e.g. changing a DEFTYPE to a DEFCLASS.
;;;
#+sb-xc-host
(labels ((xform (type-spec env parser)
           (if (not (constantp type-spec env))
               (values nil t)
               (let* ((expr (constant-form-value type-spec env))
                      (parse (funcall parser expr)))
                 (if (cold-dumpable-type-p parse)
                     parse
                     (values nil t)))))
         (cold-dumpable-type-p (ctype)
           (when (contains-unknown-type-p ctype)
             (bug "SPECIFIER-TYPE transform parsed an unknown type: ~S" ctype))
           (map-type (lambda (type)
                       (when (and (classoid-p type) (eq (classoid-name type) 'class))
                         (return-from cold-dumpable-type-p nil)))
                     ctype)
           t))
  (sb-c:define-source-transform specifier-type (type-spec &environment env)
    (xform type-spec env #'specifier-type))
  (sb-c:define-source-transform values-specifier-type (type-spec &environment env)
    (xform type-spec env #'values-specifier-type)))

(defun typexpand-1 (type-specifier &optional env)
  "Takes and expands a type specifier once like MACROEXPAND-1.
Returns two values: the expansion, and a boolean that is true when
expansion happened."
  (declare (type type-specifier type-specifier))
  (declare (type lexenv-designator env) (ignore env))
  (let* ((spec type-specifier)
         (atom (if (listp spec) (car spec) spec))
         (expander (and (symbolp atom) (info :type :expander atom))))
               ;; We do not expand builtins even though it'd be
               ;; possible to do so sometimes (e.g. STRING) for two
               ;; reasons:
               ;;
               ;; a) From a user's point of view, CL types are opaque.
               ;;
               ;; b) so (EQUAL (TYPEXPAND 'STRING) (TYPEXPAND-ALL 'STRING))
    (if (and (functionp expander) (not (info :type :builtin atom)))
        (values (funcall expander (if (symbolp spec) (list spec) spec)) t)
        (values type-specifier nil))))

(defun typexpand (type-specifier &optional env)
  "Takes and expands a type specifier repeatedly like MACROEXPAND.
Returns two values: the expansion, and a boolean that is true when
expansion happened."
  ;; TYPE-SPECIFIER is of type TYPE-SPECIFIER, but it is preferable to
  ;; defer to TYPEXPAND-1 for the typecheck. Similarly for ENV.
  (multiple-value-bind (expansion expanded)
      (typexpand-1 type-specifier env)
    (if expanded
        (values (typexpand expansion env) t)
        (values expansion expanded))))

;;; Take a list of type specifiers, computing the translation of each
;;; specifier and defining it as a builtin type.
;;; Seee the comments in 'type-init' for why this is a slightly
;;; screwy way to go about it.
(declaim (ftype (function (list) (values)) !precompute-types))
(defun !precompute-types (specs)
  (dolist (spec specs)
    (let ((res (handler-bind
                   ((parse-unknown-type
                     (lambda (c)
                       (declare (ignore c))
                       ;; We can handle conditions at this point,
                       ;; but win32 can not perform i/o here because
                       ;; !MAKE-COLD-STDERR-STREAM has no implementation.
                       ;; FIXME: where is this coming from???
                       #+nil
                       (progn (write-string "//caught: parse-unknown ")
                              (write spec)
                              (terpri)))))
             (specifier-type spec))))
      (unless (unknown-type-p res)
        (setf (info :type :builtin spec) res)
        (setf (info :type :kind spec) :primitive))))
  (values))

;;; Parse TYPE-SPECIFIER, returning NIL if any sub-part of it is unknown
(defun type-or-nil-if-unknown (type-specifier &optional allow-values)
  (dx-let ((context (make-type-context type-specifier)))
    (let ((result (if allow-values
                      (basic-parse-typespec type-specifier context)
                      (specifier-type type-specifier context))))
      ;; If it was non-cacheable, either it contained a deprecated type
      ;; or unknown type, or was a pending defstruct definition.
      (if (and (not (type-context-cacheable context))
               (contains-unknown-type-p result))
          nil
          result))))

(defun-cached (type-negation :hash-function #'type-hash-value
                             :hash-bits 8
                             :values 1)
              ((type eq))
  (declare (type ctype type))
  (funcall (type-class-negate (type-class type)) type))

(defun-cached (type-singleton-p :hash-function #'type-hash-value
                             :hash-bits 8
                             :values 2)
              ((type eq))
  (declare (type ctype type))
  (let ((function (type-class-singleton-p (type-class type))))
    (if function
        (funcall function type)
        (values nil nil))))


;;;; general TYPE-UNION and TYPE-INTERSECTION operations
;;;;
;;;; These are fully general operations on CTYPEs: they'll always
;;;; return a CTYPE representing the result.

;;; shared logic for unions and intersections: Return a list of
;;; types representing the same types as INPUT-TYPES, but with
;;; COMPOUND-TYPEs satisfying %COMPOUND-TYPE-P broken up into their
;;; component types, and with any SIMPLIFY2 simplifications applied.
(macrolet
    ((def (name compound-type-p simplify2)
         `(defun ,name (types)
            (when types
              (multiple-value-bind (first rest)
                  (if (,compound-type-p (car types))
                      (values (car (compound-type-types (car types)))
                              (append (cdr (compound-type-types (car types)))
                                      (cdr types)))
                      (values (car types) (cdr types)))
                (let ((rest (,name rest)) u)
                  (dolist (r rest (cons first rest))
                    (when (setq u (,simplify2 first r))
                      (return (,name (nsubstitute u r rest)))))))))))
  (def simplify-intersections intersection-type-p type-intersection2)
  (def simplify-unions union-type-p type-union2))

(defun maybe-distribute-one-union (union-type types)
  (let* ((intersection (%type-intersection types))
         (union (mapcar (lambda (x) (type-intersection x intersection))
                        (union-type-types union-type))))
    (if (notany (lambda (x) (or (hairy-type-p x)
                                (intersection-type-p x)))
                union)
        union
        nil)))

(defun type-intersection (&rest input-types)
  (%type-intersection input-types))
(defun-cached (%type-intersection :hash-bits 10 :hash-function #'type-list-cache-hash)
    ((input-types equal))
  (let ((simplified-types (simplify-intersections input-types)))
    (declare (type list simplified-types))
    ;; We want to have a canonical representation of types (or failing
    ;; that, punt to HAIRY-TYPE). Canonical representation would have
    ;; intersections inside unions but not vice versa, since you can
    ;; always achieve that by the distributive rule. But we don't want
    ;; to just apply the distributive rule, since it would be too easy
    ;; to end up with unreasonably huge type expressions. So instead
    ;; we try to generate a simple type by distributing the union; if
    ;; the type can't be made simple, we punt to HAIRY-TYPE.
    (if (and (cdr simplified-types) (some #'union-type-p simplified-types))
        (let* ((first-union (find-if #'union-type-p simplified-types))
               (other-types (coerce (remove first-union simplified-types)
                                    'list))
               (distributed (maybe-distribute-one-union first-union
                                                        other-types)))
          (if distributed
              (%type-union distributed)
              (%make-hairy-type `(and ,@(map 'list #'type-specifier
                                             simplified-types)))))
        (cond
          ((null simplified-types) *universal-type*)
          ((null (cdr simplified-types)) (car simplified-types))
          (t (%make-intersection-type
              (some #'type-enumerable simplified-types)
              simplified-types))))))

(defun type-union (&rest input-types)
  (%type-union input-types))
(defun-cached (%type-union :hash-bits 8 :hash-function #'type-list-cache-hash)
    ((input-types equal))
  (let ((simplified-types (simplify-unions input-types)))
    (cond
      ((null simplified-types) *empty-type*)
      ((null (cdr simplified-types)) (car simplified-types))
      (t (make-union-type
          (every #'type-enumerable simplified-types)
          simplified-types)))))

;;;; built-in types

(define-type-method (named :simple-=) (type1 type2)
  ;;(aver (not (eq type1 *wild-type*))) ; * isn't really a type.
  (values (eq type1 type2) t))

(defun cons-type-might-be-empty-type (type)
  (declare (type cons-type type))
  (let ((car-type (cons-type-car-type type))
        (cdr-type (cons-type-cdr-type type)))
    (or
     (if (cons-type-p car-type)
         (cons-type-might-be-empty-type car-type)
         (multiple-value-bind (yes surep)
             (type= car-type *empty-type*)
           (aver (not yes))
           (not surep)))
     (if (cons-type-p cdr-type)
         (cons-type-might-be-empty-type cdr-type)
         (multiple-value-bind (yes surep)
             (type= cdr-type *empty-type*)
           (aver (not yes))
           (not surep))))))

(defun cons-type-length-info (type)
  (declare (type cons-type type))
  (do ((min 1 (1+ min))
       (cdr (cons-type-cdr-type type) (cons-type-cdr-type cdr)))
      ((not (cons-type-p cdr))
       (cond
         ((csubtypep cdr (specifier-type 'null))
          (values min t))
         ((csubtypep *universal-type* cdr)
          (values min nil))
         ((type/= (type-intersection (specifier-type 'cons) cdr) *empty-type*)
          (values min nil))
         ((type/= (type-intersection (specifier-type 'null) cdr) *empty-type*)
          (values min t))
         (t (values min :maybe))))
    ()))

(define-type-method (named :complex-=) (type1 type2)
  (cond
    ((and (eq type2 *empty-type*)
          (or (and (intersection-type-p type1)
                   ;; not allowed to be unsure on these... FIXME: keep
                   ;; the list of CL types that are intersection types
                   ;; once and only once.
                   (not (or (type= type1 (specifier-type 'ratio))
                            (type= type1 (specifier-type 'keyword)))))
              (and (cons-type-p type1)
                   (cons-type-might-be-empty-type type1))))
     ;; things like (AND (EQL 0) (SATISFIES ODDP)) or (AND FUNCTION
     ;; STREAM) can get here.  In general, we can't really tell
     ;; whether these are equal to NIL or not, so
     (values nil nil))
    ((type-might-contain-other-types-p type1)
     (invoke-complex-=-other-method type1 type2))
    (t (values nil t))))

(define-type-method (named :simple-subtypep) (type1 type2)
  (aver (not (eq type1 *wild-type*))) ; * isn't really a type.
  (aver (not (eq type1 type2)))
  (values (or (eq type1 *empty-type*)
              (eq type2 *wild-type*)
              (eq type2 *universal-type*)) t))

(define-type-method (named :complex-subtypep-arg1) (type1 type2)
  ;; This AVER causes problems if we write accurate methods for the
  ;; union (and possibly intersection) types which then delegate to
  ;; us; while a user shouldn't get here, because of the odd status of
  ;; *wild-type* a type-intersection executed by the compiler can. -
  ;; CSR, 2002-04-10
  ;;
  ;; (aver (not (eq type1 *wild-type*))) ; * isn't really a type.
  (cond ((eq type1 *empty-type*)
         t)
        (;; When TYPE2 might be the universal type in disguise
         (type-might-contain-other-types-p type2)
         ;; Now that the UNION and HAIRY COMPLEX-SUBTYPEP-ARG2 methods
         ;; can delegate to us (more or less as CALL-NEXT-METHOD) when
         ;; they're uncertain, we can't just barf on COMPOUND-TYPE and
         ;; HAIRY-TYPEs as we used to. Instead we deal with the
         ;; problem (where at least part of the problem is cases like
         ;;   (SUBTYPEP T '(SATISFIES FOO))
         ;; or
         ;;   (SUBTYPEP T '(AND (SATISFIES FOO) (SATISFIES BAR)))
         ;; where the second type is a hairy type like SATISFIES, or
         ;; is a compound type which might contain a hairy type) by
         ;; returning uncertainty.
         (values nil nil))
        ((eq type1 *funcallable-instance-type*)
         (values (eq type2 (specifier-type 'function)) t))
        (t
         ;; This case would have been picked off by the SIMPLE-SUBTYPEP
         ;; method, and so shouldn't appear here.
         (aver (not (named-type-p type2)))
         ;; Since TYPE2 is not EQ *UNIVERSAL-TYPE* and is not another
         ;; named type in disguise, TYPE2 is not a superset of TYPE1.
         (values nil t))))

;;; Return T if members of this classoid certainly have INSTANCE-POINTER-LOWTAG.
;;; Logically it is the near opposite of CLASSOID-NON-INSTANCE-P, but not quite.
;;; CTYPEs which are not represented as a classoid return NIL for both predicates
;;; as do PCL types which may be either funcallable or non-funcallable.
;;;
;;; But some of that generality seems wrong. I don't think it would be allowed
;;; to have (as merely an example) an EQL-SPECIALIZER which is funcallable,
;;; having FUN-POINTER-LOWTAG instead of INSTANCE-POINTER-LOWTAG). Yet we think
;;; it could happen, because the parse of the type (AND EQL-SPECIALIZER INSTANCE)
;;; yields #<INTERSECTION-TYPE (AND SB-MOP:EQL-SPECIALIZER INSTANCE)>
;;; versus simplifying down to EQL-SPECIALIZER.
#| (loop for c being each hash-key of (classoid-subclasses (find-classoid 't))
      do (let ((not-i (classoid-non-instance-p c))
               (i (classoid-definitely-instancep c)))
           (unless (eq (not not-i) i) (format t "~S -> ~A and ~A~%" c not-i i)))) |#
(defun classoid-definitely-instancep (x)
  (or (structure-classoid-p x)
      (condition-classoid-p x)
      ;; PATHNAMEs are INSTANCEs based on the lowtag criterion
      (or (eq x (specifier-type 'logical-pathname))
          (eq x (specifier-type 'pathname)))))
(eval-when (:compile-toplevel :execute)
  (pushnew 'classoid-definitely-instancep sb-vm::*backend-cross-foldable-predicates*))

(defun classoid-is-or-inherits (sub super)
  (or (classoid-inherits-from sub super)
      (eq sub (find-classoid super))))

(define-type-method (named :complex-subtypep-arg2) (type1 type2)
  (aver (not (eq type2 *wild-type*))) ; * isn't really a type.
  (cond ((eq type2 *universal-type*)
         (values t t))
        ;; some CONS types can conceal danger
        ((and (cons-type-p type1) (cons-type-might-be-empty-type type1))
         (values nil nil))
        ((type-might-contain-other-types-p type1)
         ;; those types can be other types in disguise.  So we'd
         ;; better delegate.
         (invoke-complex-subtypep-arg1-method type1 type2))
        ((and (or (eq type2 *instance-type*)
                  (eq type2 *funcallable-instance-type*))
              (member-type-p type1))
         ;; member types can be subtypep INSTANCE and
         ;; FUNCALLABLE-INSTANCE in surprising ways.
         (invoke-complex-subtypep-arg1-method type1 type2))
        ((and (eq type2 *extended-sequence-type*) (classoid-p type1))
         (values (if (classoid-inherits-from type1 'sequence) t nil) t))
        ((and (eq type2 *instance-type*) (classoid-p type1))
         (cond ((or (classoid-non-instance-p type1)
                    (classoid-is-or-inherits type1 'function))
                (values nil t))
               ((classoid-definitely-instancep type1)
                (values t t))
               (t
                (values nil nil))))
        ((and (eq type2 *funcallable-instance-type*) (classoid-p type1))
         (if (and (not (classoid-non-instance-p type1))
                  (classoid-inherits-from type1 'function))
             (values t t)
             (values nil t)))
        (t
         ;; FIXME: This seems to rely on there only being 4 or 5
         ;; NAMED-TYPE values, and the exclusion of various
         ;; possibilities above. It would be good to explain it and/or
         ;; rewrite it so that it's clearer.
         (values nil t))))

(define-type-method (named :simple-intersection2) (type1 type2)
  (cond
    ((and (eq type1 *extended-sequence-type*)
          (or (eq type2 *instance-type*)
              (eq type2 *funcallable-instance-type*)))
     nil)
    ((and (or (eq type1 *instance-type*)
              (eq type1 *funcallable-instance-type*))
          (eq type2 *extended-sequence-type*))
     nil)
    (t
     (hierarchical-intersection2 type1 type2))))

(define-type-method (named :complex-intersection2) (type1 type2)
  ;; FIXME: This assertion failed when I added it in sbcl-0.6.11.13.
  ;; Perhaps when bug 85 is fixed it can be reenabled.
  ;;(aver (not (eq type2 *wild-type*))) ; * isn't really a type.
  (flet ((empty-unless-hairy (type)
           (unless (or (type-might-contain-other-types-p type)
                       (member-type-p type))
             *empty-type*)))
    (cond
      ((eq type2 *extended-sequence-type*)
       (typecase type1
         ((satisfies classoid-definitely-instancep) *empty-type*) ; dubious!
         (classoid (cond
                     ((classoid-non-instance-p type1) *empty-type*)
                     ((classoid-inherits-from type1 'sequence) type1)))
         (t (empty-unless-hairy type1))))
      ((eq type2 *instance-type*)
       (typecase type1
         ((satisfies classoid-definitely-instancep) type1)
         (classoid (when (or (classoid-non-instance-p type1)
                             (classoid-is-or-inherits type1 'function))
                     *empty-type*))
         (t (empty-unless-hairy type1))))
      ((eq type2 *funcallable-instance-type*)
       (typecase type1
         ((satisfies classoid-definitely-instancep) *empty-type*)
         (classoid
          (cond
            ((classoid-non-instance-p type1) *empty-type*)
            ((classoid-inherits-from type1 'function) type1)
            ((type= type1 (find-classoid 'function)) type2)))
         (fun-type nil)
         (t (empty-unless-hairy type1))))
      (t (hierarchical-intersection2 type1 type2)))))

(define-type-method (named :complex-union2) (type1 type2)
  ;; Perhaps when bug 85 is fixed this can be reenabled.
  ;;(aver (not (eq type2 *wild-type*))) ; * isn't really a type.
  (cond
    ((eq type2 *extended-sequence-type*)
     (cond ((not (classoid-p type1)) nil)
           ((and (not (classoid-non-instance-p type1))
                 (classoid-inherits-from type1 'sequence))
            type2)))
    ((eq type2 *instance-type*)
     (when (and (classoid-p type1)
                (neq type1 (specifier-type 'function))
                (not (classoid-non-instance-p type1))
                (not (classoid-inherits-from type1 'function)))
       type2))
    ((eq type2 *funcallable-instance-type*)
     (cond ((not (classoid-p type1)) nil)
           ((classoid-non-instance-p type1) nil)
           ((not (classoid-inherits-from type1 'function)) nil)
           ((eq type1 (specifier-type 'function)) type1)
           (t type2)))
    (t (hierarchical-union2 type1 type2))))

(define-type-method (named :negate) (x)
  (aver (not (eq x *wild-type*)))
  (cond
    ((eq x *universal-type*) *empty-type*)
    ((eq x *empty-type*) *universal-type*)
    ((or (eq x *instance-type*)
         (eq x *funcallable-instance-type*)
         (eq x *extended-sequence-type*))
     (make-negation-type x))
    (t (bug "NAMED type unexpected: ~S" x))))

(define-type-method (named :unparse) (x)
  (named-type-name x))

;;;; hairy and unknown types

;; ENUMERABLE-P is T because a hairy type could be equivalent to a MEMBER type.
;; e.g. any SATISFIES with a predicate returning T over a finite domain.
;; But in practice there's nothing that can be done with this information,
;; because we don't call random predicates when performing operations on types
;; as objects, only when checking for inclusion of something in the type.
(define-type-class hairy :enumerable t :might-contain-other-types t)

;;; Without some special HAIRY cases, we massively pollute the type caches
;;; with objects that are all equivalent to *EMPTY-TYPE*. e.g.
;;;  (AND (SATISFIES LEGAL-FUN-NAME-P) (SIMPLE-ARRAY CHARACTER (*))) and
;;;  (AND (SATISFIES KEYWORDP) CONS). Since the compiler doesn't know
;;; that they're just *EMPTY-TYPE*, its keeps building more and more complex
;;; expressions involving them. I'm not sure why those two are so prevalent
;;; but they definitely seem to be.  We can improve performance by reducing
;;; them to *EMPTY-TYPE* which means we need a way to recognize those hairy
;;; types in order reason about them. Interning them is how we recognize
;;; them, as they can be compared by EQ.
#+sb-xc-host
(progn
  (defvar *satisfies-keywordp-type*
    (!make-interned-hairy-type '(satisfies keywordp)))
  (defvar *fun-name-type*
    (!make-interned-hairy-type '(satisfies legal-fun-name-p))))

(define-type-method (hairy :negate) (x) (make-negation-type x))

(define-type-method (hairy :unparse) (x)
  (hairy-type-specifier x))

(define-type-method (hairy :simple-subtypep) (type1 type2)
  (let ((hairy-spec1 (hairy-type-specifier type1))
        (hairy-spec2 (hairy-type-specifier type2)))
    (cond ((list-elements-eql hairy-spec1 hairy-spec2)
           (values t t))
          ((maybe-reparse-specifier! type1)
           (csubtypep type1 type2))
          ((maybe-reparse-specifier! type2)
           (csubtypep type1 type2))
          (t
           (values nil nil)))))

(define-type-method (hairy :complex-subtypep-arg2) (type1 type2)
  (if (maybe-reparse-specifier! type2)
      (csubtypep type1 type2)
      (let ((specifier (hairy-type-specifier type2)))
        (cond ((and (consp specifier) (eql (car specifier) 'satisfies))
               (case (cadr specifier)
                 ((keywordp) (if (type= type1 (specifier-type 'symbol))
                                 (values nil t)
                                 (invoke-complex-subtypep-arg1-method type1 type2)))
                 (t (invoke-complex-subtypep-arg1-method type1 type2))))
              (t
               (invoke-complex-subtypep-arg1-method type1 type2))))))

(define-type-method (hairy :complex-subtypep-arg1) (type1 type2)
  (if (maybe-reparse-specifier! type1)
      (csubtypep type1 type2)
      (values nil nil)))

(define-type-method (hairy :complex-=) (type1 type2)
  (if (maybe-reparse-specifier! type2)
      (type= type1 type2)
      (values nil nil)))

(define-type-method (hairy :simple-intersection2 :complex-intersection2)
                     (type1 type2)
 (acond ((type= type1 type2)
         type1)
        ((eq type2 (literal-ctype *satisfies-keywordp-type*))
         ;; (AND (MEMBER A) (SATISFIES KEYWORDP)) is possibly non-empty
         ;; if A is re-homed as :A. However as a special case that really
         ;; does occur, (AND (MEMBER NIL) (SATISFIES KEYWORDP))
         ;; is empty because of the illegality of changing NIL's package.
         (if (eq type1 (specifier-type 'null))
             *empty-type*
             (multiple-value-bind (answer certain)
                 (types-equal-or-intersect type1 (specifier-type 'symbol))
               (and (not answer) certain *empty-type*))))
        ((eq type2 (literal-ctype *fun-name-type*))
         (multiple-value-bind (answer certain)
             (types-equal-or-intersect type1 (specifier-type 'symbol))
           (and (not answer)
                certain
                (multiple-value-bind (answer certain)
                    (types-equal-or-intersect type1 (specifier-type 'cons))
                  (and (not answer) certain *empty-type*)))))
        ((and (typep (hairy-type-specifier type2) '(cons (eql satisfies)))
              (info :function :predicate-truth-constraint
                    (cadr (hairy-type-specifier type2))))
         (multiple-value-bind (answer certain)
             (types-equal-or-intersect type1 (specifier-type it))
           (and (not answer) certain *empty-type*)))))

(define-type-method (hairy :simple-union2)
                     (type1 type2)
  (if (type= type1 type2)
      type1
      nil))

(define-type-method (hairy :simple-=) (type1 type2)
  ;; Specifiers really want to be compared by something that is more liberal than EQL
  ;; but it doesn't really matter too much because the containing objects would probably
  ;; be EQ if there was a cache hit on parsing.
  (if (list-elements-eql (hairy-type-specifier type1) (hairy-type-specifier type2))
      (values t t)
      (values nil nil)))

;;; This list exists so that we can turn builtin (SATISFIES fn) types into types
;;; amenable to algebra, because apparently there are some masochistic users
;;; who expect (SUBTYPEP 'COMPLEX '(AND NUMBER (SATISFIES REALP))) => NIL and T.
;;; There are possibly other entries that could go here,
;;; e.g. (SATISFIES ARRAY-HEADER-P) is something involving the AND, NOT, OR
;;; combinators. But it might render the expression too hairy to operate on.
(dolist (pair '((arrayp array)
                (atom atom)
                (bit-vector-p bit-vector)
                (characterp character)
                (compiled-function-p compiled-function)
                (complexp complex)
                (consp cons)
                (floatp float)
                (functionp function)
                (hash-table-p hash-table)
                (integerp integer)
                ;; KEYWORD is (SATISFIES KEYWORDP), so we can't turn
                ;; the predicate into KEYWORD
                (listp list)
                (numberp number)
                (packagep package)
                (pathnamep pathname)
                (random-state-p random-state)
                (rationalp rational)
                (readtablep readtable)
                (realp real)
                (simple-bit-vector-p simple-bit-vector)
                (simple-string-p simple-string)
                (simple-vector-p simple-vector)
                (streamp stream)
                (stringp string)
                (symbolp symbol)
                (vectorp vector)))
  (destructuring-bind (function type) pair
    (setf (info :function :predicate-for function) type)))

(def-type-translator satisfies :list (&whole whole predicate-name)
  ;; "* may appear as the argument to a SATISFIES type specifier, but it
  ;;  indicates the literal symbol *" (which in practice is not useful)
  (unless (symbolp predicate-name)
    (error 'simple-type-error
           :datum predicate-name
           :expected-type 'symbol
           :format-control "The SATISFIES predicate name is not a symbol: ~S"
           :format-arguments (list predicate-name)))
  (case predicate-name
   (keywordp (literal-ctype *satisfies-keywordp-type*))
   (legal-fun-name-p (literal-ctype *fun-name-type*))
   (adjustable-array-p (specifier-type '(and array (not simple-array))))
   (t (let ((type (info :function :predicate-for predicate-name)))
        (if type
            (specifier-type type)
            (%make-hairy-type whole))))))

;;;; negation types

;; Former comment was:
;;   FIXME: is this right?  It's what they had before, anyway
;; But I think the reason it's right is that "enumerable :t" is equivalent
;; to "maybe" which is actually the conservative assumption, same as HAIRY.
(define-type-class negation :enumerable t :might-contain-other-types t)

(define-type-method (negation :negate) (x)
  (negation-type-type x))

(define-type-method (negation :unparse) (x)
  (if (type= (negation-type-type x) (specifier-type 'cons))
      'atom
      `(not ,(type-specifier (negation-type-type x)))))

(define-type-method (negation :simple-subtypep) (type1 type2)
  (csubtypep (negation-type-type type2) (negation-type-type type1)))

(define-type-method (negation :complex-subtypep-arg2) (type1 type2)
  (let* ((complement-type2 (negation-type-type type2))
         (intersection2 (type-intersection2 type1
                                            complement-type2)))
    (if intersection2
        ;; FIXME: if uncertain, maybe try arg1?
        (type= intersection2 *empty-type*)
        (invoke-complex-subtypep-arg1-method type1 type2))))

(define-type-method (negation :complex-subtypep-arg1) (type1 type2)
  ;; "Incrementally extended heuristic algorithms tend inexorably toward the
  ;; incomprehensible." -- http://www.unlambda.com/~james/lambda/lambda.txt
  ;;
  ;; You may not believe this. I couldn't either. But then I sat down
  ;; and drew lots of Venn diagrams. Comments involving a and b refer
  ;; to the call (subtypep '(not a) 'b) -- CSR, 2002-02-27.
  (block nil
    ;; (Several logical truths in this block are true as long as
    ;; b/=T. As of sbcl-0.7.1.28, it seems impossible to construct a
    ;; case with b=T where we actually reach this type method, but
    ;; we'll test for and exclude this case anyway, since future
    ;; maintenance might make it possible for it to end up in this
    ;; code.)
    (multiple-value-bind (equal certain)
        (type= type2 *universal-type*)
      (unless certain
        (return (values nil nil)))
      (when equal
        (return (values t t))))
    (let ((complement-type1 (negation-type-type type1)))
      ;; Do the special cases first, in order to give us a chance if
      ;; subtype/supertype relationships are hairy.
      (multiple-value-bind (equal certain)
          (type= complement-type1 type2)
        ;; If a = b, ~a is not a subtype of b (unless b=T, which was
        ;; excluded above).
        (unless certain
          (return (values nil nil)))
        (when equal
          (return (values nil t))))
      ;; KLUDGE: ANSI requires that the SUBTYPEP result between any
      ;; two built-in atomic type specifiers never be uncertain. This
      ;; is hard to do cleanly for the built-in types whose
      ;; definitions include (NOT FOO), i.e. CONS and RATIO. However,
      ;; we can do it with this hack, which uses our global knowledge
      ;; that our implementation of the type system uses disjoint
      ;; implementation types to represent disjoint sets (except when
      ;; types are contained in other types).  (This is a KLUDGE
      ;; because it's fragile. Various changes in internal
      ;; representation in the type system could make it start
      ;; confidently returning incorrect results.) -- WHN 2002-03-08
      (unless (or (type-might-contain-other-types-p complement-type1)
                  (type-might-contain-other-types-p type2))
        ;; Because of the way our types which don't contain other
        ;; types are disjoint subsets of the space of possible values,
        ;; (SUBTYPEP '(NOT AA) 'B)=NIL when AA and B are simple (and B
        ;; is not T, as checked above).
        (return (values nil t)))
      ;; The old (TYPE= TYPE1 TYPE2) branch would never be taken, as
      ;; TYPE1 and TYPE2 will only be equal if they're both NOT types,
      ;; and then the :SIMPLE-SUBTYPEP method would be used instead.
      ;; But a CSUBTYPEP relationship might still hold:
      (multiple-value-bind (equal certain)
          (csubtypep complement-type1 type2)
        ;; If a is a subtype of b, ~a is not a subtype of b (unless
        ;; b=T, which was excluded above).
        (unless certain
          (return (values nil nil)))
        (when equal
          (return (values nil t))))
      (multiple-value-bind (equal certain)
          (csubtypep type2 complement-type1)
        ;; If b is a subtype of a, ~a is not a subtype of b.  (FIXME:
        ;; That's not true if a=T. Do we know at this point that a is
        ;; not T?)
        (unless certain
          (return (values nil nil)))
        (when equal
          (return (values nil t))))
      ;; old CSR comment ca. 0.7.2, now obsoleted by the SIMPLE-CTYPE?
      ;; KLUDGE case above: Other cases here would rely on being able
      ;; to catch all possible cases, which the fragility of this type
      ;; system doesn't inspire me; for instance, if a is type= to ~b,
      ;; then we want T, T; if this is not the case and the types are
      ;; disjoint (have an intersection of *empty-type*) then we want
      ;; NIL, T; else if the union of a and b is the *universal-type*
      ;; then we want T, T. So currently we still claim to be unsure
      ;; about e.g. (subtypep '(not fixnum) 'single-float).
      ;;
      ;; OTOH we might still get here:
      (values nil nil))))

(define-type-method (negation :complex-=) (type1 type2)
  ;; (NOT FOO) isn't equivalent to anything that's not a negation
  ;; type, except possibly a type that might contain it in disguise.
  (declare (ignore type2))
  (if (type-might-contain-other-types-p type1)
      (values nil nil)
      (values nil t)))

(define-type-method (negation :simple-intersection2) (type1 type2)
  (let ((not1 (negation-type-type type1))
        (not2 (negation-type-type type2)))
    (cond
      ((csubtypep not1 not2) type2)
      ((csubtypep not2 not1) type1)
      ;; Why no analagous clause to the disjoint in the SIMPLE-UNION2
      ;; method, below?  The clause would read
      ;;
      ;; ((EQ (TYPE-UNION NOT1 NOT2) *UNIVERSAL-TYPE*) *EMPTY-TYPE*)
      ;;
      ;; but with proper canonicalization of negation types, there's
      ;; no way of constructing two negation types with union of their
      ;; negations being the universal type.
      (t
       (aver (not (eq (type-union not1 not2) *universal-type*)))
       nil))))

(defun maybe-complex-array-refinement (type1 type2)
  ;; a :MAYBE complex array <type> intersected with (NOT <type'>)
  ;; where <type'> is the same in all aspects as <type> except that
  ;; its complexp value is in {T,NIL} should return <type> altered
  ;; with its COMPLEXP being the negation of the value from <type'>.
  ;; As a particular case which is no longer special in handling it,
  ;; the righthand side could be TYPE= to (NOT SIMPLE-ARRAY)
  ;; which will match any lefthand side and do what it always did.
  (let* ((ntype (negation-type-type type2))
         (ndims (array-type-dimensions ntype))
         (ncomplexp (array-type-complexp ntype))
         (nseltype (array-type-specialized-element-type ntype))
         (neltype (array-type-element-type ntype)))
    (when (and (eq (array-type-complexp type1) :maybe)
               (neq ncomplexp :maybe)
               (or (eql ndims '*)
                   (equal (array-type-dimensions type1) ndims))
               (or (eq nseltype *wild-type*)
                   (eq (array-type-specialized-element-type type1) nseltype))
               (or (eq neltype *wild-type*)
                   (type= (array-type-element-type type1) neltype)))
      (make-array-type (array-type-dimensions type1)
                       :complexp (not (array-type-complexp ntype))
                       :specialized-element-type (array-type-specialized-element-type type1)
                       :element-type (array-type-element-type type1)))))

(define-type-method (negation :complex-intersection2) (type1 type2)
  (cond
    ((csubtypep type1 (negation-type-type type2)) *empty-type*)
    ((eq (type-intersection type1 (negation-type-type type2)) *empty-type*)
     type1)
    ((and (array-type-p type1) (array-type-p (negation-type-type type2)))
     (maybe-complex-array-refinement type1 type2))
    (t nil)))

(define-type-method (negation :simple-union2) (type1 type2)
  (let ((not1 (negation-type-type type1))
        (not2 (negation-type-type type2)))
    (cond
      ((csubtypep not1 not2) type1)
      ((csubtypep not2 not1) type2)
      ((eq (type-intersection not1 not2) *empty-type*)
       *universal-type*)
      (t nil))))

(define-type-method (negation :complex-union2) (type1 type2)
  (cond
    ((csubtypep (negation-type-type type2) type1) *universal-type*)
    ((eq (type-intersection type1 (negation-type-type type2)) *empty-type*)
     type2)
    (t nil)))

(define-type-method (negation :simple-=) (type1 type2)
  (type= (negation-type-type type1) (negation-type-type type2)))

(def-type-translator not :list ((:context context) typespec)
  ;; "* is not permitted as an argument to the NOT type specifier."
  (type-negation (specifier-type typespec context 'not)))

;;;; numeric types

(declaim (inline numeric-type-equal))
(defun numeric-type-equal (type1 type2)
  ;; TODO: these 3 can be packed into an integer which makes for just 1 comparison.
  ;; (Maybe if 64-bit words use the %BITS slot which has plenty of unused bits)
  (and (eq (numeric-type-class type1) (numeric-type-class type2))
       (eq (numeric-type-format type1) (numeric-type-format type2))
       (eq (numeric-type-complexp type1) (numeric-type-complexp type2))))

(define-type-class number :enumerable #'numeric-type-enumerable :might-contain-other-types nil)

(define-type-method (number :simple-=) (type1 type2)
  ;; TODO: construct the hash bits for NUMBER types using a deterministic hash of
  ;; the low + high bounds. Then TYPE= can be true only if the hashes are =.
  (values
   (and (numeric-type-equal type1 type2)
        (equalp (numeric-type-low type1) (numeric-type-low type2))
        (equalp (numeric-type-high type1) (numeric-type-high type2)))
   t))

(declaim (inline bounds-unbounded-p))
(defun bounds-unbounded-p (low high)
  (and (null low) (eq high low)))

(define-type-method (number :negate) (type)
  (let ((low (numeric-type-low type))
        (high (numeric-type-high type)))
    (if (bounds-unbounded-p low high)
        (make-negation-type type)
        (type-union
         (make-negation-type (modified-numeric-type type :low nil :high nil))
         (cond
           ((null low)
            (modified-numeric-type
             type
             :low (if (consp high) (car high) (list high))
             :high nil))
           ((null high)
            (modified-numeric-type
             type
             :low nil
             :high (if (consp low) (car low) (list low))))
           (t (type-union
               (modified-numeric-type
                type
                :low nil
                :high (if (consp low) (car low) (list low)))
               (modified-numeric-type
                type
                :low (if (consp high) (car high) (list high))
                :high nil))))))))

(define-type-method (number :unparse) (type)
  (let* ((complexp (numeric-type-complexp type))
         (low (numeric-type-low type))
         (high (numeric-type-high type))
         (base (case (numeric-type-class type)
                 (integer 'integer)
                 (rational 'rational)
                 (float (or (numeric-type-format type) 'float))
                 (t 'real))))
    (let ((base+bounds
           (cond ((and (eq base 'integer) high low)
                  (let ((high-count (logcount high))
                        (high-length (integer-length high)))
                    (cond ((= low 0)
                           (cond ((= high 0) '(integer 0 0))
                                 ((= high 1) 'bit)
                                 ((and (= high-count high-length)
                                       (plusp high-length))
                                  `(unsigned-byte ,high-length))
                                 (t
                                  `(mod ,(1+ high)))))
                          ((and (= low most-negative-fixnum)
                                (= high most-positive-fixnum))
                           'fixnum)
                          ((and (= low (lognot high))
                                (= high-count high-length)
                                (> high-count 0))
                           `(signed-byte ,(1+ high-length)))
                          (t
                           `(integer ,low ,high)))))
                 (high `(,base ,(or low '*) ,high))
                 (low
                  (if (and (eq base 'integer) (= low 0))
                      'unsigned-byte
                      `(,base ,low)))
                 (t base))))
      (ecase complexp
        (:real
         (aver (neq base 'real))
         base+bounds)
        (:complex
         (aver (neq base 'real))
         `(complex ,base+bounds))
        ((nil)
         (aver (eq base+bounds 'real))
         'number)))))

(define-type-method (number :singleton-p) (type)
  (let ((low  (numeric-type-low  type))
        (high (numeric-type-high type)))
    (if (and low
             (eql low high)
             (eql (numeric-type-complexp type) :real)
             (if (eq (numeric-type-class type) 'float)
                 ;; (float 0.0 0.0) fits both -0.0 and 0.0
                 (not (zerop low))
                 (member (numeric-type-class type) '(integer rational))))
        (values t low)
        (values nil nil))))

(defun interned-numeric-type (specifier &rest args)
  (apply '%make-numeric-type
         :%bits (pack-interned-ctype-bits
                 'number nil
                 (when specifier (sb-vm::saetp-index-or-lose specifier)))
         args))

#+sb-xc-host
(progn
  ;; Work around an ABCL bug. This fails to load:
  ;;   (macrolet ((foo-it (x) `(- ,x))) (defvar *var* (foo-it 3)))
  (defvar *interned-signed-byte-types*)
  (defvar *interned-unsigned-byte-types*)
  (macrolet ((int-type (low high)
               `(interned-numeric-type (when (sb-c::find-saetp spec) spec)
                                       :class 'integer :enumerable t
                                       :low ,low :high ,high)))
    (setq *interned-signed-byte-types*
          (do ((v (make-array sb-vm:n-word-bits))
               (i 1 (1+ i))
               (j -1))
              ((> i sb-vm:n-word-bits) v)
            (let ((spec (if (= i sb-vm:n-fixnum-bits)
                            'fixnum
                            `(signed-byte ,i))))
              (setf (svref v (1- i)) (int-type j (lognot j))
                    j (ash j 1)))))
    (setq *interned-unsigned-byte-types*
          (let ((v (make-array (1+ sb-vm:n-word-bits))))
            (dotimes (i (length v) v)
              (let ((spec (if (= i 1) 'bit `(unsigned-byte ,i))))
                (setf (svref v i) (int-type 0 (1- (ash 1 i))))))))))

;;; Coerce a numeric type bound to the given type while handling
;;; exclusive bounds.
(defun coerce-numeric-bound (bound type)
  (flet ((c (thing)
           #+sb-xc-host (declare (ignore thing))
           (case type
             (rational
              #+sb-xc-host (return-from coerce-numeric-bound)
              #-sb-xc-host (if (and (floatp thing) (float-infinity-p thing))
                               (return-from coerce-numeric-bound nil)
                               (rational thing)))
             ((float single-float)
              (cond #-sb-xc-host
                    ((<= most-negative-single-float thing most-positive-single-float)
                     (coerce thing 'single-float))
                    (t
                     (return-from coerce-numeric-bound nil))))
             (double-float
              (cond #-sb-xc-host
                    ((<= most-negative-double-float thing most-positive-double-float)
                     (coerce thing 'double-float))
                    (t
                     (return-from coerce-numeric-bound nil)))))))
    (when bound
      (if (consp bound)
          (list (c (car bound)))
          (c bound)))))

(defun %make-union-numeric-type (class format complexp low high enumerable)
  (declare (type (member integer rational float nil) class))
  (macrolet ((unionize (&rest specs)
               `(type-union
                 ,@(loop for (class format coerce simple-coerce) in specs
                         collect `(make-numeric-type
                                   :class ',class
                                   :format ',format
                                   :complexp complexp
                                   :low ,(if simple-coerce
                                             `(coerce low ',coerce)
                                             `(coerce-numeric-bound low ',coerce))
                                   :high ,(if simple-coerce
                                              `(coerce high ',coerce)
                                              `(coerce-numeric-bound high ',coerce))
                                   :enumerable enumerable)))))
    (cond ((and (null class) (member complexp '(:real :complex)))
           (cond ((not (bounds-unbounded-p low high))
                  (cond ((and (floatp low) (float-infinity-p low)
                              (eql low high))
                         ;; low and high are some float
                         ;; infinity. not representable as a
                         ;; rational.
                         (let ((complexp :real)) ; TODO what if complexp was :complex?
                           (unionize (float single-float single-float t)
                                     (float double-float double-float t))))
                        (t
                         (unionize (rational nil          rational)
                                   (float    single-float single-float)
                                   (float    double-float double-float)))))
                 ((eq complexp :complex)
                  (specifier-type 'complex))
                 (t
                  (specifier-type 'real))))
          ((and (eq class 'float) (member complexp '(:real :complex))
                (eq format nil))
           (cond ((not (bounds-unbounded-p low high))
                  (if (and (floatp low) (float-infinity-p low)
                           (eql low high))
                      (let ((complexp :real))
                        (unionize (float single-float single-float t)
                                  (float double-float double-float t)
                                  #+long-float((error "long-float"))))
                      (unionize (float single-float single-float)
                                (float double-float double-float)
                                #+long-float((error "long-float")))))
                 ((eq complexp :complex)
                  (specifier-type '(complex float)))
                 (t
                  (specifier-type 'float))))
          ((and (null complexp)
                (or class format low high))
           (type-union (make-numeric-type :class class :format format :complexp :complex
                                          :low low :high high :enumerable enumerable)
                       (make-numeric-type :class class :format format :complexp :real
                                          :low low :high high :enumerable enumerable))))))

;;; Impose canonicalization rules for NUMERIC-TYPE. Note that in some
;;; cases, despite the name, we return *EMPTY-TYPE* or a UNION-TYPE instead of a
;;; NUMERIC-TYPE.
;;;
;;; FIXME: The ENUMERABLE flag is unexpectedly NIL for types that
;;; come from parsing MEMBER. But bounded integer ranges,
;;; however large, are enumerable:
;;;  (TYPE-ENUMERABLE (SPECIFIER-TYPE '(SIGNED-BYTE 99))) => T
;;;  (TYPE-ENUMERABLE (SPECIFIER-TYPE '(COMPLEX (SIGNED-BYTE 99)))) => T
;;; but, in contrast,
;;;  (TYPE-ENUMERABLE (SPECIFIER-TYPE '(EQL 5))) => NIL.
;;; I can't figure out whether this is supposed to matter.
;;; Moreover, it seems like this function should be responsible
;;; for figuring out the right value so that callers don't have to.
(defun make-numeric-type (&key class format (complexp :real) low high
                               enumerable)
  (declare (type (member integer rational float nil) class))
  (let ((union-type (%make-union-numeric-type
                     class format complexp low high enumerable)))
    (when union-type (return-from make-numeric-type union-type)))
  (multiple-value-bind (low high)
      (case class
        (integer
         ;; INTEGER types always have their LOW and HIGH bounds
         ;; represented as inclusive, not exclusive values.
         (values (if (consp low) (1+ (type-bound-number low)) low)
                 (if (consp high) (1- (type-bound-number high)) high)))
        (t
         ;; no canonicalization necessary
         (values low high)))
    ;; if interval is empty
    (when (and low high
               (if (or (consp low) (consp high)) ; if either bound is exclusive
                   (sb-xc:>= (type-bound-number low) (type-bound-number high))
                   (sb-xc:> low high)))
      (return-from make-numeric-type *empty-type*))
    (when (and (eq class 'rational) (integerp low) (eql low high))
      (setf class 'integer))
    ;; Either lookup the canonical interned object for
    ;; a point in the type lattice, or construct a new one.
    (or (case class
          (float
           (macrolet ((float-type (fmt complexp
                                   &aux (spec (if (eq complexp :complex)
                                                  `(complex ,fmt) fmt)))
                        `(literal-ctype (interned-numeric-type ',spec
                                                               :class 'float :complexp ,complexp
                                                               :format ',fmt :enumerable nil)
                             ,spec)))
             (when (bounds-unbounded-p low high)
               (ecase format
                 (single-float
                  (case complexp
                    (:real    (float-type single-float :real))
                    (:complex (float-type single-float :complex))))
                 (double-float
                  (case complexp
                    (:real    (float-type double-float :real))
                    (:complex (float-type double-float :complex))))))))
          (integer
           (macrolet ((int-type (low high)
                        `(literal-ctype
                             (interned-numeric-type nil
                                                    :class 'integer :low ,low :high ,high
                                                    :enumerable (if (and ,low ,high) t nil))
                             (integer ,(or low '*) ,(or high '*)))))
             (cond ((neq complexp :real) nil)
                   ((and (eql low 0) (eql high (1- array-dimension-limit)))
                    (int-type 0 #.(1- array-dimension-limit))) ; INDEX type
                   ((null high)
                    (cond ((not low) (int-type nil nil))
                          ((eql low 0) (int-type 0 nil))
                          ((eql low (1+ most-positive-fixnum))
                           ;; positive bignum
                           (int-type #.(1+ most-positive-fixnum) nil))))
                   ((or (eql high most-positive-word)
                        ;; is (1+ high) a power-of-2 ?
                        (and (typep high 'word) (zerop (logand (1+ high) high))))
                    (cond ((eql low 0)
                           (svref (literal-ctype-vector *interned-unsigned-byte-types*)
                                  (integer-length (truly-the word high))))
                          ((and (< high most-positive-word) (eql low (lognot high)))
                           (svref (literal-ctype-vector *interned-signed-byte-types*)
                                  (integer-length (truly-the word high))))))
                   ((and (not low) (eql high (1- most-negative-fixnum)))
                    ;; negative bignum
                    (int-type nil #.(1- most-negative-fixnum))))))
          (rational
           (cond ((and (eq complexp :real) (bounds-unbounded-p low high))
                  (literal-ctype (interned-numeric-type nil :class 'rational)
                      rational))
                 ((and (eq complexp :complex) (bounds-unbounded-p low high))
                  (literal-ctype (interned-numeric-type nil :complexp :complex
                                                            :class 'rational)
                      (complex rational)))))
          ((nil)
           (and (not format)
                (not complexp)
                (bounds-unbounded-p low high)
                (literal-ctype (interned-numeric-type nil :complexp nil) number))))
        (%make-numeric-type :class class :format format :complexp complexp
                            :low low :high high :enumerable enumerable))))

(defun modified-numeric-type (base
                              &key
                              (class      (numeric-type-class      base))
                              (format     (numeric-type-format     base))
                              (complexp   (numeric-type-complexp   base))
                              (low        (numeric-type-low        base))
                              (high       (numeric-type-high       base))
                              (enumerable (type-enumerable         base)))
  (make-numeric-type :class class
                     :format format
                     :complexp complexp
                     :low low
                     :high high
                     :enumerable enumerable))

;;; Return true if X is "less than or equal" to Y, taking open bounds
;;; into consideration. CLOSED is the predicate used to test the bound
;;; on a closed interval (e.g. <=), and OPEN is the predicate used on
;;; open bounds (e.g. <). Y is considered to be the outside bound, in
;;; the sense that if it is infinite (NIL), then the test succeeds,
;;; whereas if X is infinite, then the test fails (unless Y is also
;;; infinite).
;;;
;;; This is for comparing bounds of the same kind, e.g. upper and
;;; upper. Use NUMERIC-BOUND-TEST* for different kinds of bounds.
(defmacro numeric-bound-test (x y closed open)
  (setq closed (intern (string closed) "SB-XC")
        open (intern (string open) "SB-XC"))
  `(cond ((not ,y) t)
         ((not ,x) nil)
         ((consp ,x)
          (if (consp ,y)
              (,closed (car ,x) (car ,y))
              (,closed (car ,x) ,y)))
         (t
          (if (consp ,y)
              (,open ,x (car ,y))
              (,closed ,x ,y)))))

;;; This is used to compare upper and lower bounds. This is different
;;; from the same-bound case:
;;; -- Since X = NIL is -infinity, whereas y = NIL is +infinity, we
;;;    return true if *either* arg is NIL.
;;; -- an open inner bound is "greater" and also squeezes the interval,
;;;    causing us to use the OPEN test for those cases as well.
(defmacro numeric-bound-test* (x y closed open)
  (setq closed (intern (string closed) "SB-XC")
        open (intern (string open) "SB-XC"))
  `(cond ((not ,y) t)
         ((not ,x) t)
         ((consp ,x)
          (if (consp ,y)
              (,open (car ,x) (car ,y))
              (,open (car ,x) ,y)))
         (t
          (if (consp ,y)
              (,open ,x (car ,y))
              (,closed ,x ,y)))))

;;; Return whichever of the numeric bounds X and Y is "maximal"
;;; according to the predicates CLOSED (e.g. >=) and OPEN (e.g. >).
;;; This is only meaningful for maximizing like bounds, i.e. upper and
;;; upper. If MAX-P is true, then we return NIL if X or Y is NIL,
;;; otherwise we return the other arg.
(defmacro numeric-bound-max (x y closed open max-p)
  (setq closed (intern (string closed) "SB-XC")
        open (intern (string open) "SB-XC"))
  (once-only ((n-x x)
              (n-y y))
    `(cond ((not ,n-x) ,(if max-p nil n-y))
           ((not ,n-y) ,(if max-p nil n-x))
           ((consp ,n-x)
            (if (consp ,n-y)
                (if (,closed (car ,n-x) (car ,n-y)) ,n-x ,n-y)
                (if (,open (car ,n-x) ,n-y) ,n-x ,n-y)))
           (t
            (if (consp ,n-y)
                (if (,open (car ,n-y) ,n-x) ,n-y ,n-x)
                (if (,closed ,n-y ,n-x) ,n-y ,n-x))))))

(define-type-method (number :simple-subtypep) (type1 type2)
  (let ((class1 (numeric-type-class type1))
        (class2 (numeric-type-class type2))
        (complexp2 (numeric-type-complexp type2))
        (format2 (numeric-type-format type2))
        (low1 (numeric-type-low type1))
        (high1 (numeric-type-high type1))
        (low2 (numeric-type-low type2))
        (high2 (numeric-type-high type2)))
    ;; If one is complex and the other isn't, they are disjoint.
    (cond ((not (or (eq (numeric-type-complexp type1) complexp2)
                    (null complexp2)))
           (values nil t))
          ;; If the classes are specified and different, the types are
          ;; disjoint unless type2 is RATIONAL and type1 is INTEGER.
          ;; [ or type1 is INTEGER and type2 is of the form (RATIONAL
          ;; X X) for integral X, but this is dealt with in the
          ;; canonicalization inside MAKE-NUMERIC-TYPE ]
          ((not (or (eq class1 class2)
                    (null class2)
                    (and (eq class1 'integer) (eq class2 'rational))))
           (values nil t))
          ;; If the float formats are specified and different, the types
          ;; are disjoint.
          ((not (or (eq (numeric-type-format type1) format2)
                    (null format2)))
           (values nil t))
          ;; Check the bounds.
          ((and (numeric-bound-test low1 low2 >= >)
                (numeric-bound-test high1 high2 <= <))
           (values t t))
          (t
           (values nil t)))))

(!define-superclasses number ((number)) !cold-init-forms)

;;; If the high bound of LOW is adjacent to the low bound of HIGH,
;;; then return true, otherwise NIL. Adjacency of floating-point intervals
;;; implies that exactly one is an open interval and exactly one is closed
;;; at the adjacency point.
;;; We never (as of now) get here in the cross-compiler with target
;;; floating-point numbers. This seems legitimate as we seldom specify
;;; anything as open intervals.  The few cases seem to be the fun-types
;;; of %RANDOM-{SINGLE,DOUBLE}-FLOAT and HASH-TABLE-REHASH-mumble.
(defun numeric-types-adjacent (low high)
  (let ((low-bound (numeric-type-high low))
        (high-bound (numeric-type-low high)))
    ;; Return T if and only if X is EQL to Y, or X = 0 and -X is EQL to Y.
    ;; If both intervals have the same sign of zero at the adjacency point,
    ;; then they intersect (as per NUMERIC-TYPES-INTESECT)
    ;; so it doesn't matter much what we say here.
    (flet ((float-zeros-eqlish (x y)
             (or (eql x y)
                 ;; Calling (EQL (- X) Y) might cons. Using = would be almost the same
                 ;; but not cons, however I prefer not to assume that the caller has
                 ;; already checked for matching float formats. EQL enforces that.
                 (and (fp-zero-p x) (fp-zero-p y) (eql (sb-xc:- x) y)))))
      (cond ((not (and low-bound high-bound)) nil)
            ((and (consp low-bound) (consp high-bound)) nil)
            ((consp low-bound) (float-zeros-eqlish (car low-bound) high-bound))
            ((consp high-bound) (float-zeros-eqlish low-bound (car high-bound)))
            ((and (eq (numeric-type-class low) 'integer)
                  (eq (numeric-type-class high) 'integer))
             (eql (1+ low-bound) high-bound)) ; Integer intervals are never open
            (t
             nil)))))

;;; Return a numeric type that is a supertype for both TYPE1 and TYPE2.
;;;
;;; Binding *APPROXIMATE-NUMERIC-UNIONS* to T allows merging non-adjacent
;;; numeric types, eg (OR (INTEGER 0 12) (INTEGER 20 128)) => (INTEGER 0 128),
;;; the compiler does this occasionally during type-derivation to avoid
;;; creating absurdly complex unions of numeric types.
(defvar *approximate-numeric-unions* nil)

(defun rational-integer-union (rational integer)
  (let ((formatr (numeric-type-format rational))
        (formati (numeric-type-format integer))
        (complexpr (numeric-type-complexp rational))
        (complexpi (numeric-type-complexp integer))
        (lowi (numeric-type-low integer))
        (highi (numeric-type-high integer))
        (lowr (numeric-type-low rational))
        (highr (numeric-type-high rational)))
    (when (and (eq formatr formati) (eq complexpr complexpi))
      (cond
        ;; handle the special-case that a single integer expands the
        ;; rational interval.
        ((and (integerp lowi) (integerp highi) (= lowi highi)
              (or *approximate-numeric-unions*
                  (numeric-types-adjacent integer rational)
                  (numeric-types-adjacent rational integer)))
         (make-numeric-type
          :class 'rational :format formatr :complexp complexpr
          :low (numeric-bound-max lowr lowi <= < t)
          :high (numeric-bound-max highr highi >= > t)))
        ;; the general case:
        ;;
        ;; 1. expand the integer type by those integers contained by
        ;; the rational type, if possible.
        ;;
        ;; 2. turn open bounds in the rational contained in the
        ;; integer type into closed ones.
        ;;
        ;; (if neither of these applies, return NIL)
        (t
         (let* ((integers-of-rational
                 (make-numeric-type
                  :class 'integer :format formatr :complexp complexpr
                  :low (round-numeric-bound lowr 'integer formatr t)
                  :high (round-numeric-bound highr 'integer formatr nil)))
                (new-integer
                 (and (numeric-type-p integers-of-rational)
                      (or *approximate-numeric-unions*
                          (numeric-types-intersect integers-of-rational integer)
                          (numeric-types-adjacent integers-of-rational integer)
                          (numeric-types-adjacent integer integers-of-rational))
                     (let ((new-lowi (numeric-bound-max
                                     lowi
                                     (numeric-type-low integers-of-rational)
                                     <= < t))
                           (new-highi (numeric-bound-max
                                      highi
                                      (numeric-type-high integers-of-rational)
                                      >= > t)))
                       (and (or (not (eql new-lowi lowi))
                                (not (eql new-highi highi)))
                            (make-numeric-type
                             :class 'integer :format formatr :complexp complexpr
                             :low new-lowi :high new-highi)))))
                (new-lowr
                 (and (consp lowr)
                      (integerp (car lowr))
                      (let ((low-integer
                             (make-numeric-type
                              :class 'integer :format formati :complexp complexpi
                              :low (car lowr) :high (car lowr))))
                        (and (numeric-types-intersect integer low-integer)
                             (numeric-type-low low-integer)))))
                (new-highr
                 (and (consp highr) (integerp (car highr))
                      (let ((high-integer
                             (make-numeric-type
                              :class 'integer :format formati :complexp complexpi
                              :low (car highr) :high (car highr))))
                        (and (numeric-types-intersect integer high-integer)
                             (numeric-type-high high-integer)))))
                (new-rational
                 (and (or new-lowr new-highr)
                      (make-numeric-type
                       :class 'rational :format formatr :complexp complexpr
                       :low (or new-lowr lowr) :high (or new-highr highr)))))
           (cond
             ((or new-integer new-rational)
              (make-union-type nil (list (or new-integer integer) (or new-rational rational))))
             (t nil))))))))

(define-type-method (number :simple-union2) (type1 type2)
  (declare (type numeric-type type1 type2))
  (cond ((csubtypep type1 type2) type2)
        ((csubtypep type2 type1) type1)
        (t
         (let ((class1 (numeric-type-class type1))
               (format1 (numeric-type-format type1))
               (complexp1 (numeric-type-complexp type1))
               (class2 (numeric-type-class type2))
               (format2 (numeric-type-format type2))
               (complexp2 (numeric-type-complexp type2)))
           (cond
             ((and (eq class1 class2)
                   (eq format1 format2)
                   (eq complexp1 complexp2)
                   (or *approximate-numeric-unions*
                       (numeric-types-intersect type1 type2)
                       (numeric-types-adjacent type1 type2)
                       (numeric-types-adjacent type2 type1)))
              (make-numeric-type
               :class class1
               :format format1
               :complexp complexp1
               :low (numeric-bound-max (numeric-type-low type1)
                                       (numeric-type-low type2)
                                       <= < t)
               :high (numeric-bound-max (numeric-type-high type1)
                                        (numeric-type-high type2)
                                        >= > t)))

             ((and (eq class1 'rational) (eq class2 'integer))
              (rational-integer-union type1 type2))
             ((and (eq class1 'integer) (eq class2 'rational))
              (rational-integer-union type2 type1))
             (t nil))))))


(!cold-init-forms ;; is !PRECOMPUTE-TYPES not doing the right thing?
  (setf (info :type :kind 'number) :primitive)
  (setf (info :type :builtin 'number)
        (make-numeric-type :complexp nil)))

(def-type-translator complex ((:context context) &optional (typespec '*))
  (if (eq typespec '*)
      (specifier-type '(complex real))
      (labels ((not-numeric ()
                 (error "The component type for COMPLEX is not numeric: ~S"
                        typespec))
               (not-real ()
                 (error "The component type for COMPLEX is not a subtype of REAL: ~S"
                        typespec))
               (complex1 (component-type)
                 (unless (numeric-type-p component-type)
                   (not-numeric))
                 (unless (eq (numeric-type-complexp component-type) :real)
                   (not-real))
                 (if (csubtypep component-type (specifier-type '(eql 0)))
                     *empty-type*
                     (modified-numeric-type component-type
                                            :complexp :complex)))
               (do-complex (ctype)
                 (cond
                   ((eq ctype *empty-type*) *empty-type*)
                   ((eq ctype *universal-type*) (not-real))
                   ((typep ctype 'numeric-type) (complex1 ctype))
                   ((typep ctype 'union-type)
                    (%type-union (mapcar #'do-complex (union-type-types ctype))))
                   ((typep ctype 'member-type)
                    (%type-union
                     (mapcar-member-type-members
                      (lambda (x)
                        (if (realp x)
                            (do-complex (ctype-of x))
                            (not-real)))
                      ctype)))
                   ((and (typep ctype 'intersection-type)
                         ;; FIXME: This is very much a
                         ;; not-quite-worst-effort, but we are required to do
                         ;; something here because of our representation of
                         ;; RATIO as (AND RATIONAL (NOT INTEGER)): we must
                         ;; allow users to ask about (COMPLEX RATIO).  This
                         ;; will of course fail to work right on such types
                         ;; as (AND INTEGER (SATISFIES ZEROP))...
                         (let ((numbers (remove-if-not
                                         #'numeric-type-p
                                         (intersection-type-types ctype))))
                           (and (car numbers)
                                (null (cdr numbers))
                                (eq (numeric-type-complexp (car numbers)) :real)
                                (complex1 (car numbers))))))
                   (t
                    (multiple-value-bind (subtypep certainly)
                        (csubtypep ctype (specifier-type 'real))
                      (if (and (not subtypep) certainly)
                          (not-real)
                          ;; ANSI just says that TYPESPEC is any subtype of
                          ;; type REAL, not necessarily a NUMERIC-TYPE. In
                          ;; particular, at this point TYPESPEC could legally
                          ;; be a hairy type like (AND NUMBER (SATISFIES
                          ;; REALP) (SATISFIES ZEROP)), in which case we fall
                          ;; through the logic above and end up here,
                          ;; stumped.
                          ;; FIXME: (COMPLEX NUMBER) is not rejected but should
                          ;; be, as NUMBER is clearly not a subtype of real.
                          (bug "~@<(known bug #145): The type ~S is too hairy to be ~
used for a COMPLEX component.~:@>"
                               typespec)))))))
        (let ((ctype (specifier-type typespec context)))
          (do-complex ctype)))))

;;; If X is *, return NIL, otherwise return the bound, which must be a
;;; member of TYPE or a one-element list of a member of TYPE.
;;; This is not necessarily the canonical bound. An integer bound
;;; should always be an atom, which we'll enforce later if needed.
(defmacro valid-bound (bound type)
  `(cond ((eq ,bound '*) nil)
         ((sb-xc:typep (if (singleton-p ,bound) (car ,bound) ,bound) ',type) ,bound)
         (t
          (error ,(format nil "~A bound is not * or ~A ~A or list of one ~:*~A: ~~S"
                          (string-capitalize bound)
                          (if (eq type 'integer) "an" "a")
                          (string-downcase type))
                 ,bound))))

(def-type-translator integer (&optional (low '*) (high '*))
  (let ((lb (valid-bound low integer))
        (hb (valid-bound high integer)))
    (make-numeric-type :class 'integer :complexp :real
                       :enumerable (not (null (and lb hb)))
                       :low lb :high hb)))

(defmacro !def-bounded-type (type class format)
  `(def-type-translator ,type (&optional (low '*) (high '*))
     (let ((lb (valid-bound low ,type))
           (hb (valid-bound high ,type)))
       (make-numeric-type :class ',class :format ',format
                          :low lb :high hb))))

(!def-bounded-type rational rational nil)

;;; Unlike CMU CL, we represent the types FLOAT and REAL as
;;; UNION-TYPEs of more primitive types, in order to make
;;; type representation more unique, avoiding problems in the
;;; simplification of things like
;;;   (subtypep '(or (single-float -1.0 1.0) (single-float 0.1))
;;;             '(or (real -1 7) (single-float 0.1) (single-float -1.0 1.0)))
;;; When we allowed REAL to remain as a separate NUMERIC-TYPE,
;;; it was too easy for the first argument to be simplified to
;;; '(SINGLE-FLOAT -1.0), and for the second argument to be simplified
;;; to '(OR (REAL -1 7) (SINGLE-FLOAT 0.1)) and then for the
;;; SUBTYPEP to fail (returning NIL,T instead of T,T) because
;;; the first argument can't be seen to be a subtype of any of the
;;; terms in the second argument.
;;;
;;; The old CMU CL way was:
;;;   (!def-bounded-type float float nil)
;;;   (!def-bounded-type real nil nil)
;;;
;;; FIXME: If this new way works for a while with no weird new
;;; problems, we can go back and rip out support for separate FLOAT
;;; and REAL flavors of NUMERIC-TYPE. The new way was added in
;;; sbcl-0.6.11.22, 2001-03-21.
(defun coerce-bound (bound type upperp inner-coerce-bound-fun)
  (declare (type function inner-coerce-bound-fun))
  (if (eq bound '*)
      bound
      (funcall inner-coerce-bound-fun bound type upperp)))

(macrolet ((make-bound (val)
             `(let ((coerced ,val))
                (if (listp bound) (list coerced) coerced))))

(defun inner-coerce-real-bound (bound type upperp)
  (let ((nl most-negative-long-float)
        (pl most-positive-long-float))
    (let ((nbound (if (listp bound) (car bound) bound)))
      (ecase type
        (rational
         (make-bound (rational nbound)))
        (float
         (cond
           ((floatp nbound) bound)
           (t
            ;; Coerce to the widest float format available, to avoid
            ;; unnecessary loss of precision, but don't coerce
            ;; unrepresentable numbers.
            (ecase upperp
              ((nil)
               (when (sb-xc:< nbound nl) (return-from inner-coerce-real-bound nl)))
              ((t)
               (when (sb-xc:> nbound pl) (return-from inner-coerce-real-bound pl))))
            (make-bound (coerce nbound 'long-float)))))))))

(defun inner-coerce-float-bound (bound type upperp)
  (let ((nd most-negative-double-float)
        (pd most-positive-double-float)
        (ns most-negative-single-float)
        (ps most-positive-single-float))
    (let ((nbound (if (listp bound) (car bound) bound)))
      (ecase type
        (single-float
         (cond
           ((cl:typep nbound 'single-float) bound)
           (t
            (ecase upperp
              ((nil)
               (when (sb-xc:< nbound ns) (return-from inner-coerce-float-bound ns)))
              ((t)
               (when (sb-xc:> nbound ps) (return-from inner-coerce-float-bound ps))))
            (make-bound (coerce nbound 'single-float)))))
        (double-float
         (cond
           ((cl:typep nbound 'double-float) bound)
           (t
            (ecase upperp
              ((nil)
               (when (sb-xc:< nbound nd) (return-from inner-coerce-float-bound nd)))
              ((t)
               (when (sb-xc:> nbound pd) (return-from inner-coerce-float-bound pd))))
            (make-bound (coerce nbound 'double-float)))))))))
) ; end MACROLET

(defun coerced-real-bound (bound type upperp)
  (coerce-bound bound type upperp #'inner-coerce-real-bound))
(defun coerced-float-bound (bound type upperp)
  (coerce-bound bound type upperp #'inner-coerce-float-bound))
(def-type-translator real (&optional (low '*) (high '*))
  (specifier-type `(or (float ,(coerced-real-bound  low 'float nil)
                              ,(coerced-real-bound high 'float t))
                       (rational ,(coerced-real-bound  low 'rational nil)
                                 ,(coerced-real-bound high 'rational t)))))
(def-type-translator float (&optional (low '*) (high '*))
  (specifier-type
   `(or (single-float ,(coerced-float-bound  low 'single-float nil)
                      ,(coerced-float-bound high 'single-float t))
        (double-float ,(coerced-float-bound  low 'double-float nil)
                      ,(coerced-float-bound high 'double-float t))
        #+long-float ,(error "stub: no long float support yet"))))

(macrolet ((define-float-format (f) `(!def-bounded-type ,f float ,f)))
  (define-float-format single-float)
  (define-float-format double-float))

(defun numeric-types-intersect (type1 type2)
  (declare (type numeric-type type1 type2))
  (let* ((class1 (numeric-type-class type1))
         (class2 (numeric-type-class type2))
         (complexp1 (numeric-type-complexp type1))
         (complexp2 (numeric-type-complexp type2))
         (format1 (numeric-type-format type1))
         (format2 (numeric-type-format type2))
         (low1 (numeric-type-low type1))
         (high1 (numeric-type-high type1))
         (low2 (numeric-type-low type2))
         (high2 (numeric-type-high type2)))
    ;; If one is complex and the other isn't, then they are disjoint.
    (cond ((not (or (eq complexp1 complexp2)
                    (null complexp1) (null complexp2)))
           nil)
          ;; If either type is a float, then the other must either be
          ;; specified to be a float or unspecified. Otherwise, they
          ;; are disjoint.
          ((and (eq class1 'float)
                (not (member class2 '(float nil)))) nil)
          ((and (eq class2 'float)
                (not (member class1 '(float nil)))) nil)
          ;; If the float formats are specified and different, the
          ;; types are disjoint.
          ((not (or (eq format1 format2) (null format1) (null format2)))
           nil)
          (t
           ;; Check the bounds. This is a bit odd because we must
           ;; always have the outer bound of the interval as the
           ;; second arg.
           (if (numeric-bound-test high1 high2 <= <)
               (or (and (numeric-bound-test low1 low2 >= >)
                        (numeric-bound-test* low1 high2 <= <))
                   (and (numeric-bound-test low2 low1 >= >)
                        (numeric-bound-test* low2 high1 <= <)))
               (or (and (numeric-bound-test* low2 high1 <= <)
                        (numeric-bound-test low2 low1 >= >))
                   (and (numeric-bound-test high2 high1 <= <)
                        (numeric-bound-test* high2 low1 >= >))))))))

;;; Take the numeric bound X and convert it into something that can be
;;; used as a bound in a numeric type with the specified CLASS and
;;; FORMAT. If UP-P is true, then we round up as needed, otherwise we
;;; round down. UP-P true implies that X is a lower bound, i.e. (N) > N.
;;;
;;; This is used by NUMERIC-TYPE-INTERSECTION to mash the bound into
;;; the appropriate type number. X may only be a float when CLASS is
;;; FLOAT.
;;;
;;; ### Note: it is possible for the coercion to a float to overflow
;;; or underflow. This happens when the bound doesn't fit in the
;;; specified format. In this case, we should really return the
;;; appropriate {Most | Least}-{Positive | Negative}-XXX-Float float
;;; of desired format. But these conditions aren't currently signalled
;;; in any useful way.
;;;
;;; Also, when converting an open rational bound into a float we
;;; should probably convert it to a closed bound of the closest float
;;; in the specified format. KLUDGE: In general, open float bounds are
;;; screwed up. -- (comment from original CMU CL)
(defun round-numeric-bound (x class format up-p)
  (if x
      (let ((cx (if (consp x) (car x) x)))
        (ecase class
          ((nil rational) x)
          (integer
           (if (and (consp x) (integerp cx))
               (if up-p (1+ cx) (1- cx))
               (if up-p (ceiling cx) (floor cx))))
          (float
           (aver format)
           (let ((res
                   (cond
                     ((and format (subtypep format 'double-float))
                      (if (sb-xc:<= most-negative-double-float cx most-positive-double-float)
                          (coerce cx format)
                          nil))
                     (t
                      (if (sb-xc:<= most-negative-single-float cx most-positive-single-float)
                          ;; FIXME: bug #389
                          (coerce cx (or format 'single-float))
                          nil)))))
             (if (and (consp x) res)
                 (list res)
                 res)))))
      nil))

;;; Handle the case of type intersection on two numeric types. We use
;;; TYPES-EQUAL-OR-INTERSECT to throw out the case of types with no
;;; intersection. If an attribute in TYPE1 is unspecified, then we use
;;; TYPE2's attribute, which must be at least as restrictive. If the
;;; types intersect, then the only attributes that can be specified
;;; and different are the class and the bounds.
;;;
;;; When the class differs, we use the more restrictive class. The
;;; only interesting case is RATIONAL/INTEGER, since RATIONAL includes
;;; INTEGER.
;;;
;;; We make the result lower (upper) bound the maximum (minimum) of
;;; the argument lower (upper) bounds. We convert the bounds into the
;;; appropriate numeric type before maximizing. This avoids possible
;;; confusion due to mixed-type comparisons (but I think the result is
;;; the same).
(define-type-method (number :simple-intersection2) (type1 type2)
  (declare (type numeric-type type1 type2))
  (if (numeric-types-intersect type1 type2)
      (let* ((class1 (numeric-type-class type1))
             (class2 (numeric-type-class type2))
             (class (ecase class1
                      ((nil) class2)
                      ((integer float) class1)
                      (rational (if (eq class2 'integer)
                                    'integer
                                    'rational))))
             (format (or (numeric-type-format type1)
                         (numeric-type-format type2)))
             (low1 (numeric-type-low type1))
             (high1 (numeric-type-high type1))
             (infinity1 (and (floatp low1) (float-infinity-p low1) (eql low1 high1)))
             (low2 (numeric-type-low type2))
             (high2 (numeric-type-high type2))
             (infinity2 (and (floatp low2) (float-infinity-p low2) (eql low2 high2))))
        (make-numeric-type
         :class class
         :format format
         :complexp (or (numeric-type-complexp type1)
                       (numeric-type-complexp type2))
         :low (cond (infinity1 low1)
                    (infinity2 low2)
                    (t (numeric-bound-max
                        (round-numeric-bound low1 class format t)
                        (round-numeric-bound low2 class format t)
                        > >= nil)))
         :high (cond (infinity1 high1)
                     (infinity2 high2)
                     (t (numeric-bound-max
                         (round-numeric-bound high1 class format nil)
                         (round-numeric-bound high2 class format nil)
                         < <= nil)))))
      *empty-type*))

;;; Given two float formats, return the one with more precision. If
;;; either one is null, return NIL.
(defun float-format-max (f1 f2)
  (when (and f1 f2)
    (dolist (f *float-formats* (error "bad float format: ~S" f1))
      (when (or (eq f f1) (eq f f2))
        (return f)))))

;;; Return the result of an operation on TYPE1 and TYPE2 according to
;;; the rules of numeric contagion. This is NUMBER, some float
;;; format (possibly complex) or RATIONAL or a UNION-TYPE of
;;; these. Due to rational canonicalization, there isn't much we can
;;; do here with integers or rational complex numbers.
;;;
;;; If either argument is not a NUMERIC-TYPE, then return NUMBER. This
;;; is useful mainly for allowing types that are technically numbers,
;;; but not a NUMERIC-TYPE.
(defun numeric-contagion (type1 type2 &key (rational t)
                                           unsigned)
  (if (and (numeric-type-p type1) (numeric-type-p type2))
      (let ((class1 (numeric-type-class type1))
            (class2 (numeric-type-class type2))
            (format1 (numeric-type-format type1))
            (format2 (numeric-type-format type2))
            (complexp1 (numeric-type-complexp type1))
            (complexp2 (numeric-type-complexp type2)))
        (cond ((eq class1 'float)
               (make-numeric-type
                :class 'float
                :format (ecase class2
                          (float (float-format-max format1 format2))
                          ((integer rational) format1)
                          ((nil)
                           ;; A double-float with any real number is a
                           ;; double-float.
                           #-long-float
                           (if (eq format1 'double-float)
                             'double-float
                             nil)
                           ;; A long-float with any real number is a
                           ;; long-float.
                           #+long-float
                           (if (eq format1 'long-float)
                             'long-float
                             nil)))
                :complexp (cond ((and (eq complexp1 :real)
                                      (eq complexp2 :real))
                                 :real)
                                ((or (null complexp1) (null complexp2))
                                 nil)
                                (t :complex))))
              ((eq class2 'float) (numeric-contagion type2 type1))
              ((and (eq complexp1 :real) (eq complexp2 :real))
               (if (or rational
                       (or (neq class1 'integer)
                           (neq class2 'integer)))
                   (make-numeric-type
                    :class (and class1 class2 'rational)
                    :complexp :real)
                   (make-numeric-type
                    :class 'integer
                    :complexp :real
                    :low (and unsigned
                              (typep (numeric-type-low type1) 'unsigned-byte)
                              (typep (numeric-type-low type2) 'unsigned-byte)
                              0))))
              (t
               (specifier-type 'number))))
      (specifier-type 'number)))

;;;; array types

(define-type-class array :enumerable nil :might-contain-other-types nil)

;; All character-set types are enumerable, but it's not possible for
;; one to be TYPE= to a MEMBER type because (MEMBER #\x) is not
;; internally represented as a MEMBER type.  So in case it wasn't
;; clear already ENUMERABLE-P does not mean "possibly a MEMBER type in
;; the Lisp-theoretic sense", but means "could be implemented in SBCL
;; as a MEMBER type".
(eval-when (#+sb-xc-host :compile-toplevel :load-toplevel :execute)
  ;; may not get executed before LITERAL-CTYPE = LOAD-TIME-VALUE on
  ;; host, since LOAD-TIME-VALUE execution order with respect to top
  ;; level forms is unspecified.
  (define-type-class character-set :enumerable nil :might-contain-other-types nil))

(defun make-character-set-type (pairs)
  ; (aver (equal (mapcar #'car pairs)
  ;              (sort (mapcar #'car pairs) #'<)))
  ;; aver that the cars of the list elements are sorted into increasing order
  (when pairs
    (do ((p pairs (cdr p)))
        ((null (cdr p)))
      (aver (<= (caar p) (caadr p)))))
  (let ((pairs (let (result)
                (do ((pairs pairs (cdr pairs)))
                    ((null pairs) (nreverse result))
                  (destructuring-bind (low . high) (car pairs)
                    (loop for (low1 . high1) in (cdr pairs)
                          if (<= low1 (1+ high))
                          do (progn (setf high (max high high1))
                                    (setf pairs (cdr pairs)))
                          else do (return nil))
                    (cond
                      ((>= low char-code-limit))
                      ((< high 0))
                      (t (push (cons (max 0 low)
                                     (min high (1- char-code-limit)))
                               result))))))))
    (unless pairs
      (return-from make-character-set-type *empty-type*))
    (unless (cdr pairs)
      (macrolet ((range (low high &optional saetp-index)
                   `(return-from make-character-set-type
                      (literal-ctype (!make-interned-character-set-type
                                      (pack-interned-ctype-bits 'character-set nil ,saetp-index)
                                      '((,low . ,high)))
                                     (character-set ((,low . ,high)))))))
        (let* ((pair (car pairs))
               (low (car pair))
               (high (cdr pair)))
          (cond ((eql high (1- char-code-limit))
                 (cond ((eql low 0)
                        (range 0 #.(1- char-code-limit)
                               (sb-vm::saetp-index-or-lose 'character)))
                       #+sb-unicode
                       ((eql low base-char-code-limit)
                        (range #.base-char-code-limit
                               #.(1- char-code-limit)))))
                #+sb-unicode
                ((and (eql low 0) (eql high (1- base-char-code-limit)))
                 (range 0 #.(1- base-char-code-limit)
                        (sb-vm::saetp-index-or-lose 'base-char)))))))
    (%make-character-set-type pairs)))

;; For all ctypes which are the element types of specialized arrays,
;; 3 ctype objects are stored for the rank-1 arrays of that specialization,
;; one for each of simple, maybe-simple, and non-simple (in that order),
;; and 2 ctype objects for unknown-rank arrays, one each for simple
;; and maybe-simple. (Unknown rank, known-non-simple isn't important)
#+sb-xc-host
(progn
(defvar *interned-array-types*
  (labels ((make-1 (type-index dims complexp type)
             (aver (= (type-saetp-index type) type-index))
             (!make-interned-array-type (pack-interned-ctype-bits 'array)
                                        dims complexp type type))
           (make-all (element-type type-index array)
             (replace array
                      (list (make-1 type-index '(*) nil    element-type)
                            (make-1 type-index '(*) :maybe element-type)
                            (make-1 type-index '(*) t      element-type)
                            (make-1 type-index '*   nil    element-type)
                            (make-1 type-index '*   :maybe element-type))
                      :start1 (* type-index 5)))
           (integer-range (low high)
             (make-numeric-type :class 'integer :complexp :real
                                :enumerable t :low low :high high)))
    (let ((array (make-array (* 32 5)))
          (index 0))
      ;; Index 31 is available to store *WILD-TYPE*
      ;; because there are fewer than 32 array widetags.
      (make-all *wild-type* 31 array)
      (dovector (saetp sb-vm:*specialized-array-element-type-properties*
                       (progn (aver (< index 31)) array))
        (make-all
         (let ((x (sb-vm:saetp-specifier saetp)))
           ;; Produce element-type representation without parsing a spec.
           ;; (SPECIFIER-TYPE doesn't work when bootstrapping.)
           ;; The MAKE- constructors return an interned object as appropriate.
           (etypecase x
             ((cons (eql unsigned-byte))
              (integer-range 0 (1- (ash 1 (second x)))))
             ((cons (eql signed-byte))
              (let ((lim (ash 1 (1- (second x)))))
                (integer-range (- lim) (1- lim))))
             ((eql bit) (integer-range 0 1))
             ;; FIXNUM is its own thing, why? See comment in vm-array
             ;; saying to "See the comment in PRIMITIVE-TYPE-AUX"
             ((eql fixnum) ; One good kludge deserves another.
              (integer-range most-negative-fixnum
                             most-positive-fixnum))
             ((member single-float double-float)
              (make-numeric-type :class 'float :format x :complexp :real))
             ((cons (eql complex))
              (make-numeric-type :class 'float :format (cadr x)
                                 :complexp :complex))
             ((eql character)
              (make-character-set-type `((0 . ,(1- char-code-limit)))))
             #+sb-unicode
             ((eql base-char)
              (make-character-set-type `((0 . ,(1- base-char-code-limit)))))
             ((eql t) *universal-type*)
             ((eql nil) *empty-type*)))
         index
         array)
        (incf index)))))
(defvar *parsed-specialized-array-element-types*
  (let ((a (make-array (length sb-vm:*specialized-array-element-type-properties*))))
    (loop for i below (length a)
          do (setf (aref a i) (array-type-specialized-element-type
                               (aref *interned-array-types* (* i 5)))))
    a)))

(declaim (ftype (sfunction (t &key (:complexp t)
                                   (:element-type t)
                                   (:specialized-element-type t))
                           ctype) make-array-type))
(defun make-array-type (dimensions &key (complexp :maybe) element-type
                                        (specialized-element-type *wild-type*))
  (if (and (eq element-type specialized-element-type)
           (or (and (eq dimensions '*) (neq complexp t))
               (typep dimensions '(cons (eql *) null))))
      (let ((res (svref (literal-ctype-vector *interned-array-types*)
                        (+ (* (type-saetp-index element-type) 5)
                           (if (listp dimensions) 0 3)
                           (ecase complexp ((nil) 0) ((:maybe) 1) ((t) 2))))))
        (aver (eq (array-type-element-type res) element-type))
        res)
      (%make-array-type dimensions
                        complexp element-type specialized-element-type)))

(define-type-method (array :simple-=) (type1 type2)
  (cond ((not (and (equal (array-type-dimensions type1)
                          (array-type-dimensions type2))
                   (eq (array-type-complexp type1)
                       (array-type-complexp type2))))
         (values nil t))
        ((or (unknown-type-p (array-type-element-type type1))
             (unknown-type-p (array-type-element-type type2)))
         (type= (array-type-element-type type1)
                (array-type-element-type type2)))
        (t
         (values (type= (array-type-specialized-element-type type1)
                        (array-type-specialized-element-type type2))
                 t))))

(define-type-method (array :negate) (type)
  ;; FIXME (and hint to PFD): we're vulnerable here to attacks of the
  ;; form "are (AND ARRAY (NOT (ARRAY T))) and (OR (ARRAY BIT) (ARRAY
  ;; NIL) (ARRAY CHAR) ...) equivalent?" -- CSR, 2003-12-10
  ;; A symptom of the aforementioned is that the following are not TYPE=
  ;;   (AND (VECTOR T) (NOT SIMPLE-ARRAY)) ; an ARRAY-TYPE
  ;;   (AND (VECTOR T) (NOT SIMPLE-VECTOR)) ; an INTERSECTION-TYPE
  ;; even though (VECTOR T) makes it so that the (NOT) clause in each can
  ;; only provide one additional bit of information: that the vector
  ;; is complex as opposed to simple. The rank and element-type are fixed.
  (if (and (eq (array-type-dimensions type) '*)
           (eq (array-type-complexp type) 't)
           (eq (array-type-element-type type) *wild-type*))
      ;; (NOT <hairy-array>) = either SIMPLE-ARRAY or (NOT ARRAY).
      ;; This is deliberately asymmetric - trying to say that NOT simple-array
      ;; equals hairy-array leads to infinite recursion.
      (type-union (make-array-type '* :complexp nil
                                   :element-type *wild-type*)
                  (make-negation-type
                         (make-array-type '* :element-type *wild-type*)))
      (make-negation-type type)))

(define-type-method (array :unparse) (type)
  (let* ((dims (array-type-dimensions type))
         ;; Compare the specialised element type and the
         ;; derived element type.  If the derived type
         ;; is so small that it jumps to a smaller upgraded
         ;; element type, use the specialised element type.
         ;;
         ;; This protects from unparsing
         ;;   (and (vector (or bit symbol))
         ;;        (vector (or bit character)))
         ;; i.e., the intersection of two T array types,
         ;; as a bit vector.
         (stype (array-type-specialized-element-type type))
         (dtype (array-type-element-type type))
         (utype (%upgraded-array-element-type dtype))
         (eltype (type-specifier (if (type= stype utype)
                                     dtype
                                     stype)))
         (complexp (array-type-complexp type)))
    (cond ((eq dims '*)
           (if (eq eltype '*)
               (ecase complexp
                 ((t) '(and array (not simple-array)))
                 ((:maybe) 'array)
                 ((nil) 'simple-array))
               (ecase complexp
                 ((t) `(and (array ,eltype) (not simple-array)))
                 ((:maybe) `(array ,eltype))
                 ((nil) `(simple-array ,eltype)))))
          ((= (length dims) 1)
           (if complexp
               (let ((answer
                      (if (eq (car dims) '*)
                          (case eltype
                            (bit 'bit-vector)
                            ((base-char #-sb-unicode character) 'base-string)
                            (* 'vector)
                            (t `(vector ,eltype)))
                          (case eltype
                            (bit `(bit-vector ,(car dims)))
                            ((base-char #-sb-unicode character)
                             `(base-string ,(car dims)))
                            (t `(vector ,eltype ,(car dims)))))))
                 (if (eql complexp :maybe)
                     answer
                     `(and ,answer (not simple-array))))
               (if (eq (car dims) '*)
                   (case eltype
                     (bit 'simple-bit-vector)
                     ((base-char #-sb-unicode character) 'simple-base-string)
                     ((t) 'simple-vector)
                     (t `(simple-array ,eltype (*))))
                   (case eltype
                     (bit `(simple-bit-vector ,(car dims)))
                     ((base-char #-sb-unicode character)
                      `(simple-base-string ,(car dims)))
                     ((t) `(simple-vector ,(car dims)))
                     (t `(simple-array ,eltype ,dims))))))
          (t
           (ecase complexp
             ((t) `(and (array ,eltype ,dims) (not simple-array)))
             ((:maybe) `(array ,eltype ,dims))
             ((nil) `(simple-array ,eltype ,dims)))))))

(define-type-method (array :simple-subtypep) (type1 type2)
  (let ((dims1 (array-type-dimensions type1))
        (dims2 (array-type-dimensions type2))
        (complexp2 (array-type-complexp type2)))
    (cond (;; not subtypep unless dimensions are compatible
           (not (or (eq dims2 '*)
                    (and (not (eq dims1 '*))
                         ;; (sbcl-0.6.4 has trouble figuring out that
                         ;; DIMS1 and DIMS2 must be lists at this
                         ;; point, and knowing that is important to
                         ;; compiling EVERY efficiently.)
                         (= (length (the list dims1))
                            (length (the list dims2)))
                         (every (lambda (x y)
                                  (or (eq y '*) (eql x y)))
                                (the list dims1)
                                (the list dims2)))))
           (values nil t))
          ;; not subtypep unless complexness is compatible
          ((not (or (eq complexp2 :maybe)
                    (eq (array-type-complexp type1) complexp2)))
           (values nil t))
          ;; Since we didn't fail any of the tests above, we win
          ;; if the TYPE2 element type is wild.
          ((eq (array-type-element-type type2) *wild-type*)
           (values t t))
          (;; Since we didn't match any of the special cases above, if
           ;; either element type is unknown we can only give a good
           ;; answer if they are the same.
           (or (unknown-type-p (array-type-element-type type1))
               (unknown-type-p (array-type-element-type type2)))
           (if (type= (array-type-element-type type1)
                      (array-type-element-type type2))
               (values t t)
               (values nil nil)))
          (;; Otherwise, the subtype relationship holds iff the
           ;; types are equal, and they're equal iff the specialized
           ;; element types are identical.
           t
           (values (type= (array-type-specialized-element-type type1)
                          (array-type-specialized-element-type type2))
                   t)))))

(!define-superclasses array ((vector vector) (array)) !cold-init-forms)

(defun array-types-intersect (type1 type2)
  (declare (type array-type type1 type2))
  (let ((dims1 (array-type-dimensions type1))
        (dims2 (array-type-dimensions type2))
        (complexp1 (array-type-complexp type1))
        (complexp2 (array-type-complexp type2)))
    ;; See whether dimensions are compatible.
    (cond ((not (or (eq dims1 '*) (eq dims2 '*)
                    (and (= (length dims1) (length dims2))
                         (every (lambda (x y)
                                  (or (eq x '*) (eq y '*) (= x y)))
                                dims1 dims2))))
           (values nil t))
          ;; See whether complexpness is compatible.
          ((not (or (eq complexp1 :maybe)
                    (eq complexp2 :maybe)
                    (eq complexp1 complexp2)))
           (values nil t))
          ;; Old comment:
          ;;
          ;;   If either element type is wild, then they intersect.
          ;;   Otherwise, the types must be identical.
          ;;
          ;; FIXME: There seems to have been a fair amount of
          ;; confusion about the distinction between requested element
          ;; type and specialized element type; here is one of
          ;; them. If we request an array to hold objects of an
          ;; unknown type, we can do no better than represent that
          ;; type as an array specialized on wild-type.  We keep the
          ;; requested element-type in the -ELEMENT-TYPE slot, and
          ;; *WILD-TYPE* in the -SPECIALIZED-ELEMENT-TYPE.  So, here,
          ;; we must test for the SPECIALIZED slot being *WILD-TYPE*,
          ;; not just the ELEMENT-TYPE slot.  Maybe the return value
          ;; in that specific case should be T, NIL?  Or maybe this
          ;; function should really be called
          ;; ARRAY-TYPES-COULD-POSSIBLY-INTERSECT?  In any case, this
          ;; was responsible for bug #123, and this whole issue could
          ;; do with a rethink and/or a rewrite.  -- CSR, 2002-08-21
          ((or (eq (array-type-specialized-element-type type1) *wild-type*)
               (eq (array-type-specialized-element-type type2) *wild-type*)
               (type= (array-type-specialized-element-type type1)
                      (array-type-specialized-element-type type2)))

           (values t t))
          (t
           (values nil t)))))

(defun unite-array-types-complexp (type1 type2)
  (let ((complexp1 (array-type-complexp type1))
        (complexp2 (array-type-complexp type2)))
    (cond
      ((eq complexp1 complexp2)
       ;; both types are the same complexp-ity
       (values complexp1 t))
      ((eq complexp1 :maybe)
       ;; type1 is wild-complexp
       (values :maybe type1))
      ((eq complexp2 :maybe)
       ;; type2 is wild-complexp
       (values :maybe type2))
      (t
       ;; both types partition the complexp-space
       (values :maybe nil)))))

(defun unite-array-types-dimensions (type1 type2)
  (let ((dims1 (array-type-dimensions type1))
        (dims2 (array-type-dimensions type2)))
    (cond ((equal dims1 dims2)
           ;; both types are same dimensionality
           (values dims1 t))
          ((eq dims1 '*)
           ;; type1 is wild-dimensions
           (values '* type1))
          ((eq dims2 '*)
           ;; type2 is wild-dimensions
           (values '* type2))
          ((not (= (length dims1) (length dims2)))
           ;; types have different number of dimensions
           (values :incompatible nil))
          (t
           ;; we need to check on a per-dimension basis
           (let* ((supertype1 t)
                  (supertype2 t)
                  (compatible t)
                  (result (mapcar (lambda (dim1 dim2)
                                    (cond
                                      ((equal dim1 dim2)
                                       dim1)
                                      ((eq dim1 '*)
                                       (setf supertype2 nil)
                                       '*)
                                      ((eq dim2 '*)
                                       (setf supertype1 nil)
                                       '*)
                                      (t
                                       (setf compatible nil))))
                                  dims1 dims2)))
             (cond
               ((or (not compatible)
                    (and (not supertype1)
                         (not supertype2)))
                (values :incompatible nil))
               ((and supertype1 supertype2)
                (values result supertype1))
               (t
                (values result (if supertype1 type1 type2)))))))))

(defun unite-array-types-element-types (type1 type2)
  ;; FIXME: We'd love to be able to unite the full set of specialized
  ;; array element types up to *wild-type*, but :simple-union2 is
  ;; performed pairwise, so we don't have a good hook for it and our
  ;; representation doesn't allow us to easily detect the situation
  ;; anyway.
  ;; But see SIMPLIFY-ARRAY-UNIONS which is able to do something like that.
  (let* ((eltype1 (array-type-element-type type1))
         (eltype2 (array-type-element-type type2))
         (stype1 (array-type-specialized-element-type type1))
         (stype2 (array-type-specialized-element-type type2))
         (wild1 (eq eltype1 *wild-type*))
         (wild2 (eq eltype2 *wild-type*)))
    (cond
      ((and wild1 wild2)
       (values eltype1 stype1 t))
      (wild1
       (values eltype1 stype1 type1))
      (wild2
       (values eltype2 stype2 type2))
      ((type= eltype1 eltype2)
       (values eltype1 stype1 t))
      ((not (type= stype1 stype2))
       ;; non-wild types that don't share UAET don't unite
       (values :incompatible nil nil))
      ((csubtypep eltype1 eltype2)
       (values eltype2 stype2 type2))
      ((csubtypep eltype2 eltype1)
       (values eltype1 stype1 type1))
      (t
       (values :incompatible nil nil)))))

(defun unite-array-types-supertypes-compatible-p (&rest supertypes)
  ;; supertypes are compatible if they are all T, if there is a single
  ;; NIL and all the rest are T, or if all non-T supertypes are the
  ;; same and not NIL.
  (let ((interesting-supertypes
         (remove t supertypes)))
    (or (not interesting-supertypes)
        (equal interesting-supertypes '(nil))
        ;; supertypes are (OR BOOLEAN ARRAY-TYPE), so...
        (typep (remove-duplicates interesting-supertypes)
               '(cons array-type null)))))

(define-type-method (array :simple-union2) (type1 type2)
  (multiple-value-bind
        (result-eltype result-stype eltype-supertype)
      (unite-array-types-element-types type1 type2)
    (multiple-value-bind
          (result-complexp complexp-supertype)
        (unite-array-types-complexp type1 type2)
      (multiple-value-bind
            (result-dimensions dimensions-supertype)
          (unite-array-types-dimensions type1 type2)
        (when (and (not (eq result-dimensions :incompatible))
                   (not (eq result-eltype :incompatible))
                   (unite-array-types-supertypes-compatible-p
                    eltype-supertype complexp-supertype dimensions-supertype))
          (make-array-type result-dimensions
           :complexp result-complexp
           :element-type result-eltype
           :specialized-element-type result-stype))))))

(define-type-method (array :simple-intersection2) (type1 type2)
  (declare (type array-type type1 type2))
  (if (array-types-intersect type1 type2)
      (let ((dims1 (array-type-dimensions type1))
            (dims2 (array-type-dimensions type2))
            (complexp1 (array-type-complexp type1))
            (complexp2 (array-type-complexp type2))
            (eltype1 (array-type-element-type type1))
            (eltype2 (array-type-element-type type2))
            (stype1 (array-type-specialized-element-type type1))
            (stype2 (array-type-specialized-element-type type2)))
        (make-array-type (cond ((eq dims1 '*) dims2)
                               ((eq dims2 '*) dims1)
                               (t
                                (mapcar (lambda (x y) (if (eq x '*) y x))
                                        dims1 dims2)))
         :complexp (if (eq complexp1 :maybe) complexp2 complexp1)
         :element-type (cond
                         ((eq eltype1 *wild-type*) eltype2)
                         ((eq eltype2 *wild-type*) eltype1)
                         (t (type-intersection eltype1 eltype2)))
         :specialized-element-type (cond
                                     ((eq stype1 *wild-type*) stype2)
                                     ((eq stype2 *wild-type*) stype1)
                                     (t
                                      (aver (type= stype1 stype2))
                                      stype1))))
      *empty-type*))

;;; Check a supplied dimension list to determine whether it is legal,
;;; and return it in canonical form (as either '* or a list).
(defun canonical-array-dimensions (dims)
  (typecase dims
    ((member *) dims)
    (integer
     (when (minusp dims)
       (error "Arrays can't have a negative number of dimensions: ~S" dims))
     (when (>= dims array-rank-limit)
       (error "array type with too many dimensions: ~S" dims))
     (make-list dims :initial-element '*))
    (list
     (when (>= (length dims) array-rank-limit)
       (error "array type with too many dimensions: ~S" dims))
     (dolist (dim dims)
       (unless (eq dim '*)
         (unless (and (integerp dim)
                      (>= dim 0)
                      (< dim array-dimension-limit))
           (error "bad dimension in array type: ~S" dim))))
     dims)
    (t
     (error "Array dimensions is not a list, integer or *:~%  ~S" dims))))

;;;; MEMBER types


(define-type-class member :enumerable t
                    :might-contain-other-types nil)

(declaim (ftype (sfunction (xset list) ctype) make-member-type))
(defun member-type-from-list (members)
  (let ((xset (alloc-xset))
        (fp-zeroes))
    (dolist (elt members (make-member-type xset fp-zeroes))
      (if (fp-zero-p elt)
          (pushnew elt fp-zeroes)
          (add-to-xset elt xset)))))
(defun make-eql-type (elt) (member-type-from-list (list elt)))
;; Return possibly a union of a MEMBER type and a NUMERIC type,
;; or just one or the other, or *EMPTY-TYPE* depending on what's in the XSET
;; and the FP-ZEROES. XSET should not contains characters or real numbers.
(defun make-member-type (xset fp-zeroes)
  ;; if we have a pair of zeros (e.g. 0.0d0 and -0.0d0), then we can
  ;; canonicalize to (DOUBLE-FLOAT 0.0d0 0.0d0), because numeric
  ;; ranges are compared by arithmetic operators (while MEMBERship is
  ;; compared by EQL).  -- CSR, 2003-04-23
  (let ((presence 0)
        (unpaired nil)
        (float-types nil))
    (when fp-zeroes ; avoid doing two passes of nothing
      (dotimes (pass 2)
        (dolist (z fp-zeroes)
          (let ((sign (float-sign-bit z))
                (pair-idx
                  (etypecase z
                    (single-float 0)
                    (double-float 2
                    #+long-float (long-float 4)))))
            (if (= pass 0)
                (setf (ldb (byte 1 (+ pair-idx sign)) presence) 1)
                (if (= (ldb (byte 2 pair-idx) presence) #b11)
                    (when (= sign 0)
                      (push (ctype-of z) float-types))
                    (push z unpaired)))))))
    (let ((member-type
           (block nil
             (unless unpaired
               (macrolet ((member-type (&rest elts)
                            `(literal-ctype
                              (!make-interned-member-type
                               (pack-interned-ctype-bits 'member) (xset-from-list ',elts) nil)
                              (member ,@elts))))
                 (let ((elts (xset-data xset)))
                   (when (singleton-p elts)
                     (case (first elts)
                       ((nil) (return (member-type nil)))
                       ((t) (return (member-type t)))))
                   (when (or (equal elts '(t nil)) (equal elts '(nil t)))
                     ;; Semantically this is fine - XSETs
                     ;; are not order-preserving except by accident
                     ;; (when not represented as a hash-table).
                     (return (member-type t nil))))))
             (when (or unpaired (not (xset-empty-p xset)))
               (%make-member-type xset unpaired)))))
      ;; The actual member-type contains the XSET (with no FP zeroes),
      ;; and a list of unpaired zeroes.
      (if (not float-types)
          (or member-type *empty-type*)
          (let ((types (if member-type
                           (cons member-type float-types)
                           float-types)))
            (if (cdr types)
                (make-union-type t types)
                (car types)))))))

(defun member-type-size (type)
  (+ (length (member-type-fp-zeroes type))
     (xset-count (member-type-xset type))))

(defun member-type-member-p (x type)
  (if (fp-zero-p x)
      (and (member x (member-type-fp-zeroes type)) t)
      (xset-member-p x (member-type-xset type))))

(defun mapcar-member-type-members (function type)
  (declare (function function))
  (collect ((results))
    (map-xset (lambda (x)
                (results (funcall function x)))
              (member-type-xset type))
    (dolist (zero (member-type-fp-zeroes type))
      (results (funcall function zero)))
    (results)))

(defun mapc-member-type-members (function type)
  (declare (function function))
  (map-xset function (member-type-xset type))
  (dolist (zero (member-type-fp-zeroes type))
    (funcall function zero)))

(defun member-type-members (type)
  (append (member-type-fp-zeroes type)
          (xset-members (member-type-xset type))))

(define-type-method (member :negate) (type)
  (let ((xset (member-type-xset type))
        (fp-zeroes (member-type-fp-zeroes type)))
    (if fp-zeroes
        ;; Hairy case, which needs to do a bit of float type
        ;; canonicalization.
        (apply #'type-intersection
               (if (xset-empty-p xset)
                   *universal-type*
                   (make-negation-type (make-member-type xset nil)))
               (mapcar
                (lambda (x)
                  (let* ((opposite (sb-xc:- x))
                         (type (ctype-of opposite)))
                    (type-union
                     (make-negation-type
                      (modified-numeric-type type :low nil :high nil))
                     (modified-numeric-type type :low nil :high (list opposite))
                     (make-eql-type opposite)
                     (modified-numeric-type type :low (list opposite) :high nil))))
                fp-zeroes))
        ;; Easy case
        (make-negation-type type))))

(define-type-method (member :unparse) (type)
  (cond ((eq type (specifier-type 'null)) 'null) ; NULL type is EQ-comparable
        ((eq type (specifier-type 'boolean)) 'boolean) ; so is BOOLEAN
        (t `(member ,@(member-type-members type)))))

(define-type-method (member :singleton-p) (type)
  (if (eql 1 (member-type-size type))
      (values t (first (member-type-members type)))
      (values nil nil)))

(define-type-method (member :simple-subtypep) (type1 type2)
   (values (and (xset-subset-p (member-type-xset type1)
                               (member-type-xset type2))
                (subsetp (member-type-fp-zeroes type1)
                         (member-type-fp-zeroes type2)))
           t))

(define-type-method (member :complex-subtypep-arg1) (type1 type2)
  (block punt
    (mapc-member-type-members
     (lambda (elt)
       (multiple-value-bind (ok surep) (ctypep elt type2)
         (unless surep
           (return-from punt (values nil nil)))
         (unless ok
           (return-from punt (values nil t)))))
     type1)
    (values t t)))

;;; We punt if the odd type is enumerable and intersects with the
;;; MEMBER type. If not enumerable, then it is definitely not a
;;; subtype of the MEMBER type.
(define-type-method (member :complex-subtypep-arg2) (type1 type2)
  (cond ((not (type-enumerable type1)) (values nil t))
        ((types-equal-or-intersect type1 type2)
         (invoke-complex-subtypep-arg1-method type1 type2))
        (t (values nil t))))

(define-type-method (member :simple-intersection2) (type1 type2)
  (make-member-type (xset-intersection (member-type-xset type1)
                                       (member-type-xset type2))
                    (intersection (member-type-fp-zeroes type1)
                                  (member-type-fp-zeroes type2))))

(define-type-method (member :complex-intersection2) (type1 type2)
  (block punt
    (let ((xset (alloc-xset))
          (fp-zeroes nil))
      (mapc-member-type-members
       (lambda (member)
         (multiple-value-bind (ok sure) (ctypep member type1)
           (unless sure
             (return-from punt nil))
           (when ok
             (if (fp-zero-p member)
                 (pushnew member fp-zeroes)
                 (add-to-xset member xset)))))
       type2)
      (if (and (xset-empty-p xset) (not fp-zeroes))
          *empty-type*
          (make-member-type xset fp-zeroes)))))

;;; We don't need a :COMPLEX-UNION2, since the only interesting case is
;;; a union type, and the member/union interaction is handled by the
;;; union type method.
(define-type-method (member :simple-union2) (type1 type2)
  (make-member-type (xset-union (member-type-xset type1)
                                (member-type-xset type2))
                    (union (member-type-fp-zeroes type1)
                           (member-type-fp-zeroes type2))))

(define-type-method (member :simple-=) (type1 type2)
  (let ((l1 (member-type-fp-zeroes type1))
        (l2 (member-type-fp-zeroes type2)))
    (values (and (xset= (member-type-xset type1)
                        (member-type-xset type2))
                 (subsetp l1 l2)
                 (subsetp l2 l1))
            t)))

(define-type-method (member :complex-=) (type1 type2)
  (if (type-enumerable type1)
      (multiple-value-bind (val win) (csubtypep type2 type1)
        (if (or val (not win))
            (values nil nil)
            (values nil t)))
      (values nil t)))

(def-type-translator member :list (&rest members)
  ;; "* may appear as an argument to a MEMBER type specifier, but it indicates the
  ;;  literal symbol *, and does not represent an unspecified value."
  (if members
      (let (ms numbers char-codes)
        (dolist (m (remove-duplicates members))
          (typecase m
            (character (push (sb-xc:char-code m) char-codes))
            (real (if (and (floatp m) (zerop m))
                      (push m ms)
                      (push (ctype-of m) numbers)))
            (t (push m ms))))
        (apply #'type-union
               (member-type-from-list ms)
               (make-character-set-type (mapcar (lambda (x) (cons x x))
                                                (sort char-codes #'<)))
               (nreverse numbers)))
      *empty-type*))

;;;; intersection types
;;;;
;;;; Until version 0.6.10.6, SBCL followed the original CMU CL approach
;;;; of punting on all AND types, not just the unreasonably complicated
;;;; ones. The change was motivated by trying to get the KEYWORD type
;;;; to behave sensibly:
;;;;    ;; reasonable definition
;;;;    (DEFTYPE KEYWORD () '(AND SYMBOL (SATISFIES KEYWORDP)))
;;;;    ;; reasonable behavior
;;;;    (AVER (SUBTYPEP 'KEYWORD 'SYMBOL))
;;;; Without understanding a little about the semantics of AND, we'd
;;;; get (SUBTYPEP 'KEYWORD 'SYMBOL)=>NIL,NIL and, for entirely
;;;; parallel reasons, (SUBTYPEP 'RATIO 'NUMBER)=>NIL,NIL. That's
;;;; not so good..)
;;;;
;;;; We still follow the example of CMU CL to some extent, by punting
;;;; (to the opaque HAIRY-TYPE) on sufficiently complicated types
;;;; involving AND.

(define-type-class intersection
                    :enumerable #'compound-type-enumerable
                    :might-contain-other-types t)

(define-type-method (intersection :negate) (type)
  (%type-union
   (mapcar #'type-negation (intersection-type-types type))))

;;; A few intersection types have special names. The others just get
;;; mechanically unparsed.
(define-type-method (intersection :unparse) (type)
  (declare (type ctype type))
  ;; If magic intersection types were interned, then
  ;; we could compare by EQ here instead of calling TYPE=
  (cond ((type= type (literal-ctype (specifier-type 'keyword) keyword))
         'keyword)
        ((type= type (literal-ctype (specifier-type 'ratio) ratio))
         'ratio)
        ((type= type (literal-ctype (specifier-type 'compiled-function) compiled-function))
         'compiled-function)
        (t
         `(and ,@(mapcar #'type-specifier (intersection-type-types type))))))

(define-type-method (intersection :singleton-p) (type)
  (loop for constituent in (intersection-type-types type)
        do
        (multiple-value-bind (single value) (type-singleton-p constituent)
          (when single
            (return (values single value))))
        finally (return (values nil nil))))

;;; shared machinery for type equality: true if every type in the set
;;; TYPES1 matches a type in the set TYPES2 and vice versa
(defun type=-set (types1 types2)
  (flet ((type<=-set (x y)
           (declare (type list x y))
           (every/type (lambda (x y-element)
                         (any/type #'type= y-element x))
                       x y)))
    (and/type (type<=-set types1 types2)
              (type<=-set types2 types1))))

;;; Two intersection types are equal if their subtypes are equal sets.
;;;
;;; FIXME: Might it be better to use
;;;   (AND (SUBTYPEP X Y) (SUBTYPEP Y X))
;;; instead, since SUBTYPEP is the usual relationship that we care
;;; most about, so it would be good to leverage any ingenuity there
;;; in this more obscure method?
;;;
;;; Possibly yes, but then the SUBTYPEP methods would have to be
;;; rewritten not to use TYPE= (see the discussion around UNION
;;; :SIMPLE=)
(define-type-method (intersection :simple-=) (type1 type2)
  (type=-set (intersection-type-types type1)
             (intersection-type-types type2)))

(define-type-method (intersection :complex-=) (type1 type2)
  (let ((seen-uncertain nil))
    (dolist (itype (intersection-type-types type2)
             (if seen-uncertain
                 (values nil nil)
                 (invoke-complex-=-other-method type1 type2)))
      (let ((trial-intersection (type-intersection2 type1 itype)))
        (if (null trial-intersection)
            (setq seen-uncertain (type-might-contain-other-types-p itype))
            ;; C != (Ai n Aj...) if (C n Ai) < C.
            ;;
            ;; (CSUBTYPEP (AND C Ai) C) is T, T by construction.
            ;; We ask (SUBTYPEP C (AND C Ai)):
            ;;
            ;; T  , T  : OK, continue -- C = (AND C Ai)
            ;; NIL, T  : return early -- C > (AND C Ai)
            ;; NIL, NIL: don't know!  If we get to the end, return NIL, NIL, but
            ;;           give other types in the intersection a chance to return
            ;;           early.
            (multiple-value-bind (subtype certain?)
                (csubtypep type1 trial-intersection)
              (cond
                ((not certain?) (setq seen-uncertain t))
                ((not subtype) (return (values nil t))))))))))

(defun %intersection-complex-subtypep-arg1 (type1 type2)
  (type= type1 (type-intersection type1 type2)))

(defun %intersection-simple-subtypep (type1 type2)
  (every/type #'%intersection-complex-subtypep-arg1
              type1
              (intersection-type-types type2)))

(define-type-method (intersection :simple-subtypep) (type1 type2)
  (%intersection-simple-subtypep type1 type2))

(define-type-method (intersection :complex-subtypep-arg1) (type1 type2)
  (%intersection-complex-subtypep-arg1 type1 type2))

(defun %intersection-complex-subtypep-arg2 (type1 type2)
  (every/type #'csubtypep type1 (intersection-type-types type2)))

(define-type-method (intersection :complex-subtypep-arg2) (type1 type2)
  (%intersection-complex-subtypep-arg2 type1 type2))

;;; FIXME: This will look eeriely familiar to readers of the UNION
;;; :SIMPLE-INTERSECTION2 :COMPLEX-INTERSECTION2 method.  That's
;;; because it was generated by cut'n'paste methods.  Given that
;;; intersections and unions have all sorts of symmetries known to
;;; mathematics, it shouldn't be beyond the ken of some programmers to
;;; reflect those symmetries in code in a way that ties them together
;;; more strongly than having two independent near-copies :-/
(define-type-method (intersection :simple-union2 :complex-union2)
                     (type1 type2)
  ;; Within this method, type2 is guaranteed to be an intersection
  ;; type:
  (aver (intersection-type-p type2))
  ;; Make sure to call only the applicable methods...
  (cond ((and (intersection-type-p type1)
              (%intersection-simple-subtypep type1 type2)) type2)
        ((and (intersection-type-p type1)
              (%intersection-simple-subtypep type2 type1)) type1)
        ((and (not (intersection-type-p type1))
              (%intersection-complex-subtypep-arg2 type1 type2))
         type2)
        ((and (not (intersection-type-p type1))
              (%intersection-complex-subtypep-arg1 type2 type1))
         type1)
        ;; KLUDGE: This special (and somewhat hairy) magic is required
        ;; to deal with the RATIONAL/INTEGER special case.  The UNION
        ;; of (INTEGER * -1) and (AND (RATIONAL * -1/2) (NOT INTEGER))
        ;; should be (RATIONAL * -1/2) -- CSR, 2003-02-28
        ((and (csubtypep type2 (specifier-type 'ratio))
              (numeric-type-p type1)
              (csubtypep type1 (specifier-type 'integer))
              (csubtypep type2
                         (make-numeric-type
                          :class 'rational
                          :complexp nil
                          :low (if (null (numeric-type-low type1))
                                   nil
                                   (list (1- (numeric-type-low type1))))
                          :high (if (null (numeric-type-high type1))
                                    nil
                                    (list (1+ (numeric-type-high type1)))))))
         (let* ((intersected (intersection-type-types type2))
                (remaining   (remove (specifier-type '(not integer))
                                     intersected
                                     :test #'type=)))
           (and (not (equal intersected remaining))
                (type-union type1 (%type-intersection remaining)))))
        (t
         (let ((accumulator *universal-type*))
           (do ((t2s (intersection-type-types type2) (cdr t2s)))
               ((null t2s) accumulator)
             (let ((union (type-union type1 (car t2s))))
               (when (union-type-p union)
                 ;; we have to give up here -- there are all sorts of
                 ;; ordering worries, but it's better than before.
                 ;; Doing exactly the same as in the UNION
                 ;; :SIMPLE/:COMPLEX-INTERSECTION2 method causes stack
                 ;; overflow with the mutual recursion never bottoming
                 ;; out.
                 (if (and (eq accumulator *universal-type*)
                          (null (cdr t2s)))
                     ;; KLUDGE: if we get here, we have a partially
                     ;; simplified result.  While this isn't by any
                     ;; means a universal simplification, including
                     ;; this logic here means that we can get (OR
                     ;; KEYWORD (NOT KEYWORD)) canonicalized to T.
                     (return union)
                     (return nil)))
               (setf accumulator
                     (type-intersection accumulator union))))))))

(def-type-translator and :list ((:context context) &rest type-specifiers)
  ;; "* is not permitted as an argument to the AND type specifier."
  (%type-intersection (mapcar (lambda (x) (specifier-type x context 'and))
                              type-specifiers)))

;;;; union types

(define-type-class union
                    :enumerable #'compound-type-enumerable
                    :might-contain-other-types t)

(define-type-method (union :negate) (type)
  (declare (type ctype type))
  (%type-intersection (mapcar #'type-negation (union-type-types type))))

;;; Unlike ARRAY-TYPE-DIMENSIONS this handles union types, which
;;; includes the type STRING.
(defun ctype-array-dimensions (type)
  (labels ((process-compound-type (types)
             (let (dimensions)
               (dolist (type types)
                 (unless (or (hairy-type-p type)
                             (negation-type-p type))
                   (let ((current-dimensions (determine type)))
                     (cond ((eq current-dimensions '*)
                            (return-from ctype-array-dimensions '*))
                           ((and dimensions
                                 (not (equal current-dimensions dimensions)))
                            (if (= (length dimensions)
                                   (length current-dimensions))
                                (setf dimensions
                                      (loop for dimension in dimensions
                                            for current-dimension in current-dimensions
                                            collect (if (eql dimension current-dimension)
                                                        dimension
                                                        '*)))
                                (return-from ctype-array-dimensions '*)))
                           (t

                            (setf dimensions current-dimensions))))))
               dimensions))
           (determine (type)
             (etypecase type
               (array-type
                (array-type-dimensions type))
               (union-type
                (process-compound-type (union-type-types type)))
               (member-type
                (process-compound-type
                 (mapcar #'ctype-of (member-type-members type))))
               (intersection-type
                (process-compound-type (intersection-type-types type))))))
    (determine type)))

(defun ctype-array-specialized-element-types (type)
  (let (types)
    (labels ((process-compound-type (types)
               (loop for type in types
                     unless (or (hairy-type-p type)
                                (negation-type-p type))
                     do (determine type)))
             (determine (type)
               (etypecase type
                 (array-type
                  (when (eq (array-type-specialized-element-type type) *wild-type*)
                    (return-from ctype-array-specialized-element-types
                      *wild-type*))
                  (pushnew (array-type-specialized-element-type type)
                           types :test #'type=))
                 (union-type
                  (process-compound-type (union-type-types type)))
                 (intersection-type
                  (process-compound-type (intersection-type-types type)))
                 (member-type
                  (process-compound-type
                   (mapcar #'ctype-of (member-type-members type)))))))
      (determine type))
    types))

;;; Union unparsing involves looking for certain important type atoms in our
;;; internal representation - a/k/a "interned types" - those which have a unique
;;; object that models them; and then deciding whether some conjunction of
;;; particular atoms unparses to a prettier symbolic type.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *special-union-types*
    ;; This is order-sensitive. Prefer to recognize SEQUENCE
    ;; and extract 4 components (NULL,CONS,VECTOR,EXTENDED-SEQUENCE)
    ;; before considering LIST and extracting 2, etc.
    '(sequence list real float complex bignum)))

(define-type-method (union :unparse) (type)
  ;; This logic diverges between +/- sb-xc-host because the machinery
  ;; to parse types is obviously not usable here during make-host-1,
  ;; so the macro has to generate code that is lazier about parsing.
  (collect ((recognized))
    (let ((remainder (copy-list (union-type-types type))))
      #+sb-xc-host
      ;; Try to recognize each special type in order.
      ;; Don't use SUBTYPEP here; compare atoms instead. We're not trying
      ;; to answer complicated questions - only see whether the argument TYPE
      ;; contains (at least) each of the exact same things in SPECIAL.
      (dolist (special *special-union-types*)
        (let ((parts (union-type-types (specifier-type special))))
          (when (every (lambda (part) (memq part remainder)) parts)
            ;; Remove the parts from the remainder
            (dolist (part parts) (setq remainder (delq1 part remainder)))
            (recognized special)))) ; add to the output
      #-sb-xc-host
      (macrolet
          ((generator ()
             (let* ((constituent-types
                     (mapcar (lambda (type-specifier)
                               (union-type-types (specifier-type type-specifier)))
                             *special-union-types*))
                    ;; Get the set of atoms that we need to pick out
                    (atoms (remove-duplicates (apply #'append constituent-types))))
               (labels ((atom->bit (atom) (ash 1 (position atom atoms)))
                        (compute-mask (parts) (apply #'+ (mapcar #'atom->bit parts))))
                 `(let ((bits 0))
                    (dolist (part remainder)
                      (setq bits
                            (logior bits
                                    (cond ,@(mapcar (lambda (atom)
                                                      `((eq part ,atom) ,(atom->bit atom)))
                                                    atoms)
                                          (t 0)))))
                    ;; Now we have a bitmask of all the interesting type atoms in the
                    ;; compound type. Try to match sets of bits, and remember it is
                    ;; possible to match more than one set,
                    ;; e.g. (OR STRING FLOAT BIGNUM) matches 3 pairs of bits.
                    ,@(mapcar (lambda (name parts &aux (mask (compute-mask parts)))
                                `(when (= (logand bits ,mask) ,mask) ; is all of these
                                   (setq bits (logand bits ,(lognot mask))) ; Subtract the bits
                                   ,@(mapcar (lambda (atom)
                                               `(setq remainder (delq1 ,atom remainder)))
                                             parts)
                                   (recognized ',name))) ; add to the output
                              *special-union-types* constituent-types))))))
        (generator))
      ;; See if we can pair any two constituent types that resolve to
      ;; ({STRING|SIMPLE-STRING|non-SIMPLE-STRING} n).
      ;; Repeat until there are no more pairs. This is a kludge.
      #+sb-unicode
      (loop for tail on remainder
            do (let* ((x (car tail))
                      (peer
                       (and (array-type-p x) ; If X is a CHARACTER vector
                            (eq (array-type-element-type x) (specifier-type 'character))
                            (singleton-p (array-type-dimensions x))
                            ;; And can be matched with a BASE-CHAR vector
                            (member-if (lambda (y)
                                         (and (array-type-p y)
                                              (eq (array-type-element-type y)
                                                  (specifier-type 'base-char))
                                              (eq (array-type-complexp y)
                                                  (array-type-complexp x))
                                              (equal (array-type-dimensions y)
                                                     (array-type-dimensions x))))
                                       (cdr tail)))))
                 (when peer ; then together they comprise a subtype of STRING
                   (let* ((dim (car (array-type-dimensions x)))
                          (string-type
                           (if (array-type-complexp x)
                               (if (eq dim '*) 'string `(string ,dim))
                               (if (eq dim '*) 'simple-string `(simple-string ,dim)))))
                     (recognized (if (eq (array-type-complexp x) 't)
                                     `(and ,string-type (not simple-array))
                                     string-type)))
                   (rplaca tail nil) ; We'll delete these list elements later
                   (rplaca peer nil))))
      (let ((list (nconc (recognized)
                         (mapcar #'type-specifier (delete nil remainder)))))
        (if (cdr list) `(or ,@list) (car list))))))

;;; Two union types are equal if they are each subtypes of each
;;; other. We need to be this clever because our complex subtypep
;;; methods are now more accurate; we don't get infinite recursion
;;; because the simple-subtypep method delegates to complex-subtypep
;;; of the individual types of type1. - CSR, 2002-04-09
;;;
;;; Previous comment, now obsolete, but worth keeping around because
;;; it is true, though too strong a condition:
;;;
;;; Two union types are equal if their subtypes are equal sets.
(define-type-method (union :simple-=) (type1 type2)
  (multiple-value-bind (subtype certain?)
      (csubtypep type1 type2)
    (if subtype
        (csubtypep type2 type1)
        ;; we might as well become as certain as possible.
        (if certain?
            (values nil t)
            (multiple-value-bind (subtype certain?)
                (csubtypep type2 type1)
              (values nil (and (not subtype) certain?)))))))

(define-type-method (union :complex-=) (type1 type2)
  (declare (ignore type1))
  (if (some #'type-might-contain-other-types-p
            (union-type-types type2))
      (values nil nil)
      (values nil t)))

;;; Similarly, a union type is a subtype of another if and only if
;;; every element of TYPE1 is a subtype of TYPE2.
(defun union-simple-subtypep (type1 type2)
  (every/type (swapped-args-fun #'union-complex-subtypep-arg2)
              type2
              (union-type-types type1)))

(define-type-method (union :simple-subtypep) (type1 type2)
  (union-simple-subtypep type1 type2))

(defun union-complex-subtypep-arg1 (type1 type2)
  (every/type (swapped-args-fun #'csubtypep)
              type2
              (union-type-types type1)))

(define-type-method (union :complex-subtypep-arg1) (type1 type2)
  (union-complex-subtypep-arg1 type1 type2))

(defun union-complex-subtypep-arg2 (type1 type2)
  ;; At this stage, we know that type2 is a union type and type1
  ;; isn't. We might as well check this, though:
  (aver (union-type-p type2))
  (aver (not (union-type-p type1)))
  ;; was: (any/type #'csubtypep type1 (union-type-types type2)), which
  ;; turns out to be too restrictive, causing bug 91.
  ;;
  ;; the following reimplementation might look dodgy. It is dodgy. It
  ;; depends on the union :complex-= method not doing very much work
  ;; -- certainly, not using subtypep. Reasoning:
  ;;
  ;;     A is a subset of (B1 u B2)
  ;; <=> A n (B1 u B2) = A
  ;; <=> (A n B1) u (A n B2) = A
  ;;
  ;; But, we have to be careful not to delegate this type= to
  ;; something that could invoke subtypep, which might get us back
  ;; here -> stack explosion. We therefore ensure that the second type
  ;; (which is the one that's dispatched on) is either a union type
  ;; (where we've ensured that the complex-= method will not call
  ;; subtypep) or something with no union types involved, in which
  ;; case we'll never come back here.
  ;;
  ;; If we don't do this, then e.g.
  ;; (SUBTYPEP '(MEMBER 3) '(OR (SATISFIES FOO) (SATISFIES BAR)))
  ;; would loop infinitely, as the member :complex-= method is
  ;; implemented in terms of subtypep.
  ;;
  ;; Ouch. - CSR, 2002-04-10
  (cond ((fun-designator-type-p type1)
         (type= type2 (specifier-type 'function-designator)))
        (t
         (multiple-value-bind (sub-value sub-certain?)
             (type= type1
                    (%type-union
                     (mapcar (lambda (x) (type-intersection type1 x))
                             (union-type-types type2))))
           (if sub-certain?
               (values sub-value sub-certain?)
               ;; The ANY/TYPE expression above is a sufficient condition for
               ;; subsetness, but not a necessary one, so we might get a more
               ;; certain answer by this CALL-NEXT-METHOD-ish step when the
               ;; ANY/TYPE expression is uncertain.
               (invoke-complex-subtypep-arg1-method type1 type2))))))

(define-type-method (union :complex-subtypep-arg2) (type1 type2)
  (union-complex-subtypep-arg2 type1 type2))

(define-type-method (union :simple-intersection2 :complex-intersection2)
                     (type1 type2)
  ;; The CSUBTYPEP clauses here let us simplify e.g.
  ;;   (TYPE-INTERSECTION2 (SPECIFIER-TYPE 'LIST)
  ;;                       (SPECIFIER-TYPE '(OR LIST VECTOR)))
  ;; (where LIST is (OR CONS NULL)).
  ;;
  ;; The tests are more or less (CSUBTYPEP TYPE1 TYPE2) and vice
  ;; versa, but it's important that we pre-expand them into
  ;; specialized operations on individual elements of
  ;; UNION-TYPE-TYPES, instead of using the ordinary call to
  ;; CSUBTYPEP, in order to avoid possibly invoking any methods which
  ;; might in turn invoke (TYPE-INTERSECTION2 TYPE1 TYPE2) and thus
  ;; cause infinite recursion.
  ;;
  ;; Within this method, type2 is guaranteed to be a union type:
  (aver (union-type-p type2))
  ;; Make sure to call only the applicable methods...
  (cond ((and (union-type-p type1)
              (union-simple-subtypep type1 type2)) type1)
        ((and (union-type-p type1)
              (union-simple-subtypep type2 type1)) type2)
        ((and (not (union-type-p type1))
              (union-complex-subtypep-arg2 type1 type2))
         type1)
        ((and (not (union-type-p type1))
              (union-complex-subtypep-arg1 type2 type1))
         type2)
        (t
         ;; KLUDGE: This code accumulates a sequence of TYPE-UNION2
         ;; operations in a particular order, and gives up if any of
         ;; the sub-unions turn out not to be simple. In other cases
         ;; ca. sbcl-0.6.11.15, that approach to taking a union was a
         ;; bad idea, since it can overlook simplifications which
         ;; might occur if the terms were accumulated in a different
         ;; order. It's possible that that will be a problem here too.
         ;; However, I can't think of a good example to demonstrate
         ;; it, and without an example to demonstrate it I can't write
         ;; test cases, and without test cases I don't want to
         ;; complicate the code to address what's still a hypothetical
         ;; problem. So I punted. -- WHN 2001-03-20
         (let ((accumulator *empty-type*))
           (dolist (t2 (union-type-types type2) accumulator)
             (setf accumulator
                   (type-union accumulator
                               (type-intersection type1 t2))))))))

(def-type-translator or :list ((:context context) &rest type-specifiers)
  ;; "* is not permitted as an argument to the OR type specifier."
  (let ((type (%type-union (mapcar (lambda (x) (specifier-type x context 'or))
                                   type-specifiers))))
    (if (union-type-p type)
        (sb-kernel::simplify-array-unions type)
        type)))


;;;; ALIEN-TYPE types

(define-type-class alien :enumerable nil :might-contain-other-types nil)

(define-type-method (alien :negate) (type) (make-negation-type type))

(define-type-method (alien :unparse) (type)
  `(alien ,(unparse-alien-type (alien-type-type-alien-type type))))

(define-type-method (alien :simple-subtypep) (type1 type2)
  (values (alien-subtype-p (alien-type-type-alien-type type1)
                           (alien-type-type-alien-type type2))
          t))

(define-type-method (alien :simple-=) (type1 type2)
  (let ((alien-type-1 (alien-type-type-alien-type type1))
        (alien-type-2 (alien-type-type-alien-type type2)))
    (values (or (eq alien-type-1 alien-type-2)
                (alien-type-= alien-type-1 alien-type-2))
            t)))

(def-type-translator alien (&optional (alien-type nil))
  (typecase alien-type
    (null
     (make-alien-type-type))
    (alien-type
     (make-alien-type-type alien-type))
    (t
     (make-alien-type-type (parse-alien-type alien-type (make-null-lexenv))))))

(defun make-alien-type-type (&optional alien-type)
  (if alien-type
      (let ((lisp-rep-type (compute-lisp-rep-type alien-type)))
        (if lisp-rep-type
            (single-value-specifier-type lisp-rep-type)
            (%make-alien-type-type alien-type)))
      *universal-type*))


;;;; CONS types

(def-type-translator cons ((:context context)
                            &optional (car-type-spec '*) (cdr-type-spec '*))
  (let ((car-type (single-value-specifier-type car-type-spec context))
        (cdr-type (single-value-specifier-type cdr-type-spec context)))
    (make-cons-type car-type cdr-type)))

(define-type-class cons :enumerable nil :might-contain-other-types nil)

#+sb-xc-host
(declaim (ftype (sfunction (ctype ctype) (values t t)) type=))
(defun make-cons-type (car-type cdr-type)
  (aver (not (or (eq car-type *wild-type*)
                 (eq cdr-type *wild-type*))))
  (cond ((or (eq car-type *empty-type*)
             (eq cdr-type *empty-type*))
         *empty-type*)
        ;; It's not a requirement that (CONS T T) be interned,
        ;; but it improves the hit rate in the function caches.
        ((and (type= car-type *universal-type*)
              (type= cdr-type *universal-type*))
         (literal-ctype (!make-interned-cons-type (pack-interned-ctype-bits 'cons)
                                                  *universal-type*
                                                  *universal-type*)
                        cons))
        (t
         (%make-cons-type car-type cdr-type))))

;;; Return TYPE converted to canonical form for a situation where the
;;; "type" '* (which SBCL still represents as a type even though ANSI
;;; CL defines it as a related but different kind of placeholder) is
;;; equivalent to type T.
(defun type-*-to-t (type)
  (if (type= type *wild-type*)
      *universal-type*
      type))

(define-type-method (cons :negate) (type)
  (if (and (eq (cons-type-car-type type) *universal-type*)
           (eq (cons-type-cdr-type type) *universal-type*))
      (make-negation-type type)
      (type-union
       (make-negation-type (specifier-type 'cons))
       (cond
         ((and (not (eq (cons-type-car-type type) *universal-type*))
               (not (eq (cons-type-cdr-type type) *universal-type*)))
          (type-union
           (make-cons-type
            (type-negation (cons-type-car-type type))
            *universal-type*)
           (make-cons-type
            *universal-type*
            (type-negation (cons-type-cdr-type type)))))
         ((not (eq (cons-type-car-type type) *universal-type*))
          (make-cons-type
           (type-negation (cons-type-car-type type))
           *universal-type*))
         ((not (eq (cons-type-cdr-type type) *universal-type*))
          (make-cons-type
           *universal-type*
           (type-negation (cons-type-cdr-type type))))
         (t (bug "Weird CONS type ~S" type))))))

(define-type-method (cons :unparse) (type)
  (if (eq type (specifier-type 'cons))
      'cons
      `(cons ,(type-specifier (cons-type-car-type type))
             ,(type-specifier (cons-type-cdr-type type)))))

(define-type-method (cons :simple-=) (type1 type2)
  (declare (type cons-type type1 type2))
  (multiple-value-bind (car-match car-win)
      (type= (cons-type-car-type type1) (cons-type-car-type type2))
    (multiple-value-bind (cdr-match cdr-win)
        (type= (cons-type-cdr-type type1) (cons-type-cdr-type type2))
      (cond ((and car-match cdr-match)
             (aver (and car-win cdr-win))
             (values t t))
            (t
             (values nil
                     ;; FIXME: Ideally we would like to detect and handle
                     ;;  (CONS UNKNOWN INTEGER) (CONS UNKNOWN SYMBOL) => NIL, T
                     ;; but just returning a secondary true on (and car-win cdr-win)
                     ;; unfortunately breaks other things. --NS 2006-08-16
                     (and (or (and (not car-match) car-win)
                              (and (not cdr-match) cdr-win))
                          (not (and (cons-type-might-be-empty-type type1)
                                    (cons-type-might-be-empty-type type2))))))))))

(define-type-method (cons :simple-subtypep) (type1 type2)
  (declare (type cons-type type1 type2))
  (multiple-value-bind (val-car win-car)
      (csubtypep (cons-type-car-type type1) (cons-type-car-type type2))
    (multiple-value-bind (val-cdr win-cdr)
        (csubtypep (cons-type-cdr-type type1) (cons-type-cdr-type type2))
      (if (and val-car val-cdr)
          (values t (and win-car win-cdr))
          (values nil (or (and (not val-car) win-car)
                          (and (not val-cdr) win-cdr)))))))

;;; Give up if a precise type is not possible, to avoid returning
;;; overly general types.
(define-type-method (cons :simple-union2) (type1 type2)
  (declare (type cons-type type1 type2))
  (let ((car-type1 (cons-type-car-type type1))
        (car-type2 (cons-type-car-type type2))
        (cdr-type1 (cons-type-cdr-type type1))
        (cdr-type2 (cons-type-cdr-type type2))
        car-not1
        car-not2)
    ;; UGH.  -- CSR, 2003-02-24
    (macrolet ((frob-car (car1 car2 cdr1 cdr2 &optional not1)
                 `(let ((intersection (type-intersection ,car2
                                                         ,(or not1 `(type-negation ,car1)))))
                    (unless (type= intersection ,car2)
                      (type-union
                       (make-cons-type ,car1 (type-union ,cdr1 ,cdr2))
                       (make-cons-type intersection ,cdr2))))))
      (cond ((type= car-type1 car-type2)
             (make-cons-type car-type1
                             (type-union cdr-type1 cdr-type2)))
            ((type= cdr-type1 cdr-type2)
             (make-cons-type (type-union car-type1 car-type2)
                             cdr-type1))
            ;; (or (cons A1 D1) (cons A2 D2))
            ;;
            ;; if A1 is a subtype of A2, this is equivalent to
            ;;
            ;; (or (cons A1 (or D1 D2)) (cons (and A2 (not A1)) D2))
            ((csubtypep car-type1 car-type2)
             (frob-car car-type1 car-type2 cdr-type1 cdr-type2))
            ((csubtypep car-type2 car-type1)
             (frob-car car-type2 car-type1 cdr-type2 cdr-type1))
            ;; in general
            ;;
            ;; (or (cons A1 D1) (cons A2 D2))
            ;;
            ;; is
            ;;
            ;; (or (cons (and A1 A2) (or D1 D2))
            ;;     (cons (and A1 (not A2)) D1)
            ;;     (cons (and (not A1) A2) D2))
            ;;
            ;; (or (cons (integer 0 8) (integer 5 15))
            ;;     (cons (integer 3 15) (integer 4 14))
            ;;
            ;; ->
            ;;
            ;; (or (cons (integer 3 8) (integer 4 15))
            ;;     (cons (integer 0 2) (integer 5 15))
            ;;     (cons (integer 9 15) (integer 4 14))
            ;;
            ;; if A1 and A2 are disjoint no further simplification is
            ;; possible.  However, if they are not disjoint, and we
            ;; can tell that they are not disjoint, we should be able
            ;; to break the type up into smaller pieces.
            ((multiple-value-bind (yes win)
                 (csubtypep car-type2 (setf car-not1 (type-negation car-type1)))
               (and (not yes) win))
             (let ((cdr-union (type-union cdr-type1 cdr-type2)))
               (setf car-not2 (type-negation car-type2))
               (type-union
                (make-cons-type (type-intersection car-type1 car-type2) cdr-union)
                (make-cons-type (type-intersection car-type1 car-not2) cdr-type1)
                (make-cons-type (type-intersection car-not1 car-type2) cdr-type2))))
            ((multiple-value-bind (yes win)
                 (csubtypep car-type1 (setf car-not2 (type-negation car-type2)))
               (and (not yes) win))
             (let ((cdr-union (type-union cdr-type1 cdr-type2)))
               (type-union
                (make-cons-type (type-intersection car-type1 car-type2) cdr-union)
                (make-cons-type (type-intersection car-type1 car-not2) cdr-type1)
                (make-cons-type (type-intersection car-not1 car-type2) cdr-type2))))
            ;; Don't put these in -- consider the effect of taking the
            ;; union of (CONS (INTEGER 0 2) (INTEGER 5 7)) and
            ;; (CONS (INTEGER 0 3) (INTEGER 5 6)).
            #+nil
            ((csubtypep cdr-type1 cdr-type2)
             (frob-cdr car-type1 car-type2 cdr-type1 cdr-type2))
            #+nil
            ((csubtypep cdr-type2 cdr-type1)
             (frob-cdr car-type2 car-type1 cdr-type2 cdr-type1))))))

(define-type-method (cons :simple-intersection2) (type1 type2)
  (declare (type cons-type type1 type2))
  (let ((car-int2 (type-intersection2 (cons-type-car-type type1)
                                      (cons-type-car-type type2)))
        (cdr-int2 (type-intersection2 (cons-type-cdr-type type1)
                                      (cons-type-cdr-type type2))))
    (cond
      ((and car-int2 cdr-int2) (make-cons-type car-int2 cdr-int2))
      (car-int2 (make-cons-type car-int2
                                (type-intersection
                                 (cons-type-cdr-type type1)
                                 (cons-type-cdr-type type2))))
      (cdr-int2 (make-cons-type
                 (type-intersection (cons-type-car-type type1)
                                    (cons-type-car-type type2))
                 cdr-int2)))))

(!define-superclasses cons ((cons)) !cold-init-forms)

;;;; CHARACTER-SET types

(def-type-translator character-set
    (&optional (pairs `((0 . ,(1- char-code-limit)))))
  (make-character-set-type pairs))

(define-type-method (character-set :negate) (type)
  (let ((pairs (character-set-type-pairs type)))
    (if (and (= (length pairs) 1)
             (= (caar pairs) 0)
             (= (cdar pairs) (1- char-code-limit)))
        (make-negation-type type)
        (let ((not-character
               (make-negation-type
                (make-character-set-type
                 `((0 . ,(1- char-code-limit)))))))
          (type-union
           not-character
           (make-character-set-type
                   (let (not-pairs)
                     (when (> (caar pairs) 0)
                       (push (cons 0 (1- (caar pairs))) not-pairs))
                     (do* ((tail pairs (cdr tail))
                           (high1 (cdar tail) (cdar tail))
                           (low2 (caadr tail) (caadr tail)))
                          ((null (cdr tail))
                           (when (< (cdar tail) (1- char-code-limit))
                             (push (cons (1+ (cdar tail))
                                         (1- char-code-limit))
                                   not-pairs))
                           (nreverse not-pairs))
                       (push (cons (1+ high1) (1- low2)) not-pairs)))))))))

(define-type-method (character-set :unparse) (type)
  (cond
    ((eq type (specifier-type 'character)) 'character)
    ((eq type (specifier-type 'base-char)) 'base-char)
    ((eq type (specifier-type 'extended-char)) 'extended-char)
    ;; standard-char is not an interned type
    ((type= type (specifier-type 'standard-char)) 'standard-char)
    (t
     ;; Unparse into either MEMBER or CHARACTER-SET. We use MEMBER if there
     ;; are at most as many characters as there are character code ranges.
     ;; (basically saying to use MEMBER if each range is one character)
     (let* ((pairs (character-set-type-pairs type))
            (count (length pairs))
            (chars (loop named outer
                         for (low . high) in pairs
                         nconc (loop for code from low upto high
                                     collect (code-char code)
                                     when (minusp (decf count))
                                     do (return-from outer t)))))
       (if (eq chars t)
           `(character-set ,pairs)
           `(member ,@chars))))))

(define-type-method (character-set :singleton-p) (type)
  (let* ((pairs (character-set-type-pairs type))
         (pair  (first pairs)))
    (if (and (typep pairs '(cons t null))
             (eql (car pair) (cdr pair)))
        (values t (code-char (car pair)))
        (values nil nil))))

(define-type-method (character-set :simple-=) (type1 type2)
  (let ((pairs1 (character-set-type-pairs type1))
       (pairs2 (character-set-type-pairs type2)))
    (values (equal pairs1 pairs2) t)))

(define-type-method (character-set :simple-subtypep) (type1 type2)
  (values
   (dolist (pair (character-set-type-pairs type1) t)
     (unless (position pair (character-set-type-pairs type2)
                      :test (lambda (x y) (and (>= (car x) (car y))
                                               (<= (cdr x) (cdr y)))))
       (return nil)))
   t))

(define-type-method (character-set :simple-union2) (type1 type2)
  ;; KLUDGE: the canonizing in the MAKE-CHARACTER-SET-TYPE function
  ;; actually does the union for us.  It might be a little fragile to
  ;; rely on it.
  (make-character-set-type
          (merge 'list
                (copy-alist (character-set-type-pairs type1))
                (copy-alist (character-set-type-pairs type2))
                #'< :key #'car)))

(define-type-method (character-set :simple-intersection2) (type1 type2)
  ;; KLUDGE: brute force.
#|
  (let (pairs)
    (dolist (pair1 (character-set-type-pairs type1)
            (make-character-set-type
                    (sort pairs #'< :key #'car)))
      (dolist (pair2 (character-set-type-pairs type2))
       (cond
         ((<= (car pair1) (car pair2) (cdr pair1))
          (push (cons (car pair2) (min (cdr pair1) (cdr pair2))) pairs))
         ((<= (car pair2) (car pair1) (cdr pair2))
          (push (cons (car pair1) (min (cdr pair1) (cdr pair2))) pairs))))))
|#
  (make-character-set-type
          (intersect-type-pairs
           (character-set-type-pairs type1)
           (character-set-type-pairs type2))))

;;;
;;; Intersect two ordered lists of pairs
;;; Each list is of the form ((start1 . end1) ... (startn . endn)),
;;; where start1 <= end1 < start2 <= end2 < ... < startn <= endn.
;;; Each pair represents the integer interval start..end.
;;;
(defun intersect-type-pairs (alist1 alist2)
  (if (and alist1 alist2)
      (let ((res nil)
            (pair1 (pop alist1))
            (pair2 (pop alist2)))
        (loop
         (when (> (car pair1) (car pair2))
           (rotatef pair1 pair2)
           (rotatef alist1 alist2))
         (let ((pair1-cdr (cdr pair1)))
           (cond
            ((> (car pair2) pair1-cdr)
             ;; No over lap -- discard pair1
             (unless alist1 (return))
             (setq pair1 (pop alist1)))
            ((<= (cdr pair2) pair1-cdr)
             (push (cons (car pair2) (cdr pair2)) res)
             (cond
              ((= (cdr pair2) pair1-cdr)
               (unless alist1 (return))
               (unless alist2 (return))
               (setq pair1 (pop alist1)
                     pair2 (pop alist2)))
              (t ;; (< (cdr pair2) pair1-cdr)
               (unless alist2 (return))
               (setq pair1 (cons (1+ (cdr pair2)) pair1-cdr))
               (setq pair2 (pop alist2)))))
            (t ;; (> (cdr pair2) (cdr pair1))
             (push (cons (car pair2) pair1-cdr) res)
             (unless alist1 (return))
             (setq pair2 (cons (1+ pair1-cdr) (cdr pair2)))
             (setq pair1 (pop alist1))))))
        (nreverse res))
    nil))


;;; Return the type that describes all objects that are in X but not
;;; in Y.
(defun type-difference (x y)
  (type-intersection x (type-negation y)))

(def-type-translator array ((:context context)
                             &optional (element-type '*)
                                       (dimensions '*))
  (let ((eltype (if (eq element-type '*)
                    *wild-type*
                    (specifier-type element-type context))))
    (make-array-type (canonical-array-dimensions dimensions)
                     :complexp :maybe
                     :element-type eltype
                     :specialized-element-type (%upgraded-array-element-type
                                                eltype))))

(def-type-translator simple-array ((:context context)
                                    &optional (element-type '*)
                                              (dimensions '*))
  (let ((eltype (if (eq element-type '*)
                    *wild-type*
                    (specifier-type element-type context))))
   (make-array-type (canonical-array-dimensions dimensions)
                    :complexp nil
                    :element-type eltype
                    :specialized-element-type (%upgraded-array-element-type
                                               eltype))))

;;;; SIMD-PACK types

#+sb-simd-pack
(defun make-simd-pack-type (element-type)
  (aver (neq element-type *wild-type*))
  (if (eq element-type *empty-type*)
      *empty-type*
      (%make-simd-pack-type
       (do* ((i 0 (1+ i))
             (pack-types *simd-pack-element-types* (cdr pack-types))
             (pack-type (car pack-types) (car pack-types)))
            ((null pack-types)
             (error "~S element type must be a subtype of ~
                         ~{~/sb-impl:print-type-specifier/~#[~;, or ~
                         ~:;, ~]~}."
                    'simd-pack *simd-pack-element-types*))
         (when (csubtypep element-type (specifier-type pack-type))
           (let ((bv (make-array (length *simd-pack-element-types*)
                                 :element-type 'bit :initial-element 0)))
             (setf (sbit bv i) 1)
             (return bv)))))))

#+sb-simd-pack-256
(defun make-simd-pack-256-type (element-type)
  (aver (neq element-type *wild-type*))
  (if (eq element-type *empty-type*)
      *empty-type*
      (%make-simd-pack-256-type
       (do* ((i 0 (1+ i))
             (pack-types *simd-pack-element-types* (cdr pack-types))
             (pack-type (car pack-types) (car pack-types)))
            ((null pack-types)
             (error "~S element type must be a subtype of ~
                         ~{~/sb-impl:print-type-specifier/~#[~;, or ~
                         ~:;, ~]~}."
                    'simd-pack-256 *simd-pack-element-types*))
         (when (csubtypep element-type (specifier-type pack-type))
           (let ((bv (make-array (length *simd-pack-element-types*)
                                 :element-type 'bit :initial-element 0)))
             (setf (sbit bv i) 1)
             (return bv)))))))
#+sb-simd-pack-512
(defun make-simd-pack-512-type (element-type)
  (aver (neq element-type *wild-type*))
  (if (eq element-type *empty-type*)
      *empty-type*
      (%make-simd-pack-512-type
       (do* ((i 0 (1+ i))
             (pack-types *simd-pack-element-types* (cdr pack-types))
             (pack-type (car pack-types) (car pack-types)))
            ((null pack-types)
             (error "~S element type must be a subtype of ~
                         ~{~/sb-impl:print-type-specifier/~#[~;, or ~
                         ~:;, ~]~}."
                    'simd-pack-512 *simd-pack-element-types*))
         (when (csubtypep element-type (specifier-type pack-type))
           (let ((bv (make-array (length *simd-pack-element-types*)
                                 :element-type 'bit :initial-element 0)))
             (setf (sbit bv i) 1)
             (return bv)))))))

#+sb-simd-pack
(progn
  (define-type-class simd-pack :enumerable nil :might-contain-other-types nil)

  ;; Though this involves a recursive call to parser, parsing context need not
  ;; be passed down, because an unknown-type condition is an immediate failure.
  (def-type-translator simd-pack (&optional (element-type-spec '*))
    (if (eql element-type-spec '*)
        (%make-simd-pack-type (make-array (length *simd-pack-element-types*)
                                          :element-type 'bit :initial-element 1))
        (make-simd-pack-type (single-value-specifier-type element-type-spec))))

  (define-type-method (simd-pack :negate) (type)
    (let* ((element-type (simd-pack-type-element-type type))
           (remaining (and (/= (count 0 element-type) 0) (bit-not element-type)))
           (not-simd-pack (make-negation-type (specifier-type 'simd-pack))))
      (if remaining
          (type-union not-simd-pack (%make-simd-pack-type remaining))
          not-simd-pack)))

  (define-type-method (simd-pack :unparse) (type)
    (let* ((eltypes (simd-pack-type-element-type type)))
      (cond
        ((= (count 0 eltypes) 0) 'simd-pack)
        ((= (count 1 eltypes) 1)
         (let ((pos (position 1 eltypes)))
           (if pos
               `(simd-pack ,(elt *simd-pack-element-types* pos))
               (bug "bad simd-pack"))))
        (t
         `(or
           ,@(loop for x from 0
                   for bit across eltypes
                   if (= bit 1)
                     collect `(simd-pack ,(elt *simd-pack-element-types* x))))))))

  (define-type-method (simd-pack :simple-=) (type1 type2)
    (declare (type simd-pack-type type1 type2))
    (values
     (= 0 (count 1 (bit-xor (simd-pack-type-element-type type1)
                            (simd-pack-type-element-type type2))))
     t))

  (define-type-method (simd-pack :simple-subtypep) (type1 type2)
    (declare (type simd-pack-type type1 type2))
    (values
     (= 0 (count 1 (bit-andc2 (simd-pack-type-element-type type1)
                              (simd-pack-type-element-type type2))))
     t))

  (define-type-method (simd-pack :simple-union2) (type1 type2)
    (declare (type simd-pack-type type1 type2))
    (%make-simd-pack-type (bit-ior (simd-pack-type-element-type type1)
                                   (simd-pack-type-element-type type2))))

  (define-type-method (simd-pack :simple-intersection2) (type1 type2)
    (declare (type simd-pack-type type1 type2))
    (let ((intersection (bit-and (simd-pack-type-element-type type1)
                                 (simd-pack-type-element-type type2))))
      (if (find 1 intersection)
          (%make-simd-pack-type intersection)
          *empty-type*)))

  (!define-superclasses simd-pack ((simd-pack)) !cold-init-forms))

#+sb-simd-pack-256
(progn
  (define-type-class simd-pack-256 :enumerable nil :might-contain-other-types nil)

  ;; Though this involves a recursive call to parser, parsing context need not
  ;; be passed down, because an unknown-type condition is an immediate failure.
  (def-type-translator simd-pack-256 (&optional (element-type-spec '*))
    (if (eql element-type-spec '*)
        (%make-simd-pack-256-type (make-array (length *simd-pack-element-types*)
                                              :element-type 'bit :initial-element 1))
        (make-simd-pack-256-type (single-value-specifier-type element-type-spec))))

  (define-type-method (simd-pack-256 :negate) (type)
    (let* ((element-type (simd-pack-256-type-element-type type))
           (remaining (and (/= (count 0 element-type) 0) (bit-not element-type)))
           (not-simd-pack-256 (make-negation-type (specifier-type 'simd-pack-256))))
      (if remaining
          (type-union not-simd-pack-256 (%make-simd-pack-256-type remaining))
          not-simd-pack-256)))

  (define-type-method (simd-pack-256 :unparse) (type)
    (let* ((eltypes (simd-pack-256-type-element-type type)))
      (cond
        ((= (count 0 eltypes) 0) 'simd-pack-256)
        ((= (count 1 eltypes) 1)
         (let ((pos (position 1 eltypes)))
           (if pos
               `(simd-pack-256 ,(elt *simd-pack-element-types* pos))
               (bug "bad simd-pack-256"))))
        (t
         `(or
           ,@(loop for x from 0
                   for bit across eltypes
                   if (= bit 1)
                   collect `(simd-pack-256 ,(elt *simd-pack-element-types* x))))))))

  (define-type-method (simd-pack-256 :simple-=) (type1 type2)
    (declare (type simd-pack-256-type type1 type2))
    (values
     (= 0 (count 1 (bit-xor (simd-pack-256-type-element-type type1)
                            (simd-pack-256-type-element-type type2))))
     t))

  (define-type-method (simd-pack-256 :simple-subtypep) (type1 type2)
    (declare (type simd-pack-256-type type1 type2))
    (values
     (= 0 (count 1 (bit-andc2 (simd-pack-256-type-element-type type1)
                              (simd-pack-256-type-element-type type2))))
     t))

  (define-type-method (simd-pack-256 :simple-union2) (type1 type2)
    (declare (type simd-pack-256-type type1 type2))
    (%make-simd-pack-256-type (bit-ior (simd-pack-256-type-element-type type1)
                                       (simd-pack-256-type-element-type type2))))

  (define-type-method (simd-pack-256 :simple-intersection2) (type1 type2)
    (declare (type simd-pack-256-type type1 type2))
    (let ((intersection (bit-and (simd-pack-256-type-element-type type1)
                                 (simd-pack-256-type-element-type type2))))
      (if (find 1 intersection)
          (%make-simd-pack-256-type intersection)
          *empty-type*)))

  (!define-superclasses simd-pack-256 ((simd-pack-256)) !cold-init-forms))

#+sb-simd-pack-512
(progn
  (define-type-class simd-pack-512 :enumerable nil :might-contain-other-types nil)

  ;; Though this involves a recursive call to parser, parsing context need not
  ;; be passed down, because an unknown-type condition is an immediate failure.
  (def-type-translator simd-pack-512 (&optional (element-type-spec '*))
    (if (eql element-type-spec '*)
        (%make-simd-pack-512-type (make-array (length *simd-pack-element-types*)
                                              :element-type 'bit :initial-element 1))
        (make-simd-pack-512-type (single-value-specifier-type element-type-spec))))

  (define-type-method (simd-pack-512 :negate) (type)
    (let* ((element-type (simd-pack-512-type-element-type type))
           (remaining (and (/= (count 0 element-type) 0) (bit-not element-type)))
           (not-simd-pack-512 (make-negation-type (specifier-type 'simd-pack-512))))
      (if remaining
          (type-union not-simd-pack-512 (%make-simd-pack-512-type remaining))
          not-simd-pack-512)))

  (define-type-method (simd-pack-512 :unparse) (type)
    (let* ((eltypes (simd-pack-512-type-element-type type)))
      (cond
        ((= (count 0 eltypes) 0) 'simd-pack-512)
        ((= (count 1 eltypes) 1)
         (let ((pos (position 1 eltypes)))
           (if pos
               `(simd-pack-512 ,(elt *simd-pack-element-types* pos))
               (bug "bad simd-pack-512"))))
        (t
         `(or
           ,@(loop for x from 0
                   for bit across eltypes
                   if (= bit 1)
                   collect `(simd-pack-512 ,(elt *simd-pack-element-types* x))))))))

  (define-type-method (simd-pack-512 :simple-=) (type1 type2)
    (declare (type simd-pack-512-type type1 type2))
    (values
     (= 0 (count 1 (bit-xor (simd-pack-512-type-element-type type1)
                            (simd-pack-512-type-element-type type2))))
     t))

  (define-type-method (simd-pack-512 :simple-subtypep) (type1 type2)
    (declare (type simd-pack-512-type type1 type2))
    (values
     (= 0 (count 1 (bit-andc2 (simd-pack-512-type-element-type type1)
                              (simd-pack-512-type-element-type type2))))
     t))

  (define-type-method (simd-pack-512 :simple-union2) (type1 type2)
    (declare (type simd-pack-512-type type1 type2))
    (%make-simd-pack-512-type (bit-ior (simd-pack-512-type-element-type type1)
                                       (simd-pack-512-type-element-type type2))))

  (define-type-method (simd-pack-512 :simple-intersection2) (type1 type2)
    (declare (type simd-pack-512-type type1 type2))
    (let ((intersection (bit-and (simd-pack-512-type-element-type type1)
                                 (simd-pack-512-type-element-type type2))))
      (if (find 1 intersection)
          (%make-simd-pack-512-type intersection)
          *empty-type*)))

  (!define-superclasses simd-pack-512 ((simd-pack-512)) !cold-init-forms))

;;;; utilities shared between cross-compiler and target system

;;; Does the type derived from compilation of an actual function
;;; definition satisfy declarations of a function's type?
(defun defined-ftype-matches-declared-ftype-p (defined-ftype declared-ftype)
  (declare (type ctype defined-ftype declared-ftype))
  (flet ((is-built-in-class-function-p (ctype)
           (and (built-in-classoid-p ctype)
                (eq (built-in-classoid-name ctype) 'function))))
    (cond (;; DECLARED-FTYPE could certainly be #<BUILT-IN-CLASS FUNCTION>;
           ;; that's what happens when we (DECLAIM (FTYPE FUNCTION FOO)).
           (is-built-in-class-function-p declared-ftype)
           ;; In that case, any definition satisfies the declaration.
           t)
          (;; It's not clear whether or how DEFINED-FTYPE might be
           ;; #<BUILT-IN-CLASS FUNCTION>, but it's not obviously
           ;; invalid, so let's handle that case too, just in case.
           (is-built-in-class-function-p defined-ftype)
           ;; No matter what DECLARED-FTYPE might be, we can't prove
           ;; that an object of type FUNCTION doesn't satisfy it, so
           ;; we return success no matter what.
           t)
          (;; Otherwise both of them must be FUN-TYPE objects.
           t
           ;; FIXME: For now we only check compatibility of the return
           ;; type, not argument types, and we don't even check the
           ;; return type very precisely (as per bug 94a). It would be
           ;; good to do a better job. Perhaps to check the
           ;; compatibility of the arguments, we should (1) redo
           ;; VALUES-TYPES-EQUAL-OR-INTERSECT as
           ;; ARGS-TYPES-EQUAL-OR-INTERSECT, and then (2) apply it to
           ;; the ARGS-TYPE slices of the FUN-TYPEs. (ARGS-TYPE
           ;; is a base class both of VALUES-TYPE and of FUN-TYPE.)
           (values-types-equal-or-intersect
            (fun-type-returns defined-ftype)
            (fun-type-returns declared-ftype))))))

;;; This messy case of CTYPE for NUMBER is shared between the
;;; cross-compiler and the target system.
;;; I'm not sure whether NaNs should be numeric types versus MEMBER (like
;;; singleton signed zero without the "other" sign), but it may not matter.
;;; At a bare minimum this prevents crashing in min/max.
(defun ctype-of-number (x)
  (let ((num (if (complexp x) (realpart x) x)))
    (multiple-value-bind (complexp low high)
        (cond ((complexp x)
               (let ((imag (imagpart x)))
                 (if (and (floatp num) (or (float-nan-p num) (float-nan-p imag)))
                     (values :complex nil nil)
                     (values :complex (sb-xc:min num imag) (sb-xc:max num imag)))))
              ((and (floatp num) (float-nan-p num))
               (values :real nil nil))
              (t
               (values :real num num)))
      (make-numeric-type :class (etypecase num
                                  (integer (if (complexp x)
                                               (if (integerp (imagpart x))
                                                   'integer
                                                   'rational)
                                               'integer))
                                  (rational 'rational)
                                  (float 'float))
                         :format (and (floatp num) (float-format-name num))
                         :complexp complexp
                         :low low
                         :high high))))

;;; The following function is a generic driver for approximating
;;; set-valued functions over types.  Putting this here because it'll
;;; probably be useful for a lot of type analyses.
;;;
;;; Let f be a function from values of type X to Y, e.g., ARRAY-RANK.
;;;
;;; We compute an over or under-approximation of the set
;;;
;;;  F(TYPE) = { f(x) : x in TYPE /\ x in X } \subseteq Y
;;;
;;; via set-valued approximations of f, OVER and UNDER.
;;;
;;; These functions must have the property that
;;;   Forall TYPE, OVER(TYPE) \superseteq F(TYPE) and
;;;   Forall TYPE, UNDER(TYPE) \subseteq F(TYPE)
;;;
;;; The driver is also parameterised over the finite set
;;; representation.
;;;
;;; Union, intersection and difference are binary functions to compute
;;; set union, intersection and difference.  Top and bottom are the
;;; concrete representations for the universe and empty sets; we never
;;; call the set functions on top or bottom, so it's safe to use
;;; special values there.
;;;
;;; Arguments:
;;;
;;;  TYPE: the ctype for which we wish to approximate F(TYPE)
;;;  OVERAPPROXIMATE: true if we wish to overapproximate, nil otherwise.
;;;     You usually want T.
;;;  UNION/INTERSECTION/DIFFERENCE: implementations of finite set operations.
;;;     Conform to cl::(union/intersection/set-difference).  Passing NIL will
;;;     disable some cleverness and result in quicker computation of coarser
;;;     approximations.  However, passing difference without union and intersection
;;;     will probably not end well.
;;;  TOP/BOTTOM: concrete representation of the universe and empty set.  Finite
;;;     set operations are never called on TOP/BOTTOM, so it's safe to use special
;;;     values there.
;;;  OVER/UNDER: the set-valued approximations of F.
;;;
;;; Implementation details.
;;;
;;; It's a straightforward walk down the type.
;;; Union types -> take the union of children, intersection ->
;;; intersect.  There is some complication for negation types: we must
;;; not only negate the result, but also flip from overapproximating
;;; to underapproximating in the children (or vice versa).
;;;
;;; We represent sets as a pair of (negate-p finite-set) in order to
;;; support negation types.

(declaim (maybe-inline generic-abstract-type-function))
(defun generic-abstract-type-function
    (type overapproximate
     union intersection difference
     top bottom
     over under)
  (labels ((union* (x y)
             ;; wrappers to avoid calling union/intersection on
             ;; top/bottom.
             (cond ((or (eql x top)
                        (eql y top))
                    top)
                   ((eql x bottom) y)
                   ((eql y bottom) x)
                   (t
                    (funcall union x y))))
           (intersection* (x y)
             (cond ((or (eql x bottom)
                        (eql y bottom))
                    bottom)
                   ((eql x top) y)
                   ((eql y top) x)
                   (t
                    (funcall intersection x y))))
           (unite (not-x-p x not-y-p y)
             ;; if we only have one negated set, it's x.
             (when not-y-p
               (rotatef not-x-p not-y-p)
               (rotatef x y))
             (cond ((and not-x-p not-y-p)
                    ;; -x \/ -y = -(x /\ y)
                    (normalize t (intersection* x y)))
                   (not-x-p
                    ;; -x \/ y = -(x \ y)
                    (cond ((eql x top)
                           (values nil y))
                          ((or (eql y top)
                               (eql x bottom))
                           (values nil top))
                          ((eql y bottom)
                           (values t x))
                          (t
                           (normalize t
                                      (funcall difference x y)))))
                   (t
                    (values nil (union* x y)))))
           (intersect (not-x-p x not-y-p y)
             (when not-y-p
               (rotatef not-x-p not-y-p)
               (rotatef x y))
             (cond ((and not-x-p not-y-p)
                    ;; -x /\ -y = -(x \/ y)
                    (normalize t (union* x y)))
                   (not-x-p
                    ;; -x /\ y = y \ x
                    (cond ((or (eql x top) (eql y bottom))
                           (values nil bottom))
                          ((eql x bottom)
                           (values nil y))
                          ((eql y top)
                           (values t x))
                          (t
                           (values nil (funcall difference y x)))))
                   (t
                    (values nil (intersection* x y)))))
           (normalize (not-x-p x)
             ;; catch some easy cases of redundant negation.
             (cond ((not not-x-p)
                    (values nil x))
                   ((eql x top)
                    bottom)
                   ((eql x bottom)
                    top)
                   (t
                    (values t x))))
           (default (overapproximate)
             ;; default value
             (if overapproximate top bottom))
           (walk-union (types overapproximate)
             ;; Only do this if union is provided.
             (unless union
               (return-from walk-union (default overapproximate)))
             ;; Reduce/union from bottom.
             (let ((not-acc-p nil)
                   (acc bottom))
               (dolist (type types (values not-acc-p acc))
                 (multiple-value-bind (not x)
                     (walk type overapproximate)
                   (setf (values not-acc-p acc)
                         (unite not-acc-p acc not x)))
                 ;; Early exit on top set.
                 (when (and (eql acc top)
                            (not not-acc-p))
                   (return (values nil top))))))
           (walk-intersection (types overapproximate)
             ;; Skip if we don't know how to intersect sets
             (unless intersection
               (return-from walk-intersection (default overapproximate)))
             ;; Reduce/intersection from top
             (let ((not-acc-p nil)
                   (acc top))
               (dolist (type types (values not-acc-p acc))
                 (multiple-value-bind (not x)
                     (walk type overapproximate)
                   (setf (values not-acc-p acc)
                         (intersect not-acc-p acc not x)))
                 (when (and (eql acc bottom)
                            (not not-acc-p))
                   (return (values nil bottom))))))
           (walk-negate (type overapproximate)
             ;; Don't introduce negated types if we don't know how to
             ;; subtract sets.
             (unless difference
               (return-from walk-negate (default overapproximate)))
             (multiple-value-bind (not x)
                 (walk type (not overapproximate))
               (normalize (not not) x)))
           (walk (type overapproximate)
             (typecase type
               (union-type
                (walk-union (union-type-types type) overapproximate))
               ((cons (member or union))
                (walk-union (rest type) overapproximate))
               (intersection-type
                (walk-intersection (intersection-type-types type) overapproximate))
               ((cons (member and intersection))
                (walk-intersection (rest type) overapproximate))
               (negation-type
                (walk-negate (negation-type-type type) overapproximate))
               ((cons (eql not))
                (walk-negate (second type) overapproximate))
               (t
                (values nil
                        (if overapproximate
                            (if over
                                (funcall over type)
                                (default t))
                            (if under
                                (funcall under type)
                                (default nil))))))))
    (multiple-value-call #'normalize (walk type overapproximate))))

;;; Standard list representation of sets. Use CL:* for the universe.
(defun list-abstract-type-function (type over &key under (overapproximate t))
  #-sb-xc-host (declare (inline generic-abstract-type-function))
  (generic-abstract-type-function
   type overapproximate
   #'union #'intersection #'set-difference
   '* nil
   over under))


;;; This decides if two type expressions are equal ignoring the order of terms
;;; in AND and OR. It doesn't decide equivalence, but it's good enough
;;; to do some sanity checking in type.before-xc and genesis.
(defun brute-force-type-specifier-equalp (a b)
  (labels ((compare (a b)
             (if (symbolp a)
                 (eq a b)
                 (or (equal a b)
                     (and (listp b)
                          (eq (car a) (car b))
                          (case (car a)
                            ((and or)
                             (order-insensitive-equal (cdr a) (cdr b)))
                            ((not)
                             (compare (cadr a) (cadr b))))))))
           (order-insensitive-equal (a b)
             (and (= (length a) (length b))
                  (every (lambda (elt) (member elt b :test #'compare)) a)
                  (every (lambda (elt) (member elt a :test #'compare)) b))))
    (compare a b)))

;;;; miscellaneous interfaces

;;; Clear memoization of all type system operations that can be
;;; altered by type definition/redefinition.
;;;
(defun clear-type-caches ()
  ;; FIXME: We would like to differentiate between different cache
  ;; kinds, but at the moment all our caches pretty much are type
  ;; caches.
  (drop-all-hash-caches)
  (values))

;;; This is like TYPE-OF, only we return a CTYPE structure instead of
;;; a type specifier, and we try to return the type most useful for
;;; type checking, rather than trying to come up with the one that the
;;; user might find most informative.
;;;
;;; To avoid inadvertent memory retention we avoid using arrays
;;; and functions as keys.
;;; During cross-compilation, the CTYPE-OF function is not memoized.
;;; Constants get their type stored in their LEAF, so it's ok.
#-sb-xc-host
(defun-cached (ctype-of :hash-bits 7 :hash-function #'sxhash
                        :memoizer memoize)
;; an unfortunate aspect of using EQ is that several appearances
;; of the = double-float can be in the cache, but it's
;; probably more efficient overall to use object identity.
    ((x eq))
  (flet ((try-cache (x)
           (memoize
            ;; For functions, the input is a type specifier
            ;; of the form (FUNCTION (...) ...)
            (cond ((listp x) (specifier-type x)) ; NIL can't occur
                  ((symbolp x) (make-eql-type x))
                  (t (ctype-of-number x))))))
    (typecase x
      (function
       (if (funcallable-instance-p x)
           (classoid-of x)
           (let ((type (sb-impl::%fun-ftype x)))
             (if (typep type '(cons (eql function))) ; sanity test
                 (try-cache type)
                 (classoid-of x)))))
      (symbol (if x (try-cache x) (specifier-type 'null)))
      (number (try-cache x))
      (array (ctype-of-array x))
      (cons (specifier-type 'cons))
      (character
       (typecase x
         (standard-char (specifier-type 'standard-char))
         (base-char (specifier-type 'base-char))
         ;; If the last case were expressed as EXTENDED-CHAR,
         ;; we wrongly get "this is not a (VALUES CTYPE): NIL"
         ;; because the compiler is too naive to see that
         ;; the last 2 cases partition CHARACTER.
         (t (specifier-type 'extended-char))))
      #+sb-simd-pack
      (simd-pack
       (let ((tag (%simd-pack-tag x)))
         (svref (load-time-value
                 (coerce (cons (specifier-type 'simd-pack)
                               (mapcar (lambda (x) (specifier-type `(simd-pack ,x)))
                                       *simd-pack-element-types*))
                         'vector)
                 t)
                (if (<= 0 tag #.(1- (length *simd-pack-element-types*)))
                    (1+ tag)
                    0))))
      #+sb-simd-pack-256
      (simd-pack-256
       (let ((tag (%simd-pack-256-tag x)))
         (svref (load-time-value
                 (coerce (cons (specifier-type 'simd-pack-256)
                               (mapcar (lambda (x) (specifier-type `(simd-pack-256 ,x)))
                                       *simd-pack-element-types*))
                         'vector)
                 t)
                (if (<= 0 tag #.(1- (length *simd-pack-element-types*)))
                    (1+ tag)
                    0))))
      #+sb-simd-pack-512
      (simd-pack-512
       (let ((tag (%simd-pack-512-tag x)))
         (svref (load-time-value
                 (coerce (cons (specifier-type 'simd-pack-512)
                               (mapcar (lambda (x) (specifier-type `(simd-pack-512 ,x)))
                                       *simd-pack-element-types*))
                         'vector)
                 t)
                (if (<= 0 tag #.(1- (length *simd-pack-element-types*)))
                    (1+ tag)
                    0))))
      (t
       (classoid-of x)))))

;; Helper function that implements (CTYPE-OF x) when X is an array.
#-sb-xc-host
(defun-cached (ctype-of-array
               :values (ctype) ; Bind putative output to this when probing.
               :hash-bits 7
               :hash-function (lambda (a &aux (hash cookie))
                                (if header-p
                                    (dotimes (axis rank hash)
                                      (mixf hash (%array-dimension a axis)))
                                    (mixf hash (length a)))))
    ;; "type-key" is a perfect hash of rank + widetag + simple-p.
    ;; If it matches, then compare dims, which are read from the output.
    ;; The hash of the type-key + dims can have collisions.
    ((array (lambda (array type-key)
              (and (eq type-key cookie)
                   (let ((dims (array-type-dimensions ctype)))
                     (if header-p
                         (dotimes (axis rank t)
                           (unless (eq (pop (truly-the list dims))
                                       (%array-dimension array axis))
                             (return nil)))
                         (eq (length array) (car dims))))))
            cookie) ; Store COOKIE as the single key.
     &aux (rank (array-rank array))
          (simple-p (if (simple-array-p array) 1 0))
          (header-p (array-header-p array)) ; non-simple or rank <> 1 or both
          (cookie (the fixnum (logior (ash (logior (ash rank 1) simple-p)
                                           sb-vm:n-widetag-bits)
                                      (array-underlying-widetag array)))))
  ;; The value computed on cache miss.
  (let ((etype (sb-vm::array-element-ctype array)))
    (make-array-type (array-dimensions array)
                     :complexp (not (simple-array-p array))
                     :element-type etype
                     :specialized-element-type etype)))

(!defun-from-collected-cold-init-forms !type-cold-init)
