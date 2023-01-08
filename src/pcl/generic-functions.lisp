;;;; Mostly this file contains generic functions. The exceptions are hacks.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(in-package "SB-PCL")

;;;; readers

(defgeneric accessor-method-slot-definition (standard-accessor-method))

(defgeneric class-can-precede-list (pcl-class))

(defgeneric class-defstruct-constructor (structure-class))

(defgeneric class-defstruct-form (structure-class))

(defgeneric class-direct-subclasses (class))

(defgeneric class-direct-superclasses (class))

(defgeneric class-eq-specializer (class))

(defgeneric class-incompatible-superclass-list (pcl-class))

(defgeneric class-name (class))

(defgeneric class-precedence-list (pcl-class))

(defgeneric class-wrapper (pcl-class))

(defgeneric definition-source (definition-source-mixin))

(defgeneric eql-specializer-object (eql-specializer))

(defgeneric generic-function-declarations (generic-function))

(defgeneric generic-function-method-class (generic-function))

(defgeneric generic-function-method-combination (generic-function))

(defgeneric generic-function-methods (generic-function))

(defgeneric generic-function-name (generic-function))

(defgeneric gf-arg-info (standard-generic-function))

(defgeneric gf-dfun-state (standard-generic-function))

(defgeneric generic-function-initial-methods (standard-generic-function))

(defgeneric long-method-combination-function (long-method-combination))

(defgeneric method-combination-documentation (standard-method-combination))

(defgeneric method-combination-options (standard-method-combination))

(defgeneric method-combination-type-name (standard-method-combination))

(defgeneric method-generic-function (standard-method))

(defgeneric object-plist (plist-mixin))

(defgeneric short-combination-identity-with-one-argument
  (short-method-combination))

(defgeneric short-combination-operator (short-method-combination))

(defgeneric slot-definition-class (slot-definition))

(defgeneric slot-definition-defstruct-accessor-symbol
  (structure-slot-definition))

(defgeneric slot-definition-initargs (slot-definition))

(defgeneric slot-definition-initform (slot-definition))

(defgeneric slot-definition-initfunction (slot-definition))

(defgeneric slot-definition-internal-reader-function
  (structure-slot-definition))

(defgeneric slot-definition-internal-writer-function
  (structure-slot-definition))

(defgeneric slot-definition-location (standard-effective-slot-definition))

(defgeneric slot-definition-name (slot-definition))

(defgeneric slot-definition-info (effective-slot-definition))

(defgeneric slot-definition-readers (slot-definition))

(defgeneric slot-definition-type (slot-definition))

(defgeneric slot-definition-writers (slot-definition))

(defgeneric specializer-object (class-eq-specializer))

(defgeneric specializer-type (specializer))

;;;; writers

(defgeneric (setf class-defstruct-constructor) (new-value structure-class))

(defgeneric (setf class-defstruct-form) (new-value structure-class))

(defgeneric (setf class-direct-slots) (new-value slot-class))

(defgeneric (setf class-incompatible-superclass-list) (new-value pcl-class))

(defgeneric (setf class-name) (new-value class))

(defgeneric (setf class-slots) (new-value slot-class))

(defgeneric (setf generic-function-method-class) (new-value
                                                  standard-generic-function))

(defgeneric (setf generic-function-method-combination)
  (new-value standard-generic-function))

(defgeneric (setf generic-function-declarations) (new-value
                                                  standard-generic-function))

(defgeneric (setf generic-function-methods) (new-value
                                             standard-generic-function))

(defgeneric (setf generic-function-name) (new-value standard-generic-function))

(defgeneric (setf gf-dfun-state) (new-value standard-generic-function))

(defgeneric (setf generic-function-initial-methods)
  (new-value standard-generic-function))

(defgeneric (setf method-generic-function) (new-value standard-method))

(defgeneric (setf object-plist) (new-value plist-mixin))

(defgeneric (setf slot-definition-allocation) (new-value
                                               standard-slot-definition))

(defgeneric (setf slot-definition-class) (new-value slot-definition))

(defgeneric (setf slot-definition-defstruct-accessor-symbol)
  (new-value structure-slot-definition))

(defgeneric (setf slot-definition-initargs) (new-value slot-definition))

(defgeneric (setf slot-definition-initform) (new-value slot-definition))

(defgeneric (setf slot-definition-initfunction) (new-value slot-definition))

(defgeneric (setf slot-definition-internal-reader-function)
  (new-value structure-slot-definition))

(defgeneric (setf slot-definition-internal-writer-function)
  (new-value structure-slot-definition))

(defgeneric (setf slot-definition-location)
  (new-value standard-effective-slot-definition))

(defgeneric (setf slot-definition-name) (new-value slot-definition))

(defgeneric (setf slot-definition-info) (new-value effective-slot-definition))

(defgeneric (setf slot-definition-readers) (new-value slot-definition))

(defgeneric (setf slot-definition-type) (new-value slot-definition))

(defgeneric (setf slot-definition-writer-function)
  (new-value effective-slot-definition))

(defgeneric (setf slot-definition-writers) (new-value slot-definition))

;;;; 1 argument

(defgeneric accessor-method-slot-name (m))

(defgeneric class-default-initargs (class))

(defgeneric class-direct-default-initargs (class))

(defgeneric class-direct-slots (class))

(defgeneric class-finalized-p (class))

(defgeneric class-prototype (class))

(defgeneric class-slot-cells (class))

(defgeneric class-slots (class))

(defgeneric compute-class-precedence-list (root))

(defgeneric compute-default-initargs (class))

(defgeneric compute-discriminating-function (gf))

(defgeneric compute-discriminating-function-arglist-info (generic-function))

(defgeneric compute-slots (class))

(defgeneric finalize-inheritance (class))

(defgeneric function-keywords (method))

(defgeneric generic-function-argument-precedence-order (generic-function))

(defgeneric generic-function-lambda-list (generic-function))

(defgeneric generic-function-pretty-arglist (generic-function &optional current-defmethod))

(defgeneric gf-fast-method-function-p (gf))

(defgeneric initialize-internal-slot-functions (slotd))

(defgeneric make-instances-obsolete (class))

(defgeneric method-function (method))

(defgeneric method-lambda-list (m))

(defgeneric method-qualifiers (m))

(defgeneric method-specializers (m))

(defgeneric slot-definition-allocation (slotd))

(defgeneric specializer-class (specializer))

(defgeneric specializer-direct-generic-functions (specializer))

(defgeneric specializer-direct-methods (specializer))

(defgeneric specializer-method-table (specializer))

(defgeneric specializer-method-holder (specializer &optional create))

(defgeneric update-constructors (class))

;;;; 2 arguments

(defgeneric add-dependent (metaobject dependent))

(defgeneric add-direct-method (specializer method))

(defgeneric add-direct-subclass (class subclass))

(defgeneric add-method (generic-function method))

(defgeneric (setf class-slot-cells) (new-value class))

(defgeneric class-slot-value (class slot-name))

(defgeneric compatible-meta-class-change-p (class proto-new-class))

(defgeneric compute-applicable-methods (generic-function arguments))

(defgeneric compute-applicable-methods-using-classes
  (generic-function classes))

(defgeneric compute-effective-slot-definition-initargs (class direct-slotds))

(defgeneric describe-object (object stream))

(defgeneric direct-slot-definition-class (class &rest initargs))

(defgeneric effective-slot-definition-class (class &rest initargs))

(defgeneric make-reader-method-function (class slot-name))

(defgeneric make-writer-method-function (class slot-name))

(defgeneric map-dependents (metaobject function))

(defgeneric parse-specializer-using-class (generic-function specializer-name)
  (:documentation
   "Parse SPECIALIZER-NAME into a specializer object suitable for GENERIC-FUNCTION.

If SPECIALIZER-NAME is not well-formed with respect to the specializer
syntax of GENERIC-FUNCTION, an error of type
SB-PCL:SPECIALIZER-NAME-SYNTAX-ERROR is signaled.

If GENERIC-FUNCTION is a STANDARD-GENERIC-FUNCTION and
SPECIALIZER-NAME is a symbol that does not name an existing class, an
error of type SB-PCL:CLASS-NOT-FOUND-ERROR is signaled.

Other errors may be signaled for generic function classes other than
STANDARD-GENERIC-FUNCTION.

If GENERIC-FUNCTION is a STANDARD-GENERIC-FUNCTION and
SPECIALIZER-NAME is of the form (eql OBJECT), OBJECT is not
evaluated (in contrast to DEFMETHOD's behavior).

NOTE: This generic function is part of an SBCL-specific experimental
protocol. Interface subject to change."))

(defgeneric remove-dependent (metaobject dependent))

(defgeneric remove-direct-method (specializer method))

(defgeneric remove-direct-subclass (class subclass))

(defgeneric remove-method (generic-function method))

(defgeneric remove-reader-method (class generic-function))

(defgeneric remove-writer-method (class generic-function))

(defgeneric same-specializer-p (specl1 specl2))

(defgeneric slot-accessor-function (slotd type))

(defgeneric slot-accessor-std-p (slotd type))

;;; This controls DESCRIBE-OBJECT (SLOT-OBJECT STREAM) behavior.
(defgeneric slots-to-inspect (class object))

(defgeneric unparse-specializer-using-class (generic-function specializer))

(defgeneric validate-superclass (class superclass))

(defgeneric invalid-superclass (class superclass))


;;;; 3 arguments

(defgeneric (setf class-slot-value) (nv class slot-name))

;;; CMUCL comment (from Gerd Moellmann/Pierre Mai, 2002-10-19):
;;;
;;; According to AMOP, COMPUTE-EFFECTIVE-METHOD should return two
;;; values.  Alas, the second value is only vaguely described in AMOP,
;;; and, when asked on 2002-10-18, Gregor Kiczales said he couldn't
;;; remember what the second value was supposed to be.  So, PCL's
;;; COMPUTE-EFFECTIVE-METHOD returns one value as do Allegro and
;;; Lispworks.
(defgeneric compute-effective-method (generic-function
                                      combin
                                      applicable-methods))

(defgeneric compute-effective-slot-definition (class name dslotds))

(defgeneric compute-slot-accessor-info (slotd type gf))

(defgeneric find-method-combination (generic-function type options))

(defgeneric invalid-qualifiers (generic-function combin method))

(defgeneric (setf slot-accessor-function) (function slotd type))

(defgeneric (setf slot-accessor-std-p) (value slotd type))

(defgeneric slot-boundp-using-class (class object slotd))

(defgeneric slot-makunbound-using-class (class object slotd))

(defgeneric slot-unbound (class instance slot-name))

(defgeneric slot-value-using-class (class object slotd))

(defgeneric specializer-type-specifier
    (proto-generic-function proto-method specializer)
  (:documentation
   "Return a type specifier for SPECIALIZER, a non-parsed specializer
form or a SPECIALIZER instance.

More specifically, SPECIALIZER can be
* a non-parsed specializer form such as
  * a symbol naming a class
  * a list of the form (eql OBJECT)
  * a list of the form (SPECIALIZER-KIND &rest SPECIFIC-SYNTAX)
* an instance of a subclass of SPECIALIZER

When SPECIALIZER cannot be parsed/used as a specializer for
PROTO-GENERIC-FUNCTION and PROTO-METHOD, a STYLE-WARNING is signaled
and NIL is returned. No type declaration will be generated in this
case.

NIL can also be returned if SPECIALIZER is valid but its type should
not be declared, for example for efficiency reasons.

NOTE: This generic function is part of an SBCL-specific experimental
protocol. Interface subject to change."))

;;;; 4 arguments

(defgeneric make-method-lambda
    (proto-generic-function proto-method lambda-expression environment))

(defgeneric make-method-specializers-form
    (proto-generic-function proto-method specializer-names environment))

;;; MAKE-SPECIALIZER-FORM-USING-CLASS
;;;
;;; To free every new custom generic function class from having to
;;; implement iteration over specializers in
;;; MAKE-METHOD-SPECIALIZERS-FORM, we provide a default method
;;;
;;;   make-method-specializers-form standard-g-f standard-method
;;;
;;; which performs this iteration and calls the generic function
;;;
;;;   make-specializer-form-using-class proto-g-f proto-m specializer-name env
;;;
;;; on which custom generic function classes can install methods to
;;; handle their custom specializers. The generic function uses OR
;;; method combination to allow the following idiom:
;;;
;;;   (defmethod make-specializer-form-using-class or
;;;       (proto-generic-function MY-GENERIC-FUNCTION)
;;;       (proto-method standard-method)
;;;       (specializer-name cons)
;;;       (environment t))
;;;     (when (typep specializer-name '(cons (eql MY-SPECIALIZER)))
;;;       MY-SPECIALIZER-FORM))
;;;
;;; The OR method combination lets everything but (my-specializer ...)
;;; fall through to the next methods which will, at some point, handle
;;; class and eql specializers and eventually reach an error signaling
;;; method for invalid specializers.

(defgeneric make-specializer-form-using-class
    (proto-generic-function proto-method specializer-name environment)
  (:method-combination or)
  (:documentation
   "Return a form which, when evaluated in the lexical environment
described by ENVIRONMENT, parses the specializer SPECIALIZER-NAME and
yields the appropriate specializer object.

Both PROTO-GENERIC-FUNCTION and PROTO-METHOD may be
uninitialized. However their classes and prototypes can be
inspected.

NOTE: This generic function is part of an SBCL-specific experimental
protocol. Interface subject to change."))

(defgeneric (setf slot-value-using-class) (new-value class object slotd))

;;;; 5 arguments

(defgeneric add-reader-method (class generic-function slot-name slot-documentation source-location))

(defgeneric add-writer-method (class generic-function slot-name slot-documentation source-location))

(defgeneric make-method-initargs-form
    (proto-generic-function proto-method lambda-expression lambda-list
     environment))

;;;; 6 arguments

(defgeneric make-method-lambda-using-specializers
    (proto-generic-function proto-method qualifiers specializers
     method-lambda environment)
  (:documentation
   "Compute a method lambda form based on METHOD-LAMBDA, possibly
taking into account PROTO-GENERIC-FUNCTION, PROTO-METHOD, QUALIFIERS,
SPECIALIZERS and ENVIRONMENT.

Both PROTO-GENERIC-FUNCTION and PROTO-METHOD may be
uninitialized. However, their classes and prototypes can be inspected.

SPECIALIZERS is a list of specializer objects (i.e. parsed).

Return three values:
1. the created method lambda form
2. initargs for the method instance
3. a (possibly modified) unspecialized method lambda list or nil if
   the unspecialized lambda list contained in METHOD-LAMBDA should be
   used

NOTE: This generic function is part of an SBCL-specific experimental
protocol. Interface subject to change."))

;;;; optional arguments

(defgeneric get-method (generic-function
                        qualifiers
                        specializers
                        &optional errorp))

(defgeneric find-method (generic-function
                         qualifiers
                         specializers
                         &optional errorp))

(defgeneric slot-missing (class
                          instance
                          slot-name
                          operation
                          &optional new-value))

;;;; &KEY arguments

;;; FIXME: make the declared &KEY arguments here agree with those that
;;; AMOP specifies.
(defgeneric allocate-instance (class &rest initargs))

(defgeneric ensure-class-using-class (class
                                      name
                                      &rest args
                                      &key &allow-other-keys))

(defgeneric ensure-generic-function-using-class (generic-function
                                                 fun-name
                                                 &key &allow-other-keys))

(defgeneric initialize-instance (instance &rest initargs &key &allow-other-keys))

(defgeneric make-instance (class &rest initargs &key &allow-other-keys))

(defgeneric change-class (instance new-class-name &rest initargs &key &allow-other-keys))

(defgeneric no-applicable-method (generic-function &rest args))

(defgeneric no-next-method (generic-function method &rest args))

(defgeneric no-primary-method (generic-function &rest args))

(defgeneric reader-method-class (class direct-slot &rest initargs))

(defgeneric reinitialize-instance (instance &rest initargs &key &allow-other-keys))

(defgeneric shared-initialize (instance slot-names &rest initargs
                               &key &allow-other-keys))

(defgeneric update-dependent (metaobject dependent &rest initargs))

(defgeneric update-instance-for-different-class (previous
                                                 current
                                                 &rest initargs))

(defgeneric update-instance-for-redefined-class (instance
                                                 added-slots
                                                 discarded-slots
                                                 property-list
                                                 &rest initargs))

(defgeneric writer-method-class (class direct-slot &rest initargs))
