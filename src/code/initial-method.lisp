;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PCL")

;;; Initial methods are methods available to use immediately on system startup.
;;; They have the following constraints:
;;; -- Only one (the first) argument may be specialized on.
;;; -- They are unqualified (primary) methods.
;;; Thus exactly one primary method is chosen respecting class
;;; precedence order on method invocation.
(defvar *!initial-methods* '())

(defun !early-load-method (class name quals specls ll lambda source-loc)
  (declare (ignore class))
  (push (list quals specls lambda ll source-loc)
        (cdr (or (assoc name *!deferred-methods*)
                 (car (push (list name) *!deferred-methods*))))))

;;; Slow-but-correct logic for single-dispatch sans method
;;; combination, allowing exactly one primary method. Initial methods
;;; are sorted most-specific-first, so we can stop looking as soon as
;;; a match is found.
(defun initial-call-a-method (gf-name emergency-fallback specialized-arg &rest rest)
  (let* ((methods (the simple-vector
                       (cdr (or (assoc gf-name *!initial-methods*)
                                (error "No methods on ~S" gf-name)))))
         ;; WRAPPER-OF can't be called until its constants have been patched in,
         ;; which is potentially too early in cold init especially if trying
         ;; to debug to figure out what has been patched in.
         ;; And sometimes the thing we need to print is a FMT-CONTROL,
         ;; which means we can see a funcallable-instance here.
         (arg-wrapper
          (cond ((%instancep specialized-arg) (%instance-wrapper specialized-arg))
                ((funcallable-instance-p specialized-arg) (%fun-wrapper specialized-arg))
                ;; Non-instance-like types always call a predicate.
                (t #.(find-layout 't))))
         (applicable-method
          ;; Each "method" is represented as a vector:
          ;;  #(#<GUARD> SPECIALIZER #<FMF>)
          ;; SPECIALIZER is either a symbol for a classoid, or a genesis-time #<LAYOUT>.
          ;; Pick the first applicable one.
          (find-if (lambda (method)
                     (let ((guard (the symbol (svref method 0))))
                       (if (fboundp guard)
                           (funcall guard specialized-arg)
                           (let ((test-wrapper (svref method 1)))
                             (and (sb-kernel::wrapper-p test-wrapper)
                                  (or (find test-wrapper (wrapper-inherits arg-wrapper))
                                      (eq arg-wrapper test-wrapper)))))))
                   methods)))
    (cond (applicable-method
           ;; Call using no permutation-vector / no precomputed next method.
           (apply (svref applicable-method 2) nil nil specialized-arg rest))
          (emergency-fallback
           (apply emergency-fallback specialized-arg rest))
          (t
           (error "No applicable method for ~S on ~S~%" gf-name
                  (type-of specialized-arg))))))

(defun make-load-form (object &optional environment)
  (initial-call-a-method 'make-load-form nil object environment))
(defun print-object (object stream)
  (flet ((last-ditch-effort (object stream)
           ;; Depending on what you're poking at in cold-init it was possible to see
           ;;  "No applicable method for PRINT-OBJECT on SB-KERNEL::RANDOM-CLASS"
           ;; so just print the address and carry on.
           (format stream "#<UNPRINTABLE @ #x~X>" (get-lisp-obj-address object))))
    (initial-call-a-method 'print-object #'last-ditch-effort object stream)))

(macrolet ((ensure-gfs (names)
             `(progn ,@(mapcar (lambda (name)
                                 `(defun ,name (x) (initial-call-a-method ',name nil x)))
                               names))))
  (ensure-gfs (open-stream-p interactive-stream-p input-stream-p output-stream-p
               stream-element-type)))
(defun close (x &key abort) (initial-call-a-method 'close nil x :abort abort))

;;; FIXME: this no longer holds methods, but it seems to have an effect
;;; on the caching of a discriminating function for PRINT-OBJECT
(defvar *!delayed-defmethod-args* nil)

;;; This exists only to show that the cross-compiler can constant-fold
;;; a constant index into a literal array without crashing.
(defun !test-svref-folding ()
  (let ((z #(42 test)))
    (if (< (svref z 0) 100) t nil)))
