;;;; DEFGLOBAL and related tests

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(proclaim '(special *foo*))

(defun eval* (form)
  (let ((*evaluator-mode* :interpret))
    (eval form)))

(defun assert-foo-not-checked (fun)
  (let* ((marker (sb-kernel:make-unbound-marker))
         (*foo* marker))
    (assert (eq marker (funcall fun)))))

(defun assert-foo-checked (fun)
  (let* ((marker (sb-kernel:make-unbound-marker))
         (*foo* marker))
    (assert (eq :error
                (handler-case
                    (funcall fun)
                  (unbound-variable (e)
                    (assert (eq '*foo* (cell-error-name e)))
                    :error))))))

(with-test (:name :unbound-cannot-be-always-bound)
  (assert-error (proclaim '(sb-ext:always-bound *foo*))))

(set '*foo* t)
(proclaim '(sb-ext:always-bound *foo*))

(defun foo-safe ()
  (declare (optimize (safety 3)))
  *foo*)
;; When run interpreted, FOO-SAFE cannot help but check BOUNDP on *foo*
;; so the assertion would fail.
(compile 'foo-safe)

(with-test (:name :always-bound-elides-boundness-checking)
  (assert-foo-not-checked #'foo-safe))

(with-test (:name :cannot-unbind-always-bound)
  (assert (eq :oops
              (handler-case
                  (makunbound '*foo*)
                (error () :oops)))))

(defun can-globalize-p (x)
  (handler-case
      (progn (proclaim `(sb-ext:global ,x)) t)
    (error () nil)))

(with-test (:name :cannot-proclaim-special-global)
  (assert (not (can-globalize-p '*foo*))))

(define-symbol-macro sm 42)
(with-test (:name :cannot-proclaim-symbol-macro-global)
  (assert (not (can-globalize-p 'sm))))

(defconstant con 13)
(with-test (:name :cannot-proclaim-constant-global)
  (assert (not (can-globalize-p 'con))))

(with-test (:name :proclaim-global)
  (assert (can-globalize-p '.bar.)))

(defun bar1 () .bar.)
(with-test (:name :global-does-not-imply-always-bound)
  (assert (eq '.bar.
              (handler-case
                  (bar1)
                (unbound-variable (e)
                  (cell-error-name e))))))

(with-test (:name :set-global)
  (setf .bar. 7)
  (assert (= 7 (bar1)))
  (setf .bar. 123)
  (assert (= 123 (bar1))))

(with-test (:name :cannot-bind-globals)
  (assert-error (eval* '(let ((.bar. 6)) .bar.)))
  (multiple-value-bind (fun failure-p)
      (checked-compile `(lambda ()
                          (let ((.bar. 5)) .bar.))
                       :allow-failure t)
    (assert failure-p)
    (assert-error (funcall fun))))

(with-test (:name :cannot-define-globals-as-symmacs)
  (assert-error (eval* '(define-symbol-macro .bar. 0)))
  (assert-error (eval* `(symbol-macrolet ((.bar. 11)) .bar.)))
  (multiple-value-bind (fun failure-p)
      (checked-compile `(lambda ()
                          (symbol-macrolet ((.bar. 11)) .bar.))
                       :allow-failure t)
    (assert failure-p)
    (assert-error (funcall fun))))

;;; Cannot proclaim or declare a global as special
(with-test (:name :cannot-declare-global-special)
  (assert-error (proclaim '(special .bar. 666)))
  (assert-error (eval `(locally (declare (special .bar.)) .bar.)))
  (multiple-value-bind (fun failure-p)
      (checked-compile `(lambda ()
                          (declare (special .bar.))
                          .bar.)
                       :allow-failure t)
    (assert failure-p)
    (assert-error (funcall fun))))

;;; Dead globals get bound checks
(declaim (global this-is-unbound))
(with-test (:name :dead-unbound-global)
  (let ((fun (checked-compile '(lambda ()
                                (declare (optimize safety))
                                this-is-unbound
                                42))))
    (assert-error (funcall fun) unbound-variable)))

(defun compile-form (form)
  (with-scratch-file (lisp "lisp")
    (with-open-file (f lisp :direction :output)
      (prin1 form f))
    (let ((fasl (scratch-file-name "fasl")))
      (multiple-value-bind (fasl warn fail) (compile-file lisp :output-file fasl)
        (declare (ignore warn))
        (when fail
          (error "compiling ~S failed" form))
        fasl))))

(defvar *counter*)
(with-test (:name :defconstant-evals)
  (let* ((*counter* 0)
         (fasl (compile-form `(defglobal .counter-1. (incf *counter*)))))
    (assert (= 1 *counter*))
    (assert (= 1 (symbol-value '.counter-1.)))
    (assert (eq :global (sb-int:info :variable :kind '.counter-1.)))
    (unwind-protect
         (load fasl)
      (ignore-errors (delete-file fasl)))
    (assert (= 1 *counter*))
    (assert (= 1 (symbol-value '.counter-1.))))

  (set '.counter-2. :bound)
  (let* ((*counter* 0)
         (fasl (compile-form `(defglobal .counter-2. (incf *counter*)))))
    (assert (= 0 *counter*))
    (assert (eq :bound (symbol-value '.counter-2.)))
    (assert (eq :global (sb-int:info :variable :kind '.counter-2.)))
    (unwind-protect
         (load fasl)
      (ignore-errors (delete-file fasl)))
    (assert (= 0 *counter*))
    (assert (eq :bound (symbol-value '.counter-2.))))

  ;; This is a *really* dirty trick...
  (let* ((*counter* 0)
         (fasl (let ((.counter-3. :nasty))
                 (declare (special .counter-3.))
                 (compile-form `(defglobal .counter-3. (incf *counter*))))))
    (assert (= 0 *counter*))
    (assert (not (boundp '.counter-3.)))
    (assert (eq :global (sb-int:info :variable :kind '.counter-3.)))
    (unwind-protect
         (load fasl)
      (ignore-errors (delete-file fasl)))
    (assert (= 1 *counter*))
    (assert (= 1 (symbol-value '.counter-3.)))))

(with-test (:name :defglobal-refers-to-defglobal)
  (let ((fasl (compile-form `(progn
                               (defglobal **global-1** :fii)
                               (defglobal **global-2** **global-1**)))))
    (load fasl)
    (ignore-errors (delete-file fasl))
    (assert (eq (symbol-value '**global-1**) (symbol-value '**global-2**)))
    (assert (eq :fii (symbol-value '**global-1**)))))
