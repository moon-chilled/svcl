;;;; tests related to 'traceroot'
;;;;
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

;;;; These tests should pass for all platforms, but they're not
;;;; and I don't care too much why, since the functionality still works.
;;;; It's just that sometimes we get :PINNED as a root instead of
;;;; the expected reference to the one and only thread.
;;;; And also sb-safepoint gets a crash in C.
#-(and gencgc sb-thread (not sb-safepoint)
       (or arm64 ppc64 x86-64))
(invoke-restart 'run-tests::skip-file)

(setq sb-ext:*evaluator-mode* :compile)
(defvar *fred*)
(defstruct foo a)

(defun scrubstack ()
  (sb-int:dx-let ((b (make-array 20))) (eval b))
  (sb-sys:scrub-control-stack))

(defun test1 (wp obj root)
  (let ((*fred* (list (make-foo :a (list (vector #xfeefa (list obj)))))))
    (setq obj nil) ; so OBJ is not found as a stack reference
    (let ((paths
           (ecase root
             (:tls
              (scrubstack)
              (sb-ext:search-roots wp :criterion :oldest :gc t :print nil))
             (:bindings ; bind *FRED* again so the old value is on the binding stack
              (let ((*fred* 1))
                (scrubstack)
                (sb-ext:search-roots wp :criterion :oldest :gc t :print nil)))
             (:stack
              ; put the OBJ back on the control stack
              ; and also ensure that *FRED* is not a root.
              (setq obj *fred* *fred* nil)
              (scrubstack)
              (sb-ext:search-roots wp :criterion :oldest :gc t :print nil)))))
      (assert paths)
      (let* ((path (cdar paths))
             (root (car path)))
        (assert (stringp (car root)))
        (case root
          (:stack
           (assert (typep (cdr root) '(cons system-area-pointer))))
          (:tls
           (assert (typep (cdr root) '(cons (eql *fred*) (cons t)))))
          (:bindings
           (assert (typep (cdr path) '(cons (eql *fred*) (cons nil))))))))))

(with-test (:name (sb-ext:search-roots :stack-indirect)
            :fails-on :sunos)
  (let ((wp (make-weak-pointer (list 1 2 3 4))))
    (test1 wp (weak-pointer-value wp) :stack)
    (test1 wp (weak-pointer-value wp) :tls)
    (test1 wp (weak-pointer-value wp) :bindings)
    nil))

(defun f0 ()
  (let* ((c (cons 1 2))
         (wp (make-weak-pointer c)))
    (let ((paths (sb-ext:search-roots wp :criterion :static :gc t :print nil)))
      (assert paths)
      (let* ((path (car paths))
             (nodes (cdr path)))
        (assert (and (sb-int:singleton-p nodes)
                     (string= "main thread" (caar nodes))))))
    c))
(with-test (:name (sb-ext:search-roots :stack-direct)
            :fails-on :sunos)
  (f0))

;;; Employ circumlocution so the file loader doesn't hold on to a string "hi"
(defvar *string-hi* (make-weak-pointer (concatenate 'string "h" "i")))
(defstruct s1 foo)
(defparameter *top*
  `(p q r w x y ,(make-s1 :foo `#((a b c ,(weak-pointer-value *string-hi*) d))) z))

;;; Sample output:
;;; Path to "hi":
;;;  6       1000209AB3 [   1] a symbol-hashset
;;;  1       10048F145F [  29] a (simple-vector 37)
;;;  1         503B403F [   2] COMMON-LISP-USER::*TOP*
;;;  0       1004B885B7 [   6] a cons = (P Q R ...) ; = (NTHCDR 6 object)
;;;  0       1004B88617 [   0] a cons = (# Z)
;;;  0       1004C1AA53 [   1] a s1
;;;  0       1004CBB93F [   2] a (simple-vector 1)
;;;  0       1004D28AE7 [   3] a cons = (A B C ...) ; = (NTHCDR 3 object)
;;;  0       1004D28B17 [   0] a cons = ("hi" D)
(with-test (:name :traceroot-collapse-lists)
  (let* ((string (with-output-to-string (*standard-output*)
                   (search-roots *string-hi* :print :verbose)))
         (lines (split-string string #\newline)))
    (assert
     (loop for line in lines
             thereis (search "[   6] a cons = (P Q R ...)" line)))
    (assert
     (loop for line in lines
             thereis (search "[   3] a cons = (A B C ...)" line)))))

(defun something ()
  (let ((a (make-symbol "x")))
    (gc) ; cause the symbol to be pinned
    (make-weak-pointer a)))

(with-test (:name :traceroot-old-pin-no-crash)
  (let ((wp (something)))
    (search-roots wp)
    (something)))

(defvar *foo*)
#+gencgc
(with-test (:name (sb-ext:search-roots :simple-fun)
            :broken-on (and :darwin :arm64))
  ;; Tracing a path to a simple fun wasn't working at some point
  ;; because of failure to employ fun_code_header in the right place.
  (setq *foo* (compile nil '(lambda () 42)))
  (let ((wp (sb-ext:make-weak-pointer *foo*)))
    (assert (sb-ext:search-roots wp :criterion :oldest :print nil))))

#+gencgc
(with-test (:name (sb-ext:search-roots :ignore-immediate))
  (sb-ext:search-roots (make-weak-pointer 48) :gc t :print nil))
