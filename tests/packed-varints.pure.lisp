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

#+(and x86-64 immobile-space sb-unicode (not interpreter)) ; missing symbols otherwise
(with-test (:name :bignum-unpacker-no-consing)
  (flet ((try ()
           (let ((result 0)
                 ;; This happens to be a fairly large bignum (>2500 bits)
                 (b (sb-vm::%code-fixups
                     (sb-kernel:fun-code-header
                      #'sb-unicode::line-break-annotate))))
             (sb-int:do-packed-varints (int b)
               (setq result (logxor result int)))
             result)))
    (ctu:assert-no-consing (try))))
