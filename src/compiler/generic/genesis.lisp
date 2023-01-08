;;;; "cold" core image builder: This is how we create a target Lisp
;;;; system from scratch, by converting from fasl files to an image
;;;; file in the cross-compilation host, without the help of the
;;;; target Lisp system.
;;;;
;;;; As explained by Rob MacLachlan on the CMU CL mailing list Wed, 06
;;;; Jan 1999 11:05:02 -0500, this cold load generator more or less
;;;; fakes up static function linking. I.e. it makes sure that all the
;;;; DEFUN-defined functions in the fasl files it reads are bound to the
;;;; corresponding symbols before execution starts. It doesn't do
;;;; anything to initialize variable values; instead it just arranges
;;;; for !COLD-INIT to be called at cold load time. !COLD-INIT is
;;;; responsible for explicitly initializing anything which has to be
;;;; initialized early before it transfers control to the ordinary
;;;; top level forms.
;;;;
;;;; (In CMU CL, and in SBCL as of 0.6.9 anyway, functions not defined
;;;; by DEFUN aren't set up specially by GENESIS.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-FASL")

;;; Some build systems frown upon excessive use (or any use) of "-I" options
;;; on the C compiler invocation. So depending on the current working directory
;;; when generating headers and when building, the pathname where we produce
;;; headers may differ from the string specified in #include lines.
;;; The :C-HEADER-DIR-NAME keyword to genesis specifies the output path,
;;; and this symbol (which is normally unbound) specifies the #include prefix.
;;; The normal build is done within src/runtime and does not need
;;; anything done to set this.
(defun genesis-header-prefix ()
  (if (boundp 'cl-user::*genesis-header-prefix*)
      (symbol-value 'cl-user::*genesis-header-prefix*)
      "genesis"))
;;; By the same reasoning as above, lispobj.h is either in "." or a relative path.
(defun lispobj-dot-h ()
  (if (boundp 'cl-user::*lispobj-h-namestring*)
      (symbol-value 'cl-user::*lispobj-h-namestring*)
      "lispobj.h"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package "SB-COREFILE"))

(defun round-up (number size)
  "Round NUMBER up to be an integral multiple of SIZE."
  (* size (ceiling number size)))

;;;; implementing the concept of "vector" in (almost) portable
;;;; Common Lisp
;;;;
;;;; "If you only need to do such simple things, it doesn't really
;;;; matter which language you use." -- _ANSI Common Lisp_, p. 1, Paul
;;;; Graham (evidently not considering the abstraction "vector" to be
;;;; such a simple thing:-)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +smallvec-length+
    (expt 2 16)))

;;; an element of a BIGVEC -- a vector small enough that we have
;;; a good chance of it being portable to other Common Lisps
(deftype smallvec ()
  `(simple-array (unsigned-byte 8) (,+smallvec-length+)))

(defun make-smallvec ()
  (make-array +smallvec-length+ :element-type '(unsigned-byte 8)
              :initial-element 0))

;;; a big vector, implemented as a vector of SMALLVECs
;;;
;;; KLUDGE: This implementation seems portable enough for our
;;; purposes, since realistically every modern implementation is
;;; likely to support vectors of at least 2^16 elements. But if you're
;;; masochistic enough to read this far into the contortions imposed
;;; on us by ANSI and the Lisp community, for daring to use the
;;; abstraction of a large linearly addressable memory space, which is
;;; after all only directly supported by the underlying hardware of at
;;; least 99% of the general-purpose computers in use today, then you
;;; may be titillated to hear that in fact this code isn't really
;;; portable, because as of sbcl-0.7.4 we need somewhat more than
;;; 16Mbytes to represent a core, and ANSI only guarantees that
;;; ARRAY-DIMENSION-LIMIT is not less than 1024. -- WHN 2002-06-13
(defstruct (bigvec (:constructor %make-bigvec ()))
  (outer-vector (vector (make-smallvec)) :type (vector smallvec)))
(defun make-bigvec (&optional (min-size 0))
  (expand-bigvec (%make-bigvec) min-size))

;;; analogous to SVREF, but into a BIGVEC
(defun bvref (bigvec index)
  (multiple-value-bind (outer-index inner-index)
      (floor index +smallvec-length+)
    (aref (the smallvec
            (svref (bigvec-outer-vector bigvec) outer-index))
          inner-index)))
(defun (setf bvref) (new-value bigvec index)
  (multiple-value-bind (outer-index inner-index)
      (floor index +smallvec-length+)
    (setf (aref (the (simple-array (unsigned-byte 8) (*))
                  (svref (bigvec-outer-vector bigvec) outer-index))
                inner-index)
          new-value)))

;;; analogous to LENGTH, but for a BIGVEC
;;;
;;; the length of BIGVEC, measured in the number of BVREFable bytes it
;;; can hold
(defun bvlength (bigvec)
  (* (length (bigvec-outer-vector bigvec))
     +smallvec-length+))

(defparameter *bigvec-for-write-words* (%make-bigvec))
(defun write-words (stream &rest words)
  (let ((bigvec *bigvec-for-write-words*)
        (offset 0))
    (dolist (word words)
      (setf (bvref-word bigvec offset) (the sb-vm:word word))
      (incf offset sb-vm:n-word-bytes))
    (write-sequence (elt (bigvec-outer-vector bigvec) 0) stream :end offset)))

;;; analogous to WRITE-SEQUENCE, but for a BIGVEC
(defun write-bigvec-as-sequence (bigvec stream &key end pad-with-zeros)
  (binding* ((bvlength (bvlength bigvec))
             (data-length (min (or end bvlength) bvlength))
             ;; Compute the coordinates of the final byte to be written
             ((outer-index inner-index)
              (if (zerop data-length)
                  (values 0 -1)
                  (floor (1- data-length) +smallvec-length+))))
    ;; Each SMALLVEC prior to the one indexed by outer-index is written in its entirety
    (dotimes (i outer-index)
      (write-sequence (elt (bigvec-outer-vector bigvec) i) stream))
    ;; The SMALLVEC at OUTER-INDEX is written up to and including INNER-INDEX
    (write-sequence (elt (bigvec-outer-vector bigvec) outer-index) stream
                    :end (1+ inner-index))
    ;; FIXME: This logic from rev 243d0f6f59 says it's needed if +SMALLVEC-LENGTH+ is
    ;; less than backend page bytes, but if that were true (which it never is)
    ;; we should just increase +SMALLVEC-LENGTH+. And how can could it be right even in
    ;; that case? DATA-LENGTH is not larger than BVLENGTH, because it it were,
    ;; you asked to write more than the vector holds. Istm this is garbage
    ;; but I'm afraid to remove it.
    (when (and pad-with-zeros (< bvlength data-length))
      (loop repeat (- data-length bvlength) do (write-byte 0 stream)))))

;;; analogous to READ-SEQUENCE-OR-DIE, but for a BIGVEC
;;; FIXME: should signal error on EOF
(defun read-into-bigvec (bigvec stream start nbytes)
  ;; compute the coordinates of the start and end
  (binding* (((start-outer start-inner) (floor start +smallvec-length+))
             ;; this the INCLUSIVE bound on the ending element
             (end-outer (floor (+ start nbytes -1) +smallvec-length+)))
    ;; if it's all into a single outer vector, take the quick route
    (if (= start-outer end-outer)
        (read-sequence (elt (bigvec-outer-vector bigvec) start-outer) stream
                       :start start-inner :end (+ start-inner nbytes))
        ;;  KISS - use the slow algorithm rather than any "partial read" cleverness
        (loop for i of-type index from start repeat nbytes
              do (setf (bvref bigvec i) (read-byte stream))))))

;;; Grow BIGVEC (exponentially, so that large increases in size have
;;; asymptotic logarithmic cost per byte).
(defun expand-bigvec (bigvec required-length)
  (loop
    (when (>= (bvlength bigvec) required-length)
      (return bigvec))
    (let* ((old-outer-vector (bigvec-outer-vector bigvec))
           (length-old-outer-vector (length old-outer-vector))
           (new-outer-vector (make-array (* 2 length-old-outer-vector))))
      (replace new-outer-vector old-outer-vector)
      (loop for i from length-old-outer-vector below (length new-outer-vector)
            do (setf (svref new-outer-vector i) (make-smallvec)))
      (setf (bigvec-outer-vector bigvec)
            new-outer-vector))))

;;;; looking up bytes and multi-byte values in a BIGVEC (considering
;;;; it as an image of machine memory on the cross-compilation target)

;;; BVREF-32 and friends. These are like SAP-REF-n, except that
;;; instead of a SAP we use a BIGVEC.
(macrolet ((make-bvref-n (n)
            (let ((name (intern (format nil "BVREF-~A" n)))
                  (le-octet-indices
                   (loop with n-octets = (/ n 8)
                         for i from 0 below n-octets
                         collect `(+ byte-index #+big-endian ,(- n-octets i 1)
                                                #-big-endian ,i))))
              `(progn
                 (defun ,name (bigvec byte-index)
                   (logior ,@(loop for index in le-octet-indices
                                   for i from 0
                                   collect `(ash (bvref bigvec ,index) ,(* i 8)))))
                 (defun (setf ,name) (new-value bigvec byte-index)
                   (declare (type (unsigned-byte ,n) new-value))
                   (setf ,@(loop for index in le-octet-indices
                                 for i from 0
                          append `((bvref bigvec ,index)
                                   (ldb (byte 8 ,(* i 8)) new-value))))
                   new-value)))))
  (make-bvref-n 8)
  (make-bvref-n 16)
  (make-bvref-n 32)
  (make-bvref-n 64))

(defun (setf bvref-s32) (newval bv index)
  (setf (bvref-32 bv index) (ldb (byte 32 0) (the (signed-byte 32) newval)))
  newval)

#+host-quirks-sbcl
(progn
  (declaim (inline native-bvref-word (setf native-bvref-word)))
  (defun native-bvref-word (bigvec byte-index)
    (multiple-value-bind (outer-index inner-index) (floor byte-index +smallvec-length+)
      (host-sb-kernel:%vector-raw-bits
       (the smallvec (svref (bigvec-outer-vector bigvec) outer-index))
       (ash inner-index (- sb-vm:word-shift)))))
  (defun (setf native-bvref-word) (newval bigvec byte-index)
    (multiple-value-bind (outer-index inner-index) (floor byte-index +smallvec-length+)
      (setf (host-sb-kernel:%vector-raw-bits
             (the smallvec (svref (bigvec-outer-vector bigvec) outer-index))
             (ash inner-index (- sb-vm:word-shift)))
            newval))))

;; lispobj-sized word, whatever that may be
;; hopefully nobody ever wants a 128-bit SBCL...
(macrolet ((access (bv index &optional alignedp)
             (cond ((and alignedp
                         (and (member :sbcl cl:*features*)
                              (sb-cold::compatible-vector-raw-bits)))
                    `(native-bvref-word ,bv ,index))
                   (t
                    `(#+64-bit bvref-64 #-64-bit bvref-32 ,bv ,index)))))
  (defun (setf bvref-word-unaligned) (new-val bytes index)
    (declare (type sb-xc:fixnum index))
    (setf (access bytes index) new-val))
  (defun (setf bvref-word) (new-val bytes index)
    (declare (type sb-xc:fixnum index))
    (aver (not (logtest index (ash sb-vm:lowtag-mask -1))))
    (setf (access bytes index t) new-val))
  (defun bvref-word (bytes index)
    (declare (type sb-xc:fixnum index))
    (aver (not (logtest index (ash sb-vm:lowtag-mask -1))))
    (access bytes index t)))

;;;; representation of spaces in the core

;;; If there is more than one dynamic space in memory (i.e., if a
;;; copying GC is in use), then only the active dynamic space gets
;;; dumped to core.
(defvar *dynamic*)
(defvar *static*)
(defvar *read-only*)
(defvar core-file-name)

#+immobile-space
(progn
  (defvar *asm-routine-vector*)
  (defvar *immobile-fixedobj*)
  (defvar *immobile-text*)
  (defvar *immobile-space-map* nil))

;;; This ignores alien-linkage-table-core-space-id
;;; which is never present in the core file.
(defconstant max-core-space-id (+ 3 #+immobile-space 2
                                    #+darwin-jit 1))

(defstruct page
  (type nil :type (member nil :code :list :mixed))
  (words-used 0)
  scan-start) ; byte offset from base of the space

;;; a GENESIS-time representation of a memory space (e.g. read-only
;;; space, dynamic space, or static space)
(defstruct (gspace (:constructor %make-gspace)
                   (:copier nil))
  ;; name and identifier for this GSPACE
  (name (missing-arg) :type symbol :read-only t)
  (identifier (missing-arg) :type fixnum :read-only t)
  ;; the address where the data will be loaded
  (byte-address (missing-arg) :type unsigned-byte :read-only t)
  ;; the gspace contents as a BIGVEC
  (data (make-bigvec) :type bigvec :read-only t)
  (page-table nil) ; for dynamic space
  (cons-region) ; (word-index . limit)
  ;; lists of holes created by the allocator to segregate code from data.
  ;; Doesn't matter for cheneygc; does for gencgc.
  ;; Each free-range is (START . LENGTH) in words.
  (code-free-ranges (list nil))
  (non-code-free-ranges (list nil))
  ;; for metaspace
  #+metaspace
  current-slab
  ;; Address of every object created in this space.
  (objects (or #+sb-devel (make-array 700000 :fill-pointer 0 :adjustable t)))
  ;; the index of the next unwritten word (i.e. chunk of
  ;; SB-VM:N-WORD-BYTES bytes) in DATA, or equivalently the number of
  ;; words actually written in DATA. In order to convert to an actual
  ;; index into DATA, thus must be multiplied by SB-VM:N-WORD-BYTES.
  (free-word-index 0))

(defun gspace-upper-bound (gspace)
  (+ (gspace-byte-address gspace)
     (ash (gspace-free-word-index gspace) sb-vm:word-shift)))

(cl:defmethod print-object ((gspace gspace) stream)
  (print-unreadable-object (gspace stream :type t)
    (format stream "@#x~X ~S" (gspace-byte-address gspace) (gspace-name gspace))))

(defun make-gspace (name identifier byte-address &rest rest)
  ;; Genesis should be agnostic of space alignment except in so far as it must
  ;; be a multiple of the backend page size. We used to care more, in that
  ;; descriptor-bits were composed of a high half and low half for the
  ;; questionable motive of caring about fixnum-ness of the halves,
  ;; despite the wonderful abstraction INTEGER that transparently becomes
  ;; a BIGNUM if the host's fixnum is limited in size.
  ;; So it's not clear whether this test belongs here, because if we do need it,
  ;; then it best belongs where we assign space addresses in the first place.
  (let ((target-space-alignment sb-c:+backend-page-bytes+))
    (unless (zerop (rem byte-address target-space-alignment))
      (error "The byte address #X~X is not aligned on a #X~X-byte boundary."
             byte-address target-space-alignment)))
  (apply #'%make-gspace :name name
                :identifier identifier
                ;; Track page usage
                :page-table (if (= identifier dynamic-core-space-id)
                                (make-array 100 :adjustable t :initial-element nil))
                :byte-address byte-address
                :free-word-index (cond #+immobile-space
                                       ((= identifier immobile-fixedobj-core-space-id)
                                        (/ sb-vm:immobile-card-bytes sb-vm:n-word-bytes))
                                       (t
                                        0))
                rest))

(defstruct (model-sap (:constructor make-model-sap (address gspace)))
  (address 0 :type sb-vm:word)
  (gspace nil :type gspace))
(defun sap-int (x) (model-sap-address x))
(defun sap+ (sap x)
  (make-model-sap (+ (model-sap-address sap) x)
                  (model-sap-gspace sap)))
(macrolet ((access (name)
             `(,name (gspace-data (model-sap-gspace sap))
                     (- (+ (model-sap-address sap) offset)
                        (gspace-byte-address (model-sap-gspace sap))))))
  (defun sap-ref-8 (sap offset) (access bvref-8))
  (defun sap-ref-16 (sap offset) (access bvref-16))
  (defun sap-ref-32 (sap offset) (access bvref-32))
  (defun sap-ref-64 (sap offset) (access bvref-64))
  (defun signed-sap-ref-32 (sap offset)
    (sb-disassem:sign-extend (access bvref-32) 32))
  (defun signed-sap-ref-64 (sap offset)
    (sb-disassem:sign-extend (access bvref-64) 64))
  (defun (setf sap-ref-16) (newval sap offset)
    (setf (access bvref-16) newval))
  (defun (setf sap-ref-32) (newval sap offset)
    (setf (access bvref-32) newval))
  (defun (setf signed-sap-ref-32) (newval sap offset)
    (setf (access bvref-32) (ldb (byte 32 0) (the (signed-byte 32) newval))))
  (defun (setf sap-ref-64) (newval sap offset)
    (setf (access bvref-64) newval)))

;;;; representation of descriptors

(declaim (inline is-fixnum-lowtag))
(defun is-fixnum-lowtag (lowtag)
  (zerop (logand lowtag sb-vm:fixnum-tag-mask)))

(defun is-other-immediate-lowtag (lowtag)
  ;; The other-immediate lowtags are similar to the fixnum lowtags, in
  ;; that they have an "effective length" that is shorter than is used
  ;; for the pointer lowtags.  Unlike the fixnum lowtags, however, the
  ;; other-immediate lowtags are always effectively two bits wide.
  (= (logand lowtag 3) sb-vm:other-immediate-0-lowtag))

(defstruct (descriptor
            (:constructor make-descriptor (bits &optional gspace byte-offset))
            (:copier nil))
  ;; the GSPACE that this descriptor is allocated in, or NIL if not set yet.
  (gspace nil :type (or gspace null))
  ;; the offset in bytes (discounting the lowtag) from the start of GSPACE,
  ;; or NIL if not set yet
  (byte-offset nil :type (or sb-vm:word null))
  (bits 0 :read-only t :type (unsigned-byte #.sb-vm:n-machine-word-bits)))

(declaim (inline descriptor=))
(defun descriptor= (a b) (eql (descriptor-bits a) (descriptor-bits b)))

(defun make-random-descriptor (bits)
  (make-descriptor (logand bits sb-ext:most-positive-word)))

(declaim (inline descriptor-lowtag descriptor-widetag))
(defun descriptor-lowtag (des)
  "the lowtag bits for DES"
  (logand (descriptor-bits des) sb-vm:lowtag-mask))
(defun descriptor-widetag (des)
  (logand (read-bits-wordindexed des 0) sb-vm:widetag-mask))

(defun descriptor-base-address (des)
  (logandc2 (descriptor-bits des) sb-vm:lowtag-mask))
(defmethod print-object ((des descriptor) stream)
  (print-unreadable-object (des stream :type t)
    (let ((lowtag (descriptor-lowtag des))
          (bits (descriptor-bits des)))
      (multiple-value-call 'format stream
        (cond ((is-fixnum-lowtag lowtag)
               (values "for fixnum: ~W" (descriptor-fixnum des)))
              ((is-other-immediate-lowtag lowtag)
               (values "for other immediate: #X~X, type #b~8,'0B"
                       (ash bits (- sb-vm:n-widetag-bits))
                       (logand bits sb-vm:widetag-mask)))
              ((descriptor-gspace des)
               (values "for pointer: #X~X, lowtag #b~v,'0B, ~A"
                       (descriptor-base-address des)
                       sb-vm:n-lowtag-bits lowtag
                       (gspace-name (descriptor-gspace des))))
              (t
               (values "bits: #X~X" bits)))))))

;;; Emulate the slab allocator.
;;; If the current slab (at the head of METASPACE-SLABS)
;;; has room, then use it. Otherwise allocate a new slab
;;; at a lower address.
#+metaspace
(defun allocate-metaspace-layout (gspace nbytes)
  (aver (= nbytes (* 8 sb-vm:n-word-bytes)))
  (let ((slab (gspace-current-slab gspace)))
    (flet ((init (slab)
             (let ((bytes-avail (- sb-vm:metaspace-slab-size
                                   (* sb-vm::slab-overhead-words sb-vm:n-word-bytes))))
               (sb-vm::init-slab-header
                slab
                1 ; sizeclass
                nbytes
                ;; FIXME: Technically this should use the chunk size of the sizeclass,
                ;; not the object size. But they happen to be the same in sizeclass 1.
                (floor bytes-avail nbytes)))
             (setf (gspace-current-slab gspace) slab)))
      (unless slab
        (let ((space-size (- sb-vm:read-only-space-end sb-vm:read-only-space-start))
              (slab-base (- sb-vm:read-only-space-end sb-vm:metaspace-slab-size)))
          (expand-bigvec (gspace-data gspace) space-size)
          (setf slab (init (make-model-sap slab-base gspace)))))
      (when (= (sb-vm::slab-usage slab) (sb-vm::slab-capacity slab))
        (format t "~&Slab @ ~x is full~%" (sap-int slab))
        (setf slab (init (sap+ slab (- sb-vm:metaspace-slab-size)))))
      (let* ((count (incf (sb-vm::slab-usage slab)))
             (ptr (+ (sap-int slab)
                     (- sb-vm:metaspace-slab-size
                        (* count (sb-vm::slab-chunk-size slab))))))
        (make-descriptor (logior ptr sb-vm:instance-pointer-lowtag)
                         gspace
                         (- ptr (gspace-byte-address gspace)))))))

;;; Return a descriptor for a block of LENGTH bytes out of GSPACE. The
;;; free word index is boosted as necessary, and if additional memory
;;; is needed, we grow the GSPACE. The descriptor returned is a
;;; pointer of type LOWTAG.
(defun allocate-cold-descriptor (gspace length lowtag &optional (page-type :mixed))
  (let* ((relative-ptr (ash (gspace-claim-n-bytes gspace length page-type)
                            sb-vm:word-shift))
         (ptr (+ (gspace-byte-address gspace) relative-ptr))
         (des (if (and (eq gspace *read-only*) (eq lowtag sb-vm:instance-pointer-lowtag))
                  #+metaspace
                  (allocate-metaspace-layout gspace length)
                  #-metaspace
                  (error "Shouldn't happen.")
                  (make-descriptor (logior ptr lowtag) gspace relative-ptr))))
    (awhen (gspace-objects gspace) (vector-push-extend des it))
    des))

(defun gspace-claim-n-words (gspace n-words)
  (let* ((old-free-word-index (gspace-free-word-index gspace))
         (new-free-word-index (+ old-free-word-index n-words)))
    ;; Grow GSPACE as necessary
    (expand-bigvec (gspace-data gspace) (* new-free-word-index sb-vm:n-word-bytes))
    ;; Now that GSPACE is big enough, we can meaningfully grab a chunk of it.
    (setf (gspace-free-word-index gspace) new-free-word-index)
    old-free-word-index))

(defconstant min-usable-hole-size 10) ; semi-arbitrary constant to speed up the allocator
;; Place conses and code on their respective page type.
#+gencgc
(defun dynamic-space-claim-n-words (gspace n-words page-type
                                    &aux (words-per-page
                                          (/ sb-vm:gencgc-page-bytes sb-vm:n-word-bytes)))
  (labels ((alignedp (word-index) ; T if WORD-INDEX aligns to a GC page boundary
             (not (logtest (* word-index sb-vm:n-word-bytes)
                           (1- sb-vm:gencgc-page-bytes))))
           (page-index (word-index)
             (values (floor word-index words-per-page)))
           (pte (index) ; create on demand
             (or (aref (gspace-page-table gspace) index)
                 (setf (aref (gspace-page-table gspace) index) (make-page))))
           (assign-page-type (page-type start-word-index count)
             ;; CMUCL incorrectly warns that the result of ADJUST-ARRAY
             ;; must not be discarded.
             #+host-quirks-cmu (declare (notinline adjust-array))
             (let ((start-page (page-index start-word-index))
                   (end-page (page-index (+ start-word-index (1- count)))))
               (unless (> (length (gspace-page-table gspace)) end-page)
                 (adjust-array (gspace-page-table gspace) (1+ end-page)
                               :initial-element nil))
               (loop for page-index from start-page to end-page
                     for pte = (pte page-index)
                     do (if (null (page-type pte))
                            (setf (page-type pte) page-type)
                            (aver (eq (page-type pte) page-type))))))
           (note-words-used (start-word-index)
             (let* ((start-page (page-index start-word-index))
                    (end-word-index (+ start-word-index n-words))
                    (end-page (page-index (1- end-word-index))))
               ;; pages from start to end (exclusive) must be full
               (loop for index from start-page below end-page
                     do (setf (page-words-used (pte index)) words-per-page))
               ;; Compute the difference between the word-index at the start of
               ;; end-page and the end-word.
               (setf (page-words-used (pte end-page))
                     (- end-word-index (* end-page words-per-page)))
               ;; update the scan start of any page without it set
               (loop for index from start-page to end-page
                     do (let ((pte (pte index)))
                          (unless (page-scan-start pte)
                            (setf (page-scan-start pte) start-word-index)))))
             start-word-index)
           (get-frontier-page-type ()
             (page-type (pte (page-index (1- (gspace-free-word-index gspace))))))
           (realign-frontier ()
             ;; Align the frontier to a page, putting the empty space onto a free list
             (let* ((free-ptr (gspace-free-word-index gspace))
                    (avail (- (align-up free-ptr words-per-page) free-ptr))
                    (other-type (get-frontier-page-type)) ; before extending frontier
                    (word-index (gspace-claim-n-words gspace avail)))
               ;; the space we got should be exactly what we thought it should be
               (aver (= word-index free-ptr))
               (aver (alignedp (gspace-free-word-index gspace)))
               (aver (= (gspace-free-word-index gspace) (+ free-ptr avail)))
               (when (>= avail min-usable-hole-size)
                 ;; allocator is first-fit; space goes to the tail of the other freelist.
                 (nconc (ecase other-type
                          (:code  (gspace-code-free-ranges gspace))
                          (:mixed (gspace-non-code-free-ranges gspace)))
                        (list (cons word-index avail)))))))
    (when (eq page-type :list) ; Claim whole pages at a time
      (let* ((region
              (or (gspace-cons-region gspace)
                  (progn
                    (unless (alignedp (gspace-free-word-index gspace))
                      (realign-frontier))
                    (let ((word-index (gspace-claim-n-words gspace words-per-page)))
                      (assign-page-type page-type word-index sb-vm:cons-size)
                      (let ((pte (pte (page-index word-index))))
                        (setf (page-scan-start pte) word-index))
                      (setf (gspace-cons-region gspace)
                            (cons word-index
                                  (+ word-index (* (1- sb-vm::max-conses-per-page)
                                                   sb-vm:cons-size))))))))
             (result (car region)))
        (incf (page-words-used (pte (page-index result))) sb-vm:cons-size)
        (when (= (incf (car region) sb-vm:cons-size) (cdr region))
          (setf (gspace-cons-region gspace) nil))
        (return-from dynamic-space-claim-n-words result)))
    (let* ((holder (ecase page-type
                     (:code (gspace-code-free-ranges gspace))
                     (:mixed (gspace-non-code-free-ranges gspace))))
           (found (find-if (lambda (x) (>= (cdr x) n-words))
                           (cdr holder)))) ; dummy cons cell simplifies writeback
      (when found ; always try to backfill holes first if possible
        (let ((word-index (car found)))
          (if (< (decf (cdr found) n-words) min-usable-hole-size) ; discard this hole now?
              (rplacd holder (delete found (cdr holder) :count 1)) ; yup
              (incf (car found) n-words))
          (return-from dynamic-space-claim-n-words (note-words-used word-index))))
      ;; Avoid switching between :CODE and :MIXED on a page
      (unless (or (alignedp (gspace-free-word-index gspace))
                  (eq (get-frontier-page-type) page-type))
        (realign-frontier))
      (let ((word-index (gspace-claim-n-words gspace n-words)))
        (assign-page-type page-type word-index n-words)
        (note-words-used word-index)))))

(defun gspace-claim-n-bytes (gspace specified-n-bytes &optional (page-type :mixed))
  (declare (ignorable page-type))
  (let* ((n-bytes (round-up specified-n-bytes (ash 1 sb-vm:n-lowtag-bits)))
         (n-words (ash n-bytes (- sb-vm:word-shift))))
    (aver (evenp n-words))
    (cond #+immobile-space
          ((eq gspace *immobile-fixedobj*)
           ;; There can be at most 1 page in progress for each distinct N-WORDS.
           ;; Try to find the one which matches.
           (let* ((found (cdr (assoc n-words *immobile-space-map*)))
                  (words-per-page (/ sb-vm:immobile-card-bytes sb-vm:n-word-bytes)))
             (unless found ; grab one whole GC page from immobile space
               (let ((free-word-index (gspace-claim-n-words gspace words-per-page)))
                 (setf found (cons 0 free-word-index))
                 (push (cons n-words found) *immobile-space-map*)))
             (destructuring-bind (page-word-index . page-base-index) found
               (let ((next-word (+ page-word-index n-words)))
                 (if (> next-word (- words-per-page n-words))
                     ;; no more objects will fit on this page
                     (setf *immobile-space-map*
                           (delete n-words *immobile-space-map* :key 'car))
                     (setf (car found) next-word)))
               (+ page-word-index page-base-index))))
          #+gencgc
          ((eq gspace *dynamic*)
           (dynamic-space-claim-n-words gspace n-words page-type))
          (t
           (gspace-claim-n-words gspace n-words)))))

(defun descriptor-fixnum (des)
  (unless (is-fixnum-lowtag (descriptor-lowtag des))
    (error "descriptor-fixnum called on non-fixnum ~S" des))
  (let* ((descriptor-bits (descriptor-bits des))
         (bits (ash descriptor-bits (- sb-vm:n-fixnum-tag-bits))))
    (if (logbitp (1- sb-vm:n-word-bits) descriptor-bits)
        (logior bits (ash -1 (1+ sb-vm:n-positive-fixnum-bits)))
        bits)))

(defun descriptor-integer (des)
  (cond ((is-fixnum-lowtag (descriptor-lowtag des))
         (descriptor-fixnum des))
        ((= (descriptor-widetag des) sb-vm:bignum-widetag)
         (bignum-from-core des))))

;;; common idioms
(defun descriptor-mem (des)
  (gspace-data (descriptor-intuit-gspace des)))

;;; If DESCRIPTOR-GSPACE is already set, just return that. Otherwise,
;;; figure out a GSPACE which corresponds to DES, set it into
;;; (DESCRIPTOR-GSPACE DES), set a consistent value into
;;; (DESCRIPTOR-BYTE-OFFSET DES), and return the GSPACE.
(declaim (ftype (function (descriptor) gspace) descriptor-intuit-gspace))
(defun descriptor-intuit-gspace (des)
  (or (descriptor-gspace des)

      ;; gspace wasn't set, now we have to search for it.
      (let* ((lowtag (descriptor-lowtag des))
             (abs-addr (- (descriptor-bits des) lowtag)))

        ;; Non-pointer objects don't have a gspace.
        (unless (or (eql lowtag sb-vm:fun-pointer-lowtag)
                    (eql lowtag sb-vm:instance-pointer-lowtag)
                    (eql lowtag sb-vm:list-pointer-lowtag)
                    (eql lowtag sb-vm:other-pointer-lowtag))
          (error "don't even know how to look for a GSPACE for ~S" des))

        (dolist (gspace (list *dynamic* *static* *read-only*
                              #+immobile-space *immobile-fixedobj*
                              #+immobile-space *immobile-text*)
                 (error "couldn't find a GSPACE for ~S" des))
          ;; Bounds-check the descriptor against the allocated area
          ;; within each gspace.
          (when (or (<= (gspace-byte-address gspace) abs-addr (gspace-upper-bound gspace))
                    (and (eq gspace *read-only*) ; KLUDGE
                         (<= sb-vm:read-only-space-start abs-addr
                             sb-vm:read-only-space-end)))
            ;; Update the descriptor with the correct gspace and the
            ;; offset within the gspace and return the gspace.
            (setf (descriptor-byte-offset des)
                  (- abs-addr (gspace-byte-address gspace)))
            (return (setf (descriptor-gspace des) gspace)))))))

(defun descriptor-gspace-name (des)
  (gspace-name (descriptor-intuit-gspace des)))

(defun %fixnum-descriptor-if-possible (num)
  (and (typep num `(signed-byte ,sb-vm:n-fixnum-bits))
       (make-random-descriptor (ash num sb-vm:n-fixnum-tag-bits))))

(defun make-fixnum-descriptor (num)
  (or (%fixnum-descriptor-if-possible num)
      (error "~W is too big for a fixnum." num)))

(defun make-other-immediate-descriptor (data type)
  (make-descriptor (logior (ash data sb-vm:n-widetag-bits) type)))

(defun make-character-descriptor (data)
  (make-other-immediate-descriptor data sb-vm:character-widetag))


;;;; miscellaneous variables and other noise

;;; a handle on the trap object
(defvar *unbound-marker*
  (make-other-immediate-descriptor 0 sb-vm:unbound-marker-widetag))

;;; a handle on the NIL object
(defvar *nil-descriptor*)
(defvar *c-callable-fdefn-vector*)

;;; the head of a list of TOPLEVEL-THINGs describing stuff to be done
;;; when the target Lisp starts up
;;;
;;; Each TOPLEVEL-THING can be a function to be executed or a fixup or
;;; loadtime value, represented by (CONS KEYWORD ..).
(declaim (special *!cold-toplevels* *cold-methods*))


;;;; miscellaneous stuff to read and write the core memory
(declaim (ftype (function (descriptor sb-vm:word) descriptor) read-wordindexed))
(macrolet ((read-bits ()
             `(bvref-word (descriptor-mem address)
                          (+ (descriptor-byte-offset address)
                             (ash index sb-vm:word-shift)))))
  (defun read-bits-wordindexed (address index)
    (read-bits))
  (defun read-wordindexed (address index)
  "Return the value which is displaced by INDEX words from ADDRESS."
    (make-random-descriptor (read-bits))))

(defstruct (ltv-patch (:copier nil) (:constructor make-ltv-patch (index)))
  (index 0 :read-only t))
(declaim (ftype (function (descriptor sb-vm:word (or symbol package descriptor ltv-patch)))
                write-wordindexed))
(macrolet ((write-bits (bits)
             `(setf (bvref-word (descriptor-mem address)
                                (+ (descriptor-byte-offset address)
                                   (ash index sb-vm:word-shift)))
                    ,bits)))
  (defun write-wordindexed (address index value)
    "Write VALUE displaced INDEX words from ADDRESS."
    (write-bits
     (cond ((ltv-patch-p value)
            (if (or (= (descriptor-lowtag address) sb-vm:list-pointer-lowtag)
                    (= (descriptor-widetag address) sb-vm:code-header-widetag))
                (push (cold-list (cold-intern :load-time-value-fixup)
                                 address
                                 (number-to-core index)
                                 (number-to-core (ltv-patch-index value)))
                      *!cold-toplevels*)
                (bug "Can't patch load-time-value into ~S" address))
            sb-vm:unbound-marker-widetag)
          (t
           (descriptor-bits
            ;; If we're passed a symbol as a value then it needs to be interned.
            (cond ((symbolp value) (cold-intern value))
                  ((packagep value) (cdr (cold-find-package-info (sb-xc:package-name value))))
                  (t value)))))))

  (defun write-wordindexed/raw (address index bits)
    (declare (type descriptor address) (type sb-vm:word index)
             (type (or sb-vm:word sb-vm:signed-word) bits))
    (write-bits (logand bits sb-ext:most-positive-word))))

;;;; allocating images of primitive objects in the cold core

(defun write-header-word (des header-word)
  ;; In immobile space, all objects start life as pseudo-static as if by 'save'.
  (let* ((gen (or #+immobile-space
                  (let ((gspace (descriptor-intuit-gspace des)))
                    (aver gspace)
                    (when (or (eq gspace *immobile-fixedobj*)
                              (eq gspace *immobile-text*))
                      sb-vm:+pseudo-static-generation+))
                  0))
         (widetag (logand header-word sb-vm:widetag-mask))
         ;; Refer to depiction of "Immobile object header word" in gc-private.h
         (gen-shift (if (= widetag sb-vm:fdefn-widetag) 8 24)))
    (write-wordindexed/raw des 0 (logior (ash gen gen-shift) header-word))))

(defun write-code-header-words (descriptor boxed unboxed n-fdefns)
  (declare (ignorable n-fdefns))
  (let ((total-words (align-up (+ boxed (ceiling unboxed sb-vm:n-word-bytes)) 2)))
    (write-header-word descriptor
                       (logior (ash total-words sb-vm:code-header-size-shift)
                               sb-vm:code-header-widetag)))
  (write-wordindexed/raw
   descriptor 1
   (logior #+64-bit (ash n-fdefns 32) (* boxed sb-vm:n-word-bytes))))

(defun write-header-data+tag (des header-data widetag)
  (write-header-word des (logior (ash header-data sb-vm:n-widetag-bits)
                                 widetag)))

(defun get-header-data (object)
  (ash (read-bits-wordindexed object 0) (- sb-vm:n-widetag-bits)))

;;; There are three kinds of blocks of memory in the type system:
;;; * Boxed objects (cons cells, structures, etc): These objects have no
;;;   header as all slots, or almost all slots, are descriptors.
;;;   This also includes code objects, which are mostly non-descriptors.
;;; * Unboxed objects (bignums): There is a single header word that contains
;;;   the length.
;;; * Vector objects: There is a header word with the type, then a word for
;;;   the length, then the data.
(defun allocate-object (gspace length lowtag)
  "Allocate LENGTH words in GSPACE and return a new descriptor of type LOWTAG
  pointing to them."
  (allocate-cold-descriptor gspace (ash length sb-vm:word-shift) lowtag))
(defun allocate-otherptr (gspace length widetag)
  "Allocate LENGTH words in GSPACE and return an ``other-pointer'' descriptor.
   LENGTH must count the header word itself as 1 word.  The header word is
   initialized with the payload size as (1- LENGTH), and WIDETAG."
  (let ((des (allocate-cold-descriptor gspace (ash length sb-vm:word-shift)
                                       sb-vm:other-pointer-lowtag)))
    ;; FDEFNs don't store a length, freeing up a header byte for other use
    (write-header-data+tag des (if (= widetag sb-vm:fdefn-widetag) 0 (1- length))
                           widetag)
    des))
(defvar *simple-vector-0-descriptor*)
(defun allocate-vector (widetag length words &optional (gspace *dynamic*))
  ;; Allocate a vector with WORDS payload words (excluding the header+length).
  ;; WORDS may be an odd number.
  ;; Store WIDETAG in the header and LENGTH in the length slot.
  (when (and (= widetag sb-vm:simple-vector-widetag)
             (= length 0)
             *simple-vector-0-descriptor*)
    (return-from allocate-vector *simple-vector-0-descriptor*))
  (emplace-vector (allocate-cold-descriptor
                   gspace
                   (sb-vm:pad-data-block (+ words sb-vm:vector-data-offset))
                   sb-vm:other-pointer-lowtag)
                  widetag length))
(defun emplace-vector (des widetag length)
  #+ubsan
  (write-header-word des (logior (ash length (+ 32 sb-vm:n-fixnum-tag-bits))
                                 widetag))
  #-ubsan
  (progn (write-header-data+tag des 0 widetag)
         (write-wordindexed des sb-vm:vector-length-slot (make-fixnum-descriptor length)))
  des)

;;; The COLD-LAYOUT is a reflection of or proxy for the words stored
;;; in the core for a cold layout, so that we don't have to extract
;;; them out of the core to compare cold layouts for validity.
(defstruct (cold-layout (:constructor %make-cold-layout))
  id name depthoid length bitmap flags inherits descriptor)

;;; a map from name as a host symbol to the descriptor of its target layout
(defvar *cold-layouts*)
(defun cold-layout-descriptor-bits (name)
  (descriptor-bits (cold-layout-descriptor (gethash name *cold-layouts*))))

#+compact-instance-header
(progn
  ;; This is called to backpatch layout-of-layout into the primordial layouts.
  (defun set-instance-layout (thing layout)
    ;; High half of the header points to the layout
    (write-wordindexed/raw thing 0 (logior (ash (descriptor-bits layout) 32)
                                           (read-bits-wordindexed thing 0))))
  (defun get-instance-layout (thing)
    (make-random-descriptor (ash (read-bits-wordindexed thing 0) -32))))
#-compact-instance-header
(progn
  (defun set-instance-layout (thing layout)
    ;; Word following the header is the layout
    (write-wordindexed thing sb-vm:instance-slots-offset layout))
  (defun get-instance-layout (thing)
    (read-wordindexed thing sb-vm:instance-slots-offset)))

;; Make a structure and set the header word and layout.
;; NWORDS is the payload length (= DD-LENGTH = LAYOUT-LENGTH)
(defun allocate-struct (nwords layout &optional (gspace *dynamic*))
  ;; Add +1 for the header word when allocating.
  (let ((object (allocate-object gspace (1+ nwords) sb-vm:instance-pointer-lowtag)))
    ;; Length as stored in the header is the exact number of useful words
    ;; that follow, as is customary. A padding word, if any is not "useful"
    (write-header-word object (logior (ash nwords sb-vm:instance-length-shift)
                                      sb-vm:instance-widetag))
    (set-instance-layout object layout)
    object))
(defun type-dd-slots-or-lose (type)
  (or (car (get type 'dd-proxy)) (error "NO DD-SLOTS: ~S" type)))
;;; Return the value to supply as the first argument to ALLOCATE-STRUCT
(defun struct-size (thing)
  ;; ASSUMPTION: all slots consume 1 storage word
  (+ sb-vm:instance-data-start (length (type-dd-slots-or-lose thing))))
(defun allocate-struct-of-type (type)
  (allocate-struct (struct-size type)
                   (cold-layout-descriptor (gethash type *cold-layouts*))))

;;;; copying simple objects into the cold core

(defun cold-simple-vector-p (obj)
  (and (= (descriptor-lowtag obj) sb-vm:other-pointer-lowtag)
       (= (descriptor-widetag obj) sb-vm:simple-vector-widetag)))

(declaim (inline cold-vector-len))
(defun cold-vector-len (vector)
  #+ubsan (ash (read-bits-wordindexed vector 0) (- -32 sb-vm:n-fixnum-tag-bits))
  #-ubsan (descriptor-fixnum (read-wordindexed vector sb-vm:vector-length-slot)))

(macrolet ((vector-data (vector-descriptor)
             `(+ (descriptor-byte-offset ,vector-descriptor)
                 (* sb-vm:vector-data-offset sb-vm:n-word-bytes))))
(defun base-string-to-core (string)
  "Copy STRING (which must only contain STANDARD-CHARs) into the cold
core and return a descriptor to it."
  ;; (Remember that the system convention for storage of strings leaves an
  ;; extra null byte at the end to aid in call-out to C.)
  (let* ((length (length string))
         (des (allocate-vector sb-vm:simple-base-string-widetag
                               ;; add SAETP-N-PAD-ELEMENT
                               length (ceiling (1+ length) sb-vm:n-word-bytes)
                               *dynamic*))
         (mem (descriptor-mem des))
         (byte-base (vector-data des)))
    (dotimes (i length des) ; was prezeroed, so automatically null-terminated
      (setf (bvref mem (+ byte-base i)) (char-code (aref string i))))))

(defun base-string-from-core (descriptor)
  (let* ((mem (descriptor-mem descriptor))
         (byte-base (vector-data descriptor))
         (len (cold-vector-len descriptor))
         (str (make-string len)))
    (dotimes (i len str)
      (setf (aref str i) (code-char (bvref mem (+ byte-base i)))))))

(defun bit-vector-to-core (bit-vector &optional (gspace *dynamic*))
  (let* ((length (length bit-vector))
         (nwords (ceiling length sb-vm:n-word-bits))
         (des (allocate-vector sb-vm:simple-bit-vector-widetag length nwords gspace))
         (mem (descriptor-mem des))
         (base (vector-data des)))
    (let ((byte 0))
      (dotimes (i length)
        (let ((byte-bit (rem i 8)))
          (setf (ldb (byte 1 byte-bit) byte) (bit bit-vector i))
          (when (= byte-bit 7)
            (setf (bvref mem (+ base (floor i 8))) byte))))
      (when (/= 0 (rem length 8))
        (setf (bvref mem (+ base (floor length 8))) byte))
      des))))

;;; I would think that all strings we dump are readonly. Maybe not?
(defun string-literal-to-core (s) (set-readonly (base-string-to-core s)))

;;; Write the bits of INT to core as if a bignum, i.e. words are ordered from
;;; least to most significant regardless of machine endianness.
(defun integer-bits-to-core (int descriptor start nwords)
  (declare (fixnum nwords))
  (do ((index 0 (1+ index))
       (remainder int (ash remainder (- sb-vm:n-word-bits))))
      ((>= index nwords)
       (unless (zerop (integer-length remainder))
         (error "Nonzero remainder after writing ~D using ~D words" int nwords)))
    (write-wordindexed/raw descriptor
                           (+ start index)
                           (logand remainder sb-ext:most-positive-word))))

(defun bignum-to-core (n &optional (space *dynamic*))
  "Copy a bignum to the cold core."
  (let* ((words (ceiling (1+ (integer-length n)) sb-vm:n-word-bits))
         (handle
          #-bignum-assertions (allocate-otherptr space (1+ words) sb-vm:bignum-widetag)
          #+bignum-assertions
          (let* ((aligned-words (1+ (logior words 1))) ; round to odd, slap on a header
                 (physical-words (* aligned-words 2))
                 (handle (allocate-otherptr space physical-words sb-vm:bignum-widetag)))
            ;; rewrite the header to indicate the logical size
            (write-wordindexed/raw handle 0 (logior (ash words 8) sb-vm:bignum-widetag))
            handle)))
    (integer-bits-to-core n handle sb-vm:bignum-digits-offset words)
    (aver (= (bignum-from-core handle) n))
    handle))

(defun bignum-from-core (descriptor)
  (let ((n-words (logand (get-header-data descriptor) #x7fffff))
        (val 0))
    (dotimes (i n-words val)
      (let ((bits (read-bits-wordindexed descriptor
                                         (+ i sb-vm:bignum-digits-offset))))
        ;; sign-extend the highest word
        (when (= i (1- n-words))
          (setq bits (sb-vm::sign-extend bits sb-vm:n-word-bits)))
        (setq val (logior (ash bits (* i sb-vm:n-word-bits)) val))))))

(defun number-pair-to-core (first second type)
  "Makes a number pair of TYPE (ratio or complex) and fills it in."
  (let ((des (allocate-otherptr *dynamic* 3 type)))
    (write-wordindexed des 1 first)
    (write-wordindexed des 2 second)
    des))

(defun write-double-float-bits (address index x)
  #-64-bit
     (let ((high-bits (double-float-high-bits x))
           (low-bits (double-float-low-bits x)))
       #+little-endian
       (progn (write-wordindexed/raw address index low-bits)
              (write-wordindexed/raw address (1+ index) high-bits))
       #+big-endian
       (progn (write-wordindexed/raw address index high-bits)
              (write-wordindexed/raw address (1+ index) low-bits)))
  #+64-bit
     (write-wordindexed/raw address index (double-float-bits x))
  address)

(defun float-to-core (x)
  (ecase (sb-impl::flonum-format x)
    (single-float
     (let ((bits (single-float-bits x)))
       #+64-bit ; 64-bit platforms have immediate single-floats
       (make-random-descriptor (logior (ash bits 32) sb-vm:single-float-widetag))
       #-64-bit
       (let ((des (allocate-otherptr *dynamic* sb-vm:single-float-size
                                     sb-vm:single-float-widetag)))
         (write-wordindexed/raw des sb-vm:single-float-value-slot bits)
         des)))
    (double-float
     (let ((des (allocate-otherptr *dynamic* sb-vm:double-float-size
                                   sb-vm:double-float-widetag)))
       (write-double-float-bits des sb-vm:double-float-value-slot x)))))

(defun unsigned-bits-to-single-float (bits)
  (sb-impl::make-flonum (sb-vm::sign-extend bits 32) 'single-float))
(defun double-float-from-core (des)
  (let ((bits
         #+64-bit (read-bits-wordindexed des 1)
         #-64-bit (let* ((word0 (read-bits-wordindexed
                                 des sb-vm:double-float-value-slot))
                         (word1 (read-bits-wordindexed
                                 des (1+ sb-vm:double-float-value-slot))))
                    #+little-endian (logior (ash word1 32) word0)
                    #+big-endian    (logior (ash word0 32) word1))))
    (sb-impl::make-flonum (sb-vm::sign-extend bits 64) 'double-float)))

(defun complexnum-to-core (num &aux (r (realpart num)) (i (imagpart num)))
  (if (rationalp r)
      (number-pair-to-core (number-to-core r) (number-to-core i) sb-vm:complex-widetag)
      (ecase (sb-impl::flonum-format r)
       (single-float
        (let* ((des (allocate-otherptr *dynamic* sb-vm:complex-single-float-size
                                       sb-vm:complex-single-float-widetag))
               (where (+ (descriptor-byte-offset des)
                         (ash #+64-bit sb-vm:complex-single-float-data-slot
                              #-64-bit sb-vm:complex-single-float-real-slot
                              sb-vm:word-shift))))
          (setf (bvref-s32 (descriptor-mem des) where) (single-float-bits r)
                (bvref-s32 (descriptor-mem des) (+ where 4)) (single-float-bits i))
          des))
       (double-float
        (let ((des (allocate-otherptr *dynamic* sb-vm:complex-double-float-size
                                      sb-vm:complex-double-float-widetag)))
          (write-double-float-bits des sb-vm:complex-double-float-real-slot r)
          (write-double-float-bits des sb-vm:complex-double-float-imag-slot i)
          des)))))

;;; Copy the given number to the core.
(defun number-to-core (number)
  (typecase number
    (integer (or (%fixnum-descriptor-if-possible number)
                 (bignum-to-core number)))
    (ratio (number-pair-to-core (number-to-core (numerator number))
                                (number-to-core (denominator number))
                                sb-vm:ratio-widetag))
    (float (float-to-core number))
    (complex (complexnum-to-core number))
    (t (error "~S isn't a cold-loadable number at all!" number))))

;;; Allocate a cons cell in GSPACE and fill it in with CAR and CDR.
(defun cold-cons (car cdr &optional (gspace *dynamic*))
  (let ((cons (allocate-cold-descriptor gspace (ash 2 sb-vm:word-shift)
                                        sb-vm:list-pointer-lowtag :list)))
    (let* ((objs (gspace-objects gspace))
           (n (1- (length objs))))
      (when objs
        (setf (aref objs n) (list (aref objs n)))))
    (write-wordindexed cons sb-vm:cons-car-slot car)
    (write-wordindexed cons sb-vm:cons-cdr-slot cdr)
    cons))
(defun list-to-core (list)
  (let ((head *nil-descriptor*)
        (tail nil))
    ;; A recursive algorithm would have the first cons at the highest
    ;; address. This way looks nicer when viewed in ldb.
    (loop
     (unless list (return head))
     (let ((cons (cold-cons (pop list) *nil-descriptor*)))
       (if tail (cold-rplacd tail cons) (setq head cons))
       (setq tail cons)))))
(defun cold-list (&rest args) (list-to-core args))
(defun cold-list-length (list) ; but no circularity detection
  ;; a recursive implementation uses too much stack for some Lisps
  (let ((n 0))
    (loop (if (cold-null list) (return n))
          (incf n)
          (setq list (cold-cdr list)))))
(defun cold-push (item symbol)
  (cold-set symbol (cold-cons item (cold-symbol-value symbol))))

;;; Make a simple-vector on the target that holds the specified
;;; OBJECTS, and return its descriptor.
;;; This is really "vectorify-list-into-core" but that's too wordy,
;;; so historically it was "vector-in-core" which is a fine name.
(defun vector-in-core (objects &optional (gspace *dynamic*))
  (let* ((size (length objects))
         (result (allocate-vector sb-vm:simple-vector-widetag size size gspace)))
    (dotimes (index size result)
      (write-wordindexed result (+ index sb-vm:vector-data-offset)
                         (pop objects)))))

(defun word-vector (objects &optional (gspace *dynamic*))
  (let* ((size (length objects))
         (result (allocate-vector #+64-bit sb-vm:simple-array-unsigned-byte-64-widetag
                                  #-64-bit sb-vm:simple-array-unsigned-byte-32-widetag
                                  size size gspace)))
    (dotimes (index size result)
      (write-wordindexed/raw result (+ index sb-vm:vector-data-offset) (pop objects)))))

(defun cold-svset (vector index value)
  (let ((i (if (integerp index) index (descriptor-fixnum index))))
    (write-wordindexed vector (+ i sb-vm:vector-data-offset) value))
  value)

(declaim (inline cold-svref))
(defun cold-svref (vector i)
  (declare (type index i))
  (aver (< i (cold-vector-len vector)))
  (read-wordindexed vector (+ i sb-vm:vector-data-offset)))
(defun vector-from-core (descriptor &optional (transform #'identity))
  (let* ((len (cold-vector-len descriptor))
         (vector (make-array len)))
    (dotimes (i len vector)
      (setf (aref vector i) (funcall transform (cold-svref descriptor i))))))

;;;; symbol magic

(defvar *tls-index-to-symbol*)
#+sb-thread
(progn
  ;; Simulate *FREE-TLS-INDEX*. This is a word count, not a displacement.
  (defvar *genesis-tls-counter* sb-vm::primitive-thread-object-length)
  ;; Assign SYMBOL the tls-index INDEX. SYMBOL must be a descriptor.
  ;; This is a backend support routine, but the style within this file
  ;; is to conditionalize by the target features.
  (defun cold-assign-tls-index (symbol index)
    (push (list index (warm-symbol symbol)) *tls-index-to-symbol*)
    #+64-bit
    (write-wordindexed/raw
     symbol 0 (logior (ash index 32) (read-bits-wordindexed symbol 0)))
    #-64-bit
    (write-wordindexed/raw symbol sb-vm:symbol-tls-index-slot index))

  ;; Return SYMBOL's tls-index,
  ;; choosing a new index if it doesn't have one yet.
  (defun ensure-symbol-tls-index (symbol)
    (let* ((cold-sym (cold-intern symbol))
           (tls-index #+64-bit (ldb (byte 32 32) (read-bits-wordindexed cold-sym 0))
                      #-64-bit (read-bits-wordindexed cold-sym sb-vm:symbol-tls-index-slot)))
      (unless (plusp tls-index)
        (let ((next (prog1 *genesis-tls-counter* (incf *genesis-tls-counter*))))
          (setq tls-index (ash next sb-vm:word-shift))
          (cold-assign-tls-index cold-sym tls-index)))
      tls-index)))

(defvar *cold-symbol-gspace* (or #+immobile-space '*immobile-fixedobj* '*dynamic*))
(defun encode-symbol-name (package-id name)
  (logior (ash package-id sb-impl::symbol-name-bits) (descriptor-bits name)))

;;; Allocate (and initialize) a symbol.
;;; Even though all symbols are the same size now, I still envision the possibility
;;; of reducing gensyms to 4 words, though I'm not sure what to do if information
;;; is later attached (function, value, plist)
(defun allocate-symbol (size cold-package name &key (gspace (symbol-value *cold-symbol-gspace*)))
  (declare (simple-string name))
  (let ((symbol (allocate-otherptr gspace size sb-vm:symbol-widetag)))
    (when core-file-name
      (let* ((cold-name (string-literal-to-core name))
             (pkg-id (if cold-package
                         (descriptor-fixnum (read-slot cold-package :id))
                         sb-impl::+package-id-none+))
             (hash (make-fixnum-descriptor
                    (if core-file-name (sb-impl::symbol-name-hash name) 0))))
        (write-wordindexed symbol sb-vm:symbol-value-slot *unbound-marker*)
        (write-wordindexed symbol sb-vm:symbol-hash-slot hash)
        (write-wordindexed symbol sb-vm:symbol-info-slot *nil-descriptor*)
        #+compact-symbol
        (write-wordindexed/raw symbol sb-vm:symbol-name-slot
                               (encode-symbol-name pkg-id cold-name))
        #-compact-symbol
        (progn (write-wordindexed symbol sb-vm:symbol-package-id-slot
                                  (make-fixnum-descriptor pkg-id))
               (write-wordindexed symbol sb-vm:symbol-name-slot cold-name))))
    symbol))

;;; Set the cold symbol value of SYMBOL-OR-SYMBOL-DES, which can be either a
;;; descriptor of a cold symbol or (in an abbreviation for the
;;; most common usage pattern) an ordinary symbol, which will be
;;; automatically cold-interned.
(defun cold-set (symbol-or-symbol-des value)
  (let ((symbol-des (etypecase symbol-or-symbol-des
                      (descriptor symbol-or-symbol-des)
                      (symbol (cold-intern symbol-or-symbol-des)))))
    (write-wordindexed symbol-des sb-vm:symbol-value-slot value)))
(defun cold-symbol-value (symbol)
  (let ((val (read-wordindexed (cold-intern symbol) sb-vm:symbol-value-slot)))
    (if (= (descriptor-bits val) sb-vm:unbound-marker-widetag)
        (error "Symbol value of ~a is unbound." symbol)
        val)))
(defun cold-fdefn-fun (cold-fdefn)
  (read-wordindexed cold-fdefn sb-vm:fdefn-fun-slot))


;;;; layouts and type system pre-initialization

;;; Since we want to be able to dump structure constants and
;;; predicates with reference layouts, we need to create layouts at
;;; cold-load time. We use the name to intern layouts by, and dump a
;;; list of all cold layouts in *!INITIAL-WRAPPERS* so that type system
;;; initialization can find them. The only thing that's tricky [sic --
;;; WHN 19990816] is initializing layout's layout, which must point to
;;; itself.

;;; a map from DESCRIPTOR-BITS of cold layouts (as descriptors)
;;; to the host's COLD-LAYOUT proxy for that layout.
(defvar *cold-layout-by-addr*)

;;; Initial methods require that we sort possible methods by the depthoid.
;;; Most of the objects printed in cold-init are ordered hierarchically in our
;;; type lattice; the major exceptions are ARRAY and VECTOR at depthoid -1.
;;; Of course we need to print VECTORs because a STRING is a vector,
;;; and vector has to precede ARRAY. Kludge it for now.
(defun class-depthoid (class-name) ; DEPTHOID-ish thing, any which way you can
  (case class-name
    (vector 1/2)
    (array  1/4)
    ;; The depthoid of CONDITION has to be faked. The proper value is 1.
    ;; But STRUCTURE-OBJECT is also at depthoid 1, and its predicate
    ;; is %INSTANCEP (which is too weak), so to select the correct method
    ;; we have to make CONDITION more specific.
    ;; In reality it is type disjoint from structure-object.
    (condition 2)
    (t
     (acond ((gethash class-name *cold-layouts*)
             (cold-layout-depthoid it))
            ((info :type :compiler-layout class-name)
             (wrapper-depthoid it))
            (t
             (error "Unknown depthoid for ~S" class-name))))))

(declaim (ftype function read-slot %write-slots write-slots))
(flet ((infer-metadata (x)
         (type-dd-slots-or-lose
          (cold-layout-name (gethash (descriptor-bits (get-instance-layout x))
                                     *cold-layout-by-addr*))))
       (find-slot (slots initarg)
         (let ((dsd (or (find initarg slots
                              :test (lambda (x y) (eq x (keywordicate (dsd-name y)))))
                        (error "No slot for ~S in ~S" initarg slots))))
           (values (+ sb-vm:instance-slots-offset (dsd-index dsd))
                   (dsd-raw-type dsd)))))

  (defun %write-slots (metadata cold-object &rest assignments)
    (aver (evenp (length assignments)))
    (loop for (initarg value) on assignments by #'cddr
       do (multiple-value-bind (index repr) (find-slot metadata initarg)
            (ecase repr
              ((t) (write-wordindexed cold-object index value))
              ((word sb-vm:signed-word)
               (write-wordindexed/raw cold-object index value)))))
    cold-object)

  (defun write-slots (cold-object &rest assignments)
    (apply #'%write-slots (infer-metadata cold-object) cold-object assignments))

  ;; For symmetry, the reader takes an initarg, not a slot name.
  (defun read-slot (cold-object slot-initarg)
    (multiple-value-bind (index repr)
        (find-slot (infer-metadata cold-object) slot-initarg)
      (ecase repr
        ((t) (read-wordindexed cold-object index))
        (word (read-bits-wordindexed cold-object index))
        (sb-vm:signed-word
         (sb-vm::sign-extend (read-bits-wordindexed cold-object index)
                             sb-vm:n-word-bits))))))

(defun read-structure-definitions (pathname)
  (with-open-file (stream pathname)
    (loop
     (let ((ch (peek-char t stream)))
       (when (char= ch #\;)
         (return))
       (let* ((classoid-name (read stream))
              (*package* (find-package (cl:symbol-package classoid-name)))
              (flags+depthoid+inherits (read stream)))
         (setf (get classoid-name 'dd-proxy)
               (list (map 'vector
                          (lambda (x)
                            (destructuring-bind (bits name acc) x
                              (sb-kernel::make-dsd name nil acc bits nil)))
                          (read stream))
                     :flags (car flags+depthoid+inherits)
                     :depthoid (cadr flags+depthoid+inherits)
                     :inherits (cddr flags+depthoid+inherits))))))))

(defvar *vacuous-slot-table*)
(defvar *cold-layout-gspace* (or #+metaspace '*read-only*
                                 #+immobile-space '*immobile-fixedobj*
                                 '*dynamic*))
(declaim (ftype (function (symbol layout-depthoid integer index integer descriptor)
                          descriptor)
                make-cold-layout))

(defun cold-wrapper-id (wrapper-descriptor)
  (let* ((layout-descriptor (->layout wrapper-descriptor))
         (proxy (gethash (descriptor-bits layout-descriptor) *cold-layout-by-addr*)))
    (cold-layout-id proxy)))

(defun make-cold-layout (name depthoid flags length bitmap inherits)
  ;; Layouts created in genesis can't vary in length due to the number of ancestor
  ;; types in the IS-A vector. They may vary in length due to the bitmap word count.
  ;; But we can at least assert that there is one less thing to worry about.
  (aver (<= depthoid sb-kernel::layout-id-vector-fixed-capacity))
  (aver (cold-simple-vector-p inherits))
  (let* ((fixed-words (sb-kernel::type-dd-length sb-vm:layout))
         (bitmap-words (ceiling (1+ (integer-length bitmap)) sb-vm:n-word-bits))
         (result (allocate-struct (+ fixed-words bitmap-words)
                                  (or (awhen (gethash #+metaspace 'sb-vm:layout
                                                      #-metaspace 'wrapper *cold-layouts*)
                                        (cold-layout-descriptor it))
                                      (make-fixnum-descriptor 0))
                                  (symbol-value *cold-layout-gspace*)))
         (wrapper
          #-metaspace result ; WRAPPER and LAYOUT are synonymous in this case
          #+metaspace (allocate-struct (sb-kernel::type-dd-length wrapper)
                                       (or (awhen (gethash 'wrapper *cold-layouts*)
                                             (cold-layout-descriptor it))
                                           (make-fixnum-descriptor 0))))
         (this-id (sb-kernel::choose-layout-id name (logtest flags +condition-layout-flag+)))
         (hash (make-fixnum-descriptor (sb-impl::hash-layout-name name))))

    (let ((proxy (%make-cold-layout :id this-id
                                    :name name
                                    :depthoid depthoid
                                    :length length
                                    :bitmap bitmap
                                    :flags flags
                                    :inherits inherits
                                    :descriptor result)))
      ;; Make two different ways to look up the proxy object -
      ;; by name or by descriptor-bits.
      (setf (gethash (descriptor-bits result) *cold-layout-by-addr*) proxy
            (gethash name *cold-layouts*) proxy))
    (unless core-file-name (return-from make-cold-layout result))

    ;; Can't use the easier WRITE-SLOTS unfortunately because bootstrapping is hard
    (let* ((wrapper-metadata (type-dd-slots-or-lose 'wrapper))
           (layout-metadata #-metaspace wrapper-metadata
                            #+metaspace (type-dd-slots-or-lose 'sb-vm:layout)))

      #+64-bit
      (%write-slots layout-metadata result
                    :flags (sb-kernel::pack-layout-flags depthoid length flags))
      #-64-bit
      (%write-slots layout-metadata result
                    :depthoid (make-fixnum-descriptor depthoid)
                    :length (make-fixnum-descriptor length)
                    :flags flags)

      (%write-slots wrapper-metadata wrapper
                    :clos-hash hash
                    :invalid *nil-descriptor*
                    :inherits inherits
                    :%info *nil-descriptor*)

      (when (member name '(null list symbol pathname))
        ;; Assign an empty slot-table.  Why this is done only for four
        ;; classoids is ... too complicated to explain here in a few words,
        ;; but revision 18c239205d9349abc017b07e7894a710835c5205 broke it.
        ;; Keep this in sync with MAKE-SLOT-TABLE in pcl/slots-boot.
        (%write-slots wrapper-metadata wrapper
                      :slot-table (if (boundp '*vacuous-slot-table*)
                                      *vacuous-slot-table*
                                      (setq *vacuous-slot-table*
                                            (host-constant-to-core '#(1 nil))))))

      ;; If wrappers are used, the wrapper has a copy of the hash,
      ;; and also the two friends point to each other.
      #+metaspace
      (progn (%write-slots layout-metadata result :clos-hash hash :friend wrapper)
             (%write-slots wrapper-metadata wrapper :friend result))

      (let ((byte-offset (+ (descriptor-byte-offset result) (sb-vm::id-bits-offset))))
        (when (logtest flags +structure-layout-flag+)
          (loop for i from 2 below (cold-vector-len inherits)
                do (setf (bvref-s32 (descriptor-mem result) byte-offset)
                         (cold-wrapper-id (cold-svref inherits i)))
                   (incf byte-offset 4)))
        (setf (bvref-s32 (descriptor-mem result) byte-offset) this-id)))

    (integer-bits-to-core bitmap result (1+ fixed-words) bitmap-words)

    result))

(defun predicate-for-specializer (type-name)
  (let ((classoid (find-classoid type-name nil)))
    (typecase classoid
      (structure-classoid
       (dd-predicate-name (sb-kernel::wrapper-%info (classoid-wrapper classoid))))
      (built-in-classoid
       (let ((translation (specifier-type type-name)))
         (aver (not (contains-unknown-type-p translation)))
         (let ((predicate (find translation sb-c::*backend-type-predicates*
                                :test #'type= :key #'car)))
           (cond (predicate (cdr predicate))
                 ((eq type-name 'stream) 'streamp)
                 ((eq type-name 'pathname) 'pathnamep)
                 ((eq type-name 't) 'constantly-t)
                 (t (error "No predicate for builtin: ~S" type-name)))))))))

;;; Map from host object to target object
(defvar *host->cold-ctype*)

;;; NUMTYPE-ASPECTS are stored in a fixed-size vector.
;;; During genesis they are created on demand.
;;; (I'm not sure whether all or only some are created)
(defun numtype-aspects-to-core (val)
  (let* ((index (sb-kernel::numtype-aspects-id val))
         (vector (cold-symbol-value 'sb-kernel::*numeric-aspects-v*))
         (cold-obj (cold-svref vector index)))
    (if (eql (descriptor-bits cold-obj) 0)
        (write-slots (cold-svset vector index
                                 (allocate-struct-of-type (type-of val)))
                     :id (make-fixnum-descriptor (sb-kernel::numtype-aspects-id val))
                     :complexp (sb-kernel::numtype-aspects-complexp val)
                     :class (sb-kernel::numtype-aspects-class val)
                     :precision (sb-kernel::numtype-aspects-precision val))
        cold-obj)))

(defvar *dsd-index-cache* nil)
(defun dsd-index-cached (type-name slot-name)
  (let ((cell (find-if (lambda (x)
                         (and (eq (caar x) type-name) (eq (cdar x) slot-name)))
                       *dsd-index-cache*)))
    (if cell
        (cdr cell)
        (let* ((dd-slots (car (get type-name 'dd-proxy)))
               (dsd (find slot-name dd-slots :key #'dsd-name))
               (index (dsd-index dsd)))
          (push (cons (cons type-name slot-name) index) *dsd-index-cache*)
          index))))

(defun ctype-to-core (obj)
  (declare (type (or ctype xset list) obj))
  (cond
    ((null obj) *nil-descriptor*)
    ((gethash obj *host->cold-ctype*))
    ((listp obj)
     (if (and (proper-list-p obj) (every #'sb-kernel:ctype-p obj))
         ;; Be sure to preserving shared substructure.
         ;; There is no circularity, so inserting into the map after copying works fine
         (setf (gethash obj *host->cold-ctype*) (list-to-core (mapcar #'ctype-to-core obj)))
         (host-constant-to-core obj))) ; numeric bound, array dimension, etc
    (t
     (when (classoid-p obj) (aver (not (sb-kernel::undefined-classoid-p obj))))
     (let* ((host-type (type-of obj))
            ;; Precompute a list of slots that should be initialized to a
            ;; trivially dumpable constant in lieu of whatever complicated
            ;; substructure it currently holds.
            (overrides
             (typecase obj
               (classoid
                (let ((slots-to-omit
                       `(;; :predicate will be patched in during cold init.
                         (,(dsd-index-cached 'built-in-classoid 'sb-kernel::predicate) .
                          ,(make-random-descriptor sb-vm:unbound-marker-widetag))
                         (,(dsd-index-cached 'classoid 'sb-kernel::subclasses) . nil)
                         ;; Even though (gethash (classoid-name obj) *cold-layouts*) may exist,
                         ;; we nonetheless must set LAYOUT to NIL or else warm build fails
                         ;; in the twisty maze of class initializations.
                         (,(dsd-index-cached 'classoid 'wrapper) . nil))))
                  (if (typep obj 'built-in-classoid)
                      slots-to-omit
                      ;; :predicate is not a slot. Don't mess up the object
                      ;; by omitting a slot at the same index as it.
                      (cdr slots-to-omit))))))
            (dd-slots (type-dd-slots-or-lose host-type))
            ;; ASSUMPTION: all slots consume 1 storage word
            (dd-len (+ sb-vm:instance-data-start (length dd-slots)))
            (result (allocate-struct-of-type host-type)))
       (setf (gethash obj *host->cold-ctype*) result) ; record it
       ;; Dump the slots.
       (do ((index sb-vm:instance-data-start (1+ index)))
           ((= index dd-len) result)
         (let* ((dsd (find index dd-slots :key #'dsd-index))
                (override (assq index overrides))
                (reader (dsd-accessor-name dsd)))
           (ecase (dsd-raw-type dsd)
             ((t)
              (write-wordindexed
               result
               (+ sb-vm:instance-slots-offset index)
               (if override
                   (or (cdr override) *nil-descriptor*)
                   (let ((val (funcall reader obj)))
                     (funcall (typecase val
                                ((or ctype xset list) #'ctype-to-core)
                                (sb-kernel::numtype-aspects #'numtype-aspects-to-core)
                                (t #'host-constant-to-core))
                              val)))))
             ((word sb-vm:signed-word)
              (write-wordindexed/raw result (+ sb-vm:instance-slots-offset index)
                                     (or (cdr override) (funcall reader obj)))))))
       (cond ((classoid-p obj) ; Place classoid into its classoid-cell.
              (let ((cell (cold-find-classoid-cell (classoid-name obj) :create t)))
                (write-slots cell :classoid result)))
             ((ctype-p obj)
              ;; If OBJ belongs in a hash container, then deduce which
              (let* ((hashset (sb-kernel::ctype->hashset-sym obj))
                     (preload
                      (cond ((and hashset (hashset-find (symbol-value hashset) obj))
                             hashset)
                            ((and (member-type-p obj)
                                  ;; NULL is a hardwired case in the MEMBER type constructor
                                  (neq obj (specifier-type 'null))
                                  (type-singleton-p obj))
                             'sb-kernel::*eql-type-cache*))))
                (when preload ; Record it
                  (cold-push (cold-cons result preload) 'sb-kernel::*!initial-ctypes*)))))
       result))))

;;; Convert a layout to a wrapper and back.
;;; Each points to the other through its first data word.
(defun ->wrapper (x) #+metaspace (read-wordindexed x 1) #-metaspace x)
(defun ->layout (x) #+metaspace (read-wordindexed x 1) #-metaspace x)

(defun initialize-layouts ()
  (flet ((chill-layout (name &rest inherits)
           ;; Check that the number of specified INHERITS matches
           ;; the length of the layout's inherits in the cross-compiler.
           (let ((wrapper (info :type :compiler-layout name)))
             (aver (eql (length (wrapper-inherits wrapper)) (length inherits)))
             (->wrapper
              (make-cold-layout name
                                (wrapper-depthoid wrapper)
                                (wrapper-flags wrapper)
                                (wrapper-length wrapper)
                                (wrapper-bitmap wrapper)
                                (vector-in-core inherits))))))
    ;; The variables are named foo-LAYOUT but are actually foo-WRAPPER.
    (let* ((t-layout   (chill-layout 't))
           (s-o-layout (chill-layout 'structure-object t-layout))
           #+metaspace (layout-layout (chill-layout 'sb-vm:layout t-layout s-o-layout))
           (wrapper-layout (chill-layout 'wrapper t-layout s-o-layout)))
      (when core-file-name
        #-metaspace
        (dolist (instance (list t-layout s-o-layout wrapper-layout))
          (set-instance-layout instance wrapper-layout))
        #+metaspace
        (progn (dolist (instance (list t-layout s-o-layout layout-layout wrapper-layout))
                 (set-instance-layout instance (->layout wrapper-layout))
                 (set-instance-layout (->layout instance) (->layout layout-layout)))))
      (chill-layout 'function t-layout)
      (chill-layout 'package t-layout s-o-layout)
      (let* ((sequence (chill-layout 'sequence t-layout))
             (list     (chill-layout 'list t-layout sequence))
             (symbol   (chill-layout 'symbol t-layout)))
        (chill-layout 'null t-layout sequence list symbol))
      (chill-layout 'stream t-layout))))

;;;; interning symbols in the cold image

;;; a map from package name as a host string to
;;; ((external-symbols . internal-symbols) . cold-package-descriptor)
(defvar *cold-package-symbols*)
(declaim (type hash-table *cold-package-symbols*))

;;; preincrement on use. the first non-preassigned ID is 5
(defvar *package-id-count* 4)

;;; Initialize the cold package named by NAME. The information is
;;; usually derived from the host package of the same name, except
;;; where the host package does not reflect the target package
;;; information, as for COMMON-LISP, KEYWORD, and COMMON-LISP-USER.
(defun initialize-cold-package (cold-package name)
  (multiple-value-bind (nicknames docstring id shadow use-list)
      (cond ((string= name "COMMON-LISP")
             (values '("CL")
                     "public: home of symbols defined by the ANSI language specification"
                     sb-impl::+package-id-lisp+
                     '()
                     '()))
            ((string= name "KEYWORD")
             (values '()
                     "public: home of keywords"
                     sb-impl::+package-id-keyword+
                     '()
                     '()))
            ((string= name "COMMON-LISP-USER")
             (values '("CL-USER")
                     "public: the default package for user code and data"
                     sb-impl::+package-id-user+
                     '()
                     ;; ANSI encourages us to put extension packages in
                     ;; the USE list of COMMON-LISP-USER.
                     '("COMMON-LISP" "SB-ALIEN" "SB-DEBUG"
                       "SB-EXT" "SB-GRAY" "SB-PROFILE")))
            (t
             (let ((package (find-package name)))
               (values (package-nicknames package)
                       (documentation package t)
                       (if (string= name "SB-KERNEL")
                           sb-impl::+package-id-kernel+
                           (incf *package-id-count*))
                       (sort (package-shadowing-symbols package) #'string<)
                       ;; SB-COREFILE is not actually part of
                       ;; the use list for SB-FASL. It's
                       ;; just needed for Genesis.
                       (if (string= name "SB-FASL")
                           (remove (find-package "SB-COREFILE")
                                   (package-use-list package))
                           (package-use-list package))))))
    (write-slots cold-package
                 :id (make-fixnum-descriptor id)
                 :%name (string-literal-to-core name)
                 :%nicknames (list-to-core
                              (mapcar #'string-literal-to-core nicknames))
                 :%bits (make-fixnum-descriptor
                         (if (system-package-p name)
                             sb-impl::+initial-package-bits+
                             0))
                 :doc-string (if (and docstring #-sb-doc nil)
                                 (string-literal-to-core docstring)
                                 *nil-descriptor*)
                 :%use-list (list-to-core
                             (mapcar (lambda (use)
                                       (cdr (cold-find-package-info
                                             (sb-xc:package-name use))))
                                     use-list)))
    (dolist (use use-list)
      ;; Push onto the "used-by" list.
      (let ((cold (cdr (cold-find-package-info (sb-xc:package-name use)))))
        (write-slots cold
                     :%used-by-list (cold-cons cold-package
                                               (read-slot cold :%used-by-list)))))
    ;; COLD-INTERN AVERs that the package has an ID, so delay writing
    ;; the shadowing-symbols until the package is ready.
    (write-slots cold-package
                 :%shadowing-symbols (list-to-core
                                      (mapcar 'cold-intern shadow)))))

(defun cold-find-package-info (package-name)
  ;; Create package info on demand.
  (or (gethash package-name *cold-package-symbols*)
      (let* ((cold-package (allocate-struct-of-type 'package))
             (info (cons (cons nil nil) cold-package)))
        (write-slots cold-package :%used-by-list *nil-descriptor*)
        (setf (gethash package-name *cold-package-symbols*) info)
        (initialize-cold-package cold-package package-name)
        info)))

(defvar *classoid-cells*)
(defun cold-find-classoid-cell (name &key create)
  (aver (eq create t))
  (or (gethash name *classoid-cells*)
      (setf (gethash name *classoid-cells*)
            (write-slots (allocate-struct-of-type 'sb-kernel::classoid-cell)
                         :name name
                         :pcl-class *nil-descriptor*
                         :classoid *nil-descriptor*))))

;;; a map from descriptors to symbols, so that we can back up. The key
;;; is the address in the target core.
(defvar *cold-symbols*)
(declaim (type hash-table *cold-symbols*))

(defun set-readonly (vector)
  (write-wordindexed/raw vector 0 (logior (read-bits-wordindexed vector 0)
                                          (ash sb-vm:+vector-shareable+
                                               sb-vm:array-flags-position)))
  vector)

(defvar *uninterned-symbol-table* (make-hash-table :test #'equal))
;; This coalesces references to uninterned symbols, which is allowed because
;; "similar-as-constant" is defined by string comparison, and since we only have
;; base-strings during Genesis, there is no concern about upgraded array type.
;; There is a subtlety of whether coalescing may occur across files
;; - the target compiler doesn't and couldn't - but here it doesn't matter.
(defun get-uninterned-symbol (name)
  (ensure-gethash name *uninterned-symbol-table*
                  (allocate-symbol sb-vm:symbol-size nil name)))

;;; Dump the target representation of HOST-VALUE,
;;; the type of which is in a restrictive set.
(defun host-constant-to-core (host-value &optional helper)
  (let ((visited (make-hash-table :test #'eq)))
    (named-let target-representation ((value host-value))
      (unless (typep value '(or symbol number descriptor))
        (let ((found (gethash value visited)))
          (cond ((eq found :pending)
                 (bug "circular constant?")) ; Circularity not permitted
                (found
                 (return-from target-representation found))))
        (setf (gethash value visited) :pending))
      (setf (gethash value visited)
            (typecase value
              (descriptor value)
              (symbol (if (cl:symbol-package value)
                          (cold-intern value)
                          (get-uninterned-symbol (string value))))
              (number (number-to-core value))
              (string (base-string-to-core value))
              (simple-bit-vector (bit-vector-to-core value))
              (cons (cold-cons (target-representation (car value))
                               (target-representation (cdr value))))
              (simple-vector
               (vector-in-core (map 'list #'target-representation value)))
              (t
               (or (and helper (funcall helper value))
                   (error "host-constant-to-core: can't convert ~S"
                          value))))))))

;; Look up the target's descriptor for #'FUN where FUN is a host symbol.
(defun cold-symbol-function (symbol &optional (errorp t))
  (let* ((symbol (if (symbolp symbol)
                     symbol
                     (warm-symbol symbol)))
         (f (cold-fdefn-fun (ensure-cold-fdefn symbol))))
    (cond ((not (cold-null f)) f)
          (errorp (error "Expected a definition for ~S in cold load" symbol))
          (t nil))))

;;; Return a handle on an interned symbol. If necessary allocate the
;;; symbol and record its home package.
(defun cold-intern (symbol
                    &key (access nil)
                         (gspace (symbol-value *cold-symbol-gspace*))
                    &aux (name (symbol-name symbol))
                         (package (sb-xc:symbol-package symbol)))
  ;; Symbols that are logically in COMMON-LISP but accessed through the SB-XC package
  ;; need to be re-interned since the cold-intern-info must be associated with
  ;; exactly one of the possible lookalikes, not both. The re-interned symbol
  ;; is usually homed in CL:, but might be homed in SB-XC. When the symbols identity
  ;; matters to the type system (floating-point specifiers), we never want to see the
  ;; host's symbol; the canonical package shall be SB-XC. We can figure out the
  ;; canonical home package by finding the symbol via the XC-STRICT-CL package.
  (cond ((eq package *cl-package*)
         (setq symbol (find-symbol name (canonical-home-package name))))
        ((not (or (eq package *keyword-package*)
                  (= (mismatch (cl:package-name package) "SB-") 3)))
         (bug "~S in bad package for target: ~A" symbol package)))

  (or (get symbol 'cold-intern-info)
      ;; KLUDGE: there is no way to automatically know which macros are handled
      ;; by sb-fasteval as special forms. An extra slot should be created in
      ;; any symbol naming such a macro, though things still work if the slot
      ;; doesn't exist, as long as only a deferred interpreter processor is used
      ;; and not an immediate processor.
      (let* ((pkg-info
              (when core-file-name (cold-find-package-info (sb-xc:package-name package))))
             (handle (allocate-symbol sb-vm:symbol-size
                      (cdr pkg-info) name :gspace gspace)))
        (when pkg-info
          (aver (not (zerop (descriptor-fixnum (read-slot (cdr pkg-info) :id))))))
        (setf (get symbol 'cold-intern-info) handle)
        ;; maintain reverse map from target descriptor to host symbol
        (setf (gethash (descriptor-bits handle) *cold-symbols*) symbol)
        #+sb-thread
        (let ((index (info :variable :wired-tls symbol)))
          (when (integerp index) ; thread slot or known TLS
            (cold-assign-tls-index handle index)))
        ;; Steps that only make sense when writing a core file
        (when core-file-name
          (record-accessibility (or access (nth-value 1 (find-symbol name package)))
                                pkg-info handle package symbol)
          (when (eq package *keyword-package*)
            (cold-set handle handle)))
        handle)))

(defun record-accessibility (accessibility target-pkg-info symbol-descriptor
                             &optional host-package host-symbol)
  (let ((access-lists (car target-pkg-info)))
    (case accessibility
      (:external (push symbol-descriptor (car access-lists)))
      (:internal (push symbol-descriptor (cdr access-lists)))
      (t (error "~S inaccessible in package ~S" host-symbol host-package)))))

;;; a hash table mapping from fdefinition names to descriptors of cold
;;; objects
;;;
;;; Note: Since fdefinition names can be lists like '(SETF FOO), and
;;; we want to have only one entry per name, this must be an 'EQUAL
;;; hash table, not the default 'EQL.
(defvar *cold-fdefn-objects*)

;;; Construct and return a value for use as *NIL-DESCRIPTOR*.
;;; It might be nice to put NIL on a readonly page by itself to prevent unsafe
;;; code from destroying the world with (RPLACx nil 'kablooey)
(defun make-nil-descriptor ()
  ;; 10 words prior to NIL is an array of three 'struct alloc_region'.
  ;; The lisp objects begin at STATIC_SPACE_OBJECTS_START.
  ;; See also (DEFCONSTANT NIL-VALUE) in early-objdef.
  #+(and gencgc (not sb-thread)) (gspace-claim-n-words *static* 10)
  #+64-bit (setf (gspace-free-word-index *static*) (/ 256 sb-vm:n-word-bytes))
  (let* ((des (allocate-otherptr *static* (1+ sb-vm:symbol-size) 0))
         (nil-val (make-descriptor (+ (descriptor-bits des)
                                      (* 2 sb-vm:n-word-bytes)
                                      (- sb-vm:list-pointer-lowtag
                                         ;; ALLOCATE-OTHERPTR always adds in
                                         ;; OTHER-POINTER-LOWTAG, so subtract it.
                                         sb-vm:other-pointer-lowtag))))
         (initial-info (cold-cons nil-val nil-val)))
    (aver (= (descriptor-bits nil-val) sb-vm:nil-value))

    (setf *nil-descriptor* nil-val
          (gethash (descriptor-bits nil-val) *cold-symbols*) nil
          (get nil 'cold-intern-info) nil-val)

    ;; Alter the first word to 0 instead of the symbol size. It reads as a fixnum,
    ;; but is meaningless. In practice, Lisp code can not utilize the fact that NIL
    ;; has a widetag; any use of NIL-as-symbol must pre-check for NIL. Consider:
    ;;   50100100: 0000000000000000 = 0
    ;;   50100108: 000000000000052D      <- 5 words follow, widetag = #x2D
    ;;   50100110: 0000000050100117
    ;;   50100118: 0000000050100117
    ;;   50100120: 0000001000000007 = (NIL . #<SB-INT:PACKED-INFO len=3 {1000002FF3}>)
    ;;   50100128: 000100100000400F
    ;;   50100130: 0000000000000000 = 0
    ;;
    ;; Indeed *(char*)(NIL-0xf) = #x2D, /* if little-endian */
    ;; so why can't we exploit this to improve SYMBOLP? Hypothetically:
    ;;    if (((ptr & 7) == 7) && *(char*)(ptr-15) == SYMBOL_WIDETAG) { }
    ;; which is true of NIL and all other symbols, but wrong, because it assumes
    ;; that _any_ cons cell could be accessed at a negative displacement from its
    ;; base address. Only NIL (viewed as a cons) has this property.
    ;; Otherwise we would be reading random bytes or inaccessible memory. Finally,
    ;; the above sequence would not necessarily decrease the instruction count!
    ;; Those points aside, gencgc correctly calls scav_symbol() on NIL.

    (when core-file-name
      (let ((name (string-literal-to-core "NIL")))
        (write-wordindexed des 0 (make-fixnum-descriptor 0))
        ;; The header-word for NIL "as a symbol" contains a length + widetag.
        (write-wordindexed des 1 (make-other-immediate-descriptor (1- sb-vm:symbol-size)
                                                                  sb-vm:symbol-widetag))
        (write-wordindexed des (+ 1 sb-vm:symbol-value-slot) nil-val)
        (write-wordindexed des (+ 1 sb-vm:symbol-hash-slot) nil-val)
        (write-wordindexed des (+ 1 sb-vm:symbol-info-slot) initial-info)
        (write-wordindexed des (+ 1 sb-vm:symbol-name-slot) name)
        #+ppc64
        (progn
          ;; We don't usually create an FDEFN for NIL, however on PP64 there is one
          ;; made now. Due to unique lowtagging on that architectures, it doesn't magically
          ;; work to access slots of NIL as a symbol the way the vops expect.
          ;; There are hacks in the symbol slot reading vops, but not the writing vops,
          ;; because nothing should write to slots of NIL. The sole exception would be that
          ;; if someone tries to funcall NIL there can be an undefined tramp,
          ;; so we make one here without adding complications to the lisp side.
          (write-wordindexed des (+ 1 sb-vm:symbol-fdefn-slot) (ensure-cold-fdefn nil))
          (remhash nil *cold-fdefn-objects*))
        #+compact-symbol
        (write-wordindexed/raw des (+ 1 sb-vm:symbol-name-slot)
                               (encode-symbol-name sb-impl::+package-id-lisp+ name))
        #-compact-symbol (write-wordindexed des (+ 1 sb-vm:symbol-name-slot) name)))
    nil))

;;; Since the initial symbols must be allocated before we can intern
;;; anything else, we intern those here. We also set the value of T.
(defun initialize-static-space (tls-init)
  "Initialize the cold load symbol-hacking data structures."
  (declare (ignorable tls-init))
  ;; -1 is magic having to do with nil-as-cons vs. nil-as-symbol
  #-compact-symbol
  (write-wordindexed *nil-descriptor* (- sb-vm:symbol-package-id-slot 1)
                     (make-fixnum-descriptor sb-impl::+package-id-lisp+))
  (when core-file-name
    ;; NIL did not have its package assigned. Do that now.
    (record-accessibility :external (cold-find-package-info "COMMON-LISP")
                          *nil-descriptor*))
  ;; Intern the others.
  (dovector (symbol sb-vm:+static-symbols+)
    (let* ((des (cold-intern symbol :gspace *static*))
           (offset-wanted (sb-vm:static-symbol-offset symbol))
           (offset-found (- (descriptor-bits des)
                            (descriptor-bits *nil-descriptor*))))
      (unless (= offset-wanted offset-found)
        (error "Offset from ~S to ~S is ~W, not ~W"
              symbol
              nil
              offset-found
              offset-wanted))))

  ;; Assign TLS indices of C interface symbols
  #+sb-thread
  (progn
    (dolist (binding sb-vm::per-thread-c-interface-symbols)
      (ensure-symbol-tls-index (car (ensure-list binding))))
    ;; Assign other known TLS indices
    (dolist (pair tls-init)
      (destructuring-bind (tls-index . symbol) pair
        (aver (eql tls-index (ensure-symbol-tls-index symbol))))))

  ;; Establish the value of T.
  (let ((t-symbol (cold-intern t :gspace *static*)))
    (cold-set t-symbol t-symbol))

  ;; Establish the value of SB-VM:FUNCTION-LAYOUT
  #+compact-instance-header
  (write-wordindexed/raw (cold-intern 'sb-vm:function-layout)
                         sb-vm:symbol-value-slot
                         (ash (cold-layout-descriptor-bits 'function) 32))

  #+metaspace
  (cold-set '**primitive-object-layouts**
            (allocate-vector sb-vm:simple-vector-widetag 256 256 *read-only*))
  #+(and immobile-space (not metaspace))
  (cold-set '**primitive-object-layouts**
            (let ((filler
                   (make-random-descriptor
                    (logior (gspace-byte-address *immobile-fixedobj*)
                            sb-vm:other-pointer-lowtag)))
                  (vector
                   (make-random-descriptor
                    (logior (+ (gspace-byte-address *immobile-fixedobj*)
                               sb-vm:immobile-card-bytes
                               (* (+ 2 256) (- sb-vm:n-word-bytes)))
                            sb-vm:other-pointer-lowtag))))
              (emplace-vector filler sb-vm:simple-array-fixnum-widetag
                              (- (/ sb-vm:immobile-card-bytes sb-vm:n-word-bytes)
                                 ;; subtract 2 object headers + 256 words
                                 (+ 4 256)))
              (emplace-vector vector sb-vm:simple-vector-widetag 256)
              vector))

  ;; Immobile code prefers all FDEFNs adjacent so that code can be located
  ;; anywhere in the addressable memory allowed by the OS, as long as all
  ;; FDEFNs are near enough all code (i.e. within a 32-bit jmp offset).
  ;; That fails if static fdefns are wired to an address below 4GB
  ;; and code resides above 4GB. But as the Fundamental Theorem says:
  ;;   any problem can be solved by adding another indirection.
  #+immobile-code
  (progn
  (setf *c-callable-fdefn-vector*
        (vector-in-core (make-list (length sb-vm::+c-callable-fdefns+)
                                   :initial-element *nil-descriptor*)
                        *static*))
  ;; static-call entrypoint vector must be immediately adjacent to *asm-routine-vector*
  (word-vector (make-list (length sb-vm:+static-fdefns+) :initial-element 0) *static*)
  (setf *asm-routine-vector* (word-vector (make-list 70 :initial-element 0)
                                          *static*)))

  #-immobile-code
  (dolist (sym sb-vm::+c-callable-fdefns+)
    (ensure-cold-fdefn sym *static*))

  ;; With immobile-code, static-fdefns as a concept are useful -
  ;; the implication is that the function's definition will not change.
  ;; But the fdefn per se is not useful - callers refer to callees directly.
  #-immobile-code
  (dovector (sym sb-vm:+static-fdefns+)
    (let* ((fdefn (ensure-cold-fdefn sym *static*))
           (offset (- (+ (- (descriptor-bits fdefn)
                            sb-vm:other-pointer-lowtag)
                         (* sb-vm:fdefn-raw-addr-slot sb-vm:n-word-bytes))
                      (descriptor-bits *nil-descriptor*)))
           (desired (sb-vm:static-fun-offset sym)))
      (unless (= offset desired)
        (error "Offset from FDEFN ~S to ~S is ~W, not ~W."
               sym nil offset desired)))))

;;; Sort *COLD-LAYOUTS* to return them in a deterministic order.
(defun sort-cold-layouts ()
  (sort (%hash-table-alist *cold-layouts*) #'<
        :key (lambda (x) (descriptor-bits (cold-layout-descriptor (cdr x))))))

;;; Establish initial values for magic symbols.
;;;
(defun finish-symbols ()
  (cold-set 'sb-kernel::*!initial-wrappers*
            (vector-in-core
             (mapcar (lambda (pair)
                       (cold-cons (cold-intern (car pair))
                                  (->wrapper (cold-layout-descriptor (cdr pair)))))
                     (sort-cold-layouts))))
  ;; MAKE-LAYOUT uses ATOMIC-INCF which returns the value in the cell prior to
  ;; increment, so we need to add 1 to get to the next value for it because
  ;; we always pre-increment *general-layout-uniqueid-counter* when reading it.
  (cold-set 'sb-kernel::*layout-id-generator*
            (cold-list (make-fixnum-descriptor
                        (1+ sb-kernel::*general-layout-uniqueid-counter*))))

  ;; Consume the rest of read-only-space as metaspace
  #+metaspace
  (let* ((space *read-only*)
         (slab (gspace-current-slab space)))
    (cold-set 'sb-vm::*metaspace-tracts*
              (word-vector (list (+ sb-vm::read-only-space-start 32768) ; KLUDGE
                                 (sap-int slab)
                                 sb-vm:read-only-space-end
                                 0))))

  #+sb-thread
  (cold-set 'sb-vm::*free-tls-index*
            (make-descriptor (ash *genesis-tls-counter* sb-vm:word-shift)))

  (cold-set 'sb-c::*code-serialno* (make-fixnum-descriptor (1+ sb-c::*code-serialno*)))

  (cold-set 'sb-impl::*setf-fdefinition-hook* *nil-descriptor*)
  (cold-set 'sb-impl::*user-hash-table-tests* *nil-descriptor*)

  #+immobile-code
  (let* ((space *immobile-text*)
         (wordindex (gspace-free-word-index space))
         (words-per-page (/ sb-vm:immobile-card-bytes sb-vm:n-word-bytes)))
    ;; Put the C-callable fdefns into the static-space vector of fdefns
    (loop for i from 0 for sym in sb-vm::+c-callable-fdefns+
          do (cold-svset *c-callable-fdefn-vector* i (ensure-cold-fdefn sym)))
    (cold-set 'sb-fasl::*asm-routine-vector* *asm-routine-vector*)
    (let* ((objects (gspace-objects space))
           (count (length objects)))
      (let ((remainder (rem wordindex words-per-page)))
        (unless (zerop remainder)
          (let* ((fill-nwords (- words-per-page remainder))
                 (des
                  ;; technically FILLER_WIDETAG has no valid lowtag because it's not an object
                  ;; that lisp can address. But WRITE-WORDINDEXED requires a pointer descriptor
                  (allocate-cold-descriptor space (* fill-nwords sb-vm:n-word-bytes)
                                            sb-vm:other-pointer-lowtag)))
            (aver (zerop (rem (gspace-free-word-index space) words-per-page)))
            (write-header-word des (logior (ash fill-nwords 32) sb-vm:filler-widetag)))))
      ;; Construct a ub32 array of object offsets.
      (let* ((n-data-words (ceiling count 2)) ; lispword = 2 ub32s
             (vect (allocate-vector sb-vm:simple-array-unsigned-byte-32-widetag
                                    count n-data-words))
             (data-ptr (+ (descriptor-byte-offset vect)
                          (ash sb-vm:vector-data-offset sb-vm:word-shift))))
        (dotimes (i count)
          (setf (bvref-32 (descriptor-mem vect) data-ptr)
                (descriptor-byte-offset (aref objects i)))
          (incf data-ptr 4))
        (cold-set 'sb-vm::*immobile-codeblob-vector* vect))))

  ;; Symbols for which no call to COLD-INTERN would occur - due to not being
  ;; referenced until warm init - must be artificially cold-interned.
  ;; Inasmuch as the "offending" things are compiled by ordinary target code
  ;; and not cold-init, I think we should use an ordinary DEFPACKAGE for
  ;; the added-on bits. What I've done is somewhat of a fragile kludge.
  (let (syms)
    (with-package-iterator (iter '("SB-PCL" "SB-MOP" "SB-GRAY" "SB-SEQUENCE"
                                   "SB-PROFILE" "SB-EXT" "SB-VM"
                                   "SB-C" "SB-FASL" "SB-DEBUG")
                                 :external)
      (loop
         (multiple-value-bind (foundp sym accessibility package) (iter)
           (declare (ignore accessibility))
           (cond ((not foundp) (return))
                 ((eq (cl:symbol-package sym) package) (push sym syms))))))
    (setf syms (stable-sort syms #'string<))
    (dolist (sym syms)
      (cold-intern sym)))

  (cold-set
   'sb-impl::*!initial-symbols*
   (cold-cons
    (let (uninterned)
      (maphash (lambda (key val) (declare (ignore key)) (push val uninterned))
               *uninterned-symbol-table*)
      (vector-in-core (sort uninterned #'< :key #'descriptor-bits)))
    (list-to-core
     (mapcar
      (lambda (pkgcons)
        (destructuring-bind (pkg-name . pkg-info) pkgcons
          (unless (member pkg-name '("COMMON-LISP" "COMMON-LISP-USER" "KEYWORD")
                          :test 'string=)
            (let ((host-pkg (find-package pkg-name))
                  syms)
              ;; Now for each symbol directly present in this host-pkg,
              ;; i.e. accessible but not :INHERITED, figure out if the symbol
              ;; came from a different package, and if so, make a note of it.
              (with-package-iterator (iter host-pkg :internal :external)
                (loop (multiple-value-bind (foundp sym accessibility) (iter)
                        (unless foundp (return))
                        (unless (eq (cl:symbol-package sym) host-pkg)
                          (push (cons sym accessibility) syms)))))
              (dolist (symcons (sort syms #'string< :key #'car))
                (destructuring-bind (sym . accessibility) symcons
                  (record-accessibility accessibility pkg-info (cold-intern sym)
                                        host-pkg sym)))))
          (cold-list (cdr pkg-info)
                     (vector-in-core (caar pkg-info))
                     (vector-in-core (cdar pkg-info)))))
      (sort (%hash-table-alist *cold-package-symbols*)
            #'string< :key #'car))))) ; Sort by package-name

  ;; assign *PACKAGE* since it supposed to be always-bound
  ;; and various things assume that it is. e.g. FIND-PACKAGE has an
  ;; (IF (BOUNDP '*PACKAGE*)) test which the compiler elides.
  (cold-set '*package* (cdr (cold-find-package-info "COMMON-LISP-USER")))

  (dump-symbol-infos
   (attach-fdefinitions-to-symbols
    (attach-classoid-cells-to-symbols (make-hash-table :test #'eq))))

  #+x86-64 ; Dump a popular constant
  (let ((array
         ;; Embed the constant in an unboxed array. This shouldn't be necessary,
         ;; because the start of the scanned space is STATIC_SPACE_OBJECTS_START,
         ;; but not all uses strictly follow that rule. (They should though)
         ;; This must not conflict with the alloc regions at the start of the space.
         (make-random-descriptor (logior (- sb-vm::non-negative-fixnum-mask-constant-wired-address
                                            (* 2 sb-vm:n-word-bytes))
                                         sb-vm:other-pointer-lowtag))))
    (write-wordindexed/raw array 0 sb-vm:simple-array-unsigned-byte-64-widetag)
    (write-wordindexed array 1 (make-fixnum-descriptor 1))
    (write-wordindexed/raw array 2 sb-vm::non-negative-fixnum-mask-constant))

  #+x86
  (progn
    (cold-set 'sb-vm::*fp-constant-0d0* (number-to-core $0d0))
    (cold-set 'sb-vm::*fp-constant-1d0* (number-to-core $1d0))
    (cold-set 'sb-vm::*fp-constant-0f0* (number-to-core $0f0))
    (cold-set 'sb-vm::*fp-constant-1f0* (number-to-core $1f0))))

;;;; functions and fdefinition objects

;;; Given a cold representation of a symbol, return a warm
;;; representation.
(defun warm-symbol (des)
  ;; Note that COLD-INTERN is responsible for keeping the
  ;; *COLD-SYMBOLS* table up to date, so if DES happens to refer to an
  ;; uninterned symbol, the code below will fail. But as long as we
  ;; don't need to look up uninterned symbols during bootstrapping,
  ;; that's OK..
  (multiple-value-bind (symbol found-p)
      (gethash (descriptor-bits des) *cold-symbols*)
    (declare (type symbol symbol))
    (unless found-p
      (error "no warm symbol"))
    symbol))

;;; like CL:CAR, CL:CDR, and CL:NULL but for cold values
(defun cold-car (des)
  (aver (= (descriptor-lowtag des) sb-vm:list-pointer-lowtag))
  (read-wordindexed des sb-vm:cons-car-slot))
(defun cold-cdr (des)
  (aver (= (descriptor-lowtag des) sb-vm:list-pointer-lowtag))
  (read-wordindexed des sb-vm:cons-cdr-slot))
(defun cold-rplacd (des newval)
  (aver (= (descriptor-lowtag des) sb-vm:list-pointer-lowtag))
  (write-wordindexed des sb-vm:cons-cdr-slot newval)
  des)
(defun cold-null (des) (descriptor= des *nil-descriptor*))

;;; Given a cold representation of a function name, return a warm
;;; representation.
(declaim (ftype (function ((or symbol descriptor)) (or symbol list)) warm-fun-name))
(defun warm-fun-name (des)
  (let ((result
         (if (symbolp des)
             ;; This parallels the logic at the start of COLD-INTERN
             ;; which re-homes symbols in SB-XC to COMMON-LISP.
             (if (eq (cl:symbol-package des) (find-package "SB-XC"))
                 (intern (symbol-name des) (canonical-home-package (string des)))
                 des)
             (ecase (descriptor-lowtag des)
              (#.sb-vm:list-pointer-lowtag
               (aver (not (cold-null des))) ; function named NIL? please no..
               (let ((rest (cold-cdr des)))
                 (aver (cold-null (cold-cdr rest)))
                 (list (warm-symbol (cold-car des))
                       (warm-symbol (cold-car rest)))))
              (#.sb-vm:other-pointer-lowtag
               (warm-symbol des))))))
    (legal-fun-name-or-type-error result)
    result))

(defvar *cold-assembler-obj*) ; a single code component
;;; Writing the address of the undefined trampoline into static fdefns
;;; has to occur after the asm routines are loaded, which occurs after
;;; the static fdefns are initialized.
(defvar *deferred-undefined-tramp-refs*)
(defun fdefn-makunbound (fdefn)
  (write-wordindexed fdefn sb-vm:fdefn-fun-slot *nil-descriptor*)
  (write-wordindexed/raw fdefn sb-vm:fdefn-raw-addr-slot
                         (lookup-assembler-reference 'sb-vm::undefined-tramp :direct)))
(defun ensure-cold-fdefn (cold-name &optional
                                          (gspace #+immobile-space *immobile-fixedobj*
                                                  #-immobile-space *dynamic*))
  (declare (type (or symbol descriptor) cold-name))
  (let ((warm-name (warm-fun-name cold-name)))
    (or (gethash warm-name *cold-fdefn-objects*)
        (let ((fdefn (allocate-otherptr gspace sb-vm:fdefn-size sb-vm:fdefn-widetag)))
          (setf (gethash warm-name *cold-fdefn-objects*) fdefn)
          #+x86-64
          (write-wordindexed/raw ; write an INT instruction into the header
           fdefn 0 (logior (ash sb-vm::undefined-fdefn-header 16)
                           (read-bits-wordindexed fdefn 0)))
          (write-wordindexed fdefn sb-vm:fdefn-name-slot cold-name)
          (when core-file-name
            (when (typep warm-name '(and symbol (not null)))
              (write-wordindexed (cold-intern warm-name) sb-vm:symbol-fdefn-slot fdefn))
            (if *cold-assembler-obj*
                (fdefn-makunbound fdefn)
                (push (lambda ()
                        (when (zerop (read-bits-wordindexed fdefn sb-vm:fdefn-fun-slot))
                          ;; This is probably irrelevant - it only occurs for static fdefns,
                          ;; but every static fdefn will eventually get a definition.
                          (fdefn-makunbound fdefn)))
                      *deferred-undefined-tramp-refs*)))
          fdefn))))

(defun cold-fun-entry-addr (fun)
  (aver (= (descriptor-lowtag fun) sb-vm:fun-pointer-lowtag))
  (+ (descriptor-bits fun)
     (- sb-vm:fun-pointer-lowtag)
     (ash sb-vm:simple-fun-insts-offset sb-vm:word-shift)))

(defun cold-fset (name function)
  (aver (= (descriptor-widetag function) sb-vm:simple-fun-widetag))
  (let ((fdefn (ensure-cold-fdefn
                ;; (SETF f) was descriptorized when dumped, symbols were not.
                (if (symbolp name)
                    (cold-intern name)
                    name))))
    (let ((existing (read-wordindexed fdefn sb-vm:fdefn-fun-slot)))
      (unless (or (cold-null existing) (descriptor= existing function))
        (error "Function multiply defined: ~S. Was ~x is ~x" name
                 (descriptor-bits existing)
                 (descriptor-bits function))))
    (write-wordindexed fdefn sb-vm:fdefn-fun-slot function)
    #+x86-64
    (write-wordindexed/raw ; write a JMP instruction into the header
     fdefn 0 (dpb #x1025FF (byte 24 16) (read-bits-wordindexed fdefn 0)))
    (write-wordindexed/raw
     fdefn sb-vm:fdefn-raw-addr-slot
     (or #+(or sparc arm riscv) ; raw addr is the function descriptor
         (descriptor-bits function)
         ;; For all others raw addr is the starting address
         (+ (logandc2 (descriptor-bits function) sb-vm:lowtag-mask)
            (ash sb-vm:simple-fun-insts-offset sb-vm:word-shift))))
    fdefn))

(defun attach-classoid-cells-to-symbols (hashtable)
  (when (plusp (hash-table-count *classoid-cells*))
    (aver (gethash 'sb-kernel::classoid-cell *cold-layouts*))
    (let ((type-classoid-cell-info
            (sb-c::meta-info-number (sb-c::meta-info :type :classoid-cell)))
          (type-kind-info
            (sb-c::meta-info-number (sb-c::meta-info :type :kind))))
      ;; Iteration order is immaterial. The symbols will get sorted later.
      (maphash (lambda (symbol cold-classoid-cell)
                 (let ((packed-info
                        (packed-info-insert
                         (gethash symbol hashtable +nil-packed-infos+)
                         sb-impl::+no-auxiliary-key+
                         type-classoid-cell-info cold-classoid-cell)))
                   ;; an instance classoid won't be returned from %PARSE-TYPE
                   ;; unless the :KIND is set, but we can't set the kind
                   ;; to :INSTANCE unless the classoid is present in the cell.
                   (when (and (eq (info :type :kind symbol) :instance)
                              (not (cold-null (read-slot cold-classoid-cell :classoid))))
                     (setf packed-info
                           (packed-info-insert
                            packed-info sb-impl::+no-auxiliary-key+
                            type-kind-info (cold-intern :instance))))
                   (setf (gethash symbol hashtable) packed-info)))
               *classoid-cells*)))
  hashtable)

;; Create pointer from SYMBOL and/or (SETF SYMBOL) to respective fdefinition
;;
(defun attach-fdefinitions-to-symbols (hashtable)
  ;; Collect fdefinitions that go with one symbol, e.g. (SETF CAR) and (CAS CAR)
  ;; using the host's code for manipulating a packed-info.
  ;; Do not add fdefns for symbols to the info. It goes in a slot.
  (maphash (lambda (warm-name cold-fdefn)
             (unless (symbolp warm-name)
               (with-globaldb-name (key1 key2) warm-name
                 :hairy (error "Hairy fdefn name in genesis: ~S" warm-name)
                 :simple (setf (gethash key1 hashtable)
                               (packed-info-insert
                                (gethash key1 hashtable +nil-packed-infos+)
                                key2 +fdefn-info-num+ cold-fdefn)))))
           *cold-fdefn-objects*)
  hashtable)

(defun dump-packed-info (list)
  ;; Payload length is the element count + LAYOUT slot if necessary.
  ;; Header word is added automatically by ALLOCATE-STRUCT
  (let ((s (allocate-struct (+ sb-vm:instance-data-start (length list))
                            (cold-layout-descriptor (gethash 'packed-info *cold-layouts*)))))
    (loop for i from (+ sb-vm:instance-slots-offset sb-vm:instance-data-start)
          for elt in list do (write-wordindexed s i elt))
    s))
(defun dump-symbol-infos (hashtable)
  (cold-set 'sb-impl::+nil-packed-infos+
            (dump-packed-info (list (make-fixnum-descriptor 0))))
  ;; Emit in the same order symbols reside in core to avoid
  ;; sensitivity to the iteration order of host's maphash.
  (loop for (warm-sym . info)
        in (sort (%hash-table-alist hashtable) #'<
                 :key (lambda (x) (descriptor-bits (cold-intern (car x)))))
     do (aver warm-sym) ; enforce that NIL was specially dealt with already
        (aver (> (sb-impl::packed-info-len info) 1))
        (write-wordindexed
         (cold-intern warm-sym)
         sb-vm:symbol-info-slot
         (dump-packed-info
          ;; Each packed-info will have one fixnum, possibly the symbol SETF,
          ;; and zero, one, or two #<fdefn>, and/or a classoid-cell.
          (map 'list (lambda (elt)
                       (etypecase elt
                         (symbol (cold-intern elt))
                         (sb-xc:fixnum (make-fixnum-descriptor elt))
                         (descriptor elt)))
               (sb-impl::packed-info-cells info))))))

;;;; fixups and related stuff

;;; an EQUAL hash table
(defvar *cold-foreign-symbol-table*)
(declaim (type hash-table *cold-foreign-symbol-table*))

(defvar *cold-assembler-routines*)
(defvar *cold-static-call-fixups*)

;;: See picture in 'objdef'
(defun code-object-size (code-object) ; Return total size in bytes
  (* (ash (get-header-data code-object) (+ #+64-bit -24))
     sb-vm:n-word-bytes))

;; Boxed header length is stored directly in bytes, not words
(defun code-header-bytes (code-object)
  (ldb (byte 32 0) (read-bits-wordindexed code-object sb-vm:code-boxed-size-slot)))
(defun code-header-words (code-object) ; same, but expressed in words
  (ash (code-header-bytes code-object) (- sb-vm:word-shift)))

(defun code-instructions (code)
  (make-model-sap (- (+ (descriptor-bits code) (code-header-bytes code))
                     sb-vm:other-pointer-lowtag)
                  (descriptor-gspace code)))

;;; These are fairly straightforward translations of the similarly named accessor
;;; from src/code/simple-fun.lisp
(defun code-trailer-ref (code offset)
  "Reference a uint_32 relative to the end of code at byte offset OFFSET.
Legal values for OFFSET are -4, -8, -12, ..."
  (bvref-32 (descriptor-mem code)
            (+ (descriptor-byte-offset code) (code-object-size code) offset)))
(defun code-fun-table-count (code)
  "Return the COUNT trailer word in CODE. The COUNT is a packed integer containing
 the number of embedded SIMPLE-FUNs and the number of padding bytes in the
 instructions prior to the start of the simple-fun offset list"
  ;; The case of trailer-len = 0 (no trailer payload) can't happen during genesis,
  ;; so we don't check for it.
  (let ((word (code-trailer-ref code -4)))
    ;; TRAILER-REF returns 4-byte quantities. Extract a two-byte quantity.
    #+little-endian (ldb (byte 16  0) word)
    #+big-endian    (ldb (byte 16 16) word)))

;;; These are literally identical between cross-compiler and target.
;;; TODO: Maybe put them somewhere that gets defined for both?
;;; (Minor problem of CODE-COMPONENT not being a primitive type though)
(defun code-n-entries (code)
  (ash (code-fun-table-count code) -5))
(defun %code-fun-offset (code fun-index)
  ;; The 4-byte quantity at "END" - 4 is the trailer count, the word at -8 is
  ;; the offset to the 0th simple-fun, -12 is the next, etc...
  (code-trailer-ref code (* -4 (+ fun-index 2))))

(defun lookup-assembler-reference (symbol &optional (mode :direct))
  (let* ((code-component *cold-assembler-obj*)
         (list *cold-assembler-routines*)
         (insts (+ (logandc2 (descriptor-bits code-component) sb-vm:lowtag-mask)
                   (code-header-bytes code-component)))
         (offset (or (cdr (assq symbol list))
                     (error "Assembler routine ~S not defined." symbol)))
         (addr (+ insts offset)))
    (ecase mode
      (:direct addr)
      #+(or ppc ppc64) (:indirect (- addr sb-vm:nil-value))
      #+(or x86 x86-64)
      (:indirect
       (let ((index (count-if (lambda (x) (< (cdr x) offset)) list)))
         #-immobile-space
         (+ insts (ash (1+ index) sb-vm:word-shift)) ; add 1 for the jump table count
         #+immobile-space
         (+ (logandc2 (descriptor-bits *asm-routine-vector*) sb-vm:lowtag-mask)
            (ash (+ sb-vm:vector-data-offset index) sb-vm:word-shift)))))))

;;; Unlike in the target, FOP-KNOWN-FUN sometimes has to backpatch.
(defvar *deferred-known-fun-refs*)

(defun code-jump-table-words (code)
  (ldb (byte 14 0) (read-bits-wordindexed code (code-header-words code))))

(declaim (ftype (sfunction (descriptor sb-vm:word (or sb-vm:word
                                                      sb-vm:signed-word)
                                       keyword keyword)
                           descriptor)
                cold-fixup))
(defun cold-fixup (code-object after-header value kind flavor)
  (sb-vm:fixup-code-object code-object after-header value kind flavor)
  code-object)

(defun resolve-static-call-fixups ()
  (dolist (fixup *cold-static-call-fixups*)
    (destructuring-bind (name kind code offset) fixup
      (cold-fixup code offset
                  (cold-fun-entry-addr (cold-symbol-function name))
                  kind :static-call))))

(defun alien-linkage-table-note-symbol (symbol-name datap)
  "Register a symbol and return its address in proto-linkage-table."
  (sb-vm::alien-linkage-table-entry-address
   (ensure-gethash (if datap (list symbol-name) symbol-name)
                   *cold-foreign-symbol-table*
                   (hash-table-count *cold-foreign-symbol-table*))))

(defun foreign-symbols-to-core ()
  (flet ((to-core (list transducer target-symbol)
           (cold-set target-symbol (vector-in-core (mapcar transducer list)))))
    ;; Sort by index into linkage table
    (to-core (sort (%hash-table-alist *cold-foreign-symbol-table*) #'< :key #'cdr)
             (lambda (pair &aux (key (car pair))
                                (sym (string-literal-to-core
                                      (if (listp key) (car key) key))))
               (if (listp key) (cold-list sym) sym))
             'sb-vm::+required-foreign-symbols+)
    (cold-set (cold-intern '*assembler-routines*) *cold-assembler-obj*)
    (to-core *cold-assembler-routines*
             (lambda (rtn)
               (cold-cons (cold-intern (first rtn)) (make-fixnum-descriptor (cdr rtn))))
             '*!initial-assembler-routines*)))


;;;; general machinery for cold-loading FASL files

(defun pop-fop-stack (stack)
  (let ((top (svref stack 0)))
    (declare (type index top))
    (when (eql 0 top)
      (error "FOP stack empty"))
    (setf (svref stack 0) (1- top))
    (svref stack top)))

;;; Cause a fop to have a special definition for cold load.
;;;
;;; This is similar to DEFINE-FOP, but unlike DEFINE-FOP, this version
;;; looks up the encoding for this name (created by a previous DEFINE-FOP)
;;; instead of creating a new encoding.
(defmacro define-cold-fop ((name &optional arglist) &rest forms)
  #+c-headers-only (declare (ignore name arglist forms))
  #-c-headers-only
  (let* ((code (get name 'opcode))
         (argc (aref (car **fop-signatures**)
                     (or code
                         (error "~S is not a defined FOP." name))))
         (fname (symbolicate "COLD-" name)))
    (aver (= (length arglist) argc))
    `(progn
       (defun ,fname (.fasl-input. ,@arglist)
         (declare (ignorable .fasl-input.))
         (macrolet ((fasl-input () '(the fasl-input .fasl-input.))
                    (fasl-input-stream () '(%fasl-input-stream (fasl-input)))
                    (pop-stack ()
                      '(pop-fop-stack (%fasl-input-stack (fasl-input)))))
           ,@forms))
       ;; We simply overwrite elements of **FOP-FUNS** since the contents
       ;; of the host are never propagated directly into the target core.
       (setf (svref **fop-funs** ,code) #',fname))))

;;; Cause a fop to be undefined in cold load.
(defmacro not-cold-fop (name)
  `(define-cold-fop (,name)
     (error "The fop ~S is not supported in cold load." ',name)))

;;; COLD-LOAD loads stuff into the core image being built by calling
;;; LOAD-AS-FASL with the fop function table rebound to a table of cold
;;; loading functions.
(defun cold-load (filename verbose)
  "Load the file named by FILENAME into the cold load image being built."
  (when verbose
    (write-line (namestring filename)))
  (with-open-file (s filename :element-type '(unsigned-byte 8))
    (load-as-fasl s nil nil)))

;;;; miscellaneous cold fops

(define-cold-fop (fop-misc-trap) *unbound-marker*)

(define-cold-fop (fop-struct (size)) ; n-words incl. layout, excluding header
  (let* ((layout (->layout (pop-stack)))
         (result (allocate-struct size layout))
         (bitmap (cold-layout-bitmap (gethash (descriptor-bits layout) *cold-layout-by-addr*)))
         (stack (%fasl-input-stack (fasl-input)))
         (n-data-words (- size sb-vm:instance-data-start)))
    (do ((stack-index (fop-stack-pop-n stack n-data-words) (1+ stack-index))
         (dsd-index sb-vm:instance-data-start (1+ dsd-index)))
        ((>= dsd-index size))
      (let ((val (svref stack stack-index)))
        (if (logbitp dsd-index bitmap)
            (write-wordindexed result (+ sb-vm:instance-slots-offset dsd-index) val)
            (write-wordindexed/raw result (+ sb-vm:instance-slots-offset dsd-index)
                                   (the sb-vm:word (descriptor-integer val))))))
   result))

(defun find-in-inherits (typename inherits)
  (binding* ((proxy (gethash typename *cold-layouts*) :exit-if-null)
             (layout (->wrapper (cold-layout-descriptor proxy))))
    (dotimes (i (cold-vector-len inherits))
      (when (descriptor= (cold-svref inherits i) layout)
        (return t)))))

;;; Always return a WRAPPER if #+metaspace
(define-cold-fop (fop-layout (depthoid flags length))
  (decf depthoid) ; was bumped by 1 since non-stack args can't encode negatives
  (let* ((inherits (pop-stack))
         (bitmap-descriptor  (pop-stack))
         (bitmap-value (descriptor-integer bitmap-descriptor))
         (name (pop-stack))
         (existing-layout (gethash name *cold-layouts*)))
    (declare (type descriptor bitmap-descriptor inherits))
    (declare (type symbol name))
    ;; parameters have to match an existing FOP-LAYOUT invocation if there was one
    (when existing-layout
      (let ((old-flags (cold-layout-flags existing-layout))
            (old-depthoid (cold-layout-depthoid existing-layout))
            (old-length (cold-layout-length existing-layout))
            (old-bitmap (cold-layout-bitmap existing-layout))
            (old-inherits (cold-layout-inherits existing-layout)))
        (unless (and (= flags old-flags)
                     (= depthoid old-depthoid)
                     (= length old-length)
                     (= bitmap-value old-bitmap)
                     (eql (cold-vector-len inherits) (cold-vector-len old-inherits))
                     (dotimes (i (cold-vector-len inherits) t)
                       (unless (descriptor= (cold-svref inherits i)
                                            (cold-svref old-inherits i))
                         (return nil))))
          ;; Users will never see this.
          (format t "old=(flags=~d depthoid=~d length=~d bitmap=~d inherits=~s)~%"
                  old-flags old-depthoid old-length old-bitmap
                  (vector-from-core old-inherits))
          (format t "new=(flags=~d depthoid=~d length=~d bitmap=~d inherits=~s)~%"
                  flags depthoid length bitmap-value
                  (vector-from-core inherits))
          (bug "Messed up fop-layout for ~s" name))))
    (->wrapper
     (if existing-layout
         (cold-layout-descriptor existing-layout)
         (make-cold-layout name depthoid flags length bitmap-value inherits)))))

;;;; cold fops for loading symbols

;;; Given STRING naming a symbol exported from COMMON-LISP, return either "SB-XC"
;;; or "COMMON-LISP" depending on which we consider canonical for the symbol's
;;; home package during genesis. If finding the symbol via XC-STRICT-CL finds a
;;; symbol in SB-XC, then that package is canonical.  This is very important to
;;; get right for symbols whose identity matters (floating-point type specifiers),
;;; or else the interned ctype objects get all messed up.
(defun canonical-home-package (string)
  (if (eq (cl:symbol-package (find-symbol string "XC-STRICT-CL"))
          (find-package "SB-XC"))
      "SB-XC"
      "COMMON-LISP"))

;;; Load a symbol SIZE characters long from FASL-INPUT, and
;;; intern that symbol in PACKAGE.
(defun cold-load-symbol (length+flag package fasl-input)
  (let ((string (make-string (ash length+flag -1))))
    (read-string-as-bytes (%fasl-input-stream fasl-input) string)
    (push-fop-table (intern string (if (eq package *cl-package*)
                                       (canonical-home-package string)
                                       package))
                    fasl-input)))

(define-cold-fop (fop-symbol-in-package-save (length+flag pkg-index))
  (cold-load-symbol length+flag (ref-fop-table (fasl-input) pkg-index)
                    (fasl-input)))

(define-cold-fop (fop-symbol-in-package-internal-save (length+flag pkg-index))
  (cold-load-symbol length+flag (ref-fop-table (fasl-input) pkg-index)
                    (fasl-input)))

(define-cold-fop (fop-lisp-symbol-save (length+flag))
  (cold-load-symbol length+flag *cl-package* (fasl-input)))

(define-cold-fop (fop-keyword-symbol-save (length+flag))
  (cold-load-symbol length+flag *keyword-package* (fasl-input)))

(define-cold-fop (fop-uninterned-symbol-save (length+flag))
  (let ((name (make-string (ash length+flag -1))))
    (read-string-as-bytes (fasl-input-stream) name)
    (push-fop-table (get-uninterned-symbol name) (fasl-input))))

(defun read-cold-symbol-name (symbol)
  (base-string-from-core (read-wordindexed symbol sb-vm:symbol-name-slot)))

(define-cold-fop (fop-copy-symbol-save (index))
  (let* ((symbol (ref-fop-table (fasl-input) index))
         (name
          (if (symbolp symbol)
              (symbol-name symbol)
              (read-cold-symbol-name symbol))))
    ;; Genesis performs additional coalescing of uninterned symbols
    (push-fop-table (get-uninterned-symbol name) (fasl-input))))

;;;; cold fops for loading packages

(define-cold-fop (fop-named-package-save (namelen))
  (let ((name (make-string namelen)))
    (read-string-as-bytes (fasl-input-stream) name)
    (push-fop-table (find-package name) (fasl-input))))

;;;; cold fops for loading vectors

(define-cold-fop (fop-base-string (len))
  (let ((string (make-string len)))
    (read-string-as-bytes (fasl-input-stream) string)
    (string-literal-to-core string)))

#+sb-unicode
(define-cold-fop (fop-character-string (len))
  (bug "CHARACTER-STRING[~D] dumped by cross-compiler." len))

(define-cold-fop (fop-vector (size))
  (do* ((stack (%fasl-input-stack (fasl-input)))
        (stackptr (fop-stack-pop-n stack size) (1+ stackptr))
        (result (allocate-vector sb-vm:simple-vector-widetag
                                 size size *dynamic*))
        (index sb-vm:vector-data-offset (1+ index))
        (end (+ sb-vm:vector-data-offset size)))
       ((= index end) (set-readonly result))
    (write-wordindexed result index (svref stack stackptr))))

; (not-cold-fop fop-array) ; the syntax doesn't work
#+nil
;; This code is unexercised. The only use of FOP-ARRAY is from target-dump.
;; It would be a shame to delete it though, as it might come in handy.
(define-cold-fop (fop-array)
  (let* ((rank (read-word-arg (fasl-input-stream)))
         (data-vector (pop-stack))
         (result (allocate-object *dynamic*
                                  (+ sb-vm:array-dimensions-offset rank)
                                  sb-vm:other-pointer-lowtag)))
    (write-header-data+tag result rank sb-vm:simple-array-widetag)
    (write-wordindexed result sb-vm:array-fill-pointer-slot *nil-descriptor*)
    (write-wordindexed result sb-vm:array-data-slot data-vector)
    (write-wordindexed result sb-vm:array-displacement-slot *nil-descriptor*)
    (write-wordindexed result sb-vm:array-displaced-p-slot *nil-descriptor*)
    (write-wordindexed result sb-vm:array-displaced-from-slot *nil-descriptor*)
    (let ((total-elements 1))
      (dotimes (axis rank)
        (let ((dim (pop-stack)))
          (unless (is-fixnum-lowtag (descriptor-lowtag dim))
            (error "non-fixnum dimension? (~S)" dim))
          (setf total-elements (* total-elements (descriptor-fixnum dim)))
          (write-wordindexed result
                             (+ sb-vm:array-dimensions-offset axis)
                             dim)))
      (write-wordindexed result
                         sb-vm:array-elements-slot
                         (make-fixnum-descriptor total-elements)))
    result))


;;;; cold fops for calling (or not calling)

(defvar *load-time-value-counter*)

(define-cold-fop (fop-funcall (n))
  (if (= n 0)
      (let ((counter *load-time-value-counter*))
        (push (cold-list (cold-intern :load-time-value)
                         (pop-stack)
                         (number-to-core counter)) *!cold-toplevels*)
        (setf *load-time-value-counter* (1+ counter))
        (make-ltv-patch counter))
      (let ((des (pop-stack)))
        (unless (and (= n 1)
                     (eq (pop-stack) 'values-specifier-type))
          (error "Can't FOP-FUNCALL random stuff in cold load."))
        (let ((spec (if (descriptor-p des) (host-object-from-core des) des)))
          (ctype-to-core (if (eq spec '*)
                             *wild-type*
                             (values-specifier-type spec)))))))

(defun finalize-load-time-value-noise ()
  (cold-set '*!load-time-values*
            (allocate-vector sb-vm:simple-vector-widetag
                             *load-time-value-counter*
                             *load-time-value-counter*)))

(define-cold-fop (fop-funcall-for-effect (n))
  (if (= n 0)
      (push (pop-stack) *!cold-toplevels*)
      (error "Can't FOP-FUNCALL-FOR-EFFECT random stuff in cold load")))

(define-cold-fop (fop-named-constant-set (index))
  (push (cold-list (cold-intern :named-constant)
                   (pop-stack)
                   (number-to-core index)
                   (pop-stack))
        *!cold-toplevels*))


;;;; cold fops for fixing up circularities

(define-cold-fop (fop-rplaca (tbl-slot idx))
  (let ((obj (ref-fop-table (fasl-input) tbl-slot)))
    (write-wordindexed (cold-nthcdr idx obj) 0 (pop-stack))))

(define-cold-fop (fop-rplacd (tbl-slot idx))
  (let ((obj (ref-fop-table (fasl-input) tbl-slot)))
    (write-wordindexed (cold-nthcdr idx obj) 1 (pop-stack))))

(define-cold-fop (fop-svset (tbl-slot idx))
  (let ((obj (ref-fop-table (fasl-input) tbl-slot)))
    (write-wordindexed obj (+ idx sb-vm:vector-data-offset) (pop-stack))))

(define-cold-fop (fop-structset (tbl-slot idx))
  (let ((obj (ref-fop-table (fasl-input) tbl-slot)))
    (write-wordindexed obj (+ idx sb-vm:instance-slots-offset) (pop-stack))))

(define-cold-fop (fop-nthcdr (n))
  (cold-nthcdr n (pop-stack)))

(defun cold-nthcdr (index obj)
  (dotimes (i index)
    (setq obj (read-wordindexed obj sb-vm:cons-cdr-slot)))
  obj)

;;;; cold fops for loading code objects and functions

(define-cold-fop (fop-fset)
  (let ((fn (pop-stack))
        (name (pop-stack)))
    (cold-fset name fn)))

(define-cold-fop (fop-mset)
  (let ((fn (pop-stack))
        (specializers (pop-stack))
        (qualifiers (pop-stack))
        (name (pop-stack)))
    ;; Methods that are qualified or are specialized on more than
    ;; one argument do not work on start-up, since our start-up
    ;; implementation of method dispatch is single dispatch only.
    (when (and (null qualifiers)
               (= 1 (count-if-not (lambda (x) (eq x t)) (host-object-from-core specializers))))
      (push (list (cold-car specializers) fn)
            (cdr (or (assoc name *cold-methods*)
                     (car (push (list name) *cold-methods*))))))))

;;; Order all initial methods so that the first one whose guard
;;; returns T is the most specific method. LAYOUT-DEPTHOID is a valid
;;; sort key for this because we don't have multiple inheritance in
;;; the system object type lattice.
(defun sort-initial-methods ()
  (cold-set
   'sb-pcl::*!initial-methods*
   (list-to-core
    (loop for (gf-name . methods) in *cold-methods*
          collect
          (cold-cons
           (cold-intern gf-name)
           (vector-in-core
            (loop for (class fun)
                    ;; Methods must be sorted because we invoke
                    ;; only the first applicable one.
                    in (stable-sort methods #'> ; highest depthoid first
                                    :key (lambda (method)
                                           (class-depthoid (warm-symbol (car method)))))
                  collect
                  (vector-in-core
                   (let ((class-symbol (warm-symbol class)))
                     (list (cold-intern
                            (predicate-for-specializer class-symbol))
                           (acond ((gethash class-symbol *cold-layouts*)
                                   (->wrapper (cold-layout-descriptor it)))
                                  (t
                                   (aver (predicate-for-specializer class-symbol))
                                   class))
                           fun))))))))))

(define-cold-fop (fop-fdefn)
  (ensure-cold-fdefn (pop-stack)))

(define-cold-fop (fop-known-fun)
  (let ((name (pop-stack)))
    (or (cold-symbol-function name nil) ; no error if undefined
        `(:known-fun . ,name))))

;;; Setting this variable shows what code looks like before any
;;; fixups (or function headers) are applied.
(defvar *show-pre-fixup-code-p* nil)

(defun store-named-call-fdefn (code index fdefn)
  #+untagged-fdefns
  (write-wordindexed/raw code index (- (descriptor-bits fdefn)
                                       sb-vm:other-pointer-lowtag))
  #-untagged-fdefns (write-wordindexed code index fdefn))

(define-cold-fop (fop-load-code (header n-code-bytes n-fixup-elts))
  (let* ((n-simple-funs (read-unsigned-byte-32-arg (fasl-input-stream)))
         (n-fdefns (read-unsigned-byte-32-arg (fasl-input-stream)))
         (n-boxed-words (ash header -1))
         (n-constants (- n-boxed-words sb-vm:code-constants-offset))
         (stack-elts-consumed (+ n-constants 1 n-fixup-elts))
         (immobile (oddp header)) ; decode the representation used by dump
         ;; The number of constants is rounded up to even (if required)
         ;; to ensure that the code vector will be properly aligned.
         (aligned-n-boxed-words (align-up n-boxed-words sb-c::code-boxed-words-align))
         (stack (%fasl-input-stack (fasl-input)))
         (stack-index (fop-stack-pop-n stack stack-elts-consumed))
         (des (allocate-cold-descriptor
                  (or #+immobile-code (and immobile *immobile-text*)
                      *dynamic*)
                  (+ (ash aligned-n-boxed-words sb-vm:word-shift) n-code-bytes)
                  sb-vm:other-pointer-lowtag :code)))
    (declare (ignorable immobile))
    (write-code-header-words des aligned-n-boxed-words n-code-bytes n-fdefns)
    (write-wordindexed des sb-vm:code-debug-info-slot
                       (svref stack (+ stack-index n-constants)))

    (let ((start (+ (descriptor-byte-offset des)
                    (ash aligned-n-boxed-words sb-vm:word-shift))))
      (read-into-bigvec (descriptor-mem des) (fasl-input-stream) start n-code-bytes)
      (aver (= (code-n-entries des) n-simple-funs))
      (let ((jumptable-word (read-bits-wordindexed des aligned-n-boxed-words)))
        (aver (zerop (ash jumptable-word -14)))
        ;; assign serialno
        (write-wordindexed/raw
         des aligned-n-boxed-words
         (logior (ash (incf sb-c::*code-serialno*) (byte-position sb-vm::code-serialno-byte))
                 jumptable-word)))
      (when *show-pre-fixup-code-p*
        (format *trace-output*
                "~&LOAD-CODE: ~d header words, ~d code bytes.~%"
                n-boxed-words n-code-bytes)
        (do ((i start (+ i sb-vm:n-word-bytes))
             (count (floor n-code-bytes sb-vm:n-word-bytes) (1- count)))
            ((zerop count))
          (format *trace-output*
                  " ~X: ~V,'.X~%"
                  (+ i (gspace-byte-address (descriptor-gspace des)))
                  (* 2 sb-vm:n-word-bytes)
                  (bvref-word (descriptor-mem des) i)))))

    (apply-fixups des stack (+ stack-index (1+ n-constants)) n-fixup-elts)
    (let ((header-index sb-vm:code-constants-offset))
      (declare (type index header-index stack-index))
      (dotimes (fun-index (code-n-entries des))
        (let ((fn (%code-entry-point des fun-index)))
          #+compact-instance-header
          (write-wordindexed/raw fn 0 (logior (ash (cold-layout-descriptor-bits 'function) 32)
                                              (read-bits-wordindexed fn 0)))
          #+(or x86 x86-64 arm64) ; store a machine-native pointer to the function entry
          ;; note that the bit pattern looks like fixnum due to alignment
          (write-wordindexed/raw fn sb-vm:simple-fun-self-slot
                                 (+ (- (descriptor-bits fn) sb-vm:fun-pointer-lowtag)
                                    (ash sb-vm:simple-fun-insts-offset sb-vm:word-shift)))
          #-(or x86 x86-64 arm64) ; store a pointer back to the function itself in 'self'
          (write-wordindexed fn sb-vm:simple-fun-self-slot fn))
        (dotimes (i sb-vm:code-slots-per-simple-fun)
          (write-wordindexed des header-index (svref stack stack-index))
          (incf header-index)
          (incf stack-index)))
      (dotimes (i n-fdefns)
        (store-named-call-fdefn des header-index (svref stack stack-index))
        (incf header-index)
        (incf stack-index))
      (do () ((>= header-index n-boxed-words))
       (let ((constant (svref stack stack-index)))
         (cond ((and (consp constant) (eq (car constant) :known-fun))
                (push (list* (cdr constant) des header-index) *deferred-known-fun-refs*))
               (t
                (write-wordindexed des header-index constant))))
        (incf header-index)
        (incf stack-index)))
    des))

(defun resolve-deferred-known-funs ()
  (dolist (item *deferred-known-fun-refs*)
    (let ((fun (cold-symbol-function (car item)))
          (place (cdr item)))
      (write-wordindexed (car place) (cdr place) fun))))

(defun %code-entry-point (code-object fun-index)
  (let ((fun (sap-int (sap+ (code-instructions code-object)
                            (%code-fun-offset code-object fun-index)))))
    (unless (zerop (logand fun sb-vm:lowtag-mask))
      (error "unaligned function entry ~S ~S" code-object fun-index))
    (make-descriptor (logior fun sb-vm:fun-pointer-lowtag))))

(define-cold-fop (fop-assembler-code)
  (aver (not *cold-assembler-obj*))
  (let* ((n-routines (read-word-arg (fasl-input-stream)))
         (length (read-word-arg (fasl-input-stream)))
         (n-fixup-elts (read-word-arg (fasl-input-stream)))
         (rounded-length (round-up length (* 2 sb-vm:n-word-bytes)))
         (header-n-words (sb-c::asm-routines-boxed-header-nwords))
         (space (or #+(and immobile-code (not metaspace)) *immobile-text*
                    ;; If there is a read-only space, use it, else use static space.
                    (if (> sb-vm:read-only-space-end sb-vm:read-only-space-start)
                        *read-only*
                        *static*)))
         (asm-code
          (allocate-cold-descriptor
                  space
                  (+ (ash header-n-words sb-vm:word-shift) rounded-length)
                  sb-vm:other-pointer-lowtag)))
    (setf *cold-assembler-obj* asm-code)
    (write-code-header-words asm-code header-n-words rounded-length 0)
    (let ((start (+ (descriptor-byte-offset asm-code)
                    (ash header-n-words sb-vm:word-shift))))
      (read-into-bigvec (descriptor-mem asm-code) (fasl-input-stream) start length))
    ;; Write a bignum reference into the boxed constants.
    ;; All the backends should do this, as its avoids consing in GENERIC-NEGATE
    ;; when the argument is MOST-NEGATIVE-FIXNUM.
    #+x86-64 (write-wordindexed asm-code sb-vm:code-constants-offset
                                (bignum-to-core (- most-negative-fixnum)
                                                #-immobile-space *static*))
    ;; Update the name -> address table.
    (let (table)
      (dotimes (i n-routines)
        (let ((offset (descriptor-fixnum (pop-stack)))
              (name (pop-stack)))
          (push (cons name offset) table)))
      ;; Now that we combine all assembler routines into a single code object
      ;; at assembly time, they can all be sorted at this point.
      ;; We used to combine them with some magic in genesis.
      (setq *cold-assembler-routines* (sort table #'< :key #'cdr)))
    (let ((stack (%fasl-input-stack (fasl-input))))
      (apply-fixups asm-code stack (fop-stack-pop-n stack n-fixup-elts) n-fixup-elts))
    #+(or x86 x86-64) ; fill in the indirect call table
    (let ((base (code-header-words asm-code))
          (index 0))
      (dolist (item *cold-assembler-routines*)
        ;; Word 0 of code-instructions is the jump table count (the asm routine entrypoints
        ;; look to GC exactly like a jump table in any other codeblob)
        (let ((entrypoint (lookup-assembler-reference (car item))))
          (write-wordindexed/raw asm-code (+ base index 1) entrypoint)
          #+immobile-space
          (unless (member (car item) ; these can't be called from compiled Lisp
                          '(sb-vm::fpr-save sb-vm::save-xmm sb-vm::save-ymm
                            sb-vm::fpr-restore sb-vm::restore-xmm sb-vm::restore-ymm))
            (write-wordindexed/raw *asm-routine-vector*
                                   (+ sb-vm:vector-data-offset index) entrypoint)))
        (incf index)))))

;; The partial source info is not needed during the cold load, since
;; it can't be interrupted.
(define-cold-fop (fop-note-partial-source-info)
  (pop-stack)
  (pop-stack)
  (pop-stack)
  (values))

;;; Target variant of this is defined in 'target-load'
(defun apply-fixups (code-obj fixups index count &aux (end (1- (+ index count))))
  (let ((retained-fixups (svref fixups index)))
    (write-wordindexed code-obj sb-vm::code-fixups-slot retained-fixups)
    (incf index))
  (binding* ((alloc-points (svref fixups index) :exit-if-null))
    (cold-set 'sb-c::*!cold-allocation-patch-point*
              (cold-cons (cold-cons code-obj alloc-points)
                         (cold-symbol-value 'sb-c::*!cold-allocation-patch-point*))))
  (loop
    (when (>= index end) (return))
    (binding* (((offset kind flavor)
                (!unpack-fixup-info (descriptor-integer (svref fixups (incf index)))))
               (name (cond ((member flavor '(:code-object :gc-barrier)) nil)
                           (t (svref fixups (incf index)))))
               (string
                (when (and (descriptor-p name)
                           (= (descriptor-widetag name) sb-vm:simple-base-string-widetag))
                  (base-string-from-core name))))
      (if (eq flavor :static-call)
          (push (list name kind code-obj offset) *cold-static-call-fixups*)
          (cold-fixup
           code-obj offset
           (ecase flavor
             (:assembly-routine (lookup-assembler-reference name))
             (:assembly-routine* (lookup-assembler-reference name :indirect))
             (:foreign (alien-linkage-table-note-symbol string nil))
             (:foreign-dataref (alien-linkage-table-note-symbol string t))
             (:code-object (descriptor-bits code-obj))
             #+sb-thread ; ENSURE-SYMBOL-TLS-INDEX isn't defined otherwise
             (:symbol-tls-index (ensure-symbol-tls-index name))
             (:layout (cold-layout-descriptor-bits name))
             (:layout-id ; SYM is a #<WRAPPER>
              (cold-layout-id (gethash (descriptor-bits (->layout name))
                                       *cold-layout-by-addr*)))
             ;; The machine-dependent code decides how to patch in 'nbits'
             #+gencgc (:gc-barrier sb-vm::gencgc-card-table-index-nbits)
             (:immobile-symbol
              ;; an interned symbol is represented by its host symbol,
              ;; but an uninterned symbol is a descriptor.
              (descriptor-bits (if (symbolp name) (cold-intern name) name)))
             (:symbol-value (descriptor-bits (cold-symbol-value name)))
             (:fdefn-call ; x86-64 only
              (+ (descriptor-bits (ensure-cold-fdefn name))
                 ;; this jumps to the jump instruction embedded within an fdefn.
                 ;; (It's a terrible technique which I plan to remove.)
                 (- 2 sb-vm:other-pointer-lowtag))))
           kind flavor))))
  code-obj)

;;;; sanity checking space layouts

(defun check-spaces ()
  ;;; Co-opt type machinery to check for intersections...
  (let (types)
    (flet ((check (start end space)
             (when (= start end) ; 0 size is allowed
               (return-from check))
             (unless (< start end)
               (error "Space bounds look bad: ~A = ~X..~X" space start end))
             (let ((type (specifier-type `(integer ,start (,end)))))
               (dolist (other types)
                 (unless (eq *empty-type* (type-intersection (cdr other) type))
                   (error "Space overlap: ~A with ~A" space (car other))))
               (push (cons space type) types))))
      (check sb-vm:read-only-space-start sb-vm:read-only-space-end :read-only)
      (check sb-vm:static-space-start sb-vm:static-space-end :static)
      #+gencgc
      (check sb-vm:dynamic-space-start
             (+ sb-vm:dynamic-space-start sb-vm::default-dynamic-space-size)
             :dynamic)
      #+immobile-space
      ;; Must be a multiple of 32 because it makes the math a nicer
      ;; when computing word and bit index into the 'touched' bitmap.
      (aver (zerop (rem sb-vm:fixedobj-space-size (* 32 sb-vm:immobile-card-bytes))))
      #-gencgc
      (check sb-vm:dynamic-0-space-start sb-vm:dynamic-0-space-end :dynamic-0)
      #-immobile-space
      (let ((end (+ sb-vm:alien-linkage-table-space-start sb-vm:alien-linkage-table-space-size)))
        (check sb-vm:alien-linkage-table-space-start end :linkage-table)))))

;;;; emitting C header file

(defun tailwise-equal (string tail)
  (and (>= (length string) (length tail))
       (string= string tail :start1 (- (length string) (length tail)))))

(defun write-boilerplate (*standard-output*)
  (format t "/*~%")
  (dolist (line
           '("This is a machine-generated file. Please do not edit it by hand."
             "(As of sbcl-0.8.14, it came from WRITE-CONFIG-H in genesis.lisp.)"
             nil
             "This file contains low-level information about the"
             "internals of a particular version and configuration"
             "of SBCL. It is used by the C compiler to create a runtime"
             "support environment, an executable program in the host"
             "operating system's native format, which can then be used to"
             "load and run 'core' files, which are basically programs"
             "in SBCL's own format."))
    (format t " *~@[ ~A~]~%" line))
  (format t " */~%"))

(defun c-name (string &optional strip)
  (delete #\+
          (substitute-if #\_ (lambda (c) (member c '(#\- #\/ #\%)))
                         (remove-if (lambda (c) (position c strip))
                                    string))))

(defun c-symbol-name (symbol &optional strip)
  (c-name (symbol-name symbol) strip))

(defun write-makefile-features (*standard-output*)
  ;; propagating SB-XC:*FEATURES* into the Makefiles
  (dolist (target-feature-name (sort (mapcar #'c-symbol-name sb-xc:*features*)
                                     #'string<))
    (format t "LISP_FEATURE_~A=1~%" target-feature-name)))

(defun write-config-h (*standard-output*)
  ;; propagating SB-XC:*FEATURES* into C-level #define's
  (dolist (target-feature-name (sort (mapcar #'c-symbol-name sb-xc:*features*)
                                     #'string<))
    (format t "#define LISP_FEATURE_~A~%" target-feature-name))
  (terpri)
  ;; and miscellaneous constants
  (format t "#define SBCL_TARGET_ARCHITECTURE_STRING ~S~%"
          (substitute #\_ #\- (string-downcase (sb-cold::target-platform-keyword))))
  (format t "#define SBCL_VERSION_STRING ~S~%"
            (sb-xc:lisp-implementation-version))
  (format t "#define CORE_MAGIC 0x~X~%" core-magic)
  (format t "#ifndef __ASSEMBLER__~2%")
  (format t "#define LISPOBJ(x) ((lispobj)x)~2%")
  (format t "#else /* __ASSEMBLER__ */~2%")
  (format t "#define LISPOBJ(thing) thing~2%")
  (format t "#endif /* __ASSEMBLER__ */~2%")
  (terpri))

(defvar +c-literal-64bit+
  #+(and win32 x86-64) "LLU" ; "long" is 32 bits, "long long" is 64 bits
  #-(and win32 x86-64) "LU") ; "long" is 64 bits

(defun write-constants-h (*standard-output*)
  (let ((constants nil))
    (flet ((record (string priority symbol suffix)
             (push (list string priority (symbol-value symbol) suffix)
                   constants)))
      ;; writing entire families of named constants
      (dolist (package-name '("SB-VM"
                              ;; We also propagate magic numbers
                              ;; related to file format,
                              ;; which live here instead of SB-VM.
                              "SB-FASL"
                              ;; Home package of some constants which aren't
                              ;; in the target Lisp but are propagated to C.
                              "SB-COREFILE"))
        (do-external-symbols (symbol (find-package package-name))
          (when (cl:constantp symbol)
            (let ((name (symbol-name symbol)))
              ;; Older naming convention
              (labels ((record-camelcased (prefix string priority)
                         (record (concatenate 'simple-string
                                              prefix
                                              (delete #\- (string-capitalize string)))
                                 priority symbol ""))
                       (maybe-record (tail prefix priority)
                         (when (tailwise-equal name tail)
                           (record-camelcased prefix
                                              (subseq name 0
                                                      (- (length name) (length tail)))
                                              priority))))
                (maybe-record "-FLAG" "flag_" 2)
                (maybe-record "-TRAP" "trap_" 3)
                (maybe-record "-SC-NUMBER" "sc_" 5))
              ;; Newer naming convention
              (labels ((record-translated (priority large)
                         (record (c-name name) priority symbol
                                 (if large +c-literal-64bit+ "")))
                       (maybe-record (suffixes priority &key large)
                         (when (some (lambda (suffix) (tailwise-equal name suffix))
                                     suffixes)
                           (record-translated priority large))))
                (maybe-record '("-LOWTAG"  "-ALIGN") 0)
                (maybe-record '("-WIDETAG" "-SHIFT") 1)
                (maybe-record '("SHAREABLE+" "SHAREABLE-NONSTD+") 4)
                (maybe-record '("-SIZE" "-INTERRUPTS") 6)
                (maybe-record '("-START" "-END" "-PAGE-BYTES"
                                                     "-CARD-BYTES" "-GRANULARITY")
                                                   7 :large t)
                (maybe-record '("-CORE-ENTRY-TYPE-CODE") 8)
                (maybe-record '("-CORE-SPACE-ID") 9)
                (maybe-record '("-CORE-SPACE-ID-FLAG") 9)
                (maybe-record '("-GENERATION+") 10))))))
      (dolist (c '(sb-impl::+package-id-none+
                   sb-impl::+package-id-keyword+
                   sb-impl::+package-id-lisp+
                   sb-impl::+package-id-user+
                   sb-impl::+package-id-kernel+
                   sb-impl::symbol-name-bits))
        (record (c-symbol-name c) 3/2 #| arb |# c ""))
      ;; Other constants that aren't necessarily grouped into families.
      (dolist (c '(sb-kernel:maximum-bignum-length
                   sb-vm:n-word-bits sb-vm:n-word-bytes
                   sb-vm:n-lowtag-bits sb-vm:lowtag-mask
                   sb-vm:n-widetag-bits sb-vm:widetag-mask
                   sb-vm:n-fixnum-tag-bits sb-vm:fixnum-tag-mask
                   sb-vm:instance-length-mask
                   sb-vm:dsd-raw-type-mask
                   sb-vm:short-header-max-words
                   sb-vm:array-flags-position
                   sb-vm:array-rank-position))
        (record (c-symbol-name c) -1 c ""))
      ;; More symbols that doesn't fit into the pattern above.
      (dolist (c '(sb-impl::+magic-hash-vector-value+
                   ;; These next two flags bits use different naming conventions unfortunately,
                   ;; but one's a vector header bit, the other a layout flag bit.
                   sb-vm::+vector-alloc-mixed-region-bit+
                   sb-kernel::+strictly-boxed-flag+
                   sb-vm::nil-symbol-slots-start
                   sb-vm::nil-symbol-slots-end
                   sb-vm::static-space-objects-start))
        (record (c-symbol-name c) 7 #| arb |# c +c-literal-64bit+)))
    ;; Sort by <priority, value, alpha> which is TOO COMPLICATED imho.
    ;; Priority and then alphabetical would suffice.
    (setf constants
          (sort constants
                (lambda (const1 const2)
                  (if (= (second const1) (second const2)) ; priority
                      (if (= (third const1) (third const2)) ; value
                          (string< (first const1) (first const2))
                          (< (third const1) (third const2)))
                      (< (second const1) (second const2))))))
    (let ((prev-priority (second (car constants))))
      (dolist (const constants)
        (destructuring-bind (name priority value suffix) const
          (unless (= prev-priority priority)
            (terpri)
            (setf prev-priority priority))
          (when (minusp value)
            (error "stub: negative values unsupported"))
          (format t "#define ~A ~A~A /* 0x~X */~%" name value suffix value))))
    (terpri))

  (format t "#define CLASSOID_NAME_WORDINDEX ~d~%" sb-kernel::classoid-name-wordindex)
  ;; backend-page-bytes doesn't really mean much any more.
  ;; It's the granularity at which we can map the core file pages.
  (format t "#define BACKEND_PAGE_BYTES ~D~%" sb-c:+backend-page-bytes+)
  #+gencgc ; values never needed in Lisp, so therefore not a defconstant
  (progn
  (format t "#define MAX_CONSES_PER_PAGE ~D~%" sb-vm::max-conses-per-page)
  (format t "#define CARDS_PER_PAGE ~D~%#define GENCGC_CARD_SHIFT ~D~%"
          sb-vm::cards-per-page ; this is the "GC" page, not "backend" page
          sb-vm::gencgc-card-shift))

  (let ((size #+cheneygc (- sb-vm:dynamic-0-space-end sb-vm:dynamic-0-space-start)
              #+gencgc sb-vm::default-dynamic-space-size))
  ;; "-DDEFAULT_DYNAMIC_SPACE_SIZE=n" in CFLAGS will override this.
    (format t "#ifndef DEFAULT_DYNAMIC_SPACE_SIZE
#define DEFAULT_DYNAMIC_SPACE_SIZE ~D /* ~:*0x~X */
#endif~2%" size))

  ;; writing information about internal errors
  ;; Assembly code needs only the constants for UNDEFINED_[ALIEN_]FUN_ERROR
  ;; but to avoid imparting that knowledge here, we'll expose all error
  ;; number constants except for OBJECT-NOT-<x>-ERROR ones.
  (loop for (description name) across sb-c:+backend-internal-errors+
        for i from 0
        when (stringp description)
        do (format t "#define ~A ~D~%" (c-symbol-name name) i))

  (terpri)

  #+(and win32 x86-64)
  (format t "#define WIN64_SEH_DATA_ADDR ((void*)~DUL) /* ~:*0x~X */~%"
            sb-vm:win64-seh-data-addr)

  ;; FIXME: The SPARC has a PSEUDO-ATOMIC-TRAP that differs between
  ;; platforms. If we export this from the SB-VM package, it gets
  ;; written out as #define trap_PseudoAtomic, which is confusing as
  ;; the runtime treats trap_ as the prefix for illegal instruction
  ;; type things. We therefore don't export it, but instead do
  #+sparc
  (when (boundp 'sb-vm::pseudo-atomic-trap)
    (format t
            "#define PSEUDO_ATOMIC_TRAP ~D /* 0x~:*~X */~%"
            sb-vm::pseudo-atomic-trap)
    (terpri))
  ;; possibly this is another candidate for a rename (to
  ;; pseudo-atomic-trap-number or pseudo-atomic-magic-constant
  ;; [possibly applicable to other platforms])

  #+sb-safepoint
  (format t "#define GC_SAFEPOINT_PAGE_ADDR ((void*)0x~XUL) /* ~:*~A */~%"
            sb-vm:gc-safepoint-page-addr)
  #+sb-safepoint
  (format t "#define GC_SAFEPOINT_TRAP_ADDR ((void*)0x~XUL) /* ~:*~A */~%"
            (+ sb-vm:gc-safepoint-page-addr
               sb-c:+backend-page-bytes+
               (- sb-vm:gc-safepoint-trap-offset)))

  (dolist (symbol '(sb-vm:float-traps-byte
                    sb-vm::float-exceptions-byte
                    sb-vm:float-sticky-bits
                    sb-vm::float-rounding-mode))
    (format t "#define ~A_POSITION ~A /* ~:*0x~X */~%"
            (c-symbol-name symbol)
            (sb-xc:byte-position (symbol-value symbol)))
    (format t "#define ~A_MASK 0x~X /* ~:*~A */~%"
            (c-symbol-name symbol)
            (sb-xc:mask-field (symbol-value symbol) -1))))

(defun write-regnames-h (stream)
  (declare (ignorable stream))
  #-x86 ;; too weird - "UESP" (user-mode register ESP) is only
  ;; visible in a ucontext, so not a lisp register.
  (flet ((prettify (macro list &optional trailing-slash)
           (aver (not (member nil list)))
           (format stream "#define ~a " macro)
           (let ((linelen 100) ; force a line break
                 (delim nil))
             (dolist (item list)
               (cond ((> linelen 70)
                      (format stream "~:[~;,~]\\~%    " delim)
                      (setq delim nil linelen 4)) ; four leading spaces
                     (delim
                      (write-string ", " stream)
                      (incf linelen 2)))
               (write-string item stream)
               (incf linelen (length item))
               (setq delim t))
             (when trailing-slash (write-char #\\ stream))
             (terpri stream))))
    (let ((names sb-vm::*register-names*))
      (prettify "REGNAMES" (map 'list (lambda (x) (format nil "~s" x)) names))
      (when (boundp 'sb-vm::boxed-regs)
        (prettify "BOXED_REGISTERS {"
                  (mapcar (lambda (i) (format nil "reg_~A" (aref names i)))
                          (symbol-value 'sb-vm::boxed-regs))
                  t)
        (format stream "}~%")))))

(defun write-errnames-h (stream)
  ;; C code needs strings for describe_internal_error()
  (format stream "#define INTERNAL_ERROR_NAMES ~{\\~%~S~^, ~}~2%"
          (map 'list 'sb-kernel::!c-stringify-internal-error
               sb-c:+backend-internal-errors+))
  (format stream "#define INTERNAL_ERROR_NARGS {~{~S~^, ~}}~2%"
          (map 'list #'cddr sb-c:+backend-internal-errors+)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'sb-vm::primitive-object-variable-length-p))

(defun write-tagnames-h (out)
  (labels
      ((pretty-name (symbol strip)
         (let ((name (string-downcase symbol)))
           (substitute #\Space #\-
                       (subseq name 0 (- (length name) (length strip))))))
       (list-sorted-tags (tail)
         (loop for symbol being the external-symbols of "SB-VM"
               when (and (cl:constantp symbol)
                         (tailwise-equal (string symbol) tail))
               collect symbol into tags
               finally (return (sort tags #'< :key #'symbol-value))))
       (write-tags (visibility kind limit ash-count)
         ;; KIND is the string "-LOWTAG" or "-WIDETAG"
         (format out "~%~Aconst char *~(~A~)_names[] = {~%"
                 visibility (subseq kind 1))
         (let ((tags (list-sorted-tags kind)))
           (dotimes (i limit)
             (let ((known (eql i (ash (or (symbol-value (first tags)) -1) ash-count))))
               (if known
                   (if (string= kind "-WIDETAG")
                       (format out "    ~S" (sb-vm::widetag-string-name (pop tags)))
                       (format out "    \"~A\"" (pretty-name (pop tags) kind)))
                   (format out "    \"unknown [~D]\"" i)))
             (unless (eql i (1- limit))
               (write-string "," out))
             (terpri out)))
         (write-line "};" out)))
    (format out "#include <stddef.h>~%") ; for NULL
    (write-tags "static " "-LOWTAG" sb-vm:lowtag-limit 0)
    ;; this -2 shift depends on every OTHER-IMMEDIATE-?-LOWTAG
    ;; ending with the same 2 bits. (#b10)
    (write-tags "" "-WIDETAG" (ash (1+ sb-vm:widetag-mask) -2) -2))
  (dolist (name '(symbol ratio complex sb-vm::code simple-fun
                  closure funcallable-instance
                  weak-pointer fdefn sb-vm::value-cell))
    (format out "static char *~A_slots[] = {~%~{ \"~A: \",~} NULL~%};~%"
            (c-name (string-downcase name))
            (map 'list (lambda (x) (c-name (string-downcase (sb-vm:slot-name x))))
                 (let* ((obj (sb-vm::primitive-object name))
                        (slots (coerce (sb-vm:primitive-object-slots obj) 'list)))
                   (butlast slots
                            (if (primitive-object-variable-length-p obj) 1 0))))))
  (values))

(defun write-c-print-dispatch (out)
  (dolist (flavor '("print" "brief"))
    (let ((a (make-array (1+ sb-vm:lowtag-mask))))
      (dotimes (i (length a))
        (setf (aref a i)
              (format nil "~a_~a" flavor
                      (if (logtest i sb-vm:fixnum-tag-mask) "otherimm" "fixnum"))))
      (setf (aref a sb-vm:instance-pointer-lowtag) (format nil "~a_struct" flavor)
            (aref a sb-vm:list-pointer-lowtag) (format nil "~a_list" flavor)
            (aref a sb-vm:fun-pointer-lowtag) (format nil "~a_fun_or_otherptr" flavor)
            (aref a sb-vm:other-pointer-lowtag) (format nil "~a_fun_or_otherptr" flavor))
      (format out "static void (*~a_fns[])(lispobj obj) = {~
~{~% ~a, ~a, ~a, ~a~^,~}~%};~%" flavor (coerce a 'list)))))

(defun write-cast-operator (operator-name c-name lowtag stream)
  (format stream "static inline struct ~A* ~A(lispobj obj) {
  return (struct ~A*)(obj - ~D);~%}~%" c-name operator-name c-name lowtag)
  (case operator-name
    (fdefn
     (format stream "#define StaticSymbolFunction(x) FdefnFun(x##_FDEFN)
/* Return 'fun' given a tagged pointer to an fdefn. */
static inline lispobj FdefnFun(lispobj fdefn) { return FDEFN(fdefn)->fun; }~%"))
    (symbol
     (format stream "
lispobj symbol_function(struct symbol* symbol);
#include \"~A/vector.h\"
struct vector *symbol_name(struct symbol*);~%
lispobj symbol_package(struct symbol*);~%" (genesis-header-prefix))
     (format stream "static inline int symbol_package_id(struct symbol* s) { return ~A; }~%"
            #+compact-symbol (format nil "s->name >> ~D" sb-impl::symbol-name-bits)
            #-compact-symbol "fixnum_value(s->package_id)")
     #+compact-symbol
     (progn (format stream "#define decode_symbol_name(ptr) (ptr & (uword_t)0x~X)~%"
                    (mask-field (byte sb-impl::symbol-name-bits 0) -1))
            (format stream "static inline void set_symbol_name(struct symbol*s, lispobj name) {
  s->name = (s->name & (uword_t)0x~X) | name;~%}~%"
                    (mask-field (byte sb-impl::package-id-bits sb-impl::symbol-name-bits) -1)))
     #-compact-symbol
     (progn (format stream "#define decode_symbol_name(ptr) ptr~%")
            (format stream "static inline void set_symbol_name(struct symbol*s, lispobj name) {
  s->name = name;~%}~%")))))

(defun mangle-c-slot-name (obj-name slot-name)
  ;; For data hiding purposes, change the name of vector->length to vector->length_.
  ;; This helped me catch some erroneous C code.
  (if (and (eq obj-name 'vector) (eq slot-name 'length))
      "length_"
      (c-name (string-downcase slot-name))))

(defun write-primitive-object (obj *standard-output*)
  (let* ((name (sb-vm:primitive-object-name obj))
         (c-name (c-name (string-downcase name)))
         (slots (sb-vm:primitive-object-slots obj))
         (lowtag (or (symbol-value (sb-vm:primitive-object-lowtag obj)) 0)))
  ;; writing primitive object layouts
    (flet ((output-c ()
             (when (eq name 'sb-vm::code)
               (format t "#define CODE_SLOTS_PER_SIMPLE_FUN ~d~2%"
                       sb-vm:code-slots-per-simple-fun))
             (when (eq name 'sb-vm::thread)
               (format t "#define INIT_THREAD_REGIONS(x) \\~%")
               (let ((tlabs (map 'list
                                 (lambda (x) (c-name (string-downcase (second x))))
                                 (remove-if-not (lambda (x)
                                                  (tailwise-equal (string (second x)) "-TLAB"))
                                                slots))))
                 (format t "~{ gc_init_region(&x->~A)~^,\\~%~}~2%" tlabs))
               (when (find 'sb-vm::pseudo-atomic-bits slots :key #'sb-vm:slot-name)
                 (format t "#define HAVE_THREAD_PSEUDO_ATOMIC_BITS_SLOT 1~2%")
                 #+(or sparc ppc ppc64) (format t "typedef char pa_bits_t[~d];~2%" sb-vm:n-word-bytes)
                 #-(or sparc ppc ppc64) (format t "typedef lispobj pa_bits_t;~2%")))
             (format t "struct ~A {~%" c-name)
             (when (sb-vm:primitive-object-widetag obj)
               (format t "    lispobj header;~%"))
             (dovector (slot slots)
               (format t "    ~A ~A~@[[1]~];~%"
                       (getf (cddr slot) :c-type "lispobj")
                       (mangle-c-slot-name name (sb-vm:slot-name slot))
                       (and (primitive-object-variable-length-p obj)
                            (eq slot (aref slots (1- (length slots)))))))
             (format t "};~%")
             (when (eq name 'vector)
               ;; This is 'sword_t' because we formerly would call fixnum_value() which
               ;; is a signed int, but it isn't really; except that I made all C vars
               ;; signed to avoid comparison mismatch, and don't want to change back.
               (format t "static inline sword_t vector_len(struct vector* v) {")
               #+ubsan (format t "  return v->header >> ~d; }~%"
                                     (+ 32 sb-vm:n-fixnum-tag-bits))
               #-ubsan (format t "  return v->length_ >> ~d; }~%"
                                     sb-vm:n-fixnum-tag-bits))
             (when (member name '(cons vector symbol fdefn instance))
               (write-cast-operator name c-name lowtag *standard-output*)))
           (output-asm ()
             (format t "/* These offsets are SLOT-OFFSET * N-WORD-BYTES - LOWTAG~%")
             (format t " * so they work directly on tagged addresses. */~2%")
             (dovector (slot slots)
               (format t "#define ~A_~A_OFFSET ~D~%"
                       (c-symbol-name name)
                       (c-symbol-name (sb-vm:slot-name slot))
                       (- (* (sb-vm:slot-offset slot) sb-vm:n-word-bytes) lowtag)))
             (format t "#define ~A_SIZE ~d~%"
                     (string-upcase c-name) (sb-vm:primitive-object-length obj))))
      (when (eq name 'sb-vm::thread)
        (format t "#define THREAD_HEADER_SLOTS ~d~%" sb-vm::thread-header-slots)
        (dovector (x sb-vm::+thread-header-slot-names+)
          (let ((s (package-symbolicate "SB-VM" "THREAD-" x "-SLOT")))
            (format t "#define ~a ~d~%" (c-name (string s)) (symbol-value s))))
        (terpri))
      (format t "#ifdef __ASSEMBLER__~2%")
      (output-asm)
      (format t "~%#else /* __ASSEMBLER__ */~2%")
      (format t "#include ~S~%" (lispobj-dot-h))
      (output-c)
      (format t "~%#endif /* __ASSEMBLER__ */~%"))))

(defun write-structure-object (dd *standard-output* &optional structname)
  (flet ((cstring (designator) (c-name (string-downcase designator))))
    (format t "#ifndef __ASSEMBLER__~2%")
    (format t "#include ~S~%" (lispobj-dot-h))
    (format t "struct ~A {~%" (or structname (cstring (dd-name dd))))
    (format t "    lispobj header; // = word_0_~%")
    ;; "self layout" slots are named '_layout' instead of 'layout' so that
    ;; classoid's expressly declared layout isn't renamed as a special-case.
    #-compact-instance-header (format t "    lispobj _layout;~%")
    ;; Output exactly the number of Lisp words consumed by the structure,
    ;; no more, no less. C code can always compute the padded length from
    ;; the precise length, but the other way doesn't work.
    (let ((names
           (coerce (loop for i from sb-vm:instance-data-start below (dd-length dd)
                         collect (list (format nil "word_~D_" (1+ i))))
                   'vector)))
      (dolist (slot (dd-slots dd))
        (let ((cell (aref names (- (dsd-index slot) sb-vm:instance-data-start)))
              (name (cstring (dsd-name slot))))
          (case (dsd-raw-type slot)
            ((t) (rplaca cell name))
            ;; remind C programmers which slots are untagged
            (sb-vm:signed-word (rplaca cell (format nil "sw_~a" name)))
            (sb-vm:word (rplaca cell (format nil "uw_~a" name)))
            (t (rplacd cell name)))))
      (loop for slot across names
            do (format t "    lispobj ~A;~@[ // ~A~]~%"
                       ;; reserved word
                       (if (string= (car slot) "default") "_default" (car slot))
                       (cdr slot))))
    (format t "};~%")
    (format t "~%#endif /* __ASSEMBLER__ */~2%")))

(defun write-thread-init (stream)
  (dolist (binding sb-vm::per-thread-c-interface-symbols)
    (format stream "INITIALIZE_TLS(~A, ~A);~%"
            (c-symbol-name (if (listp binding) (car binding) binding) "*")
            (let ((val (if (listp binding) (second binding))))
              (if (eq val 't) "LISP_T" val)))))

(defun write-static-symbols (stream)
  (dolist (symbol (cons nil (coerce sb-vm:+static-symbols+ 'list)))
    (format stream "#define ~A LISPOBJ(0x~X)~%"
            ;; FIXME: It would be nice not to need to strip anything
            ;; that doesn't get stripped always by C-SYMBOL-NAME.
            (if (eq symbol 't) "LISP_T" (c-symbol-name symbol "%*.!"))
            (if *static*                ; if we ran GENESIS
              ;; We actually ran GENESIS, use the real value.
              (descriptor-bits (cold-intern symbol))
              (+ sb-vm:nil-value
                 (if symbol (sb-vm:static-symbol-offset symbol) 0)))))
  #+sb-thread
  (dolist (binding sb-vm::per-thread-c-interface-symbols)
    (let* ((symbol (car (ensure-list binding)))
           (c-symbol (c-symbol-name symbol "*")))
      (unless (member symbol sb-vm::+common-static-symbols+)
        ;; So that "#ifdef thing" works, but not as a C expression
        (format stream "#define ~A (*)~%" c-symbol))
      (format stream "#define ~A_tlsindex 0x~X~%"
              c-symbol (ensure-symbol-tls-index symbol))))
  ;; This #define is relative to the start of the fixedobj space to allow heap relocation.
  #+(or immobile-space metaspace)
  (format stream "~@{#define LAYOUT_OF_~A (lispobj)(~A_SPACE_START+0x~x)~%~}"
          "FUNCTION"
          #+metaspace "READ_ONLY" #-metaspace "FIXEDOBJ"
          (- (cold-layout-descriptor-bits 'function)
                        (gspace-byte-address (symbol-value *cold-layout-gspace*))))
  ;; For immobile code, define a constant for the address of the vector of
  ;; C-callable fdefns, and then fdefns in terms of indices to that vector.
  #+immobile-code
  (progn
    (format stream "#define STATIC_FDEFNS LISPOBJ(0x~X)~%"
            (descriptor-bits *c-callable-fdefn-vector*))
    (loop for symbol in sb-vm::+c-callable-fdefns+
          for index from 0
          do (format stream "#define ~A_fdefn ~d~0@*
#define ~A_FDEFN (VECTOR(STATIC_FDEFNS)->data[~d])~%"
                     (c-symbol-name symbol) index)))
  ;; Everybody else can address each fdefn directly.
  #-immobile-code
  (loop for symbol in sb-vm::+c-callable-fdefns+
        for index from 0
        do
    (format stream "#define ~A_FDEFN LISPOBJ(0x~X)~%"
            (c-symbol-name symbol)
            (if *static*                ; if we ran GENESIS
              ;; We actually ran GENESIS, use the real value.
              (descriptor-bits (ensure-cold-fdefn symbol))
              ;; We didn't run GENESIS, so guess at the address.
              (+ sb-vm:nil-value
                 (* (length sb-vm:+static-symbols+)
                    (sb-vm:pad-data-block sb-vm:symbol-size))
                 (* index (sb-vm:pad-data-block sb-vm:fdefn-size)))))))

(defun init-runtime-routines ()
  (dolist (symbol sb-vm::*runtime-asm-routines*)
    (let* ((des (cold-intern symbol :gspace *static*)))
      (cold-set des (make-descriptor (lookup-assembler-reference symbol))))))

(defun write-sc+offset-coding (stream)
  (flet ((write-array (name bytes)
           (format stream "static struct sc_and_offset_byte ~A[] = {~@
                      ~{    {~{ ~2D, ~2D ~}}~^,~%~}~@
                      };~2%"
                   name
                   (mapcar (lambda (byte)
                             (list (byte-size byte) (byte-position byte)))
                           bytes))))
    (format stream "struct sc_and_offset_byte {
    int size;
    int position;
};~2%")
    (write-array "sc_and_offset_sc_number_bytes" sb-c::+sc+offset-scn-bytes+)
    (write-array "sc_and_offset_offset_bytes"    sb-c::+sc+offset-offset-bytes+)))

;;;; writing map file

;;; Write a map file describing the cold load. Some of this
;;; information is subject to change due to relocating GC, but even so
;;; it can be very handy when attempting to troubleshoot the early
;;; stages of cold load.
(defparameter *boilerplate-text* "
(a note about initially undefined function references: These functions
are referred to by code which is installed by GENESIS, but they are not
installed by GENESIS. This is not necessarily a problem; functions can
be defined later, by cold init toplevel forms, or in files compiled and
loaded at warm init, or elsewhere. As long as they are defined before
they are called, everything should be OK. Things are also OK if the
cross-compiler knew their inline definition and used that everywhere
that they were called before the out-of-line definition is installed,
as is fairly common for structure accessors.)")

(defun write-map (*standard-output* &aux (*print-pretty* nil)
                                         (*print-case* :upcase))
  (format t "Table of contents~%")
  (format t "=================~%")
  (let ((sections '("assembler routines" "defined functions" "undefined functions"
                    "classoids" "layouts"
                    "packages" "symbols"
                    "type specifiers"
                    "linkage table" #+sb-thread "TLS map")))
    (dotimes (i (length sections))
      (format t "~4<~@R~>. ~A~%" (1+ i) (nth i sections))))
  (format t "=================~2%")

  (format t "I. assembler routines defined in core image: (base=~x)~2%"
          (descriptor-bits *cold-assembler-obj*))
  (dolist (routine *cold-assembler-routines*)
    (let ((name (car routine)))
      (format t "~8,'0X: ~S~%" (lookup-assembler-reference name) name)))

  (let ((funs nil) (undefs nil))
    (maphash (lambda (name fdefn &aux (fun (cold-fdefn-fun fdefn)))
               (let ((fdefn-bits (descriptor-bits fdefn)))
                 (if (cold-null fun)
                     (push `(,fdefn-bits ,name) undefs)
                     (push `(,fdefn-bits ,(descriptor-bits fun) ,name) funs))))
             *cold-fdefn-objects*)
    (format t "~%~|~%II.A. defined functions (alphabetically):

     FDEFN   FUNCTION  NAME
========== ==========  ====~:{~%~10,'0X ~10,'0X  ~S~}~%"
            (sort (copy-list funs) #'string<
                  :key (lambda (x) (fun-name-block-name (caddr x)))))
    (format t "~%~|~%II.B. defined functions (numerically):

     FDEFN   FUNCTION  NAME
========== ==========  ====~:{~%~10,'0X ~10,'0X  ~S~}~%"
              (sort (copy-list funs) #'< :key #'second))

    (format t "~%~|~A~%
III. initially undefined function references (alphabetically):

     FDEFN  NAME
==========  ====~:{~%~10,'0X  ~S~}~%"
            *boilerplate-text*
            (sort undefs
                    (lambda (a b &aux (pkg-a (sb-xc:package-name (sb-xc:symbol-package a)))
                                      (pkg-b (sb-xc:package-name (sb-xc:symbol-package b))))
                      (cond ((string< pkg-a pkg-b) t)
                            ((string> pkg-a pkg-b) nil)
                            (t (string< a b))))
                    :key (lambda (x) (fun-name-block-name (cadr x))))))

  (format t "~%~|~%IV. classoids:

      CELL   CLASSOID  NAME
========== ==========  ====~%")
  (let ((dumped-classoids))
      (dolist (x (sort (%hash-table-alist *classoid-cells*) #'string< :key #'car))
        (destructuring-bind (name . cell) x
          (format t "~10,'0x ~:[          ~;~:*~10,'0X~]  ~S~%"
                  (descriptor-bits cell)
                  (let ((classoid (read-slot cell :classoid)))
                    (unless (cold-null classoid)
                      (push classoid dumped-classoids)
                      (descriptor-bits classoid)))
                  name)))
      ;; Things sometimes go wrong with dumped classoids, so show a memory dump too
      (terpri)
      (dolist (classoid dumped-classoids)
        (let ((nwords (logand (ash (read-bits-wordindexed classoid 0)
                                   (- sb-vm:instance-length-shift))
                              sb-vm:instance-length-mask)))
          (format t "Classoid @ ~x, ~d words:~%" (descriptor-bits classoid) (1+ nwords))
          (dotimes (i (1+ nwords)) ; include the header word in output
            (format t "~2d: ~10x~%" i (read-bits-wordindexed classoid i)))
          (terpri))))

  (format t "~%~|~%V. layout names:~2%")
  (format t "~28tBitmap  Depth  ID  Name [Length]~%")
  (dolist (pair (sort-cold-layouts))
    (let* ((proxy (cdr pair))
           (descriptor (cold-layout-descriptor proxy))
           (addr (descriptor-bits descriptor)))
      (format t "~10,'0X -> ~10,'0X: ~8d   ~2D ~5D  ~S [~D]~%"
                addr
                #+metaspace (descriptor-bits (->wrapper descriptor))
                #-metaspace "          "
                (cold-layout-bitmap proxy)
                (cold-layout-depthoid proxy)
                (cold-layout-id proxy)
                (car pair)
                (cold-layout-length proxy))))

  (format t "~%~|~%VI. packages:~2%")
  (dolist (pair (sort (%hash-table-alist *cold-package-symbols*) #'<
                      :key (lambda (x) (descriptor-bits (cddr x)))))
    (let ((pkg (cddr pair)))
      (format t "~x = ~a (ID=~d)~%" (descriptor-bits pkg) (car pair)
              (descriptor-fixnum (read-slot pkg :id)))))

  (format t "~%~|~%VII. symbols (numerically):~2%")
  (mapc (lambda (cell)
          (let* ((addr (car cell))
                 (host-sym (cdr cell))
                 (val
                  (unless (or (keywordp host-sym) (null host-sym))
                    (read-bits-wordindexed (cold-intern host-sym)
                                           sb-vm:symbol-value-slot))))
            (format t "~X: ~S~@[ = ~X~]~%" addr host-sym
                    (unless (eql val sb-vm:unbound-marker-widetag) val))))
        (sort (%hash-table-alist *cold-symbols*) #'< :key #'car))

  (format t "~%~|~%VIII. parsed type specifiers:~2%")
  (format t "                        [Hash]~%")
  (let ((sorted
         (sort (%hash-table-alist *host->cold-ctype*) #'<
               :key (lambda (x) (descriptor-bits (cdr x))))))
    (mapc (lambda (cell &aux (host-obj (car cell)) (addr (descriptor-bits (cdr cell))))
            (when (ctype-p host-obj)
              (format t "~X: [~vx] ~A = ~S~%"
                      addr (* 2 sb-vm:n-word-bytes) (read-slot (cdr cell) :%bits)
                      (type-of host-obj) (type-specifier host-obj))))
          sorted)
    (format t "Lists:~%")
    (mapc (lambda (cell &aux (host-obj (car cell)) (addr (descriptor-bits (cdr cell))))
            (when (listp host-obj)
              (format t "~X: (~{#x~X~^ ~})~%" addr
                      (mapcar (lambda (x) (descriptor-bits (gethash x *host->cold-ctype*)))
                              host-obj))))
          sorted))

  (format t "~%~|~%IX. linkage table:~2%")
  (dolist (entry (sort (sb-int:%hash-table-alist *cold-foreign-symbol-table*)
                       #'< :key #'cdr))
    (let ((name (car entry)))
      (format t " ~:[   ~;(D)~] ~8x = ~a~%"
              (listp name)
              (sb-vm::alien-linkage-table-entry-address (cdr entry))
              (car (ensure-list name)))))

  #+sb-thread
  (format t "~%~|~%X. TLS map:~2%~:{~4x ~s~%~}"
          (sort *tls-index-to-symbol* #'< :key #'car))

  (values))

;;;; writing core file

(defun output-gspace (gspace data-page core-file verbose)
  (force-output core-file)
  (let* ((posn (file-position core-file))
         (bytes (cond
                  #+metaspace
                  ((eq (gspace-identifier gspace) read-only-core-space-id)
                   (- sb-vm:read-only-space-end sb-vm:read-only-space-start))
                  (t
                   (* (gspace-free-word-index gspace) sb-vm:n-word-bytes))))
         (page-count (ceiling bytes sb-c:+backend-page-bytes+))
         (total-bytes (* page-count sb-c:+backend-page-bytes+)))

    (file-position core-file (* sb-c:+backend-page-bytes+ (1+ data-page)))
    (when verbose
      (format t "writing ~S byte~:P [~S page~:P] from ~S~%"
              total-bytes page-count gspace))

    ;; Note: It is assumed that the GSPACE allocation routines always
    ;; allocate whole pages (of size +backend-page-bytes+) and that any
    ;; empty gspace between the free pointer and the end of page will
    ;; be zero-filled. This will always be true under Mach on machines
    ;; where the page size is equal. (RT is 4K, PMAX is 4K, Sun 3 is
    ;; 8K).
    (write-bigvec-as-sequence (gspace-data gspace)
                              core-file
                              :end total-bytes
                              :pad-with-zeros t)
    (force-output core-file)
    (file-position core-file posn)

    ;; Write the directory entry.
    (write-words core-file (gspace-identifier gspace) (gspace-free-word-index gspace)
                 data-page (gspace-byte-address gspace) page-count)

    (+ data-page page-count)))

#+gencgc
(defun output-page-table (gspace data-page core-file verbose)
  ;; Write as many PTEs as there are pages used.
  ;; A corefile PTE is { uword_t scan_start_offset; page_words_t words_used; }
  (let* ((data-bytes (* (gspace-free-word-index gspace) sb-vm:n-word-bytes))
         (n-ptes (ceiling data-bytes sb-vm:gencgc-page-bytes))
         (sizeof-corefile-pte (+ sb-vm:n-word-bytes 2))
         (pte-bytes (round-up (* sizeof-corefile-pte n-ptes) sb-vm:n-word-bytes))
         (n-code 0)
         (n-cons 0)
         (n-mixed 0)
         (ptes (make-bigvec)))
    (expand-bigvec ptes pte-bytes)
    (dotimes (page-index n-ptes)
      (let* ((pte-offset (* page-index sizeof-corefile-pte))
             (pte (aref (gspace-page-table gspace) page-index))
             (usage (page-words-used pte))
             (sso (if (plusp usage)
                      (- (* page-index sb-vm:gencgc-page-bytes)
                         (* (page-scan-start pte) sb-vm:n-word-bytes))
                      0))
             (type-bits (if (plusp usage)
                            (ecase (page-type pte)
                              (:code  (incf n-code)  #b111)
                              (:list  (incf n-cons)  #b101)
                              (:mixed (incf n-mixed) #b011))
                            0)))
        (setf (bvref-word-unaligned ptes pte-offset) (logior sso type-bits))
        (setf (bvref-16 ptes (+ pte-offset sb-vm:n-word-bytes)) usage)))
    (when verbose
      (format t "movable dynamic space: ~d + ~d + ~d cons/code/mixed pages~%"
              n-cons n-code n-mixed))
    (force-output core-file)
    (let ((posn (file-position core-file)))
      (file-position core-file (* sb-c:+backend-page-bytes+ (1+ data-page)))
      (write-bigvec-as-sequence ptes core-file :end pte-bytes)
      (force-output core-file)
      (file-position core-file posn))
    (write-words core-file
                 page-table-core-entry-type-code
                 6 ; = number of words in this core header entry
                 sb-vm::gencgc-card-table-index-nbits
                 n-ptes pte-bytes data-page)))

;;; Create a core file created from the cold loaded image. (This is
;;; the "initial core file" because core files could be created later
;;; by executing SAVE-LISP-AND-DIE in a running system, perhaps after we've
;;; added some functionality to the system.)
(defun write-initial-core-file (filename build-id verbose)
  (when verbose
    (let ((*print-length* nil)
          (*print-level* nil))
    (format t "~&SB-XC:*FEATURES* =~&~S~%" sb-xc:*features*))
    (format t "[building initial core file in ~S: ~%" filename))

  (with-open-file (core-file filename :direction :output
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :rename-and-delete)
   (let ((data-page 0))
      ;; Write the magic number.
      (write-words core-file core-magic)

      ;; Write the build ID, which contains a generated string
      ;; plus a suffix identifying a certain configuration of the C compiler.
      (binding* ((build-id (concatenate
                            'string
                            (or build-id
                                (with-open-file (s "output/build-id.inc") (read s)))
                            (if (member :msan sb-xc:*features*) "-msan" "")))
                 ((nwords padding) (ceiling (length build-id) sb-vm:n-word-bytes)))
        (declare (type simple-string build-id))
        ;; Write BUILD-ID-CORE-ENTRY-TYPE-CODE, the length of the header,
        ;; length of the string, then base string chars + maybe padding.
        (write-words core-file build-id-core-entry-type-code
                     (+ 3 nwords) ; 3 = fixed overhead including this word
                     (length build-id))
        (dovector (char build-id) (write-byte (char-code char) core-file))
        (dotimes (j (- padding)) (write-byte #xff core-file)))

      ;; Write the Directory entry header.
      (write-words core-file directory-core-entry-type-code)
      (let ((spaces `(,*static*
                      #+immobile-space ,@`(,*immobile-fixedobj* ,*immobile-text*)
                      ,*dynamic* ,*read-only*)))
        ;; length = (5 words/space) * N spaces + 2 for header.
        (write-words core-file (+ (* (length spaces) 5) 2))
        (dolist (space spaces)
          (setq data-page (output-gspace space data-page core-file verbose))))
      #+gencgc (output-page-table *dynamic* data-page core-file verbose)

      ;; Write the initial function.
      (let ((initial-fun (descriptor-bits (cold-symbol-function '!cold-init))))
        (when verbose (format t "~&/INITIAL-FUN=#X~X~%" initial-fun))
        (write-words core-file initial-fun-core-entry-type-code 3 initial-fun))

      ;; Write the End entry.
      (write-words core-file end-core-entry-type-code 2)))

  (when verbose
    (format t "done]~%")
    (force-output))
  (values))

;;;; the actual GENESIS function

;;; Read the FASL files in OBJECT-FILE-NAMES and produce a Lisp core,
;;; and/or information about a Lisp core, therefrom.
;;;
;;; input file arguments:
;;;   SYMBOL-TABLE-FILE-NAME names a UNIX-style .nm file *with* *any*
;;;     *tab* *characters* *converted* *to* *spaces*. (We push
;;;     responsibility for removing tabs out to the caller it's
;;;     trivial to remove them using UNIX command line tools like
;;;     sed, whereas it's a headache to do it portably in Lisp because
;;;     #\TAB is not a STANDARD-CHAR.) If this file is not supplied,
;;;     a core file cannot be built (but a C header file can be).
;;;
;;; output files arguments (any of which may be NIL to suppress output):
;;;   CORE-FILE-NAME gets a Lisp core.
;;;   C-HEADER-DIR-NAME gets the path in which to place generated headers
;;;   MAP-FILE-NAME gets the name of the textual 'cold-sbcl.map' file
(defun sb-cold:genesis (&key object-file-names tls-init
                             defstruct-descriptions
                             build-id
                             core-file-name c-header-dir-name map-file-name
                             symbol-table-file-name (verbose t))
  (declare (ignorable symbol-table-file-name))

  (when verbose
    (format t
          "~&beginning GENESIS, ~A~%"
          (if core-file-name
            ;; Note: This output summarizing what we're doing is
            ;; somewhat telegraphic in style, not meant to imply that
            ;; we're not e.g. also creating a header file when we
            ;; create a core.
            (format nil "creating core ~S" core-file-name)
            (format nil "creating headers in ~S" c-header-dir-name))))

  (let ((*cold-foreign-symbol-table* (make-hash-table :test 'equal)))

    ;; Prefill some linkage table entries perhaps
    (loop for (name datap) in sb-vm::*linkage-space-predefined-entries*
          do (alien-linkage-table-note-symbol name datap))

    ;; Now that we've successfully read our only input file (by
    ;; loading the symbol table, if any), it's a good time to ensure
    ;; that there'll be someplace for our output files to go when
    ;; we're done.
    (flet ((frob (filename)
             (when filename
               (ensure-directories-exist filename :verbose t))))
      (frob core-file-name)
      (frob map-file-name))

    ;; (This shouldn't matter in normal use, since GENESIS normally
    ;; only runs once in any given Lisp image, but it could reduce
    ;; confusion if we ever experiment with running, tweaking, and
    ;; rerunning genesis interactively.)
    (do-all-symbols (sym)
      (remprop sym 'cold-intern-info))

    (check-spaces)

    (let  ((*load-time-value-counter* 0)
           (*cold-fdefn-objects* (make-hash-table :test 'equal))
           (*cold-symbols* (make-hash-table :test 'eql)) ; integer keys
           (*cold-package-symbols* (make-hash-table :test 'equal)) ; string keys
           (*read-only* (make-gspace :read-only
                                     read-only-core-space-id
                                     sb-vm:read-only-space-start))
           (*static*    (make-gspace :static
                                     static-core-space-id
                                     sb-vm:static-space-start))
           #+immobile-space
           (*immobile-fixedobj* (make-gspace :immobile-fixedobj
                                             immobile-fixedobj-core-space-id
                                             sb-vm:fixedobj-space-start))
           #+immobile-space
           (*immobile-text* (make-gspace :immobile-text
                                         immobile-text-core-space-id
                                         sb-vm:text-space-start
                                         :objects (make-array 20000 :fill-pointer 0 :adjustable t)))
           (*dynamic*   (make-gspace :dynamic
                                     dynamic-core-space-id
                                     #+gencgc sb-vm:dynamic-space-start
                                     #-gencgc sb-vm:dynamic-0-space-start))
           (*nil-descriptor*)
           (*simple-vector-0-descriptor*)
           (*c-callable-fdefn-vector*)
           (*classoid-cells* (make-hash-table :test 'eq))
           (*host->cold-ctype* (make-hash-table))
           (*cold-layouts* (make-hash-table :test 'eq)) ; symbol -> cold-layout
           (*cold-layout-by-addr* (make-hash-table :test 'eql)) ; addr -> cold-layout
           (*tls-index-to-symbol* nil)
           ;; '*COLD-METHODS* is never seen in the target, so does not need
           ;; to adhere to the #\! convention for automatic uninterning.
           (*cold-methods* nil)
           (*!cold-toplevels* nil)
           *cold-static-call-fixups*
           *cold-assembler-routines*
           *cold-assembler-obj*
           *deferred-undefined-tramp-refs*
           (*deferred-known-fun-refs* nil))

      (make-nil-descriptor)
      (setf *simple-vector-0-descriptor* (vector-in-core nil))

      (when core-file-name
        (read-structure-definitions defstruct-descriptions))
      ;; Prepare for cold load.
      (initialize-layouts)
      (initialize-static-space tls-init)
      (cold-set 'sb-c::*!cold-allocation-patch-point* *nil-descriptor*)
      (let ((n (length sb-kernel::*numeric-aspects-v*)))
        (cold-set 'sb-kernel::*numeric-aspects-v*
                  (allocate-vector sb-vm:simple-vector-widetag n n)))
      (cold-set 'sb-kernel::*!initial-ctypes* *nil-descriptor*)

      ;; Load all assembler code
      (flet ((assembler-file-p (name) (tailwise-equal (namestring name) ".assem-obj")))
        (let ((files (remove-if-not #'assembler-file-p object-file-names)))
          ;; There should be exactly 1 assembler file, and 1 code object in it.
          (when files ; But it's present only in 2nd genesis.
            (aver (singleton-p files))
            (cold-load (car files) verbose)))
        (setf object-file-names (remove-if #'assembler-file-p object-file-names)))
      (mapc 'funcall *deferred-undefined-tramp-refs*)
      (makunbound '*deferred-undefined-tramp-refs*)

      (when *cold-assembler-obj*
        (write-wordindexed
         *cold-assembler-obj* sb-vm:code-debug-info-slot
         ;; code-debug-info stores the name->addr hashtable.
         ;; Make sure readonly space doesn't point to dynamic space here.
         (let ((z (make-fixnum-descriptor 0)))
           (cold-cons z z (ecase (gspace-name
                                  (descriptor-gspace *cold-assembler-obj*))
                            ((:read-only :static) *static*)
                            (:immobile-text *dynamic*)))))
        (init-runtime-routines))

      ;; Initialize the *COLD-SYMBOLS* system with the information
      ;; from XC-STRICT-CL.
      (do-external-symbols (symbol (find-package "XC-STRICT-CL"))
        (cold-intern (intern (symbol-name symbol) *cl-package*)
                     :access :external))

      ;; Make LOGICALLY-READONLYIZE no longer a no-op
      (setf (symbol-function 'logically-readonlyize)
            (symbol-function 'set-readonly))

      ;; Cold load.
      (dolist (file-name object-file-names)
        (push (cold-cons :begin-file (string-literal-to-core file-name))
              *!cold-toplevels*)
        (cold-load file-name verbose))

      (sb-cold::check-no-new-cl-symbols)

      (when (and verbose core-file-name)
        (format t "~&; SB-Loader: (~D~@{+~D~}) methods/other~%"
                (reduce #'+ *cold-methods* :key (lambda (x) (length (cdr x))))
                (length *!cold-toplevels*)))

      (cold-set '*!cold-toplevels* (list-to-core (nreverse *!cold-toplevels*)))
      (makunbound '*!cold-toplevels*) ; so no further PUSHes can be done

      ;; Tidy up loose ends left by cold loading. ("Postpare from cold load?")
      (sort-initial-methods)
      (resolve-deferred-known-funs)
      (resolve-static-call-fixups)
      (foreign-symbols-to-core)
      (when core-file-name
        (finish-symbols))
      (finalize-load-time-value-noise)

      ;; Write results to files.
      (when map-file-name
        (let ((all-objects (gspace-objects *dynamic*)))
          (when all-objects
            (with-open-file (stream "output/cold-sbcl.fullmap"
                                    :direction :output
                                    :if-exists :supersede)
              (format t "~&Headered objects: ~d, Conses: ~d~%"
                      (count-if-not #'consp all-objects)
                      (count-if #'consp all-objects))
              ;; Code/data separation causes nonlinear allocations
              (dovector (x (sort all-objects #'<
                                 :key (lambda (x)
                                        (descriptor-bits
                                         (if (consp x) (car x) x)))))
                (let* ((des (if (consp x) (car x) x))
                       (word (read-bits-wordindexed des 0)))
                  (format stream "~x: ~x~@[ ~x~]~%"
                          (logandc2 (descriptor-bits des) sb-vm:lowtag-mask)
                          word
                          (when (and (not (consp x))
                                     (>= (logand word sb-vm:widetag-mask) #x80))
                            (read-bits-wordindexed x 1))))))))
        (with-open-file (stream map-file-name :direction :output :if-exists :supersede)
          (write-map stream)))
      (when core-file-name
        (write-initial-core-file core-file-name build-id verbose))
      (unless c-header-dir-name
        (return-from sb-cold:genesis))
      (let ((filename (format nil "~A/Makefile.features" c-header-dir-name)))
        (ensure-directories-exist filename)
        (with-open-file (stream filename :direction :output :if-exists :supersede)
          (write-makefile-features stream)))
      (write-c-headers c-header-dir-name))))

#+gencgc
(defun write-mark-array-operators (stream &optional (ncards sb-vm::cards-per-page))
  #-soft-card-marks
  (progn
    (aver (= ncards 1))
    (format stream "static inline int cardseq_all_marked_nonsticky(long card) {
    return gc_card_mark[card] == CARD_MARKED;~%}~%")
    (format stream "static inline int cardseq_any_marked(long card) {
    return gc_card_mark[card] != CARD_UNMARKED;~%}~%")
    (format stream "static inline int cardseq_any_sticky_mark(long card) {
    return gc_card_mark[card] == STICKY_MARK;~%}~%"))
  #+soft-card-marks
  ;; In general we have to be wary of wraparound of the card index bits
  ;; - see example in comment above the definition of addr_to_card_index() -
  ;; but it's OK to treat marks as linearly addressable within a page.
  ;; The 'card' argument as supplied to these predicates will be
  ;; a page-aligned card, i.e. the first card for its page.
  (let* ((n-markwords
          ;; This is how many words (of N_WORD_BYTES) of marks there are for the
          ;; cards on a page.
          (cond ((and (= sb-vm:n-word-bytes 8) (= ncards 32)) 4)
                ((and (= sb-vm:n-word-bytes 8) (= ncards 16)) 2)
                ((and (= sb-vm:n-word-bytes 8) (= ncards 8)) 1)
                ((and (= sb-vm:n-word-bytes 4) (= ncards 8)) 2)
                (t (error "bad cards-per-page"))))
         (indices (loop for i below n-markwords collect i)))
    (format stream "static inline int cardseq_all_marked_nonsticky(long card) {
    uword_t* mark = (uword_t*)&gc_card_mark[card];
    return (~{mark[~d]~^ | ~}) == 0;~%}~%" indices)
    (format stream "static inline int cardseq_any_marked(long card) {
    uword_t* mark = (uword_t*)&gc_card_mark[card];
    return (~{mark[~d]~^ & ~}) != (uword_t)-1;~%}~%" indices)
    (format stream "static inline int cardseq_any_sticky_mark(long card) {
    uword_t* mark = (uword_t*)&gc_card_mark[card];
    return ~{word_has_stickymark(mark[~d])~^ || ~};~%}~%" indices)))

(defun write-c-headers (c-header-dir-name)
  (macrolet ((out-to (name &body body) ; write boilerplate and inclusion guard
               `(actually-out-to ,name (lambda (stream) ,@body))))
    (flet ((actually-out-to (name lambda)
             ;; A file gets a '.inc' extension, not '.h' for either or both
             ;; of two reasons:
             ;; - if it isn't self-contained, meaning that in order to #include it,
             ;;   the consumer of it has to know something about which other headers
             ;;   need to be #included first.
             ;; - it is not intended to be directly consumed because any use would
             ;;   typically need to wrap each slot in some small calculation
             ;;   such as native_pointer(), but we don't want to embed the wrapper
             ;;   accessors into the autogenerated header. So there would instead be
             ;;   a "src/runtime/foo.h" which includes "src/runtime/genesis/foo.inc"
             ;; 'thread.h' and 'gc-tables.h' violate the naming convention
             ;; by being non-self-contained.
                 (let* ((extension
                         (cond ((and (stringp name) (position #\. name)) nil)
                               (t ".h")))
                        (inclusion-guardp
                         (string= extension ".h")))
                  (with-open-file (stream (format nil "~A/~A~@[~A~]"
                                                  c-header-dir-name name extension)
                                          :direction :output :if-exists :supersede)
                    (write-boilerplate stream)
                    (when inclusion-guardp
                      (format stream
                              "#ifndef SBCL_GENESIS_~A~%#define SBCL_GENESIS_~:*~A~%"
                              (c-name (string-upcase name))))
                    (funcall lambda stream)
                    (when inclusion-guardp
                      (format stream "#endif~%"))))))
        (out-to "config" (write-config-h stream))
        (out-to "constants" (write-constants-h stream))
        (out-to "regnames" (write-regnames-h stream))
        (out-to "errnames" (write-errnames-h stream))
        (out-to "gc-tables" (sb-vm::write-gc-tables stream))
        #+soft-card-marks
        (out-to "cardmarks" (write-mark-array-operators stream))
        (out-to "tagnames" (write-tagnames-h stream))
        (out-to "print.inc" (write-c-print-dispatch stream))
        (let ((structs (sort (copy-list sb-vm:*primitive-objects*) #'string<
                             :key #'sb-vm:primitive-object-name)))
          (dolist (obj structs)
            (out-to (string-downcase (sb-vm:primitive-object-name obj))
              (write-primitive-object obj stream)))
          (out-to "primitive-objects"
            (dolist (obj structs)
              (format stream "~&#include \"~A.h\"~%"
                      (string-downcase (sb-vm:primitive-object-name obj))))))
        (out-to "layout"
          #-metaspace
          (write-structure-object (wrapper-info (find-layout 'wrapper)) stream
                                  "layout")
          #+metaspace
          (progn
            (write-structure-object (wrapper-info (find-layout 'sb-vm:layout)) stream)
            (write-structure-object (wrapper-info (find-layout 'wrapper)) stream)
            (write-cast-operator 'wrapper "wrapper" sb-vm:instance-pointer-lowtag stream))
          (write-cast-operator 'layout "layout" sb-vm:instance-pointer-lowtag stream))
        ;; For purposes of the C code, cast all hash tables as general_hash_table
        ;; even if they lack the slots for weak tables.
        (out-to "hash-table"
          (write-structure-object (wrapper-info (find-layout 'sb-impl::general-hash-table))
                                  stream "hash_table"))
        (dolist (class '(defstruct-description defstruct-slot-description
                         package
                         sb-vm::arena sb-thread::avlnode sb-thread::mutex
                         sb-c::compiled-debug-info sb-c::compiled-debug-fun))
          (out-to (string-downcase class)
            (write-structure-object (wrapper-info (find-layout class))
                                    stream)))
        (out-to "thread-instance"
          (write-structure-object (wrapper-info (find-layout 'sb-thread::thread))
                                  stream "thread_instance"))
        (with-open-file (stream (format nil "~A/thread-init.inc" c-header-dir-name)
                                :direction :output :if-exists :supersede)
          (write-boilerplate stream) ; no inclusion guard, it's not a ".h" file
          (write-thread-init stream))
        (out-to "static-symbols" (write-static-symbols stream))
        (out-to "sc-offset" (write-sc+offset-coding stream)))))

;;; Invert the action of HOST-CONSTANT-TO-CORE. If STRICTP is given as NIL,
;;; then we can produce a host object even if it is not a faithful rendition.
(defun host-object-from-core (descriptor &optional (strictp t))
  (named-let recurse ((x descriptor))
    (when (symbolp x)
      (return-from recurse x))
    (when (cold-null x)
      (return-from recurse nil))
    (when (is-fixnum-lowtag (descriptor-lowtag x))
      (return-from recurse (descriptor-fixnum x)))
    #+64-bit
    (when (is-other-immediate-lowtag (descriptor-lowtag x))
      (ecase (logand (descriptor-bits x) sb-vm:widetag-mask)
       (#.sb-vm:single-float-widetag
        (return-from recurse
         (unsigned-bits-to-single-float (ash (descriptor-bits x) -32))))))
    (ecase (descriptor-lowtag x)
      (#.sb-vm:instance-pointer-lowtag
       (if strictp (error "Can't invert INSTANCE type") "#<instance>"))
      (#.sb-vm:list-pointer-lowtag
       (cons (recurse (cold-car x)) (recurse (cold-cdr x))))
      (#.sb-vm:fun-pointer-lowtag
       (if strictp
           (error "Can't map cold-fun -> warm-fun")
           #+nil ; FIXME: not done, but only needed for debugging genesis
           (let ((name (read-wordindexed x sb-vm:simple-fun-name-slot)))
             `(function ,(recurse name)))))
      (#.sb-vm:other-pointer-lowtag
       (let ((widetag (descriptor-widetag x)))
         (ecase widetag
           (#.sb-vm:symbol-widetag
            (if strictp
                (warm-symbol x)
                (or (gethash (descriptor-bits x) *cold-symbols*) ; first try
                    (make-symbol (read-cold-symbol-name x)))))
           (#.sb-vm:simple-base-string-widetag (base-string-from-core x))
           (#.sb-vm:simple-vector-widetag (vector-from-core x #'recurse))
           #-64-bit
           (#.sb-vm:single-float-widetag
            (unsigned-bits-to-single-float (read-bits-wordindexed x 1)))
           (#.sb-vm:double-float-widetag
            (double-float-from-core x))
           (#.sb-vm:bignum-widetag (bignum-from-core x))))))))

;;; This is for FOP-SPEC-VECTOR which always supplies 0 for the start
(defun read-n-bytes (stream vector start nbytes)
  (aver (zerop start))
  (let ((start (+ (descriptor-byte-offset vector)
                  (ash sb-vm:vector-data-offset sb-vm:word-shift))))
    (read-into-bigvec (descriptor-mem vector) stream start nbytes)))
