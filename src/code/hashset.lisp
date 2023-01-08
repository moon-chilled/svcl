;;;; Robinhood-hashing weak hashset
;;;; based on https://cs.uwaterloo.ca/research/tr/1986/CS-86-14.pdf

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; See also https://dspace.mit.edu/bitstream/handle/1721.1/130693/1251799942-MIT.pdf
;;;; which has a concurrent implementation

(in-package "SB-IMPL")

(export '(make-hashset hashset-remove hashset-statistics))

(eval-when ()
  (pushnew :hashset-debug sb-xc:*features*))
#+sb-xc-host (define-symbol-macro sb-kernel::*gc-epoch* 0)

(define-load-time-global *hashset-print-statistics* nil)

;;; TODO: teach GC about these hashsets so that we can do something
;;; with address-sensitive (EQ and EQL) hashing.
;;; Then we can implement XSET in terms of this hashset.

(defstruct (robinhood-hashset-storage
            (:constructor !make-hs-storage (cells psl-vector hash-vector hv-inexact))
            (:conc-name hss-)
            (:copier nil)
            (:predicate nil))
  (cells #() :type simple-vector :read-only t)
  ;; We always store a hash-vector even if inexact (see below)
  ;; so that we can avoid calling the comparator on definite mismatches.
  (hash-vector (make-array 0 :element-type '(unsigned-byte 16))
              :type (simple-array (unsigned-byte 16) (*))
              :read-only t)
  ;; Inexact hashes occur when the size of the hash vector exceeds 2^16
  ;; in which case the stored hash has lost precision in terms of how
  ;; many storage bins there are. So we need to call the hash function
  ;; when moving keys around during insert, and when rehashing.
  (hv-inexact nil :type boolean :read-only t)
  (psl-vector (make-array 0 :element-type '(unsigned-byte 8))
              :type (simple-array (unsigned-byte 8) (*))
              :read-only t))

(defstruct (robinhood-hashset (:conc-name hashset-))
  ;; STORAGE can be swapped atomically so that readers can threadsafely read
  ;; all the relevant vectors even if there is a writer.
  ;; It _doesn't_ mean that FIND returns the right answer if writes occur while reading.
  ;; It _does_ mean that the algorithm won't crash.
  (storage (missing-arg) :type robinhood-hashset-storage)
  ;; need to allow maximum number of bits in either host or target fixnum
  ;; because the code runs on either. Will be efficient in the target at least.
  (hash-function #'error :type (sfunction (t) (or fixnum sb-xc:fixnum)))
  (test-function #'error :type function)
  #+hashset-metrics (count-find-hits 0 :type sb-vm:word)
  #+hashset-metrics (count-finds 0 :type sb-vm:word)
  (mutex nil :type (or null #-sb-xc-host sb-thread:mutex)))

;;; The last few elements in the cell vector are metadata.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant hs-storage-trailer-cells 3))
(defmacro hs-cells-capacity (v)
  `(truly-the index (- (length ,v) ,hs-storage-trailer-cells)))
(defmacro hs-cells-mask (v)
  `(truly-the index (- (length ,v) ,(1+ hs-storage-trailer-cells))))
(defmacro hs-cells-gc-epoch (v) `(aref ,v (- (length ,v) 3)))
;;; max probe sequence length for these cells
(defmacro hs-cells-max-psl (v)  `(truly-the fixnum (aref ,v (- (length ,v) 2))))
(defmacro hs-cells-n-avail (v)  `(truly-the fixnum (aref ,v (- (length ,v) 1))))

(defun hashset-cells-load-factor (cells)
  (let* ((cap (hs-cells-capacity cells))
         (used (- cap (hs-cells-n-avail cells))))
    (/ used cap)))
(defconstant +hashset-unused-cell+ 0)
(defmacro hs-chain-terminator-p (val) `(eq ,val 0))

(defun allocate-hashset-storage (capacity weakp)
  (declare (type (unsigned-byte 28) capacity)) ; 256M cells maximum
  (declare (ignorable weakp))
  (declare (sb-c::tlab :system) (inline !make-hs-storage))
  (let* ((len (+ capacity hs-storage-trailer-cells))
         (cells
          #+sb-xc-host (make-array len :initial-element 0)
          #-sb-xc-host
          (let ((type (if weakp
                          (logior (ash sb-vm:vector-weak-flag sb-vm:array-flags-position)
                                  sb-vm:simple-vector-widetag)
                          sb-vm:simple-vector-widetag)))
            (sb-vm::splat (truly-the simple-vector (allocate-vector #+ubsan nil type len len))
                          len 0)))
         (psl-vector (make-array capacity :element-type '(unsigned-byte 8)
                                          :initial-element 0))
         (hash-vector (make-array capacity :element-type '(unsigned-byte 16))))
    (setf (hs-cells-gc-epoch cells) sb-kernel::*gc-epoch*)
    (setf (hs-cells-max-psl cells) 0)
    (setf (hs-cells-n-avail cells) capacity)
    ;; Capacity 65536 is the max for which stored hashes can represent all indices
    ;; into the cell vector. Beyond that, the hashes don't have the required precision.
    ;; I might instead want the hash-vector's type to
    ;;   (OR (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (*)) (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*)))
    ;; but I'm not going there yet.
    (!make-hs-storage cells psl-vector hash-vector (> capacity 65536))))

(defun hashset-weakp (hashset)
  #+sb-xc-host (declare (ignore hashset))
  #-sb-xc-host
  (let* ((storage (hashset-storage hashset))
         (cells (hss-cells storage)))
    (not (zerop (get-header-data cells)))))

(defun make-hashset (estimated-count test-function hash-function &key weakness synchronized)
  (declare (boolean weakness synchronized))
  (let* ((capacity (power-of-two-ceiling (* estimated-count 2)))
         (storage (allocate-hashset-storage capacity weakness)))
    (make-robinhood-hashset :hash-function hash-function
                            :test-function test-function
                            :mutex (if synchronized #-sb-xc-host (sb-thread:make-mutex)
                                                    #+sb-xc-host nil)
                            :storage storage)))

;;; The following terms are synonyms: "probe sequence position", "probe sequence index".
;;; Probe sequence length is the maximum index attained for a given probe sequence.
;;; (Note that probe sequence indices are 1-origin, not 0)
;;; The table's overall max-PSL is the maximum of any PSL for any key in the table.
(defmacro hs-aver-probe-sequence-len (storage index length)
  (declare (ignorable storage index length))
  ;; This assertion is not threadsafe, because an inserter could cause the
  ;; value in the hash-vector to change by backshifting while we're reading.
  ;; Readers are otherwise threadsafe. So this is only for debugging.
  #+nil `(aver (= (aref (hss-psl-vector ,storage) ,index) ,length))
  )

;;; This definition is probably off-by-1 for what you think of as the triangular numbers,
;;; but it does what I want for the hashset probing algorithm:
;;;   f(1)=0, f(2)=1, f(3)=3, f(4)=6, etc
;;; So the first probe occurs at index+0, then "skip 1", "skip 2", "skip 3", ...
(declaim (inline triangular-number))
(defun triangular-number (n)
  (declare (fixnum n))
  (/ (* n (1- n)) 2))

#+nil
(defun hss-backshift (hashset storage current-index desired-psp key hash)
  ;; If we can grab the mutex, then shift this item backwards in its probe sequence.
  ;; If mutex is already owned, or if after acquiring the mutex a change is detected,
  ;; just skip this. If no mutex then back-shift is always OK.
  (let* ((cells (hss-cells storage))
         (desired-index (logand (+ hash (triangular-number desired-psp))
                                (hs-cells-mask cells)))
         (mutex (hashset-mutex hashset)))
    ;; this should probably be WITH-SYSTEM-MUTEX :WAIT NIL
    ;; but it's hard to structure this exceptional case that way
    (when (or (not mutex) (sb-thread::%try-mutex mutex))
      (when (and (eq (hashset-storage hashset) storage)
                 (eq (aref cells current-index) key) ; key is still here
                 (null (aref cells desired-index))) ; tombstone is still there
        #+nil
        (format t "~&Hashset moved ~S from psp ~D to ~D~%"
                key (aref (hss-psl-vector storage) current-index) desired-psp)
        ;; Put this item into the tombstone's location and make this a tombstone
        ;; and *not* +hashset-unused-cell+. Any other probing sequence for a different key
        ;; might have previously tried to claim this cell and could not, so
        ;; went to a later cell in its sequence. Therefore we can't interrupt that
        ;; other (unknown) sequence of probes with a chain-terminating marker.
        ;; Change the key's stored hash to indicate its new, shorter, probe sequence length
        (setf (aref (hss-hash-vector storage) desired-index) (ldb (byte 16 0) hash)
              (aref (hss-psl-vector storage) desired-index) desired-psp
              (aref cells desired-index) key
              (aref cells current-index) nil))
      ;; (hashset-check-invariants hashset)
      (when mutex (sb-thread:release-mutex mutex)))))

;;; Algorithm from Figure 2.1 of the paper.
;;; TODO: there's a simple optimization to avoid calling TRIANGULAR-NUMBER
;;; but I want (for the time being) to be a little closer to the structure
;;; of the reference algorithm, though already this deviates somewhat
;;; substantially by:
;;; - not abstracting out the family of hash functions
;;; - not depending on a 'findposition'
;;; - storing hashes explicitly
(defun hashset-%insert (hashset storage key hash)
  (flet ((triang (n)
           (triangular-number (the (unsigned-byte 8) n))))
    (declare (inline triang))
    (let* ((probe-sequence-pos 0) ; called 'probeposition' in the paper
           (cells (hss-cells storage))
           (max-psl (hs-cells-max-psl cells))
           (psl-vector (hss-psl-vector storage))
           (hash-vector (hss-hash-vector storage))
           (mask (hs-cells-mask cells))
           (original-key key))
      (loop
        (incf probe-sequence-pos)
        (let* ((location (logand (+ hash (triang probe-sequence-pos)) mask))
               (probed-key (aref cells location))
               ;; Get the position in its probe sequence of the key (if any)
               ;; in the slot we're considering taking.
               (probed-key-psp ; named 'recordposition' in the paper
                ;; GC-smashed cells might have a nonzero value in the psl-vector
                ;; which must be disregarded.
                (if (null probed-key) 0 (aref psl-vector location))))
          (when (> probe-sequence-pos probed-key-psp) ; KEY should get the slot
            ;; Get the hash for the key we're stomping on (if any)
            ;; before storing our hash
            (let ((probed-key-hash (aref hash-vector location)))
              (setf (aref hash-vector location) (ldb (byte 16 0) hash)
                    (aref psl-vector location) probe-sequence-pos
                    (aref cells location) key)
              #+hashset-debug
              (when (> probe-sequence-pos max-psl)
                (format *error-output* "hashset-insert(~x): max_psl was ~D is ~D, LF=~F~%"
                        (get-lisp-obj-address cells)
                        max-psl probe-sequence-pos (hashset-cells-load-factor cells)))
              (setf max-psl (max max-psl probe-sequence-pos))
              (when (or (null probed-key) (hs-chain-terminator-p probed-key)) (return))
              (setq probe-sequence-pos probed-key-psp
                    key probed-key
                    hash (if (hss-hv-inexact storage)
                             (funcall (hashset-hash-function hashset) key)
                             probed-key-hash))))))
      (setf (hs-cells-max-psl cells) max-psl)
      original-key)))

(defun hashset-statistics (storage &aux (cells (hss-cells storage))
                                        (psl-vector (hss-psl-vector storage)))
  (flet ((validp (x) (and (not (hs-chain-terminator-p x)) x)))
    (declare (inline validp))
    (do ((i (hs-cells-mask cells) (1- i))
         (histo (make-array (hs-cells-max-psl cells) :initial-element 0))
         (sum-psl 0) ; sum of probe sequence lengths
         (n-keys 0))
        ((< i 0)
         (values (if (plusp n-keys) (/ (float sum-psl) n-keys)) ; avg PSL
                 histo
                 (/ (float n-keys) (hs-cells-capacity cells)))) ; load factor
      (let ((x (aref cells i)))
        (when (validp x)
          (let ((psl (aref psl-vector i)))
            (incf (aref histo (1- psl)))
            (incf sum-psl psl)
            (incf n-keys)))))))

(defun hashset-rehash (hashset count)
  ;; (hashset-check-invariants hashset "begin rehash")
  (flet ((validp (x) (and (not (hs-chain-terminator-p x)) x)))
    (declare (inline validp))
    (let* ((old-storage (hashset-storage hashset))
           (old-cells (hss-cells old-storage))
           (old-capacity (hs-cells-capacity old-cells))
           (count (or count ; count is already known if just GC'ed
                      (count-if #'validp old-cells :end old-capacity)))
           (new-capacity (max 64 (power-of-two-ceiling (* count 2))))
           (new-storage
            (allocate-hashset-storage new-capacity (hashset-weakp hashset))))

      ;; This can be removed
      (when *hashset-print-statistics*
        (multiple-value-bind (mean-psl histo) (hashset-statistics old-storage)
          (let ((*print-length* nil)
                (*print-pretty* nil))
            (format *error-output* "~&rehash: size=(~D -> ~D), ~D avg=~f~%"
                    old-capacity new-capacity histo mean-psl))))

      (do ((i (1- old-capacity) (1- i))
           (n-inserted 0)
           (old-hash-vector (hss-hash-vector old-storage)))
          ((< i 0)
           (decf (hs-cells-n-avail (hss-cells new-storage)) n-inserted))
        (declare (type index-or-minus-1 i) (fixnum n-inserted))
        (let ((key (aref old-cells i)))
          (when (validp key)
            (incf n-inserted)
            ;; Test whether 16-bit hashes are good for the _new_ storage, not the old.
            (hashset-%insert hashset new-storage key
                             (if (hss-hv-inexact new-storage)
                                 (funcall (hashset-hash-function hashset) key)
                                 (aref old-hash-vector i))))))
      ;; Assign the new vectors
      (setf (hashset-storage hashset) new-storage)
      ;; Zap the old key vector
      (fill old-cells 0)
      ;; old vector becomes non-weak, eliminating some GC overhead
      #-sb-xc-host (assign-vector-flags old-cells 0)
      ;; (hashset-check-invariants hashset "end rehash")
      new-storage)))

(defun hashset-insert (hashset key)
  (let* ((storage (hashset-storage hashset))
         (cells (hss-cells storage))
         (capacity (hs-cells-capacity cells))
         (min-avail (ash capacity -2)))
    (cond ((hashset-weakp hashset)
           (flet ((validp (x) (and (not (hs-chain-terminator-p x)) x)))
             (declare (inline validp))
             (let ((current-epoch sb-kernel::*gc-epoch*)
                   (n-live))
               ;; First decide if the table occupancy needs to be recomputed after GC
               (unless (eq (hs-cells-gc-epoch cells) current-epoch)
                 (setf n-live (count-if #'validp cells :end capacity)
                       (hs-cells-n-avail cells) (- capacity n-live)
                       (hs-cells-gc-epoch cells) current-epoch))
               ;; Next decide if rehash should occur (LF exceeds 75%)
               ;; TODO: also do the rehash if 50% of cells are NULL
               (when (< (hs-cells-n-avail cells) min-avail)
                 ;; No big deal if GC culled some more after the counting-
                 ;; REHASH will only copy valid items.
                 (setf storage (hashset-rehash hashset n-live)
                       (hs-cells-gc-epoch (hss-cells storage))
                       current-epoch)))))
          (t
           ;; Just look at the occupancy, which has to be accurate
           (let ((n-avail (hs-cells-n-avail cells)))
             (when (< n-avail min-avail)
               (setf storage (hashset-rehash hashset (- capacity n-avail)))))))
    ;; Finally, insert
    (decf (hs-cells-n-avail cells))
    (hashset-%insert hashset storage key (funcall (hashset-hash-function hashset) key))))

;;; This is the standard open-addressing algorithm using triangular numbers for successive
;;; probes, with early termination based on the observed maximum probe sequence length
;;; as maintained by the insertion algorithm.
(defun hashset-find (hashset key)
  #-sb-xc-host (declare (optimize (sb-c::insert-array-bounds-checks 0)))
  (let* ((storage (hashset-storage hashset))
         (cells (hss-cells storage))
         (hash (funcall (hashset-hash-function hashset) key))
         (mask (hs-cells-mask cells))
         (hash-vector (hss-hash-vector storage))
         (test (hashset-test-function hashset))
         (max-psl-1 (1- (hs-cells-max-psl cells)))
         ;; Filtering on LOWTAG rejects the unused cell marker as well as NIL
         ;; stuffed in by GC, except for keys which are lists. It is assumed
         ;; that the comparator can accept NIL if it accepts lists.
         (lowtag (lowtag-of key))
         (clipped-hash (ldb (byte 16 0) hash))
         (index (logand hash mask))
         (iteration 1))
    (declare (fixnum iteration))
    #+hashset-metrics (incf (hashset-count-finds hashset))
    ;; Unroll by always fetching a pair of keys and hashes.
    ;; Theory suggests that first probing the 2nd choice location should perform better
    ;; than probing the 1st choice first, because the probability density function
    ;; for key K mapping to its Nth-choice probe-sequence-position is more highly
    ;; concentrated at 2 than 1. Despite that I have not observed that to be always true,
    ;; in the unrolled loop, this tactic is performed by checking K2 before K1.
    ;; (It's also not better for subsequent iterations, but it's good enough)
    (loop
      (let* ((next-index (logand (+ index iteration) mask))
             (k1 (aref cells index))
             (k2 (aref cells next-index))
             (h1 (aref hash-vector index))
             (h2 (aref hash-vector next-index)))
        (when (and (= (lowtag-of k2) lowtag) (= h2 clipped-hash) (funcall test k2 key))
          (hs-aver-probe-sequence-len storage next-index (1+ iteration))
          #+hashset-metrics (incf (hashset-count-find-hits hashset))
          (return k2))
        (when (and (= (lowtag-of k1) lowtag) (= h1 clipped-hash) (funcall test k1 key))
          (hs-aver-probe-sequence-len storage index iteration)
          #+hashset-metrics (incf (hashset-count-find-hits hashset))
          (return k1))
        (when (or (hs-chain-terminator-p k1)
                  (hs-chain-terminator-p k2)
                  ;; We've tested through ITERATION+1. If that is >= MAX-PSL we're done.
                  ;; That's the same as checking ITERATION >= (1- MAX-PSL)
                  (>= iteration max-psl-1))
          (return nil))
        ;; this visits every cell.
        ;; Proof at https://fgiesen.wordpress.com/2015/02/22/triangular-numbers-mod-2n/
        (setq index (logand (+ next-index iteration 1) mask))
        (incf (truly-the fixnum iteration) 2)))))

;;; This is basically FIND, storing NIL in the cell if found.
;;; Caller is responsible for guarding with the hashset-mutex if applicable.
;;; Return T if KEY was present, NIL otherwise.
(defun hashset-remove (hashset key &aux (storage (hashset-storage hashset))
                                        (cells (hss-cells storage))
                                        (test (hashset-test-function hashset)))
  #-sb-xc-host (declare (optimize (sb-c::insert-array-bounds-checks 0)))
  (let* ((mask (hs-cells-mask cells))
         (index (logand (funcall (hashset-hash-function hashset) key) mask))
         (max-psl (hs-cells-max-psl cells))
         (iteration 1))
    (declare (fixnum iteration))
    (loop
      (let ((probed-value (aref cells index)))
        (when (hs-chain-terminator-p probed-value) ; end of probe sequence
          (return nil))
        (when (and probed-value (funcall test probed-value key))
          (hs-aver-probe-sequence-len storage index iteration)
          (setf (aref cells index) nil) ; It's that simple
          (return t))
        (if (>= iteration max-psl) (return nil))
        (setq index (logand (+ index iteration) mask))
        (incf iteration)))))

;;; Search for KEY in HASHSET and if found return the matching entry.
;;; If not found, call COPIER on KEY and insert that.
;;; This operation allows the supplied key to be dynamic-extent or possibly
;;; not in GC-managed memory.
;;; The hashset is single-reader safe without the mutex, but you might or might not
;;; get a hit even if KEY is logically present, because a concurrent INSERT is
;;; allowed to reorder the physical storage. So we rely on the double-check pattern.
(declaim (ftype (sfunction (robinhood-hashset t function) t) hashset-insert-if-absent))
(defun hashset-insert-if-absent (hashset key copier)
  (or (hashset-find hashset key)
      (if (not (hashset-mutex hashset))
          (hashset-insert hashset (funcall copier key))
          (with-system-mutex ((hashset-mutex hashset))
            (or (hashset-find hashset key)
                (hashset-insert hashset (funcall copier key)))))))

(defun hashset-count (hashset &aux (cells (hss-cells (hashset-storage hashset))))
  (count-if (lambda (x) (and x (not (hs-chain-terminator-p x))))
            cells :end (hs-cells-capacity cells)))
(defmethod print-object ((self robinhood-hashset) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (let ((cells (hss-cells (hashset-storage self))))
      (format stream "~S ~D/~D keys, psl=~D"
              (%fun-name (hashset-test-function self))
              (hashset-count self)
              (hs-cells-capacity cells)
              (hs-cells-max-psl cells)))))

;;; We need to avoid dependence on the host's SXHASH function when producing
;;; hash values for hashset lookup, so that elements end up in an order
;;; that is host-lisp-insensitive. But the logic for our SXHASH is used here
;;; and also in the compiler transforms (which defines the ordinary function).
;;; Spewing it all over would lead invariably to getting it wrong,
;;; so we define the expression that the compiler will use, and then
;;; we paste the expressions into the cross-compilers emulation of our SXHASH.

(eval-when (#+sb-xc-host :compile-toplevel :load-toplevel :execute)
(defun sxhash-fixnum-xform (x)
  (let ((c (logand 1193941380939624010 most-positive-fixnum)))
    ;; shift by -1 to get sign bit into hash
    `(logand (logxor (ash ,x 4) (ash ,x -1) ,c) most-positive-fixnum)))

(defun sxhash-single-float-xform (x)
  `(let ((bits (logand (single-float-bits ,x) ,(1- (ash 1 32)))))
     (logxor 66194023
             (sxhash (the sb-xc:fixnum
                          (logand most-positive-fixnum
                                  (logxor bits (ash bits -7))))))))

(defun sxhash-double-float-xform (x)
  #-64-bit
  `(let* ((hi (logand (double-float-high-bits ,x) ,(1- (ash 1 32))))
          (lo (double-float-low-bits ,x))
          (hilo (logxor hi lo)))
     (logxor 475038542
             (sxhash (the fixnum
                          (logand most-positive-fixnum
                                  (logxor hilo
                                          (ash hilo -7)))))))
  ;; Treat double-float essentially the same as a fixnum if words are 64 bits.
  #+64-bit
  `(let ((x (double-float-bits ,x)))
     ;; ensure we mix the sign bit into the hash
     (logand (logxor (ash x 4)
                     (ash x (- (1+ sb-vm:n-fixnum-tag-bits)))
                     ;; logical negation of magic constant ensures
                     ;; that 0.0d0 hashes to something other than what
                     ;; the fixnum 0 hashes to (as tested in
                     ;; hash.impure.lisp)
                     #.(logandc1 1193941380939624010 most-positive-fixnum))
             most-positive-fixnum)))
)

#+sb-xc-host
(progn
  (defvar *sxhash-crosscheck* nil)
  (defun sb-xc:sxhash (obj)
    (let ((answer
           (etypecase obj ; croak on anything but these
            (symbol (sb-impl::symbol-name-hash obj))
            (sb-xc:fixnum #.(sxhash-fixnum-xform 'obj))
            (single-float #.(sxhash-single-float-xform 'obj))
            (double-float #.(sxhash-double-float-xform 'obj)))))
      ;; Symbol hashes are verified by CHECK-HASH-SLOT in !PACKAGE-COLD-INIT
      (unless (symbolp obj)
        (push (cons obj answer) *sxhash-crosscheck*))
      answer)))
