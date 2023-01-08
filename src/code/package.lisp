;;;; that part of the CMU CL package.lisp file which can run on the
;;;; cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; the SYMBOL-HASHSET structure

;;; Packages are implemented using a special kind of hashtable -
;;; the storage is a single vector in which each cell is both key and value.
;;; While previously we also used a parallel vector of 8-bit hash codes,
;;; that turned out to be a pessimization on modern systems, where memory
;;; can be be read faster than it was 27 years ago. (That's when it was
;;; discovered - so it is claimed - that without a comparison for hash,
;;; the majority of time loading fasls was due to paging in symbol names)
;;;
;;; But hardware-based prefetch makes it so that it costs no more to compare
;;; a symbol's hash code than to first compare a smaller hash that doesn't
;;; require dereferencing the symbol, only to use it as the guard condition
;;; to compare the full symbol-hash, and only then the symbol-name.
;;;
;;; In fairness to the former implementation, for technical reasons
;;; it was not possible to use SYMBOL-HASH as the hash value by which
;;; to avoid spurious STRING= calls that would definitely fail.
;;; Prior to change 06ccf13ed3, there were two reasons for that:
;;;   (1) we didn't force interned symbols to have a precomputed hash
;;;   (2) NIL has a strange hash. That was resolved by making any random
;;;       symbol whose name is spelled "NIL" have the identical strange hash
;;;       so that the hash is a pure function of the name's characters.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant package-id-bits 16)
  ;; Give 48 bits to SYMBOL-NAME, which spans 256 TiB of memory.
  ;; Omitting the lowtag bits could span up to 4 PiB because we could left-shift
  ;; and re-tag to read the name.  That seems excessive though. Another viable
  ;; technique would be to store a heap-base-relative pointer, which might be
  ;; needed if dynamic-space is small but at a very high address.
  ;; For now, it seems fine to treat the low 48 bits as a tagged pointer.
  (defconstant symbol-name-bits (- sb-vm:n-word-bits package-id-bits)))
(defconstant +package-id-overflow+ (1- (ash 1 package-id-bits)))
(defconstant +package-id-none+     0)
(defconstant +package-id-lisp+     1)
(defconstant +package-id-keyword+  2)
(defconstant +package-id-user+     3)
(defconstant +package-id-kernel+   4)

(sb-xc:defstruct (symbol-hashset
                  (:conc-name "SYMTBL-")
                  (:predicate nil)
                  (:constructor %make-symbol-hashset
                                (%cells size &aux (free size)))
                  (:copier nil))
  ;; An extra indirection to the symbol vector allows atomically changing the symbols
  ;; and the division magic parameters.
  (%cells (missing-arg) :type (cons t simple-vector))
  (modified nil :type boolean)
  ;; SIZE is roughly related to the number of symbols the client code asked to be
  ;; able to store. We increase that to a prime number, then scale it down to compute
  ;; a rehash trigger level. The only reason we need to remember this number is that
  ;; computing the count of things in the table has to be done by subtracting FREE
  ;; and DELETED from the originally computed size.
  (size (missing-arg) :type index)
  ;; The remaining number of entries that can be made before we have to rehash.
  (free (missing-arg) :type index)
  ;; The number of deleted entries.
  (deleted 0 :type index))

;;;; the PACKAGE structure

(sb-xc:defstruct (package
                  (:constructor %make-package
                                (internal-symbols external-symbols))
                  (:copier nil)
                  (:predicate packagep))
  "the standard structure for the description of a package"
  ;; the name of the package, or NIL for a deleted package
  (%name nil :type (or simple-string null))
  ;; A small integer ID, unless it overflows the global package index
  (id nil :type (or (unsigned-byte #.package-id-bits) null))
  ;; nickname strings
  (%nicknames () :type list)
  ;; packages used by this package
  (%use-list () :type list)
  ;; a simple-vector of the external symbol hashtables for used packages.
  ;; Derived from %USE-LIST, but maintained separately.
  (tables #() :type simple-vector)
  ;; index into TABLES of the table in which an inherited symbol was most
  ;; recently found. On the next %FIND-SYMBOL operation, the indexed table
  ;; is tested first.
  (mru-table-index 0 :type index)
  ;; packages that use this package
  (%used-by-list () :type list)
  ;; SYMBOL-HASHSETs of internal & external symbols
  (internal-symbols nil :type symbol-hashset)
  (external-symbols nil :type symbol-hashset)
  ;; shadowing symbols
  ;; Todo: dynamically changeover to a SYMBOL-HASHSET if list gets long
  (%shadowing-symbols () :type list)
  ;; documentation string for this package
  (doc-string nil :type (or simple-string null))
  ;; package locking
  (%bits 0 :type (and fixnum unsigned-byte))
  (%implementation-packages nil :type list)
  ;; Mapping of local nickname to actual package.
  ;; One vector stores the mapping sorted by string, the other stores it by
  ;; integer nickname ID so that we can binary search in either.
  (%local-nicknames nil :type (or null (cons simple-vector simple-vector)))
  ;; Definition source location
  (source-location nil :type (or null sb-c:definition-source-location)))
(proclaim '(freeze-type symbol-hashset package))

(defconstant +initial-package-bits+ 2) ; for genesis

#-sb-xc-host
(defmacro system-package-p (package) ; SBCL stuff excluding CL and KEYWORD
  `(logbitp 1 (package-%bits ,package)))

(defmacro package-lock (package) `(logbitp 0 (package-%bits ,package)))

(defmacro with-loader-package-names (&body body)
  #+sb-xc-host
  `(progn ,@body)
  #-sb-xc-host
  `(call-with-loader-package-names (lambda () ,@body)))

;;;; IN-PACKAGE
(proclaim '(special *package*))
(sb-xc:defmacro in-package (string-designator)
  (let ((string (string string-designator)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setq *package* (find-undeleted-package-or-lose ,string)))))
