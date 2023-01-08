;;;; This file implements some optimisations at the IR2 level.
;;;; Currently, the pass converts branches to conditional moves,
;;;; deletes subsequently dead blocks and then reoptimizes jumps.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; We track pred/succ info at the IR2-block level, extrapolating
;;; most of the data from IR1 to initialise.
(declaim (type hash-table *2block-info*))
;;; For blocks it's a cons with (pred . succ)
;;; For labels it maps to the label block
(defvar *2block-info*)
(defmacro ir2block-predecessors (x) `(car (gethash (the ir2-block ,x) *2block-info*)))
(defmacro ir2block-successors (x) `(cdr (gethash (the ir2-block ,x) *2block-info*)))

(defun initialize-ir2-blocks-flow-info (component)
  (labels ((block-last-2block (block)
             (declare (type cblock block))
             (do ((2block (block-info block)
                          (ir2-block-next 2block)))
                 (nil)
               (let ((next (ir2-block-next 2block)))
                 (when (or (null next)
                           (neq block (ir2-block-block next)))
                   (return 2block)))))
           (link-2blocks (pred succ)
             (declare (type ir2-block pred succ))
             (pushnew pred (car (ensure-gethash succ *2block-info*
                                                (cons '() '()))))
             (pushnew succ (cdr (ensure-gethash pred *2block-info*
                                                (cons '() '()))))))
    (do-blocks (block component :both)
      (let ((succ (block-succ block))
            (last (block-last-2block block)))
        (dolist (succ succ)
          (link-2blocks last (block-info succ)))
        (do ((2block (block-info block)
                     (ir2-block-next 2block)))
            ((eq 2block last))
          (link-2blocks 2block (ir2-block-next 2block)))))
    (do-ir2-blocks (2block component)
      (awhen (ir2-block-%label 2block)
        (setf (gethash it *2block-info*) 2block)))))

(defun update-block-succ (2block succ)
  (declare (type ir2-block 2block)
           (type list succ))
  (flet ((blockify (x)
           (etypecase x
             (label (or (gethash x *2block-info*)
                        (error "Unknown label: ~S" x)))
             (ir2-block x))))
    (let ((succ (mapcar #'blockify succ))
          (info (gethash 2block *2block-info*)))
      (dolist (old (cdr info))
        (let ((info (gethash old *2block-info*)))
          (setf (car info)
                (remove 2block (car info)))))
      (setf (cdr info) succ)
      (dolist (new succ)
        (pushnew 2block (ir2block-predecessors new))))))

;;;; Conditional move insertion support code
(defun move-value-target (2block)
  (declare (type ir2-block 2block))
  (let* ((first  (or (ir2-block-start-vop 2block)
                     (return-from move-value-target)))
         (second (vop-next first)))
    (when (and (memq (vop-name first) '(move sb-vm::double-move
                                        sb-vm::single-move))
               (or (not second)
                   (eq (vop-name second) 'branch)))
      (values (tn-ref-tn (vop-args first))
              (tn-ref-tn (vop-results first))))))

;; A conditional jump may be converted to a conditional move if
;; both branches move a value to the same TN and then continue
;; execution in the same successor block.
;;
;; The label argument is used to return possible value TNs in
;; the right order (first TN if the branch would have been taken,
;; second otherwise)
(defun cmovp (label a b)
  (declare (type label label)
           (type cblock a b))
  (let ((a2 (block-info a))
        (b2 (block-info b)))
   (cond ((eq label (ir2-block-%label a2)))
         ((eq label (ir2-block-%label b2))
          (rotatef a b)
          (rotatef a2 b2))
         (t (return-from cmovp)))
   (let ((succ-a (block-succ a))
         (succ-b (block-succ b)))
     (cond ((and (singleton-p succ-a)
                 (singleton-p succ-b)
                 (eq (car succ-a) (car succ-b))
                 (singleton-p (block-pred a))
                 (singleton-p (block-pred b)))
            (multiple-value-bind (value-a target)
                (move-value-target a2)
              (multiple-value-bind (value-b targetp)
                  (move-value-target b2)
                (and value-a value-b (eq target targetp)
                     (values (block-label (car succ-a))
                             target value-a value-b)))))
           ;; A branch jumping over a move.
           ((and (singleton-p succ-a)
                 (singleton-p (block-pred a))
                 (equal (car succ-a)
                        b))
            (multiple-value-bind (value target)
                (move-value-target a2)
              (when value
                (values (block-label b)
                        target value target))))
           ((and (singleton-p succ-b)
                 (singleton-p (block-pred b))
                 (equal (car succ-b)
                        a))
            (multiple-value-bind (value target)
                (move-value-target b2)
              (when value
                (values (block-label a)
                        target target value))))))))

#-x86-64
(defun sb-vm::computable-from-flags-p (res x y flags)
  (declare (ignorable res x y flags))
  nil)

;; To convert a branch to a conditional move:
;; 1. Convert both possible values to the chosen common representation
;; 2. Execute the conditional VOP
;; 3. Execute the chosen conditional move VOP
;; 4. Convert the result from the common representation
;; 5. Jump to the successor
(defun convert-one-cmov (cmove-vop
                         value-if value-else
                         res flags
                         label vop node 2block
                         &aux (prev (vop-prev vop)))
  (delete-vop vop)
  (let ((last (ir2-block-last-vop 2block)))
    (when (and last
               (eq (vop-name last) 'branch))
      (delete-vop last)))
  (cond
    ((and (constant-tn-p value-if)
          (constant-tn-p value-else)
          (sb-vm::computable-from-flags-p
           res (tn-value value-if) (tn-value value-else) flags))
     (emit-template node 2block (template-or-lose 'sb-vm::compute-from-flags)
                    (reference-tn-list (list value-if value-else) nil)
                    (reference-tn res t)
                    (list flags)))
    (t
     (when (and prev
                (eq (vop-name prev) 'if-eq)
                (constant-tn-p value-if))
       ;; Most of the time this means:
       ;; if X is already NIL don't load it again.
       (let* ((args (vop-args prev))
              (x-tn (tn-ref-tn args))
              (test (tn-ref-tn (tn-ref-across args))))
         (when (and (constant-tn-p test)
                    (equal (tn-value value-if)
                           (tn-value test))
                    (eq (tn-primitive-type x-tn)
                        (tn-primitive-type res)))
           (setf value-if x-tn))))
     (emit-template node 2block (template-or-lose cmove-vop)
                    (reference-tn-list (list value-if value-else)
                                       nil)
                    (reference-tn res t)
                    (list flags))))

  (vop branch node 2block label)
  (update-block-succ 2block (list label)))

(defun maybe-convert-one-cmov (vop)
  ;; The test and branch-if may be split between two IR1 blocks
  ;; due to cleanups, can't use bloc-succ of the ir2-block-block
  (let* ((node (vop-node vop))
         (succ (block-succ (node-block node)))
         (a    (first succ))
         (b    (second succ)))
    (destructuring-bind (jump-target not-p flags) (vop-codegen-info vop)
      (multiple-value-bind (label target value-a value-b)
          (cmovp jump-target a b)
        (unless label
          (return-from maybe-convert-one-cmov))
        (multiple-value-bind (cmove-vop) (convert-conditional-move-p target)
          (when cmove-vop
            (when not-p
              (rotatef value-a value-b))
            (convert-one-cmov cmove-vop
                              value-a value-b target
                              flags
                              label vop node (vop-block vop))
            t))))))

(defun delete-unused-ir2-blocks (component)
  (declare (type component component))
  (let ((live-2blocks (make-hash-table :test #'eq)))
    ;; The liveness algorithm is a straightforward DFS depending on correctness
    ;; of successor links from any reachable block. Unreached blocks could have junk
    ;; in the successor and predecessor links, but it would nice if that didn't
    ;; happen, as junk makes it hard to understand the IR2 flow graph.
    ;; Mutators should try to keep things tidy.
    (labels ((mark-2block (2block)
               (declare (type ir2-block 2block))
               (when (gethash 2block live-2blocks)
                 (return-from mark-2block))
               (setf (gethash 2block live-2blocks) t)
               (map nil #'mark-2block (cdr (gethash 2block *2block-info*)))))
      (mark-2block (block-info (component-head component))))

    (flet ((delete-2block (2block)
             (declare (type ir2-block 2block))
             (do ((vop (ir2-block-start-vop 2block) (vop-next vop)))
                 ((null vop))
               (delete-vop vop))))
      (do-ir2-blocks (2block component (values))
        (unless (gethash 2block live-2blocks)
          (delete-2block 2block))))))

(defun delete-fall-through-jumps (component)
  (flet ((jump-falls-through-p (2block)
           (let* ((last   (or (ir2-block-last-vop 2block)
                              (return-from jump-falls-through-p nil)))
                  (target (first (vop-codegen-info last))))
             (unless (eq (vop-name last) 'branch)
               (return-from jump-falls-through-p nil))
             (do ((2block (ir2-block-next 2block)
                    (ir2-block-next 2block)))
                 ((null 2block) nil)
               (cond ((ir2-block-%trampoline-label 2block)
                      (return nil))
                     ((eq target (ir2-block-%label 2block))
                      (return t))
                     ((ir2-block-start-vop 2block)
                      (return nil)))))))
    ;; Walk the blocks in reverse emission order to catch jumps
    ;; that fall-through only once another jump is deleted
    (let ((last-2block
           (do-ir2-blocks (2block component (aver nil))
             (when (null (ir2-block-next 2block))
               (return 2block)))))
      (do ((2block last-2block
             (ir2-block-prev 2block)))
          ((null 2block)
             (values))
        (when (jump-falls-through-p 2block)
          (delete-vop (ir2-block-last-vop 2block)))))))

(defun delete-no-op-vops (component)
  (do-ir2-blocks (block component)
    (do ((vop (ir2-block-start-vop block) (vop-next vop)))
        ((null vop))
      (let ((args (vop-args vop))
            (results (vop-results vop)))
       (case (vop-name vop)
         ((move sb-vm::sap-move)
          (let ((x (tn-ref-tn args))
                (y (tn-ref-tn results)))
            (when (location= x y)
              (delete-vop vop)
              ;; Deleting the copy may make it look like that register
              ;; is not used anywhere else and some optimizations,
              ;; like combine-instructions, may incorrectly trigger.
              ;; FIXME: these tn-refs never go away, so things like
              ;; combine-instructions should use a different mechanism
              ;; for checking writes/reads.
              (let ((x-reads (tn-reads x))
                    (x-writes (tn-writes x)))
                (when (tn-reads y)
                  (reference-tn x nil))
                (when (tn-writes y)
                  (reference-tn x t))
                (when x-reads
                  (reference-tn y nil))
                (when x-writes
                  (reference-tn y t)))))))))))

;;; Unchain BRANCHes that jump to a BRANCH.
;;; Remove BRANCHes that are jumped over by BRANCH-IF
;;; Should be run after DELETE-NO-OP-VOPS, otherwise the empty moves
;;; will interfere.
(defun ir2-optimize-jumps (component)
  (flet ((start-vop (block)
           (do ((block block (ir2-block-next block)))
               ((or
                 (null block)
                 (not (singleton-p (ir2block-predecessors block)))) nil)
             (let ((vop (ir2-block-start-vop block)))
               (when vop
                 (if (eq (vop-name vop) 'sb-c:note-environment-start)
                     (let ((next (vop-next vop)))
                       (when next
                         (return next)))
                     (return vop))))))
         (delete-chain (block)
           (do ((block block (ir2-block-next block)))
               ((null block) nil)
             ;; Just pop any block, *2block-info* is only used here
             ;; and it just needs to know the number of predecessors,
             ;; not their identity.
             (pop (ir2block-predecessors block))
             (if (ir2block-predecessors block)
                 (return)
                 (let ((vop (ir2-block-start-vop block)))
                   (when vop
                     (when (eq (vop-name vop) 'sb-c:note-environment-start)
                       (setf vop (vop-next vop)))
                     (when vop
                       (aver (eq (vop-name vop) 'branch))
                       (delete-vop vop)
                       (return t)))))))
         (next-label (block)
           (do ((block (ir2-block-next block)
                  (ir2-block-next block)))
               ((null block) nil)
             (let ((label (or (ir2-block-%trampoline-label block)
                              (ir2-block-%label block))))
               (cond (label
                      (return label))
                     ((ir2-block-start-vop block)
                      (return nil)))))))
    (labels ((unchain-jumps (vop)
               ;; Handle any branching vop except a multiway branch
               (setf (first (vop-codegen-info vop))
                     (follow-jumps (first (vop-codegen-info vop)))))
             (follow-jumps (target-label &optional (delete t))
               (declare (type label target-label))
               (let* ((target-block (gethash target-label *2block-info*))
                      (target-vop (start-vop target-block)))
                 (cond ((and target-vop
                             (eq (vop-name target-vop) 'branch)
                             (neq (first (vop-codegen-info target-vop))
                                  target-label))
                        (when delete
                          (setf delete (delete-chain target-block)))
                        (follow-jumps (first (vop-codegen-info target-vop)) delete))
                       (t
                        target-label))))
             (remove-jump-overs (branch-if branch)
               ;; Turn BRANCH-IF #<L1>, BRANCH #<L2>, L1:
               ;; into BRANCH-IF[NOT] L2
               (when (and branch
                          (eq (vop-name branch) 'branch))
                 (let* ((branch-if-info (vop-codegen-info branch-if))
                        (branch-if-target (first branch-if-info))
                        (branch-target (first (vop-codegen-info branch)))
                        (next (next-label (vop-block branch))))
                   (when (eq branch-if-target next)
                     (setf (first branch-if-info) branch-target)
                     ;; Reverse the condition
                     (setf (second branch-if-info) (not (second branch-if-info)))
                     (delete-vop branch)))))
             (conditional-p (vop)
               (let ((info (vop-info vop)))
                 (eq (vop-info-result-types info) :conditional))))
      ;; Pass 1: conditional | unconditional jump to an unconditional jump
      ;; should take the label of the latter.
      (do-ir2-blocks (block component)
        (let ((last (ir2-block-last-vop block)))
          (when last
            (case (vop-name last)
              (branch
               (unchain-jumps last)
               ;; A block may end up having BRANCH-IF + BRANCH after converting an IF.
               ;; Multiway can't coexist with any other branch preceding or following
               ;; in the block, so we don't have to check for that, just a BRANCH-IF.
               (let ((prev (vop-prev last)))
                 (when (and prev
                            (or (eq (vop-name prev) 'branch-if)
                                (conditional-p prev)))
                   (unchain-jumps prev))))
              (branch-if
               (unchain-jumps last))
              (multiway-branch-if-eq
               ;; codegen-info = (labels else-label key-type keys original-comparator)
               (let ((info (vop-codegen-info last)))
                 ;; Don't delete the branches when they reach zero
                 ;; predecessors as multiway-branch-if-eq inserts
                 ;; extra jumps which are not in the original ir2
                 ;; and do not correspond to any predecessors.
                 (setf (car info) (mapcar (lambda (x)
                                            (follow-jumps x nil))
                                          (car info))
                       (cadr info) (follow-jumps (cadr info) nil))))
              (t
               (when (conditional-p last)
                 (unchain-jumps last)))))))
      ;; Pass 2
      ;; Need to unchain the jumps before handling jump-overs,
      ;; otherwise the BRANCH over which BRANCH-IF jumps may be a
      ;; target of some other BRANCH
      (do-ir2-blocks (block component)
        (let ((last (ir2-block-last-vop block)))
          (case (and last (vop-name last))
            (branch-if
             (remove-jump-overs last
                                (start-vop (ir2-block-next block))))
            (branch
             ;; A block may end up having BRANCH-IF + BRANCH after coverting an IF
             (let ((prev (vop-prev last)))
               (when (and prev
                          (or (eq (vop-name prev) 'branch-if)
                              (conditional-p prev)))
                 (remove-jump-overs prev last))))
            (t
             (when (and last
                        (conditional-p last))
               (remove-jump-overs last
                                  (start-vop (ir2-block-next block))))))))
      (delete-fall-through-jumps component))))

(defun next-vop (vop)
  (or (vop-next vop)
      (do ((2block (ir2-block-next (vop-block vop))
                   (ir2-block-next 2block)))
          ((null 2block) nil)
        (cond ((or (ir2-block-%trampoline-label 2block)
                   (ir2-block-%label 2block))
               (return))
              ((ir2-block-start-vop 2block)
               (return (ir2-block-start-vop 2block)))))))

(defun prev-vop (vop)
  (or (vop-prev vop)
      (do* ((2block (vop-block vop) prev)
            (prev (ir2-block-prev 2block)
                  (ir2-block-prev 2block)))
           ((null prev) nil)
        (cond ((or (ir2-block-%trampoline-label 2block)
                   (ir2-block-%label 2block))
               (return))
              ((ir2-block-last-vop prev)
               (return (ir2-block-last-vop prev)))))))

(defun immediate-cmp-templates (fun &optional (constants t))
  (let* ((signed (mapcar #'primitive-type-or-lose
                         (cddr (assoc 'sb-vm::signed-num *backend-primitive-type-aliases*))))
         (unsigned (mapcar #'primitive-type-or-lose
                           (cddr (assoc 'sb-vm::unsigned-num *backend-primitive-type-aliases*))))
         (primitive-types (list* (primitive-type-or-lose 'character)
                                 #-x86 ;; i387 is weird
                                 (primitive-type-or-lose 'double-float)
                                 #-x86
                                 (primitive-type-or-lose 'single-float)
                                 (append signed unsigned))))
    (loop for template in (fun-info-templates (fun-info-or-lose fun))
          for types = (template-arg-types template)
          when (and (typep (template-result-types template) '(cons (eql :conditional)))
                    (loop for type in types
                          always (and (consp type)
                                      (case (car type)
                                        (:or
                                         (loop for type in (cdr type)
                                               always (memq type primitive-types)))
                                        (:constant constants))))
                    ;; signed-unsigned VOPs have more than just a single CMP instruction
                    (not (and (= (length types) 2)
                              (typep (first types) '(cons (eql :or)))
                              (typep (second types) '(cons (eql :or)))
                              (or (and (equal (cdr (first types)) signed)
                                       (equal (cdr (second types)) unsigned))
                                  (and (equal (cdr (first types)) unsigned)
                                       (equal (cdr (second types)) signed))))))
          collect (template-name template))))

(define-load-time-global *comparison-vops*
    (append (immediate-cmp-templates 'eq)
            (immediate-cmp-templates '=)
            (immediate-cmp-templates '>)
            (immediate-cmp-templates '<)
            (immediate-cmp-templates 'char<)
            (immediate-cmp-templates 'char>)
            (immediate-cmp-templates 'char=)))

(define-load-time-global *commutative-comparison-vops*
    (append (immediate-cmp-templates 'eq nil)
            (immediate-cmp-templates 'char= nil)
            (immediate-cmp-templates '= nil)))

(defun vop-arg-list (vop)
  (let ((args (loop for arg = (vop-args vop) then (tn-ref-across arg)
                    while arg
                    collect (tn-ref-tn arg))))
    (if (vop-codegen-info vop)
        (nconc args (vop-codegen-info vop))
        args)))

(defun vop-args-equal (vop1 vop2 &optional reverse)
  (let* ((args1 (vop-arg-list vop1))
         (args2 (vop-arg-list vop2)))
    (equal (if reverse
               (reverse args1)
               args1)
           args2)))

;; cmov conversion needs to know the SCs
(defoptimizer (vop-optimize branch-if select-representations) (branch-if)
  (cond ((maybe-convert-one-cmov branch-if)
         nil)
        #+(or arm arm64 x86 x86-64)
        (t
         ;; Turn CMP X,Y BRANCH-IF M CMP X,Y BRANCH-IF N
         ;; into CMP X,Y BRANCH-IF M BRANCH-IF N
         ;; Run it after SELECT-REPRESENTATIONS, after CMOVs are
         ;; converted and :after-sc-selection flags are resolved.
         ;; While it's portable the VOPs are not validated for
         ;; compatibility on other backends yet.
         (let ((prev (vop-prev branch-if)))
           (when (and prev
                      (memq (vop-name prev) *comparison-vops*))
             (let ((next (next-vop branch-if))
                   transpose)
               (when (and next
                          (memq (vop-name next) *comparison-vops*)
                          (or (vop-args-equal prev next)
                              (and (or (setf transpose
                                             (memq (vop-name prev) *commutative-comparison-vops*))
                                       (memq (vop-name next) *commutative-comparison-vops*))
                                   (vop-args-equal prev next t))))
                 (when transpose
                   ;; Could flip the flags for non-commutative operations
                   (loop for tn-ref = (vop-args prev) then (tn-ref-across tn-ref)
                         for arg in (nreverse (vop-arg-list prev))
                         do (change-tn-ref-tn tn-ref arg)))
                 (setf (sb-assem::label-comment (car (vop-codegen-info branch-if)))
                       :merged-ifs)
                 (delete-vop next))))))))

(defun next-start-vop (block)
  (loop thereis (ir2-block-start-vop block)
        while (and (= (length (ir2block-predecessors block)) 1)
                   (setf block (ir2-block-next block)))))

(defun branch-destination (branch &optional (true t))
  (unless (vop-codegen-info branch)
    (let ((next (vop-next branch)))
      (if (and next
               (eq (vop-name next) 'branch-if))
          (setf branch next)
          (return-from branch-destination))))
  (destructuring-bind (label not-p &rest rest) (vop-codegen-info branch)
    (declare (ignore rest))
    (if (eq not-p true)
        (if (eq branch (ir2-block-last-vop (vop-block branch)))
            (next-start-vop (ir2-block-next (vop-block branch)))
            (let ((next (next-vop branch)))
              (and (eq (vop-name next) 'branch)
                   (next-start-vop (gethash (car (vop-codegen-info next)) *2block-info*)))))
        (let ((dest (gethash label *2block-info*)))
          (when dest
            (next-start-vop dest))))))

;;; Replace (BOUNDP X) + (BRANCH-IF) + {(SYMBOL-VALUE X) | anything}
;;; by (FAST-SYMBOL-VALUE X) + (UNBOUND-MARKER-P) + BRANCH-IF
;;; and delete SYMBOL-VALUE, with the result of FAST-SYMBOL-VALUE flowing
;;; into the same TN as symbol-value's result.
;;; This is a valid substitution on any of the architectures, but
;;; it requires that UNBOUND-MARKER-P return its result in a flag,
;;; so presently is enabled only for x86-64.
#+x86-64
(defoptimizer (vop-optimize boundp) (vop)
  (let ((sym (tn-ref-tn (vop-args vop)))
        (next (vop-next vop)))
    (unless (and next (eq (vop-name next) 'branch-if))
      (return-from vop-optimize-boundp-optimizer nil))
    ;; Only replace if the BOUNDP=T consequent starts with SYMBOL-VALUE so that
    ;; a bad example such as (IF (BOUNDP '*FOO*) (PRINT 'HI) *FOO*)
    ;; - which has SYMBOL-VALUE as the wrong consequent of the IF - is unaffected.
    (let* ((successors (ir2block-successors (vop-block next)))
           (info (vop-codegen-info next))
           (label-block (gethash (car info) *2block-info*))
           (symbol-value-vop
            (ir2-block-start-vop
             (cond ((eq (second info) nil) label-block) ; not negated test.
                   ;; When negated, the BOUNDP case goes to the "other" successor
                   ;; block, i.e. whichever is not started by the #<label>.
                   ((eq label-block (first successors)) (second successors))
                   (t (first successors))))))
      (when (and symbol-value-vop
                 (or (eq (vop-name symbol-value-vop) 'symbol-value)
                     ;; Expect SYMBOL-GLOBAL-VALUE only for variables of kind :GLOBAL.
                     (and (eq (vop-name symbol-value-vop) 'symbol-global-value)
                          (constant-tn-p sym)
                          (eq (info :variable :kind (tn-value sym)) :global)))
                 (eq sym (tn-ref-tn (vop-args symbol-value-vop)))
                 ;; If the symbol is either a compile-time known symbol,
                 ;; or can only be the same thing for both vops,
                 ;; then we're fine to combine.
                 (or (constant-tn-p sym)
                     (not (tn-writes sym))
                     (not (tn-ref-next (tn-writes sym))))
                 ;; Elide the SYMBOL-VALUE only if there is exactly one way to get there.
                 ;; Technically we could split the IR2 block and peel off the SYMBOL-VALUE,
                 ;; and if coming from the BRANCH-IF, jump to the block that contained
                 ;; everything else except the SYMBOL-VALUE vop.
                 (not (cdr (ir2block-predecessors (vop-block symbol-value-vop)))))
        (let ((replacement (if (eq (vop-name symbol-value-vop) 'symbol-global-value)
                               'fast-symbol-global-value
                               'fast-symbol-value))
              (result-tn (tn-ref-tn (vop-results symbol-value-vop))))
          (emit-and-insert-vop (vop-node vop) (vop-block vop)
                               (template-or-lose replacement)
                               (reference-tn sym nil) (reference-tn result-tn t) next)
          (emit-and-insert-vop (vop-node vop) (vop-block vop)
                               (template-or-lose 'unbound-marker-p)
                               (reference-tn result-tn nil) nil next)
          ;; We need to invert the test since BOUNDP and UNBOUND-MARKER-P have
          ;; the opposite meaning in the language. But by chance, they specify
          ;; opposite flags in the vop, which means that the sense of the
          ;; BRANCH-IF test is automagically correct after substitution.
          ;; Of course, this is machine-dependent.
          (delete-vop vop)
          (delete-vop symbol-value-vop)
          next)))))

;;; Optimize (if x ... nil) to reuse the NIL coming from X.
(defoptimizer (vop-optimize if-eq regalloc) (if-eq)
  (let ((branch-if (vop-next if-eq)))
    (when (and branch-if
               (eq (vop-name branch-if) 'branch-if))
      (let* ((args (vop-args if-eq))
             (x (tn-ref-tn args))
             (y (tn-ref-tn (tn-ref-across args))))
        (flet ((constant-p (x)
                 (or (constant-tn-p x)
                     (type= (tn-type x)
                            (specifier-type 'null)))))
          (when (and
                 (not (member (sb-name (sc-sb (tn-sc x)))
                              #+(or x86 x86-64)
                              '(sb-vm::stack)
                              #-(or x86 x86-64)
                              '(sb-vm::control-stack sb-vm::non-descriptor-stack)))
                 (constant-p y))
            (let ((move (branch-destination branch-if)))
              (when (and move
                         (eq (vop-name move) 'move)
                         (singleton-p (ir2block-predecessors (vop-block move))))
                (let* ((args (vop-args move))
                       (from (tn-ref-tn args)))
                  (when (and (constant-p from)
                             (eq (tn-leaf y)
                                 (tn-leaf from))
                             ;; It might not make sense if there are NULL-TN or ZERO-TN.
                             #-(or x86 x86-64)
                             (or (not (memq (constant-value (tn-leaf from))
                                            '(nil #+arm64 0)))
                                 (location= x y)))
                    (change-tn-ref-tn args x)
                    (setf (tn-ref-load-tn args) nil)))))))
        nil))))

;;; If 2BLOCK ends in an IF-EQ (or similar) + BRANCH-IF where the second operand
;;; of the test is an immediate value, then return the conditional vop.
;;; There may optionally be a MOVE vop prior to the conditional test,
;;; and optionally an unconditional BRANCH after the conditional branch.
(defun ends-in-branch-if-eq-imm-p (2block)
  ;; This test occurs in two flavors: for backends in which IF-EQ takes codegen
  ;; args for TARGET and NOT-P, such as ppc64; and backends in which IF-EQ returns
  ;; its result in the flags without causing control flow, such as x86.
  ;; Rather than make this feature-dependent, we'll can look at whether the
  ;; VOP-INFO-RESULT-TYPES of IF-EQ is :CONDITIONAL versus (:CONDITIONAL <flag>).
  (let ((vop (let ((last-vop (ir2-block-last-vop 2block)))
               ;; Final BRANCH can be ignored. Finding the if/else chain is based on successors.
               ;; It's fair to assume that the branch corresponds to the "other" successor.
               (when (and last-vop (eq (vop-name last-vop) 'branch))
                 (setq last-vop (vop-prev last-vop)))
               ;; See if we're looking at a BRANCH-IF
               (if (and last-vop (eq (vop-name last-vop) 'branch-if))
                 (vop-prev last-vop)
                 last-vop))))
    (when (and vop
               (or (and (eq (vop-name vop) 'if-eq)
                        (let ((comparand (tn-ref-tn (tn-ref-across (vop-args vop)))))
                          (and (eq (tn-kind comparand) :constant)
                               ;; a load-time-value constant TN has no leaf
                               (tn-leaf comparand)
                               (typep (tn-value comparand) '(or fixnum character)))))
                   ;; Thanks to schizophrenic compiler transforms,
                   ;; character comparisons can show up as either of
                   ;; the first two vops.
                   (member (vop-name vop) '(sb-vm::fast-char=/character/c
                                            sb-vm::fast-if-eq-character/c
                                            sb-vm::fast-if-eq-fixnum/c))))
      ;; Do some extra work when the IF is preceded by a MOVE
      (let ((test (tn-ref-tn (vop-args vop)))
            (prev (vop-prev vop)))
        ;; If: - The arg to IF is the result of a move
        ;;     - The result of the move goes nowhere but the IF
        ;; then the move is into a TN that is dead as soon as the
        ;; IF happens; a multiway branch is potentially ok.
        (if (and prev
                 (eq (vop-name prev) 'move)
                 (and (eq (tn-ref-tn (vop-results prev)) test)
                      (null (tn-ref-next (tn-writes test)))  ; exactly 1 write
                      (null (tn-ref-next (tn-reads test))))) ; exactly 1 read
            prev
            vop)))))

;;; Return T if and only if 2BLOCK has exactly two successors
(defun exactly-two-successors-p (2block)
  (let ((successors (cdr (gethash 2block *2block-info*))))
    (singleton-p (cdr successors))))

;;; Compute the longest chain of if/else operations starting at VOP.
;;; This is a simple task, as all we have to do is follow the 'else' of each IF.
;;; Primary return value is (((value . block) ...) drop-thru vop-name).
;;; Secondary value is a list of blocks to delete.
;;; If code coverage is enabled, it should theoretically be possible to find
;;; the if-else chain, but in practice it won't happen, because each else block
;;; is cluttered up by the coverage noise prior to performing the next comparison.
;;; It's probably just as well, because the coverage report would have no idea
;;; how to decode the recorded information from the multiway branch vop.
;;; Not to mention, SB-ASSEM::%MARK-USED-LABELS does not understand that labels
;;; in the branch table are all used. So there's that to contend with too.
(defun longest-if-else-chain (start-vop)
  (let (chain
        blocks-to-delete
        (test-var (tn-ref-tn (vop-args start-vop)))
        (vop start-vop))
    (loop
     (binding* ((block (vop-block vop))
                (conditional (if (eq (vop-name vop) 'move) (vop-next vop) vop))
                (codegen-info
                 (vop-codegen-info
                  (ecase (vop-name conditional)
                   ((if-eq
                     sb-vm::fast-if-eq-fixnum/c
                     sb-vm::fast-if-eq-character/c
                     sb-vm::fast-char=/character/c)
                    (if (eq (vop-info-result-types (vop-info conditional)) :conditional)
                        conditional ; this vop (IF-EQ) is a branching vop
                        (let ((next (vop-next conditional)))
                          (aver (eq (vop-name next) 'branch-if))
                          next))))))
                ;; codegen-info = (label negate-p [flags])
                (target-block (gethash (car codegen-info) *2block-info*))
                ;; successors are listed in an indeterminate order (I think)
                (successors (cdr (gethash block *2block-info*)))
                (drop-thru (car (if (eq (car successors) target-block)
                                    (cdr successors)
                                    successors)))
                ((then-block else-block)
                 (if (cadr codegen-info)
                     (values drop-thru target-block)
                     (values target-block drop-thru))))
       ;; If the ELSE block was the branch target (a negated branch),
       ;; then the THEN block might have no label as it is a dropthru.
       ;; Label it now in case of that.
       (or (ir2-block-%label then-block)
           (setf (ir2-block-%label then-block) (gen-label)))
       (when chain
         (push block blocks-to-delete))
       (let ((val (if (eq (vop-name conditional) 'if-eq)
                      ;; if-eq takes a constant TN
                      (tn-value (tn-ref-tn (tn-ref-across (vop-args conditional))))
                      ;; "-eq-/C" vops take a codegen arg
                      (car (vop-codegen-info conditional))))
             (else-block-predecessors (ir2block-predecessors else-block))
             (else-vop (ir2-block-start-vop else-block)))
         (push (cons val then-block) chain)
         ;; If ELSE block has more than one predecessor, that's OK,
         ;; but the chain must stop at this IF.
         (unless (and (and (singleton-p else-block-predecessors)
                           (eq (car else-block-predecessors) block))
                      else-vop
                      (eq (ends-in-branch-if-eq-imm-p (vop-block else-vop))
                          else-vop)
                      (eq (tn-ref-tn (vop-args else-vop)) test-var)
                      (exactly-two-successors-p else-block))
           (unless (cdr chain) ; does this IF start a chain of length at least 2?
             (return-from longest-if-else-chain (values nil nil)))
           ;; The else block could have been the fallthru of the last IF,
           ;; so it may not have needed a label, but now it does need one.
           (or (ir2-block-%label else-block)
               (setf (ir2-block-%label else-block) (gen-label)))
           ;; the chain is order-insensitive but more understandable
           ;; if returned in the order that tests appeared in source.
           (return (values (list (nreverse chain)
                                 else-block
                                 (vop-name conditional))
                           blocks-to-delete)))
         (setq vop (ir2-block-start-vop else-block)))))))

;;; There could be a backend-aware aspect to the decision about whether to
;;; convert to a jump table.
(defun can-encode-jump-table-p (min max)
  (declare (ignorable min max))
  #+(or ppc ppc64) (and (typep (sb-vm:fixnumize min) '(signed-byte 16))
                        (typep (sb-vm:fixnumize (- max min)) '(signed-byte 16)))
  #+(or x86 x86-64) t)

;;; Decide whether CHAIN can be implemented as a multiway branch.
;;; As a further enhancement, it would be nice if we could factor out the
;;; parts that can be, if any can be.
;;; e.g. (case x (1 :a) (2 :b) (3 :c) (zot 'y)) ; with any order of tests
;;; could be expressed as (if (eq x 'zot) y [multiway-branch])
(defun should-use-jump-table-p (chain &aux (choices (car chain)))
  ;; Dup keys could exist. REMOVE-DUPLICATES from-end can handle that:
  ;;  "the one occurring earlier in sequence is discarded, unless from-end
  ;;   is true, in which case the one later in sequence is discarded."
  (let ((choices (remove-duplicates choices :key #'car :from-end t)))
    ;; Convert to multiway only if at least 4 key comparisons would be needed.
    (unless (>= (length choices) 4)
      (return-from should-use-jump-table-p nil))
    (let ((values (mapcar #'car choices)))
      (cond ((every #'fixnump values)) ; ok
            ((every #'characterp values)
             (setq values (mapcar #'char-code values)))
            (t
             (return-from should-use-jump-table-p nil)))
      (let* ((min (reduce #'min values))
             (max (reduce #'max values))
             (table-size (1+ (- max min )))
             (size-limit (* (length values) 2)))
        ;; Don't waste too much space, e.g. {5,6,10,20} would require 16 words
        ;; for 4 entries, which is excessive.
        (when (and (<= table-size size-limit)
                   (can-encode-jump-table-p min max))
          ;; Return the new choices
          (cons choices (cdr chain)))))))

(defun convert-if-else-chains (component)
  (do-ir2-blocks (2block component)
    (let ((head (ends-in-branch-if-eq-imm-p 2block)))
      (when (and head (exactly-two-successors-p 2block))
        (binding* (((chain delete-blocks) (longest-if-else-chain head))
                   (culled-chain (should-use-jump-table-p chain) :exit-if-null)
                   (node (vop-node head))
                   (src-ref (vop-args head)))
          (flet ((delete-test (vop)
                   ;; delete 1 to 4 vops starting at VOP, depending on whether
                   ;; there is an initial MOVE and/or final BRANCH,
                   ;; and whether IF-EQ requires a following BRANCH-IF.
                   (awhen (vop-next vop) (delete-vop it))
                   (awhen (vop-next vop) (delete-vop it))
                   (awhen (vop-next vop) (delete-vop it))
                   (delete-vop vop)))
            (delete-test head)
            ;; Delete vops that are bypassed
            (dolist (2block delete-blocks)
              (delete-test (ir2-block-start-vop 2block))
              ;; there had better be no vops remaining
              (aver (null (ir2-block-start-vop 2block)))
              ;; The block can't be reached, and goes nowhere.
              (update-block-succ 2block nil)))
          (destructuring-bind (clauses else-block test-vop-name) culled-chain
            (let* ((key-type (if (characterp (caar clauses)) 'character 'fixnum))
                   (clause-keyfn (if (eq key-type 'character)
                                     (lambda (x) (char-code (car x)))
                                     #'car))
                   ;; Sort and unzip the alist
                   (ordered (sort (copy-list clauses) #'< :key clause-keyfn))
                   (keys (mapcar clause-keyfn ordered))
                   (blocks (mapcar #'cdr ordered))
                   (labels (mapcar #'ir2-block-%label blocks))
                   (otherwise (ir2-block-%label else-block)))
              ;; Sometimes an IR1 optimization hampers detection of the original comparands
              ;; making it seem like not all branches were covered. If we have this in source:
              ;;   (CASE (TRULY-THE (MOD 4 X)) (1 ...) (2 ...) (3 ...) (0 'THING))
              ;; then we will see IF nodes testing all of 1, 2, 3, 0, and an otherwise of NIL.
              ;; But if the 0 case returns NIL, then the final branch of the COND resembles
              ;; ((EQL #:G1 '0) NIL NIL) which is flushed because both consequents of the IF
              ;; do the same thing. So then we'll only see IF nodes for 1, 2, 3.
              ;; This is bad because it doesn't allow elision of the bounds check on the
              ;; multiway branch. So if the derived type has one more possibility at either end,
              ;; add it in directing flow to the OTHERWISE label.
              ;; This could be unkindly construed as a kludge.
              (let ((key-derived-type (tn-ref-type src-ref)))
                (when (and (eq key-type 'fixnum)
                           (typep key-derived-type 'numeric-type)
                           (csubtypep key-derived-type (specifier-type 'fixnum)))
                  (let ((min (car keys))
                        (max (car (last keys))))
                    (when (eql min (1+ (numeric-type-low key-derived-type)))
                      (push (numeric-type-low key-derived-type) keys)
                      (push otherwise labels))
                    (when (eql max (1- (numeric-type-high key-derived-type)))
                      (setf keys (nconc keys (list (numeric-type-high key-derived-type))))
                      (setf labels (nconc labels (list otherwise)))))))
              (dolist (label labels)
                (setf (label-usedp label) t))
              (emit-and-insert-vop node 2block
                                   (template-or-lose 'multiway-branch-if-eq)
                                   (reference-tn (tn-ref-tn src-ref) nil) nil nil
                                   (list labels otherwise key-type keys test-vop-name))
              ;; De-duplicate the successor blocks and update the flowgraph.
              ;; The ELSE block could be identical to any of the THEN blocks.
              (update-block-succ
               2block (remove-duplicates (cons else-block blocks))))))))))

(defun component-remove-constant (constant constants)
  (let ((index (position constant constants)))
    (loop for i from index below (1- (length constants))
          for next = (aref constants (1+ i))
          do (setf (aref constants i) next)
             (cond ((and (constant-p next)
                         (constant-info next))
                    (decf (tn-offset (constant-info next))))
                   ((typep next '(cons t (cons t (cons t null))))
                    (decf (tn-offset (third next))))))
    (decf (fill-pointer constants))))

;;; Optimize (svref #(constant array) safe-index) into accessing the code constants directly,
;;; saving on one memory indirection.
;;; TODO: Can optimize any array
#+(and x86-64 (or)) ;; Performance benefits are unclear, while the vectors can't be shared across different code objects.
(defoptimizer (vop-optimize (sb-vm::data-vector-ref-with-offset/simple-vector
                             sb-vm::data-vector-ref-with-offset/simple-array-fixnum))
    (vop)
  (let* ((args (vop-args vop))
         (array (tn-ref-tn args))
         (index (tn-ref-tn (tn-ref-across args)))
         (constants (ir2-component-constants (component-info *component-being-compiled*)))
         (first-constant))
    (when (and (eq (tn-kind array) :constant)
               (tn-leaf array)
               (not (tn-ref-next (tn-reads array))))
      (component-remove-constant (tn-leaf array) constants)
      (setf (tn-offset array) (length constants))
      (loop for x across (tn-value array)
            for constant = (setf first-constant (make-constant x))
            then (make-constant x)
            do (vector-push-extend constant constants))
      (setf (tn-leaf array) first-constant
            (constant-info first-constant) array)
      (prog1
          (emit-and-insert-vop (vop-node vop)
                           (vop-block vop)
                           (template-or-lose 'sb-vm::data-vector-ref-with-offset/constant-simple-vector)
                           (reference-tn-list (list array index) nil)
                           (reference-tn (tn-ref-tn (vop-results vop)) t)
                           vop
                           (vop-codegen-info vop))
        (delete-vop vop)))))

#+(or arm64 x86-64)
(defoptimizer (vop-optimize #+arm64
                            (sb-vm::data-vector-ref/simple-bit-vector-c
                             sb-vm::data-vector-ref/simple-bit-vector)
                            #+x86-64
                            (sb-vm::data-vector-ref-with-offset/simple-bit-vector-c
                             sb-vm::data-vector-ref-with-offset/simple-bit-vector))
    (vop)
  (let* ((next (next-vop vop))
         (branch (and next
                      (next-vop next)))
         plusp)
    (when (and branch
               (or
                (eq (vop-name next) #+x86-64 'sb-vm::fast-if-eq-fixnum/c
                                    #+arm64 'sb-vm::fast-if-eq-integer/c)
                (and (eq (vop-name next)
                         #-arm64 'sb-vm::fast-if->-c/fixnum
                         #+arm64 'sb-vm::fast-if->-integer/c)
                     (eql (car (vop-codegen-info next)) 0)
                     (setf plusp t)))
               (eq (vop-name branch)
                   'branch-if))
      (let* ((result (tn-ref-tn (vop-results vop)))
             (value (if plusp
                        1
                        (car (vop-codegen-info next)))))
        (when (and (not (tn-ref-next (tn-reads result)))
                   (eq result (tn-ref-tn (vop-args next))))
          (check-type value bit)
          (let ((template (template-or-lose #+arm64
                                            (if (eq (vop-name vop) 'sb-vm::data-vector-ref/simple-bit-vector)
                                                'sb-vm::data-vector-ref/simple-bit-vector-eq
                                                'sb-vm::data-vector-ref/simple-bit-vector-c-eq)
                                            #+x86-64
                                            (if (eq (vop-name vop) 'sb-vm::data-vector-ref-with-offset/simple-bit-vector)
                                                'sb-vm::data-vector-ref-with-offset/simple-bit-vector-eq
                                                'sb-vm::data-vector-ref-with-offset/simple-bit-vector-c-eq))))
            (prog1
                (emit-and-insert-vop (vop-node vop)
                                     (vop-block vop)
                                     template
                                     (reference-tn-refs (vop-args vop) nil)
                                     nil
                                     vop
                                     (vop-codegen-info vop))
              ;; copy the condition flag
              (setf (third (vop-codegen-info branch))
                    (cdr (template-result-types template)))
              (when (eq value 1)
                (setf (second (vop-codegen-info branch))
                      (not (second (vop-codegen-info branch)))))
              (delete-vop vop)
              (delete-vop next))))))))

(when-vop-existsp (:named sb-vm::<-integer-fixnum)
  ;; The <-integer-fixnum VOPs already perform dispatch for fixnum/bignum,
  ;; and load the header byte.
  ;; Replace the preceding INTEGERP VOP with an appropriate
  ;; <-integer-fixnum, which checks for INTEGER.
  (defoptimizer (vop-optimize integerp) (vop)
    (destructuring-bind (target not-p) (vop-codegen-info vop)
      (let ((target-block (gethash target *2block-info*)))
        (when target-block
          (let* ((next (if (eq vop (ir2-block-last-vop (vop-block vop)))
                           (ir2-block-next (vop-block vop))
                           (let ((next (next-vop vop)))
                             (and (eq (vop-name next) 'branch)
                                  (gethash (car (vop-codegen-info next)) *2block-info*)))))
                 (not-target-block (if not-p
                                       target-block
                                       next))
                 (target-block (if not-p
                                   next
                                   target-block))
                 (cmp (ir2-block-start-vop target-block)))
            (when (and cmp
                       (singleton-p (ir2block-predecessors target-block)))
              (let ((integer (vop-args vop))
                    (args (vop-args cmp))
                    tn-refs)
                (when (case (vop-name cmp)
                        ((sb-vm::>-integer-fixnum sb-vm::<-integer-fixnum)
                         (when (eq (tn-ref-tn args) (tn-ref-tn integer))
                           (setf tn-refs (list integer (tn-ref-across args)))))
                        ((sb-vm::>-fixnum-integer sb-vm::<-fixnum-integer)
                         (when (eq (tn-ref-tn (tn-ref-across args))
                                   (tn-ref-tn integer))
                           (setf tn-refs (list args integer)))))
                  (destructuring-bind (cmp-target cmp-not-p) (vop-codegen-info cmp)
                    (let* ((cmp-next-block (ir2-block-next (vop-block cmp)))
                           (cmp-target-block (gethash cmp-target *2block-info*)))
                      (flet ((invert ()
                               (template-or-lose
                                (case (vop-name cmp)
                                  (sb-vm::>-integer-fixnum 'sb-vm::<=-integer-fixnum)
                                  (sb-vm::<-integer-fixnum 'sb-vm::>=-integer-fixnum)
                                  (sb-vm::>-fixnum-integer 'sb-vm::<=-fixnum-integer)
                                  (sb-vm::<-fixnum-integer 'sb-vm::>=-fixnum-integer)))))
                        (multiple-value-bind (new-vop new-target new-not-p)
                            (cond ((and not-p
                                        (eq not-target-block cmp-next-block))
                                   (if cmp-not-p
                                       (values (invert) cmp-target nil)
                                       (values (vop-info cmp) cmp-target nil)))
                                  ((and not-p
                                        (eq not-target-block cmp-target-block))
                                   (if cmp-not-p
                                       (values (vop-info cmp) cmp-target t)
                                       (values (invert) cmp-target t)))
                                  ((and (not not-p)
                                        (eq not-target-block cmp-target-block))
                                   (if cmp-not-p
                                       (values (vop-info cmp) (ir2-block-%label target-block) nil)
                                       (values (invert) (ir2-block-%label target-block) nil)))
                                  ((and (not not-p)
                                        (eq not-target-block cmp-next-block))
                                   (if cmp-not-p
                                       (values (invert) cmp-target nil)
                                       (values (vop-info cmp) cmp-target nil)))
                                  (t
                                   (return-from vop-optimize-integerp-optimizer)))
                          (prog1
                              (emit-and-insert-vop (vop-node vop) (vop-block vop)
                                                   new-vop
                                                   (reference-tn-ref-list tn-refs nil)
                                                   nil vop
                                                   (list new-target new-not-p))
                            (delete-vop vop)
                            (delete-vop cmp))))))))))))
      nil)))

;;; No need to reset the stack pointer just before returning.
(defoptimizer (vop-optimize reset-stack-pointer) (vop)
  (loop for next = (next-vop vop) then (next-vop next)
        do (cond ((not next)
                  (return))
                 ((eq (vop-name next) 'move))
                 ((memq (vop-name next) '(return-single return known-return
                                          tail-call tail-call-named
                                          static-tail-call-named))
                  (delete-vop vop)
                  ;; Delete the VOP that saves the stack pointer too.
                  (let ((tn (tn-ref-tn (vop-args vop))))
                    (unless (tn-reads tn)
                      (aver (eq (vop-name (tn-ref-vop (tn-writes tn)))
                                'current-stack-pointer))
                      (delete-vop (tn-ref-vop (tn-writes tn)))))
                  (return))
                 (t
                  (return)))))

;;; stack-analyze may have avoided creating a cleanup
;;; leaving this unused
(defoptimizer (vop-optimize current-stack-pointer) (vop)
  (let ((tn (tn-ref-tn (vop-results vop))))
    (when (not (tn-reads tn))
      (delete-vop vop))))

;;; Load the WIDETAG once for a series of type tests.
(when-vop-existsp (:named sb-vm::load-other-pointer-widetag)
  (defoptimizer (vop-optimize %other-pointer-subtype-p) (vop)
    (let (vops
          stop
          null
          (value (tn-ref-tn (vop-args vop))))
      (labels ((good-vop-p (vop)
                 (and (singleton-p (ir2block-predecessors (vop-block vop)))
                      (or (getf sb-vm::*other-pointer-type-vops* (vop-name vop))
                          (eq (vop-name vop) '%other-pointer-subtype-p))
                      (eq (tn-ref-tn (vop-args vop)) value)))
               (chain (vop &optional (collect t))
                 (let ((next (branch-destination vop nil)))
                   (cond ((and next
                               (or (neq (vop-name vop) 'symbolp)
                                   (and (not null)
                                        (setf null (branch-destination vop)))))
                          (when collect
                            (push vop vops))
                          (cond ((good-vop-p next)
                                 (chain next))
                                ((not stop)
                                 (setf stop (vop-block next)))))
                         ((not stop)
                          (setf stop (vop-block vop)))))
                 (let ((true (branch-destination vop)))
                   (when (and true
                              (good-vop-p true))
                     (push true vops)
                     (chain true nil))))
               (ir2-block-label (block)
                 (or (ir2-block-%label block)
                     (setf (ir2-block-%label block) (gen-label)))))
        (chain vop)
        (when (> (length vops) 1)
          (let ((widetag (make-representation-tn (primitive-type-or-lose 'sb-vm::unsigned-byte-64)
                                                 sb-vm:unsigned-reg-sc-number))
                (block (vop-block vop)))
            (setf (tn-type value)
                  (tn-ref-type (vop-args vop)))
            (emit-and-insert-vop (vop-node vop)
                                 block
                                 (template-or-lose 'sb-vm::load-other-pointer-widetag)
                                 (reference-tn value nil)
                                 (reference-tn widetag t)
                                 vop
                                 (list (ir2-block-label stop)
                                       (and null
                                            (ir2-block-label (vop-block null)))))
            (update-block-succ block
                               (cons stop
                                     (ir2block-successors block)))

            (let ((test-vop (template-or-lose 'sb-vm::test-widetag)))
              (loop for vop in vops
                    for info = (vop-codegen-info vop)
                    for tags = (if (eq (vop-name vop) '%other-pointer-subtype-p)
                                   (third info)
                                   (getf sb-vm::*other-pointer-type-vops* (vop-name vop)))
                    do
                    (let ((next (vop-next vop)))
                      (when (and next
                                 (eq (vop-name next) 'branch-if))
                        (setf info (vop-codegen-info next))
                        (delete-vop next))
                      (emit-and-insert-vop (vop-node vop)
                                           (vop-block vop)
                                           test-vop
                                           (reference-tn widetag nil)
                                           nil
                                           vop
                                           (list (first info) (second info) tags)))
                    (delete-vop vop)))))))
    nil)

  (loop for (vop) on sb-vm::*other-pointer-type-vops* by #'cddr
        do
        (set-vop-optimizer (template-or-lose vop)
                           #'vop-optimize-%other-pointer-subtype-p-optimizer)))

(when-vop-existsp (:named sb-vm::load-instance-layout)
  (defoptimizer (vop-optimize structure-typep) (vop)
    (let (vops
          stop
          (value (tn-ref-tn (vop-args vop))))
      (labels ((good-vop-p (vop)
                 (and (singleton-p (ir2block-predecessors (vop-block vop)))
                      (eq (vop-name vop) 'structure-typep)
                      (eq (tn-ref-tn (vop-args vop)) value)))
               (chain (vop &optional (collect t))
                 (let ((next (branch-destination vop nil)))
                   (cond (next
                          (when collect
                            (push vop vops))
                          (cond ((good-vop-p next)
                                 (chain next))
                                ((not stop)
                                 (setf stop (vop-block next)))))
                         ((not stop)
                          (setf stop (vop-block vop)))))
                 (let ((true (branch-destination vop)))
                   (when (and true
                              (good-vop-p true))
                     (push true vops)
                     (chain true nil))))
               (ir2-block-label (block)
                 (or (ir2-block-%label block)
                     (setf (ir2-block-%label block) (gen-label)))))
        (chain vop)
        (when (> (length vops) 1)
          (let ((layout (make-representation-tn *backend-t-primitive-type*
                                                sb-vm:descriptor-reg-sc-number))
                (block (vop-block vop)))
            (setf (tn-type value)
                  (tn-ref-type (vop-args vop)))
            (emit-and-insert-vop (vop-node vop)
                                 block
                                 (template-or-lose 'sb-vm::load-instance-layout)
                                 (reference-tn value nil)
                                 (reference-tn layout t)
                                 vop
                                 (list (ir2-block-label stop)))
            (update-block-succ block
                               (cons stop
                                     (ir2block-successors block)))
            (let ((test-vop (template-or-lose 'sb-vm::structure-typep*)))
              (loop for vop in vops
                    for info = (vop-codegen-info vop)
                    do
                    (emit-and-insert-vop (vop-node vop)
                                         (vop-block vop)
                                         test-vop
                                         (reference-tn layout nil)
                                         nil
                                         vop
                                         info)
                    (delete-vop vop)))))))
    nil))

(defun very-temporary-p (tn)
  (let ((writes (tn-writes tn))
        (reads (tn-reads tn)))
    (and writes reads (not (tn-ref-next writes)) (not (tn-ref-next reads)))))

(defun next-vop-is (vop names)
  (let ((next (next-vop vop)))
    (and next
         (let ((name (vop-name next)))
           (if (atom names) (eq name names) (memq name names)))
         next)))

(defun previous-vop-is (vop names)
  (let ((prev (vop-prev vop)))
    (and prev
         (let ((name (vop-name prev)))
           (if (atom names) (eq name names) (memq name names)))
         prev)))

;;; Possibly replace HOWMANY vops starting at FIRST with a vop named REPLACEMENT.
;;; Each deleted vop must have exactly one argument and one result.
;;; - There must be no way to begin execution in the middle of the pattern.
;;; - The argument to each successive vop must be the result of its predecessor.
;;; - There must be no other refs to TNs which are to be deleted.
;;;
;;; On the x86 backends this can potentially form the basis of an optimization
;;; which folds together a memory read from {CAR, CDR, INSTANCE-REF} etc with
;;; a following integer comparison and/or fixnum tag test as long as the base object
;;; is in a register (so we get the load as part of the instruction).
(defun replace-vops (howmany first replacement)
  (flet ((can-replace (vop &aux (result (tn-ref-tn (vop-results vop)))
                                (next (next-vop vop)))
           (and (very-temporary-p result)
                (eq (tn-ref-tn (vop-args next)) result))))
    (let ((last (ecase howmany
                  (2 (when (can-replace first)
                       (next-vop first)))
                  (3 (when (and (can-replace first)
                                (can-replace (next-vop first)))
                       (next-vop (next-vop first)))))))
      (when last
        (let ((new (emit-and-insert-vop (vop-node first)
                                        (vop-block first)
                                        (template-or-lose replacement)
                                        (reference-tn (tn-ref-tn (vop-args first)) nil)
                                        (reference-tn (tn-ref-tn (vop-results last)) t)
                                        first))) ; insert before this
          (dotimes (i (1- howmany)) ; if 3, replace 2 "NEXT"'s and then first, etc
            (delete-vop (next-vop first)))
          (delete-vop first)
          new))))) ; return suitable value for RUN-VOP-OPTIMIZERS

(defun run-vop-optimizers (component &optional stage (without-stage (not stage)))
  (do-ir2-blocks (block component)
    (let ((vop (ir2-block-start-vop block)))
      (loop while vop
            do
            ;; Avoid following the NEXT pointer of a deleted vop, which despite
            ;; possibly being accidentally correct, is dubious.
            ;; Optimizer must return NIL or a VOP whence to resume scanning.
            (setq vop (or (awhen (vop-info-optimizer (vop-info vop))
                            (if (consp it)
                                (and (eq (cdr it) stage)
                                     (funcall (car it) vop))
                                (and without-stage
                                     (funcall it vop))))
                          (vop-next vop)))))))

(defun merge-instance-set-vops (vop)
  (let ((instance (tn-ref-tn (vop-args vop)))
        (this vop)
        (pairs))
    (loop
       (let ((index (tn-ref-tn (tn-ref-across (vop-args this)))))
         (unless (constant-tn-p index) (return))
         (push (cons (tn-value index) (tn-ref-tn (sb-vm::vop-nth-arg 2 this)))
               pairs))
       (let ((next (vop-next this)))
         (unless (and next
                      (eq (vop-name next) 'sb-vm::instance-index-set)
                      (eq (tn-ref-tn (vop-args next)) instance))
           (return))
         (setq this next)))
    (unless (cdr pairs) ; if at least 2
      (return-from merge-instance-set-vops nil))
    (setq pairs (nreverse pairs))
    (let ((new (emit-and-insert-vop
                (vop-node vop) (vop-block vop)
                (template-or-lose 'sb-vm::instance-set-multiple)
                (reference-tn-list (cons instance (mapcar #'cdr pairs)) nil)
                nil vop (list (mapcar #'car pairs)))))
      (loop (let ((next (vop-next vop)))
              (delete-vop vop)
              (pop pairs)
              (setq vop next))
            (unless pairs (return)))
      new)))

(defun ir2-optimize-stores (component)
  ;; This runs after representation selection. It's the same as RUN-VOP-OPTIMIZERS,
  ;; but with hardcoded vop names and function to call.
  ;; It seems like this should also supplant the #+arm64 hack in GENERATE-CODE.
  (do-ir2-blocks (block component)
    (let ((vop (ir2-block-start-vop block)))
      (loop (unless vop (return))
            (let ((optimizer
                   (case (vop-name vop)
                     (sb-vm::instance-index-set
                      (when (gethash 'sb-vm::instance-set-multiple
                                     *backend-parsed-vops*)
                        'merge-instance-set-vops)))))
              (setq vop (or (awhen optimizer (funcall it vop))
                            (vop-next vop))))))))

(defun ir2-optimize (component &optional stage)
  (let ((*2block-info* (make-hash-table :test #'eq)))
    (initialize-ir2-blocks-flow-info component)
    (case stage
      (regalloc
       (run-vop-optimizers component stage)
       (delete-no-op-vops component)
       (ir2-optimize-jumps component)
       (optimize-constant-loads component))
      (select-representations
       ;; Give the optimizers a second opportunity to alter newly inserted vops
       ;; by looking for patterns that have a shorter expression as a single vop.
       (run-vop-optimizers component stage t)
       (delete-unused-ir2-blocks component)
       ;; Try to combine consecutive uses of %INSTANCE-SET.
       ;; This can't be done prior to selecting representations
       ;; because SELECT-REPRESENTATIONS might insert some
       ;; things like MOVE-FROM-DOUBLE which makes the
       ;; "consecutive" vops no longer consecutive.
       (ir2-optimize-stores component))
      (t
       (when (and *compiler-trace-output*
                  (member :pre-ir2-optimize *compile-trace-targets*))
         (let ((*standard-output* *compiler-trace-output*))
           ;; We really ought to print the IR1 before IR2 but this achieves its
           ;; purpose of helping figure out what changes were made to IR2.
           (format t "~&Before IR2-optimize:~%")
           (print-ir2-blocks component)))
       ;; Look for if/else chains before cmovs, because a cmov
       ;; affects whether the last if/else is recognizable.
       #+(or ppc ppc64 x86 x86-64) (convert-if-else-chains component)
       (run-vop-optimizers component)
       (delete-unused-ir2-blocks component))))

  (values))

(defun delete-unnecessary-move (vop)
  (when (and (vop-next vop)
             (eq (vop-name (vop-next vop)) 'move)
             ;; the source of the move is the result of this
             (eq (tn-ref-tn (vop-args (vop-next vop)))
                 (tn-ref-tn (vop-results vop)))
             ;; the destination of the move is the same as the input of this
             (eq (tn-ref-tn (vop-results (vop-next vop)))
                 (tn-ref-tn (vop-args vop)))
             ;; there is exactly one write and one read of the intermediate TN
             (very-temporary-p (tn-ref-tn (vop-results vop))))
    ;; Change my result ref to the same TN as the input and delete the MOVE
    (change-tn-ref-tn (vop-results vop) (tn-ref-tn (vop-args vop)))
    (delete-vop (vop-next vop))))
