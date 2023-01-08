;;;; This file contains the code that finds the initial components and
;;;; DFO, and recomputes the DFO if it is invalidated.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; Find the DFO for a component, deleting any unreached blocks and
;;; merging any other components we reach. We repeatedly iterate over
;;; the entry points, since new ones may show up during the walk.
(declaim (ftype (function (component) (values)) find-dfo))
(defun find-dfo (component)
  (clear-flags component)
  (setf (component-reanalyze component) nil)
  (let ((head (component-head component)))
    (do ()
        ((dolist (ep (block-succ head) t)
           (unless (or (block-flag ep)
                       (block-to-be-deleted-p ep))
             (find-dfo-aux ep head component)
             (return nil))))))
  (let ((num 0))
    (declare (fixnum num))
    (do-blocks-backwards (block component :both)
      (if (block-flag block)
          (setf (block-number block) (incf num))
          (delete-block-lazily block)))
    (clean-component component (component-head component)))
  (values))

;;; Move all the code and entry points from OLD to NEW. The code in
;;; OLD is inserted at the head of NEW. This is also called during LET
;;; conversion when we are about in insert the body of a LET in a
;;; different component. [A local call can be to a different component
;;; before FIND-INITIAL-DFO runs.]
(declaim (ftype (function (component component) (values)) join-components))
(defun join-components (new old)
  (aver (eq (component-kind new) (component-kind old)))
  (let ((old-head (component-head old))
        (old-tail (component-tail old))
        (head (component-head new))
        (tail (component-tail new)))

    (do-blocks (block old)
      (setf (block-flag block) nil)
      (setf (block-component block) new))

    (let ((old-next (block-next old-head))
          (old-last (block-prev old-tail))
          (next (block-next head)))
      (unless (eq old-next old-tail)
        (setf (block-next head) old-next)
        (setf (block-prev old-next) head)

        (setf (block-prev next) old-last)
        (setf (block-next old-last) next))

      (setf (block-next old-head) old-tail)
      (setf (block-prev old-tail) old-head))

    (setf (component-lambdas new)
          (nconc (component-lambdas old) (component-lambdas new)))
    (setf (component-lambdas old) nil)
    (setf (component-new-functionals new)
          (nconc (component-new-functionals old)
                 (component-new-functionals new)))
    (setf (component-new-functionals old) nil)

    (dolist (xp (block-pred old-tail))
      (unlink-blocks xp old-tail)
      (link-blocks xp tail))
    (dolist (ep (block-succ old-head))
      (unlink-blocks old-head ep)
      (link-blocks head ep)))
  (values))

;;; Do a depth-first walk from BLOCK, inserting ourself in the DFO
;;; after HEAD. If we somehow find ourselves in another component,
;;; then we join that component to our component.
(declaim (ftype (function (cblock cblock component) (values)) find-dfo-aux))
(defun find-dfo-aux (block head component)
  (unless (eq (block-component block) component)
    (join-components component (block-component block)))
  (unless (or (block-flag block) (block-delete-p block))
    (setf (block-flag block) t)
    (dolist (succ (block-succ block))
      (find-dfo-aux succ head component))
    (remove-from-dfo block)
    (add-to-dfo block head))
  (values))

;;; This function is called on each block by FIND-INITIAL-DFO-AUX
;;; before it walks the successors. It looks at the home lambda's
;;; bind block to see whether that block is in some other component:
;;; -- If the block is in the initial component, then do
;;;    DFO-WALK-CALL-GRAPH on the home function to move it
;;;    into COMPONENT.
;;; -- If the block is in some other component, join COMPONENT into
;;;    it and return that component.
;;; -- If the home function is deleted, do nothing. BLOCK must
;;;    eventually be discovered to be unreachable as well. This can
;;;    happen when we have a NLX into a function with no references.
;;;    The escape function still has refs (in the deleted function).
;;;
;;; This ensures that all the blocks in a given environment will be in
;;; the same component, even when they might not seem reachable from
;;; the environment entry. Consider the case of code that is only
;;; reachable from a non-local exit.
(defun walk-home-call-graph (block component)
  (declare (type cblock block) (type component component))
  (let ((home-lambda (block-home-lambda block)))
    (if (eq (functional-kind home-lambda) :deleted)
        component
        (let ((home-component (lambda-component home-lambda)))
          (cond ((eq (component-kind home-component) :initial)
                 (dfo-walk-call-graph home-lambda component))
                ((eq home-component component)
                 component)
                (t
                 (join-components home-component component)
                 home-component))))))

;;; This is somewhat similar to FIND-DFO-AUX, except that it merges
;;; the current component with any strange component, rather than the
;;; other way around. This is more efficient in the common case where
;;; the current component doesn't have much stuff in it.
;;;
;;; We return the current component as a result, allowing the caller
;;; to detect when the old current component has been merged with
;;; another.
;;;
;;; We walk blocks in initial components as though they were already
;;; in the current component, moving them to the current component in
;;; the process. The blocks are inserted at the head of the current
;;; component.
(defun find-initial-dfo-aux (block component)
  (declare (type cblock block) (type component component))
  (let ((this (block-component block)))
    (cond
     ((not (or (eq this component)
               (eq (component-kind this) :initial)))
      (join-components this component)
      this)
     ((block-flag block) component)
     (t
      (setf (block-flag block) t)
      (let ((current (walk-home-call-graph block component)))
        (dolist (succ (block-succ block))
          (setq current (find-initial-dfo-aux succ current)))
        (remove-from-dfo block)
        (add-to-dfo block (component-head current))
        current)))))

;;; Return a list of all the home lambdas that reference FUN (may
;;; contain duplications).
;;;
;;; References to functions which local call analysis could not (or
;;; were chosen not) to local call convert will appear as references
;;; to XEP lambdas. We can ignore references to XEPs that appear in
;;; :TOPLEVEL components, since environment analysis goes to special
;;; effort to allow closing over of values from a separate top level
;;; component. (And now that HAS-EXTERNAL-REFERENCES-P-ness
;;; generalizes :TOPLEVEL-ness, we ignore those too.) All other
;;; references must cause components to be joined.
;;;
;;; References in deleted functions are also ignored, since this code
;;; will be deleted eventually.
(defun find-reference-funs (fun)
  (collect ((res))
    (dolist (ref (leaf-refs fun))
      (let* ((home (node-home-lambda ref))
             (home-kind (functional-kind home))
             (home-externally-visible-p
               (or (eq home-kind :toplevel)
                   (functional-has-external-references-p home))))
        (unless (or (and home-externally-visible-p
                         (eq (functional-kind fun) :external))
                    (eq home-kind :deleted))
          (res home))))
    (res)))

;;; Move the code for FUN and all functions called by it into
;;; COMPONENT. If FUN is already in COMPONENT, just return that
;;; component.
;;;
;;; If the function is in an initial component, then we move its head
;;; and tail to COMPONENT and add it to COMPONENT's lambdas. It is
;;; harmless to move the tail (even though the return might be
;;; unreachable) because if the return is unreachable it (and its
;;; successor link) will be deleted in the post-deletion pass.
;;;
;;; We then do a FIND-DFO-AUX starting at the head of FUN. If this
;;; flow-graph walk encounters another component (which can only
;;; happen due to a non-local exit), then we move code into that
;;; component instead. We then recurse on all functions called from
;;; FUN, moving code into whichever component the preceding call
;;; returned.
;;;
;;; If FUN is in the initial component, but the BLOCK-FLAG is set in
;;; the bind block, then we just return COMPONENT, since we must have
;;; already reached this function in the current walk (or the
;;; component would have been changed).
;;;
;;; If the function is an XEP, then we also walk all functions that
;;; contain references to the XEP. This is done so that environment
;;; analysis doesn't need to cross component boundaries. This also
;;; ensures that conversion of a full call to a local call won't
;;; result in a need to join components, since the components will
;;; already be one.
(defun dfo-walk-call-graph (fun component)
  (declare (type clambda fun) (type component component))
  (aver (not (eql (lambda-kind fun) :deleted)))
  (let* ((bind-block (node-block (lambda-bind fun)))
         (this (block-component bind-block))
         (return (lambda-return fun)))
    (cond
     ((eq this component)
      component)
     ((not (eq (component-kind this) :initial))
      (join-components this component)
      this)
     ((block-flag bind-block)
      component)
     (t
      (push fun (component-lambdas component))
      (setf (component-lambdas this)
            (delete fun (component-lambdas this)))
      (link-blocks (component-head component) bind-block)
      (unlink-blocks (component-head this) bind-block)
      (when return
        (let ((return-block (node-block return)))
          (link-blocks return-block (component-tail component))
          (unlink-blocks return-block (component-tail this))))
      (let ((res (find-initial-dfo-aux bind-block component)))
        (declare (type component res))
        (flet ((walk (fun)
                 (unless (eq (lambda-kind fun) :deleted)
                   (setq res (dfo-walk-call-graph fun res)))))
          (do-sset-elements (fun (lambda-calls fun))
            (walk fun))
          (when (eq (lambda-kind fun) :external)
            (dolist (fun (find-reference-funs fun))
              (walk fun)))
          res))))))

;;; Return true if FUN either is an XEP or has EXITS to some of its
;;; ENTRIES.
(defun has-xep-or-nlx (fun)
  (declare (type clambda fun))
  (or (eq (functional-kind fun) :external)
      (let ((entries (lambda-entries fun)))
        (and entries
             (find-if #'entry-exits entries)))))

;;; Compute the result of FIND-INITIAL-DFO given the list of all
;;; resulting components. Components with a :TOPLEVEL lambda, but no
;;; normal XEPs or potential non-local exits are marked as :TOPLEVEL.
;;; If there is a :TOPLEVEL lambda, and also a normal XEP, then we
;;; treat the component as normal. Components with no entry of any
;;; sort are deleted.
(defun separate-toplevelish-components (components)
  (declare (list components))
  (collect ((real)
            (top)
            (real-top))
    (dolist (component components)
      (unless (eq (block-next (component-head component))
                  (component-tail component))
        (let* ((funs (component-lambdas component))
               (has-top (find :toplevel funs :key #'functional-kind))
               (has-external-references
                 (some #'functional-has-external-references-p funs)))
          (cond (;; The FUNCTIONAL-HAS-EXTERNAL-REFERENCES-P concept
                 ;; is newer than the rest of this function, and
                 ;; doesn't really seem to fit into its mindset. Here
                 ;; we mark components which contain such FUNCTIONs
                 ;; them as :COMPLEX-TOPLEVEL, since they do get
                 ;; executed at run time, and since it's not valid to
                 ;; delete them just because they don't have any
                 ;; references from pure :TOPLEVEL components. -- WHN
                 has-external-references
                 (setf (component-kind component) :complex-toplevel)
                 (real component)
                 (real-top component))
                ((or (some #'has-xep-or-nlx funs)
                     (and has-top (rest funs)))
                 (setf (component-name component)
                       (possibly-base-stringize
                        (find-component-name component)))
                 (real component)
                 (when has-top
                   (setf (component-kind component) :complex-toplevel)
                   (real-top component)))
                (has-top
                 (setf (component-kind component) :toplevel)
                 (setf (component-name component) "top level form")
                 (top component))
                (t
                 (delete-component component))))))
    (values (real) (top) (real-top))))

;;; Given a list of top-level lambdas, return three lists of components
;;; representing the actual component division:
;;;  1] the non-top-level components,
;;;  2] and the second is the top-level components, and
;;;  3] Components in [1] that also have a top-level lambda.
;;;
;;; We assign the DFO for each component, and delete any unreachable
;;; blocks. We assume that the FLAGS have already been cleared.
;;;
;;; We iterate over the lambdas in each initial component, trying
;;; to put each function in its own component, but joining it to
;;; an existing component if we find that there are references
;;; between them. Any code that is left in an initial component
;;; must be unreachable, so we can delete it. Stray links to the
;;; initial component tail (due to NIL function terminated blocks)
;;; are moved to the appropriate new component tail.
(defun find-initial-dfo (top-level-lambdas)
  (declare (list top-level-lambdas))
  (collect ((components))
    (let ((new (make-empty-component)))
      (dolist (initial-component (mapcar #'lambda-component top-level-lambdas))
        (unless (eq (component-kind initial-component) :deleted)
          (dolist (component-lambda (component-lambdas initial-component))
            (aver (member (functional-kind component-lambda)
                          '(:optional :external :toplevel nil :escape
                            :cleanup)))
            (let ((res (dfo-walk-call-graph component-lambda new)))
              (when (eq res new)
                (aver (not (member new (components))))
                (components new)
                (setq new (make-empty-component)))))
          (aver (eq (component-kind initial-component) :initial))
          (aver (null (component-lambdas initial-component)))
          (let ((tail (component-tail initial-component)))
            (dolist (pred (block-pred tail))
              (let ((pred-component (block-component pred)))
                (unless (eq pred-component initial-component)
                  (unlink-blocks pred tail)
                  (link-blocks pred (component-tail pred-component))))))
          (delete-component initial-component))))

    ;; When we are done, we assign DFNs.
    (dolist (component (components))
      (let ((num 0))
        (declare (fixnum num))
        (do-blocks-backwards (block component :both)
          (setf (block-number block) (incf num)))))

    (dolist (component (components))
      (delete-if-no-entries component))

    ;; Pull out top-level-ish code.
    (separate-toplevelish-components (components))))

;;; Insert the code in LAMBDA at the end of RESULT-LAMBDA.
(defun merge-1-toplevel-lambda (result-lambda lambda)
  (declare (type clambda result-lambda lambda))

  ;; Delete the lambda, and combine the LETs and entries.
  (setf (functional-kind lambda) :deleted)
  (dolist (let (lambda-lets lambda))
    (setf (lambda-home let) result-lambda)
    (setf (lambda-environment let) (lambda-environment result-lambda))
    (push let (lambda-lets result-lambda)))
  (setf (lambda-entries result-lambda)
        (nconc (lambda-entries result-lambda)
               (lambda-entries lambda)))

  (let* ((bind (lambda-bind lambda))
         (bind-block (node-block bind))
         (component (block-component bind-block))
         (result-component (lambda-component result-lambda))
         (result-return-block (node-block (lambda-return result-lambda))))

    ;; Move blocks into the new COMPONENT, and move any nodes directly
    ;; in the old LAMBDA into the new one (with LETs implicitly moved
    ;; by changing their home.)
    (do-blocks (block component)
      (do-nodes (node nil block)
        (let ((lexenv (node-lexenv node)))
          (when (eq (lexenv-lambda lexenv) lambda)
            (setf (lexenv-lambda lexenv) result-lambda))))
      (setf (block-component block) result-component))

    ;; Splice the blocks into the new DFO, and unlink them from the
    ;; old component head and tail. Non-return blocks that jump to the
    ;; tail (NIL-returning calls) are switched to go to the new tail.
    (let* ((head (component-head component))
           (first (block-next head))
           (tail (component-tail component))
           (last (block-prev tail))
           (prev (block-prev result-return-block)))
      (setf (block-next prev) first)
      (setf (block-prev first) prev)
      (setf (block-next last) result-return-block)
      (setf (block-prev result-return-block) last)
      (dolist (succ (block-succ head))
        (unlink-blocks head succ))
      (dolist (pred (block-pred tail))
        (unlink-blocks pred tail)
        (let ((last (block-last pred)))
          (unless (return-p last)
            (aver (basic-combination-p last))
            (link-blocks pred (component-tail result-component))))))

    (let ((lambdas (component-lambdas component)))
      (aver (and (null (rest lambdas))
                 (eq (first lambdas) lambda))))

    ;; Switch the end of the code from the return block to the start of
    ;; the next chunk.
    (dolist (pred (block-pred result-return-block))
      (unlink-blocks pred result-return-block)
      (link-blocks pred bind-block))
    (unlink-node bind)

    ;; If there is a return, then delete it (making the preceding node
    ;; the last node) and link the block to the result return. There
    ;; is always a preceding REF NIL node in top level lambdas.
    (let ((return (lambda-return lambda)))
      (when return
        (link-blocks (node-block return) result-return-block)
        (flush-dest (return-result return))
        (unlink-node return)))))

;;; Given a non-empty list of top level LAMBDAs, smash them into a
;;; top level lambda and component, returning these as values. We use
;;; the first lambda and its component, putting the other code in that
;;; component and deleting the other lambdas.
(defun merge-toplevel-lambdas (lambdas)
  (declare (cons lambdas))
  (let* ((result-lambda (first lambdas))
         (result-return (lambda-return result-lambda)))
    (cond
      ((null (rest lambdas)))
      (result-return
       ;; Make sure the result's return node starts a block so that we
       ;; can splice code in before it.
       (let ((prev (node-prev
                    (lvar-uses (return-result result-return)))))
         (when (ctran-use prev)
           (node-ends-block (ctran-use prev))))

       (dolist (lambda (rest lambdas))
         (merge-1-toplevel-lambda result-lambda lambda)))
      (t
       (dolist (lambda (rest lambdas))
         (setf (functional-entry-fun lambda) nil)
         (delete-component (lambda-component lambda)))))

    (values (lambda-component result-lambda) result-lambda)))
