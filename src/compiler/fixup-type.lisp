(in-package "SB-C")

(!begin-collecting-cold-init-forms)

(!cold-init-forms
 #-sb-xc-host
 (dovector (saetp sb-vm:*specialized-array-element-type-properties*)
   (setf (aref sb-vm::*saetp-widetag-ctype* (ash (- (sb-vm:saetp-typecode saetp) 128) -2))
         (sb-vm:saetp-ctype saetp)))
  ;; This seems so weird and random. I really wanted to remove it, but
  ;; adding the :BUILTIN property makes sense because without it, the type
  ;; does not have a unique parse. Probably a missing entry in one of the
  ;; pieces of type machinery voodoo.
  ;; Unfortunately, there are lots of other non-unique parses amongst the
  ;; standardized specifiers, most notably LIST which can be internalized
  ;; as either permutation of (OR (MEMBER NIL) CONS), and ATOM which
  ;; might create a new instance of (NOT CONS) on each parse.
  ;;
  ;; Changing the :KIND from :DEFINED to :PRIMITIVE makes sense too so that
  ;; it can't be redefined. (Package lock on CL prevents that anyway)
  ;; But some of of these properties are seemingly redundant with each other-
  ;; :BUILTIN essentially means :PRIMITIVE in this case.
 (let ((spec 'compiled-function))
   (setf (info :type :builtin spec) (specifier-type spec)
         (info :type :kind spec) :primitive)))

(!defun-from-collected-cold-init-forms !fixup-type-cold-init)
