; vim: ft=lisp et
(in-package :asdf)
(defsystem "vs-haskell"
  :depends-on
  ("named-readtables" "trivia" "alexandria" "millet" "introspect-environment"
   "closer-mop" "trivial-types" "trestrul" "cl-unification" "expander")
  :components
  ((:file "package")
   ;; bottoms
   (:file "util" :depends-on ("package"))
   (:file "qualified-use" :depends-on ("package"))
   (:file "readtables" :depends-on ("package"))
   (:file "unify" :depends-on ("package"))
   ;;
   (:file "adt" :depends-on ("readtables" "util" "unify"))
   ;;
   (:file "action" :depends-on ("adt"))
   (:file "type-class" :depends-on ("adt"))
   ))

;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "vs-haskell"))))
  (test-system :vs-haskell.test))
(defmethod operate :around ((o test-op)(c (eql (find-system "vs-haskell")))
                            &rest keys)
  (let((verbose(getf keys :examine))
       (on-fails(getf keys :on-fails)))
    (declare(special verbose on-fails))
    (call-next-method)))
                                
