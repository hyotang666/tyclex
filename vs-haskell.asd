; vim: ft=lisp et
(in-package :asdf)
(defsystem "vs-haskell"
  :depends-on
  ("named-readtables" "trivia" "alexandria" "millet" "introspect-environment"
   "closer-mop" "trivial-types" "trestrul" "cl-unification" "expander")
  :components
  ((:file "package")
   ;; bottoms
   (:file "qualified-use" :depends-on ("package"))
   (:file "readtables" :depends-on ("package"))
   ;;
   (:file "adt" :depends-on ("readtables"))
   ;;
   (:file "action" :depends-on ("adt"))
   (:file "type-class" :depends-on ("adt"))
   ))
