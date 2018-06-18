; vim: ft=lisp et
(in-package :asdf)

(defsystem "vs-haskell"
  :depends-on
  (
   "named-readtables"
   "trivia"                     ; pattern matcher.
   "alexandria"                 ; pure cl utilities.
   "millet"                     ; implementation dependent utilities.
   "introspect-environment"     ; compiler introspection.
   "closer-mop"                 ; mop wrapper.
   "trivial-types"
   "trestrul"                   ; tree structural list utils.
   "agnostic-lizard"            ; macroexpand all.
   "expander"                   ; macroexpand all.
   "cl-unification"             ; unification.
   "curried-function"
   )
  :components
  ((:file "package")
   ;; bottoms
   (:file "util" :depends-on ("package"))
   (:file "qualified-use" :depends-on ("package"))
   (:file "readtables" :depends-on ("package"))
   (:file "haskell" :depends-on ("package"))
   (:file "newtype" :depends-on ("package"))
   ;;
   (:file "unify" :depends-on ("package" "newtype"))
   (:file "adt" :depends-on ("readtables" "util" "unify"))
   ;;
   (:file "action" :depends-on ("adt"))
   (:file "type-class" :depends-on ("adt"))
   )
  )

;; Perform method below is added by JINGOH.GENERATOR.
(defmethod component-depends-on ((o test-op) (c (eql (find-system "vs-haskell"))))
  (append (call-next-method)'((test-op "vs-haskell.test"))))
