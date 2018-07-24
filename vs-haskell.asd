; vim: ft=lisp et
(in-package :asdf)

(defsystem "vs-haskell"
  :depends-on
  (
   "trivia"                     ; pattern matcher.
   "alexandria"                 ; pure cl utilities.
   "millet"                     ; implementation dependent utilities.
   "introspect-environment"     ; compiler introspection.
   "closer-mop"                 ; mop wrapper.
   "trivial-types"
   "trestrul"                   ; tree structural list utils.
   "expander"                   ; macroexpand all.
   "cl-unification"             ; unification.
   "curried-function"
   "matrix-case"                ; control flow.
   "lambda-list"                ; tiny utilities for lambda list processing.
   "split-sequence"
   )
  :components
  ((:file "package")
   ;; bottoms
   (:file "util" :depends-on ("package"))
   (:file "newtype" :depends-on ("package"))
   (:file "type-class-core" :depends-on ("package"))
   ;;
   (:file "unify" :depends-on ("package" "newtype"))
   (:file "adt" :depends-on ("util" "unify" "type-class-core"))
   ;;
   (:file "action" :depends-on ("adt"))
   ;;
   (:file "curry" :depends-on ("adt" "action" "type-class-core" "unify"))
   ;;
   (:file "type-class" :depends-on ("action" "curry"))
   )
  )

;; Perform method below is added by JINGOH.GENERATOR.
(defmethod component-depends-on ((o test-op) (c (eql (find-system "vs-haskell"))))
  (append (call-next-method)'((test-op "vs-haskell.test"))))
