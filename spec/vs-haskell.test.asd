; vim: ft=lisp et
(in-package :asdf)
(defsystem :vs-haskell.test
  :depends-on
  (:jingoh "vs-haskell" "millet" "curried-function" "incf-cl")
  :components
  ((:file "vs-haskell")
   ;; Mid
   (:file "adt" :depends-on ("vs-haskell"))
   (:file "type-class" :depends-on ("vs-haskell")))
  :perform
  (test-op(o c)
    (symbol-call :jingoh :examine :vs-haskell)))

(defmethod perform :before
  ((o compile-op)(c (eql (find-component :vs-haskell.test "type-class"))))
  (symbol-call :jingoh.org :delete-subject
               (find-symbol "DEFINE-TYPE-CLASS" "VS-HASKELL.SPEC")
               (symbol-call :jingoh.org :find-org :vs-haskell)))
