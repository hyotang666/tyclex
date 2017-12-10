; vim: ft=lisp et
(in-package :asdf)
(defsystem :vs-haskell.test
  :depends-on
  (:jingoh "vs-haskell" "millet")
  :components
  ((:file "vs-haskell")
   (:file "adt"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :vs-haskell)))
