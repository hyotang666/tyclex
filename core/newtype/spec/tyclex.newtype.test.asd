; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.newtype.test
  :depends-on
  (:jingoh "tyclex.newtype")
  :components
  ((:file "tyclex.newtype"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.newtype args)))