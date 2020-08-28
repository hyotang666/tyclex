; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.newtype.test
  :version "0.0.1"
  :depends-on
  (:jingoh "tyclex.newtype" "millet")
  :components
  ((:file "tyclex.newtype"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.newtype args)))
