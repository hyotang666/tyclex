; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.io.test
  :depends-on
  (:jingoh "tyclex.io")
  :components
  ((:file "tyclex.io"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.io args)))