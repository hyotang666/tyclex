; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.curry.test
  :depends-on
  (:jingoh "tyclex.curry")
  :components
  ((:file "tyclex.curry"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.curry args)))