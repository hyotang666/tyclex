; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.dsl.defio.test
  :depends-on
  (:jingoh "tyclex.dsl.defio")
  :components
  ((:file "tyclex.dsl.defio"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.dsl.defio args)))