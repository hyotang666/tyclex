; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.dsl.test
  :depends-on
  (:jingoh "tyclex.dsl")
  :components
  ((:file "tyclex.dsl"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.dsl args)))