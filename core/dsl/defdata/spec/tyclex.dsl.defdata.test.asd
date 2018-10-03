; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.dsl.defdata.test
  :depends-on
  (:jingoh "tyclex.dsl.defdata")
  :components
  ((:file "tyclex.dsl.defdata"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.dsl.defdata args)))