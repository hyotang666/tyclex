; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.compute-return-type.test
  :depends-on
  (:jingoh "tyclex.compute-return-type")
  :components
  ((:file "tyclex.compute-return-type"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.compute-return-type args)))