; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.adt.test
  :depends-on
  (:jingoh "tyclex.adt")
  :components
  ((:file "tyclex.adt"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.adt args)))