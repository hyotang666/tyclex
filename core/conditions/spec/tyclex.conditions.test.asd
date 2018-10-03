; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.conditions.test
  :depends-on
  (:jingoh "tyclex.conditions")
  :components
  ((:file "tyclex.conditions"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.conditions args)))