; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.type-class-core.test
  :depends-on
  (:jingoh "tyclex.type-class-core")
  :components
  ((:file "tyclex.type-class-core"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.type-class-core args)))