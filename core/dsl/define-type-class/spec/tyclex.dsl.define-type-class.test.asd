; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.dsl.define-type-class.test
  :depends-on
  (:jingoh "tyclex.dsl.define-type-class"
           "tyclex.dsl.defio")
  :components
  ((:file "tyclex.dsl.define-type-class"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.dsl.define-type-class args)))
