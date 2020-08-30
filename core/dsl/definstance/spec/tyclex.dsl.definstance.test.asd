; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.dsl.definstance.test
  :version "0.0.1"
  :depends-on
  (:jingoh "tyclex.dsl.definstance")
  :components
  ((:file "tyclex.dsl.definstance"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.dsl.definstance args)))
