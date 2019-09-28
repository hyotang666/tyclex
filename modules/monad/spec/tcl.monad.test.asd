; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.monad.test
  :version "0.0.0"
  :depends-on
  (:jingoh "tcl.monad")
  :components
  ((:file "tcl.monad"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.monad args)))
