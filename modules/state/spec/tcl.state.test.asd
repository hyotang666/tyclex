; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.state.test
  :depends-on
  (:jingoh "tcl.state" "tcl.monad" "tcl")
  :components
  ((:file "tcl.state"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.state args)))
