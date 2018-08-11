; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.test
  :depends-on
  (:jingoh "tcl")
  :components
  ((:file "tcl"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl args)))