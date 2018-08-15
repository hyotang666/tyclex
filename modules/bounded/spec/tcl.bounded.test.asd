; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.bounded.test
  :depends-on
  (:jingoh "tcl.bounded")
  :components
  ((:file "tcl.bounded"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.bounded args)))