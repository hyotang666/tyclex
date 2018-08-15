; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.enum.test
  :depends-on
  (:jingoh "tcl.enum")
  :components
  ((:file "tcl.enum"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.enum args)))