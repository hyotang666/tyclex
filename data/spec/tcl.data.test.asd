; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.data.test
  :depends-on
  (:jingoh "tcl.data")
  :components
  ((:file "tcl.data"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.data args)))