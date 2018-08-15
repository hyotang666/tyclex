; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.io.test
  :depends-on
  (:jingoh "tcl.io")
  :components
  ((:file "tcl.io"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.io args)))