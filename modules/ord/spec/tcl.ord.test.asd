; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.ord.test
  :depends-on
  (:jingoh "tcl.ord")
  :components
  ((:file "tcl.ord"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.ord args)))