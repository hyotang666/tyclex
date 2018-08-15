; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.eq.test
  :depends-on
  (:jingoh "tcl.eq")
  :components
  ((:file "tcl.eq"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.eq args)))