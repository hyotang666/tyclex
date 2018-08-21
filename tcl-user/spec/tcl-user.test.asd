; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl-user.test
  :depends-on
  (:jingoh "tcl-user")
  :components
  ((:file "tcl-user"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl-user args)))