; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.compare.test
  :depends-on
  (:jingoh "tcl.compare")
  :components
  ((:file "tcl.compare"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.compare args)))