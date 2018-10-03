; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.applicative.test
  :depends-on
  (:jingoh "tcl.applicative" "tcl.io" "tcl.data" "tcl")
  :components
  ((:file "tcl.applicative"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.applicative args)))
