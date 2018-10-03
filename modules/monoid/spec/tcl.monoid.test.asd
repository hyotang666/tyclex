; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.monoid.test
  :depends-on
  (:jingoh "tcl.monoid" "tcl" "tcl.compare" "tcl.data")
  :components
  ((:file "tcl.monoid"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.monoid args)))
