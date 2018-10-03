; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.zip-list.test
  :depends-on
  (:jingoh "tcl.zip-list" "tcl.applicative" "tcl")
  :components
  ((:file "tcl.zip-list"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.zip-list args)))
