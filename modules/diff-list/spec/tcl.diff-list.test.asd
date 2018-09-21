; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.diff-list.test
  :depends-on
  (:jingoh "tcl.diff-list")
  :components
  ((:file "tcl.diff-list"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.diff-list args)))