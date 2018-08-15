; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.functor.test
  :depends-on
  (:jingoh "tcl.functor")
  :components
  ((:file "tcl.functor"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.functor args)))