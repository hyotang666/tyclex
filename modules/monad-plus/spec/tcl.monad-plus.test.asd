; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.monad-plus.test
  :depends-on
  (:jingoh "tcl.monad-plus" "tcl.monad")
  :components
  ((:file "tcl.monad-plus"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.monad-plus args)))
