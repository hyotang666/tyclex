; vim: ft=lisp et
(in-package :asdf)
(defsystem :tcl.writer.test
  :version "0.0.1"
  :depends-on
  (:jingoh "tcl.writer" "tcl.monad" "tcl.monoid")
  :components
  ((:file "tcl.writer"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tcl.writer args)))
