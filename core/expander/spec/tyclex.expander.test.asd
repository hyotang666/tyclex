; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.expander.test
  :depends-on
  (:jingoh "tyclex.expander")
  :components
  ((:file "tyclex.expander"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.expander args)))