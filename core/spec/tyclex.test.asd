; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.test
  :depends-on
  (:jingoh "tyclex")
  :components
  ((:file "tyclex"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex args)))