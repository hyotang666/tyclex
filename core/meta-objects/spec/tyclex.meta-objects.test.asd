; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.meta-objects.test
  :depends-on
  (:jingoh "tyclex.meta-objects")
  :components
  ((:file "tyclex.meta-objects"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.meta-objects args)))