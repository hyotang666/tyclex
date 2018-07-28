; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.objects.test
  :depends-on
  (:jingoh "tyclex.objects")
  :components
  ((:file "tyclex.objects"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.objects args)))