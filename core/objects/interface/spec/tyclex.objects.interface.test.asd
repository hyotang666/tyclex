; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.objects.interface.test
  :depends-on
  (:jingoh "tyclex.objects.interface")
  :components
  ((:file "tyclex.objects.interface"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.objects.interface args)))