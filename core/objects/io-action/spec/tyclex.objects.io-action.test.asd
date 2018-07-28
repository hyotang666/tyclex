; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.objects.io-action.test
  :depends-on
  (:jingoh "tyclex.objects.io-action")
  :components
  ((:file "tyclex.objects.io-action"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.objects.io-action args)))