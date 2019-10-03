; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.objects.instance.test
  :version "0.0.0"
  :depends-on
  (:jingoh "tyclex.objects.instance")
  :components
  ((:file "tyclex.objects.instance"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.objects.instance args)))
