; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.objects.adt.test
  :version "0.0.0"
  :depends-on
  (:jingoh "tyclex.objects.adt")
  :components
  ((:file "tyclex.objects.adt"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.objects.adt args)))
