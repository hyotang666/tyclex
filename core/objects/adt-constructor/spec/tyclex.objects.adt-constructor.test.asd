; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.objects.adt-constructor.test
  :version "0.0.0"
  :depends-on
  (:jingoh "tyclex.objects.adt-constructor")
  :components
  ((:file "tyclex.objects.adt-constructor"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.objects.adt-constructor args)))
