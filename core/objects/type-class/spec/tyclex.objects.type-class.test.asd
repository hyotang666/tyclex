; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.objects.type-class.test
  :version "0.0.0"
  :depends-on
  (:jingoh "tyclex.objects.type-class")
  :components
  ((:file "tyclex.objects.type-class"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.objects.type-class args)))
