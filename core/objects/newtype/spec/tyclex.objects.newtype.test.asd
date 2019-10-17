; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.objects.newtype.test
  :version "0.1.1"
  :depends-on
  (:jingoh "tyclex.objects.newtype")
  :components
  ((:file "tyclex.objects.newtype"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.objects.newtype args)))
