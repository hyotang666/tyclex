; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.objects.cell.test
  :depends-on
  (:jingoh "tyclex.objects.cell")
  :components
  ((:file "tyclex.objects.cell"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.objects.cell args)))