; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.meta-objects.cell.test
  :depends-on
  (:jingoh "tyclex.meta-objects.cell")
  :components
  ((:file "tyclex.meta-objects.cell"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.meta-objects.cell args)))