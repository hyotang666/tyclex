; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.unifier.test
  :depends-on
  (:jingoh "tyclex.unifier")
  :components
  ((:file "tyclex.unifier"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.unifier args)))