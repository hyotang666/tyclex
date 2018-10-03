; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.type-matcher.test
  :depends-on
  (:jingoh "tyclex.type-matcher" "millet")
  :components
  ((:file "tyclex.type-matcher"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.type-matcher args)))
