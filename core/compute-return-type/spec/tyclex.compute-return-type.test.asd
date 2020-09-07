; vim: ft=lisp et
(in-package :asdf)
(defsystem :tyclex.compute-return-type.test
  :version "0.0.3"
  :depends-on
  (:jingoh "tyclex.compute-return-type"
           :expander ; Macroexpand-all.
           )
  :components
  ((:file "tyclex.compute-return-type"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tyclex.compute-return-type args)))
