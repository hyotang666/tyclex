; vim: ft=lisp et
(in-package :asdf)
(defsystem :vs-haskell.test
  :depends-on
  (:jingoh "vs-haskell" "millet" "curried-function")
  :components
  ((:file "vs-haskell")
   ;; Mid
   (:file "adt" :depends-on ("vs-haskell"))
   (:file "type-class" :depends-on ("vs-haskell")))
  :perform
  (test-op(o c)
    (declare(special verbose on-fails))
    (symbol-call :jingoh :examine :vs-haskell :verbose verbose :on-fails on-fails)))
