; vim: ft=lisp et
(in-package :asdf)
(defsystem "tcl-user"
  :depends-on
  (
   "tcl"
   "tcl.applicative"
   "tcl.compare"
   "tcl.eq"
   "tcl.io"
   "tcl.monad-plus"
   "tcl.ord"
   "tcl.bounded"
   "tcl.enum"
   "tcl.functor"
   "tcl.monad"
   "tcl.monoid"
   "tcl.zip-list"
   )
  :pathname
  "src/"
  :components
  ((:file "tcl-user")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tcl-user"))))
  (append (call-next-method) '((test-op "tcl.applicative.test")
                               (test-op "tcl.compare.test")
                               (test-op "tcl.eq.test")
                               (test-op "tcl.io.test")
                               (test-op "tcl.monad-plus.test")
                               (test-op "tcl.ord.test")
                               (test-op "tcl.bounded.test")
                               (test-op "tcl.enum.test")
                               (test-op "tcl.functor.test")
                               (test-op "tcl.monad.test")
                               (test-op "tcl.monoid.test")
                               (test-op "tcl.zip-list.test")
                               )))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tcl-user"))) &rest keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
