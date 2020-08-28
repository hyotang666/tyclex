; vim: ft=lisp et
(in-package :asdf)
(defsystem "tcl.state"
  :version "0.0.2"
  :depends-on
  (
   "tcl"
   "tcl.monad"
   )
  :pathname
  "src/"
  :components
  ((:file "tcl.state")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tcl.state"))))
  (append (call-next-method) '((test-op "tcl.state.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tcl.state"))) &rest keys)
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
