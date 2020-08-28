; vim: ft=lisp et
(in-package :asdf)
(defsystem "tcl.zip-list"
  :version "0.0.2"
  :depends-on
  (
   "tcl"
   "tcl.applicative"
   "tcl.functor"
   "series"
   "alexandria"
   )
  :pathname
  "src/"
  :components
  ((:file "tcl.zip-list")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tcl.zip-list"))))
  (append (call-next-method) '((test-op "tcl.zip-list.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tcl.zip-list"))) &rest keys)
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
