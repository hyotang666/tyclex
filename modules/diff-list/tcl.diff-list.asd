; vim: ft=lisp et
(in-package :asdf)
(defsystem "tcl.diff-list"
  :version "0.0.1"
  :depends-on
  (
   "tcl"
   "tcl.monoid"
   "alexandria"
   )
  :pathname
  "src/"
  :components
  ((:file "tcl.diff-list")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tcl.diff-list"))))
  (append (call-next-method) '((test-op "tcl.diff-list.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tcl.diff-list"))) &rest keys)
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
