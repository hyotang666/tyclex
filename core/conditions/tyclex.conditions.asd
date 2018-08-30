; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.conditions"
  :depends-on
  nil
  :pathname
  "src/"
  :components
  ((:file "tyclex.conditions")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.conditions"))))
  (append (call-next-method) '((test-op "tyclex.conditions.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.conditions"))) &rest keys)
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