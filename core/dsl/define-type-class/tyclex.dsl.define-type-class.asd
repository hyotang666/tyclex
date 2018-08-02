; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.dsl.define-type-class"
  :depends-on
  nil
  :pathname
  "src/"
  :components
  ((:file "tyclex.dsl.define-type-class")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.dsl.define-type-class"))))
  (append (call-next-method) '((test-op "tyclex.dsl.define-type-class.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.dsl.define-type-class")))
            &rest keys)
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