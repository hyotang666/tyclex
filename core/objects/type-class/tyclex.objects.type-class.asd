; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.objects.type-class"
  :depends-on
  (
   "tyclex.conditions"
   "tyclex.objects.interface"
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.objects.type-class")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.objects.type-class"))))
  (append (call-next-method) '((test-op "tyclex.objects.type-class.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.objects.type-class")))
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
