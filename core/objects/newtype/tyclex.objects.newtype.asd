; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.objects.newtype"
  :version "0.0.0"
  :depends-on
  nil
  :pathname
  "src/"
  :components
  ((:file "tyclex.objects.newtype")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.objects.newtype"))))
  (append (call-next-method) '((test-op "tyclex.objects.newtype.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.objects.newtype")))
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
