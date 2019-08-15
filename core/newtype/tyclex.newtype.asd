; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.newtype"
  :version "0.0.1"
  :depends-on
  (
   "tyclex.objects.newtype"
   "tyclex.compute-return-type"
   "tyclex.type-matcher"
   "alexandria"
   "millet"
   "lambda-fiddle"
   "expander"
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.newtype")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.newtype"))))
  (append (call-next-method) '((test-op "tyclex.newtype.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.newtype"))) &rest keys)
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
