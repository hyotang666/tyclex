; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.objects"
  :depends-on
  (
   "tyclex.objects.adt-constructor"
   "tyclex.objects.instance"
   "tyclex.objects.interface"
   "tyclex.objects.type-class"
   "tyclex.objects.adt"
   "tyclex.objects.io-action"
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.objects")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.objects"))))
  (append (call-next-method) '(
                               (test-op "tyclex.objects.adt-constructor.test")
                               (test-op "tyclex.objects.instance.test")
                               (test-op "tyclex.objects.interface.test")
                               (test-op "tyclex.objects.type-class.test")
                               (test-op "tyclex.objects.io-action.test")
                               (test-op "tyclex.objects.adt.test"))))

(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.objects"))) &rest keys)
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
