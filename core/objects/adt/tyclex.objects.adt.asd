; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.objects.adt"
  :version "0.0.0"
  :depends-on
  (
   "tyclex.conditions"
   "tyclex.unifier"
   "tyclex.objects.adt-constructor"
   "tyclex.objects.io-action"
   "millet"                             ; Wrapper for implementation dependent utilities.
   "introspect-environment"             ; Wrapper for environment introspection.
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.objects.adt")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.objects.adt"))))
  (append (call-next-method) '((test-op "tyclex.objects.adt.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.objects.adt")))
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
