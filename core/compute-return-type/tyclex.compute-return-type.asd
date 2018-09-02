; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.compute-return-type"
  :depends-on
  (
   "tyclex.objects.adt"
   "tyclex.unifier"
   "tyclex.curry"
   "tyclex.type-matcher"
   "introspect-environment"     ; Wrapper for environment introspection.
   "trestrul"                   ; Utilities for tree structured list.
   "expander"                   ; Macroexpand all.
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.compute-return-type")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.compute-return-type"))))
  (append (call-next-method) '((test-op "tyclex.compute-return-type.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.compute-return-type")))
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
