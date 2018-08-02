; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.type-matcher"
  :depends-on
  (
   "tyclex.newtype"     ; Module for newtype.
   "tyclex.adt"         ; Module for algebraic data types.
   "tyclex.unifier"     ; Module for unification.
   "tyclex.objects.adt" ; Module for adt objects.
   "millet"             ; Wrapper for implementation dependent utilities.
   "trestrul"           ; Tiny utilities for tree structured list.
   "matrix-case"        ; Tiny utilities for control flow.
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.type-matcher")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.type-matcher"))))
  (append (call-next-method) '((test-op "tyclex.type-matcher.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.type-matcher")))
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
