; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.dsl.definstance"
  :depends-on
  (
   "tyclex.unifier"             ; Module for Unification.
   "tyclex.objects.type-class"  ; Module for type-class object.
   "tyclex.objects.interface"   ; Module for interface object.
   "tyclex.objects.instance"    ; Module for cell object.
   "split-sequence"             ; Sequence splitting utilities.
   "trestrul"                   ; Utilities for tree structured list.
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.dsl.definstance")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.dsl.definstance"))))
  (append (call-next-method) '((test-op "tyclex.dsl.definstance.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.dsl.definstance")))
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
