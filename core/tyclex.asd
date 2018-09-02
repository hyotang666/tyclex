; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex"
  :depends-on
  (
   "tyclex.curry"               ; Currying system as tyclex module.
   "tyclex.newtype"             ; Newtype system as tyclex module.
   "tyclex.unifier"             ; Unification system for tyclex module.

   "tyclex.objects"             ; Objects for tyclex modules.

   "tyclex.type-matcher"        ; Type matching system as tyclex module.

   "tyclex.compute-return-type" ; Module for computing S-Expression return type.
   "tyclex.dsl"                 ; DSL for tyclex.
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex") 
   ))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on ((o test-op) (c (eql (find-system "tyclex"))))
  (append (call-next-method) '(
                               (test-op "tyclex.curry.test")
                               (test-op "tyclex.newtype.test")
                               (test-op "tyclex.unifier.test")
                               (test-op "tyclex.objects")
                               (test-op "tyclex.type-matcher.test")
                               (test-op "tyclex.compute-return-type.test")
                               (test-op "tyclex.dsl")
                               )))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex"))) &rest keys)
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
