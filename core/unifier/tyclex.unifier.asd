; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.unifier"
  :depends-on
  (
   "cl-unification"     ; Unification.
   "trestrul"           ; Utilities for tree structured list.
   "millet"             ; Wrapper for implementation dependent utilities.
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.unifier")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.unifier"))))
  (append (call-next-method) '((test-op "tyclex.unifier.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.unifier"))) &rest keys)
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
