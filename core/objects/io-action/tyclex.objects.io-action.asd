; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.objects.io-action"
  :depends-on
  (
   "closer-mop"         ; Wrapper for meta object protocols.
   "tyclex.newtype"     ; Tyclex module for newtype.
   "tyclex.conditions"
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.objects.io-action")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.objects.io-action"))))
  (append (call-next-method) '((test-op "tyclex.objects.io-action.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.objects.io-action")))
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
