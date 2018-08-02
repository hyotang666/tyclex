; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.dsl.defdata"
  :depends-on
  (
   "tyclex.objects.adt"         ; Module for adt objects.
   "tyclex.objects.type-class"  ; Module for type-class objects.
   "tyclex.objects.instance"    ; Module for instance objects.
   "tyclex.type-matcher"        ; Module for type matching.
   "tyclex.dsl.definstance"     ; Module for dsl definstance.
   "millet"                     ; Wrapper for implementation dependent utilities.
   "introspect-environment"     ; Wrapper for environment introspection.
   "alexandria"                 ; Public domain utilities.
   "trivia"                     ; Pattern matcher.
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.dsl.defdata")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.dsl.defdata"))))
  (append (call-next-method) '((test-op "tyclex.dsl.defdata.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.dsl.defdata")))
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
