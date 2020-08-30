; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.compute-return-type"
  :version "0.0.6"
  :depends-on
  (
   "tyclex.objects.newtype"
   "tyclex.objects.adt"
   "tyclex.objects.interface"
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

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "tyclex.compute-return-type").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.compute-return-type"))))
  (append (call-next-method) '((test-op "tyclex.compute-return-type.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.compute-return-type")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
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
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod perform :after
               ((o load-op)
                (c (eql (find-system "tyclex.compute-return-type"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
