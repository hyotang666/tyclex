; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.objects.newtype"
  :version "1.0.0"
  :depends-on
  (
   "tyclex.conditions"
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.objects.newtype")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "tyclex.objects.newtype").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.objects.newtype"))))
  (append (call-next-method) '((test-op "tyclex.objects.newtype.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.objects.newtype")))
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
               ((o load-op) (c (eql (find-system "tyclex.objects.newtype"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
