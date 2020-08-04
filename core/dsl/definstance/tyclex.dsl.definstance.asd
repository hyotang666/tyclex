; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.dsl.definstance"
  :version "0.1.0"
  :depends-on
  (
   "tyclex.unifier"             ; Module for Unification.
   "tyclex.objects.type-class"  ; Module for type-class object.
   "tyclex.objects.interface"   ; Module for interface object.
   "tyclex.objects.instance"    ; Module for cell object.
   "tyclex.conditions"
   "split-sequence"             ; Sequence splitting utilities.
   "trestrul"                   ; Utilities for tree structured list.
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.dsl.definstance")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "tyclex.dsl.definstance").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.dsl.definstance"))))
  (append (call-next-method) '((test-op "tyclex.dsl.definstance.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.dsl.definstance")))
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
               ((o load-op) (c (eql (find-system "tyclex.dsl.definstance"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
