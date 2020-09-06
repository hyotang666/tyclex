; vim: ft=lisp et
(in-package :asdf)
(defsystem "tcl-user"
  :version "1.0.0"
  :depends-on
  (
   "tcl"
   "tcl.data"
   "tcl.applicative"
   "tcl.compare"
   "tcl.eq"
   "tcl.io"
   "tcl.monad-plus"
   "tcl.ord"
   "tcl.bounded"
   "tcl.enum"
   "tcl.functor"
   "tcl.monad"
   "tcl.monoid"
   "tcl.zip-list"
   "tcl.writer"
   "tcl.diff-list"
   "tcl.state"
   )
  :pathname
  "src/"
  :components
  ((:file "tcl-user")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "tcl-user").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tcl-user"))))
  (append (call-next-method) '(
                               (test-op "tcl.eq.test")
                               (test-op "tcl.io.test")
                               (test-op "tcl.ord.test")
                               (test-op "tcl.bounded.test")
                               (test-op "tcl.enum.test")
                               (test-op "tcl.compare.test")
                               (test-op "tcl.functor.test")
                               (test-op "tcl.applicative.test")
                               (test-op "tcl.monad.test")
                               (test-op "tcl.monad-plus.test")
                               (test-op "tcl.monoid.test")
                               (test-op "tcl.zip-list.test")
                               (test-op "tcl.writer.test")
                               (test-op "tcl.diff-list.test")
                               (test-op "tcl.state.test")
                               )))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tcl-user")))
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
    (defmethod perform :after ((o load-op) (c (eql (find-system "tcl-user"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
