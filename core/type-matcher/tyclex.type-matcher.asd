; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.type-matcher"
  :version "0.0.0"
  :depends-on
  (
   "tyclex.objects.newtype"     ; Module for newtype.
   "tyclex.unifier"     ; Module for unification.
   "tyclex.objects.adt" ; Module for adt objects.
   "millet"             ; Wrapper for implementation dependent utilities.
   "closer-mop"         ; Wrapper for meta object protocols.
   "trestrul"           ; Tiny utilities for tree structured list.
   "matrix-case"        ; Tiny utilities for control flow.
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.type-matcher")))

;; These forms below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.type-matcher"))))
  (append (call-next-method) '((test-op "tyclex.type-matcher.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.type-matcher")))
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
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod operate :around
               ((o load-op) (c (eql (find-system "tyclex.type-matcher"))) &key)
      (let* ((seen nil)
             (*default-pathname-defaults*
              (merge-pathnames "spec/" (system-source-directory c)))
             (*macroexpand-hook*
              (let ((outer-hook *macroexpand-hook*))
                (lambda (expander form env)
                  (if (not (typep form '(cons (eql defpackage) *)))
                      (funcall outer-hook expander form env)
                      (if (find (cadr form) seen :test #'string=)
                          (funcall outer-hook expander form env)
                          (progn
                           (push (cadr form) seen)
                           `(progn
                             ,form
                             ,@(symbol-call :jingoh.documentizer :importer
                                            form)))))))))
        (call-next-method)))))
