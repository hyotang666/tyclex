; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.objects.adt"
  :version "0.0.3"
  :depends-on
  (
   "tyclex.conditions"
   "tyclex.unifier"
   "tyclex.objects.adt-constructor"
   "tyclex.objects.io-action"
   "millet"                             ; Wrapper for implementation dependent utilities.
   "introspect-environment"             ; Wrapper for environment introspection.
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.objects.adt")))

;; These forms below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.objects.adt"))))
  (append (call-next-method) '((test-op "tyclex.objects.adt.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.objects.adt")))
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
               ((o load-op) (c (eql (find-system "tyclex.objects.adt"))) &key)
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
