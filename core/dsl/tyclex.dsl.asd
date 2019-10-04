; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex.dsl"
  :version "0.0.0"
  :depends-on
  (
   "tyclex.dsl.defio"
   "tyclex.dsl.definstance"
   "tyclex.dsl.defdata"
   "tyclex.dsl.define-type-class"
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex.dsl")))

;; These forms below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "tyclex.dsl"))))
  (append (call-next-method) '((test-op "tyclex.dsl.defio.test")
                               (test-op "tyclex.dsl.definstance.test")
                               (test-op "tyclex.dsl.defdata.test")
                               (test-op "tyclex.dsl.define-type-class.test")
                               )))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex.dsl")))
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
               ((o load-op) (c (eql (find-system "tyclex.dsl"))) &key)
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
