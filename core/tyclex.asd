; vim: ft=lisp et
(in-package :asdf)
(defsystem "tyclex"
  :version "0.0.0"
  :depends-on
  (
   "tyclex.curry"               ; Currying system as tyclex module.
   "tyclex.newtype"             ; Newtype system as tyclex module.
   "tyclex.unifier"             ; Unification system for tyclex module.

   "tyclex.objects"             ; Objects for tyclex modules.

   "tyclex.type-matcher"        ; Type matching system as tyclex module.
   "tyclex.expander"            ; Tyclex own expander.

   "tyclex.compute-return-type" ; Module for computing S-Expression return type.
   "tyclex.dsl"                 ; DSL for tyclex.
   )
  :pathname
  "src/"
  :components
  ((:file "tyclex") 
   ))

;; These forms below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on ((o test-op) (c (eql (find-system "tyclex"))))
  (append (call-next-method) '(
                               (test-op "tyclex.curry.test")
                               (test-op "tyclex.newtype.test")
                               (test-op "tyclex.unifier.test")
                               (test-op "tyclex.objects")
                               (test-op "tyclex.type-matcher.test")
                               (test-op "tyclex.compute-return-type.test")
                               (test-op "tyclex.dsl")
                               )))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tyclex")))
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
               ((o load-op) (c (eql (find-system "tyclex"))) &key)
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
