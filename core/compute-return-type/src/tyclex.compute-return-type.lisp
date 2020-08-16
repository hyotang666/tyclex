(in-package :cl-user)

(defpackage :tyclex.compute-return-type
  (:use :cl)
  (:shadowing-import-from :tyclex.objects.newtype #:list)
  (:import-from :tyclex.objects.adt
                #:get-adt
                #:adt-lambda-list
                #:cons-type-specifier
                #:data-type-of
                #:adt-value-p
                #:class-name-of
                #:adt-type-specifier-p)
  (:import-from :tyclex.objects.adt-constructor
                #:adt-constructor-form-p
                #:get-adt-constructor
                #:adt-constructor-type-of
                #:adt-constructor-arg-types)
  (:import-from :tyclex.objects.interface
                #:interface-form-p
                #:interface-return-type
                #:interface-lambda-list)
  (:import-from :tyclex.objects.io-action
                #:io-form-p
                #:get-io
                #:action-type
                #:io-action-construct-form-p
                #:io-action-construct-form-return-type)
  (:import-from :tyclex.curry
                #:expanded-curry-form-p
                #:expanded-curry-form-arity
                #:expanded-curry-form-return-type
                #:function-type-of
                #:canonicalize-return-type)
  (:import-from :tyclex.type-matcher #:great-common-type)
  (:export #:compute-return-type #:compute-return-types))

(in-package :tyclex.compute-return-type)

;;;; COMPUTE-RETURN-TYPE

(define-condition internal-logical-error (cell-error)
  ((datum :initarg :datum :accessor error-datum))
  (:report
   (lambda (c *standard-output*)
     (format t "INTERNAL LOGICAL ERROR: ~S~%~S trapped with ~S." (type-of c)
             (cell-error-name c) (error-datum c)))))

(define-condition exhausts-clauses (internal-logical-error) ())

(define-condition unexpected-quote (internal-logical-error) ())

(define-condition unknown-special-operator (internal-logical-error) ())

(defun compute-return-types (var* &optional env)
  (loop :for var :in var*
        :collect (compute-return-type var env)))

(defun compute-return-type (var &optional env)
  (cond
   ((safe-constantp var env) ; lisp object.
    (constant-return-type var env))
   ((symbolp var) ; free variable.
    (free-variable-return-type var env))
   ((typep var '(cons (cons (eql lambda) *) *)) ; ((lambda(...)...)...)
    (compute-return-type (car (last (cddar var))) env))
   ((interface-form-p var) (interface-form-return-type var))
   ((io-form-p var) (action-type (get-io (car var))))
   ((adt-constructor-form-p var) ; constructor call.
    (constructor-form-return-type var env))
   ((io-action-construct-form-p var)
    (io-action-construct-form-return-type var))
   ((expanded-curry-form-p var) (curry-form-return-type var))
   #+sbcl
   ((typep var '(cons (eql sb-impl::make-hash-table-using-defaults)))
    'hash-table)
   ((and (listp var) (symbolp (car var)))
    (compute-standard-form-return-type var env))
   (t (error 'exhausts-clauses :name 'compute-return-type :datum var))))

;;; Constant value clause

(defun safe-constantp (form env)
  ;; constantp may do macroexpand.
  (let ((*macroexpand-hook*
         (lambda (fn form env)
           (let ((expanded (funcall fn form env)))
             (if (eq form expanded)
                 (return-from safe-constantp nil)
                 expanded)))))
    (constantp form env)))

(defun constant-return-type (var &optional env)
  (if (typep var '(cons (eql the) t))
      (second var)
      (let ((value (introspect-environment:constant-form-value var env)))
        (if (adt-value-p value) ; literal adt.
            (data-type-of value)
            (if (adt-type-specifier-p value)
                value
                (let ((type (class-name-of value)))
                  (if (not (eq 'cons type))
                      type
                      (let ((types
                             (handler-case (mapcar #'class-name-of value)
                               (error () ; dot list comes
                                 (return-from constant-return-type
                                   (cons-type-specifier
                                     (trestrul:mapleaf #'class-name-of
                                                       value)))))))
                        (if (null (cdr (remove-duplicates types)))
                            `(list ,(class-name-of (car value)))
                            (cons-type-specifier types))))))))))

;;; Free variable clause.

(defun free-variable-return-type (var env)
  (let ((type (introspect-environment:variable-type var env)))
    (if (not (eql t type))
        type
        (if (boundp var)
            (data-type-of (eval var))
            t))))

(defun interface-form-return-type (call-form)
  (let ((pattern (interface-return-type (car call-form)))
        (environment
         (tyclex.unifier:unify (interface-lambda-list (car call-form))
                               (tyclex.unifier:enwild
                                 (compute-return-types (cdr call-form))))))
    (tyclex.unifier:substitute-pattern pattern environment)))

;;; Adt constructor clause

(defun constructor-form-return-type (var env)
  (let* ((adt-constructor (get-adt-constructor var))
         (type (adt-constructor-type-of adt-constructor)))
    (if (atom type)
        type
        (cons (car type)
              (tyclex.unifier:substitute-pattern
                (adt-lambda-list (get-adt type))
                (tyclex.unifier:unify
                  (adt-constructor-arg-types adt-constructor)
                  (loop :for v :in (cdr var)
                        :collect (compute-return-type v env))))))))

;;; Curry form clause

(defun curry-form-return-type (var)
  (let ((arity (expanded-curry-form-arity var)))
    (if (null arity)
        (error "Arity did not exist ~S" var)
        (if (= 1 arity)
            `(function *
              ,(canonicalize-return-type
                 (expanded-curry-form-return-type var)))
            `(function * function)))))

;;; Standard form

(defvar *cl-strict-return-type-computers* (make-hash-table :test #'eq))

;; setup.

(macrolet ((def (name &body body)
             (if (symbolp name)
                 (let ((n (make-symbol (symbol-name name))))
                   `(setf (gethash ',name *cl-strict-return-type-computers*)
                            (flet ((,n ,@body
                                     ))
                              #',n)))
                 `(setf ,@(loop :for name :in name
                                :for n = (make-symbol (symbol-name name))
                                :collect `(gethash ',name
                                                   *cl-strict-return-type-computers*)
                                :collect `(flet ((,n ,@body
                                                   ))
                                            #',n))))))
  (def coerce (form env)
   (when (constantp (third form) env)
     (introspect-environment:constant-form-value (third form) env)))
  (def (map concatenate make-sequence merge) (form env)
   (when (constantp (second form) env)
     (introspect-environment:constant-form-value (second form) env)))
  (def
   (reverse nreverse subseq copy-seq fill map-into sort stable-sort replace
            remove-duplicates delete-duplicates print princ prin1 write)
   (form env) (compute-return-type (cadr form) env))
  (def
   (substitute substitute-if substitute-if-not nsubstitute nsubstitute-if
               nsubstitute-if-not)
   (form env) (compute-return-type (fourth form) env))
  (def (remove remove-if remove-if-not delete delete-if delete-if-not)
   (form env) (compute-return-type (third form) env))
  (def (funcall apply) (form env)
   (if (and (expanded-curry-form-p (second form))
            (= (length (cddr form)) (expanded-curry-form-arity (second form))))
       (canonicalize-return-type
         (expanded-curry-form-return-type (second form)))
       (compute-function-form-return-type (second form) env)))
  (def cons (form env) `(cons ,@(compute-return-types (cdr form) env))))

(defun compute-standard-form-return-type (form env)
  (cond ((function-type-of (car form)) (third (function-type-of (car form))))
        ((funcall
           (gethash (car form) *cl-strict-return-type-computers*
                    (constantly nil))
           form env))
        (t
         (multiple-value-bind (type localp declaration)
             (introspect-environment:function-information (car form) env)
           (declare (ignore localp))
           (case type
             ((nil) t) ; undefined function.
             (:special-form (special-operator-return-type form env))
             (:macro
              (compute-return-type (expander:expand (copy-tree form) env) env))
             (:function
              (let ((ftype (assoc 'ftype declaration)))
                (if ftype
                    (ftype-return-type (cdr ftype))
                    t))))))))

(defun ftype-return-type (form)
  (if (symbolp form)
      t
      (canonicalize-return-type (third form))))

;;; Special operator clause

(defvar *special-operator-return-type-computers* (make-hash-table :test #'eq))

;; setup.

(macrolet ((def (name &body body)
             (if (symbolp name)
                 `(setf (gethash ',name
                                 *special-operator-return-type-computers*)
                          (lambda ,@body))
                 `(setf ,@(loop :for name :in name
                                :collect `(gethash ',name
                                                   *special-operator-return-type-computers*)
                                :collect `(lambda ,@body))))))
  (def (let let*) (form env)
   (multiple-value-bind (body decls)
       (alexandria:parse-body (cddr form))
     (compute-return-type (car (last body))
                          (sb-cltl2:augment-environment env
                                                        :variable (mapcar
                                                                    #'alexandria:ensure-car
                                                                    (second
                                                                      form))
                                                        :declare (alexandria:mappend
                                                                   #'cdr
                                                                   decls)))))
  (def (flet labels) (form env)
   (multiple-value-bind (body decls)
       (alexandria:parse-body (cddr form))
     (compute-return-type (car (last body))
                          (handler-bind ((style-warning #'muffle-warning))
                            (sb-cltl2:augment-environment env
                                                          :function (mapcar
                                                                      #'car
                                                                      (second
                                                                        form))
                                                          :declare (alexandria:mappend
                                                                     #'cdr
                                                                     decls))))))
  (def symbol-macrolet (form env)
   (multiple-value-bind (body decls)
       (alexandria:parse-body (cddr form))
     (compute-return-type (car (last body))
                          (sb-cltl2:augment-environment env
                                                        :symbol-macro (second
                                                                        form)
                                                        :declare (alexandria:mappend
                                                                   #'cdr
                                                                   decls)))))
  (def macrolet (form env)
   (multiple-value-bind (body decls)
       (alexandria:parse-body (cddr form))
     (compute-return-type (car (last body))
                          (sb-cltl2:augment-environment env
                                                        :macro (loop :for definition
                                                                          :in (second
                                                                                form)
                                                                     :collect (sb-cltl2:enclose
                                                                                (multiple-value-call
                                                                                    #'sb-cltl2:parse-macro
                                                                                  (values-list
                                                                                    definition)
                                                                                  env)
                                                                                env))
                                                        :declare (alexandria:mappend
                                                                   #'cdr
                                                                   decls)))))
  (def locally (form env)
   (multiple-value-bind (body decls)
       (alexandria:parse-body (cdr form))
     (compute-return-type (car (last body))
                          (sb-cltl2:augment-environment env
                                                        :variable (decls-variables
                                                                    decls)
                                                        :declare (alexandria:mappend
                                                                   #'cdr
                                                                   decls)))))
  (def lambda (form env)
   (destructuring-bind
       (lambda-list . body)
       (cdr form)
     (multiple-value-bind (body decls)
         (alexandria:parse-body body)
       `(function *
         ,(compute-return-type (car (last body))
                               (sb-cltl2:augment-environment env
                                                             :variable (lambda-fiddle:extract-all-lambda-vars
                                                                         lambda-list)
                                                             :declare (alexandria:mappend
                                                                        #'cdr
                                                                        decls)))))))
  (def (progn progv setq eval-when) (form env)
   (compute-return-type (car (last form)) env))
  (def the (form env) (declare (ignore env))
   (canonicalize-return-type (second form)))
  (def (unwind-protect multiple-value-prog1 load-time-value) (form env)
   (compute-return-type (second form) env))
  (def multiple-value-call (form env)
   (compute-function-form-return-type (second form) env))
  (def tagbody (form env) (declare (ignore form env)) 'null)
  (def function (form env)
   (if (typep (second form) '(cons (eql lambda) (cons * *)))
       (special-operator-return-type (second form) env)
       (canonicalize-ftype
         (or (function-type-of (second form))
             (introspect-environment:function-type (second form) env)))))
  (def if (form env)
   (let ((then (compute-return-type (third form) env))
         (else (compute-return-type (fourth form) env)))
     (if then
         (if else
             (great-common-type then else)
             then)
         else)))
  (def quote (form env) (declare (ignore form env)) 'list)
  (def (go throw catch) (form env) (declare (ignore form env)) t)
  (def return-from (form env) (compute-return-type (third form) env))
  (def block (form env)
   (let ((return-types
          (delete-if (lambda (x) (member x '(t nil null)))
                     (uiop:while-collecting (acc)
                       (acc (compute-return-type (car (last form)) env))
                       (trestrul:traverse
                         (lambda (node)
                           (when (typep node
                                        `(cons (eql return-from)
                                               (cons (eql ,(second form)) t)))
                             (acc (compute-return-type (third node) env))))
                         form)))))
     (if return-types
         (if (cdr return-types)
             (reduce #'great-common-type return-types)
             (car return-types))
         t))) ; give up.
  )

(defun decls-variables (decls)
  (loop :for decl :in decls
        :nconc (loop :for option :in (cdr decl)
                     :append (case (car option)
                               ((type) (cddr option))
                               ((ignore ignorable) (cdr option))))))

(defun special-operator-return-type (form env)
  (funcall
    (gethash (car form) *special-operator-return-type-computers*
             (lambda (&rest args)
               (declare (ignore args))
               (error 'unknown-special-operator
                      :datum form
                      :name 'special-operator-return-type)))
    form env))

(defun canonicalize-ftype (ftype)
  (list (first ftype) (second ftype) (canonicalize-return-type (third ftype))))

(defun compute-function-form-return-type (form env)
  (cond
   ((symbolp form)
    (let ((decls
           (nth-value 2
                      (introspect-environment:variable-information form env))))
      (if decls
          (canonicalize-return-type (third (cdr (assoc :type decls))))
          t)))
   ((typep form '(cons (or (eql quote) (eql function)) (cons symbol null)))
    (canonicalize-return-type
      (third (introspect-environment:function-type (cadr form) env))))
   (t (third (compute-return-type form env)))))
