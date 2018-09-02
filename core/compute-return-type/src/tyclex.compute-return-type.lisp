(in-package :cl-user)
(defpackage :tyclex.compute-return-type
  (:use :cl)

  (:import-from :tyclex.objects.adt
		#:get-adt #:adt-lambda-list
		#:data-type-of #:adt-value-p #:class-name-of #:adt-type-specifier-p)
  (:import-from :tyclex.objects.adt-constructor
		#:adt-constructor-form-p #:get-adt-constructor
		#:adt-constructor-type-of #:adt-constructor-arg-types)
  (:import-from :tyclex.objects.interface #:interface-form-p #:interface-return-type #:interface-lambda-list)
  (:import-from :tyclex.objects.io-action #:io-form-p #:get-io #:action-type #:io-action-construct-form-p #:io-action-construct-form-return-type)
  (:import-from :tyclex.curry #:expanded-curry-form-p #:expanded-curry-form-arity #:expanded-curry-form-return-type #:function-type-of #:canonicalize-return-type)
  (:import-from :tyclex.type-matcher #:great-common-type)

  (:export
    #:compute-return-type #:compute-return-types
    ))
(in-package :tyclex.compute-return-type)

;;;; COMPUTE-RETURN-TYPE
(define-condition internal-logical-error(cell-error)
  ((datum :initarg :datum :accessor error-datum))
  (:report(lambda(c *standard-output*)
	    (format t "INTERNAL LOGICAL ERROR: ~S~%~S trapped with ~S."
		    (type-of c)
		    (cell-error-name c)
		    (error-datum c)))))
(define-condition exhausts-clauses(internal-logical-error)())
(define-condition unexpected-quote(internal-logical-error)())
(define-condition unknown-special-operator(internal-logical-error)())

(defun compute-return-types(var* &optional env)
  (loop :for var :in var*
	:collect (compute-return-type var env)))

(defun compute-return-type(var &optional env)
  (cond
    ((safe-constantp var env) ; lisp object.
     (constant-return-type var env))
    ((symbolp var) ; free variable.
     (free-variable-return-type var env))
    ((typep var '(cons (cons (eql lambda) *) *)) ; ((lambda(...)...)...)
     (compute-return-type (car(last(cddar var))) env))
    ((Interface-form-p var)
     (interface-form-return-type var))
    ((Io-form-p var)
     (Action-type(Get-io (car var))))
    ((Adt-constructor-form-p var) ; constructor call.
     (constructor-form-return-type var env))
    ((Io-action-construct-form-p var)
     (Io-action-construct-form-return-type var))
    ((Expanded-curry-form-p var)
     (curry-form-return-type var))
    ((and (listp var)
	  (symbolp (car var)))
     (compute-standard-form-return-type var env))
    (t (error 'exhausts-clauses :name 'compute-return-type :datum var))))

;;; Constant value clause
(defun safe-constantp(form env)
  ;; constantp may do macroexpand.
  (let((*macroexpand-hook*(lambda(fn form env)
			    (let((expanded(funcall fn form env)))
			      (if (eq form expanded)
				(return-from safe-constantp nil)
				expanded)))))
    (constantp form env)))

(defun constant-return-type(var &optional env)
  (if(typep var '(CONS (EQL THE)T))
    (second var)
    (let((value(introspect-environment:constant-form-value var env)))
      (if(Adt-value-p value) ; literal adt.
	(Data-type-of value)
	(if(Adt-type-specifier-p value)
	  value
	  (let((type (Class-name-of value)))
	    (if(not(eq 'cons type))
	      type
	      (let((types(handler-case(mapcar #'class-name-of value)
			   (error() ; dot list comes
			     (return-from constant-return-type
					  (tyclex.dsl.defdata::cons-type-specifier(trestrul:mapleaf #'class-name-of value)))))))
		(if(null(cdr(remove-duplicates types)))
		  `(LIST ,(Class-name-of(car value)))
		  (tyclex.dsl.defdata::cons-type-specifier types))))))))))

;;; Free variable clause.
(defun free-variable-return-type(var env)
  (let((type (introspect-environment:variable-type var env)))
    (if(not(eql t type))
      type
      (if(boundp var)
	(Data-type-of (eval var))
	T))))

(defun interface-form-return-type(call-form)
  (let((pattern(Interface-return-type(car call-form)))
       (environment(tyclex.unifier:unify (Interface-lambda-list(car call-form))
					 (tyclex.unifier:enwild (compute-return-types(cdr call-form))))))
    (tyclex.unifier:substitute-pattern pattern environment)))

;;; Adt constructor clause
(defun constructor-form-return-type(var env)
  (let*((adt-constructor(Get-adt-constructor var))
	(type(Adt-constructor-type-of adt-constructor)))
    (if(atom type)
      type
      (cons (car type)
	    (tyclex.unifier:substitute-pattern (Adt-lambda-list (Get-adt type))
					       (tyclex.unifier:unify (Adt-constructor-arg-types adt-constructor)
								     (loop :for v :in (cdr var)
									   :collect (compute-return-type v env))))))))

;;; Curry form clause
(defun curry-form-return-type(var)
  (let((arity(Expanded-curry-form-arity var)))
    (if(null arity)
      (error "Arity did not exist ~S" var)
      (if(= 1 arity)
	`(function * ,(canonicalize-return-type (Expanded-curry-form-return-type var)))
	`(function * function)))))

;;; Standard form
(defun compute-standard-form-return-type(form env)
  (cond
    ((Function-type-of(car form))
     (third(Function-type-of(car form))))
    ((and (eq 'coerce (car form))
	  (constantp (third form)))
     (introspect-environment:constant-form-value(third form)))
    ((and (find (car form)'(map concatenate make-sequence merge):test #'eq)
	  (constantp (second form)))
     (introspect-environment:constant-form-value(second form)))
    ((find (car form) '(reverse nreverse subseq copy-seq fill map-into sort stable-sort replace remove-duplicates delete-duplicates):test #'eq)
     (compute-return-type (cadr form) env))
    ((find (car form) '(substitute substitute-if substitute-if-not nsubstitute nsubstitute-if nsubstitute-if-not):test #'eq)
     (compute-return-type (fourth form)env))
    ((find (car form) '(remove remove-if remove-if-not delete delete-if delete-if-not):test #'eq)
     (compute-return-type (third form)env))
    (t
      (multiple-value-bind(type localp declaration)(introspect-environment:function-information (car form)env)
	(declare(ignore localp))
	(case type
	  ((nil) T) ; undefined function.
	  (:special-form (special-operator-return-type form env))
	  (:macro (compute-return-type (expander:expand (copy-tree form)env)
				       env))
	  (:function
	    (let((ftype(assoc 'ftype declaration)))
	      (if ftype
		(ftype-return-type (cdr ftype))
		T))))))))

(defun ftype-return-type(form)
  (if(symbolp form)
    T
    (canonicalize-return-type(third form))))

;;; Special operator clause
(defun special-operator-return-type(form env)
  (case (car form)
    ((progn progv let let* flet labels lambda setq locally eval-when)
     (compute-return-type(car(last form))env))
    ((the)(canonicalize-return-type(second form)))
    ((unwind-protect multiple-value-prog1 multiple-value-call load-time-value)
     (compute-return-type(second form)env))
    ((tagbody)'null)
    ((function)
     (if(listp (second form))
       `(function * ,(compute-return-type(car(last(cddr(second form))))env))
       (canonicalize-ftype(introspect-environment:function-type(second form)env))))
    ((if)
     (let((then(compute-return-type(third form)env))
	  (else(compute-return-type(fourth form)env)))
       (if then
	 (if else
	   (Great-common-type then else)
	   then)
	 else)))
    ((quote)
     (error 'unexpected-quote :datum form :name 'special-operator-return-type))
    ((macrolet symbol-macrolet)
     (compute-return-type (expander:expand (copy-tree form)env)env))
    ((go throw catch) t) ; give up.
    ((return-from)(compute-return-type (third form)))
    ((block)
     (let((return-types(delete-if (lambda(x)
				    (member x '(t nil null)))
				  (uiop:while-collecting(acc)
				    (acc(compute-return-type(car(last form))env))
				    (trestrul:traverse
				      (lambda(node)
					(when(typep node `(CONS (EQL RETURN-FROM)(CONS (EQL,(second form)) T)))
					  (acc (compute-return-type(third node)env))))
				      form)))))
       (if return-types
	 (if(cdr return-types)
	   (reduce #'Great-common-type return-types)
	   (car return-types))
	 T))) ; give up.
    (otherwise (error 'unknown-special-operator :datum form :name 'special-operator-return-type))))

(defun canonicalize-ftype(ftype)
  (list (first ftype)
	(second ftype)
	(canonicalize-return-type (third ftype))))

