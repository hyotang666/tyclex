(in-package :cl-user)
(defpackage :tyclex.expander
  (:import-from :tyclex.curry
		#:expanded-curry-form-p #:decurry #:curry-form-p #:recurry)
  (:import-from :tyclex.objects.io-action
		#:action-body #:action-lambda-list
		#:get-io #:io-form-p
		#:io-action-construct-form-p #:io-action-construct-form-function-form)
  (:use :cl)
  (:export))
(in-package :tyclex.expander)

(eval-when(:compile-toplevel :load-toplevel :execute)
  (defun cons-type-specifier(list)
    (typecase list
      (null '*)
      (atom (if (eq '* list)
	      list
	      `(eql ,list)))
      (cons `(cons ,(cons-type-specifier (car list))
		   ,(cons-type-specifier (cdr list)))))))

(defun |funcall-expander|(form env)
  (destructuring-bind(op function . args)form
    (let((the (when (and (listp function)
			 (eq 'the (car function)))
		(setf function (third function)) ; as canonicalize.
		(second function))))
      (when(Curry-form-p function)
	(return-from |funcall-expander| (expander:expand (Recurry function args) env)))
      (setf function (expander:expand function env))
      (when(and (listp function)
		(eq 'the (car function)))
	(setf the (second function)
	      function(third function)))
      (cond
	((Expanded-curry-form-p function)
	 (expander:expand (Decurry function args) env))
	((Io-form-p function)
	 (let((io(Get-io(car function))))
	   (if(null (cdr function))
	     (pretty-body (expander:expand* (Action-body io)env) the)
	     (expander:expand `(DESTRUCTURING-BIND,(Action-lambda-list io)(LIST ,@(cdr function))
				 ,@(Action-body io))
			      env))))
	((Io-action-construct-form-p function)
	 (|funcall-expander| `(,(car form) ,(Io-action-construct-form-function-form function),@args)
			     env))
	((typep function'(cons (eql function)(cons symbol null)))
	 `(,(cadr function),@(expander:expand* args env)))
	((typep function '#.(cons-type-specifier '#'(lambda())))
	 (destructuring-bind(function(lambda lambda-list . body))function
	   (declare(ignore function lambda))
	   (if(expander::intersectionp lambda-list lambda-list-keywords :test #'eq)
	     `(,op #'(lambda ,lambda-list ,@body) ,@(expander:expand* args env))
	     (let((binds(mapcar #'list lambda-list args)))
	       (if binds
		 (expander:expand `(let,binds,@body) env)
		 (if(cdr body)
		   `(locally ,@body)
		   (car body)))))))
	((typep function '#.(cons-type-specifier '((lambda()))))
	 (destructuring-bind((lambda lambda-list . body) . actual-args) function
	   (declare(ignore lambda))
	   (if(expander::intersectionp lambda-list lambda-list-keywords :test #'eq)
	     `((lambda,(expander::expand-params lambda-list env),@(expander:expand* body env))
	       ,@(expander:expand* actual-args env))
	     (let((binds(mapcar #'list lambda-list actual-args)))
	       (if binds
		 (multiple-value-bind(binds decls prebody main)(parse-bubble-let `(let,binds,@body))
		   (expander:expand `(let,binds,@decls,@prebody(,op ,main ,@args))env))
		 (if(cdr body)
		   (expander:expand `(,op (locally ,@body),@args) env)
		   (expander:expand `(,op ,(car body) ,@args))))))))
	((typep function '#.(cons-type-specifier '(constantly *)))
	 (let((arg-forms(remove-if (lambda(x)(constantp x env))
				   (expander:expand* args env))))
	   (if arg-forms
	     `(progn ,@arg-forms ,(cadr function))
	     (cadr function))))
	((typep function '#.(cons-type-specifier '(let())))
	 (multiple-value-bind(binds decls prebody main)(expander::parse-bubble-let function)
	   (expander:expand `(let ,binds ,@decls ,@prebody(,op ,main ,@args))env)))
	(t `(,op ,function ,@(expander:expand* args env)))))))

(defun pretty-body(form the)
  (let((return-type (when the
		      (typecase the
			((cons (eql io)*)(second the))
			((cons (eql function)*) (third the))))))
    (if(cdr form)
      (if return-type
	`(the ,return-type (progn ,@form))
	`(locally ,@form))
      (if return-type
	`(the ,return-type ,(car form))
	(car form)))))

(handler-bind((expander:expander-conflict #'expander:use-next))
  (expander:defexpandtable :tyclex
    (:use optimize)
    (:add |funcall-expander| funcall)
    ))

