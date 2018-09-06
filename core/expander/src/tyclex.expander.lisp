(in-package :cl-user)
(defpackage :tyclex.expander
  (:import-from :tyclex.curry
		#:expanded-curry-form-p #:decurry)
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
  (let*((function (second form))
	(args (cddr form))
	(the (when (and (listp function)
			(eq 'the (car function)))
	       (setf function (third function)) ; as canonicalize.
	       (second function)))
	(expanded-function (expander:expand function env)))
    (cond
      ((Expanded-curry-form-p expanded-function)
       (expander:expand (Decurry expanded-function args) env))
      ((Io-form-p expanded-function)
       (let((io(Get-io(car expanded-function))))
	 (if(null (cdr expanded-function))
	   (pretty-body (expander:expand* (Action-body io)env) the)
	   (expander:expand `(DESTRUCTURING-BIND,(Action-lambda-list io)(LIST ,@(cdr expanded-function))
			       ,@(Action-body io))
			    env))))
      ((Io-action-construct-form-p expanded-function)
       (|funcall-expander| `(,(car form) ,(Io-action-construct-form-function-form expanded-function),@args)
			   env))
      ((or (typep expanded-function '(cons (eql quote)(cons symbol null)))
	   (typep expanded-function '(cons (eql function)(cons symbol null))))
       `(,(cadr expanded-function),@(expander:expand* args env)))
      ((typep expanded-function '#.(cons-type-specifier '#'(lambda())))
       ;; To avoid to generate rebinding form, i.e. (let((a a)) ...)
       (let((binds(loop :for var :in (second(second expanded-function))
			:for arg :in args
			:unless (eq var arg)
			:collect `(,var ,(expander:expand arg env)))))
	 (if binds
	   (expander:expand `(LET,binds ,@(cddadr expanded-function)) env)
	   (if(cdr(cddadr expanded-function)) ; some forms
	     `(progn ,@(cddadr expanded-function))
	     (car (cddadr expanded-function))))))
      ((typep expanded-function '#.(cons-type-specifier '(let())))
       (destructuring-bind(op binds body)expanded-function
	 (multiple-value-bind(body decls)(alexandria:parse-body body)
	   `(,op ,binds ,@decls ,(|funcall-expander| `(funcall ,body ,@args)env)))))
      ((typep expanded-function '#.(cons-type-specifier '(constantly *)))
       (cadr expanded-function))
      ((typep expanded-function '#.(cons-type-specifier '((lambda * *))))
       (destructuring-bind((op lambda-list . body) . lambda-args)expanded-function
	 (declare(ignore op))
	 ;; To avoid to generate rebinding form i.e. (let((a a))...)
	 (let((binds(loop :for var :in lambda-list
			  :for arg :in lambda-args
			  :unless (eq var arg)
			  :collect `(,var ,arg))))
	   (if binds
	     (|funcall-expander| (list* (car form)`(let,binds ,@body)args)
				 env)
	     (if(cdr body) ; some forms
	       (|funcall-expander| (list* (car form)`(progn ,@body)args)
				   env)
	       (|funcall-expander| (list* (car form) (car body)args) env))))))
      (t (let((expanded(expander:expand* (cddr form) env)))
	   (if(and (every #'equalp expanded (cddr form))
		   (equal expanded-function function))
	     form
	     (|funcall-expander| (list* (car form) expanded-function expanded)env)))))))

(defun pretty-body(form the)
  (let((return-type (when the
		      (typecase the
			((cons (eql io)*)(second the))
			((cons (eql function)*) (third the))))))
    (if(cdr form)
      (if return-type
	`(the ,return-type (progn ,@form))
	`(PROGN ,@form))
      (if return-type
	`(the ,return-type ,(car form))
	(car form)))))

(handler-bind((expander:expander-conflict #'expander:use-next))
  (expander:defexpandtable :tyclex
    (:use standard)
    (:add |funcall-expander| funcall)
    ))

