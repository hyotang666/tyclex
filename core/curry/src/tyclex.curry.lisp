(in-package :cl-user)
(defpackage :tyclex.curry
  (:use :cl)
  (:export
    ;; Main API
    #:curry
    ;; Helpers
    #:function-type-of #:function-type
    ;; Decurry
    #:decurry #:recurry #:curry-form-p #:expanded-curry-form-p
    #:expanded-curry-form-arity #:expanded-curry-form-return-type
    ))
(in-package :tyclex.curry)

;;;; CURRY data structure.
(defclass curry ()
  ((function :initarg :function :reader curried-function)
   (arity :initarg :arity :reader arity)
   (return-type :initarg :return-type :reader return-type))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((c curry) &key)
  (c2mop:set-funcallable-instance-function c (curried-function c)))

;;;; CURRY
(defmacro curry (&whole whole op &rest args)
  ;; Trivial syntax check.
  (check-type op symbol)
  (assert args)
  ;; body
  (<Section-Form> op args whole))

;;; <Section-Form>
(defun <Section-Form>(op args whole)
  (let*((gensyms(underscore-gensyms args))
	(optional-lambda-list(optional-lambda-list gensyms)))
    (if gensyms
      (<Curry-Form> (<Section-Body-Form> op args gensyms)
		    optional-lambda-list
		    (or (third(function-type-of op))
			(third(introspect-environment:function-type op)))
		    whole)
      `(,op ,@args))))

(defun underscore-gensyms(args)
  (alexandria:make-gensym-list(count-if #'underscorep args)))

(defun underscorep (thing)
  (and (symbolp thing)
       (string= "_" thing)))

(defun optional-lambda-list(lambda-list)
  (mapcar (lambda(x)
	    `(,x NIL ,(gensym (format nil "~A-P"x))))
	  lambda-list))

;;; <Section-Body-Form>
(defun <Section-Body-Form>(op args gensyms)
  (labels((rec(args gensyms &optional acc)
	    (if(endp args)
	      (nreverse acc)
	      (body(car args)(cdr args)gensyms acc)))
	  (body(arg rest gensyms acc)
	    (if(underscorep arg)
	      (rec rest (cdr gensyms)(push (car gensyms)acc))
	      (rec rest gensyms (push arg acc)))))
    `((,op ,@(rec args gensyms)))))

;;; <Curry-Form>
(defun <Curry-Form> (body optional-lambda-list return-type whole)
  (let((curry (gensym "CURRY")))
    (labels((ENTRY-POINT(list)
	      (if(endp list)
		(<BODY-FORM> body)
		`(LABELS((,curry(&OPTIONAL ,@list)
			   (IF ,(caddar list)
			     ,(rec (cdr list))
			     (MAKE-INSTANCE 'CURRY
					    :FUNCTION #',curry
					    :ARITY ,(length list)
					    :RETURN-TYPE ',return-type
					    ))))
		   ',whole ; for decurry.
		   (MAKE-INSTANCE 'CURRY
				  :FUNCTION #',curry
				  :ARITY ,(length list)
				  :RETURN-TYPE ',return-type
				  ))))
	    (REC(list)
	      (if(endp list)
		(<body-form> body)
		`(IF,(caddar list)
		   ,(REC (cdr list))
		   ,(ENTRY-POINT list))))
	    (<BODY-FORM>(body)
		(if(cdr body)
		  `(LOCALLY ,@body)
		  (car body)))
	    )
      (ENTRY-POINT optional-lambda-list))))

;;;; DECURRY
(defun decurry (form actual-args)
  (recurry (introspect-environment:constant-form-value(third form))
	   actual-args))

(defun recurry(curry-form actual-args)
  (if(null actual-args)
    curry-form
    (let*((underscore-num(count-if #'underscorep (cddr curry-form)))
	  (arg-num(length actual-args))
	  (diff(- underscore-num arg-num)))
      (flet((UNDERSCORE-TO-ACTUAL-ARG(whole gensyms)
	      (loop :for elt :in whole
		    :when (and (symbolp elt)
			       (string= "_" elt))
		    :collect (or (pop gensyms)
				 elt)
		    :else :collect elt)))
	(if (minusp diff)
	  (error "Too much args. ~S ~S"curry-form actual-args)
	   (UNDERSCORE-TO-ACTUAL-ARG curry-form actual-args))))))

(defun curry-form-p(form)
  (typep form '(cons (eql curry) *)))

(defun expanded-curry-form-p(form)
  (and (listp form)
       (eq 'labels (car form))
       (let((body (cddr form)))
	 (and (every #'listp body)
	      (= 2 (length body))
	      (eq 'make-instance (caadr body))
	      (equal (cadadr body) '(quote curry))))))

(defun expanded-curry-form-arity(form)
  (let*((make-instance(fourth form))
	(arity(getf make-instance :arity)))
    (introspect-environment:constant-form-value arity)))

(defun expanded-curry-form-return-type(form)
  (let*((make-instance(fourth form))
	(return-type(getf make-instance :return-type)))
    (introspect-environment:constant-form-value return-type)))

;;;; FUNCTION-TYPE
(defmacro function-type (name args return)
  ;; trivial syntax checks.
  (check-type name (and symbol (not (or keyword boolean))))
  (check-type args (or (eql *)list))
  (check-type return (or symbol list))
  ;; body
  `(PROGN (DECLAIM(FTYPE(FUNCTION,args,return),name))
	  (SETF (GET ',name 'FTYPE)'(FUNCTION ,args ,return))
	  ',name))

(defun function-type-of(name)
  (get name 'ftype))

