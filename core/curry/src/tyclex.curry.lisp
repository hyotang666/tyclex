(defpackage :tyclex.curry
  (:use :cl)
  (:export
    ;; Main API
    #:curry
    ;; Helpers
    #:function-type-of #:function-type #:<Non-Curry-Form>
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
(defmacro curry (op &rest args)
  ;; Trivial syntax check.
  (check-type op (or symbol (cons (eql lambda)T)))
  (when(typep op '(cons (eql lambda)t))
    (assert (every #'symbolp (cadr op)))
    (assert (notany (lambda(elt)
		      (find elt lambda-list-keywords))
		    (cadr op))))
  ;; body
  (if(null args)
    `#',op
    (<Section-Form> op args)))

;;; <Section-Form>
(defun <Section-Form>(op args)
  (let*((gensyms(underscore-gensyms args))
	(optional-lambda-list(optional-lambda-list gensyms)))
    (if gensyms
      (<Curry-Form> (<Section-Body-Form> op args gensyms)
		    optional-lambda-list
		    (and (symbolp op)
			 (or (third(function-type-of op))
			     (third(introspect-environment:function-type op)))))
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
(defun <Curry-Form> (body optional-lambda-list return-type)
  (let((curry (gensym "CURRY")))
    (labels((ENTRY-POINT(list)
	      (if(endp list)
		(<BODY-FORM> body)
		`(LABELS((,curry(&OPTIONAL ,@list)
			   (IF ,(caddar list)
			     ,(rec (cdr list))
			     (MAKE-INSTANCE 'CURRYi
					    :FUNCTION #',curry
					    :ARITY ,(length list)
					    :RETURN-TYPE ',return-type
					    ))))
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

;;;; <non-curry-form>
(defun <non-curry-form> (op args)
  (let((gensyms(underscore-gensyms args)))
    `(lambda(,@gensyms)
       ,@(<section-body-form> op args gensyms))))

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

