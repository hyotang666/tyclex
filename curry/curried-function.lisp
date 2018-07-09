(defpackage :ehcl.curry
  (:use :cl)
  (:export
    #:curry
    ))
(in-package :ehcl.curry)

;;;; CURRY data structure.
(defclass curry ()
  ((function :initarg :function :reader curried-function))
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
  (let*((gensyms(gensyms(count-if #'underscorep args)))
	(optional-lambda-list(optional-lambda-list gensyms)))
    (if gensyms
      (<Curry-Form> (<Section-Body-Form> op args gensyms) optional-lambda-list)
      `(,op ,@args))))

(defun gensyms(num)
  (loop :repeat num :collect (gensym)))

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
(defun <Curry-Form> (body optional-lambda-list &optional types return-type ignores name)
  (let((curry (or name (gensym "CURRY"))))
    (labels((ENTRY-POINT(list types)
	      (if(endp list)
		(<BODY-FORM> body)
		`(LABELS((,curry(&OPTIONAL ,@list)
			   ,@(<Ignore-declaration> list ignores)
			   (IF ,(caddar list)
			     ,(rec (cdr list)(cdr types))
			     (MAKE-INSTANCE 'CURRY :FUNCTION #',curry))))
		   (MAKE-INSTANCE 'CURRY :FUNCTION #',curry))))
	    (REC(list types)
	      (if(endp list)
		(<body-form> body)
		`(IF,(caddar list)
		   ,(<ARG-SUPPLIED-FORM> types list
					 (lambda()(REC (cdr list)
						       (cdr types))))
		   ,(ENTRY-POINT list types))))
	    (<ARG-SUPPLIED-FORM>(types list cont)
	      (if types
		(WRAP-WITH-DECLARE (caar list)
				   (car types)
				   (funcall cont))
		(funcall cont)))
	    (WRAP-WITH-DECLARE(var type body)
	      `(LOCALLY
		 (DECLARE(TYPE ,type ,var))
		 ,body))
	    (<BODY-FORM>(body)
	      (if return-type
		`(FLET((#0=#:BODY()
			,@body))
		   (DECLARE(FTYPE(FUNCTION(),return-type)#0#))
		   (#0#))
		(if(cdr body)
		  `(LOCALLY ,@body)
		  (car body))))
	    )
      (ENTRY-POINT optional-lambda-list types))))

(defun <Ignore-Declaration>(optional-lambda-list ignores)
  (loop :for (var) :in optional-lambda-list
	:when (find var ignores)
	:collect var :into i
	:finally (when i
		   (return `((DECLARE(IGNORE ,@i)))))))

