(in-package :vs-haskell)
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
  (let*((gensyms(gensyms(count-if #'underscorep args)))
	(optional-lambda-list(optional-lambda-list gensyms)))
    (if gensyms
      (<Curry-Form> (<Section-Body-Form> op args gensyms) optional-lambda-list
		    (and (symbolp op)
			 (or (third(function-type-of op))
			     (third(introspect-environment:function-type op)))))
      `(,op ,@args))))

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

;;;; FUNCTION-TYPE
(defmacro function-type (name args return)
  `(PROGN (SETF (GET ',name 'FTYPE)'(FUNCTION ,args ,return))
	  ',name))

(defun function-type-of(name)
  (get name 'ftype))

;;;; DEFUN
(defmacro defun* (name lambda-list &body body)
  (if(or (listp name) ; setf form, ignore.
	 (or (null(function-type-of name))
	     (null(introspect-environment:function-type name))))
    `(cl:defun ,name ,lambda-list ,@body)
    ;; bind
    (let((types(second(or (function-type-of name)
			  (introspect-environment:function-type name)))))
      (multiple-value-bind(body decls doc)(alexandria:parse-body body)
	(setf doc (alexandria:ensure-list doc)) ; as canonicalize.
	`(cl:defun ,name ,lambda-list
		   ,@doc
		   ,@(append decls (lambda-var-decls lambda-list types))
		   ,@body)))))

(defun lambda-var-decls(lambda-list types)
  (labels((entry-point(lambda-list types &optional acc)
	    (if(endp lambda-list)
	      acc
	      (body (car lambda-list)(cdr lambda-list)types acc)))
	  (body(elt rest types acc)
	    (if(lambda-list:lambda-list-keyword-p elt)
	      (diverge elt rest types acc)
	      (entry-point rest (cdr types)(cons (make-declare elt(car types))
						 acc))))
	  (diverge(key rest types acc)
	    (ecase key
	      (&optional (optional rest types acc))
	      (&key (key rest types acc))
	      (&rest (&rest rest types acc))
	      (&allow-other-keys(otherkeys rest types acc))
	      (&aux (aux rest types acc))))
	  (make-declare(var type)
	    `(declare(type ,type ,var)))
	  (optional(rest types acc)
	    (if(not(eq '&optional(car types)))
	      (error "Not match lambda-list ~S" lambda-list)
	      (optional-body rest (cdr types)acc)))
	  (optional-body(rest types acc)
	    (if(lambda-list:lambda-list-keyword-p(car rest))
	      (diverge (car rest)(cdr rest)types acc)
	      (optional-body (cdr rest)(cdr types)(cons (make-declare (alexandria:ensure-car (car rest))
								      (car types))
							acc))))
	  (key(rest types acc)
	    (if(lambda-list:lambda-list-keyword-p(car rest))
	      (diverge(car rest)(cdr rest)types acc)
	      (key (cdr rest)types
		   (let((var(ensure-var (car rest))))
		     (cons (make-declare var (some (compare var) types))
			   acc)))))
	  (ensure-var(thing)
	    (etypecase thing
	      (symbol thing)
	      ((cons symbol t)(car thing))
	      ((cons (cons keyword (cons symbol null))t)
	       (cadar thing))))
	  (compare(var)
	    (lambda(type)
	      (and (not(lambda-list:lambda-list-keyword-p type))
		   (listp type)
		   (string= var (car type))
		   (cadr type))))
	  (otherkeys(rest types acc)
	    (if(endp rest)
	      acc
	      (diverge(car rest)(cdr rest)types acc)))
	  (aux(rest types acc)
	    (if(endp rest)
	      acc
	      (aux (cdr rest)types
		   (cons (make-declare (ensure-var (car rest))
				       (compute-return-type (cadr rest)))
			 acc))))
	  (&rest(rest types acc)
	    (if(endp (cdr rest))
	      (cons (make-declare (car rest)
				  (loop :for (key value) :in types
					:when (eq 'rest key)
					:return value))
		    acc)
	      (diverge (cadr rest)
		       (cddr rest)
		       types
		       acc)))
	    )
    (entry-point lambda-list types)))
