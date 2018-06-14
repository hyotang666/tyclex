(in-package #:vs-haskell)

;;;; TYPE-CLASS OBJECT
(defstruct(type-class(:constructor make-info)(:copier nil)
                     (:predicate nil)(:conc-name type-))
  (name (error "Name is required.") :type (or symbol list) :read-only t)
  (var (error "Var is required.") :type symbol :read-only t)
  (direct-superclasses nil :type list)
  (direct-subclasses nil :type list))

;;;; INSTANCE OBJECT
(defstruct(type-class-instance(:constructor instance-info)(:copier nil)
                              (:predicate nil))
  (lambda-list nil :type list :read-only t)
  (return-type nil :type (or list symbol):read-only t)
  (type-class (error "required") :type symbol :read-only t)
  (table nil :type list)
  (default nil :type list :read-only t))

(defun instance-type-class(interface)
  (type-class-instance-type-class(get interface 'instance)))

(defun instance-default(interface)
  (type-class-instance-default(get interface 'instance)))

(defun instance-lambda-list(interface)
  (type-class-instance-lambda-list(get interface 'instance)))

(defun instance-return-type(interface)
  (type-class-instance-return-type(get interface 'instance)))

(defun instance-p(interface)
  (get interface 'instance))

;;;; INSTANCE-TABLE
(defun instance-table(interface)
  (type-class-instance-table(get interface 'instance)))
(defun (setf instance-table)(new interface)
  (setf(type-class-instance-table(get interface 'instance))new))

;;;; DEFINE-TYPE-CLASS
(defmacro define-type-class((name type-var)super-classes methods &rest rest)
  ;; trivial syntax checking.
  (assert(symbolp name))
  (assert(symbolp type-var))
  (assert(listp super-classes))
  (assert(every #'symbolp super-classes))
  ;; as canonicalize
  (setf type-var (Envar type-var))
  ;; body
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (SETF(GET ',name 'TYPE-CLASS)(MAKE-INFO :NAME ',name :VAR ',type-var))
     ,@(when super-classes
	 (<type-class-relation-setter> name super-classes))
     ,@(loop
	 :for (method lambda-list return-type) :in methods
	 :for gensyms = (Gensyms lambda-list)
	 :do (setf ; as canonicalise
	       lambda-list (patternize lambda-list)
	       return-type (patternize return-type))
	 :collect (<instance-info-setter> method name lambda-list return-type rest)
	 :collect (<defmacro> method gensyms lambda-list return-type))
     ',name))

;;; <type-class-relation-setter>
(defun <type-class-relation-setter>(name super-classes)
  `((SETF (TYPE-DIRECT-SUPERCLASSES(GET ',name 'TYPE-CLASS))',super-classes)
    ,@(loop :for type-class :in super-classes
	    :collect
	    `(PUSHNEW ',name (TYPE-DIRECT-SUBCLASSES(GET ',type-class 'TYPE-CLASS))))))

;;; <instance-info-setter>
(defun <instance-info-setter>(method name lambda-list return-type rest)
  `(SETF (GET ',method 'INSTANCE)
	 (INSTANCE-INFO :TYPE-CLASS ',name
			:LAMBDA-LIST ',lambda-list
			:RETURN-TYPE ',return-type
			,@(let((default(find method rest :key #'cadr)))
			    (when default
			      `(:DEFAULT '(,(cdr default))))))))

;;; <defmacro>
(defvar *sub-expand* nil)
(defun <defmacro>(method gensyms lambda-list return-type &aux (sub-name(sub-name method)))
  `(DEFMACRO,method(&WHOLE WHOLE ,@gensyms &ENVIRONMENT ENV)
     (IF (EQ *SUB-EXPAND* WHOLE)
	 WHOLE
	 (LET*((*SUB-EXPAND* WHOLE)
	       (EXPANDED(LOOP :FOR FORM :IN (LIST ,@gensyms)
			      :COLLECT (EXPANDER:EXPAND FORM ENV)))
	       (INFOS(CHECK-SIGNATURE ',lambda-list (COMPUTE-RETURN-TYPES EXPANDED ENV)))
	       (IL(GET-INSTANCE-LAMBDA ',method INFOS))
	       (MACROS(LOOP :FOR (NAME . REST) :IN IL
			    :COLLECT (CONS (SUB-NAME NAME) REST)))
	       (BODY`(,',sub-name
		       ,@(LOOP :FOR FORM :IN (TRESTRUL:ASUBST-IF
					       #'SUB-NAME
					       (LAMBDA(X)(FIND X IL :KEY #'CAR :TEST #'EQ))
					       EXPANDED)
			       :COLLECT (expander:expand
					  `(MACROLET,MACROS,FORM) env)))))
	   (IF IL
	      ,(if(millet:type-specifier-p return-type)
		 ``(MACROLET,MACROS (THE ,',return-type ,BODY))
		`(LET((RETURN(SUBSTITUTE-PATTERN ',return-type (TYPE-UNIFY:UNIFY ',lambda-list (ENWILD INFOS)))))
		   (IF(MILLET:TYPE-SPECIFIER-P RETURN)
		     `(MACROLET,MACROS (THE ,RETURN ,BODY))
		     `(MACROLET,MACROS ,BODY))))
	      (ERROR "Instance is not found. ~S ~S"',method (LIST ,@gensyms)))))))

(defun sub-name(symbol)
  (intern(format nil "%~A"symbol)))

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
    ((constantp var) ; lisp object.
     (let((value(introspect-environment:constant-form-value var env)))
       (if(adv-p value) ; literal adt.
	 (data-type-of value)
	 (class-name(class-of value)))))
    ((symbolp var) ; free variable.
     (or (introspect-environment:variable-type var env)
	 T))
    ((typep var '(cons (cons (eql lambda) *) *))
     (compute-standard-form-return-type (car(last(cddar var))) env))
    ((and (listp var)
	  (instance-p (car var))) ; instance call.
     (compute-instance-call-return-type var))
    ((and (listp var)
	  (action-boundp (car var))) ; action call.
     (action-type(action-boundp (car var))))
    ((adv-p var) ; constructor call.
     (compute-constructor-form-return-type var env))
    ((and (listp var)
	  (symbolp (car var)))
     (compute-standard-form-return-type var env))
    (t (error 'exhausts-clauses :name 'compute-return-type :datum var))))

(defun compute-instance-call-return-type(call-form)
  (let((pattern(instance-return-type(car call-form)))
       (environment(type-unify:unify (instance-lambda-list(car call-form))
				     (enwild (compute-return-types(cdr call-form))))))
    (substitute-pattern pattern environment)))

(defun substitute-pattern(pattern environment)
  (let((type-spec (dewild (trestrul:asubst-if (lambda(var)
						(let((return-type (type-unify:find-variable-value var environment)))
						  (typecase return-type
						    ((cons (eql values)t) (cadr return-type))
						    (null var)
						    (t return-type))))
					      #'type-unify:variablep
					      pattern))))
    (typecase type-spec
      ((cons (eql function)(cons * null))
       `(FUNCTION * ,(cadr type-spec)))
      ((cons (eql list)(cons * null))
       'list)
      (otherwise type-spec))))

(defun compute-standard-form-return-type(form env)
  (multiple-value-bind(type localp declaration)(introspect-environment:function-information (car form)env)
    (declare(ignore localp))
    (case type
      ((nil) (warn "Undefined function ~S. ~S"(car form)form))
      (:special-form (special-operator-return-type form env))
      (:macro (compute-return-type (agnostic-lizard:macroexpand-all (copy-tree form)env)
				   env))
      (:function
	(let((ftype(assoc 'ftype declaration)))
	  (if ftype
	    (ftype-return-type (cdr ftype))
	    (progn (warn "Could not determine type of ~S"form)
		   T)))))))

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
       (compute-return-type(car(last(cddr(second form))))env)
       (introspect-environment:function-type(second form)env)))
    ((if)
     (let((then(compute-return-type(third form)env))
	  (else(compute-return-type(fourth form)env)))
       (if then
	 (if else
	   `(or ,then ,else)
	   then)
	 else)))
    ((quote)
     (error 'unexpected-quote :datum form :name 'special-operator-return-type))
    ((macrolet symbol-macrolet)
     (compute-return-type (agnostic-lizard:macroexpand-all (copy-tree form)env)env))
    ((go throw catch return-from block) t) ; give up.
    (otherwise (error 'unknown-special-operator :datum form :name 'special-operator-return-type))))

(defun ftype-return-type(form)
  (if(symbolp form)
    T
    (canonicalize-return-type(third form))))

(defun canonicalize-return-type(return-type)
  (flet((ensure-t(thing)
	  (if(eq '* thing)
	    T
	    thing)))
    (if(symbolp return-type)
      (ensure-t return-type)
      (if(eq 'values (car return-type))
	(ensure-t (cadr return-type))
	return-type))))

(defun compute-constructor-form-return-type(var env)
  (let*((meta-info(adv-p var))
	(type(adt-type-of meta-info)))
    (if(atom type)
      type
      (cons (car type)
	    (substitute-pattern (adt-lambda-list meta-info)
				(type-unify:unify (adt-types meta-info)
						  (loop :for v :in (cdr var)
							:collect (compute-return-type v env))))))))

(defun insert-declare(form values)
  `(,@(subseq form 0 2)
     (DECLARE,@(mapcar (lambda(value var)
			 (if(adv-p value)
			   `(TYPE ,(data-type-of value) ,var)
			   `(TYPE ,(type-of value),var)))
		       values
		       (cadr form)))
     ,@(subseq form 2)))

;;;; CHECK-SIGNATURE
(defun check-signature(lambda-list type*)
  (dewild (substitute-pattern lambda-list
			      (type-unify:unify (enwild type*)
						(enwild lambda-list)))))

;;;; GET-INSTANCE-LAMBDA
(defun get-instance-lambda(interface type*)
  (if(every (lambda(x)
	      (eq T x))
	    type*)
    nil
    (or (compute-applicable-instance(collect-instance type* interface))
	(instance-default interface))))

;;;; COLLECT-INSTANCE
(defun collect-instance(type* interface)
  (remove-if-not (lambda(type)
		   (every #'subtype? type* type))
		 (instance-table interface)
		 :key #'car))

;;;; COMPUTE-APPLICABLE-INSTANCE
(defun compute-applicable-instance(list)
  (cdar(sort-instance list)))

(defun sort-instance(list)
  (flet((type<(ts1 ts2)
	  (every #'subtype?  ts1 ts2)))
    (sort list #'type< :key #'car)))

;;;; DEFISTANCE
(defmacro definstance((type-class type) definition)
  `(progn ,@(loop :for (name) :in definition
		  :collect `(add-instance ',name
					  ',(subst type
						   (type-var (get type-class 'type-class))
						   (instance-lambda-list name))
					  ',definition))

	  ',type-class))

;;;; ADD-INSTANCE
(defun add-instance(interface signature definition)
  (push(cons signature definition)(instance-table interface)))

#|
(defdata maybe (a)
  :nothing
  (just a))
(define-type-class(demo a)()
  ((demo(a)t)))
(definstance demo ((a bit)) 
  (format nil "Bit ~S"a))
(definstance demo ((a fixnum))
  (format nil "Fixnum ~S" a))
(definstance demo ((a integer))
  (format nil "Integer ~S"a))
(definstance demo ((a (maybe string)))
  (format nil "(maybe string) ~S" a))
(definstance demo ((a (maybe *)))
  (format nil "(maybe *) ~S" a))
(definstance demo ((a list))
  (format nil "list ~S" a))
|#
