(defpackage :tyclex.dsl.define-type-class
  (:use :cl #:tyclex.objects #:tyclex.dsl.defdata)
  (:import-from #:tyclex.type-matcher #:type-match-p)
  (:export
    ;; Main API
    #:define-type-class
    ;; Useful helpers
    #:compute-return-type
    #:infinite-expansion-detecter
    ))
(in-package :tyclex.dsl.define-type-class)

;;;; DEFINE-TYPE-CLASS
(defmacro define-type-class((name &rest type-var+)(&rest var-constraint*) signature+ &rest rest)
  ;; trivial syntax checking.
  (assert(symbolp name))
  (assert type-var+)
  (assert(every #'symbolp type-var+))
  (assert(loop :for (constraint var) :in var-constraint*
	       :always (and (Find-type-class constraint)
			    (find var type-var+))))
  (assert signature+)
  ;; as canonicalize
  (setf type-var+ (tyclex.unifier:envar type-var+))
  ;; body
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (ADD-TYPE-CLASS ',name :NAME ',name :VARS ',type-var+ :INTERFACES ',(mapcar #'car signature+))
     ,@(when var-constraint*
	 (<constraints-setter> name var-constraint*))
     ,@(loop
	 :for (interface lambda-list return-type) :in signature+
	 :for gensyms = (alexandria:make-gensym-list (length lambda-list))
	 :do (setf ; as canonicalise
	       lambda-list (tyclex.unifier:patternize lambda-list)
	       return-type (tyclex.unifier:patternize return-type))
	 :collect (<add-interface> interface name lambda-list return-type rest)
	 :collect (<defmacro> interface gensyms lambda-list return-type))
     ,(<type-class-predicate> name)
     ',name))

;;; <constraints-setter>
(defun <constraints-setter>(name var-constraints)
  (loop :for (constraint) :in var-constraints
	:collect
	`(PUSHNEW ',name (TYPE-CLASS-CONSTRAINTS ',constraint))))

;;; <add-interface>
(defun <add-interface>(interface name lambda-list return-type rest)
  `(ADD-INTERFACE ',interface
		  :TYPE-CLASS ',name
		  :LAMBDA-LIST ',lambda-list
		  :RETURN-TYPE ',return-type
		  ,@(let((default(find interface rest :key #'cadr)))
		      (when default
			`(:DEFAULT ',(cdr default))))))

;;; <defmacro>
(defun <defmacro>(interface gensyms lambda-list return-type &aux (sub-name(sub-name interface)))
  `(DEFMACRO,interface(&WHOLE WHOLE ,@gensyms &ENVIRONMENT ENV)
     (DECLARE(IGNORE ,@gensyms))
     (MULTIPLE-VALUE-BIND(EXPANDED RETURN-TYPE INFOS INSTANCE MACROS)(PARSE-WHOLE WHOLE ',sub-name ENV)
       (DECLARE (IGNORE RETURN-TYPE INSTANCE)
		(IGNORABLE INFOS))
       (LET((BODY`(,',sub-name
		    ,@(LOOP :FOR FORM :IN EXPANDED
			    :COLLECT (expander:expand `(MACROLET,MACROS,FORM) env)))))
	 (IF MACROS
	     ,(if(millet:type-specifier-p return-type)
		``(MACROLET,MACROS (THE ,',return-type ,BODY))
		`(LET((RETURN(SUBSTITUTE-PATTERN ',return-type (TYCLEX.UNIFIER:UNIFY ',lambda-list (TYCLEX.UNIFIER:ENWILD INFOS)))))
		   (IF(MILLET:TYPE-SPECIFIER-P RETURN)
		     `(MACROLET,MACROS (THE ,RETURN ,BODY))
		     `(MACROLET,MACROS ,BODY))))
	     (PROGN
	       WHOLE))))))

(defun parse-whole(form &optional (sub-name '#:sub-name) env)
  (let*((*macroexpand-hook* 'funcall) ; for easy debugging.
	(expanded(loop :for form :in (copy-list (cdr form))
		       :collect(expander:expand form env)))
	(return-types(compute-return-types expanded env))
	(infos(check-signature (Interface-lambda-list (car form))
			       return-types))
	(instance(get-instance (car form) infos))
	(definitions(and instance (Instance-definitions instance)))
	(types(and instance (Instance-types instance)))
	(type-class(Interface-type-class (car form)))
	(constraints(and instance
			 (remove type-class (Instance-constraints instance):key #'car)))
	(instance-constraints-definitions
	  (when constraints
	    (let((constructors(mapcan (lambda(type)
					(let((constructor (trestrul:find-node-if
							    (lambda(x)
							      (eq (car x) (alexandria:ensure-car type)))
							    return-types)))
					  (when constructor
					    (list constructor))))
				      types)))
	      (when constructors
		(constraints-definitions constraints (mapcar #'second constructors))))))
	(macros(loop :for (name . rest) :in definitions
		     :when (eq name (car form))
		     :collect (cons sub-name rest)
		     :else :collect (cons name rest)))
	(type-class-constraints-definitions(constraints-definitions (Type-class-constraints type-class)
								    types)))
    (if(some (lambda(x)
	       (let((x(alexandria:ensure-car x)))
		 (or (tyclex.unifier:variablep x)
		     (eq t x))))
	     infos)
      (values expanded return-types infos nil nil)
      (values expanded return-types infos instance (append macros
							   type-class-constraints-definitions
							   instance-constraints-definitions)))))

#++
(let*((return-types(mapcar #'second constructors))
      (instance-tables(loop :for tc :in constraints
			    :append (dolist(interface (Type-class-interfaces tc))
				      (let((instances (Interface-instances interface)))
					(when instances
					  (return (list instances)))))))
      (instances(loop :for instance-table :in instance-tables
		      :collect (find-if (lambda(instance)
					  (find-if (lambda(return-type)
						     (Type-match-p return-type (car(Instance-types instance))))
						   return-types))
					instance-table))))
  (when instances
    (alexandria:mappend #'Instance-definitions instances)))

(defun constraints-definitions(constraints types)
  (loop :for constraint :in constraints
	:append (loop :for interface :in (Type-class-interfaces (alexandria:ensure-car constraint))
		      :thereis (loop :for instance :in (Interface-instances interface)
				     :when (find types (Instance-types instance)
						 :test #'Type-match-p)
				     :return (Instance-definitions instance)))))

(defun sub-name(symbol)
  (gensym(symbol-name symbol)))

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
     (let((type (introspect-environment:variable-type var env)))
       (if(not(eql t type))
	 type
	 (if(boundp var)
	   (Data-type-of (eval var))
	   T))))
    ((typep var '(cons (cons (eql lambda) *) *)) ; ((lambda(...)...)...)
     (compute-return-type (car(last(cddar var))) env))
    ((and (listp var) ; interface call.
	  (Interface-boundp (car var)))
     (compute-interface-call-return-type var))
    ((and (listp var) ; action call.
	  (Io-boundp (car var)))
     (Action-type(Io-boundp (car var))))
    ((Adt-value-p var) ; constructor call.
     (compute-constructor-form-return-type var env))
    ((and (listp var) ; (make-instance 'io-action ...) call
	  (eq 'make-instance (car var))
	  (constantp (cadr var))
	  (eq 'Io-action (introspect-environment:constant-form-value (cadr var))))
     '(Io *))
    ((and (listp var) ; (make-instance 'curry ...) call
	  (eq 'make-instance (car var))
	  (constantp (cadr var))
	  (eq 'tyclex.curry:curry (introspect-environment:constant-form-value(cadr var))))
     (let((arity(introspect-environment:constant-form-value(getf var :arity))))
       (if(null arity)
	 (error "Arity did not exist ~S" var)
	 (if(= 1 arity)
	   `(function * ,(canonicalize-return-type (introspect-environment:constant-form-value (getf var :return-type))))
	   `(function * function)))))
    ((and (listp var)
	  (symbolp (car var)))
     (compute-standard-form-return-type var env))
    (t (error 'exhausts-clauses :name 'compute-return-type :datum var))))

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

(defun compute-interface-call-return-type(call-form)
  (let((pattern(Interface-return-type(car call-form)))
       (environment(tyclex.unifier:unify (Interface-lambda-list(car call-form))
					 (tyclex.unifier:enwild (compute-return-types(cdr call-form))))))
    (substitute-pattern pattern environment)))

(defun substitute-pattern(pattern environment)
  (let((type-spec (tyclex.unifier:dewild (trestrul:asubst-if (lambda(var)
							       (let((return-type (tyclex.unifier:find-variable-value var environment)))
								 (typecase return-type
								   ((cons (eql values)t) (cadr return-type))
								   (null var)
								   (t return-type))))
							     #'tyclex.unifier:variablep
							     pattern))))
    (typecase type-spec
      ((cons (eql function)(cons * null))
       `(FUNCTION * ,(cadr type-spec)))
      ((cons (eql list)(cons * null))
       'list)
      (otherwise type-spec))))

(defun compute-standard-form-return-type(form env)
  (cond
    ((tyclex.curry:function-type-of(car form))
     (third(tyclex.curry:function-type-of(car form))))
    ((and (eq 'coerce (car form))
	  (constantp (third form)))
     (introspect-environment:constant-form-value(third form)))
    ((and (eq 'map (car form))
	  (constantp (second form)))
     (introspect-environment:constant-form-value(second form)))
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
	   (great-common-type then else)
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
	   (reduce #'great-common-type return-types)
	   (car return-types))
	 T))) ; give up.
    (otherwise (error 'unknown-special-operator :datum form :name 'special-operator-return-type))))

(defun type-category-of(type)
  (if(millet:type-specifier-p type)
    (if(symbolp type)
      (if(find type '(function standard-generic-function) :test #'eq)
	:function
	(if(find-class type nil)
	  :class
	  :type))
      :compound)
    (if(symbolp type)
      (if(tyclex.unifier:variablep type)
	:type-variable
	:unknown)
      (if(typep type '(cons (eql function)t))
	:function
	:may-pattern))))

(defun great-common-type(t1 t2 &optional reccursivep)
  (matrix-case:matrix-case((type-category-of t1)(type-category-of t2))
    ((:function		:function)		'function)
    ((:function		:class)			(find-diverging-class 'function t2))
    ((:function		(:compound :type))	(find-diverging-class 'function t2))
    ((t			:type-variable) 	t2)
    ((t			:unknown)		(error "nyi ~S ~S"t1 t2))
    ((:function		:may-pattern)
     (or (tyclex.unifier:ignore-unification-failure(tyclex.unifier:unify t1 t2))
	 t2))
    ((:class		:class)			(find-diverging-class t1 t2))
    ((:class		:function)		(find-diverging-class t1 'function))
    ((:class		(:compound :type))	(find-diverging-class t1 t2))
    (((:type :class :compound)	:may-pattern)	T) ; give up.
    (((:compound :type)	(:compound :type))
     (cond
       ((subtypep t1 t2)t2)
       ((subtypep t2 t1)t1)
       (t (if reccursivep
	    T ; give up.
	    (great-common-type (millet:type-expand t1)
			       (millet:type-expand t2)
			       t)))))
    ((:type		:function)		(find-diverging-class 'function t1))
    ((:type		:class)			(find-diverging-class t2 t1))
    ((:compound		:function)		(find-diverging-class 'function t1))
    ((:compound		:class)			(find-diverging-class t2 t1))
    ((:type-variable	:type-variable)
     (if(eq t1 t2)
       t1
       (error "nyi ~S ~S"t1 t2)))
    ((:type-variable	t)			t1)
    ((:unknown		:unknown)		(error "nyi ~S ~S" t1 t2))
    ((:unknown		t)			(error "nyi ~S ~S" t1 t2))
    ((:may-pattern	:may-pattern)		(mapcar #'great-common-type t1 t2))
    ((:may-pattern	:function)
     (or (tyclex.unifier:ignore-unification-failure(tyclex.unifier:unify t1 t2))
	 t1))
    ((:may-pattern	(:type :class :compound))	t) ; give up.
    ))

(defun find-diverging-class(class type)
  (labels((rec(classes &optional acc)
	    (if(endp classes)
	      (if(null(cdr acc))
		(car acc)
		(reduce (lambda(pre suc)
			  (if(subtypep pre suc)
			    pre
			    suc))
			acc))
	      (if(subtypep type (car classes))
		(rec (cdr classes)(cons (class-name(car classes)) acc))
		(rec (append (cdr classes)
			     (c2mop:class-direct-superclasses (car classes)))
		     acc)))))
    (setf class (find-class class)) ; as canonicalize.
    (cond
      ((subtypep class type) type)
      ((subtypep type class) (class-name class))
      (t (rec (c2mop:class-direct-superclasses class))))))

(defun ftype-return-type(form)
  (if(symbolp form)
    T
    (canonicalize-return-type(third form))))

(defun canonicalize-return-type(return-type)
  (flet((ensure-t(thing)
	  (sublis '((* . T)
		    (simple-vector . vector)
		    (simple-array . array)
		    (simple-string . string)
		    (base-string . string)
		    )
		  thing)))
    (if(typep return-type '(CONS (EQL VALUES)T))
      (if(typep return-type '(CONS * (CONS (EQL &OPTIONAL) T)))
	(ensure-t (caddr return-type))
	(ensure-t (cadr return-type)))
      (ensure-t return-type))))

(defun canonicalize-ftype(ftype)
  (list (first ftype)
	(second ftype)
	(canonicalize-return-type (third ftype))))

(defun compute-constructor-form-return-type(var env)
  (let*((adt-constructor(nth-value 1 (Adt-value-p var)))
	(type(Adt-constructor-type-of adt-constructor)))
    (if(atom type)
      type
      (cons (car type)
	    (substitute-pattern (Adt-lambda-list (Find-adt(alexandria:ensure-car type)))
				(tyclex.unifier:unify (Adt-constructor-arg-types adt-constructor)
						      (loop :for v :in (cdr var)
							    :collect (compute-return-type v env))))))))

;;;; CHECK-SIGNATURE
(defun check-signature(lambda-list type*)
  (loop :with environment = (tyclex.unifier:unify (tyclex.unifier:enwild type*)
						  (tyclex.unifier:enwild lambda-list))
	:for pattern :in lambda-list
	:collect (tyclex.unifier:dewild (substitute-pattern pattern environment))))

;;;; GET-INSTANCE
(defun get-instance(interface type*)
  (if(every (lambda(x)
	      (eq T x))
	    type*)
    nil
    (compute-applicable-instance(collect-instance type* interface))))

;;;; COLLECT-INSTANCE
(defun collect-instance(type* interface)
  (remove-if-not (lambda(signature)
		   (every #'Type-match-p (canonicalize-return-type type*)
			  (canonicalize-return-type signature)))
		 (Interface-instances interface)
		 :key #'Instance-signature))

;;;; COMPUTE-APPLICABLE-INSTANCE
(defun compute-applicable-instance(list)
  (if(null(cdr list))
    (car list) ; only one element, does not need to sort.
    (let((sorted(sort-instance list)))
      (if(find (Instance-signature(car sorted))
	       (cdr sorted)
	       :key #'Instance-signature :test #'equal)
	nil ; duplicate signature, give up.
	(car sorted)))))

(defun sort-instance(list)
  (flet((type<(ts1 ts2)
	  (every #'Type-match-p (canonicalize-return-type ts1)
		 (canonicalize-return-type ts2))))
    (sort list #'type< :key #'Instance-signature)))

;;; <type-class-predicate>
(defun <type-class-predicate>(name)
  `(DEFUN,(intern(format nil "~A-P" name))(#0=#:arg)
     (find #0# (Type-class-member ',name):test #'Type-match-p)))

;;;; MACROEXPAND-HOOK
(define-condition infinite-expansion()())

(defun infinite-expansion-detecter(expander form env)
  (let*((*macroexpand-hook* #'funcall)
	(expanded(funcall expander form env)))
    (if(and (eq expanded form)
	    (macro-function(car form)))
      (progn (cerror "Re-expand it."'infinite-expansion)
	     form)
      expanded)))

(if(or (eq #'funcall *macroexpand-hook*)
       (eq 'funcall *macroexpand-hook*))
  (setq *macroexpand-hook* 'infinite-expansion-detecter)
  (if(eq 'infinite-expansion-detecter *macroexpand-hook*)
    nil
    (if(y-or-n-p "~%TYCLEX try to replace *MACROEXPAND-HOOK*, but already set. ~S~%Really replace it?"*macroexpand-hook*)
      (setq *macroexpand-hook* 'infinite-expansion-detecter)
      (warn "TYCLEX could not detect infinite macro expansion."))))
