(defpackage :tyclex.dsl.define-type-class
  (:use :cl #:tyclex.objects #:tyclex.dsl.defdata)
  (:import-from #:tyclex.type-matcher #:type-match-p)
  (:export
    ;; Main API
    #:define-type-class
    ))
(in-package :tyclex.dsl.define-type-class)

;;;; DEFINE-TYPE-CLASS
(defmacro define-type-class((name &rest type-var+)(&rest var-constraint*) method+ &rest rest)
  ;; trivial syntax checking.
  (assert(symbolp name))
  (assert type-var+)
  (assert(every #'symbolp type-var+))
  (assert(loop :for (constraint var) :in var-constraint*
	       :always (and (Find-type-class constraint)
			    (find var type-var+))))
  (assert method+)
  ;; as canonicalize
  (setf type-var+ (tyclex.unifier:envar type-var+))
  ;; body
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (ADD-TYPE-CLASS ',name :NAME ',name :VARS ',type-var+ :INSTANCES ',(mapcar #'car method+))
     ,@(when var-constraint*
	 (<constraints-setter> name var-constraint*))
     ,@(loop
	 :for (method lambda-list return-type) :in method+
	 :for gensyms = (alexandria:make-gensym-list (length lambda-list))
	 :do (setf ; as canonicalise
	       lambda-list (tyclex.unifier:patternize lambda-list)
	       return-type (tyclex.unifier:patternize return-type))
	 :collect (<add-instance> method name lambda-list return-type rest)
	 :collect (<defmacro> method gensyms lambda-list return-type))
     ,(<type-class-predicate> name)
     ',name))

;;; <constraints-setter>
(defun <constraints-setter>(name var-constraints)
  (loop :for (constraint) :in var-constraints
	:collect
	`(PUSHNEW ',name (TYPE-CLASS-CONSTRAINTS ',constraint))))

;;; <add-instance>
(defun <add-instance>(method name lambda-list return-type rest)
  `(ADD-INSTANCE ',method
		 :TYPE-CLASS ',name
		 :LAMBDA-LIST ',lambda-list
		 :RETURN-TYPE ',return-type
		 ,@(let((default(find method rest :key #'cadr)))
		     (when default
		       `(:DEFAULT ',(cdr default))))))

;;; <defmacro>
(defun <defmacro>(method gensyms lambda-list return-type &aux (sub-name(sub-name method)))
  `(DEFMACRO,method(&WHOLE WHOLE ,@gensyms &ENVIRONMENT ENV)
     (MULTIPLE-VALUE-BIND(EXPANDED RETURN-TYPE INFOS CELL MACROS)(PARSE-WHOLE WHOLE ',sub-name ENV)
       (DECLARE (IGNORE RETURN-TYPE)
		(IGNORABLE INFOS))
       (LET((BODY`(,',sub-name
		    ,@(LOOP :FOR FORM :IN EXPANDED
			    :COLLECT (expander:expand `(MACROLET,MACROS,FORM) env)))))
	 (IF CELL
	     ,(if(millet:type-specifier-p return-type)
		``(MACROLET,MACROS (THE ,',return-type ,BODY))
		`(LET((RETURN(SUBSTITUTE-PATTERN ',return-type (TYCLEX.UNIFIER:UNIFY ',lambda-list (ENWILD INFOS)))))
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
	(infos(check-signature (Instance-lambda-list (car form))
			       return-types))
	(cell(get-cell (car form) infos))
	(defs(and cell (Instances cell)))
	(types(and cell (Types cell)))
	(constraints(and cell (Constraints cell)))
	(consts(when constraints
		 (alexandria:when-let((constructors(mapcan (lambda(type)
							     (let((constructor (trestrul:find-node-if
										      (lambda(x)
											(eq (car x) (alexandria:ensure-car type)))
										      return-types)))
							       (when constructor
								 (list constructor))))
							   types)))
		   (let*((return-types(mapcar #'second constructors))
			 (instance-tables(loop :for constraint :in constraints
					       :append (dolist(instance (Type-class-instances constraint))
							 (let((table (Instance-table instance)))
							   (when table
							     (return (list table)))))))
			 (cells(loop :for instance-table :in instance-tables
				     :collect (find-if (lambda(cell)
							 (find-if (lambda(return-type)
								    (Type-match-p return-type (car(Types cell))))
								  return-types))
						       instance-table))))
		     (when cells
		       (alexandria:mappend #'Instances cells))))))
	(macros(loop :for (name . rest) :in defs
		     :when (eq name (car form))
		     :collect (cons sub-name rest)
		     :else :collect (cons name rest)))
	(type-class(Instance-type-class (car form)))
	(defs(loop :for tc :in (Type-class-constraints type-class)
		   :append (loop :for instance :in (Type-class-instances tc)
				 :thereis (loop :for cell :in (Instance-table instance)
						:when (find types (Types cell)
							    :test #'Type-match-p)
						:return (Instances cell))))))
    (if(some (lambda(x)
	       (let((x(alexandria:ensure-car x)))
		 (or (tyclex.unifier:variablep x)
		     (eq t x))))
	      infos)
      (values expanded return-types infos nil nil)
      (values expanded return-types infos cell (append macros defs consts)))))

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
     (compute-standard-form-return-type (car(last(cddar var))) env))
    ((and (listp var) ; instance call.
	  (Instancep (car var)))
     (compute-instance-call-return-type var))
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
	  (let((type (class-name(class-of value))))
	    #+sbcl(when(eq 'sb-kernel:simple-character-string type)
		    (setq type 'string))
	    (if(not(eq 'cons type))
	      type
	      (let((types(handler-case(mapcar (lambda(x)
						(class-name(class-of x)))
					      value)
			   (error() ; dot list comes
			     (return-from constant-return-type
					  (labels((rec(cons)
						    (if(atom cons)
						      (class-name(class-of cons))
						      `(cons ,(class-name(class-of(car cons)))
							     ,(rec (cdr cons))))))
					    (rec value)))))))
		(if(null(cdr(remove-duplicates types)))
		  `(LIST ,(class-name(class-of(car value))))
		  (labels((rec(list)
			    (if(endp(cdr list))
			      (car list)
			      `(CONS ,(car list)
				     ,(rec(cdr list))))))
		    (rec types)))))))))))

(defun compute-instance-call-return-type(call-form)
  (let((pattern(instance-return-type(car call-form)))
       (environment(tyclex.unifier:unify (Instance-lambda-list(car call-form))
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
       (introspect-environment:function-type(second form)env)))
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

(defun great-common-type(t1 t2)
  (labels((entry-point(t1 t2)
	    (if(millet:type-specifier-p t1)
	      (type-specifier- t1 t2)
	      (not-type-specifier- t1 t2)))
	  (type-specifier-(t1 t2)
	    (if(millet:type-specifier-p t2)
	      (both-type-specifier t1 t2)
	      (type-specifier/not-type-specifier t1 t2)))
	  (both-type-specifier(t1 t2)
	    (if(symbolp t1)
	      (symbol-type-specifier- t1 t2)
	      (compound-type-specifier- t1 t2)))
	  (symbol-type-specifier-(t1 t2)
	    (if(symbolp t2)
	      (both-symbol-type-specifier t1 t2)
	      (symbol-type-specifier/compound-type-specifier t1 t2)))
	  (both-symbol-type-specifier(t1 t2)
	    (if(eq t1 t2)
	      t1
	      (different-symbol-type-specifier t1 t2)))
	  (different-symbol-type-specifier(t1 t2)
	    (if(c2mop:subclassp (class-of t1) (class-of t2))
	      t2
	      (t1-is-not-subclass-of-t2 t1 t2)))
	  (t1-is-not-subclass-of-t2(t1 t2)
	    (if(c2mop:subclassp (class-of t2)(class-of t1))
	      t1
	      (both-are-not-subclass-each-other t1 t2)))
	  (both-are-not-subclass-each-other(t1 t2)
	    (find-diverging-point t1
				  #'c2mop:subclassp
				  (c2mop:class-direct-superclasses(find-class t2))))
	  (find-diverging-point(t1 test classes)
	    (let((class (find t1 classes :test test)))
	      (if class
		(class-name class)
		(find-diverging-point t1
				      test
				      (loop :for c :in classes
					    :append (c2mop:class-direct-superclasses c))))))
	  (symbol-type-specifier/compound-type-specifier(t1 t2)
	    (if(subtypep t1 t2)
	      t1
	      (sts-is-not-subtype-of-cts t1 t2)))
	  (sts-is-not-subtype-of-cts(t1 t2)
	    (if(subtypep t2 t1)
	      t1
	      (sts/cts-are-not-subtype-each-other t1 t2)))
	  (sts/cts-are-not-subtype-each-other(t1 t2)
	    (call-with-find-class
	      t1 t2 (lambda()(not-class-name/cts-are-not-subtype-each-other t1 t2))))
	  (not-class-name/cts-are-not-subtype-each-other(t1 t2)
	    (call-with-find-class
	      (car t2) t1
	      (lambda()(call-with-compound-typecase t1 t2 #'symbol-type-specifier-))))
	  (call-with-find-class(class-name diverge-arg1 cont)
	    (let((class(find-class class-name nil)))
	      (if class
		(find-diverging-point diverge-arg1 #'subtypep
				      (c2mop:class-direct-superclasses class))
		(funcall cont))))
	  (compound-type-specifier-(t1 t2)
	    (if(symbolp t2)
	      (symbol-type-specifier/compound-type-specifier t2 t1)
	      (both-compound-type-specifier t1 t2)))
	  (both-compound-type-specifier(t1 t2)
	    (if(subtypep t1 t2)
	      t2
	      (cts1-is-not-subtype-of-cts2 t1 t2)))
	  (cts1-is-not-subtype-of-cts2(t1 t2)
	    (if(subtypep t2 t1)
	      t1
	      (both-cts-are-not-subtype-each-other t1 t2)))
	  (both-cts-are-not-subtype-each-other(t1 t2)
	    (call-with-find-class
	      (car t1) t2
	      (lambda()
		(call-with-find-class
		  (car t2) t1
		  (lambda()
		    (call-with-compound-typecase t1 t2 #'compound-type-specifier-))))))
	  (call-with-compound-typecase(t1 cts cont)
	    (typecase cts
	      ((CONS (EQL AND)T)
	       (funcall cont t1 (cadr t2)))
	      ((CONS (EQL OR)T)
	       (funcall cont t1 (reduce #'great-common-type (cdr t2))))
	      ((CONS (EQL NOT)T)
	       T) ; give-up
	      ((CONS (EQL EQL)T)
	       (funcall cont t1 (class-name(class-of (second t2)))))
	      ((CONS (EQL MEMBER)T)
	       (funcall cont t1 (reduce #'great-common-type (cdr t2)
					:key (lambda(x)(class-name(class-of x))))))
	      ((CONS (EQL SATISFIES)T)
	       (funcall cont t1 (canonicalize-return-type(alexandria:ensure-car(cadr(introspect-environment:function-type(second t2)))))))
	      ((CONS (EQL CONS)T)
	       (funcall cont t1 'cons))
	      (T (funcall cont t1 (millet:type-expand t2)))))
	  (not-type-specifier-(t1 t2)
	    (if(millet:type-specifier-p t2)
	      (type-specifier/not-type-specifier t2 t1)
	      (both-are-not-type-specifier t1 t2)))
	  (both-are-not-type-specifier(t1 t2)
	    (matrix-case:matrix-etypecase(t1 t2)
	      ((LIST LIST)(mapcar #'great-common-type t1 t2))
	      (((SATISFIES tyclex.unifier:VARIABLEP)(SATISFIES tyclex.unifier:VARIABLEP))
	       (if(eq t1 t2)
		 t1
		 T))
	      (((SATISFIES tyclex.unifier:VARIABLEP)T)
	       T)
	      ((T (SATISFIES tyclex.unifier:VARIABLEP))
	       T)
	      ((T T)
	       (error "nyi ~S ~S" t1 t2))))
	  (type-specifier/not-type-specifier(t1 t2)
	    (typecase t2
	      ((CONS (EQL FUNCTION)T)
	       (both-type-specifier t1 'function))
	      ((SATISFIES tyclex.unifier:VARIABLEP)
	       t2)
	      (T (substitute-pattern t2
				     (tyclex.unifier:unify (canonicalize-return-type t1)
							   (canonicalize-return-type t2))))))
	  )
    (entry-point t1 t2)))

(defun ftype-return-type(form)
  (if(symbolp form)
    T
    (canonicalize-return-type(third form))))

(defun canonicalize-return-type(return-type)
  (flet((ensure-t(thing)
	  (subst T '* thing)))
    (if(typep return-type '(CONS (EQL VALUES)T))
      (if(typep return-type '(CONS * (CONS (EQL &OPTIONAL) T)))
	(ensure-t (caddr return-type))
	(ensure-t (cadr return-type)))
      (ensure-t return-type))))

(defun compute-constructor-form-return-type(var env)
  (let*((adt-constructor(nth-value 1 (Adt-value-p var)))
	(type(Adt-constructor-type-of adt-constructor)))
    (if(atom type)
      type
      (cons (car type)
	    (substitute-pattern (Adt-lambda-list (alexandria:ensure-car type))
				(tyclex.unifier:unify (Adt-constructor-arg-types adt-constructor)
						      (loop :for v :in (cdr var)
							    :collect (compute-return-type v env))))))))

;;;; CHECK-SIGNATURE
(defun check-signature(lambda-list type*)
  (tyclex.unifier:dewild (substitute-pattern lambda-list
					     (tyclex.unifier:unify (tyclex.unifier:enwild type*)
								   (tyclex.unifier:enwild lambda-list)))))

;;;; GET-INSTANCE-LAMBDA
(defun get-cell(interface type*)
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
		 (Instance-table interface)
		 :key #'Signature))

;;;; COMPUTE-APPLICABLE-INSTANCE
(defun compute-applicable-instance(list)
  (if(null(cdr list))
    (car list) ; only one element, does not need to sort.
    (let((sorted(sort-instance list)))
      (if(find (Signature(car sorted))
	       (cdr sorted)
	       :key #'Signature :test #'equal)
	nil ; duplicate signature, give up.
	(car sorted)))))

(defun sort-instance(list)
  (flet((type<(ts1 ts2)
	  (every #'Type-match-p (canonicalize-return-type ts1)
		 (canonicalize-return-type ts2))))
    (sort list #'type< :key #'Signature)))

;;; <type-class-predicate>
(defun <type-class-predicate>(name)
  `(DEFUN,(intern(format nil "~A-P" name))(#0=#:arg)
     (find #0# (Type-class-member ',name))))

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

#++
(if(or (eq #'funcall *macroexpand-hook*)
       (eq 'funcall *macroexpand-hook*))
  (setq *macroexpand-hook* 'infinite-expansion-detecter)
  (if(eq 'infinite-expansion-detecter *macroexpand-hook*)
    nil
    (if(y-or-n-p "~%TYCLEX try to replace *MACROEXPAND-HOOK*, but already set. ~S~%Really replace it?"*macroexpand-hook*)
      (setq *macroexpand-hook* 'infinite-expansion-detecter)
      (warn "TYCLEX could not detect infinite macro expansion."))))
