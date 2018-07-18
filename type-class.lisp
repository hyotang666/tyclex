(in-package #:vs-haskell)

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
     (SETF(GET ',name 'TYPE-CLASS)(MAKE-INFO :NAME ',name :VAR ',type-var
					     :INSTANCES ',(mapcar #'car methods)))
     ,@(when super-classes
	 (<type-class-relation-setter> name super-classes))
     ,@(loop
	 :for (method lambda-list return-type) :in methods
	 :for gensyms = (Gensyms (length lambda-list))
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
			      `(:DEFAULT ',(cdr default)))))))

;;; <defmacro>
(defvar *sub-expand* nil)
(defvar *expand-verbose* T)
(defun <defmacro>(method gensyms lambda-list return-type &aux (sub-name(sub-name method)))
  `(DEFMACRO,method(&WHOLE WHOLE ,@gensyms &ENVIRONMENT ENV)
     (IF (EQ *SUB-EXPAND* WHOLE)
	 (ERROR "Trap infinite expansion ~S" whole)
	 (LET((*SUB-EXPAND* WHOLE))
	   (MULTIPLE-VALUE-BIND(EXPANDED RETURN-TYPE INFOS IL MACROS)(PARSE-WHOLE WHOLE ENV)
	     (DECLARE (IGNORE RETURN-TYPE)
		      (IGNORABLE INFOS))
	     (LET((BODY`(,',sub-name
			  ,@(LOOP :FOR FORM :IN EXPANDED
				  :COLLECT (expander:expand
					     `(MACROLET,MACROS,FORM) env)))))
	       (IF IL
		   ,(if(millet:type-specifier-p return-type)
		      ``(MACROLET,MACROS (THE ,',return-type ,BODY))
		      `(LET((RETURN(SUBSTITUTE-PATTERN ',return-type (TYPE-UNIFY:UNIFY ',lambda-list (ENWILD INFOS)))))
			 (IF(MILLET:TYPE-SPECIFIER-P RETURN)
			   `(MACROLET,MACROS (THE ,RETURN ,BODY))
			   `(MACROLET,MACROS ,BODY))))
		   (PROGN
		     (WHEN *EXPAND-VERBOSE*
			   (WARN "Instance is not found. ~S ~S"',method (LIST ,@gensyms)))
		     WHOLE))))))))

(defun parse-whole(form &optional env)
  (let*((expanded(loop :for form :in (copy-list (cdr form))
		       :collect(expander:expand form env)))
	(return-types(compute-return-types expanded env))
	(infos(check-signature (instance-lambda-list (car form))
			       return-types))
	(instances(get-instance-lambda (car form) infos))
	(defs(first instances))
	(type(second instances))
	(constraint(third instances))
	(consts(when constraint
		 (alexandria:when-let((constructor(trestrul:find-node-if
						    (lambda(x)
						      (eq (car x)
							  (alexandria:ensure-car type)))
						    return-types)))
		   (let((return-type(second constructor))
			(instance-table(some #'instance-table
					     (type-instances (find-type-class constraint)))))
		     (second(find-if (lambda(cell)
				       (subtype? return-type (third cell)))
				     instance-table))))))
	(macros(loop :for (name . rest) :in defs
		     :when (eq name (car form))
		     :collect (cons (sub-name name) rest)
		     :else :collect (cons name rest)))
	(type-class(instance-type-class (car form)))
	(defs(loop :for tc :in (type-direct-subclasses type-class)
		   :append (loop :for instance :in (type-instances (find-type-class tc))
				 :thereis (loop :for (nil defs type%) :in (instance-table instance)
						:when (eq type type%)
						:return defs)))))
    (if(some (lambda(x)
	       (let((x(alexandria:ensure-car x)))
		 (or (type-unify:variablep x)
		     (eq t x))))
	      infos)
      (values expanded return-types infos nil nil)
      (values expanded return-types infos instances (append macros defs consts)))))

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

(defvar *return-type-verbose* t)
(defun compute-return-type(var &optional env)
  (cond
    ((block() ; constantp may do macroexpand.
       (let((*macroexpand-hook*(lambda(fn form env)
				 (let((expanded(funcall fn form env)))
				   (if (eq form expanded)
				     (return nil)
				     expanded)))))
	 (constantp var env))) ; lisp object.
     (if(typep var '(CONS (EQL THE)T))
       (second var)
       (let((value(introspect-environment:constant-form-value var env)))
	 (if(adv-p value) ; literal adt.
	   (data-type-of value)
	   (if(adt-p value)
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
				(return-from compute-return-type
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
    ((symbolp var) ; free variable.
     (let((type (introspect-environment:variable-type var env)))
       (if(not(eql t type))
	 type
	 (if(boundp var)
	   (data-type-of (eval var))
	   T))))
    ((typep var '(cons (cons (eql lambda) *) *)) ; ((lambda(...)...)...)
     (compute-standard-form-return-type (car(last(cddar var))) env))
    ((and (listp var) ; instance call.
	  (instance-p (car var)))
     (compute-instance-call-return-type var))
    ((and (listp var) ; action call.
	  (action-boundp (car var)))
     (action-type(action-boundp (car var))))
    ((adv-p var) ; constructor call.
     (compute-constructor-form-return-type var env))
    ((and (listp var) ; (make-instance 'io-action ...) call
	  (eq 'make-instance (car var))
	  (constantp (cadr var))
	  (eq 'io-action (introspect-environment:constant-form-value (cadr var))))
     '(io *))
    ((and (listp var) ; (make-instance 'curry ...) call
	  (eq 'make-instance (car var))
	  (constantp (cadr var))
	  (eq 'curry (introspect-environment:constant-form-value(cadr var))))
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
  (cond
    ((function-type-of(car form))
     (third(function-type-of(car form))))
    ((and (eq 'coerce (car form))
	  (constantp (third form)))
     (introspect-environment:constant-form-value(third form)))
    (t
      (multiple-value-bind(type localp declaration)(introspect-environment:function-information (car form)env)
	(declare(ignore localp))
	(case type
	  ((nil) (when *return-type-verbose*
		   (warn "Undefined function ~S. ~S"(car form)form)))
	  (:special-form (special-operator-return-type form env))
	  (:macro (compute-return-type (agnostic-lizard:macroexpand-all (copy-tree form)env)
				       env))
	  (:function
	    (let((ftype(assoc 'ftype declaration)))
	      (if ftype
		(ftype-return-type (cdr ftype))
		(progn (when *return-type-verbose*
			 (warn "Could not determine type of ~S"form))
		       T)))))))))

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
     (compute-return-type (agnostic-lizard:macroexpand-all (copy-tree form)env)env))
    ((go throw catch) t) ; give up.
    ((return-from)(compute-return-type (third form)))
    ((block)
     (let((return-types(delete-if (lambda(x)
				    (member x '(t nil null)))
				  (uiop:while-collecting(acc)
				    (acc(ehcl::compute-return-type(car(last form))env))
				    (trestrul:traverse
				      (lambda(node)
					(when(typep node `(CONS (EQL RETURN-FROM)(CONS (EQL,(second form)) T)))
					  (acc (ehcl::compute-return-type(third node)env))))
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
	      (((SATISFIES TYPE-UNIFY:VARIABLEP)(SATISFIES TYPE-UNIFY:VARIABLEP))
	       (if(eq t1 t2)
		 t1
		 T))
	      (((SATISFIES TYPE-UNIFY:VARIABLEP)T)
	       T)
	      ((T (SATISFIES TYPE-UNIFY:VARIABLEP))
	       T)
	      ((T T)
	       (error "nyi ~S ~S" t1 t2))))
	  (type-specifier/not-type-specifier(t1 t2)
	    (typecase t2
	      ((CONS (EQL FUNCTION)T)
	       (both-type-specifier t1 'function))
	      ((SATISFIES TYPE-UNIFY:VARIABLEP)
	       t2)
	      (T (substitute-pattern t2
				     (type-unify:unify (canonicalize-return-type t1)
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
  (let*((meta-info(adv-p var))
	(type(adt-type-of meta-info)))
    (if(atom type)
      type
      (cons (car type)
	    (substitute-pattern (adt-lambda-list meta-info)
				(type-unify:unify (adt-types meta-info)
						  (loop :for v :in (cdr var)
							:collect (compute-return-type v env))))))))

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
    (compute-applicable-instance(collect-instance type* interface))))

;;;; COLLECT-INSTANCE
(defun collect-instance(type* interface)
  (remove-if-not (lambda(type)
		   (every #'subtype? (canonicalize-return-type type*)
			  (canonicalize-return-type type)))
		 (instance-table interface)
		 :key #'car))

;;;; COMPUTE-APPLICABLE-INSTANCE
(defun compute-applicable-instance(list)
  (if(null(cdr list))
    (cdar list) ; only one element, does not need to sort.
    (let((sorted(sort-instance list)))
      (if(find(caar sorted)(cdr sorted):key #'car :test #'equal)
	nil ; duplicate signature, give up.
	(cdar list)))))

(defun sort-instance(list)
  (flet((type<(ts1 ts2)
	  (every #'subtype? (canonicalize-return-type ts1)
		 (canonicalize-return-type ts2))))
    (sort list #'type< :key #'car)))

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
