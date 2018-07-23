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
	      (entry-point rest (cdr types)(acc-declare elt(car types) acc))))
	  (diverge(key rest types acc)
	    (ecase key
	      (&optional (optional rest types acc))
	      (&key (key rest types acc))
	      (&rest (&rest rest types acc))
	      (&allow-other-keys(otherkeys rest types acc))
	      (&aux (aux rest types acc))))
	  (acc-declare(var type acc)
	    (if(find type '(t *))
	      acc
	      (cons `(declare(type ,type ,var))
		    acc)))
	  (optional(rest types acc)
	    (if(not(eq '&optional(car types)))
	      (error "Not match lambda-list ~S" lambda-list)
	      (optional-body rest (cdr types)acc)))
	  (optional-body(rest types acc)
	    (if(endp rest)
	      acc
	      (if(lambda-list:lambda-list-keyword-p(car rest))
		(diverge (car rest)(cdr rest)types acc)
		(optional-body (cdr rest)(cdr types)(acc-declare (alexandria:ensure-car (car rest))
								 (car types)
								 (if(typep (car rest)'(cons symbol (cons * (cons symbol null))))
								   (acc-declare (third(car rest))
										'BOOLEAN
										acc)
								   acc))))))
	  (key(rest types acc)
	    (if(endp rest)
	      acc
	      (if(lambda-list:lambda-list-keyword-p(car rest))
		(diverge(car rest)(cdr rest)types acc)
		(key (cdr rest)types
		     (multiple-value-bind(var key)(ensure-var (car rest))
		       (acc-declare var (some (compare key) types)acc))))))
	  (ensure-var(thing)
	    (etypecase thing
	      (symbol (values thing thing))
	      ((cons symbol t)(values(car thing)(car thing)))
	      ((cons (cons keyword (cons symbol null))t)
	       (values(cadar thing)(caar thing)))))
	  (compare(key)
	    (lambda(type)
	      (and (not(lambda-list:lambda-list-keyword-p type))
		   (listp type)
		   (string= key (car type))
		   (cadr type))))
	  (otherkeys(rest types acc)
	    (if(endp rest)
	      acc
	      (diverge(car rest)(cdr rest)types acc)))
	  (aux(rest types acc)
	    (if(endp rest)
	      acc
	      (aux (cdr rest)types
		   (acc-declare (ensure-var (car rest))
				(compute-return-type (cadr rest))
				acc))))
	  (&rest(rest types acc)
	    (if(endp (cdr rest))
	      (acc-declare (car rest)
			   (find-&rest-type types)
			   acc) 
	      (diverge (cadr rest)
		       (cddr rest)
		       types
		       (acc-declare (car rest)
				    (find-&rest-type types)
				    acc))))
	  (find-&rest-type(types)
	    (loop :for (key . value) :on types
		  :when (eq '&rest key)
		  :return `(list ,(car value))))
	  )
    (entry-point lambda-list types)))

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
	  (:macro (compute-return-type (expander:expand (copy-tree form)env)
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
     (compute-return-type (expander:expand (copy-tree form)env)env))
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

