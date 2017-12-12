(in-package #:vs-haskell)

;;;; TYPE-CLASS OBJECT
(defstruct(type-class(:constructor make-info)(:copier nil)
                     (:predicate nil)(:conc-name type-))
  (name (error "Name is required.") :type symbol :read-only t)
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
(defmacro define-type-class((name &rest vars)super-classes methods &rest rest)
  ;; trivial syntax checking.
  (assert(every #'symbolp vars))
  ;; as canonicalize
  (map-into vars #'Envar vars)
  ;; body
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (SETF(GET ',name 'TYPE-CLASS)(MAKE-INFO :NAME ',name))
     ,@(when super-classes
	 (<type-class-relation-setter> name super-classes))
     ,@(loop
	 :for (method lambda-list return-type) :in methods
	 :for gensyms = (Gensyms lambda-list)
	 :do (setf ; as canonicalise
	       lambda-list (patternize lambda-list)
	       return-type (patternize return-type))
	 :collect (<instance-info-setter> method name lambda-list return-type rest)
	 :collect (<instance-compiler-macro> method gensyms lambda-list return-type)
	 :collect (<instance-interpreter> method gensyms lambda-list))
     ',name))

(defun patternize(thing)
  (if(millet:type-specifier-p thing)
    thing
    (if(listp thing)
      (trestrul:mapleaf #'patternize thing)
      (Envar thing))))

;;; <type-class-relation-setter>
(defun <type-class-relation-setter>(name super-classes)
  `((SETF (TYPE-DIRECT-SUPERCLASSES(GET ',name 'TYPE-CLASS))',super-classes)
    ,@(loop :for type-class :in super-classes
	    :collect
	    `(PUSH ',name (TYPE-DIRECT-SUBCLASSES(GET ',type-class 'TYPE-CLASS))))))

;;; <instance-info-setter>
(defun <instance-info-setter>(method name lambda-list return-type rest)
  `(SETF (GET ',method 'INSTANCE)
	 (INSTANCE-INFO :TYPE-CLASS ',name
			:LAMBDA-LIST ',lambda-list
			:RETURN-TYPE ',return-type
			,@(let((default(find method rest :key #'cadr)))
			    (when default
			      `(:DEFAULT '(LAMBDA,@(cddr default))))))))

;;; <instance-compiler-macro>
(defun <instance-compiler-macro>(method gensyms lambda-list return-type)
  `(DEFINE-COMPILER-MACRO,method(&WHOLE WHOLE ,@gensyms &ENVIRONMENT ENV)
     (SETF ; as canonicalise. In order to retrieve return type.
       ,@(loop :for gensym :in gensyms
	       :append `(,gensym (EXPANDER:EXPAND ,gensym))))
     (LET((INFOS(COMPUTE-RETURN-TYPES (list ,@gensyms) ENV)))
       ,@(when(cdr lambda-list)
	   `((CHECK-SIGNATURE ',lambda-list INFOS)))
       (LET((IL(GET-INSTANCE-LAMBDA ',method INFOS)))
	 (IF IL
	     ,(if(millet:type-specifier-p return-type)
		`(LIST 'THE ',return-type (LIST IL ,@gensyms))
		`(LET((RETURN(SUBSTITUTE-PATTERN ',return-type (UNIFY:UNIFY ',lambda-list (SUBST '_ '* INFOS)))))
		   (IF RETURN
		       (LIST 'THE RETURN (LIST IL ,@gensyms))
		       (LIST IL ,@gensyms))))
	     (PROGN (WHEN *COMPILE-FILE-PATHNAME*
		      (WARN "Can not get instance of ~S" WHOLE))
		    ;; In order to avoid expanding macros twice,
		    ;; we should use canonicalized `GENSYMS`.
		    ;; And in order to trick `compiler-macroexpand-1` returns nil
		    ;; as second value, we must destructively modify `WHOLE`.
		    ;; Or `compiler-macroexpand` get into infinite expanding.
		    (RPLACD WHOLE (LIST ,@gensyms))))))))

(defmethod unify:unify :around ((a symbol)(b symbol)
				&optional (env (unify:make-empty-environment))
				&key &allow-other-keys)
  (cond ((unify:variable-any-p a)env)
	((unify:variablep a)(unify::var-unify a b env))
	((unify:variable-any-p b) env)
	((unify:variablep b)(unify::var-unify b a env))
	((eq a b)env)
	((%compatible-type-p b a)env)
	(t (error 'unify::unification-failure
		  :format-control "Cannot unify two different symbols: ~S ~S"
		  :format-arguments (list a b)))))

(defmethod unify:unify ((a (eql t))(b list)
			&optional (env (unify:make-empty-environment))
			&key &allow-other-keys)
  (extend-environment-with-t b env))

(defun extend-environment-with-t(pattern env)
  (trestrul:traverse (lambda(x)
		       (when(and (unify:variablep x)
				 (null(nth-value 1 (unify:find-variable-value x env))))
			 (unify:extend-environment x t env)))
		     pattern)
  env)

(defmethod unify:unify ((b list)(a (eql t))
			&optional (env (unify:make-empty-environment))
			&key &allow-other-keys)
  (extend-environment-with-t b env))

(defmethod unify:unify ((a (eql 'function))(b list)
			&optional (env (unify:make-empty-environment))
			&key &allow-other-keys)
  (if(eq 'function (car b))
    (unify:unify t b env)
    (if(not(unify:variablep(car b)))
      (call-next-method)
      (if(cdddr b)
	(call-next-method)
	(if(unify:find-variable-value (car b)env)
	  (extend-environment-with-t (cdr b)env)
	  (extend-environment-with-t (cdr b)
				     (unify:extend-environment (car b) a env)))))))

(defmethod unify:unify ((b list)(a (eql 'function))
			&optional (env (unify:make-empty-environment))
			&key &allow-other-keys)
  (if(eq 'function (car b))
    (unify:unify t b env)
    (if(not(unify:variablep(car b)))
      (call-next-method)
      (if(cdddr b)
	(call-next-method)
	(if(unify:find-variable-value (car b)env)
	  (extend-environment-with-t (cdr b)env)
	  (extend-environment-with-t (cdr b)
				     (unify:extend-environment (car b) a env)))))))

(defstruct(constant (:constructor wrap-value(value))(:copier nil))
  (value nil :read-only t))

(define-condition internal-logical-error(cell-error)
  ((datum :initarg :datum :accessor error-datum))
  (:report(lambda(c *standard-output*)
	    (format t "INTERNAL LOGICAL ERROR: ~S~%~S trapped with ~S."
		    (type-of c)
		    (cell-error-name c)
		    (error-datum c)))))
(define-condition exhausts-clauses(internal-logical-error)())
(define-condition unexpected-macro(internal-logical-error)())
(define-condition unexpected-quote(internal-logical-error)())
(define-condition unexpected-local-macro(internal-logical-error)())

(defun compute-return-types(var* &optional env)
  (loop :for var :in var*
	:collect (compute-return-type var env)))

(defun compute-return-type(var &optional env)
  (cond
    ((constantp var) ; lisp object.
     (let((value(introspect-environment:constant-form-value var env)))
       (if(adv-p value) ; literal adt.
	 (data-type-of value)
	 (wrap-value value))))
    ((symbolp var) ; free variable.
     (or (introspect-environment:variable-type var env)
	 T))
    ((and (listp var)
	  (instance-p (car var))) ; instance call.
     (compute-instance-call-return-type var))
    ((and (listp var)
	  (action-boundp (car var))) ; action call.
     (action-type(action-boundp (car var))))
    ((adv-p var) ; constructor call.
     (data-type-of var))
    ((and (listp var)
	  (symbolp (car var)))
     (compute-standard-form-return-type var env))
    (t (error 'exhausts-clauses :name 'compute-return-type :datum var))))

(defun compute-instance-call-return-type(call-form)
  (let((pattern(instance-return-type(car call-form)))
       (environment(unify:unify (instance-lambda-list(car call-form))
				(subst '_ '* (compute-return-types(cdr call-form))))))
    (substitute-pattern pattern environment)))

(defun substitute-pattern(pattern environment)
  (subst '* '_
	 (trestrul:asubst-if (lambda(var)
			       (let((return-type (unify:find-variable-value var environment)))
				 (if(typep return-type '(cons (eql values)t))
				   (cadr return-type)
				   return-type)))
			     #'unify:variablep
			     pattern)))

(defun compute-standard-form-return-type(form env)
  (multiple-value-bind(type localp declaration)(introspect-environment:function-information (car form)env)
    (declare(ignore localp))
    (case type
      ((nil) (warn "Undefined function ~S. ~S"(car form)form))
      (:special-form (special-operator-return-type form env))
      (:macro (error 'unexpected-macro :name 'compute-standard-form-return-type
		     :datum form))
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
       (compute-return-type(car(last(second form)))env)
       (introspect-environment:function-type(second form)env)))
    ((if)
     `(or ,(compute-return-type(third form)env)
	  ,(compute-return-type(fourth form)env)))
    ((quote)
     (error 'unexpected-quote :datum form :name 'special-operator-return-type))
    ((macrolet symbol-macrolet)
     (error 'unexpected-local-macro :datum form :name 'special-operator-return-type))
    (otherwise ; give up. (go throw catch return-from block)
      t)))

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

;;; <instance-interpreter>
(defun <instance-interpreter>(method gensyms lambda-list)
  `(DEFUN,method,gensyms
     ,@(when(cdr lambda-list)
	 `((CHECK-SIGNATURE ',lambda-list (LIST ,@(mapcar (lambda(sym)
							    `(DATA-TYPE-OF ,sym))
							  gensyms)))))
     (LET((INSTANCE(OR (GET-INSTANCE-LAMBDA ',method (LIST ,@(loop :for s :in gensyms
								   :collect `(DATA-TYPE-OF ,s))))
		       (INSTANCE-DEFAULT ',method))))
       (IF INSTANCE
	   (LET((DECLARED(INSERT-DECLARE INSTANCE (LIST ,@gensyms))))
	     (FUNCALL (COERCE DECLARED 'FUNCTION)
		      ,@gensyms))
	   (ERROR "Instance is not found. ~S ~S"',method (LIST ,@gensyms))))))

(defun insert-declare(form values)
  (labels((actual-type(type value)
	    (if(listp value)
	      (rec(cdr type)(cdr value)(list (car type)))
	      type))
	  (rec(type value acc)
	    (if (endp value)
	      (nreconc acc type)
	      (body (car type)(cdr type)(car value)(cdr value)acc)))
	  (body(type type-rest value value-rest acc)
	    (if(eq '* type)
	      (rec type-rest value-rest (push (class-name(class-of value))acc))
	      (rec type-rest value-rest (push type acc))))
	    )
    `(,@(subseq form 0 2)
       (DECLARE,@(mapcar (lambda(value var)
			   (if(adv-p value)
			     (let((type(data-type-of value)))
			       (if(listp type)
				 `(TYPE ,(actual-type type value),var)
				 `(TYPE ,type ,var)))
			     `(TYPE ,(type-of value),var)))
			 values
			 (cadr form)))
       ,@(subseq form 2))))

;;;; CHECK-SIGNATURE
(defun check-signature($pattern $type*)
  (labels((rec(pattern type* acc)
	    (if(endp pattern)
	      (if(endp type*)
		acc ; <--- For debug use.
		(error "Unmatch length ~S ~S" $pattern $type*))
	      (if(endp type*)
		(error "Unmatch length ~S ~S" $pattern $type*)
		(body (car pattern)(cdr pattern)(car type*)(cdr type*)acc))))
	  (body(pat pat-rest type type-rest acc)
	    (let((seen(assoc pat acc :test #'equal)))
	      (if seen
		(if(compatible-type-p type (cdr seen))
		  (rec pat-rest type-rest (progn (pushnew type (cdr seen):test #'equalp)
						 acc))
		  (error "Uncompatible type ~S ~S"type seen))
		(rec pat-rest type-rest (push(%check pat type)acc)))))
	  )
    (rec (cdr $pattern)(cdr $type*)`(,(%check(car $pattern)(car $type*))))))

(defun %check(pattern type)
  (if(atom pattern)
    (list pattern type)
    (if(not(eq 'function (car pattern)))
      (progn (unify:unify pattern (subst '_ '* type))
	     (list pattern type))
      (cond
	((constant-p type)
	 (let((ftype(cdr(assoc 'ftype (nth-value 2 (introspect-environment:function-information (constant-value type)))))))
	   (unify:unify pattern (subst '_ '* ftype))
	   (list pattern ftype)))
	((typep type '(cons (eql function) t))
	 (unify:unify pattern (subst '_ '* type))
	 (list pattern type))
	((or (eq t type)(eq 'function type))
	 (when *compile-file-pathname*
	   (warn "Could not match ~S ~S"pattern type))
	 (list pattern type))
	(t (error "%CHECK: Unknown type comes.~%TYPE: ~S" type))))))

(defun compatible-type-p(type type*)
  (loop :for t1 :in type*
	:always (%compatible-type-p type t1)))

(defun %compatible-type-p(t1 t2)
  (if(constant-p t1)
    (if(constant-p t2)
      (%compatible-type-p (type-of(constant-value t1))
			  (type-of(constant-value t2)))
      (typep (constant-value t1)t2))
    (if(constant-p t2)
      (typep (constant-value t2)t1)
      (subtype? t1 t2))))

(defun subtype?(t1 t2)
  (if(millet:type-specifier-p t1)
    (if(millet:type-specifier-p t2)
      (subtypep t1 t2)
      (or (eql t t1)
	  (unify:unify t1 (patternize t2))))
    (if(millet:type-specifier-p t2)
      (unless(eql t t2)
	(unify:unify (patternize t1)t2))
      (unify:unify (patternize t1)(patternize t2)))))

;;;; GET-INSTANCE-LAMBDA
(defun get-instance-lambda(interface type*)
  (if(every (lambda(x)
	      (eq t x))
	    type*)
    nil
    (or (compute-applicable-instance(collect-instance type* interface))
	(instance-default interface))))

;;;; COLLECT-INSTANCE
(defun collect-instance(type* interface)
  (remove-if-not (lambda(ts2)
		   (every #'%compatible-type-p type* ts2))
		 (instance-table interface)
		 :key #'car))

;;;; COMPUTE-APPLICABLE-INSTANCE
(defun compute-applicable-instance(list)
  (cdar(sort-instance list)))

(defun sort-instance(list)
  (flet((type<(ts1 ts2)
	  (every #'subtypep ts1 ts2)))
    (sort list #'type< :key #'car)))

;;;; DEFISTANCE
(defmacro definstance(interface instance-lambda-list &body body)
  (flet((parse-lambda-list(lambda-list)
	  (loop :for elt :in lambda-list
		:collect (car elt) :into vars
		:collect (cadr elt):into types
		:finally (return (values vars types)))))
    (multiple-value-bind(vars types)(parse-lambda-list instance-lambda-list)
      (let((scs(type-direct-superclasses(get(instance-type-class interface)'type-class))))
	(dolist(sc scs)
	  (assert(or (get-instance-lambda sc types)
		     (instance-default sc)))))
      `(progn (add-instance ',interface
			    ',types
			    '(lambda,vars,@body))
	      ',interface))))

;;;; ADD-INSTANCE
(defun add-instance(interface type* lambda-form)
  (push(cons type* lambda-form)(instance-table interface)))

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
