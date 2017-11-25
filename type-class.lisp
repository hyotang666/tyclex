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
  (map-into vars #'envar vars)
  ;; body
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (SETF(GET ',name 'TYPE-CLASS)(MAKE-INFO :NAME ',name))
     ,@(when super-classes
	 (<type-class-relation-setter> name super-classes))
     ,@(loop
	 :for (method lambda-list return-type) :in methods
	 :for gensyms = (gensyms lambda-list)
	 :do (setf ; as canonicalise
	       lambda-list (patternize lambda-list)
	       return-type (patternize return-type))
	 :collect (<instance-info-setter> method name lambda-list return-type rest)
	 :collect (<instance-compiler-macro> method gensyms lambda-list return-type)
	 :collect (<instance-interpreter> method gensyms lambda-list))
     ',name))

(defun envar(thing)
  (intern(format nil "?~A"thing)))

(defun patternize(thing)
  (if(millet:type-specifier-p thing)
    thing
    (if(listp thing)
      (trestrul:mapleaf #'patternize thing)
      (envar thing))))

(defun gensyms (list)
  (loop :repeat (length list)
	:collect (gensym)))

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
  `(DEFINE-COMPILER-MACRO,method(,@gensyms &ENVIRONMENT ENV)
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
	     ;; In order to avoid expanding macros twice.
	     (LET((WHOLE(LIST ',method ,@gensyms)))
	       (WARN "Can not get instance of ~S" WHOLE)
	       WHOLE))))))

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
    ((adt-p var) ; adt literal.
     (data-type-of var))
    ((constantp var) ; lisp object.
     (wrap-value(introspect-environment:constant-form-value var env)))
    ((symbolp var) ; free variable.
     (or (introspect-environment:variable-type var env)
	 T))
    ((and (listp var) ; instance call.
	  (instance-p (car var)))
     (compute-instance-call-return-type var))
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
  (trestrul:asubst-if (lambda(var)
			(let((return-type (unify:find-variable-value var environment)))
			  (if(typep return-type '(cons (eql values)t))
			    (cadr return-type)
			    return-type)))
		      #'unify:variablep
		      pattern))

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
     (LET((INSTANCE(GET-INSTANCE ',method (LIST ,@gensyms))))
       (IF INSTANCE
	   (FUNCALL (COERCE (INSERT-DECLARE INSTANCE
					    (LIST ,@gensyms))
			    'FUNCTION)
		    ,@gensyms)
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
			   (if(adt-p value)
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
		T
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
  (if(listp pattern)
    (if(eq 'function (car pattern))
      (cond
	((constant-p type)
	 (let((ftype(cdr(assoc 'ftype (nth-value 2 (introspect-environment:function-information (constant-value type)))))))
	   (unify:unify pattern (subst '_ '* ftype))
	   (list pattern ftype)))
	((typep type '(cons (eql function) t))
	 (unify:unify pattern (subst '_ '* type))
	 (list pattern type))
	((or (eq t type)(eq 'function type))
	 (warn "Could not match ~S ~S"pattern type)
	 (list pattern type))
	(t (error "%CHECK: Unknown type comes.~%TYPE: ~S" type)))
      (progn (unify:unify pattern (subst '_ '* type))
	     (list pattern type)))
    (list pattern type)))

(defun compatible-type-p(type type*)
  (loop :for t1 :in type*
	:always (%compatible-type-p t1 type)))

(defun %compatible-type-p(t1 t2)
  (if(constant-p t1)
    (if(constant-p t2)
      (%compatible-type-p (type-of(constant-value t1))
			  (type-of(constant-value t2)))
      (typep (constant-value t1)t2))
    (if(constant-p t2)
      (typep (constant-value t2)t1)
      (subtypep t1 t2))))

;;;; GET-INSTANCE-LAMBDA
(defun get-instance-lambda(interface type*)
  (if(every (lambda(x)
	      (eq t x))
	    type*)
    (instance-default interface)
    (or (cdr(assoc type* (instance-table interface)
		   :test (lambda(ts1 ts2)
			   (every #'%compatible-type-p ts1 ts2))))
	(instance-default interface))))

;;;; GET-INSTANCE
(defun get-instance(interface value*)
  (let*((default(instance-default interface))
	(collected(collect-instance value* (instance-table interface))))
    (compute-applicable-instance (if default
				   (acons (mapcar (constantly t)(cadr default))
					  default collected)
				   collected))))

;;;; COLLECT-INSTANCE
(defun collect-instance(value* table)
  (loop :for kv :in table
	:when (every #'typep value* (car kv))
	:collect kv))

;;;; COMPUTE-APPLICABLE-INSTANCE
(defun compute-applicable-instance(kvs)
  (flet((type<(ts1 ts2)
	  (every #'subtypep ts1 ts2)))
    (cdar (sort kvs #'type< :key #'car))))

;;;; DEFISTANCE
(defmacro definstance(interface instance-lambda-list &body body)
  (flet((parse-lambda-list(lambda-list)
	  (loop :for elt :in lambda-list
		:collect (car elt) :into vars
		:collect (let((type(cadr elt)))
			   (if(find type '(list keyword):test #'eq)
			     (ERROR "Invalid type. ~S"type)
			     type))
		:into types
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
(progn
  (define-type-class(eq a)()
    ((==(a a) boolean)
     (/==(a a) boolean))
    (:default == (x y)
	      (not (/== x y)))
    (:default /== (x y)
	      (not (== x y))))
  (definstance == ((a number)(b number))
    (= a b))
  (defdata maybe (a) :nothing (just a))
  (named-readtables:in-readtable :cl-vs-haskell)
  (definstance == ((a (maybe *))(b (maybe *)))
    (trivia:match*(a b)
      ((:nothing :nothing)T)
      ((#[just x]#[just y])(== x y)) ; <--- == introduce error. why?
      ((_ _) nil)))
  )
;|#
