(in-package :cl-user)
(defpackage :tyclex.type-matcher
  (:use :cl :tyclex.unifier)
  (:shadowing-import-from :tyclex.objects.newtype #:list)
  (:import-from :tyclex.objects.newtype #:newtype-type-specifier-p)
  (:import-from :tyclex.objects.adt #:adt-type-specifier-p #:lflatten)
  (:import-from :tyclex.objects.adt-constructor #:first-atom)
  (:export
    #:type-match-p #:great-common-type
    ))
(in-package :tyclex.type-matcher)

(defmethod Unify :around ((a symbol)(b symbol)
			  &optional (env (Make-empty-environment))
			  &key &allow-other-keys)
  (cond ((Variable-any-p a)env)
	((Variablep a)(tyclex.unifier::var-unify a b env))
	((Variable-any-p b) env)
	((Variablep b)(tyclex.unifier::var-unify b a env))
	((eq a b)env)
	((type-match-p b a T)env)
	(t (call-next-method))))

(defmacro defunify(lambda-list &body body)
  `(PROGN (DEFMETHOD UNIFY(,@lambda-list
			    &OPTIONAL (ENV (MAKE-EMPTY-ENVIRONMENT))
			    &KEY &ALLOW-OTHER-KEYS)
		     ,@body)
	  (DEFMETHOD UNIFY(,@(reverse lambda-list)
			    &OPTIONAL (ENV (MAKE-EMPTY-ENVIRONMENT))
			    &KEY &ALLOW-OTHER-KEYS)
		     ,@body)))

(defunify((a (eql t))(b cl:list))
  (extend-environment-with-t b env))

(defun extend-environment-with-t(pattern env)
  (trestrul:traverse (lambda(x)
		       (when(and (variablep x)
				 (null(nth-value 1 (Find-variable-value x env))))
			 (Extend-environment x t env)))
		     pattern)
  env)

(defunify((a (eql 'function))(b cl:list))
  (if(eq 'function (car b))
    (Unify t b env)
    (if(not(Variablep(car b)))
      (call-next-method)
      (if(cdddr b)
	(call-next-method)
	(if(Find-variable-value (car b)env)
	  (extend-environment-with-t (cdr b)env)
	  (extend-environment-with-t (cdr b)
				     (Extend-environment (car b) a env)))))))

(defunify((a (eql 'cons))(b cl:list))
  (if(Variablep(car b))
    (extend-environment-with-t (cdr b)
			       (Extend-environment(car b)a env))
    (if(eq 'list (car b))
      env
      (call-next-method))))

(defunify((a (eql 'null))(b cl:list))
  (if(not(Variablep(car b)))
    (if(find (car b) '(cl:list list cons) :test #'eq)
      (let((variable(find-value-variable a env)))
	(if variable
	  (replace-bind variable b env)
	  env))
      (call-next-method))
    (if(cddr b)
      (call-next-method)
      (extend-environment-with-t (cdr b)
				 (Extend-environment(car b)'list env)))))

(defunify((a (eql 'null))(b (eql 'cons)))
  (let((variable(find-value-variable a env)))
    (if variable
      (replace-bind variable b env)
      env)))

#++original[fail]
; I do not why this code fails to unexpected diverge. (in SBCL, ECL)
(defmethod Unify :around ((a cl:list)(b cl:list)
			  &optional(env(Make-empty-environment))
			  &key &allow-other-keys)
  (matrix-case:matrix-etypecase((car a)(car b))
    (((satisfies Variablep)(eql function))
     (setf b (ensure-value b))
     (Unify (cdr a)
	    (or (cddr b) ; lisp style, e.g. (function(arg)return)
		(cdr b)) ; probably haskell style, e.g. (function return).
	    (Extend-environment (car a) (car b) env)))
    (((eql function)(satisfies Variablep))
     (setf a (ensure-value a))
     (Unify (or (cddr a) ; lisp style, e.g. (function(arg)return)
		(cdr a)) ; probably haskell style, e.g. (function return).
	    (cdr b)
	    (Extend-environment (car b)(car a)env)))
    (((satisfies Variablep)t)
     (let((len-a (length a))
	  (len-b (length b)))
       (cond
	 ((= len-a len-b)(call-next-method))
	 ((< len-a len-b)(call-next-method a (curry-simulate b a)))
	 (t (call-next-method)))))
    ((t (satisfies Variablep))
     (let((len-a (length a))
	  (len-b (length b)))
       (cond
	 ((= len-a len-b)(call-next-method))
	 ((< len-a len-b)(call-next-method))
	 (t (call-next-method (curry-simulate a b) b)))))
    (otherwise (call-next-method))))

;#++macroexpand-1ed-when-readtime[success]
; I do not why but this one works. (in ECL, SBCL)
(defmethod Unify :around ((a cl:list)(b cl:list)
			  &optional(env(Make-empty-environment))
			  &key &allow-other-keys)
  #.(macroexpand-1'(matrix-case:matrix-etypecase((car a)(car b))
		     (((satisfies Variablep)(eql function))
		      (setf b (ensure-value b))
		      (Unify (cdr a)
			     (or (cddr b) ; lisp style, e.g. (function(arg)return)
				 (cdr b)) ; probably haskell style, e.g. (function return).
			     (Extend-environment (car a) (car b) env)))
		     (((eql function)(satisfies Variablep))
		      (setf a (ensure-value a))
		      (Unify (or (cddr a) ; lisp style, e.g. (function(arg)return)
				 (cdr a)) ; probably haskell style, e.g. (function return).
			     (cdr b)
			     (Extend-environment (car b)(car a)env)))
		     (((satisfies Variablep)t)
		      (let((len-a (length a))
			   (len-b (length b)))
			(cond
			  ((= len-a len-b)(call-next-method))
			  ((< len-a len-b)(call-next-method a (curry-simulate b a)env))
			  (t (call-next-method)))))
		     ((t (satisfies Variablep))
		      (let((len-a (length a))
			   (len-b (length b)))
			(cond
			  ((= len-a len-b)(call-next-method))
			  ((< len-a len-b)(call-next-method))
			  (t (call-next-method (curry-simulate a b) b env)))))
		     (otherwise (call-next-method)))))

(defun curry-simulate(long short)
  (labels((rec(count list)
	    (if(zerop count)
	      list
	      (rec (1- count)(cons (subseq list 0 2)(cddr list))))))
    (rec (- (length long)(length short))long)))

(defunify((a (eql 'cons))(b (eql 'list)))
  env)

(defunify((a (eql 'list))(b cl:list))
  (if(Variablep(car b))
    (Extend-environment (car b)a env)
    (if(eq a (car b)) ; list (list *)
      env
      (call-next-method))))

(defunify((a (eql 'cl:list))(b cl:list))
  (if(Variablep(car b))
    (Extend-environment (car b)a env)
    (if(string= a (car b)) ; list (list *)
      env
      (call-next-method))))

(defunify((a (eql 'vector))(b cl:list))
  (if(variablep(car b))
    (extend-environment (car b) a env)
    (if(or (eq a (car b))
	   (eq 'simple-vector (car b)))
      env
      (call-next-method))))

(defunify((a (eql 'simple-vector))(b cl:list))
  (if(variablep(car b))
    (extend-environment (car b) a env)
    (if(or (eq a (car b))
	   (eq 'vector (car b)))
      env
      (call-next-method))))

(defunify((a (eql 'string))(b cl:list))
  (if(variablep(car b))
    (extend-environment (car b) a env)
    (if(eq a (car b))
      env
      (call-next-method))))

(defunify((a (eql 'array))(b cl:list))
  (if(variablep(car b))
    (extend-environment (car b) a env)
    (if(eq a (car b))
      env
      (call-next-method))))

(defunify((a (eql 'hash-table))(b cl:list))
  (if(variablep(car b))
    (extend-environment (car b) a env)
    (if(eq a (car b))
      env
      (call-next-method))))

(defunify((a symbol)(b cl:list))
  (if(Variablep a)
    (if(eq 'function (car b)) ; ?B (FUNCTION(?A)?B)
      (Extend-environment a b env)
      (if(not(Newtype-type-specifier-p b))
	(call-next-method)
	(let*((new-env(call-next-method))
	      (value(Find-variable-value a new-env)))
	  (if(eq value b)
	    new-env
	    (flet((ENSURE-WILDCARD(thing)
		    (if (trestrul:find-leaf-if #'Variable-any-p thing)
		      (Subst-wildcard-to-var thing)
		      thing))
		  (ENSURE-VAR(thing)
		    (if (trestrul:find-leaf-if #'Gensymed-var-p thing)
		      (Subst-var-to-wildcard thing)
		      thing))
		  (APPLY-SUBST(env pattern)
		    (handler-bind((simple-warning #'muffle-warning))
		      (Apply-substitution env pattern))))
	      (let*((b% (ENSURE-WILDCARD b))
		    (v (ENSURE-WILDCARD value))
		    (e (if (and (eq b% b)
				(eq v value))
			 new-env
			 (Unify b% v))))
		(Replace-bind a
			      (great-common-type (ENSURE-VAR (APPLY-SUBST e b%))
						 (ENSURE-VAR (APPLY-SUBST e v)))
			      new-env)))))))
    (if(Newtype-type-specifier-p a)
      (if(Variablep (car b))
	(if(Find-variable-value (car b) env)
	  env
	  (Extend-environment (car b) a env))
	(if(type-match-p a b)
	  env
	  (call-next-method)))
      (if(Newtype-type-specifier-p b)
	(unify a (millet:type-expand b)env)
	(call-next-method)))))

(defunify((a (eql t))(b symbol))
  (if(Variablep b)
    (Extend-environment a t env)
    env))

(defun ensure-value(ftype-spec)
  (trestrul:asubst-if #'second
		      (lambda(elt)
			(typep elt '(cons (eql values) *)))
		      ftype-spec))

;;;; TYPE-MATCH-P
(defun type-match-p(t1 t2 &optional reccursivep)
  (labels((car-eq(t1 t2)
	    (eq (First-atom t1)(First-atom t2))))
    (matrix-case:matrix-case((category-of t1)(category-of t2))
      ((:newtype	:newtype)					(car-eq t1 t2))
      ((:newtype	(:adt :list :function))				nil)
      ((:newtype	:type-specifier)
       (multiple-value-bind(ts expanded?)(millet:type-expand t1)
	 (if expanded?
	   (type-match-p ts t2)
	   nil)))
      ((:adt		:adt)
       (matrix-case:matrix-typecase(t1 t2)
	 ((list list)(and (car-eq t1 t2)
			  (every #'type-match-p
				 (cdr (Lflatten t1))
				 (cdr (Lflatten t2)))))
	 (otherwise (car-eq t1 t2))))
      ((:adt		(:newtype :type-specifier :function :list))	nil)
      ((:type-specifier	:function)					nil)
      ((:type-specifier :newtype)
       (multiple-value-bind(ts expanded?)(millet:type-expand t2)
	 (if expanded?
	   (type-match-p t1 ts)
	   nil)))
      ((:type-specifier	:list)						(eq 'null t1))
      ((:type-specifier	:type-specifier)				(subtypep t1 t2))
      ((:type-specifier	:adt)
       (when(millet:type-specifier-p t2)
	 (subtypep t1 t2)))
      ((:wildcard	t)						T)
      ((t		:wildcard)					T)
      ((:function	(:newtype :adt :type-specifier :list))		nil)
      ((:function	:function)					T)
      ((:list		:list)
       (matrix-case:matrix-etypecase(t1 t2)
	 ((list list)(every #'type-match-p (cdr t1)(cdr t2)))
	 (otherwise t)))
      ((:list		(:adt :function))				nil)
      ((:list		:newtype)
       (if(eq 'cons (First-atom t1))
	 (multiple-value-bind(ts expanded?)(millet:type-expand t2)
	   (if expanded?
	     (type-match-p t1 ts)
	     nil))
	 nil))
      ((:list		:type-specifier)				(eq 'null t2))
      ((t :satisfier)							(typep t1 t2))
      (otherwise (if reccursivep
		   nil
		   (Ignore-unification-failure(unify t1 t2)))))))

(defun category-of(thing)
  (cond
    ((Newtype-type-specifier-p thing):newtype)
    ((Adt-type-specifier-p thing) :adt)
    ((or (find thing '(* T))
	 (variablep thing))
     :wildcard)
    ((millet:type-specifier-p thing)
     (cond
       ((or (eq 'function thing)
	    (typep thing '(cons (eql function)t)))
	:function)
       ((or (eq 'list thing)
	    (eq 'cl:list thing)
	    (eq 'cons thing)
	    (typep thing '(cons (eql list)t))
	    (typep thing '(cons (eql cons)t)))
	:list)
       ((typep thing '(cons (eql satisfies)))
	:satisfier)
       (t :type-specifier)))
    (t (cond
	 ((typep thing '(cons (eql function)t)) :function)
	 ((typep thing '(cons (eql cl:list)t)) :list)
	 ((typep thing '(cons (eql cons)t)) :list)
	 (t :unknown)))))

;;;; GREAT-COMMON-TYPE
(defun great-common-type(t1 t2 &optional reccursivep)
  (setf t1 (dewild t1) t2 (dewild t2))
  (matrix-case:matrix-case((type-category-of t1)(type-category-of t2))
    ((:function		:function)		'function)
    ((:function		:class)			(find-diverging-class 'function t2))
    ((:function		(:compound :type))	(find-diverging-class 'function t2))
    ((t			:type-variable)		(if (eq t t1) t1 t2))
    ((t			:unknown)		(error "nyi ~S ~S"t1 t2))
    ((:function		:may-pattern)
     (or (Ignore-unification-failure(Unify t1 t2))
	 t2))
    ((:class		:class)			(find-diverging-class t1 t2))
    ((:class		:function)		(find-diverging-class t1 'function))
    ((:class		(:compound :type))	(find-diverging-class t1 t2))
    (((:type :class :compound)	:may-pattern)	T) ; give up.
    (((:compound :type)	(:compound :type))
     (setf t1 (subst t '* t1)
	   t2 (subst t '* t2))
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
     (if(string= t1 t2)
       t1
       (error "nyi ~S ~S"t1 t2)))
    ((:type-variable	t)			(if (eq t t2) t2 t1))
    ((:unknown		:unknown)		(error "nyi ~S ~S" t1 t2))
    ((:unknown		t)			(error "nyi ~S ~S" t1 t2))
    ((:may-pattern	:may-pattern)		(mapcar #'great-common-type t1 t2))
    ((:may-pattern	:function)
     (or (Ignore-unification-failure(Unify t1 t2))
	 t1))
    ((:may-pattern	(:type :class :compound))	t) ; give up.
    ))

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
      (if(Variablep type)
	:type-variable
	:unknown)
      (if(typep type '(cons (eql function)t))
	:function
	:may-pattern))))

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

