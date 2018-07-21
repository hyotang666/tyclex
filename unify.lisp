(in-package :vs-haskell)

(defun envar(thing)
  (if(char= #\? (char(string thing)0))
    thing
    (intern(format nil "?~A"thing))))

(defun patternize(thing)
  (if(millet:type-specifier-p thing)
    thing
    (if(listp thing)
      (trestrul:mapleaf #'patternize thing)
      (Envar thing))))

(defun enwild (type-spec)
  (sublis'((* . _)(T . _))type-spec))

(defun dewild (pattern)
  (subst T '_ pattern))

(defmethod type-unify:unify :around ((a symbol)(b symbol)
				     &optional (env (type-unify:make-empty-environment))
				     &key &allow-other-keys)
  (cond ((type-unify:variable-any-p a)env)
	((type-unify:variablep a)(type-unify::var-unify a b env))
	((type-unify:variable-any-p b) env)
	((type-unify:variablep b)(type-unify::var-unify b a env))
	((eq a b)env)
	((subtype? b a)env)
	(t (call-next-method))))

(defmacro defunify(lambda-list &body body)
  `(PROGN (DEFMETHOD TYPE-UNIFY:UNIFY(,@lambda-list
				       &OPTIONAL (ENV (TYPE-UNIFY:MAKE-EMPTY-ENVIRONMENT))
				       &KEY &ALLOW-OTHER-KEYS)
		     ,@body)
	  (DEFMETHOD TYPE-UNIFY:UNIFY(,@(reverse lambda-list)
				       &OPTIONAL (ENV (TYPE-UNIFY:MAKE-EMPTY-ENVIRONMENT))
				       &KEY &ALLOW-OTHER-KEYS)
		     ,@body)))

(defunify((a (eql t))(b cl:list))
  (extend-environment-with-t b env))

(defun extend-environment-with-t(pattern env)
  (trestrul:traverse (lambda(x)
		       (when(and (type-unify:variablep x)
				 (null(nth-value 1 (type-unify:find-variable-value x env))))
			 (type-unify:extend-environment x t env)))
		     pattern)
  env)

(defunify((a (eql 'function))(b cl:list))
  (if(eq 'function (car b))
    (type-unify:unify t b env)
    (if(not(type-unify:variablep(car b)))
      (call-next-method)
      (if(cdddr b)
	(call-next-method)
	(if(type-unify:find-variable-value (car b)env)
	  (extend-environment-with-t (cdr b)env)
	  (extend-environment-with-t (cdr b)
				     (type-unify:extend-environment (car b) a env)))))))

(defunify((a (eql 'cons))(b cl:list))
  (if(type-unify:variablep(car b))
    (extend-environment-with-t (cdr b)
			       (type-unify:extend-environment(car b)a env))
    (if(eq 'list (car b))
      env
      (call-next-method))))

(defunify((a (eql 'null))(b cl:list))
  (if(not(type-unify:variablep(car b)))
    (call-next-method)
    (if(cddr b)
      (call-next-method)
      (extend-environment-with-t (cdr b)
				 (type-unify:extend-environment(car b)'list env)))))

(defmethod type-unify:unify :around ((a cl:list)(b cl:list)
				     &optional(env(type-unify:make-empty-environment))
				     &key &allow-other-keys)
  (trivia:match*((car a)(car b))
    (((satisfies type-unify:variablep)(eq 'function))
     (setf b (ensure-value b))
     (type-unify:unify (cdr a)
		       (or (cddr b) ; lisp style, e.g. (function(arg)return)
			   (cdr b)) ; probably haskell style, e.g. (function return).
		       (type-unify:extend-environment (car a) (car b) env)))
    (((eq 'function)(satisfies type-unify:variablep))
     (setf a (ensure-value a))
     (type-unify:unify (or (cddr a) ; lisp style, e.g. (function(arg)return)
			   (cdr a)) ; probably haskell style, e.g. (function return).
		       (cdr b)
		       (type-unify:extend-environment (car b)(car a)env)))
    ((_ _)(call-next-method))))

(defunify((a (eql 'cons))(b (eql 'list)))
  env)

(defunify((a (eql 'list))(b cl:list))
  (if(type-unify:variablep(car b))
    (type-unify:extend-environment (car b)a env)
    (if(eq a (car b)) ; list (list *)
      env
      (call-next-method))))

(defunify((a (eql 'cl:list))(b cl:list))
  (if(type-unify:variablep(car b))
    (type-unify:extend-environment (car b)a env)
    (if(string= a (car b)) ; list (list *)
      env
      (call-next-method))))

(defunify((a symbol)(b cl:list))
  (if(type-unify:variablep a)
    (if(eq 'function (car b)) ; ?B (FUNCTION(?A)?B)
      (type-unify:extend-environment a b env)
      (call-next-method))
    (if(newtypep a)
      (if(type-unify:variablep (car b))
	(if(type-unify:find-variable-value (car b) env)
	  env
	  (type-unify:extend-environment (car b) a env))
	(call-next-method))
      (call-next-method))))

(defunify((a (eql t))(b symbol))
  (if(type-unify:variablep b)
    (type-unify:extend-environment a t env)
    env))

(defun ensure-value(ftype-spec)
  (trestrul:asubst-if #'second
		      (lambda(elt)
			(typep elt '(cons (eql values) *)))
		      ftype-spec))


(defparameter *subtype-verbose* T)

(defmacro with-subtype-verbose(form)
  `(handler-case,form
     (error()(when *subtype-verbose*
	       (warn "Unify fails. ~S" (list ',(car form)
					     ,@(cdr form)))))))

(defun subtype?(t1 t2)
  (labels((car-eq(t1 t2)
	    (matrix-case:matrix-typecase(t1 t2)
	      ((symbol symbol)(eq t1 t2))
	      ((list list)(car-eq (car t1)(car t2)))
	      ((symbol list)(car-eq t1 (car t2)))
	      ((list symbol)(car-eq (car t1)t2))))
	  (entry-point(t1 t2)
	    (matrix-case:matrix-case((category-of t1)(category-of t2))
	      ((:newtype	:newtype)					(car-eq t1 t2))
	      ((:newtype	(:adt :type-specifier :function :list))		nil)
	      ((:newtype	:wildcard)					t)
	      ((:adt		:adt)						(car-eq t1 t2))
	      ((:adt		(:newtype :type-specifier :function :list))	nil)
	      ((:adt		:wildcard)					t)
	      ((:type-specifier	:type-specifier)				(subtypep t1 t2))
	      ((:type-specifier	(:newtype :function :list))			nil)
	      ((:type-specifier	:adt)
	       (when(millet:type-specifier-p t2)
		 (subtypep t1 t2)))
	      ((:type-specifier	:wildcard)					T)
	      ((:wildcard	t)						T)
	      ((:function	(:newtype :adt :type-specifier :list))		nil)
	      ((:function	(:function :wildcard))				T)
	      ((:list		:list)			
	       (matrix-case:matrix-etypecase(t1 t2)
		 ((list list)(loop :for e1 :in (cdr t1)
				   :for e2 :in (cdr t2)
				   :always (subtype? e1 e2)))
		 (otherwise t)))
	      ((:list		(:newtype :adt :type-specifier :function))	nil)
	      ((:list		:wildcard)					T)
	      (otherwise (with-subtype-verbose(type-unify:unify t1 t2))))))
    (entry-point t1 t2)))

(defun category-of(thing)
  (cond
    ((newtypep thing):newtype)
    ((adt-p thing) :adt)
    ((or (find thing '(* T))
	 (type-unify:variablep thing))
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
       (t :type-specifier)))
    (t (cond
	 ((typep thing '(cons (eql function)t)) :function)
	 ((typep thing '(cons (eql cl:list)t)) :list)
	 ((typep thing '(cons (eql cons)t)) :list)
	 (t :unknown)))))
