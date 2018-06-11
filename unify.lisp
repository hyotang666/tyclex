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
  (subst '* '_ pattern))

(defmethod type-unify:unify :around ((a symbol)(b symbol)
				     &optional (env (type-unify:make-empty-environment))
				     &key &allow-other-keys)
  (cond ((type-unify:variable-any-p a)env)
	((type-unify:variablep a)(type-unify::var-unify a b env))
	((type-unify:variable-any-p b) env)
	((type-unify:variablep b)(type-unify::var-unify b a env))
	((eq a b)env)
	((subtype? b a)env)
	(t (error 'type-unify::unification-failure
		  :format-control "Cannot unify two different symbols: ~S ~S"
		  :format-arguments (list a b)))))

(defmethod type-unify:unify ((a (eql t))(b list)
			     &optional (env (type-unify:make-empty-environment))
			     &key &allow-other-keys)
  (extend-environment-with-t b env))

(defun extend-environment-with-t(pattern env)
  (trestrul:traverse (lambda(x)
		       (when(and (type-unify:variablep x)
				 (null(nth-value 1 (type-unify:find-variable-value x env))))
			 (type-unify:extend-environment x t env)))
		     pattern)
  env)

(defmethod type-unify:unify ((b list)(a (eql t))
			     &optional (env (type-unify:make-empty-environment))
			     &key &allow-other-keys)
  (extend-environment-with-t b env))

(defmethod type-unify:unify ((a (eql 'function))(b list)
			     &optional (env (type-unify:make-empty-environment))
			     &key &allow-other-keys)
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

(defmethod type-unify:unify ((b list)(a (eql 'function))
			     &optional (env (type-unify:make-empty-environment))
			     &key &allow-other-keys)
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

(defmethod type-unify:unify ((a (eql 'cons))(b list)
			     &optional(env(type-unify:make-empty-environment))
			     &key &allow-other-keys)
  (if(type-unify:variablep(car b))
    (extend-environment-with-t (cdr b)
			       (type-unify:extend-environment(car b)a env))
    (call-next-method)))

(defmethod type-unify:unify ((b list)(a (eql 'cons))
			     &optional(env(type-unify:make-empty-environment))
			     &key &allow-other-keys)
  (if(type-unify:variablep(car b))
    (extend-environment-with-t (cdr b)
			       (type-unify:extend-environment(car b)a env))
    (call-next-method)))

(defmethod type-unify:unify ((a (eql 'null))(b list)
			     &optional(env(type-unify:make-empty-environment))
			     &key &allow-other-keys)
  (if(not(type-unify:variablep(car b)))
    (call-next-method)
    (if(cddr b)
      (call-next-method)
      (extend-environment-with-t (cdr b)
				 (type-unify:extend-environment(car b)'list env)))))

(defmethod type-unify:unify ((b list)(a (eql 'null))
			     &optional(env(type-unify:make-empty-environment))
			     &key &allow-other-keys)
  (if(not(type-unify:variablep(car b)))
    (call-next-method)
    (if(cddr b)
      (call-next-method)
      (extend-environment-with-t (cdr b)
				 (type-unify:extend-environment(car b)'list env)))))

(defmethod type-unify:unify :around ((a list)(b list)
				     &optional(env(type-unify:make-empty-environment))
				     &key &allow-other-keys)
  (trivia:match*((car a)(car b))
    (((satisfies type-unify:variablep)(eq 'function))
     (setf b (ensure-value b))
     (type-unify:unify (cdr a)(cddr b)
		       (type-unify:extend-environment (car a) (car b) env)))
    (((eq 'function)(satisfies type-unify:variablep))
     (setf a (ensure-value a))
     (type-unify:unify (cddr a)(cdr b)
		       (type-unify:extend-environment (car a)(car b)env)))
    ((_ _)(call-next-method))))

(defun ensure-value(ftype-spec)
  (trestrul:asubst-if #'second
		      (lambda(elt)
			(typep elt '(cons (eql values) *)))
		      ftype-spec))

(defun subtype?(t1 t2)
  (if(millet:type-specifier-p t1)
    (if(millet:type-specifier-p t2)
      (subtypep t1 t2)
      (or (eql t t1)
	  (type-unify:unify t1 (patternize t2))))
    (if(millet:type-specifier-p t2)
      (unless(eql t t2)
	(type-unify:unify (patternize t1)t2))
      (type-unify:unify (patternize t1)(patternize t2)))))

