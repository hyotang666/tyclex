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

(defunify((a (eql t))(b list))
  (extend-environment-with-t b env))

(defun extend-environment-with-t(pattern env)
  (trestrul:traverse (lambda(x)
		       (when(and (type-unify:variablep x)
				 (null(nth-value 1 (type-unify:find-variable-value x env))))
			 (type-unify:extend-environment x t env)))
		     pattern)
  env)

(defunify((a (eql 'function))(b list))
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

(defunify((a (eql 'cons))(b list))
  (if(type-unify:variablep(car b))
    (extend-environment-with-t (cdr b)
			       (type-unify:extend-environment(car b)a env))
    (if(eq 'list (car b))
      env
      (call-next-method))))

(defunify((a (eql 'null))(b list))
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

(defunify((a (eql 'list))(b list))
  (if(type-unify:variablep(car b))
    (type-unify:extend-environment (car b)a env)
    (if(eq a (car b)) ; list (list *)
      env
      (call-next-method))))

(defunify((a symbol)(b list))
  (if(type-unify:variablep a)
    (if(eq 'function (car b)) ; ?B (FUNCTION(?A)?B)
      (type-unify:extend-environment a b env)
      (call-next-method))
    (if(newtypep a)
      (if(type-unify:variablep (car b))
	(type-unify:extend-environment (car b) a env)
	(call-next-method))
      (call-next-method))))

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
  (if(millet:type-specifier-p t1)
    (if(millet:type-specifier-p t2)
      (if(subtypep t1 t2)
	(let((expanded?1 (nth-value 1(millet:type-expand t1)))
	     (expanded?2 (nth-value 1(millet:type-expand t2))))
	  (if expanded?1
	    (if expanded?2
	      (eq (alexandria:ensure-car t1)
		  (alexandria:ensure-car t2))
	      T)
	    (if expanded?2
	      nil
	      T)))
	nil)
      (if(adt-p t2)
	(eq (alexandria:ensure-car t1)(alexandria:ensure-car t2))
	(or (eql t t1)
	    (with-subtype-verbose(type-unify:unify t1 (patternize t2))))))
    (if(millet:type-specifier-p t2)
      (if(adt-p t1)
	(eq (alexandria:ensure-car t1)(alexandria:ensure-car t2))
	(or (eql t t2)
	    (with-subtype-verbose(type-unify:unify (patternize t1)t2))))
      (if(adt-p t1)
	(if(adt-p t2)
	  (eq (alexandria:ensure-car t1)(alexandria:ensure-car t2)) ; TODO Is eq enough?
	  nil)
	(if(adt-p t2)
	  nil
	  (with-subtype-verbose(type-unify:unify (patternize t1)(patternize t2))))))))
