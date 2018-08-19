(in-package :cl-user)
(eval-when(:compile-toplevel :load-toplevel :execute)
  (unless(find-package :tyclex.unifier)
    (rename-package :unify :tyclex.unifier)
    (let((asdf::*asdf-session* nil))
      (declare(ignorable asdf::*asdf-session*)) ; for CCL.
      (asdf:load-system :cl-unification :force t))))

(in-package :tyclex.unifier)
(handler-bind((package-error(lambda(c) ; for ECL.
			      (declare(ignore c))
			      (when(find-restart 'continue)
				(invoke-restart 'continue)))))
  (export '(envar patternize enwild dewild ignore-unification-failure find-value-variable
		  replace-bind)))

(defun envar(thing)
  (trestrul:asubst-if (lambda(x)(intern(format nil "?~A"x)))
		      (lambda(x)(and (typep x '(and symbol (not (or keyword boolean))))
				     (not (char= #\? (char (string x)
							   0)))))
		      thing))

(defun patternize(thing)
  (if(millet:type-specifier-p thing)
    thing
    (if(listp thing)
      (trestrul:mapleaf #'patternize thing)
      (envar thing))))

(defun enwild (type-spec)
  (sublis'((* . _)(T . _))type-spec))

(defun dewild (pattern)
  (subst T '_ pattern))

(defmacro ignore-unification-failure(form)
  `(handler-case,form
     (unification-failure()NIL)))

(defun find-value-variable(value env &key (test #'eql))
  (declare (type environment env))
  (labels ((find-value-var(frames)
	     (unless(endp frames)
	       (or (let((binding(find value (frame-bindings (car frames)) :test test :key #'binding-value)))
		     (when binding
		       (binding-variable binding)))
		   (find-value-var (cdr frames))))))
    (find-value-var (environment-frames env))))

(defun replace-bind(variable value env)
  (setf (binding-value (find-variable-binding variable env))
	value)
  env)
