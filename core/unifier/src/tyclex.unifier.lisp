(eval-when(:compile-toplevel :load-toplevel :execute)
  (unless(find-package :tyclex.unifier)
    (rename-package :unify :tyclex.unifier)
    (let((asdf::*asdf-session* nil))
      (asdf:load-system :cl-unification :force t))))

(in-package :tyclex.unifier)
(export '(envar patternize enwild dewild ignore-unification-failure))

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
