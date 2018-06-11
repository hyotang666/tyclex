(in-package :vs-haskell)

#|
(defmacro def((name &rest pattern) consequent)
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (DEFMACRO ,name (&whole whole)
       `(force ',whole))
     (SETF (GET ',name 'HASKELL)
	   (APPEND (GET ',name 'HASKELL)
		   (LIST (CONS ',pattern ',consequent))))
     ',name))

(def(double-me x)
  (+ x x))

(defun haskell-p(form)
  (and (listp form)
       (symbol (car form))
       (get (car form)'haskell)))

(defmacro haskell (form)
  (let((*macroexpand-hook*
	 (lambda(macro-function form environment)
	   (if(haskell-p form)
	     `(force ',form)
	     (funcall macro-function form environment)))))
    (expander:expand form)))

(defun force (form)
  (cond
    ((haskell-p form)
     (let((clause(assoc form (get (car form)'haskell):test #'matcher)))
       (assert clause)
       (apply (compile nil `(lambda,(parse-lambda-list (car clause))
			      ,(cdr clause)))
	      (mapcar #'force (cdr form)))))
    ((and (listp form)
	  (symbolp (car form))
	  (special-operator-p (car form)))
     #|TODO|#)))

|#
