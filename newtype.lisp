(in-package :vs-haskell)

(defmacro define-newtype(name lambda-list &body body)
  `(PROGN (SETF (GET ',name 'newtype) T)
	  (DEFTYPE ,name ,lambda-list ,@body)
	  (DEFMACRO,name(arg)
	    `(THE ,',name ,arg))))

(setf (symbol-function 'denew)#'third)

(defun newtypep(type-specifier)
  (labels((ensure-symbol(thing)
	    (if(atom thing)
	      thing
	      (ensure-symbol(car thing)))))
    (get (ensure-symbol type-specifier) 'newtype)))
