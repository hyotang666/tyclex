(in-package :vs-haskell)

(defmacro define-newtype(name lambda-list &body body)
  `(PROGN (DEFTYPE ,name ,lambda-list ,@body)
	  (DEFMACRO,name(arg)
	    `(THE ,',name ,arg))))

(setf (symbol-function 'denew)#'third)
