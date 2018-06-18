(in-package :vs-haskell)

(defmacro define-newtype(name lambda-list &body body)
  `(PROGN (DEFTYPE ,name ,lambda-list ,@body)
	  (DEFMACRO,name(arg)
	    `(THE (,',name ,',@(mapcar (constantly '*)
				       (lambda-list:vars<=lambda-list lambda-list)))
		  ,arg))))

(setf (symbol-function 'denew)#'third)
