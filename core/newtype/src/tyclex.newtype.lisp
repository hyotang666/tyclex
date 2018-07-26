(defpackage :tyclex.newtype
  (:use :cl)
  (:export ; main api
    #:define-newtype
    #:denew
    )
  (:export ; for hackers
    #:newtypep
    ))
(in-package :tyclex.newtype)

(defvar *newtypes* (make-hash-table :test #'eq))

(defmacro define-newtype(name lambda-list &body body)
  `(PROGN (SETF (GETHASH ',name *NEWTYPES*) T)
	  (DEFTYPE ,name ,lambda-list ,@body)
	  (DEFMACRO,name(arg)
	    `(THE ,',name ,arg))))

(setf (symbol-function 'denew)#'third)

(defun newtypep(type-specifier)
  (labels((ensure-symbol(thing)
	    (if(atom thing)
	      thing
	      (ensure-symbol(car thing)))))
    (gethash (ensure-symbol type-specifier) *newtypes*)))
