(defpackage :tyclex.newtype
  (:use :cl)
  (:export ; main api
    #:define-newtype
    #:denew
    )
  (:export ; for hackers
    #:newtype-type-specifier-p
    ))
(in-package :tyclex.newtype)

(defvar *newtypes* (make-hash-table :test #'eq))

(defmacro define-newtype(name lambda-list &body body)
  ;; trivial-syntax-check.
  (assert(typep name '(and symbol (not (or keyword boolean)))))
  (assert(listp lambda-list))
  ;; body
  `(PROGN (SETF (GETHASH ',name *NEWTYPES*) T)
	  (DEFTYPE ,name ,lambda-list ,@body)
	  (DEFMACRO,name(arg)
	    `(THE ,',name ,arg))))

(setf (symbol-function 'denew)#'third)

(defun newtypep-type-specifier-p(type-specifier)
  (labels((ensure-symbol(thing)
	    (if(atom thing)
	      thing
	      (ensure-symbol(car thing)))))
    (gethash (ensure-symbol type-specifier) *newtypes*)))
