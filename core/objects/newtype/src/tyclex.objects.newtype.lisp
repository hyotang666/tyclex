(in-package :cl-user)
(defpackage :tyclex.objects.newtype
  (:use :cl)
  (:shadow #:list)
  (:export #:list)
  (:export #:newtype-type-specifier-p #:add-newtype
    )
  )
(in-package :tyclex.objects.newtype)

(setf(symbol-function 'list)#'cl:list)

(deftype list(&optional a)
  (declare(ignore a))
  'cl:list)
(declaim(ftype (function (&rest t)
			 (values list &optional))
	       list))

(defvar *newtypes* (make-hash-table :test #'eq))

(defun add-newtype(name)
  (check-type name (and symbol (not (or keyword boolean))))
  (setf (gethash name *newtypes*)t))

(defun newtype-type-specifier-p(type-specifier)
  (labels((ensure-symbol(thing)
	    (if(atom thing)
	      thing
	      (ensure-symbol(car thing)))))
    (values(gethash (ensure-symbol type-specifier) *newtypes*))))
