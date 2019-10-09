(in-package :cl-user)
(defpackage :tyclex.objects.newtype
  (:use :cl)
  (:shadow #:list)
  (:export #:list)
  (:export #:newtype-type-specifier-p #:add-newtype
    )
  )
(in-package :tyclex.objects.newtype)

;;;; LIST
;;; as function.
(declaim(ftype (function (&rest t)
			 (values list &optional))
	       list))

(setf(symbol-function 'list)#'cl:list)

;;; as type.
(deftype list(&optional a)
  (declare(ignore a))
  'cl:list)

(defvar *newtypes* (make-hash-table :test #'eq))

;;;; ADD-NEWTYPE
(defun add-newtype(name)
  (check-type name (and symbol (not (or keyword boolean))))
  (setf (gethash name *newtypes*)t))

;;;; NEWTYPE-TYPE-SPECIFIER-P
(defun newtype-type-specifier-p(type-specifier)
  (labels((ensure-symbol(thing)
	    (if(atom thing)
	      thing
	      (ensure-symbol(car thing)))))
    (values(gethash (ensure-symbol type-specifier) *newtypes*))))
