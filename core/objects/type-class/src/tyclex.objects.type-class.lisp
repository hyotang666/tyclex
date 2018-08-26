(in-package :cl-user)
(defpackage :tyclex.objects.type-class
  (:use :cl)
  (:export
    ;; as type name
    #:type-class
    ;; as constructor
    #:make-type-class
    ;; predicate
    #:type-class-p
    ;; readers
    #:type-class-name #:type-class-vars #:type-class-interfaces
    ;; accessors
    #:type-class-member #:type-class-constraints
    ;; helpers
    #:find-type-class #:add-type-class #:remove-type-class
    )
  )
(in-package :tyclex.objects.type-class)

;;;; TYPE-CLASS OBJECT
(defstruct(type-class (:copier nil)
		      (:conc-name type-))
  (name 	(error "Name is required.")	:type (or symbol list)	:read-only t)
  (vars		(error "Var is required.")	:type list		:read-only t)
  (interfaces	nil				:type list		:read-only t)
  (member	nil				:type list)
  (constraints	nil				:type list))

(defvar *type-classes* (make-hash-table :test #'eq))

;; helper
(defun find-type-class(arg &optional (errorp T))
  (if(type-class-p arg)
    arg
    (or (gethash arg *type-classes*)
	(when errorp
	  (error "Missing type-class named ~S." arg)))))

(defun add-type-class(name &rest args)
  (check-type name (and symbol (not (or keyword boolean))))
  (setf (gethash name *type-classes*)
	(apply #'make-type-class :name name args)))

(defun remove-type-class(name)
  (remhash name *type-classes*))

;; readers and accessors.
(defun type-class-name(arg)
  (type-name(find-type-class arg)))

(defun type-class-vars(arg)
  (type-vars(find-type-class arg)))

(defun type-class-interfaces(arg)
  (type-interfaces(find-type-class arg)))

(defun type-class-member(arg)
  (type-member(find-type-class arg)))
(defun (setf type-class-member)(new arg)
  (setf(type-member(find-type-class arg))new))

(defun type-class-constraints(arg)
  (type-constraints(find-type-class arg)))
(defun (setf type-class-constraints)(new arg)
  (setf(type-constraints(find-type-class arg))new))
