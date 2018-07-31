(defpackage :tyclex.objects.adt
  (:use :cl)
  (:export
    ;; type-name
    #:adt
    ;; constructor
    #:make-adt
    ;; readers
    #:adt-constructors #:adt-lambda-list
    ;; helpers
    #:add-adt #:remove-adt #:find-adt #:adt-makunbound
    )
  (:export
    ;; type-name
    #:adt-constructor
    ;; constructor
    #:make-adt-constructor
    ;; readers
    #:adt-constructor-type-of #:adt-constructor-arg-types
    ;; helpers
    #:add-adt-constructor #:remove-adt-constructor #:find-adt-constructor
    #:adt-constructor-boundp
    )
  )
(in-package :tyclex.objects.adt)

(defstruct (adt (:copier nil)(:predicate nil))
  (constructors (error "required")	:type list	:read-only t)
  (lambda-list	(error "required")	:type list	:read-only t))

(defvar *adts* (make-hash-table :test #'eq))

(defun add-adt(name &rest args)
  (setf (gethash name *adts*)
	(apply #'make-adt args)))

(defun remove-adt(name)
  (remhash name *adts*))

(defun find-adt(name &optional(errorp t))
  (or (gethash name *adts*)
      (when errorp
	(error "Missing ADT named ~S" name))))

(defun adt-makunbound(name)
  (let((constructors(adt-constructors(or (find-adt name nil)
					 (return-from adt-makunbound (makunbound name))))))
    (remove-adt name)
    (mapc (lambda(symbol)
	    (remove-adt-constructor symbol)
	    (fmakunbound symbol))
	  constructors)
    (makunbound name)))

;;;; ADT-CONSTRUCTOR data structure
(defstruct(adt-constructor (:copier nil)(:predicate nil))
  (type-of	(error "required") :type (or symbol list)	:read-only t)
  (arg-types	(error "required") :type (or symbol list)	:read-only t))

(defvar *adt-constructors* (make-hash-table :test #'eq))

(defun add-adt-constructor(name &rest args)
  (setf (gethash name *adt-constructors*)
	(apply #'make-adt-constructor args)))

(defun remove-adt-constructor(name)
  (remhash name *adt-constructors*))

(defun find-adt-constructor(name &optional (errorp t))
  (or (gethash name *adt-constructors*)
      (when errorp
	(error "Missing ADT-CONSTRUCTOR named ~S" name))))

(defun adt-constructor-boundp(symbol)
  (check-type symbol symbol)
  (find-adt-constructor symbol))
