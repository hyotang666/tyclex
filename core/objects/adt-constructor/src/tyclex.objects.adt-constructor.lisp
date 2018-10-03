(in-package :cl-user)
(defpackage :tyclex.objects.adt-constructor
  (:use :cl)
  (:export
    ;; type-name
    #:adt-constructor
    ;; constructor
    #:make-adt-constructor
    ;; readers
    #:adt-constructor-type-of #:adt-constructor-arg-types
    ;; helpers
    #:get-adt-constructor #:add-adt-constructor #:remove-adt-constructor
    #:adt-constructor-boundp #:adt-constructor-form-p #:adt-constructor-makunbound
    ;; conditions
    #:missing-adt-constructor
    ;; Negligible utilities
    #:first-atom
    )
  )
(in-package :tyclex.objects.adt-constructor)

(define-condition missing-adt-constructor(tyclex.conditions:missing)())

;;;; object ADT-CONSTRUCTOR
;;;; constructor MAKE-ADT-CONSTRUCTOR
;;;; slot-readers ADT-CONSTRUCOTR-TYPE-OF, ADT-CONSTRUCTOR-ARG-TYPES
(defstruct(adt-constructor (:copier nil)(:predicate nil))
  (type-of	(error 'tyclex.conditions:slot-uninitialized :name 'type-of)
		:type (or symbol list)	:read-only t)
  (arg-types	(error 'tyclex.conditions:slot-uninitialized :name 'arg-types)
		:type list		:read-only t))

; underlying database.
(defvar *adt-constructors* (make-hash-table :test #'eq))

;;;; Trivial helpers.
(defun get-adt-constructor(form &optional (errorp t))
  (or (gethash (first-atom form) *adt-constructors*)
      (when errorp
	(error 'missing-adt-constructor :name form))))

(defun first-atom(thing)
  (if(atom thing)
    thing
    (first-atom(car thing))))

(defun (setf get-adt-constructor)(new-value name &optional (errorp t))
  (declare(ignore errorp))
  (check-type new-value adt-constructor)
  (check-type name (and symbol (not boolean)))
  (when (gethash name *adt-constructors*)
    (warn 'tyclex.conditions:redefinition-warning :name name))
  (setf (gethash name *adt-constructors*)
	new-value))

(defun add-adt-constructor(name &rest args)
  (setf (get-adt-constructor name)
	(apply #'make-adt-constructor args)))

(defun remove-adt-constructor(name)
  (remhash name *adt-constructors*))

(defun adt-constructor-makunbound(name)
  (check-type name symbol)
  (let((constructor(get-adt-constructor name nil)))
    (when constructor
      (cond
	((keywordp name))
	((null(adt-constructor-arg-types constructor))
	 (makunbound name))
	(t (fmakunbound name)))
      (remove-adt-constructor name))))

(defun adt-constructor-boundp(symbol)
  (check-type symbol symbol)
  (get-adt-constructor symbol nil))

(defun adt-constructor-form-p(thing)
  (get-adt-constructor thing nil))
