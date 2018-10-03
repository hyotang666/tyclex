(in-package :cl-user)
(defpackage :tyclex.objects.instance
  (:use :cl)
  (:export
    ;; type name
    #:instance
    ;; Constructor
    #:make-type-class-instance
    ;; Readers
    #:instance-signature #:instance-definitions #:instance-types #:instance-constraints
    ;; Predicate
    #:instance=
    ))
(in-package :tyclex.objects.instance)

;;;; INSTANCE
(defstruct(instance (:predicate nil)
		    (:constructor make-type-class-instance)
		    (:copier nil))
  "Represents interface instance."
  (signature  	(error 'tyclex.conditions:slot-uninitialized :name 'signature)
		:type list :read-only t)
  (definitions	(error 'tyclex.conditions:slot-uninitialized :name 'definitions)
		:type list :read-only t)
  (types      	(error 'tyclex.conditions:slot-uninitialized :name 'types)
		:type list :read-only t)
  (constraints	(error 'tyclex.conditions:slot-uninitialized :name 'constraints)
		:type list :read-only t))

(defun instance=(x y)
  (equal (instance-signature x)
	 (instance-signature y)))
