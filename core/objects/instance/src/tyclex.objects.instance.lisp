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
    ))
(in-package :tyclex.objects.instance)

;;;; INSTANCE
(defstruct(instance (:predicate nil)
		    (:constructor make-type-class-instance)
		    (:copier nil))
  "Represents interface instance."
  (signature  	(error "SIGNATURE is required.")  	:type list :read-only t)
  (definitions	(error "DEFINITIONS is required.")  	:type list :read-only t)
  (types      	(error "TYPES is required.")      	:type list :read-only t)
  (constraints	(error "CONSTRAINTS is required.")	:type list :read-only t))

