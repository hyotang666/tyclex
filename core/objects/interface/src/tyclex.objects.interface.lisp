(in-package :cl-user)
(defpackage :tyclex.objects.interface
  (:use :cl)
  (:export
    ;; as type name
    #:interface
    ;; as constructor
    #:make-interface
    ;; predicate
    #:interfacep
    ;; readers
    #:interface-lambda-list #:interface-return-type #:interface-type-class #:interface-default
    ;; accessor
    #:interface-instances
    ;; helpers
    #:find-interface #:add-interface #:remove-interface #:augment-instances
    #:interface-boundp #:interface-form-p
    )
  )
(in-package :tyclex.objects.interface)

;;;; Condition
(define-condition missing-interface(tyclex.conditions:missing)())

;;;; INTERFACE OBJECT
(defstruct(interface (:copier nil)
		     (:predicate interfacep)
		     (:conc-name nil))
  "Represents type class interface"
  (lambda-list	nil			:type list		:read-only t)
  (return-type	nil			:type (or list symbol)	:read-only t)
  (type-class	(error 'tyclex.conditions:slot-uninitialized :name 'type-class)
					:type symbol		:read-only t)
  (instances	nil			:type list			    )
  (default	nil			:type list		:read-only t))

(defvar *interfaces* (make-hash-table :test #'eq))

;; helper
(defun find-interface(interface &optional (errorp T))
  (if(interfacep interface)
    interface
    (or (gethash interface *interfaces*)
	(when errorp
	  (error 'missing-interface :name interface)))))

(defun add-interface(interface &rest args)
  (check-type interface (and symbol (not (or keyword boolean))))
  (setf (gethash interface *interfaces*)
	(apply #'make-interface args)))

(defun remove-interface(interface)
  (remhash interface *interfaces*))

(defun augment-instances(interface cell)
  (push cell (interface-instances interface)))

(defun interface-boundp(symbol)
  (check-type symbol (and symbol (not (or keyword boolean))))
  (find-interface symbol nil))

(defun interface-form-p(thing)
  (and (listp thing)
       (symbolp (car thing))
       (find-interface (car thing)nil)))

;;;; EASY READERS
(defun interface-lambda-list(interface)
  (lambda-list(find-interface interface)))

(defun interface-return-type(interface)
  (return-type(find-interface interface)))

(defun interface-type-class(interface)
  (type-class(find-interface interface)))

(defun interface-default(interface)
  (default(find-interface interface)))

;;;; INSTANCE-TABLE
(defun interface-instances(interface)
  (instances(find-interface interface)))
(defun (setf interface-instances)(new interface)
  (setf(instances(find-interface interface))new))

