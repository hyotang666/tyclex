(in-package :tyclex.type-class-core)

;;;; INSTANCE OBJECT
(defstruct(instance (:copier nil)
		    (:predicate instancep)
		    (:constructor make-type-class-instance)
		    (:conc-name nil))
  "Represents type class instance"
  (lambda-list	nil			:type list		:read-only t)
  (return-type	nil			:type (or list symbol)	:read-only t)
  (type-class	(error "required")	:type symbol		:read-only t)
  (table	nil			:type list			    )
  (default	nil			:type list		:read-only t))

;; TYPE
(deftype interface()
  '(and symbol (satisfies instancep)))

(defvar *instances* (make-hash-table :test #'eq))

;; helper
(defun find-instance(interface &optional (errorp T))
  (if(typep interface 'instance)
    interface
    (or (gethash interface *instances*)
	(when errorp
	  (error "Missing instance named ~S" interface)))))

(defun add-instance(interface &rest args)
  (check-type interface (and symbol (not (or keyword boolean))))
  (setf (gethash interface *instances*)
	(apply #'make-type-class-instance args)))

(defun remove-instance(interface)
  (remhash interface *instances*))

(defun augment-table(interface cell)
  (push cell (instance-table interface)))

;;;; EASY READERS
(defun instance-lambda-list(interface)
  (lambda-list(find-instance interface)))

(defun instance-return-type(interface)
  (return-type(find-instance interface)))

(defun instance-type-class(interface)
  (type-class(find-instance interface)))

(defun instance-default(interface)
  (default(find-instance interface)))

;;;; INSTANCE-TABLE
(defun instance-table(interface)
  (table(find-instance interface)))
(defun (setf instance-table)(new interface)
  (setf(table(find-instance interface))new))
