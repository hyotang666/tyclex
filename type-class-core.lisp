(in-package #:vs-haskell)

;;;; TYPE-CLASS OBJECT
(defstruct(type-class(:constructor make-info)(:copier nil)
                     (:predicate nil)(:conc-name type-))
  (name (error "Name is required.") :type (or symbol list) :read-only t)
  (vars (error "Var is required.") :type list :read-only t)
  (instances nil :type list :read-only t)
  (direct-superclasses nil :type list)
  (direct-subclasses nil :type list))

;; helper
(defun find-type-class(name &optional (errorp T))
  (or (get name 'type-class)
      (when errorp
	(error "Type-class named ~S is not found." name))))

;;;; INSTANCE OBJECT
(defstruct(type-class-instance(:constructor instance-info)(:copier nil)
                              (:predicate nil))
  (lambda-list nil :type list :read-only t)
  (return-type nil :type (or list symbol):read-only t)
  (type-class (error "required") :type symbol :read-only t)
  (table nil :type list)
  (default nil :type list :read-only t))

(defun instance-type-class(interface)
  (find-type-class(type-class-instance-type-class(get interface 'instance))))

(defun instance-default(interface)
  (type-class-instance-default(get interface 'instance)))

(defun instance-lambda-list(interface)
  (type-class-instance-lambda-list(get interface 'instance)))

(defun instance-return-type(interface)
  (type-class-instance-return-type(get interface 'instance)))

(defun instance-p(interface)
  (get interface 'instance))

;;;; INSTANCE-TABLE
(defun instance-table(interface)
  (type-class-instance-table(get interface 'instance)))
(defun (setf instance-table)(new interface)
  (setf(type-class-instance-table(get interface 'instance))new))

;;;; CELL
(defstruct(cell (:predicate nil)
		(:copier nil)
		(:conc-name nil)
		(:constructor make-cell (signature instances types constraint)))
  (signature (error "SIGNATURE is required.") :type list :read-only t)
  (instances (error "Instances is required.") :type list :read-only t)
  (types     (error "Types is required.")     :type list :read-only t)
  (constraint(error "Constraint is required."):type t    :read-only t))

;;;; DEFISTANCE
(defmacro definstance((type-class &rest args) definition)
  (let*((|(types constraint)|(split-sequence:split-sequence :constraint args))
	(types (first |(types constraint)|))
	(constraint (caadr |(types constraint)|))
	(instances(type-instances (find-type-class type-class)))
	(defs(loop :for instance :in (set-difference instances (mapcar #'car definition))
		   :collect (or (instance-default instance)
				(if(find instance instances)
				  (error "Default instance missing. ~S" instance)
				  (error "Unknown instance. ~S~%~S supports only ~S"
					 instance type-class instances)))
		   :into defaults
		   :finally (return (append definition defaults)))))
    `(progn ,@(loop :for (name) :in defs
		    :for signature = (sublis (mapcan (lambda(var)
						       (mapcar (lambda(type)
								 (cons var type))
							       types))
						     (type-vars (find-type-class type-class)))
					     (instance-lambda-list name))
		    :when (trestrul:find-leaf-if (complement #'type-unify:variablep)
						 signature)
		    :collect `(add-instance ',name ',signature ',defs ',types ',constraint))
	    ,@(loop :for type :in types
		    :collect `(pushnew ',type (type-member (find-type-class ',type-class))
				       :test #'equal))
	    ',type-class)))

;;;; ADD-INSTANCE
(defun add-instance(interface signature definition types constraint)
  (push(make-cell signature definition types constraint)(instance-table interface)))

