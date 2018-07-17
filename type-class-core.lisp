(in-package #:vs-haskell)

;;;; TYPE-CLASS OBJECT
(defstruct(type-class(:constructor make-info)(:copier nil)
                     (:predicate nil)(:conc-name type-))
  (name (error "Name is required.") :type (or symbol list) :read-only t)
  (var (error "Var is required.") :type symbol :read-only t)
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

;;;; DEFISTANCE
(defmacro definstance((type-class type &optional constraint) definition)
  (let((defs(loop :for instance :in (set-difference (type-instances (find-type-class type-class))
						    (mapcar #'car definition))
		  :collect (or (instance-default instance)
			       (error "Default instance missing. ~S" instance))
		  :into defaults
		  :finally (return (append definition defaults)))))
    `(progn ,@(loop :for (name) :in defs
		    :for signature = (subst type
					    (type-var (find-type-class type-class))
					    (instance-lambda-list name))
		    :when (trestrul:find-leaf-if (complement #'type-unify:variablep)
						 signature)
		    :collect `(add-instance ',name ',signature ',defs ',type ',constraint))
	    ',type-class)))

;;;; ADD-INSTANCE
(defun add-instance(interface signature definition type constraint)
  (push(list signature definition type constraint)(instance-table interface)))

