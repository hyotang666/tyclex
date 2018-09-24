(in-package :cl-user)
(defpackage :tyclex.dsl.definstance
  (:use :cl)

  (:import-from :tyclex.objects.type-class
		#:find-type-class
		#:type-class-interfaces #:type-class-vars #:type-class-member)
  (:import-from :tyclex.objects.interface
		#:interface-default #:interface-lambda-list #:augment-instances)
  (:import-from :tyclex.objects.instance
		#:make-type-class-instance)

  (:export
    #:definstance
    ))
(in-package :tyclex.dsl.definstance)

;;;; Conditions
(define-condition missing-default-instance(tyclex.conditions:missing)())

(define-condition unknown-interface(tyclex.conditions:tyclex-error simple-condition)()
  (:report (lambda(condition stream)
	     (apply #'format stream
		    (simple-condition-format-control condition)
		    (simple-condition-format-arguments condition)))))

;;;; DEFISTANCE
(defmacro definstance((type-class &rest args)definition+)
  ;; trivial syntax checks.
  (assert(Find-type-class type-class))
  ;; Binds
  (multiple-value-bind(types constraints)(parse-args args type-class)
    (let*((all-interfaces(Type-class-interfaces type-class))
	  (unimplemented-interfaces(set-difference all-interfaces (mapcar #'car definition+)))
	  (defs(loop :for interface :in unimplemented-interfaces
		     :collect (default-instance interface all-interfaces)
		     :into defaults
		     :finally (return (append definition+ defaults))))
	  (type-class-vars(Type-class-vars type-class)))
      ;; Body
      `(PROGN ,@(loop :for (name) :in defs
		      :for lambda-list = (Interface-lambda-list name)
		      :for signature = (make-signature name type-class-vars types)
		      :when (trestrul:find-leaf-if (lambda(leaf)
						     (find leaf type-class-vars :test #'eq))
						   lambda-list)
		      :collect `(AUGMENT-INSTANCES ',name (MAKE-TYPE-CLASS-INSTANCE
							    :SIGNATURE ',signature
							    :DEFINITIONS ',defs
							    :TYPES ',types
							    :CONSTRAINTS ',constraints)))
	      ,@(loop :for type :in types
		      :collect `(PUSHNEW ',type (TYPE-CLASS-MEMBER  ',type-class)
					 :TEST #'EQUAL))
	      ',type-class))))

(defun parse-args(args type-class)
  (destructuring-bind(types . constraints)(split-sequence:split-sequence :constraints args)
    ;; canonicalize
    (setf constraints (car constraints))
    ;; trivial syntax checks.
    (assert (every (lambda(x)
		     (typep x '(or symbol list)))
		   types))
    (loop :for (type var) :in constraints :do
	  (assert (or (eq type type-class)
		      (Find-type-class type nil)))
	  (assert (trestrul:find-leaf var types)))
    (when constraints
      (setf types ; as canonicalize.
	    (trestrul:asubst-if
	      (lambda(var)
		`(satisfies ,(intern(format nil "~A-P"
					    (some (lambda(constraint)
						    (when(eq var (second constraint))
						      (first constraint)))
						  constraints)))))
	      (lambda(elt)
		(find elt constraints
		      :key #'second :test #'eq))
	      types)))
    (values types constraints)))

(defun default-instance(interface all-interfaces)
  (or (Interface-default interface)
      (if(find interface all-interfaces)
	(error 'missing-default-instance :name interface)
	(error 'unknown-interface
	       :format-control "Unknown interface. ~S~%Supported are ~S"
	       :format-arguments (list interface all-interfaces)))))

(defun make-signature(name type-class-vars types)
  (sublis (mapcan (lambda(var)
		    (mapcar (lambda(type)
			      (cons var type))
			    types))
		  type-class-vars)
	  (Interface-lambda-list name)))
