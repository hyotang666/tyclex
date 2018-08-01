(defpackage :tyclex.dsl.definstance
  (:use :cl #:tyclex.objects.type-class #:tyclex.objects.instance)
  (:import-from #:tyclex.objects.cell #:make-cell)
  (:export
    #:definstance
    ))
(in-package :tyclex.dsl.definstance)

;;;; DEFISTANCE
(defmacro definstance((type-class &rest args)(&rest definition+))
  ;; trivial syntax checks.
  (assert(Find-type-class type-class))
  ;; Binds
  (destructuring-bind(types constraints)(split-sequence:split-sequence :constraints args)
    ;; trivial syntax checks.
    (assert (every #'symbolp types))
    (assert (and (every #'symbolp constraints)
		 (notany #'keywordp constraints)))
    ;; Binds
    (let*((instances(Type-class-instances type-class))
	  (defs(loop :for instance :in (set-difference instances (mapcar #'car definition+))
		     :collect (or (Instance-default instance)
				  (if(find instance instances)
				    (error "Default instance missing. ~S" instance)
				    (error "Unknown instance. ~S~%~S supports only ~S"
					   instance type-class instances)))
		     :into defaults
		     :finally (return (append definition+ defaults)))))
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
      ;; Body
      `(PROGN ,@(loop :for (name) :in defs
		      :for signature = (sublis (mapcan (lambda(var)
							 (mapcar (lambda(type)
								   (cons var type))
								 types))
						       (Type-class-vars type-class))
					       (Instance-lambda-list name))
		      :when (trestrul:find-leaf-if (complement #'tyclex.unifier:variablep)
						   signature)
		      :collect `(AUGMENT-TABLE ',name (MAKE-CELL :SIGNATURE ',signature
								 :INSTANCES ',defs
								 :TYPES ',types
								 :CONSTRAINTS ',constraints)))
	      ,@(loop :for type :in types
		      :collect `(PUSHNEW ',type (TYPE-CLASS-MEMBER  ',type-class)
					 :TEST #'EQUAL))
	      ',type-class))))

