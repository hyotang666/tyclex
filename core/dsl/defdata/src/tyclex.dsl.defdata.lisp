(in-package :cl-user)
(defpackage :tyclex.dsl.defdata
  (:use :cl)

  (:import-from :tyclex.objects.type-class
		#:find-type-class #:type-class-constraints #:type-class-interfaces)
  (:import-from :tyclex.objects.adt
		#:adt-value-p
		#:add-adt #:get-adt #:adt-lambda-list #:adt-constructors)
  (:import-from :tyclex.objects.adt-constructor
		#:add-adt-constructor #:get-adt-constructor #:adt-constructor-arg-types #:adt-constructor-type-of)
  (:import-from :tyclex.objects.interface
		#:interface-default)
  (:import-from :tyclex.objects.io-action
		#:io-action #:io-type)
  (:import-from :tyclex.dsl.definstance
		#:definstance)

  (:export
    ;; Main API
    #:defdata
    ;; Helpers
    #:data-order
    ))
(in-package :tyclex.dsl.defdata)

;;;; DEFDATA
(defmacro defdata(name&options lambda-list &rest constructor*)
  ;; binding
  (destructuring-bind(name &key deriving)(alexandria:ensure-list name&options)
    ;; trivial syntax check.
    (check-type name valid-name)
    (assert(every #'Find-type-class deriving))
    (assert(every (lambda(x)(typep x 'valid-name)) lambda-list))
    (assert(notany (lambda(x)(find x lambda-list-keywords :test #'eq))lambda-list))
    (dolist(constructor constructor*)
      (assert(symbolp(alexandria:ensure-car constructor))))
    ;; body
    `(eval-when(:compile-toplevel :load-toplevel :execute)
       ,(<deftype> name lambda-list constructor*)
       ,(<add-adt> name lambda-list constructor*)
       ,@(mapcan (lambda(constructor)
		   (<constructors> name lambda-list constructor))
		 constructor*)
       ,@(loop :for c :in constructor*
	       :collect (<add-adt-constructor> c lambda-list name))
       ,@(mapcan #'<pattern-matcher> constructor*)
       ,@(loop :for tc :in deriving
	       :append (<derivings> tc name))
       ',name)))

;;; VALID-NAME
(deftype valid-name()
  '(and symbol (not (or keyword boolean))))

;;; <deftype>
(defun <deftype>(name lambda-list constructor*)
  (labels((OPTIONAL-LAMBDA-LIST(list)
	    (if(cdr list)
	      (list* (car list) '&optional (cdr list))
	      list))
	  (TYPE-NAME(args constructor)
	    (cond
	      ((symbolp constructor)`'(eql ,constructor))
	      (args (comma-type-specifier args constructor))
	      ((list-constructor-p constructor)
	       `',(cons-type-specifier `((eql ,(car constructor))
					 ,@(cdr constructor))))
	      (t `',(cons-type-specifier `((eql ,(car constructor))
					   ,@(loop :for clause :in (cdr constructor)
						   :collect (typecase clause
							      (SYMBOL T)
							      (LIST (getf clause :type T))))))))))
    `(DEFTYPE,name,(OPTIONAL-LAMBDA-LIST lambda-list)
       (LIST 'OR ,@(mapcar (lambda(constructor)
			     (TYPE-NAME lambda-list constructor))
			   constructor*)))))

(defun comma-type-specifier(args constructor)
  ``(cons (eql ,',(car constructor))
	 ,,(labels((rec(types)
		    (if(endp types)
		      ''null
		      `(list 'cons ,(let((arg(car types)))
				      (cond
					((find arg args :test #'eq)arg)
					((millet:type-specifier-p arg)`',arg)
					(t
					  `(list ',(car arg) ,@(cdr arg)))))
			     ,(rec (cdr types))))))
	    (rec (cdr constructor)))))

(defun list-constructor-p(constructor)
  (every #'millet:type-specifier-p (cdr constructor)))

(defun cons-type-specifier(types)
  (if(atom types)
    (if(null types)
      'null
      types)
    `(CONS ,(car types),(cons-type-specifier (cdr types)))))

;;; <add-adt>
(defun <add-adt> (name lambda-list constructor*)
  `(ADD-ADT ',name
	    :CONSTRUCTORS ',(mapcar #'alexandria:ensure-car constructor*)
	    :LAMBDA-LIST ',(mapcar (lambda(elt)
				     (if(millet:type-specifier-p elt)
				       elt
				       (tyclex.unifier:envar elt)))
				   lambda-list)))

;;; <constructors>
(defun <constructors>(name args constructor)
  (labels((ARG-TYPES(lambda-list arg-types &optional acc)
	    (if(endp arg-types)
	      (nreverse acc)
	      (if(find(car arg-types)lambda-list :test #'eq)
		(ARG-TYPES lambda-list (cdr arg-types)(push t acc))
		(ARG-TYPES lambda-list (cdr arg-types)(push (car arg-types)acc))))))
    (cond
      ((symbolp constructor)
       (unless(keywordp constructor)
	 `((ALEXANDRIA:DEFINE-CONSTANT ,constructor ',constructor))))
      ((or args (list-constructor-p constructor))
       (let((lambda-list(alexandria:make-gensym-list(length(cdr constructor)))))
	 `(,@(if args
	       `((DECLAIM(FTYPE (FUNCTION ,(ARG-TYPES args (cdr constructor))
					  ,(constructor-return-type name))
				,(constructor-name constructor))))
	       `((DECLAIM(FTYPE (FUNCTION,(cdr constructor),name)
				,(constructor-name constructor)))))
	    (DEFUN ,(constructor-name constructor),lambda-list
	      (LIST ',(car constructor),@lambda-list)))))
      (t `((DEFSTRUCT(,(car constructor)
		       :NAMED (:TYPE LIST) (:CONC-NAME NIL)
		       (:COPIER NIL) (:PREDICATE NIL)
		       (:CONSTRUCTOR ,(constructor-name constructor)))
	     ,@(cdr constructor)))))))

(defun constructor-return-type(name)
  (list name '*))

(defun constructor-name(constructor)
  (intern(symbol-name(car constructor))))

;;; <add-adt-constructor>
(defun <add-adt-constructor>(constructor lambda-list name)
  `(ADD-ADT-CONSTRUCTOR ',(alexandria:ensure-car constructor)
			:TYPE-OF ',(if lambda-list
				     (constructor-return-type name)
				     name)
			:ARG-TYPES ',(arg-types constructor lambda-list)))

(defun arg-types(constructor args)
  (cond
    ((symbolp constructor))
    ((or args (list-constructor-p constructor))
     (sublis (mapcar (lambda(elt)
		       (cons elt (tyclex.unifier:envar elt)))
		     args)
	     (cdr constructor)))
    (t (mapcar (lambda(slot)
		 (if(symbolp slot)
		   t
		   (getf slot :type t)))
	       (cdr constructor)))))

;;; <pattern-matcher>
(defun <pattern-matcher>(constructor)
  (when(listp constructor)
    `((TRIVIA:DEFPATTERN,(constructor-name constructor)(&REST ARGS)
	`(LIST (EQ ',',(car constructor)) ,@ARGS)))))

;;; <derivings>
(defun <derivings> (type-class-name type)
  (labels((rec(type-class-names &optional acc)
	    (if(endp type-class-names)
	      acc ; order is not issue.
	      (body(car type-class-names)(cdr type-class-names) acc)))
	  (body(type-class-name rest acc)
	    (rec (append (Type-class-constraints type-class-name) rest)
		 (loop :for interface :in (Type-class-interfaces type-class-name)
		       :collect (or (Interface-default interface)
				    (error "Default instance is not found. ~S"interface))
		       :into result
		       :finally (return (cons `(Definstance(,type-class-name ,type)
						 ,result)
					      acc))))))
    (rec(list type-class-name))))

(declaim(ftype(function((satisfies adt-value-p))fixnum)data-order))
(defun data-order(thing)
  (position (alexandria:ensure-car thing)
	    (Adt-constructors(Get-adt(Adt-constructor-type-of (nth-value 1(adt-value-p thing)))))
	    :test #'eq))
