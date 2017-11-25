(in-package #:vs-haskell)

(named-readtables:in-readtable :cl-vs-haskell)

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

(defun cons-type-specifier(&rest types)
  (if(endp types)
    'null
    `(cons ,(car types),(apply #'cons-type-specifier (cdr types)))))

(defmacro defdata(name lambda-list &rest constructor*)
  ;; trivial syntax check.
  (check-type name symbol)
  (assert(every #'symbolp lambda-list))
  (dolist(constructor constructor*)
    (assert(symbolp(alexandria:ensure-car constructor))))
  ;; local functions.
  (labels((type-name(args constructor)
	    (cond
	      ((symbolp constructor)`'(eql ,constructor))
	      (args (comma-type-specifier  args constructor))
	      ((list-constructor-p constructor)
	       `',(apply #'cons-type-specifier
			 `(eql ,(car constructor))
			 (cdr constructor)))
	      (t `',(apply #'cons-type-specifier
			   `(eql ,(car constructor))
			   (loop :for clause :in (cdr constructor)
				 :collect (or(getf clause :type)T))))))
	  (gensyms(args)
	    (loop :repeat (length args)
		  :collect (gensym)))
	  (constructors(args constructor)
	    (cond
	      ((keywordp constructor) nil)
	      ((or args (list-constructor-p constructor))
	       (let((lambda-list(gensyms(cdr constructor))))
		 `(,@(if args
		       `((declaim(ftype(function,(Ts (cdr constructor)),(return-type name))
				   ,(car constructor))))
		       `((declaim(ftype(function,(cdr constructor),name),(car constructor)))))
		   (defun ,(car constructor),lambda-list
		     (list ',(car constructor),@lambda-list)))))
	      (t `((defstruct(,(car constructor) :named (:type list)
						 (:conc-name nil)
						 (:copier nil)
						 (:predicate nil)
						 (:constructor ,(car constructor)
							       ,@(when(find-if #'symbolp (cdr constructor))
								   `(,(mapcar #'alexandria:ensure-car (cdr constructor))))))
		     ,@(cdr constructor))))))
	  (Ts(args)
	    (make-list(length args):initial-element t))
	  (return-type(name)
	    (list name '*))
	  (meta-info(constructor order)
	    (let((c (alexandria:ensure-car constructor)))
	      `(SETF (GET ',c 'adt-meta-info)
		     (MAKE-ADT :TYPE-OF ',(if lambda-list
					    (return-type name)
					    name)
			       :ORDER ,order))))
	  (optional-lambda-list(list)
	    (if(cdr list)
	      (list* (car list) '&optional (cdr list))
	      list))
	  )
    ;; body
    `(eval-when(:compile-toplevel :load-toplevel :execute)
       (deftype ,name,(optional-lambda-list lambda-list)
	 (list 'or ,@(mapcar #`(% #'type-name lambda-list) constructor*)))
       ,@(mapcan #`(% #'constructors lambda-list)constructor*)
       ,@(loop :for c :in constructor*
	       :for o :upfrom 0
	       :collect (meta-info c o))
       ,@(mapcan #'pattern-matcher constructor*)
       ',name)))

(defun pattern-matcher(constructor)
  (when(listp constructor)
    (let((name(car constructor)))
      `((TRIVIA:DEFPATTERN,name(&REST ARGS)
	  `(LIST (EQ ',',name) ,@ARGS))))))

(defstruct(adt (:copier nil)(:predicate nil))
  (type-of (error "required") :read-only t :type (or symbol list))
  (order (error "required") :type fixnum :read-only t))

(defun adt-p(thing)
  (or (when(keywordp thing)
	(get thing 'adt-meta-info))
      (when(and (listp thing)
		(symbolp (car thing)))
	(get (car thing) 'adt-meta-info))))

(defun data-type-of(thing)
  (let((adt(adt-p thing)))
    (if adt
      (adt-type-of adt)
      (type-of thing))))

(defun data-order(thing)
  (let((adt(adt-p thing)))
    (when adt
      (adt-order adt))))

