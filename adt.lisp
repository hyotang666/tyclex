(in-package #:vs-haskell)

(named-readtables:in-readtable :cl-vs-haskell)

;;;; DEFDATA
(defmacro defdata(name lambda-list &rest constructor*)
  ;; trivial syntax check.
  (check-type name symbol)
  (assert(every #'symbolp lambda-list))
  (dolist(constructor constructor*)
    (assert(symbolp(alexandria:ensure-car constructor))))
  ;; body
  `(eval-when(:compile-toplevel :load-toplevel :execute)
     ,(<deftype> name lambda-list constructor*)
     ,@(mapcan #`(% #'<constructors> name lambda-list)constructor*)
     ,@(loop :for c :in constructor*
	     :for o :upfrom 0
	     :collect (<meta-info-setter> c o lambda-list name))
     ,@(mapcan #'<pattern-matcher> constructor*)
     ',name))

;;; <deftype>
(defun <deftype>(name lambda-list constructor*)
  (labels((optional-lambda-list(list)
	    (if(cdr list)
	      (list* (car list) '&optional (cdr list))
	      list))
	  (type-name(args constructor)
	    (cond
	      ((symbolp constructor)`'(eql ,constructor))
	      (args (comma-type-specifier args constructor))
	      ((list-constructor-p constructor)
	       `',(apply #'cons-type-specifier
			 `(eql ,(car constructor))
			 (cdr constructor)))
	      (t `',(apply #'cons-type-specifier
			   `(eql ,(car constructor))
			   (loop :for clause :in (cdr constructor)
				 :collect (or(getf clause :type)T)))))))
    `(DEFTYPE,name,(optional-lambda-list lambda-list)
       (LIST 'OR ,@(mapcar (lambda(constructor)
			     (type-name lambda-list constructor))
			   constructor*)))))

(defun list-constructor-p(constructor)
  (every #'millet:type-specifier-p (cdr constructor)))

(defun cons-type-specifier(&rest types)
  (if(endp types)
    'null
    `(cons ,(car types),(apply #'cons-type-specifier (cdr types)))))

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

;;; <constructors>
(defun <constructors>(name args constructor)
  (flet((Ts(args)
	  (make-list(length args):initial-element t)))
    (cond
      ((keywordp constructor) nil)
      ((or args (list-constructor-p constructor))
       (let((lambda-list(Gensyms(cdr constructor))))
	 `(,@(if args
	       `((DECLAIM(FTYPE (FUNCTION ,(Ts (cdr constructor))
					  ,(constructor-return-type name))
				,(car constructor))))
	       `((DECLAIM(FTYPE(FUNCTION,(cdr constructor),name),(car constructor)))))
	    (DEFUN ,(car constructor),lambda-list
	      (LIST ',(car constructor),@lambda-list)))))
      (t `((DEFSTRUCT(,(car constructor)
		       :NAMED (:TYPE LIST) (:CONC-NAME NIL)
		       (:COPIER NIL) (:PREDICATE NIL)
		       (:CONSTRUCTOR ,(car constructor)
				     ,@(when(find-if #'symbolp (cdr constructor))
					 `(,(mapcar #'alexandria:ensure-car (cdr constructor))))))
	     ,@(cdr constructor)))))))

(defun constructor-return-type(name)
  (list name '*))

;;; <meta-info-setter>
(defun <meta-info-setter>(constructor order lambda-list name)
  (let((c (alexandria:ensure-car constructor)))
    `(SETF (GET ',c 'adt-meta-info)
	   (MAKE-ADT :TYPE-OF ',(if lambda-list
				  (constructor-return-type name)
				  name)
		     :TYPES ',(arg-types constructor lambda-list)
		     :ORDER ,order))))

(defun arg-types(constructor args)
  (cond
    ((keywordp constructor) `(eql ,constructor))
    ((or args (list-constructor-p constructor))
     (sublis (mapcar (lambda(elt)
		       (cons elt (Envar elt)))
		     args)
	     (cdr constructor)))
    (t (mapcar (lambda(slot)
		 (getf slot :type t))
	       (cdr constructor)))))

;;; <pattern-matcher>
(defun <pattern-matcher>(constructor)
  (when(listp constructor)
    (let((name(car constructor)))
      `((TRIVIA:DEFPATTERN,name(&REST ARGS)
	  `(LIST (EQ ',',name) ,@ARGS))))))

;;;; ADT data structure
(defstruct(adt (:copier nil)(:predicate nil))
  (type-of (error "required") :read-only t :type (or symbol list))
  (types (error "required") :read-only t :type (or symbol list))
  (order (error "required") :type fixnum :read-only t))

;;;; Trivial helpers
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
      (if(not(functionp thing))
	(type-of thing)
	(let((name(millet:function-name thing)))
	  (if name
	    (introspect-environment:function-type name)
	    'function))))))

(defun data-order(thing)
  (let((adt(adt-p thing)))
    (when adt
      (adt-order adt))))

(defun data-types(thing)
  (let((adt(adt-p thing)))
    (when adt
      (adt-types adt))))

;;;; MATCH
(defmacro match (thing &rest clauses)
  `(trivia:match ,thing
     ,@(mapcar (lambda(clause)
		 (<match-clause> thing clause))
	       clauses)))

(defun <match-clause>(thing clause)
  `(,(car clause),@(<consequent-declares> thing(car clause)),@(cdr clause)))

(defun <consequent-declares>(thing pattern)
  (labels((rec(pattern data-types thing-types &optional acc)
	    (if(endp pattern)
	      (do-return acc)
	      (body (car pattern)(cdr pattern)
		    (car data-types)(cdr data-types)
		    (car thing-types)(cdr thing-types)
		    acc)))
	  (body(pat pat-rest type type-rest thing thing-rest acc)
	    (if(string= '#:_ pat)
	      (rec pat-rest type-rest thing-rest acc)
	      (if(unify:variablep type)
		(if thing
		  (rec pat-rest type-rest thing-rest
		       (cons `(,thing ,pat) acc))
		  (rec pat-rest type-rest thing-rest
		       (cons `(,type ,pat)acc))))))
	  (do-return(acc)
	    (when acc
	      `((DECLARE ,@acc)))))
    (when(and (adt-p pattern)
	      (not(symbolp pattern)))
      (rec (cdr pattern)
	   (data-types pattern)
	   (thing-types thing (car pattern))))))

(defun thing-types(thing name)
  (if(symbolp thing)
    (let((types(introspect-environment:variable-type thing)))
      (unless(eq T types)
	(list-type-specifier (find-type-specifier name types))))
    (let((types(third(introspect-environment:function-type (car thing)))))
      (unless(or (eq '* types)
		 (eq T types)
		 (typep types '(cons (eql values)
				     (cons (or (eql *)
					       (eql T))
					   t))))
	(list-type-specifier (find-type-specifier name(introspect-environment:typexpand types)))))))

(defun find-type-specifier(name types)
  (let((type-specifier `(CONS (EQL CONS)
			      (CONS (CONS * (CONS (EQL ,name) NULL))
				    *))))
    (trestrul:traverse (lambda(elt)
			 (when(typep elt type-specifier)
			   (return-from find-type-specifier elt)))
		       types)))

(defun list-type-specifier(type-spec)
  (labels((rec(list &optional acc)
	    (if(eq 'null list)
	      (nreverse acc)
	      (rec (third list)(push (second list)acc)))))
    (rec(third type-spec))))

;;;; MATCH*
(defmacro match*(things &rest clauses)
  `(TRIVIA:MATCH*,things
     ,@(mapcar (lambda(clause)
		 (<match*-clause> things clause))
	       clauses)))

;;; <match*-clause>
(defun <match*-clause>(things clause)
  `(,(car clause)
     ,@(mapcan #'<consequent-declares> things (car clause))
     ,@(cdr clause)))

;;;; EMATCH
(defmacro ematch (thing &rest clauses)
  `(trivia:ematch ,thing
     ,@(mapcar (lambda(clause)
		 (<match-clause> thing clause))
	       clauses)))

;;;; EMATCH*
(defmacro ematch*(things &rest clauses)
  `(TRIVIA:EMATCH*,things
     ,@(mapcar (lambda(clause)
		 (<match*-clause> things clause))
	       clauses)))

