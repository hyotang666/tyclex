(in-package #:vs-haskell)

(named-readtables:in-readtable :cl-vs-haskell)

;;;; DEFDATA
(defmacro defdata(name lambda-list &rest constructor*)
  ;; trivial syntax check.
  (check-type name symbol)
  (assert(every #'symbolp lambda-list))
  (dolist(constructor constructor*)
    (assert(symbolp(alexandria:ensure-car constructor))))
  ;; as canonicalize
  (dolist(c constructor*)
    (when(listp c)
      (rplaca c (make-symbol(symbol-name(car c))))))
  ;; body
  `(eval-when(:compile-toplevel :load-toplevel :execute)
     ,(<deftype> name lambda-list constructor*)
     (SETF(GET ',name 'ADT)T)
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
	       `',(cons-type-specifier `((eql ,(car constructor))
					 ,@(cdr constructor))))
	      (t `',(cons-type-specifier `((eql ,(car constructor))
					   ,@(loop :for clause :in (cdr constructor)
						   :collect (typecase clause
							      (SYMBOL T)
							      (LIST (getf clause :type T))))))))))
    `(DEFTYPE,name,(optional-lambda-list lambda-list)
       (LIST 'OR ,@(mapcar (lambda(constructor)
			     (type-name lambda-list constructor))
			   constructor*)))))

(defun list-constructor-p(constructor)
  (every #'millet:type-specifier-p (cdr constructor)))

(defun cons-type-specifier(types)
  (if(endp types)
    'null
    `(cons ,(car types),(cons-type-specifier (cdr types)))))

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
  (labels((arg-types(lambda-list arg-types &optional acc)
	    (if(endp arg-types)
	      (nreverse acc)
	      (if(find(car arg-types)lambda-list :test #'eq)
		(arg-types lambda-list (cdr arg-types)(push t acc))
		(arg-types lambda-list (cdr arg-types)(push (car arg-types)acc))))))
    (cond
      ((symbolp constructor)
       `((ALEXANDRIA:DEFINE-CONSTANT ,constructor ',constructor)))
      ((or args (list-constructor-p constructor))
       (let((lambda-list(Gensyms(cdr constructor))))
	 `(,@(if args
	       `((DECLAIM(FTYPE (FUNCTION ,(arg-types args (cdr constructor))
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

(defun constructor-name(constructor)
  (intern(symbol-name(car constructor))))

(defun constructor-return-type(name)
  (list name '*))

;;; <meta-info-setter>
(defun <meta-info-setter>(constructor order lambda-list name)
  (let((c (alexandria:ensure-car constructor))
       (meta-info `(MAKE-ADT :TYPE-OF ',(if lambda-list
					  (constructor-return-type name)
					  name)
			     :LAMBDA-LIST ',(mapcar (lambda(elt)
						      (if(millet:type-specifier-p elt)
							elt
							(Envar elt)))
						    lambda-list)
			     :TYPES ',(arg-types constructor lambda-list)
			     :ORDER ,order)))
    `(SETF (GET ',c 'adt-meta-info),meta-info
	   ,@(unless(symbolp constructor)
	       `((GET ',(constructor-name constructor) 'ADT-META-INFO)
		 ,meta-info)))))

(defun arg-types(constructor args)
  (cond
    ((symbolp constructor) `(eql ,constructor))
    ((or args (list-constructor-p constructor))
     (sublis (mapcar (lambda(elt)
		       (cons elt (Envar elt)))
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

;;;; ADT data structure
(defstruct(adt (:copier nil)(:predicate nil))
  (type-of (error "required") :read-only t :type (or symbol list))
  (types (error "required") :read-only t :type (or symbol list))
  (lambda-list (error "required") :read-only t :type list)
  (order (error "required") :type fixnum :read-only t))

;;;; Trivial helpers
(defun adv-p(thing)
  (or (when(symbolp thing)
	(get thing 'adt-meta-info))
      (when(and (listp thing)
		(symbolp (car thing)))
	(get (car thing) 'adt-meta-info))))

(defun adt-p(thing)
  (typecase thing
    (symbol (get thing 'adt))
    (list (adt-p (car thing)))))

(defun data-type-of(thing)
  (let((adt(adv-p thing)))
    (if adt
      (let((types(adt-types adt)))
	(if(null types)
	  (adt-type-of adt)
	  (if(eq 'eql (car types))
	    (adt-type-of adt)
	    (list* (car (adt-type-of adt))
		   (let((env(loop :with env = (unify:make-empty-environment)
				  :for v :in (cdr thing)
				  :for type :in (adt-types adt)
				  :when (unify:variablep type)
				  :do (setf env (unify:extend-environment type
									  (data-type-of v)
									  env))
				  :finally (return env))))
		     (mapcar (lambda(elt)
			       (unify:find-variable-value elt env))
			     (adt-lambda-list adt)))))))
      (typecase thing
	(io-action (io-type thing))
	(function (let((name(millet:function-name thing)))
		    (if name
		      (introspect-environment:function-type name)
		      'function)))
	(t (class-name(class-of thing)))))))

(defun data-order(thing)
  (let((adt(adv-p thing)))
    (when adt
      (adt-order adt))))

(defun data-types(thing)
  (let((adt(adv-p thing)))
    (when adt
      (adt-types adt))))

;;;; MATCH
(defmacro match (thing &rest clauses)
  `(trivia:match ,thing
     ,@(mapcar (lambda(clause)
		 (<match-clause> thing clause))
	       clauses)))

(defun <match-clause>(thing clause)
  `(,(car clause) ; <--- as pattern.
     ,@(<consequent-declares> thing(car clause))
     ,@(cdr clause))) ; <--- as body.

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
		       (if(eq t thing)
			 acc
			 (cons `(TYPE ,thing ,pat) acc)))
		  (rec pat-rest type-rest thing-rest
		       (cons `(TYPE ,type ,pat)acc))))))
	  (do-return(acc)
	    (when acc
	      `((DECLARE ,@acc)))))
    (let((adt-tag (adt-pattern-p pattern)))
      (when adt-tag
	(rec (cdr pattern)
	     (adt-types (get adt-tag 'adt-meta-info))
	     (thing-types thing adt-tag))))))

(defun adt-pattern-p(pattern)
  (let((pattern(trivia:pattern-expand-1 pattern)))
    (when(typep pattern '(CONS (EQL LIST)
			       (CONS (CONS (EQL EQ)
					   (CONS (CONS (EQL QUOTE)
						       *)
						 NULL))
				     *)))
      (second(second(second pattern))))))

(defun thing-types(thing name)
  (if(symbolp thing)
    (let((types(introspect-environment:variable-type thing)))
      (unless(eq T types)
	(list-type-specifier (find-type-specifier name types))))
    (let((types(third(introspect-environment:function-type (car thing)))))
      (unless(or (eq '* types)
		 (eq T types)
		 (typep types '(CONS (EQL VALUES)
				     (CONS (OR (EQL *)
					       (EQL T))
					   T))))
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

