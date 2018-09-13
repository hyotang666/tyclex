(in-package :cl-user)
(defpackage :tyclex.dsl.define-type-class
  (:use :cl)

  (:import-from :tyclex.type-matcher
		#:type-match-p)
  ;; Objects
  (:import-from :tyclex.objects.type-class
		;; Slot readers.
		#:type-class-constraints #:type-class-interfaces #:type-class-member
		;; Helpers.
		#:find-type-class #:add-type-class)
  (:import-from :tyclex.objects.instance
		;; Slot readers.
		#:instance-constraints #:instance-definitions #:instance-signature #:instance-types)
  (:import-from :tyclex.objects.interface
		;; SLot readers.
		#:interface-instances #:interface-lambda-list #:interface-type-class
		;; Helpers.
		#:add-interface)
  (:import-from :tyclex.compute-return-type
		#:compute-return-types)
  (:import-from :tyclex.curry
		#:canonicalize-return-type)

  (:export
    ;; Main API
    #:define-type-class
    ;; Useful helpers
    #:infinite-expansion-detecter
    ))
(in-package :tyclex.dsl.define-type-class)

;;;; DEFINE-TYPE-CLASS
(defmacro define-type-class((name &rest type-var+)(&rest var-constraint*) signature+ &rest rest)
  ;; trivial syntax checking.
  (assert(symbolp name))
  (assert type-var+)
  (assert(every #'symbolp type-var+))
  (assert(loop :for (constraint var) :in var-constraint*
	       :always (and (Find-type-class constraint)
			    (find var type-var+))))
  (assert signature+)
  ;; as canonicalize
  (setf type-var+ (tyclex.unifier:envar type-var+))
  ;; body
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (ADD-TYPE-CLASS ',name :NAME ',name :VARS ',type-var+ :INTERFACES ',(mapcar #'car signature+))
     ,@(when var-constraint*
	 (<constraints-setter> name var-constraint*))
     ,@(loop
	 :for (interface lambda-list return-type) :in signature+
	 :for gensyms = (alexandria:make-gensym-list (length lambda-list))
	 :do (setf ; as canonicalise
	       lambda-list (tyclex.unifier:patternize lambda-list)
	       return-type (tyclex.unifier:patternize return-type))
	 :collect (<add-interface> interface name lambda-list return-type rest)
	 :collect (<defmacro> interface gensyms lambda-list return-type))
     ,(<type-class-predicate> name)
     ',name))

;;; <constraints-setter>
(defun <constraints-setter>(name var-constraints)
  (loop :for (constraint) :in var-constraints
	:collect
	`(PUSHNEW ',name (TYPE-CLASS-CONSTRAINTS ',constraint))))

;;; <add-interface>
(defun <add-interface>(interface name lambda-list return-type rest)
  `(ADD-INTERFACE ',interface
		  :TYPE-CLASS ',name
		  :LAMBDA-LIST ',lambda-list
		  :RETURN-TYPE ',return-type
		  ,@(let((default(find interface rest :key #'cadr)))
		      (when default
			`(:DEFAULT ',(cdr default))))))

;;; <defmacro>
(defun <defmacro>(interface gensyms lambda-list return-type &aux (sub-name(sub-name interface)))
  `(DEFMACRO,interface(&WHOLE WHOLE ,@gensyms &ENVIRONMENT ENV)
     (DECLARE(IGNORE ,@gensyms))
     (MULTIPLE-VALUE-BIND(EXPANDED RETURN-TYPE INFOS INSTANCE MACROS)(PARSE-WHOLE WHOLE ',sub-name ENV)
       (DECLARE (IGNORE RETURN-TYPE)
		(IGNORABLE INFOS))
       (LET((BODY`(,',sub-name
		    ,@(LOOP :FOR FORM :IN EXPANDED
			    :COLLECT (expander:expand `(MACROLET,MACROS,FORM) env)))))
	 (IF(NULL INSTANCE)
	   (RPLACD WHOLE EXPANDED)
	   (IF MACROS
	       ,(if(millet:type-specifier-p return-type)
		  ``(MACROLET,MACROS (THE ,',return-type ,BODY))
		  `(LET((RETURN(TYCLEX.UNIFIER:SUBSTITUTE-PATTERN ',return-type (TYCLEX.UNIFIER:UNIFY ',lambda-list (TYCLEX.UNIFIER:ENWILD INFOS)))))
		     (IF(MILLET:TYPE-SPECIFIER-P RETURN)
		       `(MACROLET,MACROS (THE ,RETURN ,BODY))
		       `(MACROLET,MACROS ,BODY))))
	       (PROGN
		 WHOLE)))))))

(defun parse-whole(form &optional (sub-name '#:sub-name) env)
  (let*((*macroexpand-hook* 'funcall) ; for easy debugging.
	(expanded(loop :with *macroexpand-hook* = 'funcall ; for easy debugging.
		       :for form :in (copy-list (cdr form))
		       :collect(expander:expand form env)))
	(return-types(Compute-return-types expanded env))
	(infos(check-signature (Interface-lambda-list (car form))
			       return-types))
	(instance(get-instance (car form) infos))
	(definitions(and instance (Instance-definitions instance)))
	(types(and instance (Instance-types instance)))
	(type-class(Interface-type-class (car form)))
	(constraints(and instance
			 (remove type-class (Instance-constraints instance):key #'car)))
	(instance-constraints-definitions
	  (when constraints
	    (let((constructors(mapcan (lambda(type)
					(let((constructor (trestrul:find-node-if
							    (lambda(x)
							      (eq (car x) (alexandria:ensure-car type)))
							    return-types)))
					  (when constructor
					    (list constructor))))
				      types)))
	      (when constructors
		(constraints-definitions constraints (mapcar #'second constructors))))))
	(macros(loop :for (name . rest) :in definitions
		     :when (eq name (car form))
		     :collect (cons sub-name rest)
		     :else :collect (cons name rest)))
	(type-class-constraints-definitions(constraints-definitions (Type-class-constraints type-class)
								    types)))
    (if(some (lambda(x)
	       (let((x(alexandria:ensure-car x)))
		 (or (tyclex.unifier:variablep x)
		     (eq t x))))
	     infos)
      (values expanded return-types infos nil nil)
      (values expanded return-types infos instance (append macros
							   type-class-constraints-definitions
							   instance-constraints-definitions)))))

(defun constraints-definitions(constraints types)
  (loop :for constraint :in constraints
	:append (loop :for interface :in (Type-class-interfaces (alexandria:ensure-car constraint))
		      :thereis (loop :for instance :in (Interface-instances interface)
				     :when (every #'Type-match-p
						  types
						  (Instance-types instance))
				     :return (Instance-definitions instance)))))

(defun sub-name(symbol)
  (gensym(symbol-name symbol)))

;;;; CHECK-SIGNATURE
(defun check-signature(lambda-list type*)
  (loop :with environment = (tyclex.unifier:unify (tyclex.unifier:enwild type*)
						  (tyclex.unifier:enwild lambda-list))
	:for pattern :in lambda-list
	:collect (tyclex.unifier:dewild (tyclex.unifier:substitute-pattern pattern environment))))

;;;; GET-INSTANCE
(defun get-instance(interface type*)
  (if(every (lambda(x)
	      (eq T x))
	    type*)
    nil
    (compute-applicable-instance(collect-instance type* interface))))

;;;; COLLECT-INSTANCE
(defun collect-instance(type* interface)
  (remove-if-not (lambda(signature)
		   (every #'Type-match-p (Canonicalize-return-type type*)
			  (Canonicalize-return-type signature)))
		 (Interface-instances interface)
		 :key #'Instance-signature))

;;;; COMPUTE-APPLICABLE-INSTANCE
(defun compute-applicable-instance(list)
  (if(null(cdr list))
    (car list) ; only one element, does not need to sort.
    (let((sorted(sort-instance list)))
      (if(find (Instance-signature(car sorted))
	       (cdr sorted)
	       :key #'Instance-signature :test #'equal)
	nil ; duplicate signature, give up.
	(car sorted)))))

(defun sort-instance(list)
  (flet((type<(ts1 ts2)
	  (every #'Type-match-p (Canonicalize-return-type ts1)
		 (Canonicalize-return-type ts2))))
    (sort list #'type< :key #'Instance-signature)))

;;; <type-class-predicate>
(defun <type-class-predicate>(name)
  `(DEFUN,(intern(format nil "~A-P" name))(#0=#:arg)
     (find #0# (Type-class-member ',name):test #'Type-match-p)))

;;;; MACROEXPAND-HOOK
(define-condition infinite-expansion()())

(defun infinite-expansion-detecter(expander form env)
  (let*((*macroexpand-hook* #'funcall)
	(expanded(funcall expander form env)))
    (if(and (eq expanded form)
	    (macro-function(car form)))
      (progn (cerror "Re-expand it."'infinite-expansion)
	     form)
      expanded)))

(if(or (eq #'funcall *macroexpand-hook*)
       (eq 'funcall *macroexpand-hook*)
       (and (symbolp *macroexpand-hook*)
	    (null(symbol-package *macroexpand-hook*))
	    (string= *macroexpand-hook* 'infinite-expansion-detecter)))
  (setq *macroexpand-hook* 'infinite-expansion-detecter)
  (if(eq 'infinite-expansion-detecter *macroexpand-hook*)
    nil
    (if(y-or-n-p "~%TYCLEX try to replace *MACROEXPAND-HOOK*, but already set. ~S~%Really replace it?"*macroexpand-hook*)
      (setq *macroexpand-hook* 'infinite-expansion-detecter)
      (warn "TYCLEX could not detect infinite macro expansion."))))
