(in-package #:vs-haskell)

;;;; DEFINE-TYPE-CLASS
(defmacro define-type-class((name &rest type-var+)(&rest var-constraint*) method+ &rest rest)
  ;; trivial syntax checking.
  (assert(symbolp name))
  (assert type-var+)
  (assert(every #'symbolp type-var+))
  (assert(loop :for (constraint var) :in var-constraint*
	       :always (and (Find-type-class constraint)
			    (find var type-var+))))
  (assert method+)
  ;; as canonicalize
  (setf type-var+ (Envar type-var+))
  ;; body
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (SETF(GET ',name 'TYPE-CLASS)(MAKE-INFO :NAME ',name :VARS ',type-var+
					     :INSTANCES ',(mapcar #'car method+)))
     ,@(when var-constraint*
	 (<type-class-relation-setter> name var-constraint*))
     ,@(loop
	 :for (method lambda-list return-type) :in method+
	 :for gensyms = (Gensyms (length lambda-list))
	 :do (setf ; as canonicalise
	       lambda-list (Patternize lambda-list)
	       return-type (Patternize return-type))
	 :collect (<instance-info-setter> method name lambda-list return-type rest)
	 :collect (<defmacro> method gensyms lambda-list return-type))
     ',name))

;;; <type-class-relation-setter>
(defun <type-class-relation-setter>(name var-constraints)
  `((SETF (TYPE-DIRECT-SUPERCLASSES(GET ',name 'TYPE-CLASS))',var-constraints)
    ,@(loop :for (constraint) :in var-constraints
	    :collect
	    `(PUSHNEW ',name (TYPE-DIRECT-SUBCLASSES(GET ',constraint 'TYPE-CLASS))))))

;;; <instance-info-setter>
(defun <instance-info-setter>(method name lambda-list return-type rest)
  `(SETF (GET ',method 'INSTANCE)
	 (INSTANCE-INFO :TYPE-CLASS ',name
			:LAMBDA-LIST ',lambda-list
			:RETURN-TYPE ',return-type
			,@(let((default(find method rest :key #'cadr)))
			    (when default
			      `(:DEFAULT ',(cdr default)))))))

;;; <defmacro>
(defvar *expand-verbose* T)
(defun <defmacro>(method gensyms lambda-list return-type &aux (sub-name(sub-name method)))
  `(DEFMACRO,method(&WHOLE WHOLE ,@gensyms &ENVIRONMENT ENV)
     (MULTIPLE-VALUE-BIND(EXPANDED RETURN-TYPE INFOS IL MACROS)(PARSE-WHOLE WHOLE ',sub-name ENV)
       (DECLARE (IGNORE RETURN-TYPE)
		(IGNORABLE INFOS))
       (LET((BODY`(,',sub-name
		    ,@(LOOP :FOR FORM :IN EXPANDED
			    :COLLECT (expander:expand `(MACROLET,MACROS,FORM) env)))))
	 (IF IL
	     ,(if(millet:type-specifier-p return-type)
		``(MACROLET,MACROS (THE ,',return-type ,BODY))
		`(LET((RETURN(SUBSTITUTE-PATTERN ',return-type (TYPE-UNIFY:UNIFY ',lambda-list (ENWILD INFOS)))))
		   (IF(MILLET:TYPE-SPECIFIER-P RETURN)
		     `(MACROLET,MACROS (THE ,RETURN ,BODY))
		     `(MACROLET,MACROS ,BODY))))
	     (PROGN
	       (WHEN *EXPAND-VERBOSE*
		     (WARN "Instance is not found. ~S ~S"',method (LIST ,@gensyms)))
	       WHOLE))))))

(defun parse-whole(form &optional (sub-name '#:sub-name) env)
  (let*((*macroexpand-hook* 'funcall) ; for easy debugging.
	(expanded(loop :for form :in (copy-list (cdr form))
		       :collect(expander:expand form env)))
	(return-types(Compute-return-types expanded env))
	(infos(check-signature (Instance-lambda-list (car form))
			       return-types))
	(instances(get-instance-lambda (car form) infos))
	(defs(first instances))
	(types(second instances))
	(constraint(car(third instances)))
	(consts(when constraint
		 (alexandria:when-let((constructors(mapcan (lambda(type)
							     (let((constructor (trestrul:find-node-if
										      (lambda(x)
											(eq (car x) (alexandria:ensure-car type)))
										      return-types)))
							       (when constructor
								 (list constructor))))
							   types)))
		   (let((return-types(mapcar #'second constructors))
			(instance-table(some #'Instance-table
					     (Type-instances (Find-type-class constraint)))))
		     (second(find-if (lambda(cell)
				       (find-if (lambda(return-type)
						  (Subtype? return-type (car(third cell))))
						return-types))
				     instance-table))))))
	(macros(loop :for (name . rest) :in defs
		     :when (eq name (car form))
		     :collect (cons sub-name rest)
		     :else :collect (cons name rest)))
	(type-class(Instance-type-class (car form)))
	(defs(loop :for tc :in (Type-direct-subclasses type-class)
		   :append (loop :for instance :in (Type-instances (Find-type-class tc))
				 :thereis (loop :for (nil defs type%) :in (Instance-table instance)
						:when (equal types type%)
						:return defs)))))
    (if(some (lambda(x)
	       (let((x(alexandria:ensure-car x)))
		 (or (type-unify:variablep x)
		     (eq t x))))
	      infos)
      (values expanded return-types infos nil nil)
      (values expanded return-types infos instances (append macros defs consts)))))

(defun sub-name(symbol)
  (gensym(symbol-name symbol)))

;;;; CHECK-SIGNATURE
(defun check-signature(lambda-list type*)
  (dewild (substitute-pattern lambda-list
			      (type-unify:unify (enwild type*)
						(enwild lambda-list)))))

;;;; GET-INSTANCE-LAMBDA
(defun get-instance-lambda(interface type*)
  (if(every (lambda(x)
	      (eq T x))
	    type*)
    nil
    (compute-applicable-instance(collect-instance type* interface))))

;;;; COLLECT-INSTANCE
(defun collect-instance(type* interface)
  (remove-if-not (lambda(type)
		   (every #'subtype? (canonicalize-return-type type*)
			  (canonicalize-return-type type)))
		 (instance-table interface)
		 :key #'car))

;;;; COMPUTE-APPLICABLE-INSTANCE
(defun compute-applicable-instance(list)
  (if(null(cdr list))
    (cdar list) ; only one element, does not need to sort.
    (let((sorted(sort-instance list)))
      (if(find(caar sorted)(cdr sorted):key #'car :test #'equal)
	nil ; duplicate signature, give up.
	(cdar list)))))

(defun sort-instance(list)
  (flet((type<(ts1 ts2)
	  (every #'subtype? (canonicalize-return-type ts1)
		 (canonicalize-return-type ts2))))
    (sort list #'type< :key #'car)))

;;;; MACROEXPAND-HOOK
(defun ehcl-macroexpand-hook(expander form env)
  (let*((*macroexpand-hook* #'funcall)
	(expanded(funcall expander form env)))
    (if(and (eq expanded form)
	    (macro-function(car form)))
      (error "Trap into infinite expansion. ~S"form)
      expanded)))

#++(if(or (eq #'funcall *macroexpand-hook*)
       (eq 'funcall *macroexpand-hook*))
  (setq *macroexpand-hook* 'ehcl-macroexpand-hook)
  (if(eq 'ehcl-macroexpand-hook *macroexpand-hook*)
    nil
    (if(y-or-n-p "~%EHCL try to replace *MACROEXPAND-HOOK*, but already set. ~S~%Really replace it?"*macroexpand-hook*)
      (setq *macroexpand-hook* 'ehcl-macroexpand-hook)
      (warn "EHCL could not detect infinite macro expansion."))))
#|
(defdata maybe (a)
  :nothing
  (just a))
(define-type-class(demo a)()
  ((demo(a)t)))
(definstance demo ((a bit)) 
  (format nil "Bit ~S"a))
(definstance demo ((a fixnum))
  (format nil "Fixnum ~S" a))
(definstance demo ((a integer))
  (format nil "Integer ~S"a))
(definstance demo ((a (maybe string)))
  (format nil "(maybe string) ~S" a))
(definstance demo ((a (maybe *)))
  (format nil "(maybe *) ~S" a))
(definstance demo ((a list))
  (format nil "list ~S" a))
|#
