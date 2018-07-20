(in-package #:vs-haskell)

;;;; DEFINE-TYPE-CLASS
(defmacro define-type-class((name type-var)super-classes methods &rest rest)
  ;; trivial syntax checking.
  (assert(symbolp name))
  (assert(symbolp type-var))
  (assert(listp super-classes))
  (assert(every #'symbolp super-classes))
  ;; as canonicalize
  (setf type-var (Envar type-var))
  ;; body
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (SETF(GET ',name 'TYPE-CLASS)(MAKE-INFO :NAME ',name :VAR ',type-var
					     :INSTANCES ',(mapcar #'car methods)))
     ,@(when super-classes
	 (<type-class-relation-setter> name super-classes))
     ,@(loop
	 :for (method lambda-list return-type) :in methods
	 :for gensyms = (Gensyms (length lambda-list))
	 :do (setf ; as canonicalise
	       lambda-list (patternize lambda-list)
	       return-type (patternize return-type))
	 :collect (<instance-info-setter> method name lambda-list return-type rest)
	 :collect (<defmacro> method gensyms lambda-list return-type))
     ',name))

;;; <type-class-relation-setter>
(defun <type-class-relation-setter>(name super-classes)
  `((SETF (TYPE-DIRECT-SUPERCLASSES(GET ',name 'TYPE-CLASS))',super-classes)
    ,@(loop :for type-class :in super-classes
	    :collect
	    `(PUSHNEW ',name (TYPE-DIRECT-SUBCLASSES(GET ',type-class 'TYPE-CLASS))))))

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
(defvar *sub-expand* nil)
(defvar *expand-verbose* T)
(defun <defmacro>(method gensyms lambda-list return-type &aux (sub-name(sub-name method)))
  `(DEFMACRO,method(&WHOLE WHOLE ,@gensyms &ENVIRONMENT ENV)
     (IF (EQ *SUB-EXPAND* WHOLE)
	 (ERROR "Trap infinite expansion ~S" whole)
	 (LET((*SUB-EXPAND* WHOLE))
	   (MULTIPLE-VALUE-BIND(EXPANDED RETURN-TYPE INFOS IL MACROS)(PARSE-WHOLE WHOLE ',sub-name ENV)
	     (DECLARE (IGNORE RETURN-TYPE)
		      (IGNORABLE INFOS))
	     (LET((BODY`(,',sub-name
			  ,@(LOOP :FOR FORM :IN EXPANDED
				  :COLLECT (expander:expand
					     `(MACROLET,MACROS,FORM) env)))))
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
		     WHOLE))))))))

(defun parse-whole(form sub-name &optional env)
  (let*((expanded(loop :for form :in (copy-list (cdr form))
		       :collect(expander:expand form env)))
	(return-types(compute-return-types expanded env))
	(infos(check-signature (instance-lambda-list (car form))
			       return-types))
	(instances(get-instance-lambda (car form) infos))
	(defs(first instances))
	(type(second instances))
	(constraint(third instances))
	(consts(when constraint
		 (alexandria:when-let((constructor(trestrul:find-node-if
						    (lambda(x)
						      (eq (car x)
							  (alexandria:ensure-car type)))
						    return-types)))
		   (let((return-type(second constructor))
			(instance-table(some #'instance-table
					     (type-instances (find-type-class constraint)))))
		     (second(find-if (lambda(cell)
				       (subtype? return-type (third cell)))
				     instance-table))))))
	(macros(loop :for (name . rest) :in defs
		     :when (eq name (car form))
		     :collect (cons sub-name rest)
		     :else :collect (cons name rest)))
	(type-class(instance-type-class (car form)))
	(defs(loop :for tc :in (type-direct-subclasses type-class)
		   :append (loop :for instance :in (type-instances (find-type-class tc))
				 :thereis (loop :for (nil defs type%) :in (instance-table instance)
						:when (eq type type%)
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
