(in-package :cl-user)
(defpackage :tcl.builder(:use :cl))
(in-package :tcl.builder)

(eval-when(:compile-toplevel :load-toplevel :execute)
  (handler-bind((package-error #'continue))
    (make-package "TCL" :use nil)))

(defmacro tcl::defun (name lambda-list &body body &environment env)
  (if(or (listp name) ; setf form, ignore.
	 (or (null(tyclex.curry:function-type-of name))
	     (equal '(function * *)(introspect-environment:function-type name))))
    `(cl:defun ,name ,lambda-list ,@body)
    ;; bind
    (let((types(second(or (tyclex.curry:function-type-of name)
			  (introspect-environment:function-type name)))))
      (multiple-value-bind(body decls doc)(alexandria:parse-body body :documentation t)
	(setf doc (alexandria:ensure-list doc)) ; as canonicalize.
	`(cl:defun ,name ,lambda-list
		   ,@doc
		   ,@(append decls (lambda-var-decls lambda-list types env))
		   ,@body)))))

(defun lambda-var-decls(lambda-list types &optional env)
  (labels((entry-point(lambda-list types &optional acc)
	    (if(endp lambda-list)
	      acc
	      (body (car lambda-list)(cdr lambda-list)types acc)))
	  (body(elt rest types acc)
	    (if(lambda-list:lambda-list-keyword-p elt)
	      (diverge elt rest types acc)
	      (entry-point rest (cdr types)(acc-declare elt(car types) acc))))
	  (diverge(key rest types acc)
	    (ecase key
	      (&optional (optional rest types acc))
	      (&key (key rest types acc))
	      (&rest (&rest rest types acc))
	      (&allow-other-keys(otherkeys rest types acc))
	      (&aux (aux rest types acc))))
	  (acc-declare(var type acc)
	    (if(find type '(t *))
	      acc
	      (cons `(declare(type ,type ,var))
		    acc)))
	  (optional(rest types acc)
	    (if(not(eq '&optional(car types)))
	      (error "Not match lambda-list ~S" lambda-list)
	      (optional-body rest (cdr types)acc)))
	  (optional-body(rest types acc)
	    (if(endp rest)
	      acc
	      (if(lambda-list:lambda-list-keyword-p(car rest))
		(diverge (car rest)(cdr rest)types acc)
		(optional-body (cdr rest)
			       (cdr types)
			       (acc-declare (alexandria:ensure-car (car rest))
					    (car types)
					    (if(typep (car rest)'(cons symbol (cons * (cons symbol null))))
					      (acc-declare (third(car rest))
							   'BOOLEAN
							   acc)
					      acc))))))
	  (key(rest types acc)
	    (if(endp rest)
	      acc
	      (if(lambda-list:lambda-list-keyword-p(car rest))
		(diverge(car rest)(cdr rest)types acc)
		(key (cdr rest)types
		     (multiple-value-bind(var key)(ensure-var (car rest))
		       (acc-declare var (some (compare key) types)acc))))))
	  (ensure-var(thing)
	    (etypecase thing
	      (symbol (values thing thing))
	      ((cons symbol t)(values(car thing)(car thing)))
	      ((cons (cons keyword (cons symbol null))t)
	       (values(cadar thing)(caar thing)))))
	  (compare(key)
	    (lambda(type)
	      (and (not(lambda-list:lambda-list-keyword-p type))
		   (listp type)
		   (string= key (car type))
		   (cadr type))))
	  (otherkeys(rest types acc)
	    (if(endp rest)
	      acc
	      (diverge(car rest)(cdr rest)types acc)))
	  (aux(rest types acc)
	    (if(endp rest)
	      acc
	      (aux (cdr rest)types
		   (acc-declare (ensure-var (car rest))
				(tyclex:compute-return-type (cadr rest)env)
				acc))))
	  (&rest(rest types acc)
	    (if(endp (cdr rest))
	      (acc-declare (car rest)
			   (find-&rest-type types)
			   acc) 
	      (diverge (cadr rest)
		       (cddr rest)
		       types
		       (acc-declare (car rest)
				    (find-&rest-type types)
				    acc))))
	  (find-&rest-type(types)
	    (loop :for (key . value) :on types
		  :when (eq '&rest key)
		  :return `(list ,(car value))))
	  )
    (entry-point lambda-list types)))

(defmacro tcl::let((&rest bind*)&body body &environment env)
  (multiple-value-bind(body decls)(alexandria:parse-body body)
    `(let ,bind*
       ,(integrate-declares (alexandria:mappend #'cdr decls)
			    (mapcan (lambda(bind)
				      (when(and (listp bind)
						(cadr bind)
						(not(and (introspect-environment:specialp (car bind))
							 (eq (find-package :cl)(symbol-package (car bind)))))
						)
					`((,(car bind)
					    ,(tyclex:compute-return-type (cadr bind)env)))))
				    bind*))
       ,@body)))

(defun integrate-declares(decls bind-types)
  `(DECLARE ,@(reduce (lambda(acc elt)
			(if(some (lambda(clause)
				   (or (and (typep clause '(cons (eql type)(cons * T)))
					    (find (car elt) (cddr clause) :test #'eq))
				       (and (typep clause '(cons (eql ignore) *))
					    (find (car elt) (cdr clause) :test #'eq))))
				 decls)
			  acc
			  (cons `(type ,(second elt),(first elt))
				acc)))
		      bind-types
		      :initial-value decls)))

(defmacro tcl::let*((&rest bind*)&body body &environment env)
  (multiple-value-bind(body decls)(alexandria:parse-body body)
    `(let* ,bind*
       ,(integrate-declares (alexandria:mappend #'cdr decls)
			    (mapcan (lambda(bind)
				      (when(and (listp bind)
						(cadr bind))
					`((,(car bind)
					    ,(tyclex:compute-return-type (cadr bind)env)))))
				    bind*))
       ,@body)))

(defmacro tcl::declaim(&rest decls)
  `(progn 
     ,@(loop :for decl :in decls
	     :when (ftype-function-formp decl)
	     :collect (destructuring-bind(ftype (function arg return)name)decl
			(declare(ignore ftype function))
			`(tyclex.curry:function-type ,name ,arg ,return))
	     :else :collect `(declaim ,decl))))

(defun ftype-function-formp(form)
  (typep form '(cons (eql ftype)
		     (cons (cons (eql function)
				 *)
			   *))))

(defmacro tcl::defpackage(name &rest args)
  `(eval-when(:compile-toplevel :load-toplevel :execute)
     (handler-bind(#+sbcl (sb-ext:name-conflict (restart-invoker 'sb-impl::take-new))
		   #+sbcl (warning #'muffle-warning)
		   )
       (defpackage ,name ,@args))))

(defun restart-invoker(restart-name &rest args)
  (lambda(condition)
    (let((restart(find-restart restart-name condition)))
      (when restart
	(apply #'invoke-restart restart args)))))

(define-condition package-warning(style-warning simple-condition)
  ()
  (:report (lambda(condition stream)
	     (apply #'format stream
		    (simple-condition-format-control condition)
		    (simple-condition-format-arguments condition)))))

(dolist(package '(:tyclex :cl))
  (let(import export)
    (do-external-symbols(symbol package)
      (multiple-value-bind(s status)(find-symbol(symbol-name symbol) :tcl)
	(if status
	  (pushnew s export :test #'string=)
	  (progn (push symbol import)
		 (push symbol export)))))
    (import import :tcl)
    (export export :tcl)))
