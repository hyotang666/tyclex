(in-package :cl-user)
(defpackage :tyclex.newtype
  (:use :cl)
  (:import-from :tyclex.objects.newtype #:add-newtype)
  (:import-from :tyclex.compute-return-type #:compute-return-type)
  (:import-from :tyclex.unifier
		#:unify #:find-variable-value #:enwild #:dewild #:make-variable-list)
  (:export ; main api
    #:define-newtype
    #:denew
    )
  )
(in-package :tyclex.newtype)

(defmacro define-newtype(name lambda-list &body body)
  ;; trivial-syntax-check.
  (assert(typep name '(and symbol (not (or keyword boolean)))))
  (assert(listp lambda-list))
  ;; body
  (let((arg(gensym "ARG"))
       (env(gensym "ENV")))
    `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
       (ADD-NEWTYPE ',name)
       (DEFTYPE ,name ,lambda-list ,@body)
       ,(<rewinder> name lambda-list)
       (DEFMACRO,name(,arg &environment ,env)
	 ,@(if(null lambda-list)
	     `((declare(ignore ,env))`(THE ,',name ,,arg))
	     `((LET*((expanded(expander:expand ,arg ,env))
		     (return-type(Compute-return-type expanded ,env)))
		 `(THE ,(,(intern (format nil "REWIND-~A" name))return-type),,arg))))))))

(defun denew(thing)
  (if(typep thing '(cons (eql the)*))
    (third thing)
    thing))

(defun <rewinder>(name lambda-list)
  (alexandria:with-gensyms(type-specifier)
    `(DEFUN ,(intern (format nil "REWIND-~A" name))(,type-specifier)
       ,@(if(null lambda-list)
	   `((DECLARE(IGNORE ,type-specifier)) ',name)
	   (let((required(lambda-fiddle:required-lambda-vars lambda-list)))
	     (if(null required)
	       `((DECLARE(IGNORE ,type-specifier))',name)
	       (let*((variables(Make-variable-list(length required)))
		     (expanded(millet:type-expand`(,name ,@variables))))
		 `((let((env(Unify ',(Enwild expanded)
				   (Enwild ,type-specifier))))
		     `(,',name ,@(loop :for variable :in ',variables
				       :collect (or (Find-variable-value variable env)
						    '*)
				       :into args
				       :finally (return(Dewild args)))))))))))))
