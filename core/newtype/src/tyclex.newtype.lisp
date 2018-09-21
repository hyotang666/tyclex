(in-package :cl-user)
(defpackage :tyclex.newtype
  (:use :cl)
  (:import-from :tyclex.objects.newtype #:add-newtype)
  (:import-from :tyclex.compute-return-type #:compute-return-type)
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
    `(PROGN (ADD-NEWTYPE ',name)
	    (DEFTYPE ,name ,lambda-list ,@body)
	    ,(<rewinder> name lambda-list body)
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

(defun <rewinder>(name lambda-list body)
  (alexandria:with-gensyms(type-specifier)
    `(DEFUN ,(intern (format nil "REWIND-~A" name))(,type-specifier)
       ,@(if(null lambda-list)
	   `((declare(ignore ,type-specifier)) ',name)
	   (let*((decls(nth-value 1(alexandria:parse-body body)))
		 (required(remove-if (lambda(x)(ignorep x decls))
				     (lambda-fiddle:extract-lambda-vars lambda-list))))
	     (if(null required)
	       `((declare(ignore ,type-specifier))',name)
	       (let*((gensyms(alexandria:make-gensym-list(length required)))
		     (expanded(millet:type-expand`(,name ,@gensyms)))
		     (paths(loop :for symbol :in gensyms
				 :for (path foundp) = (multiple-value-list (trestrul:path-to symbol expanded))
				 :when foundp :collect path)))
		 `(`(,',name ,@(loop :for path :in ',paths
				     :collect (trestrul:follow path ,type-specifier)))))))))))

(defun ignorep(symbol decls)
  (loop :for (nil . options) :in decls
	:do (loop :for (name . rest) :in options
		  :when (and (find name '(ignore) :test #'eq)
			     (find symbol rest :test #'eq))
		  :do (return-from ignorep T))))
