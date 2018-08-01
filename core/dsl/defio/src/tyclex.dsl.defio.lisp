(defpackage :tyclex.dsl.defio
  (:use :cl #:tyclex.objects.io-action)
  (:export
    #:defio
    ))
(in-package :tyclex.dsl.defio)

;;;; DEFIO
(defmacro defio ((name signature return) &body body)
  (multiple-value-bind(vars types)(parse-signature signature)
    `(LOCALLY
       ;; SBCL emits compiler note about return type `IO-ACTION`.
       ;; note: type assertion too complex to check:
       #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (DECLAIM(FTYPE(FUNCTION,types(IO ,return)),name))
       (ADD-IO ',name :TYPE '(IO ,return))
       (DEFUN,name,vars
	 (MAKE-INSTANCE 'IO-ACTION
			:INSTANCE (LAMBDA(),@body)
			:TYPE '(IO ,return))))))

(defun parse-signature(signature)
  (labels((rec(signature &optional vars types)
	    (if(endp signature)
	      (do-return vars types)
	      (body (car signature)(cdr signature)vars types)))
	  (do-return(vars types)
	    (values (nreverse vars)
		    (nreverse types)))
	  (body(sig sig-rest vars types)
	    (if(find sig lambda-list-keywords :test #'eq)
	      (rec sig-rest (push sig vars) (push sig types))
	      (rec sig-rest
		   (push (car sig)vars)
		   (push (cadr sig)types)))))
    (rec signature)))
