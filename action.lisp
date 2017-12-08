(in-package :vs-haskell)

(defclass io-action ()
  ((instance :initarg :instance :reader action-of)
   (type :initarg :type :reader io-type))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((c io-action) &key)
  (closer-mop:set-funcallable-instance-function c (action-of c)))

(deftype io(&optional return)
  `(and io-action (function * ,return)))

(defmacro defio ((name signature return) &body body)
  (multiple-value-bind(vars types)(parse-signature signature)
    `(LOCALLY
       ;; SBCL emits compiler note about return type `IO-ACTION`.
       ;; note: type assertion too complex to check:
       #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (DECLAIM(FTYPE(FUNCTION,types IO-ACTION),name))
       (DEFUN,name,vars
	 (MAKE-INSTANCE 'IO-ACTION
			:INSTANCE (LAMBDA(),@body)
			:TYPE '(IO ,return))))))

(eval-when(:compile-toplevel :load-toplevel :execute)
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
      (rec signature))))

(defio(put-string((string string))null)
  (write-string string)(force-output)(values))

(defio(put-char((char character))null)
  (write-char char)(values))

(defio(.print((arg t))null)
  (print arg)(values))

(defio(put-string-line((string string))null)
  (write-line string)(values))

(defio(get-line()string)
  (values(read-line)))

(defio(.sequence((ios list))list)
  (mapcar #'funcall ios))

(defun mapmonad (io arg*)
  (.sequence (mapcar io arg*)))

(defio(mapm ((io (and io-action (function(t)io-action)))
	     (arg* list))
	    null)
  (map nil (lambda(arg)(funcall(funcall io arg)))arg*))

(defio(forever((io io-action))null)
  (loop (funcall io)))

(defio(for-monad ((args list)
		  (io(and io-action (function(t)io-action))))
		 list)
  (mapcar (lambda(x)(funcall(funcall io x))) args))

(defio(interact(&optional((function #'identity)function))null)
  (loop :for content = (read-line nil nil)
	:while content
	:do (write-line(funcall function content))))

(defio(bracket ((prologue io-action)
		(epilogue (function(t)io-action))
		(body(function(t)io-action)))
	       T)
  (let(handle)
    (unwind-protect(progn (setf handle(funcall prologue))
			  (funcall(funcall body handle)))
      (funcall epilogue handle))))

(defdata io-mode()
  :read-mode :write-mode :append-mode :read-write-mode)

(defio(open-file ((path trivial-types:pathname-designator)
		  (mode io-mode))
		 stream)
  (apply #'open path (ecase mode
		       (:read-mode)
		       (:write-mode `(:direction :output :if-does-not-exist :create :if-exists :supersede))
		       (:append-mode `(:direction :output :if-exists :append :if-does-not-exist :create))
		       (:read-write-mode `(:direction :io :if-does-not-exist :create :if-exists :supersede)))))

(defio(h-close ((handle stream))T)
  (close handle))

(defmacro defaction(name lambda-list &body body)
  (check-type name symbol)
  `(PROGN (SETF (SYMBOL-FUNCTION ',name)
		(LAMBDA ,lambda-list
		  (FUNCALL(ACTION ,@body))))
	  ',name))

(defun may-call(arg)
  (if(functionp arg)
    (funcall arg)
    arg))

(defmacro action(&rest exp*)
  (labels((rec(exp* &optional acc)
	    (if(endp exp*)
	      (nreverse acc)
	      (cond
		((eq 'let (first exp*))
		 `((LET*,(second exp*)
		    ,@(rec (nthcdr 2 exp*)))))
		((eq '<- (second exp*))
		 `(,@(nreverse acc)
		    (LET((,(first exp*)(MAY-CALL ,(third exp*))))
		      ,@(rec (nthcdr 3 exp*)))))
		(T (rec (cdr exp*)(cons `(MAY-CALL ,(first exp*))acc))))))
	  )
    `(MAKE-INSTANCE 'IO-ACTION :INSTANCE (LAMBDA(),@(rec exp*))
		    :TYPE '(IO *))))

(defio(.return ((value T))T)
  value)

(defmacro do-contents((var &optional(reader '#'read)(stream '*standard-input*))&body body)
  `(LOOP :FOR ,var = (FUNCALL ,reader ,stream NIL)
	 :WHILE ,var
	 :DO ,@(mapcar(lambda(form)
	                `(MAY-CALL ,form))
		      body)))

(defio(with-file ((file-path trivial-types:pathname-designator)
		  (io-mode io-mode)
		  (function(function(stream)io-action)))
		 T)
  (let((*terminal-io* *terminal-io*))
    (unwind-protect(progn (setq *terminal-io* (apply #'open file-path (ecase io-mode
									(:read-mode)
									(:write-mode `(:DIRECTION :OUTPUT))
									(:append-mode `(:DIRECTION :OUTPUT :IF-EXISTS :APPEND))
									(:read-write-mode `(:DIRECTION :IO)))))
			  (funcall(funcall function *terminal-io*)))
      (close *terminal-io*))))

(defun lines(string)
  (uiop:split-string string :separator #.(string #\newline)))

(defun unlines(string*)
  (format nil "~{~A~^~%~}"string*))
