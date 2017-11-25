(in-package :vs-haskell)

(defclass io-action ()
  ((instance :initarg :instance :reader action-of))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((c io-action) &key)
  (closer-mop:set-funcallable-instance-function c (action-of c)))

(defmacro defio (name lambda-list declarations &body body)
  `(LOCALLY
     ;; SBCL emits compiler note about return type `IO-ACTION`.
     ;; note: type assertion too complex to check:
     #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     (DECLAIM(FTYPE(FUNCTION,declarations IO-ACTION),name))
     (DEFUN,name,lambda-list
       (MAKE-INSTANCE 'IO-ACTION :INSTANCE (LAMBDA(),@body)))))

(defio put-string (string)(string)
  (write-string string)(force-output)(values))

(defio put-char(char)(character)
  (write-char char)(values))

(defio .print (arg)(t)
  (print arg)(values))

(defio put-string-line(string)(string)
  (write-line string)(values))

(defio get-line()()
  (read-line))

(defio .sequence(ios)(list)
  (mapcar #'funcall ios))

(defun mapmonad (io arg*)
  (.sequence (mapcar io arg*)))

(defio mapm (io arg*)((and io-action (function(t)io-action)) list)
  (map nil (lambda(arg)(funcall(funcall io arg)))arg*))

(defio forever (io)(io-action)
  (loop (funcall io)))

(defio for-monad(args io)(list(and io-action (function(t)io-action)))
  (mapcar (lambda(x)(funcall(funcall io x))) args))

(defio interact (&optional(function #'identity))(&optional function)
  (loop :for content = (read-line nil nil)
	:while content
	:do (write-line(funcall function content))))

(defio bracket(prologue epilogue body)(io-action (function(t)io-action)(function(t)io-action))
  (let(handle)
    (unwind-protect(progn (setf handle(funcall prologue))
			  (funcall(funcall body handle)))
      (funcall epilogue handle))))

(defdata io-mode()
  :read-mode :write-mode :append-mode :read-write-mode)

(defio open-file(path mode)(trivial-types:pathname-designator io-mode)
  (apply #'open path (ecase mode
		       (:read-mode)
		       (:write-mode `(:direction :output :if-does-not-exist :create :if-exists :supersede))
		       (:append-mode `(:direction :output :if-exists :append :if-does-not-exist :create))
		       (:read-write-mode `(:direction :io :if-does-not-exist :create :if-exists :supersede)))))

(defio h-close(handle)(stream)
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
    `(MAKE-INSTANCE 'IO-ACTION :INSTANCE (LAMBDA(),@(rec exp*)))))

(defio .return(value)(t)
  value)

(defmacro do-contents((var &optional(reader '#'read)(stream '*standard-input*))&body body)
  `(LOOP :FOR ,var = (FUNCALL ,reader ,stream NIL)
	 :WHILE ,var
	 :DO ,@(mapcar(lambda(form)
	                `(MAY-CALL ,form))
		      body)))

(defio with-file(file-path io-mode function)(trivial-types:pathname-designator io-mode (function(stream)io-action))
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

