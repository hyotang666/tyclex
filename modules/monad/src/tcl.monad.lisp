(tcl:defpackage :tcl.monad
  (:use :tcl #:tcl.data)
  (:shadow #:return #:do)
  (:export
    ;; type-class
    #:monad
    ;; interfaces
    #:return #:>>= #:>> #:fail
    ;; predicate
    #:monad-p
    ;; helpers
    #:>>=* #:do #:lift-m #:ap #:join
    ))
(in-package :tcl.monad)

(define-type-class(monad m)()
  ((return(a)(m a))
   (>>=((m a)(function(a)(m b)))(m b))
   (>>((m a)(m b))(m b))
   (fail(string)(m a)))
  (:default >> (x y)
    `(>>= ,x (lambda(#0=#:arg)
	       (declare(ignore #0#))
	       ,y)))
  (:default fail(msg)
    `(error ,msg)))

;;;; helpers
(defmacro >>=* (&rest expression*)
  (labels((rec(list)
	    (if(endp(cdr list))
	      (car list)
	      `(>>= ,(rec (cdr list))
		    ,(car list)))))
    (rec (reverse expression*))))

(defmacro do(&rest expression*)
  (labels((rec(list)
	    (let((second(second list)))
	      (if(and (symbolp second)
		      (string= '#:<- second))
		`(>>= ,(third list)
		      ,(if(symbolp(first list))
			 `(lambda(,(first list))
			    (declare(ignorable ,(first list)))
			    ,(rec(nthcdr 3 list)))
			 `(lambda(arg)
			    (trivia:match arg
			      (,(first list),(rec(nthcdr 3 list)))
			      (_ (fail "Pattern missing."))))))
		(if(cdr list)
		  `(>> ,(first list)
		       ,(rec (cdr list)))
		  (car list))))))
    (rec expression*)))

(defmacro lift-m(function &rest monad*)
  (let((gensyms(alexandria:make-gensym-list (length monad*))))
    `(do ,@(loop :for monad :in monad*
		 :for gensym :in gensyms
		 :collect gensym
		 :collect '<-
		 :collect monad)
       (return (funcall ,function ,@gensyms)))))

(defmacro ap(mf &rest monad*)
  (let((gensyms(alexandria:make-gensym-list(length monad*)))
       (function(gensym "FUNCTION")))
    `(do ,function <- ,mf
       ,@(loop :for monad :in monad*
	       :for gensym :in gensyms
	       :collect gensym
	       :collect '<-
	       :collect monad)
       (return(funcall ,function ,@gensyms)))))

(defmacro join(mm)
  (let((monad(gensym"MONAD")))
    `(do ,monad <- ,mm
       ,monad)))

;;;; Instances
(definstance(monad list)
  ((return(x)`(list ,x))
   (>>=(xs f)
     `(mapcan ,f ,xs))
   (fail(x)
     (declare(ignore x))
     NIL)))

(definstance(monad function)
  ((return(x)
     `(lambda(#0=#:_)
        (declare(ignore #0#))
        ,x))
   (>>=(h f)
     `(lambda(#1=#:w)
        (funcall (funcall ,f (funcall ,h #1#))
      	   #1#)))))

(definstance(monad io)
  ((return(x)
     `(make-instance 'io-action
		     :instance (lambda(),x)
		     :type '(io *)))
   (>>=(io fun)
     `(funcall ,fun (funcall ,io)))))

(definstance(monad maybe)
  ((return(x)
     `(just ,x))
   (>>=(m f)
     (trivia:match m
       (nothing nothing)
       ((just x)`(funcall ,f ,x))
       (_ (alexandria:with-gensyms(x g)
	    `(trivia:ematch*(,m ,f)
	       ((nothing _)nothing)
	       (((just ,x),g)(funcall ,g ,x)))))))
   (fail(a)
     (declare(ignore a))
     'nothing)))
