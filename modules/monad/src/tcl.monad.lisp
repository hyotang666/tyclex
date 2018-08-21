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
    #:>>=* #:do
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
   (>>=(a b)
     (alexandria:with-gensyms(x f)
       `(trivia:ematch*(,a ,b)
          ((nothing _)nothing)
          (((just ,x),f)(funcall ,f ,x)))))
   (fail(a)
     (declare(ignore a))
     'nothing)))

#++
(definstance(monad (writer w) :constraints (monoid w))
  ((return(x)
     `(writer (cons ,x (mempty))))
   (>>=(writer f)
     `(destructuring-bind(#0=#:x . #1=#:v),writer
        (destructuring-bind(#2=#:y . #3=#:v%)(funcall ,f #0#)
          (writer(cons #2# (mappend #1# #3#))))))))

#++
(definstance(monad state)
  ((return(x)
     `(state(lambda(#0=#:s)
      	(cons ,x #0#))))
   (>>=(h f)
     `(state(lambda(#0#)
      	(destructuring-bind(#1=#:a . #2=#:new-state)(funcall ,h #0#)
      	  (let((#3=#:g(funcall ,f #1#)))
      	    (funcall #3# #2#))))))))

