(defpackage :tcl.applicative
  (:use :tcl #:tcl.functor #:tcl.data)
  (:export
    ;; type-class
    #:applicative
    ;; interfaces
    #:pure #:<*>
    ;; predicate
    #:applicative-p
    ;; helper
    #:<*>* #:<$>
    ))
(in-package :tcl.applicative)

(define-type-class(applicative f)((functor f))
  ((pure(a)(f a))
   (<*>((f(function(a)b))(f a))(f b))))

(defmacro <*>*(&rest body)
  (labels((rec(body)
	    (if(endp (cdr body))
	      (car body)
	      `(<*> ,(rec(cdr body))
		    ,(car body)))))
    (rec(reverse body))))

(defmacro <$>(function &rest functors)
  (labels((rec(body)
	    (if(endp (cdr body))
	      `(fmap ,function ,(car body))
	      `(<*> ,(rec(cdr body))
		    ,(car body)))))
    (rec(reverse functors))))

(definstance(applicative list)
  ((<*>(list arg)
     (alexandria:with-gensyms(f)
       `(mapcan (lambda(,f)
		  (mapcar ,f ,arg))
		,list)))
   (pure(x)
     `(list ,x))))

(definstance(applicative function)
  ((pure(x)
     `(lambda(#0=#:arg)
        (declare(ignore #0#))
        ,x))
   (<*>(f g)
     (alexandria:with-gensyms(x)
     `(lambda(,x)
        (funcall(funcall ,f ,x) (funcall ,g ,x)))))))

(definstance(applicative io)
  ((<*>(functor arg)
     `(make-instance 'io-action
		     :instance (lambda()
				 (funcall (funcall ,functor)
					  (funcall ,arg)))
		     :type '(io *)))
   (pure(x)
     `(make-instance 'io-action
		     :instance (lambda(),x)
		     :type '(io *)))))

(definstance(applicative maybe)
  ((<*>(functor arg)
     (alexandria:with-gensyms(f)
       `(trivia:ematch ,functor
          (nothing nothing)
	  ((just ,f)(fmap ,f ,arg)))))
   (pure(x)
     `(just ,x))))
