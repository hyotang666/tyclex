(defpackage :tcl.functor
  (:use :tcl #:tcl.data)
  (:export
    ;; type-class
    #:functor
    ;; interface
    #:fmap
    ;; predicate
    #:functor-p
    ))
(in-package :tcl.functor)

(define-type-class(functor f)()
  ((fmap((function(a)b)(f a))(f b))))


(definstance(functor list)
  ((fmap(f l)
     `(mapcar ,f ,l))))

(definstance(functor vector)
  ((fmap(f v)
     `(map 'vector ,f ,v))))

(definstance(functor string)
  ((fmap(f s)
     `(map 'string ,f ,s))))

(definstance(functor array)
  ((fmap(f a)
     (alexandria:with-gensyms(gf ga new i)
       `(let*((,gf ,f)
	      (,ga ,a)
	      (,new (alexandria:copy-array ,ga)))
	  (dotimes(,i (array-total-size ,ga),new)
	    (setf (row-major-aref ,new ,i)
		  (funcall ,gf (row-major-aref ,new ,i)))))))))

(definstance(functor hash-table)
  ((fmap(f h)
     `(alexandria:copy-hash-table ,h :key ,f))))

(definstance(functor function)
  ((fmap(f g)
     `(alexandria:compose ,f ,g))))

(definstance(functor maybe)
  ((fmap(f m)
     (alexandria:with-gensyms(x)
       `(trivia:ematch,m
	  ((just ,x)(just(funcall ,f ,x)))
	  (nothing nothing))))))

(definstance(functor io)
  ((fmap(f io)
     `(lambda()
	(funcall ,f (funcall ,io))))))
