(defpackage :tcl.monad.spec
  (:use #:tcl :jingoh #:tcl.monad #:tcl.data)
  (:shadowing-import-from :tcl.monad #:do #:return)
  )
(in-package :tcl.monad.spec)
(setup :tcl.monad)

(requirements-about monad)

;; Requirements.
#+* (equal (>>= (return x)
		f)
	   (f x))
 
#+* (equal (>>= m (lambda(x)(return x)))
	   m)

#+* (equal (>>= (>>= m f)
		g)
	   (>>= m (lambda(x)
		    (>>= (f x)
			 g))))

;; List
#?(>>= (return 1) (lambda(x)
		    (list (1+ x))))
:equivalents ((lambda(x)
		(list (1+ x)))
	      1)
,:test equal

#?(>>= (list 1)
       (lambda(x)(return x)))
:equivalents (list 1)
,:test equal

#?(>>= (>>= (list 1) (lambda(x)(list x 2)))
       (lambda(y)(list y 3)))
:equivalents (>>= (list 1)
		  (lambda(x)
		    (>>= ((lambda(x)(list x 2))x)
			 (lambda(y)(list y 3)))))
,:test equal

;; Function
#?(funcall (>>= (return 1) (lambda(x)
			     (lambda(y)
			       (declare(ignore y))
			       x)))
	   :hoge)
=> 1

#?(funcall ((lambda(x)
	      (lambda(y)
		(declare(ignore y))
		x))
	    1)
	   :hoge)
=> 1

#?(funcall (>>= (>>= (constantly 1) (lambda(x)(lambda(y)(list x y))))
		(lambda(x)(lambda(y)(list x y))))
	   :hoge)
=> ((1 :hoge) :hoge)
,:test equal

#?(funcall (>>= (constantly 1)
		(lambda(x)
		  (>>= ((lambda(x)(lambda(y)(list x y)))x)
		       (lambda(x)(lambda(y)(list x y))))))
	   :hoge)
=> ((1 :hoge) :hoge)
,:test equal

;; Io
#?(funcall (>>= (return 1) (lambda(x)
			     (make-instance 'io-action
					    :instance (lambda(y)(list x y))
					    :type '(io *))))
	   :hoge)
=> (1 :hoge)
,:test equal

#?(funcall ((lambda(x)
	      (make-instance 'io-action
			     :instance (lambda(y)(list x y))
			     :type '(io *)))
	    1)
	   :hoge)
=> (1 :hoge)
,:test equal

#?(funcall (>>= (>>= (make-instance 'io-action
				    :instance (lambda() "one")
				    :type '(io string))
		     (lambda(x)
		       (make-instance 'io-action
				      :instance (lambda()(concatenate 'string x "two"))
				      :type '(io string))))
		(lambda(x)
		  (make-instance 'io-action
				 :instance (lambda()(concatenate 'string x "three"))
				 :type '(io string)))))
=> "onetwothree"
,:test equal

#?(funcall (>>= (make-instance 'io-action
			       :instance (lambda()"one")
			       :type '(io string))
		(lambda(X)
		  (>>= ((lambda(x)
			  (make-instance 'io-action
					 :instance (lambda()(concatenate 'string x "two"))
					 :type '(io string)))
			x)
		       (lambda(x)
			 (make-instance 'io-action
					:instance (lambda()(concatenate 'string x "three"))
					:type '(io string)))))))
=> "onetwothree"
,:test equal

;; Maybe
#?(>>= (return 1)(lambda(x)(just x)))
:equivalents ((lambda(x)(just x)) 1)
,:test equal

#?(>>= (just 1)(lambda(x)(return x)))
:equivalents (JUST 1)
,:test equal

#?(>>= (>>= (just 0)
	    (lambda(x)
	      (just (1+ x))))
       (lambda(x)
	 (just (1+ x))))
:equivalents (>>= (just 0)
		  (lambda(x)
		    (>>= ((lambda(y)(just (1+ y)))x)
			 (lambda(x)
			   (just (1+ x))))))
,:test equal

;;;; tight rope
#?(deftype birds()'integer)
=> BIRDS
#?(deftype pole()'(cons birds birds))
=> POLE

#?(defun land-left(n pole)
    (destructuring-bind(left . right)pole
      (if(< (abs (- (+ left n)
		    right))
	    4)
	 (just (cons (+ left n) right))
	 nothing)))
=> LAND-LEFT
,:before (fmakunbound 'land-left)

#?(defun land-right(n pole)
    (destructuring-bind(left . right)pole
      (if(< (abs (- left
		    (+ right n)))
	    4)
	 (just (cons left (+ n right)))
	 nothing)))
=> LAND-RIGHT
,:before (fmakunbound 'land-right)

#?(function-type land-right (fixnum pole)(maybe *))
=> LAND-RIGHT
#?(function-type land-left (fixnum pole)(maybe *))
=> LAND-LEFT

#?(land-left 2 '(0 . 0))
=> (JUST (2 . 0))
,:test equal

#?(land-left 10 '(0 . 3))
=> NOTHING

#?(>>= (land-right 1 '(0 . 0))
       (curry land-left 2 _))
=> (JUST (2 . 1))
,:test equal

#?(>>= nothing (curry land-left 2 _))
=> NOTHING

#?(>>= (>>= (>>= (return '(0 . 0))
		 (curry land-right 2 _))
	    (curry land-left 2 _))
       (curry land-right 2 _))
=> (JUST (2 . 4))
,:test equal

#?(>>=* (return '(0 . 0))
	(curry land-right 2 _)
	(curry land-left 2 _)
	(curry land-right 2 _))
=> (JUST (2 . 4))
,:test equal

#?(>>=* (return '(0 . 0))
	(curry land-left 1 _)
	(curry land-right 4 _)
	(curry land-left -1 _)
	(curry land-right -2 _))
=> NOTHING

#?(setf(symbol-function 'banana)(constantly nothing))
:be-the function

#?(>>=* (return '(0 . 0))
	(curry land-left 1 _)
	#'banana
	(curry land-right 1 _))
=> NOTHING

#?(>> nothing (just 3))
=> NOTHING

#?(>> (just 3)(just 4))
=> (JUST 4)
,:test equal

#?(>> (just 3) nothing)
=> NOTHING

#?(>>= (>> (>>= (return '(0 . 0))
		(curry land-left 1 _))
	   nothing)
       (curry land-right 1 _))
=> NOTHING

;;; DO
#?(>>= (just 3)(lambda(x)
		 (just (format nil "~S!" x))))
=> (JUST "3!")
,:test equal

#?(>>= (just 3)
       (lambda(x)
	 (>>= (just "!")
	      (lambda(y)
		(just (format nil "~A~A" x y))))))
=> (JUST "3!")
,:test equal

#?(>>= nothing
       (lambda(x)
	 (>>= (just "!")
	      (lambda(y)
		(just (format nil "~A~A" x y))))))
=> NOTHING

#?(>>= (just 3)
       (lambda(x)
	 (>>= nothing
	      (lambda(y)
		(just (format nil "~A~A" x y))))))
=> NOTHING

#?(>>= (just 3)
       (lambda(x)
	 (declare(ignore x))
	 (>>= (just "!")
	      (lambda(y)
		(declare(ignore y))
		nothing))))
=> NOTHING

#?(do x <- (just 3)
    y <- (just "!")
    (just (format nil "~A~A" x y)))
:expanded-to
(>>= (just 3)
     (lambda(x)
       (declare(ignorable x))
       (>>= (just "!")
	    (lambda(y)
	      (declare(ignorable y))
	      (just (format nil "~A~A" x y))))))

#?(do x <- (just 9)
    (just (> x 8)))
=> (JUST T)
,:test equal

#?(do start <- (return '(0 . 0))
    first <- (land-left 2 start)
    second <- (land-right 2 first)
    (land-left 1 second))
=> (JUST (3 . 2))
,:test equal

#?(do start <- (return '(0 . 0))
    first <- (land-left 2 start)
    nothing
    second <- (land-right 2 first)
    (land-left 1 second))
=> NOTHING

#?(do (trivia:string* x _) <- (just "hello")
    (return x))
=> (JUST #\h)
,:test equal

#?(do (string x _) <- (just "")
    (return x))
=> NOTHING
