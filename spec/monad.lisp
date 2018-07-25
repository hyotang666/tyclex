(in-package :vs-haskell.spec)
(named-readtables:in-readtable jingoh.reader:syntax)

(requirements-about MONAD)

;;;; 13.3
#?(>>= (just 9)(lambda(x)(.return (* x 10))))
=> (just 90)
,:test equal
,:around (let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	   (call-body))
,:lazy t

#?(>>= nothing(lambda(x)(.return (* x 10))))
=> NOTHING
,:test equal
,:around (let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	   (call-body))
,:lazy t

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

#?(vs-haskell::function-type land-right (fixnum pole)(maybe *))
=> LAND-RIGHT
#?(vs-haskell::function-type land-left (fixnum pole)(maybe *))
=> LAND-LEFT

#?(land-left 2 '(0 . 0))
=> (JUST (2 . 0))
,:test equal

#?(land-left 10 '(0 . 3))
=> NOTHING

#?(>>= (land-right 1 '(0 . 0))
       (curried-function:section land-left 2 _))
=> (JUST (2 . 1))
,:test equal
,:around(let(ehcl::*subtype-verbose*)(call-body))
,:lazy t

#?(>>= nothing (curried-function:section land-left 2 _))
=> NOTHING
,:around(let(ehcl::*subtype-verbose*)(call-body))
,:lazy t

#?(>>= (>>= (>>= (.return '(0 . 0))
		 (ehcl::curry land-right 2 _))
	    (ehcl::curry land-left 2 _))
       (ehcl::curry land-right 2 _))
=> (JUST (2 . 4))
,:test equal
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(defmacro >>=* (&rest expression*)
    (labels((rec(list)
	      (if(endp(cdr list))
		(car list)
		`(>>= ,(rec (cdr list))
		      ,(car list)))))
      (rec (reverse expression*))))
=> >>=*
,:before (fmakunbound '>>=*)

#?(>>=* (.return '(0 . 0))
	(ehcl::curry land-right 2 _)
	(ehcl::curry land-left 2 _)
	(ehcl::curry land-right 2 _))
=> (JUST (2 . 4))
,:test equal
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>>=* (.return '(0 . 0))
	(ehcl::curry land-left 1 _)
	(ehcl::curry land-right 4 _)
	(ehcl::curry land-left -1 _)
	(ehcl::curry land-right -2 _))
=> NOTHING
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(setf(symbol-function 'banana)(constantly nothing))
:be-the function

#?(>>=* (.return '(0 . 0))
	(ehcl::curry land-left 1 _)
	#'banana
	(ehcl::curry land-right 1 _))
=> NOTHING
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>> nothing (just 3))
=> NOTHING
,:around(let(ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>> (just 3)(just 4))
=> (JUST 4)
,:test equal
,:around(let(ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>> (just 3) nothing)
=> NOTHING
,:around(let(ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>>= (>> (>>= (.return '(0 . 0))
		(ehcl::curry land-left 1 _))
	   nothing)
       (ehcl::curry land-right 1 _))
=> NOTHING
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(defmacro with-monad(first &rest rest)
    (assert(evenp(length rest)))
    (labels((rec(list acc)
	      (if(endp list)
		acc
		(rec (cddr list)
		     `(,(car list)
			,acc
			,(cadr list))))))
      (rec rest first)))
=> WITH-MONAD
,:before (fmakunbound 'with-monad)

#?(with-monad (.return '(0 . 0))
	      >>= (ehcl::curry land-left 1 _)
	      >> nothing
	      >>= (ehcl::curry land-right 1 _))
:expanded-to (>>= (>> (>>= (.return '(0 . 0))
			   (ehcl::curry land-left 1 _))
		      nothing)
		  (ehcl::curry land-right 1 _))

;;; DO
#?(>>= (just 3)(lambda(x)
		 (just (format nil "~S!" x))))
=> (JUST "3!")
,:test equal
,:around(let(ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>>= (just 3)
       (lambda(x)
	 (>>= (just "!")
	      (lambda(y)
		(just (format nil "~A~A" x y))))))
=> (JUST "3!")
,:test equal
,:around(let(ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>>= nothing
       (lambda(x)
	 (>>= (just "!")
	      (lambda(y)
		(just (format nil "~A~A" x y))))))
=> NOTHING
,:around(let(ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>>= (just 3)
       (lambda(x)
	 (>>= nothing
	      (lambda(y)
		(just (format nil "~A~A" x y))))))
=> NOTHING
,:around(let(ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>>= (just 3)
       (lambda(x)
	 (declare(ignore x))
	 (>>= (just "!")
	      (lambda(y)
		(declare(ignore y))
		nothing))))
=> NOTHING
,:around(let(ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(defmacro domonad(&rest expression*)
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
=> DOMONAD
,:before (fmakunbound 'domonad)

#?(domonad x <- (just 3)
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

#?(domonad x <- (just 9)
	   (just (> x 8)))
=> (JUST T)
,:test equal
,:around(let(ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(domonad start <- (.return '(0 . 0))
	   first <- (land-left 2 start)
	   second <- (land-right 2 first)
	   (land-left 1 second))
=> (JUST (3 . 2))
,:timeout 2
,:test equal
,:around(let(vs-haskell::*expand-verbose* vs-haskell::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(domonad start <- (.return '(0 . 0))
	   first <- (land-left 2 start)
	   nothing
	   second <- (land-right 2 first)
	   (land-left 1 second))
=> NOTHING
,:timeout 10
,:around(let(vs-haskell::*expand-verbose* vs-haskell::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(domonad (trivia:string* x _) <- (just "hello")
	   (.return x))
=> (JUST #\h)
,:test equal
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(domonad (string x _) <- (just "")
	   (.return x))
=> NOTHING
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

;;; LIST MONAD
#?(>>= '(3 4 5)
       (lambda(x)
	 (list x (- x))))
=> (3 -3 4 -4 5 -5)
,:test equal
,:around(let(vs-haskell::*return-type-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>>= nil
       (constantly '("bad" "mad" "rad")))
=> NIL
,:around(let(ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>>= '(1 2 3)
       (constantly nil))
=> NIL
,:around(let(ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>>= '(1 2)
       (lambda(n)
	 (>>= '(#\a #\b)
	      (lambda(c)
		(.return (cons n c))))))
=> ((1 . #\a)(1 . #\b)(2 . #\a)(2 . #\b))
,:test equal
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)(call-body))
,:lazy t

#?(domonad n <- '(1 2)
	   c <- '(#\a #\b)
	   (.return (cons n c)))
=> ((1 . #\a)(1 . #\b)(2 . #\a)(2 . #\b))
,:test equal
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)(call-body))
,:lazy t

#?(defmacro lc (&rest expression*)
    `(domonad ,@(labels((rec(list &optional acc)
			  (if(endp list)
			    (nreverse acc)
			    (if(and (symbolp(second list))
				    (string= '#:<- (second list)))
			      (rec (cdddr list)(list* (third list)(second list)(first list)acc))
			      (rec (cdr list)(cons `(guard ,(car list))acc))))))
		  (rec (cddr expression*)))
	      (.return ,(car expression*))))
=> LC
,:before (fmakunbound 'lc)

#?(lc (cons n c) || n <- '(1 2) c <- '(#\a #\b))
=> ((1 . #\a)(1 . #\b)(2 . #\a)(2 . #\b))
,:test equal
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)(call-body))
,:lazy t

;;; MONAD+
#?(defmacro guard(bool)
    `(if ,bool
       (.return nil)
       (mzero)))
=> GUARD
,:before (fmakunbound 'guard)

#?(>>= (the list(loop :for i :upfrom 1 :to 50 :collect i))
       (lambda(x)
	 (>> (guard(find #\7(princ-to-string x)))
	     (.return x))))
=> (7 17 27 37 47)
,:test equal
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose* ehcl::*return-type-verbose*)
	  (call-body))
,:lazy t

#?(domonad x <- (the list(loop :for i :upfrom 1 :to 50 :collect i))
	   (guard (find #\7 (princ-to-string x)))
	   (.return x))
=> (7 17 27 37 47)
,:test equal
,:around(let(ehcl::*expand-verbose* ehcl::*subtype-verbose* ehcl::*return-type-verbose*)
	  (call-body))
,:lazy t

#?(lc x || x <- (the list(loop :for i :upfrom 1 :to 50 :collect i))
      (find #\7 (princ-to-string x)))
=> (7 17 27 37 47)
,:test equal
,:around(let(ehcl::*expand-verbose* ehcl::*subtype-verbose* ehcl::*return-type-verbose*)
	  (call-body))
,:lazy t

;;;; KNIGHT
#?(deftype knight-pos()
    '(cons fixnum fixnum))
=> KNIGHT-POS

#?(defun move-knight(pos)
    (destructuring-bind(c . r)pos
      (domonad (cons c% r%) <- (list (cons (+ 2 c)(1- r))
				     (cons (+ 2 c)(1+ r))
				     (cons (- c 2)(- r 1))
				     (cons (- c 2)(1+ r))
				     (cons (1+ c)(- r 2))
				     (cons (1+ c)(+ r 2))
				     (cons (1- c)(- r 2))
				     (cons (1- c)(+ r 2)))
	       (guard(and (find c% '(1 2 3 4 5 6 7 8))
			  (find r% '(1 2 3 4 5 6 7 8))))
	       (.return (cons c% r%)))))
=> MOVE-KNIGHT
,:before (fmakunbound 'move-knight)
,:around(let(vs-haskell::*expand-verbose* ehcl::*return-type-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(declaim(ftype(function(knight-pos)(list knight-pos))move-knight))
=> implementation-dependent
,:lazy t

#?(move-knight '(6 . 2))
=> ((8 . 1)(8 . 3)(4 . 1)(4 . 3)(7 . 4)(5 . 4))
,:test equal

#?(move-knight '(8 . 1))
=> ((6 . 2)(7 . 3))
,:test equal

#?(defun in3(start)
    (domonad first <- (move-knight start)
	     second <- (move-knight first)
	     (move-knight second)))
=> IN3
,:before (fmakunbound 'in3)
,:around(let(vs-haskell::*expand-verbose* ehcl::*return-type-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(defun reach-in3-p(start end)
    (find end (in3 start) :test #'equal))
=> REACH-IN3-P
,:before (fmakunbound 'reach-in3-p)

#?(reach-in3-p '(6 . 2)'(6 . 1))
:satisfies identity

#?(reach-in3-p '(6 . 2)'(7 . 3))
=> NIL

;;;; 13.7
#?(>>= (.return 3)
       (lambda(x)
	 (just (+ 10000 x))))
=> (JUST 10003)
,:test equal
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(funcall (lambda(x)
	     (just (+ 10000 x)))
	   3)
=> (JUST 10003)
,:test equal

#?(>>= (.return "WaM")
       (lambda(x)
	 (list x x x)))
=> ("WaM" "WaM" "WaM")
,:test equal
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>>= (just "move on up")
       (lambda(x)(.return x)))
=> (JUST "move on up")
,:test equal
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>>= '(1 2 3 4)
       (lambda(x)(.return x)))
=> (1 2 3 4)
,:test equal
,:around(let(vs-haskell::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(>>= (put-string-line "Wah!")
       (lambda(x)(.return x)))
:outputs "Wah!
"
,:around(let(vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

;;;; 8.3
#?(funcall (domonad (put-string-line "Hello, what's your name?")
		    name <- (get-line)
		    (put-string-line (uiop:strcat "Hey " name ", you rock!"))))
:outputs "Hello, what's your name?
Hey hoge, you rock!
"
,:around (with-input-from-string(*standard-input* "hoge")
	   (call-body))

#?(domonad (put-string-line "What's your first name?")
	   first-name <- (get-line)
	   (put-string-line "What's your last name?")
	   last-name <- (get-line)
	   (let((big-first-name(string-upcase first-name))
		(big-last-name(string-upcase last-name)))
	     (funcall(put-string-line (uiop:strcat "Hey " big-first-name " " big-last-name ", how are you?")))))
:outputs "What's your first name?
What's your last name?
Hey HOGE FUGA, how are you?
"
,:around (with-input-from-string(*standard-input* (format nil "hoge~%fuga"))
	   (call-body))

#?(defun main()
    (domonad line <- (get-line)
	     (if(equal "" line)
	       (.return ())
	       (domonad (put-string-line(reverse line))
			(main)))))
=> MAIN
,:around(let(ehcl::*expand-verbose* ehcl::*return-type-verbose*)
	  (call-body))
,:lazy t
,:before(fmakunbound 'main)

#?(main)
:outputs "egoh
aguf
"
,:around(with-input-from-string(*standard-input* (format nil "hoge~%fuga~%~%"))
	  (call-body))

;;;;1.5
#?(lc (* x 2)|| x <- '#.(loop :for i :upfrom 1 :to 10 :collect i))
=> (2 4 6 8 10 12 14 16 18 20)
,:test equal
,:around(let(ehcl::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(lc (* x 2)|| x <- '#.(loop :for i :upfrom 1 :to 10 :collect i)(>= (* x 2)12))
=> (12 14 16 18 20)
,:test equal
,:around(let(ehcl::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(lc x || x <- '#.(loop :for i :upfrom 50 :to 100 :collect i)(= 3 (mod x 7)))
=> (52 59 66 73 80 87 94)
,:test equal
,:around(let(ehcl::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(defun boom-bangs(xs)
    (declare(type list xs))
    (lc (if (< x 10) "BOOM!" "BANG!") || x <- xs (oddp x)))
=> BOOM-BANGS
,:around(let(ehcl::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t
,:before (fmakunbound 'boom-bangs)

#?(boom-bangs '#.(loop :for i :upfrom 7 :to 13 :collect i))
=> ("BOOM!" "BOOM!" "BANG!" "BANG!")
,:test equal

#?(lc x || x <- '#.(loop :for i :upfrom 10 :to 20 :collect i)(/= x 13)(/= x 15)(/= x 19))
=> (10 11 12 14 16 17 18 20)
,:test equal
,:around(let(ehcl::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(lc (+ x y) || x <- '(1 2 3) y <- '(10 100 1000))
=> (11 101 1001 12 102 1002 13 103 1003)
,:test equal
,:around(let(ehcl::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(let((nouns'("hobo" "frog" "pope"))
       (adjectives '("lazy" "grouchy" "scheming")))
    (declare(type list nouns adjectives))
    (lc (uiop:strcat adjective " " noun) || adjective <- adjectives noun <- nouns))
=> ("lazy hobo" "lazy frog" "lazy pope" "grouchy hobo" "grouchy frog" "grouchy pope" "scheming hobo" "scheming frog" "scheming pope")
,:test equal
,:around(let(ehcl::*expand-verbose* ehcl::*subtype-verbose* ehcl::*return-type-verbose*)
	  (call-body))
,:lazy t

#?(apply #'+ (lc 1 || _ <- '(1 2 3)))
=> 3
,:around(let(ehcl::*expand-verbose* ehcl::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(let((xxs '((1 3 5 2 3 1 2 4 5)
	      (1 2 3 4 5 6 7 8 9)
	      (1 2 4 2 1 6 3 1 3 2 3 6))))
    (declare(type list xxs))
    (lc (lc x || x <- (the list xs) (evenp x)) || xs <- xxs))
=> ((2 2 4)(2 4 6 8)(2 4 2 6 2 6))
,:test equal
,:around(let(ehcl::*expand-verbose* ehcl::*subtype-verbose* ehcl::*return-type-verbose*)
	  (call-body))
,:lazy t

