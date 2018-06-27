(in-package :vs-haskell.spec)
(named-readtables:in-readtable jingoh.reader:syntax)

(requirements-about product)

#?(define-newtype product()
     'integer)
=> PRODUCT
,:before (fmakunbound 'product)

#?(definstance(monoid product)
    ((mempty()`(product 1))
     (mappend(a b)
       `(product (* ,a ,b)))))
=> MONOID

#?(mappend (product 3)(product 9))
=> 27

#?(mappend (product 3)(mempty))
=> 3
,:around (let(vs-haskell::*expand-verbose*)
	   (call-body))
,:lazy t

#?(mappend (product 3)
	   (mappend (product 4)
		    (product 2)))
=> 24

#?(defmacro mappend*(&body body)
    (if(endp (cdr body))
      (car body)
      `(MAPPEND ,(car body)
		(mappend* ,@(cdr body)))))
=> MAPPEND*
,:before (fmakunbound 'mappend*)

#?(mappend* (product 3) (product 4) (product 2))
=> 24

#?(mconcat(the(list product)'(3 4 2)))
=> 24
,:around (let(vs-haskell::*expand-verbose*)
	   (call-body))
,:lazy t

#?(define-newtype sum() 'integer)
=> SUM
,:before (fmakunbound 'sum)

#?(definstance(monoid sum)
    ((mempty()`(sum 0))
     (mappend(a b)
       `(sum (+ ,a ,b)))))
=> MONOID

#?(mappend (sum 2)(sum 9))
=> 11

#?(mappend (mempty)(sum 3))
=> 3
,:around(let(vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(mconcat (the (list sum)'(1 2 3)))
=> 6

;;; ANY

#?(define-newtype any() 'boolean)
=> ANY
,:before (fmakunbound 'any)

#?(definstance(monoid any)
    ((mempty()`(any nil))
     (mappend(a b)
       `(or ,a ,b))))
=> MONOID

#?(mappend (any t)(any nil))
=> T
#?(mappend (mempty)(any t))
=> T
,:around (let(vs-haskell::*expand-verbose*)
	   (call-body))
,:lazy t
#?(mconcat (the (list any) '(nil nil nil t)))
=> T
#?(mappend (the any (mempty))(mempty)) => NIL
,:around (let(vs-haskell::*expand-verbose*)
	   (call-body))
,:lazy t

;;; ALL

#?(define-newtype all() 'boolean)
=> ALL
,:before (fmakunbound 'all)

#?(definstance(monoid all)
    ((mempty()`(all t))
     (mappend(a b)
       `(and ,a ,b))))
=> MONOID

#?(mappend (mempty)(all t))
=> T
,:around (let(vs-haskell::*expand-verbose*)
	   (call-body))
,:lazy t

#?(mappend (mempty)(all nil))
=> NIL
,:around (let(vs-haskell::*expand-verbose*)
	   (call-body))
,:lazy t

#?(mconcat (the (list all) '(t t t)))
=> T
#?(mconcat (the (list all) '(t t nil)))
=> NIL

;;; ORDERING
#?(defdata ordering()
	   :lt :eq :gt)
=> ORDERING

#?(definstance(monoid ordering)
    ((mempty():eq)
     (mappend(a b)
       `(trivia:ematch*(,a ,b)
	  ((:lt _):lt)
	  ((:eq y)y)
	  ((:gt _):gt)))))
=> MONOID

#?(mappend :lt :gt)
=> :LT

#?(mappend :gt :lt) => :gt
#?(mappend (mempty) :lt) => :lt
,:around (let(vs-haskell::*expand-verbose*)
	   (call-body))
,:lazy t
#?(mappend (mempty) :gt) => :GT
,:around (let(vs-haskell::*expand-verbose*)
	   (call-body))
,:lazy t

#?(definstance(compare string)
    ((lt(a b)
       `(string< ,a ,b))
     (lte(a b)
       `(string<= ,a ,b))
     (gt(a b)
       `(string> ,a ,b))
     (gte(a b)
       `(string>= ,a ,b))
     (compare(a b)
       `(cond
	  ((string= ,a ,b):eq)
	  ((string< ,a ,b):lt)
	  (t :gt)))))
=> COMPARE

#?(defun length-compare(x y)
    (mappend (compare (length x)(length y))
	     (compare x y)))
=> LENGTH-COMPARE
,:before(fmakunbound 'length-compare)

#?(length-compare "zen" "ants") => :LT
#?(length-compare "zen" "ant") => :GT

#?(defun length-compare2(x y)
    (flet((vowels(x)
	    (count-if (curried-function:section find _ "aeiou")
		      x)))
      (declare(ftype (function(string)fixnum)vowels))
      (mappend* (compare (length x)(length y))
		(compare (vowels x)(vowels y))
		(compare x y))))
=> length-compare2
,:before (fmakunbound 'length-compare2)

#?(length-compare2 "zen" "anna") => :LT
#?(length-compare2 "zen" "ana") => :LT
#?(length-compare2 "zen" "ann") => :GT

;;; MAYBE
#?(definstance(monoid maybe)
    ((mempty()nothing)
     (mappend(a b)
       (trivia:ematch*(a b)
	 ((nothing m)m)
	 ((m nothing)m)
	 (((just m1)(just m2))
	  `(just(mappend ,m1 ,m2)))))))
=> MONOID

#?(mappend nothing (just "andy"))
=> (JUST "andy")
,:test equal

#?(mappend (just :lt)nothing)
=> (JUST :LT)
,:test equal

#?(mappend (just (sum 3))(just (sum 4)))
=> (JUST 7)
,:test equal

#?(define-newtype 1st(&optional a)
    `(maybe ,a))
=> 1ST
,:before (fmakunbound '1st)

#?(definstance(monoid 1st)
    ((mempty()'(1st nothing))
     (mappend(a b)
       `(trivia:ematch*(,a ,b)
	  (((just x)_)(1st(just x)))
	  ((nothing x)x)))))
=> MONOID

#?(mappend (1st(just #\a))(1st(just #\b))) => (just #\a)
,:test equal

#?(mappend (1st nothing)(1st (just #\b))) => (JUST #\b)
,:test equal

#?(mappend (1st (just #\a))(1st nothing)) => (JUST #\a)
,:test equal

#?(mconcat (the (list 1st)'(nothing (just 9)(just 10)))) => (JUST 9)
,:test equal

