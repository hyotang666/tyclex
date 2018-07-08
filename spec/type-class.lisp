(in-package :vs-haskell.spec)
(named-readtables:in-readtable jingoh.reader:syntax)

(requirements-about DEFINE-TYPE-CLASS)

;;;; Description:

#+syntax
(DEFINE-TYPE-CLASS (name &rest vars) super-classes methods &rest rest) ; => result

;;;; Arguments and Values:

; name := 

; vars := 

; super-classes := 

; methods := 

; rest := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:

#?(== red red) => T
#?(== green green) => T
#?(== yellow yellow) => T
#?(== red green) => NIL
#?(== red yellow) => NIL
#?(== green red) => NIL
#?(== green yellow) => NIL

#?(== red 'not-traffic-light) :signals error
,:lazy t

#?(== nothing nothing) => T
#?(== (just red)(just red)) => T
#?(== nothing (just red)) => NIL
#?(== (just red)(just green)) => NIL

#?(yes-no 0) => NIL
#?(yes-no 1) => T
#?(yes-no nil) => NIL
#?(yes-no '(a)) => T
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(yes-no T) => T
#?(yes-no nothing) => NIL
#?(yes-no (just 0)) => T
#?(yes-no red) => NIL
#?(yes-no yellow) => T
#?(yes-no green) => T

#?(compare true false) => :GT
,:around(let(vs-haskell::*subtype-verbose*
	      vs-haskell::*expand-verbose*
	      vs-haskell::*return-type-verbose*)
	  (call-body))
,:lazy t
#?(gt true false) => T
,:around(let(vs-haskell::*subtype-verbose*
	      vs-haskell::*expand-verbose*
	      vs-haskell::*return-type-verbose*)
	  (call-body))
,:lazy t
#?(lt true false) => NIL
,:around(let(vs-haskell::*subtype-verbose*
	      vs-haskell::*expand-verbose*
	      vs-haskell::*return-type-verbose*)
	  (call-body))
,:lazy t

#?(gt saturday friday) => T
,:around(let(vs-haskell::*subtype-verbose*
	      vs-haskell::*expand-verbose*
	      vs-haskell::*return-type-verbose*)
	  (call-body))
,:lazy t
#?(compare monday wednesday) => :LT
,:around(let(vs-haskell::*subtype-verbose*
	      vs-haskell::*expand-verbose*
	      vs-haskell::*return-type-verbose*)
	  (call-body))
,:lazy t

#?(min-bound 'week) => (EQL MONDAY)
,:test equal
,:around(let(vs-haskell::*subtype-verbose*
	      vs-haskell::*expand-verbose*
	      vs-haskell::*return-type-verbose*)
	  (call-body))
,:lazy t
#?(max-bound 'week) => (EQL SUNDAY)
,:test equal
,:around(let(vs-haskell::*subtype-verbose*
	      vs-haskell::*expand-verbose*
	      vs-haskell::*return-type-verbose*)
	  (call-body))
,:lazy t

#?(succ monday) => TUESDAY
,:around(let(vs-haskell::*subtype-verbose*
	      vs-haskell::*expand-verbose*
	      vs-haskell::*return-type-verbose*)
	  (call-body))
,:lazy t
#?(pred saturday) => FRIDAY
,:around(let(vs-haskell::*subtype-verbose*
	      vs-haskell::*expand-verbose*
	      vs-haskell::*return-type-verbose*)
	  (call-body))
,:lazy t

#?(fmap #'1+ #'1+)
:satisfies #`(& (functionp $result)
		(eql 3 (funcall $result 1)))
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t

#?(fmap #'1+ nothing) => NOTHING
,:around(let(ehcl::*subtype-verbose*)(call-body))
,:lazy t

#?(fmap #'1+ (just 0)) :be-the (maybe (eql 1))
,:around(let(ehcl::*subtype-verbose*)(call-body))
,:lazy t

#?(fmap #'identity (just 0)) :equivalents (identity (just 0))
,:test equal
,:around(let(ehcl::*subtype-verbose*)(call-body))
,:lazy t

#?(fmap #'reverse (get-line))
:satisfies #`(with-input-from-string(*standard-input* "hoge")
	       (& (functionp $result)
		  (typep $result 'io-action)
		  (equal "egoh" (funcall $result))))
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(fmap #'1+ '(1 2 3)) => (2 3 4)
,:test equal
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t
#?(fmap #'1+ nil) => NIL
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t
#?(fmap #'identity '(1 2 3)) :equivalents (identity '(1 2 3))
,:test equal
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t
#?(fmap #'identity nil) :equivalents (identity nil)
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t

#?(fmap #'1+ (counter-just 0 1))
:satisfies #`(not (equal $result (counter-just 0 1)))
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t

#?(defvar *a* (fmap (curried-function::section * _ _)'(1 2 3 4)))
=> *A*
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t

#?(fmap (lambda(f)(funcall f 9)) *a*)
=> (9 18 27 36)
,:test equal
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (locally(declare(special *a*))
	    (call-body)))
,:lazy t

;;;; applicative
#?(<*> (just (curried-function::section + 3 _)) (just 9))
:satisfies #`(equal $result (just 12))
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t

#?(<*> (just (curried-function::section + 3 _)) nothing)
=> NOTHING
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t

#?(<*> (just (curried-function:section uiop:strcat _ "hahaha")) nothing)
=> NOTHING
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t

#?(<*> nothing (just "woot"))
=> NOTHING
,:around(let(ehcl::*subtype-verbose*)(call-body))
,:lazy t

#?(<*> (pure (curried-function::section + 3 _)) (just 9))
:satisfies #`(equal $result (just 12))
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(<*> (<*> (pure (curried-function:section + _ _))
	    (just 3))
       (just 5))
=> (JUST 8)
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(<*> (<*> (pure (curried-function::section + _ _))
	    (just 3))
       nothing)
=> NOTHING
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(<*> (<*> (pure (curried-function::section + _ _))
	    nothing)
       (just 5))
=> NOTHING
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(defmacro <*>*(&rest body)
    (labels((rec(body)
	      (if(endp (cdr body))
		(car body)
		`(<*> ,(rec(cdr body))
		      ,(car body)))))
      (rec(reverse body))))
=> <*>*
,:before (fmakunbound '<*>*)

#?(<*>* (pure (curried-function:section + _ _))
	(just 3)
	(just 5))
=> (JUST 8)
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(<*>* (pure (curried-function::section + _ _))
	nothing
	(just 5))
:expanded-to
(<*> (<*> (pure (curried-function::section + _ _))
	  nothing)
     (just 5))

#?(defmacro <$>(function &rest functors)
    (labels((rec(body)
	      (if(endp (cdr body))
		`(fmap ,function ,(car body))
		`(<*> ,(rec(cdr body))
		      ,(car body)))))
      (rec(reverse functors))))
=> <$>
,:before (fmakunbound '<$>)

#?(<$> (curried-function::section concatenate 'string _ _)
       (just "johntra")
       (just "volta"))
=> (just "johntravolta")
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

;;; LIST
#?(<*> (list (curried-function::section * 0 _)
	     (curried-function::section + 100 _)
	     (curried-function::section expt _ 2))
       '(1 2 3))
=> (0 0 0 101 102 103 1 4 9)
,:test equal
,:around(let(vs-haskell::*return-type-verbose* vs-haskell::*subtype-verbose*)
	  (call-body))
,:lazy t

#?(<$> (curried-function::section concatenate 'string _ _)
       '("ha" "heh" "hmm")
       '("?" "!" "."))
=> ("ha?" "ha!" "ha." "heh?" "heh!" "heh." "hmm?" "hmm!" "hmm.")
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(<$> (curried-function::section * _ _)
       '(2 5 10)
       '(8 10 11))
=> (16 20 22 40 50 55 80 100 110)
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(remove-if-not (curried-function::section > _ 50)
		 (<$> (curried-function::section * _ _)
		      '(2 5 10)
		      '(8 10 11)))
=> (55 80 100 110)
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

;;; IO
#?(<$> (curried-function::section concatenate 'string _ _)
       (get-line)
       (get-line))
:satisfies #`(with-input-from-string(*standard-input* (format nil "one~%two"))
	       (& (functionp $result)
		  (equal "onetwo"
			 (funcall $result))))
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(action a <- (<$> (curried-function::section concatenate 'string _ _)
		    (get-line)
		    (get-line))
	  (put-string-line (concatenate 'string
					"The two lines concatenated turn out to be: "
					a)))
:satisfies
#`(with-input-from-string(*standard-input*(format nil "one~%two"))
    (& (functionp $result)
       (equal #.(format nil "The two lines concatenated turn out to be: onetwo~%")
	      (with-output-to-string(*standard-output*)
		(funcall $result)))))
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

;;; FUNCTION
#?(funcall (<$> (curried-function::section + _ _)
		(curried-function::section + _ 3)
		(curried-function::section * _ 100))
	   5)
=> 508
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(funcall (<$> (curried-function:section list _ _ _)
		(curried-function:section + _ 3)
		(curried-function:section * _ 2)
		(curried-function:section / _ 2))
	   5)
=> (8 10 5/2)
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

;;; 11.4
#?(defmacro lift(function &rest functor*)
    `(<$> ,function ,@functor*))
=> LIFT
,:before (fmakunbound 'lift)

#?(fmap #'list (just 4))
=> (JUST (4))
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(lift (curried-function:section cons _ _) (just 3)(just '(4)))
=> (just (3 4))
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(<$> (curried-function:section cons _ _)
       (just 3)
       (just '(4)))
=> (just (3 4))
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(defmacro sequence-a(applicative*)
    (trivia:ematch applicative*
      ((null)`(pure nil))
      ((cons x xs)`(<$> (curried-function:section cons _ _)
			,x
			(sequence-a ,xs)))))
=> SEQUENCE-A
,:before (fmakunbound 'sequence-a)

#?(sequence-a ((just 1)(just 2)))
=> (just (1 2))
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(sequence-a ((just 3)(just 2)(just 1)))
=> (just (3 2 1))
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(sequence-a ((just 3)nothing(just 1)))
=> NOTHING
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(funcall (sequence-a ((curried-function:section + _ 3)
			(curried-function:section + _ 2)
			(curried-function:section + _ 1)))
	   3)
=> (6 5 4)
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(mapcar (lambda(f)
	    (funcall f 7))
	  (list (curried-function:section > _ 4)
		(curried-function:section < _ 10)
		#'oddp))
=> (T T T)
,:test equal

#?(every #'identity (mapcar (lambda(f)
			      (funcall f 7))
			    (list (curried-function:section > _ 4)
				  (curried-function:section < _ 10)
				  #'oddp)))
=> T

#?(funcall (sequence-a ((curried-function:section > _ 4)
			(curried-function:section < _ 10)
			#'oddp))
	   7)
=> (T T T)
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(every #'identity
	 (funcall (sequence-a((curried-function:section > _ 4)
			      (curried-function:section < _ 10)
			      #'oddp))
		  7))
=> T
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(sequence-a ('(1 2 3)'(4 5 6)))
=> ((1 4)(1 5)(1 6)(2 4)(2 5)(2 6)(3 4)(3 5)(3 6))
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(sequence-a ('(1 2 3)'(4 5 6)'(3 4 4)nil))
=> nil
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(sequence-a ('(1 2)'(3 4)))
=> ((1 3)(1 4)(2 3)(2 4))
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(sequence-a ('(1 2)'(3 4)'(5 6)))
=> ((1 3 5)(1 3 6)(1 4 5)(1 4 6)(2 3 5)(2 3 6)(2 4 5)(2 4 6))
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(with-input-from-string(*standard-input* (format nil "heyh~%ho~%woo"))
    (funcall (sequence-a ((get-line)(get-line)(get-line)))))
=> ("heyh" "ho" "woo")
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

;;; 12.2
#?(mappend '(1 2 3)'(4 5 6))
=> (1 2 3 4 5 6)
,:test equal

#?(mappend "one" (mappend "two" "three"))
=> "onetwothree"
,:test equal

#?(mappend (mappend "one" "two")
	   "three")
=> "onetwothree"
,:test equal

#?(defmacro mappend* (&body form*)
    (labels((rec(form*)
	      (if(endp(cdr form*))
		(car form*)
		`(mappend ,(car form*)
			  ,(rec (cdr form*))))))
      (rec form*)))
=> MAPPEND*
,:before (fmakunbound 'mappend*)

#?(mappend* "one" "two" "three")
=> "onetwothree"
,:test equal

#?(mappend "pang" (mempty))
=> "pang"
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(mconcat '((1 2)(3 6)(9)))
=> (1 2 3 6 9)
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t
