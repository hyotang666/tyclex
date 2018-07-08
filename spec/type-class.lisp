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

#?(define-type-class(eq a)()
    ((==(a a) boolean)
     (/==(a a) boolean))
    (:default == (x y)
      `(not (/== ,x ,y)))
    (:default /== (x y)
      `(not (== ,x ,y))))
=> EQ
,:before (mapc #'fmakunbound '(== /==))

#?(defdata traffic-light()
     red yellow green)
=> TRAFFIC-LIGHT

#?(definstance(eq traffic-light)
    ((==(a b)
       (trivia:match*(a b)
	 ((red red)T)
	 ((green green)T)
	 ((yellow yellow)T)))))
=> EQ

#?(definstance(eq (maybe *))
    ((==(a b)
       (trivia:match*(a b)
	 ((nothing nothing)T)
	 (((just x)(just y))`(== ,x ,y))))))
=> EQ

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

#?(define-type-class(yes-no a)()
    ((yes-no(a)boolean)))
=> YES-NO
,:before (mapc #'fmakunbound '(yes-no))

#?(definstance(yes-no fixnum)
    ((yes-no(a)
       `(not(zerop ,a)))))
=> YES-NO

#?(definstance(yes-no symbol)
    ((yes-no(a)
       `(not(null ,a)))))
=> YES-NO

#?(definstance(yes-no list)
    ((yes-no(a)
       `(not(null ,a)))))
=> YES-NO

#?(definstance(yes-no (maybe *))
    ((yes-no(a)
       `(not(eq nothing ,a)))))
=> YES-NO

#?(definstance(yes-no traffic-light)
    ((yes-no(a)
       `(not(eq red ,a)))))
=> YES-NO

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

#?(defdata bool () false true)
=> BOOL

#?(define-type-class(ord a)()
    ((ord(a)integer))
    (:default ord(x)
      `(data-order ,x)))
=> ORD
,:before (mapc #'fmakunbound '(ord))
#?(definstance(ord number)
     ((ord(a)
	a)))
=> ORD
#?(definstance(ord character)
    ((ord(a)
       `(char-code ,a))))
=> ORD

#?(define-type-class(compare a)(ord)
    ((compare(a a)(member :eq :lt :gt))
     (lt(a a)boolean)
     (gt(a a)boolean)
     (lte(a a)boolean)
     (gte(a a)boolean))
    (:default compare (x y)
      `(compare (ord ,x)(ord ,y)))
    (:default lt (x y)
      `(< (ord ,x)(ord ,y)))
    (:default gt (x y)
      `(> (ord ,x)(ord ,y)))
    (:default lte (x y)
      `(<= (ord ,x)(ord ,y)))
    (:default gte (x y)
      `(>= (ord ,x)(ord ,y))))
=> COMPARE
,:before (mapc #'fmakunbound '(compare lt gt lte gte))

#?(definstance(compare number)
    ((compare(a b)
       `(let((a ,a)
	     (b ,b))
	  (cond
	    ((= a b):eq)
	    ((< a b):lt)
	    (t :gt))))))
=> COMPARE

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

#?(define-type-class(bounded a)()
    ((min-bound(a)T)
     (max-bound(a)T))
    (:default min-bound(x)
      `(cadr(introspect-environment:typexpand-1 ,x)))
    (:default max-bound(x)
      `(car(last(introspect-environment:typexpand-1 ,x)))))
=> BOUNDED
,:before (mapc #'fmakunbound '(min-bound max-bound))

#?(define-type-class(enum a)()
    ((succ(a)a)
     (pred(a)a))
    (:default succ (x)
      `(let((x ,x))
	 (cadr (nth (1+(data-order x))
		    (cdr(introspect-environment:typexpand-1(data-type-of x)))))))
    (:default pred (x)
      `(let((x ,x))
	 (cadr (nth (1-(data-order x))
		    (cdr(introspect-environment:typexpand-1(data-type-of x))))))))
=> ENUM
,:before (mapc #'fmakunbound '(succ pred))

#?(defdata week ()
    monday tuesday wednesday thursday friday saturday sunday)
=> WEEK

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

#?(define-type-class(functor f)()
    ((fmap((function(a)b)(f a))(f b))))
=> FUNCTOR
,:before (mapc #'fmakunbound '(fmap))

#?(definstance(functor maybe)
    ((fmap(f m)
       (trivia:ematch m
         ((just x)`(just(funcall ,f ,x)))
	 (nothing nothing)))))
=> FUNCTOR

#?(definstance(functor io)
    ((fmap(f a)
       `(action result <- ,a
		(vs-haskell::.return (funcall ,f result))))))
=> FUNCTOR

#?(definstance(functor function)
    ((fmap(f g)
       `(alexandria:compose ,f ,g))))
=> FUNCTOR

#?(definstance(functor list)
    ((fmap(f l)
       `(mapcar ,f ,l))))
=> FUNCTOR

#?(defdata counter-maybe(a)
    counter-nothing
    (counter-just fixnum a))
=> COUNTER-MAYBE
,:before (fmakunbound 'counter-just)
,:lazy t

#?(definstance(functor counter-maybe)
    ((fmap(f cm)
       (trivia:ematch cm
	 (counter-nothing counter-nothing)
	 ((counter-just counter x)`(counter-just (1+ ,counter)(funcall ,f ,x)))))))
=> FUNCTOR

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
#?(define-type-class(applicative f)(functor)
    ((pure(a)(f a))
     (<*>((f(function(a)b))(f a))(f b))))
=> APPLICATIVE
,:before (mapc #'fmakunbound '(pure <*>))

#?(definstance(applicative maybe)
    ((<*>(functor arg)
       (trivia:ematch functor
         (nothing nothing)
	 ((just f)`(fmap ,f ,arg))))
     (pure(x)
       `(just ,x))))
=> APPLICATIVE

#?(definstance(applicative list)
    ((<*>(functor arg)
       `(incf-cl:lc (funcall f x)
		    (incf-cl:<- f ,functor)
		    (incf-cl:<- x ,arg)))
     (pure(x)
       `(list ,x))))
=> APPLICATIVE

#?(definstance(applicative io)
    ((<*>(functor arg)
       `(action f <- ,functor
		x <- ,arg
		(vs-haskell::.return (funcall f x))))
     (pure(x)
       `(vs-haskell::.return ,x))))
=> APPLICATIVE

#?(definstance(applicative function)
    ((pure(x)
       `(constantly ,x))
     (<*>(f g)
       `(lambda(x)
	  (funcall (funcall ,f x) (funcall ,g x))))))
=> APPLICATIVE

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
#?(define-type-class(monoid m)()
    ((mempty()m)
     (mappend(m m)m)
     (mconcat((list m))m))
    (:default mconcat(ms)
      `(reduce (lambda(a m)(mappend a m))
	       ,ms
	       :from-end t
	       :initial-value (mempty))))
=> MONOID
,:before (mapc #'fmakunbound '(mempty mappend mconcat))

#?(definstance(monoid list)
    ((mempty()nil)
     (mappend(a b)
       `(append ,a ,b))))
=> MONOID

#?(definstance(monoid string)
    ((mempty()"")
     (mappend(a b)
       `(concatenate 'string ,a ,b))))
=> MONOID

#?(define-newtype product()
     'integer)
=> PRODUCT
,:before (fmakunbound 'product)

#?(definstance(monoid product)
    ((mempty()`(product 1))
     (mappend(a b)
       `(product (* ,a ,b)))))
=> MONOID

#?(define-newtype sum() 'integer)
=> SUM
,:before (fmakunbound 'sum)

#?(definstance(monoid sum)
    ((mempty()`(sum 0))
     (mappend(a b)
       `(sum (+ ,a ,b)))))
=> MONOID

#?(define-newtype any() 'boolean)
=> ANY
,:before (fmakunbound 'any)

#?(definstance(monoid any)
    ((mempty()`(any nil))
     (mappend(a b)
       `(or ,a ,b))))
=> MONOID

#?(define-newtype all() 'boolean)
=> ALL
,:before (fmakunbound 'all)

#?(definstance(monoid all)
    ((mempty()`(all t))
     (mappend(a b)
       `(and ,a ,b))))
=> MONOID

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

#?(definstance(monoid maybe)
    ((mempty()nothing)
     (mappend(a b)
       (trivia:ematch*(a b)
	 ((nothing m)m)
	 ((m nothing)m)
	 (((just m1)(just m2))
	  `(just(mappend ,m1 ,m2)))))))
=> MONOID

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
