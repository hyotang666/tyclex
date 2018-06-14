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

#?(== red red) => T
#?(== green green) => T
#?(== yellow yellow) => T
#?(== red green) => NIL
#?(== red yellow) => NIL
#?(== green red) => NIL
#?(== green yellow) => NIL

#?(== red 'not-traffic-light) :signals error
,:lazy t

#?(definstance(eq (maybe *))
    ((==(a b)
       (trivia:match*(a b)
	 ((nothing nothing)T)
	 (((just x)(just y))`(== ,x ,y))))))
=> EQ

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

#?(compare true false) => :GT
#?(gt true false) => T
#?(lt true false) => NIL

#?(defdata week ()
    monday tuesday wednesday thursday friday saturday sunday)
=> WEEK

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

#?(gt saturday friday) => T
#?(compare monday wednesday) => :LT

#?(min-bound 'week) => (EQL MONDAY)
,:test equal
#?(max-bound 'week) => (EQL SUNDAY)
,:test equal

#?(succ monday) => TUESDAY
#?(pred saturday) => FRIDAY

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

#?(fmap #'1+ nothing) => NOTHING
#?(fmap #'1+ (just 0)) :be-the (maybe (eql 1))
#?(fmap #'identity (just 0)) :equivalents (identity (just 0))
,:test equal

#?(definstance(functor io)
    ((fmap(f a)
       `(action result <- ,a
		(.return (funcall ,f result))))))
=> FUNCTOR

#?(fmap #'reverse (get-line))
:satisfies #`(with-input-from-string(*standard-input* "hoge")
	       (& (functionp $result)
		  (typep $result 'io-action)
		  (equal "egoh" (funcall $result))))

#?(definstance(functor function)
    ((fmap(f g)
       `(alexandria:compose ,f ,g))))
=> FUNCTOR
#?(fmap #'1+ #'1+)
:satisfies #`(& (functionp $result)
		(eql 3 (funcall $result 1)))
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t

#?(definstance(functor list)
    ((fmap(f l)
       `(mapcar ,f ,l))))
=> functor
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

#?(<*> (just (curried-function::section + 3 _)) (just 9))
:satisfies #`(equal $result (just 12))
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t

#?(<*> (just (curried-function::section + 3 _)) nothing)
=> NOTHING

#?(<*> nothing (just "woot"))
=> NOTHING

#?(<*> (pure (curried-function::section + 3 _)) (just 9))
:satisfies #`(equal $result (just 12))
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t

#?(<*> (<*> (pure (curried-function::section + _ _))
	    (just 3))
       nothing)
=> NOTHING
,:around(let((vs-haskell::*subtype-verbose* nil))
	  (call-body))
,:lazy t

#?(<*> (<*> (pure (curried-function::section + _ _))
	    nothing)
       (just 5))
=> NOTHING
