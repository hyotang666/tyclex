(in-package :vs-haskell.spec)
(named-readtables:in-readtable jingoh.reader:syntax)

(requirements-about SETUP)

;;;; DATA
#?(defdata traffic-light()
     red yellow green)
=> TRAFFIC-LIGHT

#?(defdata bool () false true)
=> BOOL

#?(defdata week ()
    monday tuesday wednesday thursday friday saturday sunday)
=> WEEK

#?(defdata counter-maybe(a)
    counter-nothing
    (counter-just fixnum a))
=> COUNTER-MAYBE
,:before (fmakunbound 'counter-just)
,:lazy t

#?(defdata ordering()
	   :lt :eq :gt)
=> ORDERING

;;;; NEWTYPE
#?(define-newtype product()
     'integer)
=> PRODUCT
,:before (fmakunbound 'product)

#?(define-newtype sum() 'integer)
=> SUM
,:before (fmakunbound 'sum)

#?(define-newtype any() 'boolean)
=> ANY
,:before (fmakunbound 'any)

#?(define-newtype all() 'boolean)
=> ALL
,:before (fmakunbound 'all)

#?(define-newtype 1st(&optional a)
    `(maybe ,a))
=> 1ST
,:before (fmakunbound '1st)

;;;; type-class EQ
#?(define-type-class(eq a)()
    ((==(a a) boolean)
     (/==(a a) boolean))
    (:default == (x y)
      `(not (/== ,x ,y)))
    (:default /== (x y)
      `(not (== ,x ,y))))
=> EQ
,:before (mapc #'fmakunbound '(== /==))

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

;;;; type-class YES-NO
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

;;;; type-class ORD
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

;;;; type-class COMPARE
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

;;;; type-class BOUNDED
#?(define-type-class(bounded a)()
    ((min-bound(a)T)
     (max-bound(a)T))
    (:default min-bound(x)
      `(cadr(introspect-environment:typexpand-1 ,x)))
    (:default max-bound(x)
      `(car(last(introspect-environment:typexpand-1 ,x)))))
=> BOUNDED
,:before (mapc #'fmakunbound '(min-bound max-bound))

;;;; type-class ENUM
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

;;;; type-class FUNCTOR
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

#?(definstance(functor counter-maybe)
    ((fmap(f cm)
       (trivia:ematch cm
	 (counter-nothing counter-nothing)
	 ((counter-just counter x)`(counter-just (1+ ,counter)(funcall ,f ,x)))))))
=> FUNCTOR

;;;; type-class APPLICATIVE
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

;;;; MONOID
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

#?(definstance(monoid product)
    ((mempty()`(product 1))
     (mappend(a b)
       `(product (* ,a ,b)))))
=> MONOID

#?(definstance(monoid sum)
    ((mempty()`(sum 0))
     (mappend(a b)
       `(sum (+ ,a ,b)))))
=> MONOID

#?(definstance(monoid any)
    ((mempty()`(any nil))
     (mappend(a b)
       `(or ,a ,b))))
=> MONOID

#?(definstance(monoid all)
    ((mempty()`(all t))
     (mappend(a b)
       `(and ,a ,b))))
=> MONOID

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

#?(definstance(monoid 1st)
    ((mempty()'(1st nothing))
     (mappend(a b)
       `(trivia:ematch*(,a ,b)
	  (((just x)_)(1st(just x)))
	  ((nothing x)x)))))
=> MONOID

;;;; MONAD
#?(define-type-class(monad m)()
    ((.return(a)(m a))
     (>>=((m a)(function(a)(m b)))(m b))
     (>>((m a)(m b))(m b))
     (fail(string)(m a)))
    (:default >> (x y)
      `(>>= ,x (constantly ,y)))
    (:default fail(msg)
      `(error ,msg)))
=> MONAD
,:before (mapc #'fmakunbound '(.return >>= >> fail))

#?(definstance(monad maybe)
    ((.return(x)
       `(just ,x))
     (>>=(a b)
       (alexandria:with-gensyms(x f)
	 `(trivia:ematch*(,a ,b)
	    ((nothing _)nothing)
	    (((just ,x),f)(funcall ,f ,x)))))
     (fail(a)
       (declare(ignore a))
       'nothing)))
=> MONAD

#?(definstance(monad list)
    ((.return(x)`(list ,x))
     (>>=(xs f)
       `(mapcan ,f ,xs))
     (fail(x)
       (declare(ignore x))
       NIL)))
=> MONAD

#?(definstance(monad io)
    ((.return(x)
       `(constantly ,x))
     (>>=(io fun)
       `(funcall ,fun (funcall ,io)))))
=> MONAD

;;;; MONAD+
#?(define-type-class(monad+ m)(monad)
    ((mzero()(m a))
     (mplus((m a)(m a))(m a))))
=> MONAD+
,:before (mapc #'fmakunbound '(mzero mplus))

#?(definstance(monad+ list)
    ((mzero()nil)
     (mplus(a b)`(append ,a ,b))))
=> MONAD+

