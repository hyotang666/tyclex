(in-package :vs-haskell.spec)
(named-readtables:in-readtable jingoh.reader:syntax)

(requirements-about SETUP)

;;;; TYPE-CLASSES
#?(define-type-class(eq a)()
    ((==(a a) boolean)
     (/==(a a) boolean))
    (:default == (x y)
      `(not (/== ,x ,y)))
    (:default /== (x y)
      `(not (== ,x ,y))))
=> EQ
,:before (mapc #'fmakunbound '(== /==))

#?(define-type-class(yes-no a)()
    ((yes-no(a)boolean)))
=> YES-NO
,:before (mapc #'fmakunbound '(yes-no))

#?(define-type-class(ord a)()
    ((ord(a)integer))
    (:default ord(x)
      `(data-order ,x)))
=> ORD
,:before (mapc #'fmakunbound '(ord))

#?(define-type-class(compare a)((ord a))
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

#?(define-type-class(functor f)()
    ((fmap((function(a)b)(f a))(f b))))
=> FUNCTOR
,:before (mapc #'fmakunbound '(fmap))

#?(define-type-class(applicative f)((functor f))
    ((pure(a)(f a))
     (<*>((f(function(a)b))(f a))(f b))))
=> APPLICATIVE
,:before (mapc #'fmakunbound '(pure <*>))

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

#?(define-type-class(monad m)()
    ((.return(a)(m a))
     (>>=((m a)(function(a)(m b)))(m b))
     (>>((m a)(m b))(m b))
     (fail(string)(m a)))
    (:default >> (x y)
      `(>>= ,x (lambda(#0=#:arg)
		 (declare(ignore #0#))
		 ,y)))
    (:default fail(msg)
      `(error ,msg)))
=> MONAD
,:before (mapc #'fmakunbound '(.return >>= >> fail))

#?(define-type-class(monad+ m)((monad m))
    ((mzero()(m a))
     (mplus((m a)(m a))(m a))))
=> MONAD+
,:before (mapc #'fmakunbound '(mzero mplus))

#?(define-type-class(monad-writer w m)((monoid w)(monad m))
    ((.write((cons a w))(m a))
     (tell(w)(m null))
     (.listen((m a))(m (cons a w)))
     (pass((m (cons a (function(w)w))))(m a)))
    (:default .write(cons)
	      `(destructuring-bind(#0=#:a . #1=#:w),cons
		 (tell #1#)
		 (.return #0#)))
    (:default tell (w)
	      `(.write(cons nil ,w))))
=> MONAD-WRITER
,:before (mapc #'fmakunbound '(.write tell .listen pass))

;;;; DATA
#?(defdata traffic-light()
     red yellow green)
=> TRAFFIC-LIGHT

#?(defdata(bool (:deriving ord))()
    false true)
=> BOOL

#?(defdata(week (:deriving ord bounded enum)) ()
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

#?(define-newtype writer(&optional w a)
    `(cons ,a ,w))
=> WRITER
,:before (fmakunbound 'writer)

#?(define-newtype diff-list(&optional a)
    `(function((list ,a))(list ,a)))
=> DIFF-LIST
,:before (fmakunbound 'diff-list)

#?(define-newtype state(&optional s a)
    `(function(,s)(cons ,a ,s)))
=> STATE
,:before (fmakunbound 'state)

;;;; Instances of EQ
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

;;;; instances of YES-NO
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

;;;; instances of ORD
#?(definstance(ord number)
     ((ord(a)
	a)))
=> ORD

#?(definstance(ord character)
    ((ord(a)
       `(char-code ,a))))
=> ORD

;;;; instance of COMPARE
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

;;;; instances of FUNCTOR
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

#?(definstance(functor pair)
    ((fmap(f pair)
       `(trivia:match ,pair
          ((cons x y)(pair (cons (funcall ,f x)y)))))))
=> FUNCTOR

#?(definstance(functor zip-list)
    ((fmap(f zl)
       `(zip-list (fmap ,f ,(denew zl))))))
=> FUNCTOR

;;;; Instances of APPLICATIVE
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
       `(lambda(#0=#:arg)
	  (declare(ignore #0#))
	  ,x))
     (<*>(f g)
       `(lambda(x)
	  (funcall(funcall ,f x) (funcall ,g x))))))
=> APPLICATIVE

#?(definstance(applicative zip-list)
    ((pure(x)
       `(series:series ,x))
     (<*>(fs xs)
       `(zip-list (let((fn ,fs))
		    (series:collect (series:map-fn t #'funcall (series:scan fn)
						   (series:scan ,xs))))))))
=> APPLICATIVE

;;;; MONOID
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
       `(lambda(),x))
     (>>=(io fun)
       `(funcall ,fun (funcall ,io)))))
=> MONAD

#?(definstance(monad (writer w) :constraint (monoid w))
    ((.return(x)
       `(writer (cons ,x (mempty))))
     (>>=(writer f)
       `(destructuring-bind(#0=#:x . #1=#:v),writer
	  (destructuring-bind(#2=#:y . #3=#:v%)(funcall ,f #0#)
	    (writer(cons #2# (mappend #1# #3#))))))))
=> MONAD

#?(definstance(monad function)
    ((.return(x)
       `(lambda(#0=#:_)
	  (declare(ignore #0#))
	  ,x))
     (>>=(h f)
       `(lambda(#1=#:w)
	  (funcall (funcall ,f (funcall ,h #1#))
		   #1#)))))
=> MONAD

#?(definstance(monad state)
    ((.return(x)
       `(state(lambda(#0=#:s)
		(cons ,x #0#))))
     (>>=(h f)
       `(state(lambda(#0#)
		(destructuring-bind(#1=#:a . #2=#:new-state)(funcall ,h #0#)
		  (let((#3=#:g(funcall ,f #1#)))
		    (funcall #3# #2#))))))))
=> MONAD

;;;; MONAD+
#?(definstance(monad+ list)
    ((mzero()nil)
     (mplus(a b)`(append ,a ,b))))
=> MONAD+

;;;; MONAD-WRITAER
#?(definstance(monad-writer w (cons w) :constraint (monoid w))
    ((.write(cons)
       `(destructuring-bind(#0=#:a #1=#:w),cons
	  (cons #1# #0#)))
     (tell(w)
       `(cons ,w nil))
     (.listen(cons)
       `(destructuring-bind(#1# #0#),cons
	  (cons #1# (cons #0# #1#))))
     (pass(cons)
       `(destructuring-bind(#1# #0# . #2=#:f)cons
	  (cons (funcall #2# #1#)#0#)))))
=> MONAD-WRITER
