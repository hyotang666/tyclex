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
      (not (/== x y)))
    (:default /== (x y)
      (not (== x y))))
=> EQ
,:before (mapc #'fmakunbound '(== /==))

#?(defdata traffic-light()
     red yellow green)
=> TRAFFIC-LIGHT

#?(definstance == ((a traffic-light)(b traffic-light))
    (trivia:match*(a b)
      ((red red)T)
      ((green green)T)
      ((yellow yellow)T)
      ((_ _) nil)))
=> ==

#?(== red red) => T
#?(== green green) => T
#?(== yellow yellow) => T
#?(== red green) => NIL
#?(== red yellow) => NIL
#?(== green red) => NIL
#?(== green yellow) => NIL

#?(== red 'not-traffic-light) :signals error
,:lazy t

#?(definstance == ((a (maybe *))(b (maybe *)))
    (trivia:match*(a b)
      ((nothing nothing)T)
      (((just x)(just y))(== x y))
      ((_ _)nil)))
=> ==

#?(== nothing nothing) => T
#?(== (just red)(just red)) => T
#?(== nothing (just red)) => NIL
#?(== (just red)(just green)) => NIL

#?(define-type-class(yes-no a)()
    ((yes-no(a)boolean)))
=> YES-NO
,:before (mapc #'fmakunbound '(yes-no))

#?(definstance yes-no ((a fixnum))
    (not(zerop a)))
=> YES-NO

#?(definstance yes-no ((a list))
    (not(null a)))
=> YES-NO

#?(definstance yes-no ((a symbol))
    (not(null a)))
=> YES-NO

#?(definstance yes-no ((a (maybe *)))
    (unless(eq nothing a)
      T))
=> YES-NO

#?(definstance yes-no ((a traffic-light))
    (unless(eq red a)
      T))
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
      (data-order x)))
=> ORD
,:before (mapc #'fmakunbound '(ord))
#?(definstance ord ((a number))
     a)
=> ORD
#?(definstance ord ((a character))
    (char-code a))
=> ORD

#?(define-type-class(compare a)(ord)
    ((compare(a a)(member :eq :lt :gt))
     (lt(a a)boolean)
     (gt(a a)boolean)
     (lte(a a)boolean)
     (gte(a a)boolean))
    (:default compare (x y)
      (compare (ord x)(ord y)))
    (:default lt (x y)
      (< (ord x)(ord y)))
    (:default gt (x y)
      (> (ord x)(ord y)))
    (:default lte (x y)
      (<= (ord x)(ord y)))
    (:default gte (x y)
      (>= (ord x)(ord y))))
=> COMPARE
,:before (mapc #'fmakunbound '(compare lt gt lte gte))

#?(definstance compare((a number)(b number))
    (cond
      ((= a b):eq)
      ((< a b):lt)
      (t :gt)))
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
      (cadr(introspect-environment:typexpand-1 x)))
    (:default max-bound(x)
      (car(last(introspect-environment:typexpand-1 x)))))
=> BOUNDED
,:before (mapc #'fmakunbound '(min-bound max-bound))

#?(define-type-class(enum a)()
    ((succ(a)a)
     (pred(a)a))
    (:default succ (x)
      (cadr (nth (1+(data-order x))
                 (cdr(introspect-environment:typexpand-1(data-type-of x))))))
    (:default pred (x)
      (cadr (nth (1-(data-order x))
                 (cdr(introspect-environment:typexpand-1(data-type-of x)))))))
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

#?(definstance fmap ((f function)(m (maybe *)))
    (trivia:ematch m
      ((just x)(just(funcall f x)))
      (nothing nothing)))
=> FMAP

#?(fmap #'1+ nothing) => NOTHING
#?(fmap #'1+ (just 0)) :be-the (maybe (eql 1))
#?(fmap #'identity (just 0)) :equivalents (identity (just 0))
,:test equal

#?(definstance fmap ((f (function(a)b))(action (io a)))
    (action result <- action
            (.return (funcall f result))))
=> FMAP

#?(fmap #'reverse (get-line))
:satisfies #`(with-input-from-string(*standard-input* "hoge")
	       (& (functionp $result)
		  (typep $result 'io-action)
		  (equal "egoh" (funcall $result))))

#?(definstance fmap ((f function)(g function))
     (alexandria:compose f g))
=> FMAP
#?(fmap #'1+ #'1+)
:satisfies #`(& (functionp $result)
		(eql 3 (funcall $result 1)))

#?(definstance fmap ((f function)(l list))
    (mapcar f l))
=> FMAP
#?(fmap #'1+ '(1 2 3)) => (2 3 4)
,:test equal
#?(fmap #'1+ nil) => NIL
#?(fmap #'identity '(1 2 3)) :equivalents (identity '(1 2 3))
#?(fmap #'identity nil) :equivalents (identity nil)

#?(defdata counter-maybe(a)
    counter-nothing
    (counter-just fixnum a))
=> COUNTER-MAYBE

#?(definstance fmap ((f function)(cm (counter-maybe *)))
    (trivia:ematch cm
      (counter-nothing counter-nothing)
      ((counter-just counter x)(counter-just (1+ counter)(funcall f x)))))
=> FMAP

#?(fmap #'1+ (counter-just 0 1))
:satisfies #`(not (equal $result)
		  (counter-just 0 1))
