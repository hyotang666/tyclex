(in-package :vs-haskell.spec)
(named-readtables:in-readtable jingoh.reader:syntax)

(requirements-about DEFINE-NEWTYPE)

;;;; Description:

#+syntax
(DEFINE-NEWTYPE name lambda-list &body body) ; => result

;;;; Arguments and Values:

; name := 

; lambda-list := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

#?(define-newtype char-list(&optional char)
    (declare(ignore char))
    'list)
=> CHAR-LIST
,:before (fmakunbound 'char-list)

#?(char-list (coerce "this" 'list))
=> (#\t #\h #\i #\s)
,:test equal

#?(equal (char-list (coerce "bunny" 'list))
	 (char-list (coerce "bunny" 'list)))
=> T

#?(equal (char-list (coerce "benny" 'list))
	 (char-list (coerce "oister" 'list)))
=> NIL

#?(char-list "Not list") :signals ERROR
,:ignore-signals warning
,:lazy t

#?(define-newtype pair(&optional b a)
    `(cons ,a ,b))
=> PAIR
,:before (fmakunbound 'pair)

#?(definstance(functor pair)
    ((fmap(f pair)
       `(trivia:match ,pair
          ((cons x y)(pair (cons (funcall ,f x)y)))))))
=> FUNCTOR

#?(fmap (curried-function:section * _ 100)
	(pair '(2 . 3)))
=> (200 . 3)
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(fmap #'reverse (pair '("london calling" . 3)))
=> ("gnillac nodnol" . 3)
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

(requirements-about DENEW)

;;;; Description:
; Return the 3rd object in a list or NIL if there is no 3rd object.

#+syntax
(DENEW list) ; => result

;;;; Arguments and Values:

; list := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

