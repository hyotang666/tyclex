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

;;; ZIP LIST.
#?(define-newtype zip-list(&optional a)
    (declare(ignore a))
    'list)
=> ZIP-LIST
,:before (fmakunbound 'zip-list)

#?(<$> (curried-function:section + _ _)
       (zip-list '(1 2 3))
       (zip-list '(100 100 100)))
=> (101 102 103)
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(<$> (curried-function:section max _ _)
       (zip-list '(1 2 3 4 5 3))
       (zip-list '(5 3 1 2)))
=> (5 3 3 4)
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t

#?(<$> (curried-function:section list _ _ _)
       (zip-list (coerce "dog" 'list))
       (zip-list (coerce "cat" 'list))
       (zip-list (coerce "rat" 'list)))
=> ((#\d #\c #\r)(#\o #\a #\a)(#\g #\t #\t))
,:test equal
,:around(let(vs-haskell::*subtype-verbose* vs-haskell::*expand-verbose*)
	  (call-body))
,:lazy t
,:ignore-signals warning ; <--- series signals.
,:stream nil

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

