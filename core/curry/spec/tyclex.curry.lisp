(defpackage :tyclex.curry.spec
  (:use :cl :jingoh :tyclex.curry))
(in-package :tyclex.curry.spec)
(setup :tyclex.curry)

(requirements-about CURRY)
;;;; Description:
; CURRYied function

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:
; none

(requirements-about CURRY)

;;;; Description:
; Currying function and apply argment patially.
#?(curry + _ _)
:satisfies #`(& ; result is curried function.
	       (typep $result 'curry)
	       ; When one argument applied, curried function returned.
	       (typep (funcall $result 1) 'curry)
	       ; When all of arguments are applied, body form is evaluated.
	       (= 3 (funcall (funcall $result 1)
			     2))
	       ; We can apply arguments at once.
	       (= 3 (funcall $result 1 2))
	       )

#+syntax
(CURRY op &rest args) ; => result

;;;; Arguments and Values:

; op := Function name, otherwise error
#?(curry #'+ _ _) :signals error
; Lambda form is also function name.
#?(curry (lambda(x y)(+ x y)) _ _)
:satisfies #`(& ; result is curried function.
	       (typep $result 'curry)
	       ; When one argument applied, curried function returned.
	       (typep (funcall $result 1) 'curry)
	       ; When all of arguments are applied, body form is evaluated.
	       (= 3 (funcall (funcall $result 1)
			     2))
	       ; We can apply arguments at once.
	       (= 3 (funcall $result 1 2))
	       )

; args := Arguments for `OP`, when there is `_`, result `CURRY`ied function awaits it.
#?(curry + 1 2 _)
:satisfies #`(& (typep $result 'curry)
		(= 6 (funcall $result 3)))

; result := `CURRY`ied function.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FUNCTION-TYPE-OF)

;;;; Description:
; Alter `INTROSPECT-ENVIRONMENT:FUNCTION-TYPE`.

#+syntax
(FUNCTION-TYPE-OF name) ; => result

;;;; Arguments and Values:

; name := Symbol, otherwise error.
#?(function-type-of "not symbol")
:signals error
; When NAME is not set, return NIL.
#?(function-type-of '#:dummy) => NIL

; result := ftype function form, when `NAME` is `FUNCTION-TYPE`ed.

;;;; Affected By:
; Lisp environment.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FUNCTION-TYPE)

;;;; Description:
; Alter DECLAIM FTYPE.
#?(function-type dummy (fixnum) float)
:expanded-to (progn (declaim(ftype(function(fixnum)float)dummy))
		    (setf (get 'dummy 'ftype)'(function(fixnum)float))
		    'dummy)

#+syntax
(FUNCTION-TYPE name args return) ; => result

;;;; Arguments and Values:

; name := Symbol, otherwise error.
#?(function-type "not symbol" * *) :signals error
,:lazy t

; args := [ * | list ], otherwise error.
#?(function-type dummy "not valid" *) :signals error
; For ftype form second element.

; return := type-specifier, otherwise unspecified.
#?(function-type dummy * "not type specifier") :signals error
#?(function-type dummy * not-type-specifier) => unspecified

; result := NAME

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify lisp environment.

;;;; Notes:

;;;; Exceptional-Situations:

