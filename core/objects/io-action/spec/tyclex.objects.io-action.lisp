(defpackage :tyclex.objects.io-action.spec
  (:use :cl :jingoh :tyclex.objects.io-action))
(in-package :tyclex.objects.io-action.spec)
(setup :tyclex.objects.io-action)

(requirements-about IO-ACTION)
;;;; Description:
; Funcallable object especially with side effect.

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:
; none

(requirements-about IO-TYPE)

;;;; Description:
; Reader for `IO-ACTION`'s `TYPE` slot.
#?(io-type(make-instance 'io-action :type '(io yes) :instance (constantly "dummy")))
=> (IO YES)
,:test equal

#+syntax
(IO-TYPE arg) ; => result

;;;; Argument Precedence Order:
; sb-pcl::object

;;;; Method signature:
#+signature(IO-TYPE (IO-ACTION IO-ACTION))

;;;; Arguments and Values:

; arg := io-action, otherwise error.
#?(io-type "not-io-action") :signals error

; result := io type specifier. i.e. (cons (eql io)(cons t null))

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ACTION)
;;;; Description:
; Represents action meta infomation.

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:
; none

(requirements-about MAKE-ACTION)

;;;; Description:
; Constructor for `ACTION`.
#?(make-action :type '(io "dummy"):body '("dummy") :lambda-list nil)
:be-the action

#+syntax
(MAKE-ACTION &key
	     ((:type #:type)
	      (error 'tyclex.conditions:slot-uninitialized :name 'type))
	     ((:body #:body)
	      (error 'tyclex.conditions:slot-uninitialized :name 'body))
	     ((:lambda-list #:lambda-list)
	      (error 'tyclex.conditions:slot-uninitialized :name 'lambda-list)))
; => result

;;;; Arguments and Values:

; type := io type specifier. i.e. (cons (eql io)(cons * null)), otherwise unspecified.
#?(make-action :type "not io type specifier") => unspecified

; body := (S-Expression+)

; lambda-list := lambda list

; result := an `ACTION` object.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ACTION-TYPE)

;;;; Description:
; Reader for `ACTION`'s `TYPE` slot.
#?(action-type(make-action :type '(io yes):body '("dummy"):lambda-list nil))
=> (IO YES)
,:test equal

#+syntax
(ACTION-TYPE sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := action, otherwise error.
#?(action-type "not-action") :signals error
,:lazy t ; for sbcl

; result := io type specifier.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-IO)

;;;; Description:
; Adding name to lisp environemnt as io.
#?(add-io 'name :type '(io "dummy") :body '("dummy") :lambda-list nil)
:be-the action

#+syntax
(ADD-IO name &rest args) ; => result

;;;; Arguments and Values:

; name := (and symbol (not (or keyword boolean))), otherwise condition.
#?(add-io "invalid type") :signals condition

; args := keyword parameters for `MAKE-ACTION`.
; When not supported keyword parameter comes, an condition is signaled.
#?(add-io '#:dummy :not-supported-keyword "dummy") :signals condition

; result := action

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REMOVE-IO)

;;;; Description:
; Remove io from lisp environment.
#?(remove-io 'name) => T

#+syntax
(REMOVE-IO name) ; => result

;;;; Arguments and Values:

; name := IO name, when `NAME` exists as io, return true, otherwise false.
#?(remove-io "not io name") => NIL

; result := BOOLEAN, T when io exists, otherwise nil.
#?(values (remove-io 'io-name)
	  (progn (add-io 'io-name :type '(io "dummy"):body '("dummy"):lambda-list nil)
		 (remove-io 'io-name)))
:values (NIL T)

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about IO-BOUNDP)

;;;; Description:
; Return t when arg names io.
#?(progn (add-io 'io-name :type '(io "dummy") :body '("dummy") :lambda-list nil)
	 (io-boundp 'io-name))
:satisfies identity
,:after (remove-io 'io-name)
#?(io-boundp '#:not-io-name) => NIL

#+syntax
(IO-BOUNDP symbol) ; => result

;;;; Arguments and Values:

; symbol := symbol, otherwise condition.
#?(io-boundp "not symbol") :signals condition

; result := BOOLEAN

;;;; Affected By:

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about IO-MAKUNBOUND)

;;;; Description:
; Unbound io from symbol.
#?(io-makunbound 'io-name) => IO-NAME

#+syntax
(IO-MAKUNBOUND symbol) ; => result

;;;; Arguments and Values:

; symbol := symbol, otherwise condition.
#?(io-makunbound "not symbol") :signals condition

; result := symbol

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

