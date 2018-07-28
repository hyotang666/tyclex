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

(requirements-about IO)

;;;; Description:
; Shorthand macro for `CL:THE` form.
#?(io "dummy")
:expanded-to
(THE (IO *) "dummy")

#+syntax
(IO tyclex.newtype::arg) ; => result

;;;; Arguments and Values:

; arg := S-Expression which generates type of `IO`.

; result := return value for `ARG`.

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
#?(make-action :type '(io "dummy")) :be-the action

#+syntax
(MAKE-ACTION &key ((:type #:type) (error "required"))) ; => result

;;;; Arguments and Values:

; type := io type specifier. i.e. (cons (eql io)(cons * null)), otherwise signals condition.
#?(make-action :type "not io type specifier")
:signals condition

; result := action object.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ACTION-TYPE)

;;;; Description:
; Reader for `ACTION`'s `TYPE` slot.
#?(action-type(make-action :type '(io yes)))
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

(requirements-about ADD-IO
		    :around(let((tyclex.objects.io-action::*io-functions*(make-hash-table)))
			     (call-body)))

;;;; Description:
; Adding name to lisp environemnt as io.
#?(add-io '#:name :type '(io "dummy")) :be-the action

#+syntax
(ADD-IO name &rest args) ; => result

;;;; Arguments and Values:

; name := (and symbol (not (or keyword boolean))), otherwise error.
#?(add-io "invalid type") :signals error

; args := keyword parameters for `MAKE-ACTION`.
; When not supported keyword parameter comes, an error is signaled.
#?(add-io '#:dummy :not-supported-keyword "dummy") :signals error

; result := action

;;;; Affected By:
; TYCLEX.OBJECTS.IO-ACTION::*IO-FUNCTIONS*

;;;; Side-Effects:
; Modify state of TYCLEX.OBJECTS.IO-ACTION::*IO-FUNCTIONS*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REMOVE-IO
		    :around(let((tyclex.objects.io-action::*io-functions*(make-hash-table)))
			     (add-io 'io-name :type '(io "dummy"))
			     (call-body)))

;;;; Description:
; Remove io from lisp environment.
#?(remove-io 'io-name) => T

#+syntax
(REMOVE-IO name) ; => result

;;;; Arguments and Values:

; name := IO name, i.e. (and symbol (not (or keyword boolean))), otherwise error.
#?(move-io "not io name") :signals error

; result := BOOLEAN, T when io exists, otherwise nil.
#?(values (remove-io 'io-name)
	  (remove-io 'io-name))
:values (T NIL)

;;;; Affected By:
; TYCLEX.OBJECTS.IO-ACTION::*IO-FUNCTIONS*

;;;; Side-Effects:
; Modify state of TYCLEX.OBJECTS.IO-ACTION::*IO-FUNCTIONS*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about IO-BOUNDP
		    :around(let((tyclex.objects.io-action::*io-functions*(make-hash-table)))
			     (add-io 'io-name :type '(io "dummy"))
			     (call-body)))

;;;; Description:
; Return t when arg names io.
#?(io-boundp 'io-name) :satisfies identity
#?(io-boundp '#:not-io-name) => NIL

#+syntax
(IO-BOUNDP symbol) ; => result

;;;; Arguments and Values:

; symbol := symbol, otherwise error.
#?(io-boundp "not symbol") :signals error

; result := BOOLEAN

;;;; Affected By:
; TYCLEX.OBJECTS.IO-ACTION::*IO-FUNCTIONS*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about IO-MAKUNBOUND
		    :around(let((tyclex.objects.io-action::*io-functions*(make-hash-table)))
			     (add-io 'io-name :type '(io "dummy"))
			     (call-body)))

;;;; Description:
; Unbound io from symbol.
#?(io-makunbound 'io-name) => IO-NAME

#+syntax
(IO-MAKUNBOUND symbol) ; => result

;;;; Arguments and Values:

; symbol := symbol, otherwise error.
#?(io-makunbound "not symbol") :signals error

; result := symbol

;;;; Affected By:
; TYCLEX.OBJECTS.IO-ACTION::*IO-FUNCTIONS*

;;;; Side-Effects:
; Modify state of TYCLEX.OBJECTS.IO-ACTION::*IO-FUNCTIONS*

;;;; Notes:

;;;; Exceptional-Situations:

