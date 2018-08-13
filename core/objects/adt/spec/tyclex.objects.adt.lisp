(defpackage :tyclex.objects.adt.spec
  (:use :cl :jingoh :tyclex.objects.adt))
(in-package :tyclex.objects.adt.spec)
(setup :tyclex.objects.adt)

(requirements-about ADT)
;;;; Description:
; Represents `ADT`.

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:
; none

(requirements-about MAKE-ADT)

;;;; Description:
; Constructor for `ADT` object.
#?(make-adt :constructors nil :lambda-list nil)
:be-the adt

#+syntax
(MAKE-ADT &key
	  ((:constructors #:constructors) (error "required"))
	  ((:lambda-list #:lambda-list) (error "required")))
; => result

;;;; Arguments and Values:

; constructors := List which contains constructor names, otherwise unspecified.
#?(make-adt :constructors "not list" :lambda-list nil) => unspecified

; lambda-list := List whicn contains variables, otherwise unspecified.
#?(make-adt :constructors nil :lambda-list "not list") => unspecified

; result := `ADT` object.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADT-CONSTRUCTORS)

;;;; Description:
; Reader for `ADT`'s `CONSTRUCTORS` slot.
#?(adt-constructors (make-adt :constructors '(yes) :lambda-list nil))
=> (YES)
,:test equal

#+syntax
(ADT-CONSTRUCTORS sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := `ADT` object, otherwise error.
#?(adt-constructors "not adt") :signals error
,:lazy t

; result := List which contains constructor names.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADT-LAMBDA-LIST)

;;;; Description:
; Reader for `ADT`'s `LAMBDA-LIST` slot.
#?(adt-lambda-list (make-adt :constructors nil :lambda-list '(yes)))
=> (YES)
,:test equal

#+syntax
(ADT-LAMBDA-LIST sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := `ADT` object, otherwise error.
#?(adt-lambda-list "not adt") :signals error
,:lazy t

; result := List which represents lambda list.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-ADT
		    :around (let((tyclex.objects.adt::*adts*(make-hash-table)))
			      (call-body)))

;;;; Description:
; Add name to lisp environment as adt.

#?(values (find-adt 'name nil)
	  (add-adt 'name :constructors nil :lambda-list nil)
	  (find-adt 'name))
:multiple-value-satisfies
#`(& (null $first)
     (typep $second 'adt)
     (typep $third 'adt))

#+syntax
(ADD-ADT name &rest args) ; => result

;;;; Arguments and Values:

; name := (and symbol (not (or keyword boolean))), otherwise error.
#?(add-adt "not symbol") :signals error

; args := Keyword parameters for `MAKE-ADT`, when not supported parameter comes, an error is signalsed.
#?(add-adt '#:dummy :not-supported-parameter nil) :signals error

; result := `ADT` object.

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADTS*

;;;; Side-Effects:
; Modify state of TYCLEX.OBJECTS.ADT::*ADTS*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FIND-ADT
		    :around (let((tyclex.objects.adt::*adts*(make-hash-table)))
			      (add-adt 'name :constructors nil :lambda-list nil)
			      (call-body)))

;;;; Description:
; Find adt named `NAME` from lisp environment.
#?(find-adt 'name) :be-the adt

#+syntax
(FIND-ADT name &optional (errorp t)) ; => result

;;;; Arguments and Values:

; name := (and symbol (not (or keyword boolean))), otherwise error.
#?(find-adt "not symbol") :signals error

; errorp := T, specify error or not when `ADT` is not found.
; The default it true.
#?(find-adt '#:not-exist) :signals error
#?(find-adt '#:not-exist nil) => NIL

; result := `ADT` object when found, otherwise error when `ERRORP` is true, otherwise `NIL`.

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADTS*

;;;; Side-Effects:
; Modify state of TYCLEX.OBJECTS.ADT::*ADTS*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REMOVE-ADT
		    :around (let((tyclex.objects.adt::*adts*(make-hash-table)))
			      (add-adt 'name :constructors nil :lambda-list nil)
			      (call-body)))

;;;; Description:
; Remove adt named `NAME` from lisp environment.
#?(remove-adt 'name) => T

#+syntax
(REMOVE-ADT name) ; => result

;;;; Arguments and Values:

; name := (and symbol (not (or keyword boolean))), otherwise error.
#?(remove-adt "not symbol") :signals error

; result := `BOOLEAN`, `T` when specified adt exists, otherwise `NIL`.
#?(remove-adt '#:not-exist) => NIL

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADTS*

;;;; Side-Effects:
; Modify state of TYCLEX.OBJECTS.ADT::*ADTS*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADT-MAKUNBOUND
		    :around (let((tyclex.objects.adt::*adts*(make-hash-table)))
			      (add-adt 'name :constructors nil :lambda-list nil)
			      (call-body)))

;;;; Description:
; Like `CL:MAKUNBOUND`, but for `ADT`.
; Responds to `CL:FMAKUNBOUND` for each `ADT-CONSTRUCTOR`.
#?(adt-makunbound 'name) => NAME

#+syntax
(ADT-MAKUNBOUND name) ; => result

;;;; Arguments and Values:

; name := SYMBOL otherwise error.
#?(adt-makunbound "not symbol") :signals error

; result := `NAME`

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADTS*

;;;; Side-Effects:
; Modify state of TYCLEX.OBJECTS.ADT::*ADTS*

;;;; Notes:
; Even is specified `ADT` is not found, return `NAME` successfully.
#?(adt-makunbound 'not-exist) => NOT-EXIST

;;;; Exceptional-Situations:

(requirements-about ADT-CONSTRUCTOR)

;;;; Description:
; Represents adt constructor.

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:
; none

(requirements-about MAKE-ADT-CONSTRUCTOR)

;;;; Description:
; Constructor for `ADT-CONSTRUCTOR`.
#?(make-adt-constructor :type-of nil :arg-types nil)
:be-the adt-constructor

#+syntax
(MAKE-ADT-CONSTRUCTOR &key
		      ((:type-of #:type-of) (error "required"))
		      ((:arg-types #:arg-types) (error "required")))
; => result

;;;; Arguments and Values:

; type-of := type specifier, which `ADT-CONSTRUCTOR` belongs.

; arg-types := List which contains type specifiers.

; result := `ADT-CONSTRUCTOR` object.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADT-CONSTRUCTOR-TYPE-OF)

;;;; Description:
; Reader for `ADT-CONSTRUCTOR`'s `TYPE-OF` slot.
#?(adt-constructor-type-of(make-adt-constructor :type-of 'yes :arg-types nil))
=> YES

#+syntax
(ADT-CONSTRUCTOR-TYPE-OF sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := `ADT-CONSTRUCTOR` object, otherwise error.
#?(adt-constructor-type-of "not adt-constructor") :signals error
,:lazy t

; result := type specifier, which `ADT-CONSTRUCTOR` belongs to.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADT-CONSTRUCTOR-ARG-TYPES)

;;;; Description:
; Reader for `ADT-CONSTRUCTOR`'s `ARG-TYPES` slot.
#?(adt-constructor-arg-types (make-adt-constructor :type-of nil :arg-types '(yes)))
=> (YES)
,:test equal

#+syntax
(ADT-CONSTRUCTOR-ARG-TYPES sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := `ADT-CONSTRUCTOR` object, otherwise error.
#?(adt-constructor-arg-types "not adt-constructor") :signals error
,:lazy t

; result := List which contains type specifiers.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-ADT-CONSTRUCTOR
		    :around(let((tyclex.objects.adt::*adt-constructors*(make-hash-table)))
			     (call-body)))

;;;; Description:
; Add name to lisp environment as `ADT-CONSTRUCTOR`.
#?(add-adt-constructor '#:dummy :type-of nil :arg-types nil)
:be-the adt-constructor

#+syntax
(ADD-ADT-CONSTRUCTOR name &rest args) ; => result

;;;; Arguments and Values:

; name := symbol, otherwise error.
#?(add-adt-constructor "not symbol" :type-of nil :arg-types nil) :signals error

; args := Keyword parameters for `MAKE-ADT-CONSTRUCTOR`.
; When not supported keyword parameter comes, an error is signaled.
#?(add-adt-constructor '#:dummy :type-of nil :not-supported-keyword '#:dummy) :signals error

; result := `ADT-CONSTRUCTOR` object.

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Side-Effects:
; Modify state of TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REMOVE-ADT-CONSTRUCTOR
		    :around(let((tyclex.objects.adt::*adt-constructors*(make-hash-table)))
			     (add-adt-constructor 'name :type-of nil :arg-types nil)
			     (call-body)))

;;;; Description:
; Remove `NAME` from lisp environment as `ADT-CONSTRUCTOR`.
#?(remove-adt-constructor 'name) => T

#+syntax
(REMOVE-ADT-CONSTRUCTOR name) ; => result

;;;; Arguments and Values:

; name := SYMBOL, otherwise error.
#?(remove-adt-constructor "not symbol") :signals error

; result := BOOLEAN, `T` when `ADT-CONSTRUCTOR` is found, otherwise `NIL`.
#?(remove-adt-constructor '#:not-exist) => NIL

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Side-Effects:
; Modify state of TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FIND-ADT-CONSTRUCTOR
		    :around(let((tyclex.objects.adt::*adt-constructors*(make-hash-table)))
			     (add-adt-constructor 'name :type-of nil :arg-types nil)
			     (call-body)))

;;;; Description:
; Find `NAME` from lisp environment as `ADT-CONSTRUCTOR`.
#?(find-adt-constructor 'name) :be-the ADT-CONSTRUCTOR

#+syntax
(FIND-ADT-CONSTRUCTOR name &optional (errorp t)) ; => result

;;;; Arguments and Values:

; name := symbol, otherwise error
#?(find-adt-constructor "not symbol") :signals error

; errorp := BOOLEAN, specify error or not whcn `ADT-CONSTRUCTOR` is not found.
; The default is true.
#?(find-adt-constructor '#:not-exist) :signals error
#?(find-adt-constructor '#:not-exist nil) => NIL

; result := `ADT-CONSTRUCTOR` object, otherwise error when `ERRORP` is true, otherwise `NIL`.

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Side-Effects:
; Modify state of TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADT-CONSTRUCTOR-BOUNDP
		    :around(let((tyclex.objects.adt::*adt-constructors*(make-hash-table)))
			     (add-adt-constructor 'name :type-of nil :arg-types nil)
			     (call-body)))

;;;; Description:
; Return generalized boolean when symbol bound as `ADT-CONSTRUCTOR`.
#?(adt-constructor-boundp 'name) :satisfies identity

#+syntax
(ADT-CONSTRUCTOR-BOUNDP symbol) ; => result

;;;; Arguments and Values:

; symbol := symbol, otherwise error.
#?(adt-constructor-boundp "not symbol") :signals error

; result := Generalized boolean.

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Side-Effects:
; Modify state of TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADT-TYPE-SPECIFIER-P)

;;;; Description:
;; setup
#?(add-adt 'maybe :lambda-list '(a) :constructors '(just nothing))
:be-the adt
,:before (remove-adt 'maybe)

;; example
#?(adt-type-specifier-p 'maybe) => T
#?(adt-type-specifier-p '(maybe *)) => T
#?(adt-type-specifier-p '(maybe * *)) => NIL
#?(adt-type-specifier-p 'not-exist) => NIL
#?(adt-type-specifier-p "not adt type specifier") => NIL

#+syntax
(ADT-TYPE-SPECIFIER-P thing) ; => result

;;;; Arguments and Values:

; thing := T

; result := BOOLEAN

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; `ADT-TYPE-SPECIFIER-P` checks only first elt is ADT, and length of lambda-list.
#?(adt-type-specifier-p '(maybe "not type specifier")) => unspecified

; Can handle curried simulated adt-type-specifier.
#?(adt-type-specifier-p '((maybe) *)) => T

;;;; Exceptional-Situations:

