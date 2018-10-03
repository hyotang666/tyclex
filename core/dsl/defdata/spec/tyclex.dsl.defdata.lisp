(defpackage :tyclex.dsl.defdata.spec
  (:use :cl :jingoh :tyclex.dsl.defdata))
(in-package :tyclex.dsl.defdata.spec)
(setup :tyclex.dsl.defdata)

(requirements-about DEFDATA)

;;;; Description:
; Define new Algebraic Data Types.
#?(defdata maybe(a)
    :nothing
    (just a))
=> MAYBE
,:before (tyclex.objects.adt:remove-adt 'maybe)

#+syntax
(DEFDATA name&option lambda-list &rest constructor*) ; => result

;;;; Arguments and Values:

; name&option := [ name | (name :deriving deriving+) ]
; name := symbol, otherwise error.
#?(defdata "not symbol" (a)
    :nothing
    (just a))
:signals error

; deriving := type-class-name, otherwise error
#?(defdata(maybe :deriving not-type-class-name)(a)
    :nothing
    (just a))
:signals error

; lambda-list := (type-variable*)
; type-variable := symbol, otherwise error.
#?(defdata maybe ("not symbol")
    :nothing
    (just a))
:signals error
; lambda-list-keywords are invalid as type-variable.
#?(defdata maybe (&optional a)
    :nothing
    (just a))
:signals error

; constructor := [ [ nullary-constructor
;                  | standard-constructor ]+
;                | structural-constructor+ ]
; nullary-constructor := symbol, otherwise error.
#?(defdata maybe (a)
    "not symbol"
    (just a))
:signals error

; standard-constructor := (constructor-name type-pattern+)
; constructor-name := symbol, otherwise error.
#?(defdata maybe (a)
    :nothing
    ("not symbol" a))
:signals error

; type-pattern := [ type-variable | type-specifier ], otherwise unspecified.
#?(defdata maybe (a)
    :nothing
    (just a not-type-specifier))
=> unspecified

; structural-constructor := (constructor-name slot-reader*)
; slot-reader := (reader-name init-form slot-option*)
; reader-name := symbol, otherwise error.
#?(defdata dummy ()
    (construct ("not symbol" nil :type null)))
:signals error

; init-form := t, evaluated when initial value is not specified.

; slot-option := type-option? reader-option?
; type-option := :type type-specifier
; reader-option := :read-only boolean

; result := `NAME`.

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADTS*
; TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Side-Effects:
; Modify TYCLEX.OBJECTS.ADT::*ADTS*
; Modify TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DATA-ORDER)

;;;; Description:
; Default instance for type-class ORD
#?(data-order :nothing) => 0
#?(data-order (just 0)) => 1

#+syntax
(DATA-ORDER thing) ; => result

;;;; Arguments and Values:

; thing := adt value, otherwise error.
#?(data-order "not adt value") :signals error
,:lazy t

; result := fixnum

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

