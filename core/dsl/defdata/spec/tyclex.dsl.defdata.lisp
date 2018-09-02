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

(requirements-about ADT-VALUE-P)

;;;; Description:
; Return true when `THING` is adt-value.
#?(adt-value-p :nothing) :satisfies identity
#?(adt-value-p (just 0)) :satisfies identity
#?(adt-value-p 0) => NIL

#+syntax
(ADT-VALUE-P thing) ; => result

;;;; Arguments and Values:

; thing := T

; result := (values tyclex.unifier:environment tyclex.objects.adt:adt-constructor) when successed.
#?(adt-value-p :nothing)
:multiple-value-satisfies
#`(& (typep $first 'tyclex.unifier:environment)
     (typep $second 'tyclex.objects.adt:adt-constructor))

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DATA-TYPE-OF)

;;;; Description:
; Compute type of data.
#?(data-type-of (just 0)) => (MAYBE FIXNUM)
,:test equal

#+syntax
(DATA-TYPE-OF thing) ; => result

;;;; Arguments and Values:

; thing := T

; result := type-pattern

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples
#?(data-type-of #'car) => (FUNCTION (LIST) (VALUES T &OPTIONAL))
,:test equal
#?(data-type-of #'documentation) => (FUNCTION * *)
,:test equal
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

(requirements-about CLASS-NAME-OF)

;;;; Description:
; Wrapper for `(CLASS-NAME(CLASS-OF ...))`.

#+syntax
(CLASS-NAME-OF thing) ; => result

;;;; Arguments and Values:

; thing := T

; result := SYMBOL

;;;; Affected By:
; Lisp environment.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
#?(class-name-of 0) => FIXNUM
#?(class-name-of (1+ most-positive-fixnum)) => BIGNUM
#?(class-name-of 1.4) => SINGLE-FLOAT
#?(class-name-of 1.4d0) => DOUBLE-FLOAT
#?(class-name-of 1/3) => RATIO
#?(class-name-of #C(1 2)) => COMPLEX
#?(class-name-of #\a) => CHARACTER
#?(class-name-of "string") => STRING
#?(class-name-of #("vector")) => VECTOR
#?(class-name-of nil) => NULL
#?(class-name-of '(1)) => CONS
#?(class-name-of '(1 . 2)) => CONS
#?(class-name-of (make-array '(2 2) :initial-contents '((1 2)(3 4)))) => ARRAY
#?(class-name-of *package*) => PACKAGE
#?(class-name-of *readtable*) => READTABLE
#?(class-name-of *standard-output*) => STREAM
#?(class-name-of *standard-input*) => STREAM
#?(class-name-of *debug-io*) => STREAM
#?(class-name-of *random-state*) => RANDOM-STATE
#?(class-name-of 'car) => SYMBOL
#?(class-name-of :keyword) => SYMBOL
#?(class-name-of #'car) => FUNCTION
#?(class-name-of #'documentation) => STANDARD-GENERIC-FUNCTION

