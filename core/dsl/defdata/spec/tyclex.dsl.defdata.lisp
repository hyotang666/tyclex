(defpackage :tyclex.dsl.defdata.spec
  (:use :cl :jingoh :tyclex.dsl.defdata))
(in-package :tyclex.dsl.defdata.spec)
(setup :tyclex.dsl.defdata)

(requirements-about DEFDATA
		    :around
		    (let((tyclex.objects.adt::*adts*(make-hash-table))
			 (tyclex.objects.adt::*adt-constructors*(make-hash-table)))
		      (call-body)))

;;;; Description:
; Define new Algebraic Data Types.
#?(defdata maybe(a)
    :nothing
    (just a))
=> MAYBE
,:before (fmakunbound 'just)
,:around (call-body)

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

;;;; Examples.
#?(defdata maybe (a)
    :nothing
    (just a))
:expanded-to
(eval-when(:compile-toplevel :load-toplevel :execute)
  (deftype maybe (a)
    (list 'or '(eql :nothing)
	  `(cons (eql ,'just)
		 ,(list 'cons a 'null))))
  (tyclex.objects.adt:add-adt 'maybe
			      :constructors '(:nothing just)
			      :lambda-list '(?A))
  (declaim(ftype(function(t)(maybe *))just))
  (defun just (a)
    (list 'just a))
  (tyclex.objects.adt:add-adt-constructor ':nothing
					 :type-of '(maybe *)
					 :arg-types '(eql :nothing))
  (tyclex.objects.adt:add-adt-constructor 'just
					 :type-of '(maybe *)
					 :arg-types '(?a))
  (trivia:defpattern just(&rest tyclex.dsl.defdata::args)
    `(list (eq ','just) ,@tyclex.dsl.defdata::args))
  'maybe)

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

;;;; Examples:
#?(data-type-of 0) => FIXNUM
#?(data-type-of 1.4) => SINGLE-FLOAT
#?(data-type-of 1.4d0) => DOUBLE-FLOAT
#?(data-type-of 1/3) => RATIO
#?(data-type-of #C(1 2)) => COMPLEX
#?(data-type-of #\a) => CHARACTER
#?(data-type-of "string") => STRING
#?(data-type-of #("vector")) => SIMPLE-VECTOR
#?(data-type-of nil) => NULL
#?(data-type-of '(1)) => CONS
#?(data-type-of '(1 . 2)) => CONS
#?(data-type-of (make-array '(2 2) :initial-contents '((1 2)(3 4)))) => SIMPLE-ARRAY
#?(data-type-of *package*) => PACKAGE
#?(data-type-of *readtable*) => READTABLE
#?(data-type-of *standard-output*) => STREAM
#?(data-type-of *random-state*) => RANDOM-STATE
#?(data-type-of 'car) => SYMBOL
#?(data-type-of :keyword) => SYMBOL
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

; result := fixnum

;;;; Affected By:
; TYCLEX.OBJECTS.ADT::*ADT-CONSTRUCTORS*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

