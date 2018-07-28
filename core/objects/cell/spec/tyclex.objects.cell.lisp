(defpackage :tyclex.objects.cell.spec
  (:use :cl :jingoh :tyclex.objects.cell))
(in-package :tyclex.objects.cell.spec)
(setup :tyclex.objects.cell)

(requirements-about CELL)

;;;; Description:
; Represents instance table cell.

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:

(requirements-about MAKE-CELL)

;;;; Description:

#+syntax
(MAKE-CELL &key
	   ((:signature #:signature) (error "signature is required."))
	   ((:instances #:instances) (error "instances is required."))
	   ((:types #:types) (error "types is required."))
	   ((:constraints #:constraints) (error "constraints is required.")))
; => result

;;;; Arguments and Values:

; signature := list as type signature, otherwise unspecified.
#?(make-cell :signature "not-list" :instances nil :types nil :constraints nil)
=> unspecified

; instances := list as macrolet first argument, otherwise unspecified.
#?(make-cell :signature nil :instances "not-list" :types nil :constraints nil)
=> unspecified

; types := list as instance type contexts, otherwise unspecified.
#?(make-cell :signature nil :instances nil :types "not-list" :constraints nil)
=> unspecified

; constraints := list as instance vars's constraints, otherwise unspecified.
#?(make-cell :signature nil :instances nil :types nil :constraints "not-list")
=> unspecified

; result := CELL object.
#?(make-cell :signature nil :instances nil :types nil :constraints nil)
:be-the cell

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SIGNATURE)

;;;; Description:
; reader for cell's signature slot.

#+syntax
(SIGNATURE sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := cell object, otherwise error.
#?(signature (make-cell :signature '(yes) :instances nil :types nil :constraints nil))
=> (YES)
,:test equal
#?(signature '#:not-cell)
:signals (or error
	     warning) ; for sbcl

; result := list as type signature.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about INSTANCES)

;;;; Description:
; reader for cell's instances slot.
#?(instances(make-cell :signature nil :instances '(yes) :types nil :constraints nil))
=> (YES)
,:test equal

#+syntax
(INSTANCES sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := cell object, otherwise error.
#?(instances '#:not-cell) :signals (or error
				       warning) ; for sbcl

; result := list as macrolet first argment.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about TYPES)

;;;; Description:
; Reader for `CELL`'s `TYPES` slot.
#?(types(make-cell :signature nil :instances nil :types '(yes) :constraints nil))
=> (YES)
,:test equal

#+syntax
(TYPES sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := `CELL` object, otherwise error.
#?(types '#:not-cell) :signals (or error
				   warning) ; for sbcl

; result := List as specified types for instance.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CONSTRAINTS)

;;;; Description:
; Reader for `CELL`'s `CONSTRAINTS` slot.
#?(constraints (make-cell :signature nil :instances nil :types nil :constraints '(yes)))
=> (YES)
,:test equal

#+syntax
(CONSTRAINTS sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := `CELL` object, otherwise error.
#?(constraints '#:not-cell) :signals (or error
					 warning) ; for sbcl

; result := List as constraints for instance parameter.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

