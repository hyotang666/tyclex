(defpackage :tyclex.objects.instance.spec
  (:use :cl :jingoh :tyclex.objects.instance))
(in-package :tyclex.objects.instance.spec)
(setup :tyclex.objects.instance)

(requirements-about INSTANCE)

;;;; Description:
; Represents interface instances.

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:

(requirements-about MAKE-TYPE-CLASS-INSTANCE)

;;;; Description:

#+syntax
(MAKE-TYPE-CLASS-INSTANCE &key
			  ((:signature #:signature) (error "signature is required."))
			  ((:definitions #:definitions) (error "definitions is required."))
			  ((:types #:types) (error "types is required."))
			  ((:constraints #:constraints) (error "constraints is required.")))
; => result

;;;; Arguments and Values:

; signature := list as type signature, otherwise unspecified.
#?(make-type-class-instance :signature "not-list" :definitions nil :types nil :constraints nil)
=> unspecified

; definitions := list as macrolet first argument, otherwise unspecified.
#?(make-type-class-instance :signature nil :definitions "not-list" :types nil :constraints nil)
=> unspecified

; types := list as instance type contexts, otherwise unspecified.
#?(make-type-class-instance :signature nil :definitions nil :types "not-list" :constraints nil)
=> unspecified

; constraints := list as instance vars's constraints, otherwise unspecified.
#?(make-type-class-instance :signature nil :definitions nil :types nil :constraints "not-list")
=> unspecified

; result := instance object.
#?(make-type-class-instance :signature nil :definitions nil :types nil :constraints nil)
:be-the instance

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about instance-signature)

;;;; Description:
; reader for instance's signature slot.

#+syntax
(instance-signature sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := instance object, otherwise error.
#?(instance-signature (make-type-class-instance :signature '(yes) :definitions nil :types nil :constraints nil))
=> (YES)
,:test equal
#?(instance-signature '#:not-instance)
:signals error
,:lazy t

; result := list as type signature.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about instance-definitions)

;;;; Description:
; reader for instance's definitions slot.
#?(instance-definitions(make-type-class-instance :signature nil :definitions '(yes) :types nil :constraints nil))
=> (YES)
,:test equal

#+syntax
(instance-definitions sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := instance object, otherwise error.
#?(instance-definitions '#:not-instance) :signals (or error
				       warning) ; for sbcl

; result := list as macrolet first argment.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about instance-types)

;;;; Description:
; Reader for `instance`'s `TYPES` slot.
#?(instance-types(make-type-class-instance :signature nil :definitions nil :types '(yes) :constraints nil))
=> (YES)
,:test equal

#+syntax
(instance-types sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := `instance` object, otherwise error.
#?(instance-types '#:not-instance) :signals (or error
				   warning) ; for sbcl

; result := List as specified types for instance.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about instance-constraints)

;;;; Description:
; Reader for `instance`'s `CONSTRAINTS` slot.
#?(instance-constraints (make-type-class-instance :signature nil :definitions nil :types nil :constraints '(yes)))
=> (YES)
,:test equal

#+syntax
(instance-constraints sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := `instance` object, otherwise error.
#?(instance-constraints '#:not-instance) :signals (or error
					 warning) ; for sbcl

; result := List as constraints for instance parameter.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

