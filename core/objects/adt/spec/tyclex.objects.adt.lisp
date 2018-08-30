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
; Creates a new `ADT` object.
#?(make-adt :constructors nil :lambda-list nil)
:be-the adt

#+syntax
(MAKE-ADT &key
	  ((:constructors #:constructors)
	   (error 'tyclex.conditions:slot-uninitialized :name 'constructors))
	  ((:lambda-list #:lambda-list)
	   (error 'tyclex.conditions:slot-uninitialized :name 'lambda-list)))
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

(requirements-about GET-ADT)

;;;; Description:
; Finds an `ADT` associated with `NAME`.
#?(get-adt 'name) :be-the adt
,:before (add-adt 'name :constructors nil :lambda-list nil)
,:after (remove-adt 'name)

#+syntax
(GET-ADT name &optional (errorp t)) ; => result

; `SETF` of `GET-ADT` may be used to associate a new `ADT` with an existing `NAME` aleady on the system, or to create a new association if not exists.
#?(setf (get-adt 'name)
	(make-adt :constructors nil :lambda-list nil))
:be-the adt
,:after (remove-adt 'name)

#+setf
(SETF (GET-ADT NAME &OPTIONAL ERRORP) NEW-VALUE) ; => new-value

;;;; Arguments and Values:

; name := T, but when combined with `SETF` it must be symbol.
#?(setf (get-adt "not symbol")
	(make-adt :constructors nil :lambda-list nil))
:signals error

; errorp := generalized boolean. The default is true.
; Specify signals an error when `ADT` is not found.
#?(get-adt '#:not-exist) :signals MISSING-ADT
#?(get-adt '#:not-exist nil) => NIL

; result := an `ADT` when found, otherwise `NIL` when `ERRORP` is specified `NIL`, otherwise an error of type MISSING-ADT.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; In case of combined with `SETF`, warning may be signaled when specified `NAME` is already exists.
#?(progn (setf (get-adt 'name)
	       (make-adt :constructors nil :lambda-list nil))
	 (setf (get-adt 'name)
	       (make-adt :constructors nil :lambda-list nil)))
:signals TYCLEX.CONDITIONS:REDEFINITION-WARNING

(requirements-about ADD-ADT)

;;;; Description:
; Add name to lisp environment as adt.

#?(values (get-adt 'name nil)
	  (add-adt 'name :constructors nil :lambda-list nil)
	  (get-adt 'name))
:multiple-value-satisfies
#`(& (null $first)
     (typep $second 'adt)
     (typep $third 'adt))
,:before (remove-adt 'name)

#+syntax
(ADD-ADT name &rest args) ; => result

;;;; Arguments and Values:

; name := (and symbol (not (or keyword boolean))), otherwise error.
#?(add-adt "not symbol") :signals error

; args := Keyword parameters for `MAKE-ADT`, when not supported parameter comes, an error is signalsed.
#?(add-adt '#:dummy :not-supported-parameter nil) :signals error

; result := `ADT` object.

;;;; Affected By:

;;;; Side-Effects:
; May 

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REMOVE-ADT)

;;;; Description:
; Remove adt named `NAME` from lisp environment.
#?(remove-adt 'name) => T

#+syntax
(REMOVE-ADT name) ; => result

;;;; Arguments and Values:

; name := T

; result := generalized boolean, `T` when specified adt exists, otherwise `NIL`.
#?(remove-adt 'name) => NIL

;;;; Affected By:

;;;; Side-Effects:

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

(requirements-about MISSING-ADT)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; missing-adt missing tyclex-error cell-error error serious-condition tyclex-condition condition slot-object t

;;;; Effective Slots:

; NAME [Type] T
; [READER] cell-error-name

;;;; Notes:

