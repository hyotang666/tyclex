(defpackage :tyclex.objects.adt-constructor.spec
  (:use :cl :jingoh :tyclex.objects.adt-constructor))
(in-package :tyclex.objects.adt-constructor.spec)
(setup :tyclex.objects.adt-constructor)

(requirements-about ADT-CONSTRUCTOR :doc-type STRUCTURE)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; adt-constructor structure-object slot-object t

;;;; Effective Slots:

; TYPE-OF [Type] (OR SYMBOL LIST)

; ARG-TYPES [Type] LIST

;;;; Notes:

(requirements-about MAKE-ADT-CONSTRUCTOR)

;;;; Description:
; Creates a new adt-constructor.
#?(make-adt-constructor :type-of nil :arg-types nil) :be-the adt-constructor

#+syntax
(MAKE-ADT-CONSTRUCTOR &key
		      ((:type-of #:type-of)
		       (error 'tyclex.conditions:slot-uninitialized :name 'type-of))
		      ((:arg-types #:arg-types)
		       (error 'tyclex.conditions:slot-uninitialized :name 'arg-types)))
; => result

;;;; Arguments and Values:

; type-of := `ADT-TYPE-SPECIFIER` which `ADT-CONSTRUCTOR` belongs to, otherwise unspecified.
#?(make-adt-constructor :type-of "not adt type specifier" :arg-types nil)
=> unspecified

; arg-types := LIST, which contains `TYPE-PATTERN`s as constructors lambda-list, otherwise unspecified.
#?(make-adt-constructor :type-of nil :arg-types "not type patterns")
=> unspecified

; result := an `ADT-CONSTRUCTOR`.

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

(requirements-about GET-ADT-CONSTRUCTOR)

;;;; Description:
; Finds a `ADT-CONSTRUCTOR` which associated with `FIRST-ATOM` of the `FORM`.
#?(progn (add-adt-constructor 'name :type-of nil :arg-types nil)
	 (get-adt-constructor 'name))
:be-the adt-constructor
,:before (remove-adt-constructor 'name)

#+syntax
(GET-ADT-CONSTRUCTOR form &optional (errorp t)) ; => result

; `SETF` of `GET-ADT-CONSTRUCTOR` may be used to associates a new `ADT-CONSTRUCTOR` wich an existing indicator `NAME` aleady on the system, or to create a new association if none exists.
#?(progn (setf (get-adt-constructor 'name)
	       (make-adt-constructor :type-of nil :arg-types nil))
	 (get-adt-constructor 'name))
:be-the adt-constructor
,:before (remove-adt-constructor 'name)
,:after (remove-adt-constructor 'name)

#+setf
(SETF (GET-ADT-CONSTRUCTOR NAME &OPTIONAL (ERRORP T)) NEW-VALUE) ; => new-value

;;;; Arguments and Values:

; form := T, but in case with `SETF`, must be `SYMBOL`.
#?(setf (get-adt-constructor "not symbol")
	(make-adt-constructor :type-of nil :arg-types nil))
:signals error

; errorp := a generalized boolean, the default is true.
#?(get-adt-constructor '#:not-exist) :signals missing-adt-constructor
#?(get-adt-constructor '#:not-exist nil) => NIL

; result := a `ADT-CONSTRUCTOR` when found, otherwise `NIL` when `ERRORP` is specified `NIL`, otherwise an error of `MISSING-ADT-CONSTRUCTOR` is signaled.

;;;; Affected By:
; The existence of `ADT-CONSTRUCTOR` in the lisp system.

;;;; Side-Effects:
; none

;;;; Notes:
; When `FORM` is list, its `FIRST-ATOM` is used as key.
#?(get-adt-constructor '((name))) :be-the adt-constructor
,:before (add-adt-constructor 'name :type-of nil :arg-types nil)
,:after (remove-adt-constructor 'name)

;;;; Exceptional-Situations:
; In case of with `SETF`, when `NEW-VALUE` is not `ADT-CONSTRUCTOR`, an error is signaled.
#?(setf (get-adt-constructor 'name)
	"Not adt constructor")
:signals type-error

(requirements-about ADD-ADT-CONSTRUCTOR)

;;;; Description:
; Add name associated with `ADT-CONSTRUCTOR` to lisp environment.
#?(add-adt-constructor 'name :type-of nil :arg-types nil)
:be-the adt-constructor
,:after (remove-adt-constructor 'name)

#+syntax
(ADD-ADT-CONSTRUCTOR name &rest args) ; => result

;;;; Arguments and Values:

; name := symbol, otherwise condition.
#?(add-adt-constructor "not symbol" :type-of nil :arg-types nil) :signals condition

; args := Keyword parameters for `MAKE-ADT-CONSTRUCTOR`.
; When not supported keyword parameter comes, an error is signaled.
#?(add-adt-constructor '#:dummy :type-of nil :not-supported-keyword '#:dummy) :signals error

; result := `ADT-CONSTRUCTOR` object.

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:
; When `NAME` is already exists as `ADT-CONSTRUCTOR` name, `TYCLEX.CONDITIONS:REDEFINITION-WARNING` is signaled.
#?(progn (add-adt-constructor 'name :type-of nil :arg-types nil)
	 (add-adt-constructor 'name :type-of nil :arg-types nil))
:signals TYCLEX.CONDITIONS:REDEFINITION-WARNING

(requirements-about REMOVE-ADT-CONSTRUCTOR)

;;;; Description:
; Remove `NAME` from lisp environment as `ADT-CONSTRUCTOR`.
#?(remove-adt-constructor 'name) => T

#+syntax
(REMOVE-ADT-CONSTRUCTOR name) ; => result

;;;; Arguments and Values:

; name := T

; result := BOOLEAN, `T` when `ADT-CONSTRUCTOR` is found, otherwise `NIL`.
#?(remove-adt-constructor 'name) => NIL

;;;; Affected By:
; The existence of `ADT-CONSTRUCTOR` in the lisp system.

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADT-CONSTRUCTOR-BOUNDP)

;;;; Description:
; Return generalized boolean when symbol bound as `ADT-CONSTRUCTOR`.
#?(adt-constructor-boundp 'name) :satisfies identity
,:before(add-adt-constructor 'name :type-of nil :arg-types nil)

#+syntax
(ADT-CONSTRUCTOR-BOUNDP symbol) ; => result

;;;; Arguments and Values:

; symbol := symbol, otherwise error.
#?(adt-constructor-boundp "not symbol") :signals error

; result := Generalized boolean.

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADT-CONSTRUCTOR-FORM-P)

;;;; Description:
; When `THING` is looks like adt-constructor-form, return true.
#?(adt-constructor-form-p '(name)) :satisfies identity
; otherwise `NIL`.
#?(adt-constructor-form-p "not looks like adt-constructor-form") => NIL

#+syntax
(ADT-CONSTRUCTOR-FORM-P thing) ; => result

;;;; Arguments and Values:

; thing := T

; result := generalized boolean

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADT-CONSTRUCTOR-MAKUNBOUND)

;;;; Description:
; Remove `ADT-CONSTRUCTOR` which associated with `NAME`, in addition, remove the bindings of name in the global environment.

#+syntax
(ADT-CONSTRUCTOR-MAKUNBOUND name) ; => result

;;;; Arguments and Values:

; name := symbol, otherwise condition.
#?(adt-constructor-makunbound "not symbol") :signals condition

; result := generalized boolean. True when name is exist as `ADT-CONSTRUCTOR`.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MISSING-ADT-CONSTRUCTOR)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; missing-adt-constructor missing tyclex-error cell-error error serious-condition tyclex-condition condition slot-object t

;;;; Effective Slots:

; NAME [Type] T
; [READER] cell-error-name

;;;; Notes:

(requirements-about FIRST-ATOM)

;;;; Description:
; Return first atom of `THING`, `CAR` oriented recursion only.

#+syntax
(FIRST-ATOM thing) ; => result

;;;; Arguments and Values:

; thing := T

; result := ATOM

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
#?(first-atom :atom) => :ATOM
#?(first-atom nil) => NIL
#?(first-atom '(1 2 3)) => 1
#?(first-atom '(() 2 3)) => NIL
#?(first-atom '(((()1)2)3)) => NIL
#?(first-atom '((((a)b)c)d)) => A
