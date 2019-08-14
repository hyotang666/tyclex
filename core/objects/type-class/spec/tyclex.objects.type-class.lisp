(defpackage :tyclex.objects.type-class.spec
  (:use :cl :jingoh :tyclex.objects.type-class))
(in-package :tyclex.objects.type-class.spec)
(setup :tyclex.objects.type-class)

(requirements-about TYPE-CLASS)
;;;; Description:
;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:
; none

(requirements-about MAKE-TYPE-CLASS)

;;;; Description:
; Constructor for type-class object.

#+syntax
(MAKE-TYPE-CLASS &key
		 ((:name #:name) (error "name is required."))
		 ((:vars #:vars) (error "var is required."))
		 ((:interfaces #:interfaces) nil)
		 ((:member #:member) nil)
		 ((:constraints #:constraints) nil))
; => result

;;;; Arguments and Values:

; name := (and symbol (not (or keyword boolean))), otherwise unspecified.
#?(make-type-class :name "not-symbol" :vars nil) => unspecified

; vars := List as type class parameter variables.

; interfaces := List which contains interface name (i.e. (and symbol (not (or keyword boolean))))

; member := List which contains type specifiers.

; constraints := List which contains type constraints type class names. (i.e. (and symbol (not (or keyword boolean))))

; result := type-class object.
#?(make-type-class :name '#:dummy :vars nil) :be-the type-class

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about TYPE-CLASS-P)

;;;; Description:
; Return true whcn argument is type-class object, otherwise nil.
#?(type-class-p (make-type-class :name '#:dummy :vars nil)) => T
#?(type-class-p '#:not-type-class) => NIL

#+syntax
(TYPE-CLASS-P sb-kernel::object) ; => result

;;;; Arguments and Values:

; object := T

; result := BOOLEAN

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about TYPE-CLASS-NAME)

;;;; Description:
; Reader for TYPE-CLASS's NAME slot.
#?(type-class-name (make-type-class :name 'yes :vars nil))
=> YES

#+syntax
(TYPE-CLASS-NAME arg) ; => result

;;;; Arguments and Values:

; arg := type-class designator, i.e. type-class name is acceptable.
#?(let((tyclex.objects.type-class::*type-classes*(make-hash-table)))
    (add-type-class 'type-class-name :vars nil)
    (type-class-name 'type-class-name))
=> TYPE-CLASS-NAME

; result := type-class name.

;;;; Affected By:
; State of TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When specified type-class is not found, an error is signaled.
#?(type-class-name '#:not-exist) :signals error

(requirements-about TYPE-CLASS-VARS)

;;;; Description:
; Reader for `TYPE-CLASS`'s `VARS` slot.
#?(type-class-vars (make-type-class :name '#:dummy :vars '(yes)))
=> (YES)
,:test equal

#+syntax
(TYPE-CLASS-VARS arg) ; => result

;;;; Arguments and Values:

; arg := type-class designator, i.e. type-class name is acceptable.
#?(let((tyclex.objects.type-class::*type-classes*(make-hash-table)))
    (add-type-class 'type-class-name :vars '(yes))
    (type-class-vars 'type-class-name))
=> (YES)
,:test equal

; result := List which contains type-class parameter variables.

;;;; Affected By:
; State of TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When specified type-class is not found, an error is signaled.
#?(type-class-vars '#:not-exist) :signals error

(requirements-about TYPE-CLASS-INTERFACES)

;;;; Description:
; Reader for `TYPE-CLASS`'s `INTERFACES` slot.
#?(type-class-interfaces (make-type-class :name '#:dummy :vars nil :interfaces '(yes)))
=> (YES)
,:test equal

#+syntax
(TYPE-CLASS-INTERFACES arg) ; => result

;;;; Arguments and Values:

; arg := type-class designator, i.e. type-class name is acceptable.
#?(let((tyclex.objects.type-class::*type-classes*(make-hash-table)))
    (add-type-class 'type-class-name :vars nil :interfaces '(yes))
    (type-class-interfaces 'type-class-name))
=> (YES)
,:test equal

; result := List which contains interface names.

;;;; Affected By:
; TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When specified type-class is not found, an error is signaled.
#?(type-class-interfaces '#:not-exist) :signals error

(requirements-about TYPE-CLASS-MEMBER)

;;;; Description:
; Accessor for `TYPE-CLASS`'s `MEMBER` slot.
#?(type-class-member(make-type-class :name '#:dummy :vars nil :member '(yes)))
=> (YES)
,:test equal
; SETFable.
#?(let((type-class(make-type-class :name '#:dummy :vars nil :member '(#:initial-value))))
    (setf (type-class-member type-class) '(yes))
    (type-class-member type-class))
=> (YES)
,:test equal

#+syntax
(TYPE-CLASS-MEMBER arg) ; => result

#+setf
(SETF (TYPE-CLASS-MEMBER ARG) NEW) ; => new-value

;;;; Arguments and Values:

; arg := type-class designator, i.e. type-class name is acceptable.
#?(let((tyclex.objects.type-class::*type-classes*(make-hash-table)))
    (add-type-class 'type-class-name :vars nil :member '(yes))
    (type-class-member 'type-class-name))
=> (YES)
,:test equal

; result := List which contains type specifiers.

;;;; Affected By:
; TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; when specified type-class is not found, an error is signaled.
#?(type-class-member '#:not-exist) :signals error

(requirements-about TYPE-CLASS-CONSTRAINTS)

;;;; Description:
; Accessor for `TYPE-CLASS`'s `CONSTRAINTS` slot.
#?(type-class-constraints (make-type-class :name '#:dummy :vars nil :constraints '(yes)))
=> (YES)
,:test equal
; SETFable.
#?(let((type-class(make-type-class :name '#:dummy :vars nil :constraints '(#:initial-value))))
    (setf(type-class-constraints type-class)'(yes))
    (type-class-constraints type-class))
=> (YES)
,:test equal

#+syntax
(TYPE-CLASS-CONSTRAINTS arg) ; => result

#+setf
(SETF (TYPE-CLASS-CONSTRAINTS ARG) NEW) ; => new-value

;;;; Arguments and Values:

; arg := type-class designaor, i.e. type-class name is acceptable.
#?(let((tyclex.objects.type-class::*type-classes*(make-hash-table)))
    (add-type-class 'type-class-name :vars nil :constraints '(yes))
    (type-class-constraints 'type-class-name))
=> (YES)
,:test equal

; result := List which contains type-class names.

;;;; Affected By:
; TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When specified type-class is not found, an error is signaled.
#?(type-class-constraints '#:not-exist) :signals error

(requirements-about FIND-TYPE-CLASS
		    :around (let((tyclex.objects.type-class::*type-classes*(make-hash-table)))
			      (add-type-class 'type-class-name :vars nil)
			      (call-body)))

;;;; Description:
; Find type-class from environment.
#?(find-type-class 'type-class-name) :be-the type-class

#+syntax
(FIND-TYPE-CLASS arg &optional (errorp t)) ; => result

;;;; Arguments and Values:

; arg := type-class designator, i.e. type-class object itself is acceptable.
#?(find-type-class(make-type-class :name '#:dummy :vars nil)) :be-the type-class

; errorp := BOOLEAN specifies signaling error or not, when type-class is not found.
; The default is true.
#?(find-type-class '#:not-exist) :signals error
#?(find-type-class '#:not-exist nil) => NIL

; result := type-class object when found, otherwise signals error when `ERRORP` is true, otherwise NIL

;;;; Affected By:
; TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-TYPE-CLASS
		    :around (let((tyclex.objects.type-class::*type-classes*(make-hash-table)))
			      (call-body)))

;;;; Description:
; Adding newly constructed type-class to environemt.
#?(values (find-type-class 'type-class-name nil)
	  (add-type-class 'type-class-name :vars nil)
	  (find-type-class 'type-class-name))
:multiple-value-satisfies
(lambda($first $second $third)
  (& (null $first)
     (type-class-p $second)
     (type-class-p $third)))

#+syntax
(ADD-TYPE-CLASS name &rest args) ; => result

;;;; Arguments and Values:

; name := (and symbol (not (or keyword boolean))), otherwise signals an error.
#?(add-type-class "not-symbol" :vars nil) :signals error

; args := keyword parameters for `MAKE-TYPE-CLASS` except `:NAME`.
; When unsupported keyword parameter comes, an error is signaled.
#?(add-type-class '#:dummy :not-supported-keyword-parameter 0) :signals error

; result := type-class
#?(add-type-class '#:dummy :vars nil) :be-the type-class

;;;; Affected By:

;;;; Side-Effects:
; Modify TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REMOVE-TYPE-CLASS
		    :around (let((tyclex.objects.type-class::*type-classes*(make-hash-table)))
			      (add-type-class 'type-class-name :vars nil)
			      (call-body)))

;;;; Description:
; Remove specified type-class from environment.
#?(values (find-type-class 'type-class-name)
	  (remove-type-class 'type-class-name)
	  (find-type-class 'type-class-name nil))
:multiple-value-satisfies
(lambda($first $second $third)
  (& (type-class-p $first)
     (eq t $second)
     (null $third)))

#+syntax
(REMOVE-TYPE-CLASS name) ; => result

;;;; Arguments and Values:

; name := type-class name.

; result := BOOLEAN, when t specified type-class is exists, otherwise nil.
#?(remove-type-class 'type-class-name) => T
#?(remove-type-class '#:not-exist) => NIL

;;;; Affected By:
; TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Side-Effects:
; Modify TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Notes:

;;;; Exceptional-Situations:

