(defpackage :tyclex.objects.interface.spec
  (:use :cl :jingoh :tyclex.objects.interface))
(in-package :tyclex.objects.interface.spec)
(setup :tyclex.objects.interface)

(requirements-about INTERFACE)
;;;; Description:
; Represents type class interface

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:
; none

(requirements-about MAKE-INTERFACE)

;;;; Description:
; Constructor for object which represents type class interface.
#?(make-interface :type-class '#:type-class-name)
:be-the interface

#+syntax
(MAKE-INTERFACE &key
		((:lambda-list #:lambda-list) nil)
		((:return-type #:return-type) nil)
		((:type-class #:type-class) (error "required"))
		((:instances #:instances) nil)
		((:default #:default) nil))
; => result

;;;; Arguments and Values:

; lambda-list := a macro lambda list, see CLHS.
; When not list, unspecified.
#?(make-interface :type-class '#:dummy :lambda-list '#:not-list)
=> unspecified

; return-type := Unification pattern for interface return type.

; type-class := type-class name which interface belongs to.

; instances := List which contains `INSTANCE`.

; default := List which represents default interface definition.
; (i.e. Acceptable for `CL:MACROLET` definition.

; result := `interface` object.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about INTERFACEP)

;;;; Description:
; Return true whcn argument is `interface` object.

#+syntax
(INTERFACEP sb-kernel::object) ; => result

;;;; Arguments and Values:

; object := T
#?(interfacep (make-interface :type-class '#:dummy))
=> T
#?(interfacep '#:not-interface)
=> NIL

; result := BOOLEAN

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about INTERFACE-LAMBDA-LIST)

;;;; Description:
; Reader for `interface`'s `LAMBDA-LIST` slot.
#?(interface-lambda-list (make-interface :type-class '#:dummy :lambda-list '(yes)))
=> (YES)
,:test equal

#+syntax
(INTERFACE-LAMBDA-LIST interface) ; => result

;;;; Arguments and Values:

; interface := interface designator, i.e. interface name is acceptable.
#?(let((tyclex.objects.interface::*interfaces*(make-hash-table)))
    (add-interface 'interface-name :type-class '#:dummy :lambda-list '(yes))
    (interface-lambda-list 'interface-name))
=> (YES)
,:test equal

; result := List as interface lambda-list.

;;;; Affected By:
; State of `TYCLEX.OBJECTS.INTERFACE::*INTERFACES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When `INTERFACE` is interface name (i.e. symbol), and such interface is not exists,
; an error is signaled.
#?(interface-lambda-list '#:not-exists) :signals error

(requirements-about INTERFACE-RETURN-TYPE)

;;;; Description:
; Reader for `interface`'s `RETURN-TYPE` slot.
#?(interface-return-type (make-interface :type-class '#:dummy :return-type '(yes)))
=> (YES)
,:test equal

#+syntax
(INTERFACE-RETURN-TYPE interface) ; => result

;;;; Arguments and Values:

; interface := interface designator, i.e. interface name is acceptable.
#?(let((tyclex.objects.interface::*interfaces*(make-hash-table)))
    (add-interface 'interface-name :type-class '#:dummy :return-type '(yes))
    (interface-return-type 'interface-name))
=> (YES)
,:test equal

; result := Unification pattern for interface return type.

;;;; Affected By:
; State of `TYCLEX.OBJECTS.INTERFACE::*INTERFACES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When `INTERFACE` is interface name (i.e. symbol), and such interface is not exists,
; an error is signaled.
#?(interface-return-type '#:not-exists) :signals error

(requirements-about INTERFACE-TYPE-CLASS)

;;;; Description:
; Reader for `interface`'s `TYPE-CLASS` slot.
#?(interface-type-class (make-interface :type-class 'yes))
=> YES

#+syntax
(INTERFACE-TYPE-CLASS interface) ; => result

;;;; Arguments and Values:

; interface := interface designator, i.e. interface name is acceptable.
#?(let((tyclex.objects.interface::*interfaces*(make-hash-table)))
    (add-interface 'interface-name :type-class 'yes)
    (interface-type-class 'interface-name))
=> YES

; result := type class name.

;;;; Affected By:
; State of `TYCLEX.OBJECTS.INTERFACE::*INTERFACES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When `INTERFACE` is interface name (i.e. symbol), and such interface is not exists,
; an error is signaled.
#?(interface-type-class '#:not-exists) :signals error

(requirements-about INTERFACE-DEFAULT)

;;;; Description:
; Reader for `interface`'s `DEFAULT` slot.
#?(interface-default(make-interface :type-class '#:dummy :default '(yes)))
=> (YES)
,:test equal

#+syntax
(INTERFACE-DEFAULT interface) ; => result

;;;; Arguments and Values:

; interface := interface designator, i.e. interface name is acceptable.
#?(let((tyclex.objects.interface::*interfaces*(make-hash-table)))
    (add-interface 'interface-name :type-class '#:dummy :default '(yes))
    (interface-default 'interface-name))
=> (YES)
,:test equal

; result := Default definition for interface.

;;;; Affected By:
; State of `TYCLEX.OBJECTS.INTERFACE::*INTERFACES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When `INTERFACE` is interface name (i.e. symbol), and such interface is not exists,
; an error is signaled.
#?(interface-default '#:not-exists) :signals error

(requirements-about INTERFACE-INSTANCES)

;;;; Description:
; Accessor for `interface`'s `instances` slot.
#?(interface-instances(make-interface :type-class '#:dummy :instances '(yes)))
=> (YES)
,:test equal

; SETFable.
#?(let((interface(make-interface :type-class '#:dummy :instances '(#:initial-value))))
    (setf(interface-instances interface)'(new-value))
    (interface-instances interface))
=> (NEW-VALUE)
,:test equal

#+syntax
(INTERFACE-INSTANCES interface) ; => result

#+setf
(SETF (INTERFACE-INSTANCES INTERFACE) NEW) ; => new-value

;;;; Arguments and Values:

; interface := interface designator, i.e. interface name is acceptable.
#?(let((tyclex.objects.interface::*interfaces*(make-hash-table)))
    (add-interface 'interface-name :type-class '#:dummy :instances '(yes))
    (interface-instances 'interface-name))
=> (YES)
,:test equal

; result := List which contains `instance`s.

;;;; Affected By:
; State of `TYCLEX.OBJECTS.INTERFACE::*INTERFACES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When `INTERFACE` is interface name (i.e. symbol), and such interface is not exists,
; an error is signaled.
#?(interface-instances '#:not-exists) :signals error

(requirements-about FIND-INTERFACE)

;;;; Description:
; Find interface object from lisp environemt.
#?(let((tyclex.objects.interface::*interfaces*(make-hash-table)))
    (add-interface 'interface-name :type-class '#:dummy)
    (find-interface 'interface-name))
:be-the interface

#+syntax
(FIND-interface interface &optional (errorp t)) ; => result

;;;; Arguments and Values:

; interface := interface designator, i.e. interface object itself is acceptable.
; In such cases, argument itself is returned.
#?(find-interface(make-interface :type-class 'dummy))
:satisfies
#`(& (interfacep $result)
     (eq 'dummy (interface-type-class $result)))

; errorp := BOOLEAN, which specifies signals error or not, when interface is not found.
; The default is T.
#?(find-interface '#:not-exist) :signals error
#?(find-interface '#:not-exist nil) => NIL

; result := interface object when found, otherwise NIL when ERRORP specified NIL, otherwise signals error.

;;;; Affected By:
; State of `TYCLEX.OBJECTS.INTERFACE::*INTERFACES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-INTERFACE
		    :around (let((tyclex.objects.interface::*interfaces*(make-hash-table)))
			      (call-body)))

;;;; Description:
; Add interface to lisp environment.
#?(let((result nil))
    (push (find-interface 'interface-name nil)result)
    (add-interface 'interface-name :type-class 'dummy)
    (push (find-interface 'interface-name) result)
    result)
:satisfies
#`(& (listp $result)
     (= 2 (length $result))
     (interfacep (first $result))
     (null (second $result)))

#+syntax
(ADD-INTERFACE interface &rest args) ; => result

;;;; Arguments and Values:

; interface := (and symbol (not (or keyword boolean))), otherwise error.
#?(add-interface "not-symbol" :type-class '#:dummy) :signals error

; args := keyword parameters for MAKE-INTERFACE, otherwise error.
#?(add-interface 'interface-name :not-supported-parameter '#:dummy)
:signals error

; result := interface
#?(add-interface 'interface-name :type-class '#:dummy)
:be-the interface

;;;; Affected By:
; TYCLEX.OBJECTS.INTERFACE::*INTERFACES*

;;;; Side-Effects:
; Modify TYCLEX.OBJECTS.INTERFACE::*INTERFACES*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REMOVE-INTERFACE
		    :around (let((tyclex.objects.interface::*interfaces*(make-hash-table)))
			      (add-interface 'interface-name :type-class '#:dummy)
			      (call-body)))

;;;; Description:
; Remove interface from environment.
#?(values (find-interface 'interface-name)
	  (remove-interface 'interface-name)
	  (find-interface 'interface-name nil))
:multiple-value-satisfies
#`(& (interfacep $first)
     (eq t $second)
     (null $third))

#+syntax
(REMOVE-INTERFACE interface) ; => result

;;;; Arguments and Values:

; interface := (and symbol (nor (or keyword boolean))), otherwise unspecified.
#?(remove-interface "not-symbol") => unspecified

; result := BOOLEAN when T interface is exists, otherwise nil.
#?(remove-interface 'interface-name) => T
#?(remove-interface 'not-exist) => NIL

;;;; Affected By:
; Status of TYCLEX.OBJECTS.INTERFACE::*INTERFACES*

;;;; Side-Effects:
; Modify TYCLEX.OBJECTS.INTERFACE::*INTERFACES*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about AUGMENT-INSTANCES
		    :around (let((tyclex.objects.interface::*interfaces*(make-hash-table)))
			      (add-interface 'interface-name :type-class '#:dummy)
			      (call-body)))

;;;; Description:
; Augment interface instances.
#?(augment-instances 'interface-name 0) => (0)
,:test equal

#+syntax
(AUGMENT-INSTANCES interface instance) ; => result

;;;; Arguments and Values:

; interface := interface designator, i.e. interface object is acceptable.
#?(augment-instances (make-interface :type-class '#:dummy :instances '(1 2 3))
		     0)
=> (0 1 2 3)
,:test equal

; instance := instance object, otherwise unspecified.

; result := augmented instances.

;;;; Affected By:

;;;; Side-Effects:
; interface instances is modified.
#?(values (interface-instances 'interface-name)
	  (augment-instances 'interface-name 0)
	  (interface-instances 'interface-name))
:multiple-value-satisfies
#`(& (null $first)
     (equal '(0) $second)
     (equal '(0) $third))

;;;; Notes:

;;;; Exceptional-Situations:
; When specified interface is not found, an error is signaled.
#?(augment-instances '#:not-exist '#:dummy) :signals error

