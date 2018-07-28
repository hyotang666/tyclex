(defpackage :tyclex.objects.instance.spec
  (:use :cl :jingoh :tyclex.objects.instance))
(in-package :tyclex.objects.instance.spec)
(setup :tyclex.objects.instance)

(requirements-about INSTANCE)
;;;; Description:
; Represents type class instance

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:
; none

(requirements-about MAKE-TYPE-CLASS-INSTANCE)

;;;; Description:
; Constructor for object which represents type class instance.
#?(make-type-class-instance :type-class '#:type-class-name)
:be-the instance

#+syntax
(MAKE-TYPE-CLASS-INSTANCE &key
			  ((:lambda-list #:lambda-list) nil)
			  ((:return-type #:return-type) nil)
			  ((:type-class #:type-class) (error "required"))
			  ((:table #:table) nil)
			  ((:default #:default) nil))
; => result

;;;; Arguments and Values:

; lambda-list := a macro lambda list, see CLHS.
; When not list, unspecified.
#?(make-type-class-instance :type-class '#:dummy :lambda-list '#:not-list)
=> unspecified

; return-type := Unification pattern for instance return type.

; type-class := type-class name which has the instance.

; table := List which contains `CELL` which represents each instances.

; default := List which represents default instance definition.
; (i.e. Acceptable for `CL:MACROLET` definitions.

; result := `INSTANCE` object.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about INSTANCEP)

;;;; Description:
; Return true whcn argument is `INSTANCE` object.

#+syntax
(INSTANCEP sb-kernel::object) ; => result

;;;; Arguments and Values:

; object := T
#?(instancep (make-type-class-instance :type-class '#:dummy))
=> T
#?(instancep '#:not-instance)
=> NIL

; result := BOOLEAN

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about INSTANCE-LAMBDA-LIST)

;;;; Description:
; Reader for `INSTANCE`'s `LAMBDA-LIST` slot.
#?(instance-lambda-list (make-type-class-instance :type-class '#:dummy :lambda-list '(yes)))
=> (YES)
,:test equal

#+syntax
(INSTANCE-LAMBDA-LIST interface) ; => result

;;;; Arguments and Values:

; interface := Instance designator, i.e. instance name is acceptable.
#?(let((tyclex.objects.instance::*instances*(make-hash-table)))
    (add-instance 'instance-name :type-class '#:dummy :lambda-list '(yes))
    (instance-lambda-list 'instance-name))
=> (YES)
,:test equal

; result := List as instance lambda-list.

;;;; Affected By:
; State of `TYCLEX.OBJECTS.INSTANCE::*INSTANCES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When `INTERFACE` is instance name (i.e. symbol), and such instance is not exists,
; an error is signaled.
#?(instance-lambda-list '#:not-exists) :signals error

(requirements-about INSTANCE-RETURN-TYPE)

;;;; Description:
; Reader for `INSTANCE`'s `RETURN-TYPE` slot.
#?(instance-return-type (make-type-class-instance :type-class '#:dummy :return-type '(yes)))
=> (YES)
,:test equal

#+syntax
(INSTANCE-RETURN-TYPE interface) ; => result

;;;; Arguments and Values:

; interface := Instance designator, i.e. instance name is acceptable.
#?(let((tyclex.objects.instance::*instances*(make-hash-table)))
    (add-instance 'instance-name :type-class '#:dummy :return-type '(yes))
    (instance-return-type 'instance-name))
=> (YES)
,:test equal

; result := Unification pattern for instance return type.

;;;; Affected By:
; State of `TYCLEX.OBJECTS.INSTANCE::*INSTANCES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When `INTERFACE` is instance name (i.e. symbol), and such instance is not exists,
; an error is signaled.
#?(instance-return-type '#:not-exists) :signals error

(requirements-about INSTANCE-TYPE-CLASS)

;;;; Description:
; Reader for `INSTANCE`'s `TYPE-CLASS` slot.
#?(instance-type-class (make-type-class-instance :type-class 'yes))
=> YES

#+syntax
(INSTANCE-TYPE-CLASS interface) ; => result

;;;; Arguments and Values:

; interface := Instance designator, i.e. instance name is acceptable.
#?(let((tyclex.objects.instance::*instances*(make-hash-table)))
    (add-instance 'instance-name :type-class 'yes)
    (instance-type-class 'instance-name))
=> YES

; result := type class name.

;;;; Affected By:
; State of `TYCLEX.OBJECTS.INSTANCE::*INSTANCES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When `INTERFACE` is instance name (i.e. symbol), and such instance is not exists,
; an error is signaled.
#?(instance-type-class '#:not-exists) :signals error

(requirements-about INSTANCE-DEFAULT)

;;;; Description:
; Reader for `INSTANCE`'s `DEFAULT` slot.
#?(instance-default(make-type-class-instance :type-class '#:dummy :default '(yes)))
=> (YES)
,:test equal

#+syntax
(INSTANCE-DEFAULT interface) ; => result

;;;; Arguments and Values:

; interface := Instance designator, i.e. instance name is acceptable.
#?(let((tyclex.objects.instance::*instances*(make-hash-table)))
    (add-instance 'instance-name :type-class '#:dummy :default '(yes))
    (instance-default 'instance-name))
=> (YES)
,:test equal

; result := Default definition for instance.

;;;; Affected By:
; State of `TYCLEX.OBJECTS.INSTANCE::*INSTANCES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When `INTERFACE` is instance name (i.e. symbol), and such instance is not exists,
; an error is signaled.
#?(instance-default '#:not-exists) :signals error

(requirements-about INSTANCE-TABLE)

;;;; Description:
; Accessor for `INSTANCE`'s `TABLE` slot.
#?(instance-table(make-type-class-instance :type-class '#:dummy :table '(yes)))
=> (YES)
,:test equal

; SETFable.
#?(let((instance(make-type-class-instance :type-class '#:dummy :table '(#:initial-value))))
    (setf(instance-table instance)'(new-value))
    (instance-table instance))
=> (NEW-VALUE)
,:test equal

#+syntax
(INSTANCE-TABLE interface) ; => result

#+setf
(SETF (INSTANCE-TABLE INTERFACE) NEW) ; => new-value

;;;; Arguments and Values:

; interface := Instance designator, i.e. instance name is acceptable.
#?(let((tyclex.objects.instance::*instances*(make-hash-table)))
    (add-instance 'instance-name :type-class '#:dummy :table '(yes))
    (instance-table 'instance-name))
=> (YES)
,:test equal

; result := List which contains `CELL`s.

;;;; Affected By:
; State of `TYCLEX.OBJECTS.INSTANCE::*INSTANCES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When `INTERFACE` is instance name (i.e. symbol), and such instance is not exists,
; an error is signaled.
#?(instance-table '#:not-exists) :signals error

(requirements-about FIND-INSTANCE)

;;;; Description:
; Find instance object from lisp environemt.
#?(let((tyclex.objects.instance::*instances*(make-hash-table)))
    (add-instance 'instance-name :type-class '#:dummy)
    (find-instance 'instance-name))
:be-the instance

#+syntax
(FIND-INSTANCE interface &optional (errorp t)) ; => result

;;;; Arguments and Values:

; interface := Instance designator, i.e. instance object itself is acceptable.
; In such cases, argument itself is returned.
#?(find-instance(make-type-class-instance :type-class 'dummy))
:satisfies
#`(& (instancep $result)
     (eq 'dummy (instance-type-class $result)))

; errorp := BOOLEAN, which specifies signals error or not, when instance is not found.
; The default is T.
#?(find-instance '#:not-exist) :signals error
#?(find-instance '#:not-exist nil) => NIL

; result := Instance object when found, otherwise NIL when ERRORP specified NIL, otherwise signals error.

;;;; Affected By:
; State of `TYCLEX.OBJECTS.INSTANCE::*INSTANCES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-INSTANCE
		    :around (let((tyclex.objects.instance::*instances*(make-hash-table)))
			      (call-body)))

;;;; Description:
; Add instance to lisp environment.
#?(let((result nil))
    (push (find-instance 'instance-name nil)result)
    (add-instance 'instance-name :type-class 'dummy)
    (push (find-instance 'instance-name) result)
    result)
:satisfies
#`(& (listp $result)
     (= 2 (length $result))
     (instancep (first $result))
     (null (second $result)))

#+syntax
(ADD-INSTANCE interface &rest args) ; => result

;;;; Arguments and Values:

; interface := (and symbol (not (or keyword boolean))), otherwise error.
#?(add-instance "not-symbol" :type-class '#:dummy) :signals error

; args := keyword parameters for MAKE-TYPE-CLASS-INSTANCE, otherwise error.
#?(add-instance 'instance-name :not-supported-parameter '#:dummy)
:signals error

; result := instance
#?(add-instance 'instance-name :type-class '#:dummy)
:be-the instance

;;;; Affected By:
; TYCLEX.OBJECTS.INSTANCE::*INSTANCES*

;;;; Side-Effects:
; Modify TYCLEX.OBJECTS.INSTANCE::*INSTANCES*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REMOVE-INSTANCE
		    :around (let((tyclex.objects.instance::*instances*(make-hash-table)))
			      (add-instance 'instance-name :type-class '#:dummy)
			      (call-body)))

;;;; Description:
; Remove instance from environment.
#?(values (find-instance 'instance-name)
	  (remove-instance 'instance-name)
	  (find-instance 'instance-name nil))
:multiple-value-satisfies
#`(& (instancep $first)
     (eq t $second)
     (null $third))

#+syntax
(REMOVE-INSTANCE interface) ; => result

;;;; Arguments and Values:

; interface := (and symbol (nor (or keyword boolean))), otherwise unspecified.
#?(remove-instance "not-symbol") => unspecified

; result := BOOLEAN when T instance is exists, otherwise nil.
#?(remove-instance 'instance-name) => T
#?(remove-instance 'not-exist) => NIL

;;;; Affected By:
; Status of TYCLEX.OBJECTS.INSTANCE::*INSTANCES*

;;;; Side-Effects:
; Modify TYCLEX.OBJECTS.INSTANCE::*INSTANCES*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about AUGMENT-TABLE
		    :around (let((tyclex.objects.instance::*instances*(make-hash-table)))
			      (add-instance 'instance-name :type-class '#:dummy)
			      (call-body)))

;;;; Description:
; Augment instance table.
#?(augment-table 'instance-name 0) => (0)
,:test equal

#+syntax
(AUGMENT-TABLE interface cell) ; => result

;;;; Arguments and Values:

; interface := instance designator, i.e. instance object is acceptable.
#?(augment-table (make-type-class-instance :type-class '#:dummy :table '(1 2 3))
		 0)
=> (0 1 2 3)
,:test equal

; cell := CELL object, otherwise unspecified.

; result := augmented table.

;;;; Affected By:

;;;; Side-Effects:
; instance table is modified.
#?(values (instance-table 'instance-name)
	  (augment-table 'instance-name 0)
	  (instance-table 'instance-name))
:multiple-value-satisfies
#`(& (null $first)
     (equal '(0) $second)
     (equal '(0) $third))

;;;; Notes:

;;;; Exceptional-Situations:
; When specified instance is not found, an error is signaled.
#?(augment-table '#:not-exist '#:dummy) :signals error

