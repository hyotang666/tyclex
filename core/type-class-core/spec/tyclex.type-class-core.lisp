(defpackage :tyclex.type-class-core.spec
  (:use :cl :jingoh :tyclex.type-class-core))
(in-package :tyclex.type-class-core.spec)
(setup :tyclex.type-class-core)

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
#?(let((tyclex.type-class-core::*instances*(make-hash-table)))
    (add-instance 'instance-name :type-class '#:dummy :lambda-list '(yes))
    (instance-lambda-list 'instance-name))
=> (YES)
,:test equal

; result := List as instance lambda-list.

;;;; Affected By:
; State of `TYCLEX.TYPE-CLASS-CORE::*INSTANCES*.

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
#?(let((tyclex.type-class-core::*instances*(make-hash-table)))
    (add-instance 'instance-name :type-class '#:dummy :return-type '(yes))
    (instance-return-type 'instance-name))
=> (YES)
,:test equal

; result := Unification pattern for instance return type.

;;;; Affected By:
; State of `TYCLEX.TYPE-CLASS-CORE::*INSTANCES*.

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
#?(let((tyclex.type-class-core::*instances*(make-hash-table)))
    (add-instance 'instance-name :type-class 'yes)
    (instance-type-class 'instance-name))
=> YES

; result := type class name.

;;;; Affected By:
; State of `TYCLEX.TYPE-CLASS-CORE::*INSTANCES*.

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
#?(let((tyclex.type-class-core::*instances*(make-hash-table)))
    (add-instance 'instance-name :type-class '#:dummy :default '(yes))
    (instance-default 'instance-name))
=> (YES)
,:test equal

; result := Default definition for instance.

;;;; Affected By:
; State of `TYCLEX.TYPE-CLASS-CORE::*INSTANCES*.

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
#?(let((tyclex.type-class-core::*instances*(make-hash-table)))
    (add-instance 'instance-name :type-class '#:dummy :table '(yes))
    (instance-table 'instance-name))
=> (YES)
,:test equal

; result := List which contains `CELL`s.

;;;; Affected By:
; State of `TYCLEX.TYPE-CLASS-CORE::*INSTANCES*.

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
#?(let((tyclex.type-class-core::*instances*(make-hash-table)))
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
; State of `TYCLEX.TYPE-CLASS-CORE::*INSTANCES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-INSTANCE
		    :around (let((tyclex.type-class-core::*instances*(make-hash-table)))
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
; TYCLEX.TYPE-CLASS-CORE::*INSTANCES*

;;;; Side-Effects:
; Modify TYCLEX.TYPE-CLASS-CORE::*INSTANCES*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REMOVE-INSTANCE
		    :around (let((tyclex.type-class-core::*instances*(make-hash-table)))
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
; Status of TYCLEX.TYPE-CLASS-CORE::*INSTANCES*

;;;; Side-Effects:
; Modify TYCLEX.TYPE-CLASS-CORE::*INSTANCES*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about AUGMENT-TABLE
		    :around (let((tyclex.type-class-core::*instances*(make-hash-table)))
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
		 ((:instances #:instances) nil)
		 ((:member #:member) nil)
		 ((:constraints #:constraints) nil))
; => result

;;;; Arguments and Values:

; name := (and symbol (not (or keyword boolean))), otherwise unspecified.
#?(make-type-class :name "not-symbol" :vars nil) => unspecified

; vars := List as type class parameter variables.

; instances := List which contains instance name (i.e. (and symbol (not (or keyword boolean))))

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
#?(let((tyclex.type-class-core::*type-classes*(make-hash-table)))
    (add-type-class 'type-class-name :vars nil)
    (type-class-name 'type-class-name))
=> TYPE-CLASS-NAME

; result := type-class name.

;;;; Affected By:
; State of TYCLEX.TYPE-CLASS-CORE::*TYPE-CLASSES*.

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
#?(let((tyclex.type-class-core::*type-classes*(make-hash-table)))
    (add-type-class 'type-class-name :vars '(yes))
    (type-class-vars 'type-class-name))
=> (YES)
,:test equal

; result := List which contains type-class parameter variables.

;;;; Affected By:
; State of TYCLEX.TYPE-CLASS-CORE::*TYPE-CLASSES*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When specified type-class is not found, an error is signaled.
#?(type-class-vars '#:not-exist) :signals error

(requirements-about TYPE-CLASS-INSTANCES)

;;;; Description:
; Reader for `TYPE-CLASS`'s `INSTANCE` slot.
#?(type-class-instances (make-type-class :name '#:dummy :vars nil :instances '(yes)))
=> (YES)
,:test equal

#+syntax
(TYPE-CLASS-INSTANCES arg) ; => result

;;;; Arguments and Values:

; arg := type-class designator, i.e. type-class name is acceptable.
#?(let((tyclex.type-class-core::*type-classes*(make-hash-table)))
    (add-type-class 'type-class-name :vars nil :instances '(yes))
    (type-class-instances 'type-class-name))
=> (YES)
,:test equal

; result := List which contains instance names.

;;;; Affected By:
; TYCLEX.TYPE-CLASS-CORE::*TYPE-CLASSES*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When specified type-class is not found, an error is signaled.
#?(type-class-instances '#:not-exist) :signals error

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
#?(let((tyclex.type-class-core::*type-classes*(make-hash-table)))
    (add-type-class 'type-class-name :vars nil :member '(yes))
    (type-class-member 'type-class-name))
=> (YES)
,:test equal

; result := List which contains type specifiers.

;;;; Affected By:
; TYCLEX.TYPE-CLASS-CORE::*TYPE-CLASSES*

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
#?(let((tyclex.type-class-core::*type-classes*(make-hash-table)))
    (add-type-class 'type-class-name :vars nil :constraints '(yes))
    (type-class-constraints 'type-class-name))
=> (YES)
,:test equal

; result := List which contains type-class names.

;;;; Affected By:
; TYCLEX.TYPE-CLASS-CORE::*TYPE-CLASSES*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; When specified type-class is not found, an error is signaled.
#?(type-class-constraints '#:not-exist) :signals error

(requirements-about FIND-TYPE-CLASS
		    :around (let((tyclex.type-class-core::*type-classes*(make-hash-table)))
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
; TYCLEX.TYPE-CLASS-CORE::*TYPE-CLASSES*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-TYPE-CLASS
		    :around (let((tyclex.type-class-core::*type-classes*(make-hash-table)))
			      (call-body)))

;;;; Description:
; Adding newly constructed type-class to environemt.
#?(values (find-type-class 'type-class-name nil)
	  (add-type-class 'type-class-name :vars nil)
	  (find-type-class 'type-class-name))
:multiple-value-satisfies
#`(& (null $first)
     (type-class-p $second)
     (type-class-p $third))

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
; Modify TYCLEX.TYPE-CLASS-CORE::*TYPE-CLASSES*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REMOVE-TYPE-CLASS
		    :around (let((tyclex.type-class-core::*type-classes*(make-hash-table)))
			      (add-type-class 'type-class-name :vars nil)
			      (call-body)))

;;;; Description:
; Remove specified type-class from environment.
#?(values (find-type-class 'type-class-name)
	  (remove-type-class 'type-class-name)
	  (find-type-class 'type-class-name nil))
:multiple-value-satisfies
#`(& (type-class-p $first)
     (eq t $second)
     (null $third))

#+syntax
(REMOVE-TYPE-CLASS name) ; => result

;;;; Arguments and Values:

; name := type-class name.

; result := BOOLEAN, when t specified type-class is exists, otherwise nil.
#?(remove-type-class 'type-class-name) => T
#?(remove-type-class '#:not-exist) => NIL

;;;; Affected By:
; TYCLEX.TYPE-CLASS-CORE::*TYPE-CLASSES*

;;;; Side-Effects:
; Modify TYCLEX.TYPE-CLASS-CORE::*TYPE-CLASSES*

;;;; Notes:

;;;; Exceptional-Situations:

