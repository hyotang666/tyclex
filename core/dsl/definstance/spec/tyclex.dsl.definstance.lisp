(defpackage :tyclex.dsl.definstance.spec
  (:use :cl :jingoh :tyclex.dsl.definstance))
(in-package :tyclex.dsl.definstance.spec)
(setup :tyclex.dsl.definstance)

(requirements-about DEFINSTANCE
		    :around
		    (let((tyclex.objects.type-class::*type-classes*(make-hash-table))
			 (tyclex.objects.interface::*interfaces*(make-hash-table)))
		      (tyclex.objects:add-type-class 'dummy :vars nil :interfaces '(face))
		      (tyclex.objects:add-interface 'face :type-class 'dummy :lambda-list '(v))
		      (call-body)))

;;;; Description:
; Define instance.

#+syntax
(DEFINSTANCE (type-class &rest args) definition+) ; => result

;;;; Arguments and Values:

; type-class := symbol, otherwise error.
#?(definstance("not symbol" t)
    ((face(v)v))) :signals error
; It must names already exists type-class name, otherwise error.
#?(definstance(not-exist t)
    ((face(v)v))) :signals error
; otherwise, newly created instance is associated with `TYPE-CLASS`.

; args := (type+ [:constraints constraint*]*)
; type := type-specifier, otherwise unspecified.
#?(definstance(dummy not-type-specifier)
    ((face(v)v)))
=> unspecified
; When no `type`, an error is signaled.
#?(definstance(dummy)
    ((face(v)v)))
:signals error

; Supported keyword parameter is only :constraints, other keyword comes, an error is signaled.
#?(definstance(dummy t :not-supported t)
    ((face(v)v)))
:signals error

; constraint := type-class name (i.e. symbol), otherwise error.
#?(definstance(dummy t :constraints "not symbol")
    ((face(v)v)))
:signals error
; It must names already exists type-class name, otherwise error.
#?(definstance(dummy t :constraints not-exist)
    ((face(v)v)))
:signals error

; definition := Form for `CL:MACROLET` first argument. See CLHS.
; When no definitions, and `INTERFACE` does not have default instance, an error is signaled.
#?(definstance(dummy t)
    ())
:signals error
; When not supported interface is specified, an error is signaled.
#?(definstance(dummy t)
    ((not-supported()t)))
:signals error

; result := `TYPE-CLASS`

;;;; Affected By:
; TYCLEX.OBJECTS.INTERFACE::*INTERFACES*
; TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Side-Effects:
; Modify TYCLEX.OBJECTS.INTERFACE::*INTERFACES*
; Modify elements of TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
#?(macroexpand-1 '(definstance(dummy t)
		    ((face(a)a))))
=> (PROGN (TYCLEX.OBJECTS.INTERFACE:AUGMENT-INSTANCES
	    'FACE
	    (TYCLEX.OBJECTS.INSTANCE:MAKE-TYPE-CLASS-INSTANCE
	      :SIGNATURE '(V)
	      :DEFINITIONS '((FACE(A)A))
	      :TYPES '(T)
	      :CONSTRAINTS 'NIL))
	  (PUSHNEW 'T (TYCLEX.OBJECTS.TYPE-CLASS:TYPE-CLASS-MEMBER 'DUMMY)
		   :TEST #'EQUAL)
	  'DUMMY)
,:test equal
