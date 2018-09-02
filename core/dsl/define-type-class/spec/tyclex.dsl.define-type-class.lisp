(defpackage :tyclex.dsl.define-type-class.spec
  (:use :cl :jingoh :tyclex.dsl.define-type-class))
(in-package :tyclex.dsl.define-type-class.spec)
(setup :tyclex.dsl.define-type-class)

(requirements-about DEFINE-TYPE-CLASS)

;;;; Description:

#+syntax
(DEFINE-TYPE-CLASS (name &rest type-var+) (&rest var-constraint*) signature+ &rest rest) ; => result

;;;; Arguments and Values:

; name := SYMBOL otherwise error.
#?(define-type-class("not symbol")()()) :signals error
,:lazy t

; type-var := SYMBOL otherwise error.
#?(define-type-class(dummy "not symbol")()()) :signals error
,:lazy t

; var-constraint := (type-class-name type-var)
; type-class-name := symbol otherwise error.
#?(define-type-class(dummy)("not symbol")()) :signals error
,:lazy t
; It must names already defined other type class, otherwise error.
#?(define-type-class(dummy)(not-exist)()) :signals error
,:lazy t

; signature := (interface arg-signatures type-pattern)

; interface := SYMBOL, otherwise error.
#?(define-type-class(dummy)()
    (("not symbol" () fixnum)))
:signals error
,:lazy t

; arg-signatures := (type-pattern*), otherwise error.
#?(define-type-class(dummy)()
    ((interface "not list" fixnum)))
:signals error
,:lazy t

; rest := (:default interface lambda-list body+)

; result := `NAME`

;;;; Affected By:
; State of TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Side-Effects:
; Modify TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about INFINITE-EXPANSION-DETECTER)

;;;; Description:

#+syntax
(INFINITE-EXPANSION-DETECTER expander form env) ; => result

;;;; Arguments and Values:

; expander := 

; form := 

; env := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

