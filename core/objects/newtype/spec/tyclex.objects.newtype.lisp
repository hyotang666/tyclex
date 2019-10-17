(defpackage :tyclex.objects.newtype.spec
  (:shadowing-import-from :tyclex.objects.newtype #:list)
  (:use :cl :jingoh :tyclex.objects.newtype))
(in-package :tyclex.objects.newtype.spec)
(setup :tyclex.objects.newtype)

(requirements-about NEWTYPE-TYPE-SPECIFIER-P)

;;;; Description:
; Return T when argument may newtype type specifier.
#?(newtype-type-specifier-p "Not newtype type specifier")
=> NIL

#+syntax
(NEWTYPE-TYPE-SPECIFIER-P type-specifier) ; => result

;;;; Arguments and Values:

; type-specifier := T

; result := BOOLEAN

;;;; Affected By:
; Lisp environment.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-NEWTYPE :doc-type function)

;;;; Description:

#+syntax
(ADD-NEWTYPE name) ; => result

;;;; Arguments and Values:

; name := (and symbol (not (or keyword boolean))), otherwise error.
#?(add-newtype "not symbol") :signals condition

; result := (EQL T)
#?(add-newtype 'newtype) => T
,:around
(let((tyclex.objects.newtype::*newtypes*(make-hash-table)))
  (call-body))

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify lisp environment.

;;;; Notes:

;;;; Exceptional-Situations:

