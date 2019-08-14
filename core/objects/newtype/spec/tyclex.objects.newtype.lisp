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

