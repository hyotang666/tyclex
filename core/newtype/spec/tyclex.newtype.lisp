(defpackage :tyclex.newtype.spec
  (:shadowing-import-from :tyclex.newtype #:list)
  (:use :cl :jingoh :tyclex.newtype))
(in-package :tyclex.newtype.spec)
(setup :tyclex.newtype)

(requirements-about DEFINE-NEWTYPE)

;;;; Description:
; Define newtype.
#?(define-newtype zip-list(a)
    (declare(ignore a))
    'cl:list)
=> ZIP-LIST
,:before (mapc #'fmakunbound '(zip-list rewind-zip-list))

#+syntax
(DEFINE-NEWTYPE name lambda-list &body body) ; => result

;;;; Arguments and Values:

; name := (and symbol (nor (or keyword boolean))), otherwise error.
#?(define-newtype "not-symbol"()'null)
:signals error
,:lazy t

#?(define-newtype :keyword () 'null)
:signals error
,:lazy t

#?(define-newtype t () 'null)
:signals error
,:lazy t

; lambda-list := completely same with `CL:DEFTYPE`.

; body := completely same with `CL:DEFTYPE`.

; result := name

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify lisp environment.
; Modify state of `TYCLEX.OBJECTS.NEWTYPE::*NEWTYPES*`.

;;;; Notes:
; Macro which has same name with `NAME` is also defined.
#?(macro-function 'zip-list)
:satisfies identity

; Such macro expanded to `CL:THE` form.
#?(zip-list (list 1 2 3))
:expanded-to (THE (ZIP-LIST LIST) (LIST 1 2 3))

;;;; Exceptional-Situations:

(requirements-about DENEW)

;;;; Description:
; Get body form from `CL:THE` form.
#?(denew '(the fixnum 0)) => 0
#?(denew (macroexpand-1 '(zip-list (list 1 2 3)))) => (list 1 2 3)
,:test equal

#+syntax
(DENEW list) ; => result

;;;; Arguments and Values:

; list := `CL:THE` form, otnerwise unspecified.
#?(denew '(not the form)) => unspecified
#?(denew "Not `THE` form") => unspecified

; result := T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; In many cases, this is not needed, unless to dispatch underlying type instance.

;;;; Exceptional-Situations:

