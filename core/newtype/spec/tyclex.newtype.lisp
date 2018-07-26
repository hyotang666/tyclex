(defpackage :tyclex.newtype.spec
  (:import-from :tyclex.newtype #:enough-type-specifier)
  (:use :cl :jingoh :tyclex.newtype))
(in-package :tyclex.newtype.spec)
(setup :tyclex.newtype)

(requirements-about DEFINE-NEWTYPE)

;;;; Description:
; Define newtype.
#?(define-newtype zip-list(a)
    (declare(ignore a))
    'list)
=> ZIP-LIST
,:before (fmakunbound 'zip-list)

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
; Modify state of `TYCLEX::*NEWTYPES*`.

;;;; Notes:
; Macro which has same name with `NAME` is also defined.
#?(macro-function 'zip-list)
:satisfies identity

; Such macro expanded to `CL:THE` form.
#?(zip-list (list 1 2 3))
:expanded-to (THE (ZIP-LIST *) (LIST 1 2 3))

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

(requirements-about ENOUGH-TYPE-SPECIFIER)

;;;; Description:
; Return enough type specifier.
; Required parameter is treated as `*`.

#+syntax
(ENOUGH-TYPE-SPECIFIER name lambda-list) ; => result

;;;; Arguments and Values:

; name := (and symbol (not (or keyword boolean))) is expected. (i.e. no checks.)

; lambda-list := a deftype lambda list, see CLHS.
; When not list, an error is signaled.
#?(enough-type-specifier '#:dummy '#:not-list)
:signals error

; result := newtype type specifier is expected. (i.e. not be guaranteed.)
#?(enough-type-specifier 'zip-list ()) => ZIP-LIST

#?(enough-type-specifier 'zip-list '(a b)) => (ZIP-LIST * *)
,:test equal

#?(enough-type-specifier 'zip-list '(a &optional b)) => (ZIP-LIST *)
,:test equal

#?(enough-type-specifier 'zip-list '(&optional a b)) => ZIP-LIST

#?(enough-type-specifier 'zip-list '(&whole whole &environment env a b &key c))
=> (ZIP-LIST * *)
,:test equal

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; `ENOUGH-TYPE-SPECIFIER` does not guarantee return value is type-specifier.
#?(enough-type-specifier :not-type-name ())
:satisfies
#`(& (eq :not-type-name $result)
     (not(millet:type-specifier-p $result)))

;;;; Exceptional-Situations:

