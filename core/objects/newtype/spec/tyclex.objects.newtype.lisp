(defpackage :tyclex.objects.newtype.spec
  (:shadowing-import-from :tyclex.objects.newtype #:list)
  (:use :cl :jingoh :tyclex.objects.newtype))
(in-package :tyclex.objects.newtype.spec)
(setup :tyclex.objects.newtype)

(requirements-about NEWTYPE :doc-type STRUCTURE)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; newtype structure-object slot-object t

;;;; Effective Slots:

; NAME [Type] (AND SYMBOL (NOT (OR KEYWORD BOOLEAN)))

; LAMBDA-LIST [Type] LIST

;;;; Notes:

(requirements-about MAKE-NEWTYPE :doc-type function)

;;;; Description:

#+syntax
(MAKE-NEWTYPE &key
              ((:name #:name)
               (error 'tyclex.conditions:slot-uninitialized :name 'name))
              ((:lambda-list #:lambda-list) nil))
; => result

;;;; Arguments and Values:

; name := symbol, otherwise condition.
#?(MAKE-NEWTYPE :NAME "not-symbol") :signals condition
; When not specified, an error is signaled.
#?(MAKE-NEWTYPE) :signals TYCLEX.CONDITIONS:SLOT-UNINITIALIZED
,:lazy t

; lambda-list := list, otherwise condition.
#?(MAKE-NEWTYPE :NAME 'DUMMY :LAMBDA-LIST "not list") :signals TYPE-ERROR
,:lazy t

; result := newtype.

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about NEWTYPE-NAME :doc-type function)

;;;; Description:

#+syntax
(NEWTYPE-NAME sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := newtype object, otherwise condition.
#?(NEWTYPE-NAME '#:NOT-NEWTYPE-OBJECT) :signals TYPE-ERROR
,:lazy t

; result := symbol which is slot value.
#?(NEWTYPE-NAME (MAKE-NEWTYPE :NAME 'THIS)) => THIS

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about NEWTYPE-LAMBDA-LIST :doc-type function)

;;;; Description:

#+syntax
(NEWTYPE-LAMBDA-LIST sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := newtype object, otherwise conditioin.
#?(NEWTYPE-LAMBDA-LIST '#:NOT-NEWTYPE-OBJECT) :signals TYPE-ERROR
,:lazy t

; result := list, which is slot value.
#?(NEWTYPE-LAMBDA-LIST
   (MAKE-NEWTYPE :NAME '#:DUMMY :LAMBDA-LIST '(THI IS RETURNED)))
=> (THI IS RETURNED)
, :test equal(requirements-about NEWTYPEP :doc-type function)

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about NEWTYPEP :doc-type function)

;;;; Description:

#+syntax (NEWTYPEP sb-kernel::object) ; => result

#?(NEWTYPEP '#:NOT-NEWTYPE-OBJECT) => NIL
#?(NEWTYPEP (MAKE-NEWTYPE :NAME '#:DUMMY)) => T

;;;; Arguments and Values:

; object := T

; result := Boolean.

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-NEWTYPE :doc-type function
                    :around (let ((tyclex.objects.newtype::*newtypes*
                                    (make-hash-table)))
                              (call-body)))

;;;; Description:

#+syntax (ADD-NEWTYPE name &key lambda-list) ; => result

;;;; Arguments and Values:

; name := (and symbol (not boolean) (not (satisfies keywordp))), otherwise condition.
#?(add-newtype "not symbol") :signals condition

; lambda-list := list, otherwise condition.
#?(add-newtype '#:dummy :lambda-list "not list") :signals condition

; result := newtype

;;;; Affected By:
; none.

;;;; Side-Effects:
; Modify tyclex.objects.newtype::*NEWTYPES*.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REMOVE-NEWTYPE :doc-type function
                    :around (let ((tyclex.objects.newtype::*newtypes*
                                    (make-hash-table)))
                              (call-body)))

;;;; Description:

#+syntax (REMOVE-NEWTYPE symbol) ; => result

;;;; Arguments and Values:

; symbol := symbol, otherwise condition.
#?(remove-newtype "not symbol") :signals condition

; result := boolean
; When specified newtype is not exist, NIL is returned.
#?(remove-newtype '#:no-such) => NIL
; When remove newtype successfully, T is returned.
#?(progn (add-newtype 'this)
         (remove-newtype 'this))
=> T

;;;; Affected By:
; tyclex.objects.newtype::*NEWTYPES*

;;;; Side-Effects:
; Modify tyclex.objects.newtype::*NEWTYPES*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FIND-NEWTYPE :doc-type function
                    :around (let ((tyclex.objects.newtype::*newtypes*
                                    (make-hash-table)))
                              (add-newtype 'this)
                              (call-body)))

;;;; Description:

#+syntax (FIND-NEWTYPE name &optional (errorp t)) ; => result

;;;; Arguments and Values:

; name := t
#?(find-newtype 'this) :be-the newtype

; errorp := boolean
#?(FIND-NEWTYPE '#:NO-SUCH) :signals missing-newtype
#?(FIND-NEWTYPE '#:NO-SUCH NIL) => NIL

; result := (or newtype null)

;;;; Affected By:
; tyclex.objects.newtype::*NEWTYPES*

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:


(requirements-about NEWTYPE-TYPE-SPECIFIER-P :doc-type function
                    :around (let ((tyclex.objects.newtype::*newtypes*
                                    (make-hash-table)))
                              (add-newtype 'newtype)
                              (call-body)))

;;;; Description:

#+syntax (NEWTYPE-TYPE-SPECIFIER-P type-specifier) ; => result

;;;; Arguments and Values:

; type-specifier := t

; result := (or newtype null)
#?(NEWTYPE-TYPE-SPECIFIER-P "not newtype specifier") => NIL
#?(NEWTYPE-TYPE-SPECIFIER-P 'newtype) :be-the newtype

;;;; Affected By:
; tyclex.objects.newtype::*NEWTYPES*

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:


