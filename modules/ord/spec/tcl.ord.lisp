(defpackage :tcl.ord.spec
  (:use :cl :jingoh :tcl.ord))
(in-package :tcl.ord.spec)
(setup :tcl.ord)

(requirements-about ORD)

;;;; Description:

#+syntax
(ORD #:g0) ; => result

;;;; Arguments and Values:

; g0 := S-Expression

; result := NUMBER

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
;; integer
#?(ord 1) => 1
;; character
#?(ord #\a) => 97

(requirements-about ORD-P)

;;;; Description:

#+syntax
(ORD-P #:arg) ; => result

;;;; Arguments and Values:

; arg := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

