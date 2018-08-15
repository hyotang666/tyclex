(defpackage :tcl.eq.spec
  (:use :cl :jingoh :tcl.eq))
(in-package :tcl.eq.spec)
(setup :tcl.eq)

(requirements-about EQ-P)

;;;; Description:

#+syntax
(EQ-P #:arg) ; => result

;;;; Arguments and Values:

; arg := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ==)

;;;; Description:

#+syntax
(== #:g0 #:g1) ; => result

;;;; Arguments and Values:

; g0 := 

; g1 := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
;; symbol
#?(== :hoge :hoge) => T
#?(== :hoge nil) => NIL
#?(== nil nil) => T

;; number
#?(== 0 0) => T
#?(== 0 1) => NIL
#?(== 0.0 0.0) => T
#?(== 1.0 0.0) => NIL
;; TODO #?(== 0 1.0) => NIL ; currently type must match.

;; character
#?(== #\a #\a) => T
#?(== #\b #\a) => NIL
#?(== #\newline #\a) => NIL

;; string
#?(== "" "") => T
#?(== "hoge" "hoge") => T
#?(== "hoge" "HOGE") => NIL

;; list
#?(== '(1) '(1)) => T
#?(== '(1) '(2)) => NIL
#?(== nil '(a)) => NIL
#?(== '(NIL) '((1))) => NIL

;; vector
#?(== #(1 2 3) #(1 2 3)) => T
#?(== #(1 2 3) #(1 2 3 4)) => NIL

;; bit-vector
#?(== #*1010 #*1010) => T
#?(== #*1010 #*1011) => NIL

;; pathname
#?(== #P"" #P"") => T
#?(== #P"/" #P"") => NIL

;; hash-table
#?(== (make-hash-table)(make-hash-table)) => T

;; array
#?(== #2A((1 2)(3 4)) #2A((1 2)(3 4))) => T
#?(== #2A((1 2)(3 4)) #2A((1 2)(3 5))) => NIL
#?(== #2A((1 2)(3 4)) #0A:hoge) => NIL

;; maybe
#?(== nothing nothing) => T
#?(== nothing (just 0)) => NIL
#?(== (just 0)(just 0)) => T
#?(== (just 0)(just 1)) => NIL

(requirements-about /==)

;;;; Description:

#+syntax
(/== #:g3 #:g4) ; => result

;;;; Arguments and Values:

; g3 := 

; g4 := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:
