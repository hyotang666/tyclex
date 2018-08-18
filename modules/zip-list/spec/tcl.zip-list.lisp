(defpackage :tcl.zip-list.spec
  (:use #:tcl :jingoh :tcl.zip-list #:tcl.applicative))
(in-package :tcl.zip-list.spec)
(setup :tcl.zip-list)

(requirements-about ZIP-LIST)
;;;; Description:
;;;; Compound Type Specifier Kind:

;;;; Compound Type Specifier Syntax:

;;;; Compound Type Specifier Arguments:

;;;; Compound Type Specifier Description:

(requirements-about ZIP-LIST)

;;;; Description:

#+syntax
(ZIP-LIST tyclex.newtype::arg) ; => result

;;;; Arguments and Values:

; arg := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:

#?(<$> (curry + _ _)
       (zip-list '(1 2 3))
       (zip-list '(100 100 100)))
=> (101 102 103)
,:test equal

#?(<$> (curry max _ _)
       (zip-list '(1 2 3 4 5 3))
       (zip-list '(5 3 1 2)))
=> (5 3 3 4)
,:test equal

#?(<$> (curry list _ _ _)
       (zip-list (coerce "dog" 'list))
       (zip-list (coerce "cat" 'list))
       (zip-list (coerce "rat" 'list)))
=> ((#\d #\c #\r)(#\o #\a #\a)(#\g #\t #\t))
,:test equal
,:ignore-signals warning ; <--- series signals.
,:stream nil ; <--- ignore series claims.
,:lazy t

