(defpackage :vs-haskell.spec
  (:use :cl :jingoh #:vs-haskell))
(in-package :vs-haskell.spec)
(setup :vs-haskell)

(requirements-about DEFDATA)

;;;; Description:

#+syntax
(DEFDATA name lambda-list &rest constructor*) ; => result

;;;; Arguments and Values:

; name := 

; lambda-list := 

; constructor* := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
; 7.2
; Definition form.
#?(defdata shape()
    (circle real real real)
    (rectangle real real real real))
=> SHAPE
,:before (mapc #'fmakunbound '(circle rectangle))

; Constructors are defined.
#?(fboundp 'circle) => T
#?(fboundp 'rectangle) => T

; Type is defined.
#?(millet:type-specifier-p 'shape) => T
; Constructors are not types.
#?(millet:type-specifier-p 'circle) => NIL
#?(millet:type-specifier-p 'rectangle) => NIL

; Constructors constructs value which type of defined type.
#?(typep (circle 1 2 3) 'shape) => T
#?(typep (rectangle 1 2 3 4) 'shape) => T

; Trivia's pattern matchers are also defined.
#?(nth-value 1 (trivia:pattern-expand-1 '(circle a b c)))
=> T
#?(nth-value 1 (trivia:pattern-expand-1 '(rectangle a b c d)))
=> T

; We can define record data.
#?(defdata .car ()
    (.car (company "" :type string)
	  (model "" :type string)
	  (year 0 :type fixnum)))
=> .CAR
,:before (mapc #'fmakunbound '(.car company model year))

; Record has reader.
#?(fboundp 'company) => T
#?(fboundp 'model) => T
#?(fboundp 'year) => T

; Does not have writer.
#?(fboundp '(setf company)) => NIL
#?(fboundp '(setf model)) => NIL
#?(fboundp '(setf year)) => NIL

; Example of reader.
#?(company (.car :company "hoge"))
=> "hoge"
,:test equal
#?(model (.car :model "hoge"))
=> "hoge"
,:test equal
#?(year (.car :year 1))
=> 1

; DEFDATA support type variable.
#?(defdata maybe(a)
    :nothing
    (just a))
=> MAYBE
,:before (fmakunbound 'just)

#?(typep :nothing '(maybe *)) => T
#?(typep (just 0) '(maybe *)) => T
#?(typep :nothing '(maybe fixnum)) => T
#?(typep (just 0) '(maybe fixnum)) => T
#?(typep :nothing '(maybe string)) => T
#?(typep (just 0) '(maybe string)) => NIL

#?(defdata .vector(a)
    (.vector a a a))
=> .VECTOR
,:before (fmakunbound '.vector)

;; TODO signature checking.

; DATAs can be checked equality by EQUAL.
#?(equal :nothing :nothing) => T
#?(equal :nothing (just 0)) => NIL
#?(equal (just 0) (just 0)) => T
#?(equal (just 0) (just 1)) => NIL

; Recursive data type is invalid.
#?(defdata .list (a)
    .null
    (.cons a .list))
:signals error
