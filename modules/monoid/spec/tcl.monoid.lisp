(defpackage :tcl.monoid.spec
  (:use #:tcl :jingoh #:tcl.monoid #:tcl.compare #:tcl.data))
(in-package :tcl.monoid.spec)
(setup :tcl.monoid)

(requirements-about MONOID)

;;;; Requirements
; * (equal (mappend (mempty) x) x)
; * (equal (mappend x (mempty)) x)
; * (equal (mappend (mappend x y) z) (mappend x (mappend y z)))

;; List
#?(mappend (mempty) '(1 2 3))
=> (1 2 3)
,:test equal
#?(mappend '(1 2 3)(mempty))
=> (1 2 3)
,:test equal
#?(mappend (mappend '(1 2) '(3 4))
	   '(5 6))
:equivalents (mappend '(1 2)
		      (mappend '(3 4)'(5 6)))
,:test equal

;; Vector
#?(mappend (mempty) #(1 2 3))
=> #(1 2 3)
,:test equalp
#?(mappend #(1 2 3)(mempty))
=> #(1 2 3)
,:test equalp
#?(mappend (mappend #(1 2) #(3 4))
	   #(5 6))
:equivalents (mappend #(1 2)
		      (mappend #(3 4)#(5 6)))
,:test equalp

;; String
#?(mappend (mempty) "abc")
=> "abc"
,:test equal
#?(mappend "abc" (mempty))
=> "abc"
,:test equal
#?(mappend (mappend "ab" "cd")
	   "ef")
:equivalents (mappend "ab" (mappend "cd" "ef"))
,:test equal

;; Bit-vector
#?(mappend (mempty) #*101)
=> #*101
,:test equal
#?(mappend #*101 (mempty))
=> #*101
,:test equal
#?(mappend (mappend #*10 #*01)
	   #*11)
:equivalents (mappend #*10
		      (mappend #*01 #*11))
,:test equal

;; Product
#?(mappend (mempty)(product 2)) => 2
#?(mappend (product 2)(mempty)) => 2
#?(mappend (mappend (product 2) (product 3)) (product 4))
:equivalents (mappend (product 2) (mappend (product 3) 4))

;; Sum
#?(mappend (mempty)(sum 2)) => 2
#?(mappend (sum 2) (mempty)) => 2
#?(mappend (mappend (sum 2) 3)(sum 4))
:equivalents (mappend (sum 2)(mappend (sum 3) 4))

;; Any
#?(mappend (mempty) (any t)) => T
#?(mappend (any t) (mempty)) => T
#?(mappend (mappend (any nil)(any nil))(any t))
:equivalents (mappend (any nil)
		      (mappend (any nil)(any t)))

;; ALL
#?(mappend (mempty)(all t)) => T
#?(mappend (all t)(mempty)) => T
#?(mappend (mappend (all t)(all t)) (all t))
:equivalents (mappend (all t)
		      (mappend (all t)(all t)))

;; Ordering
#?(mappend (mempty):lt) => :LT
#?(mappend :lt (mempty)) => :LT
#?(mappend (mappend :lt :gt) :eq)
:equivalents (mappend :lt (mappend :gt :eq))

;; Maybe
#?(mappend (mempty) (just 3)) => (just 3)
,:test equal
#?(mappend (just 3)(mempty)) => (just 3)
,:test equal
#?(mappend (mappend (just (sum 2)) (just (sum 3)))
	   (just (sum 4)))
:equivalents (mappend (just (sum 2))
		      (mappend (just (sum 3))
			       (just (sum 4))))
,:test equal

;; 1st
#?(mappend (mempty) (1st (just :a))) => (just :a)
,:test equal
#?(mappend (1st (just :a)) (mempty)) => (just :a)
,:test equal
#?(mappend (1st(just #\a))(1st(just #\b))) => (just #\a)
,:test equal
#?(mappend (1st nothing)(1st (just #\b))) => (JUST #\b)
,:test equal
#?(mappend (1st (just #\a))(1st nothing)) => (JUST #\a)
,:test equal
#?(mconcat (the (list 1st)'(nothing (just 9)(just 10)))) => (JUST 9)
,:test equal

;;;; Examples:
;; List
#?(mappend '(1 2 3)'(4 5 6))
=> (1 2 3 4 5 6)
,:test equal

#?(mappend "one" (mappend "two" "three"))
=> "onetwothree"
,:test equal

#?(mappend (mappend "one" "two")
	   "three")
=> "onetwothree"
,:test equal

#?(mappend* "one" "two" "three")
=> "onetwothree"
,:test equal

#?(mappend "pang" (mempty))
=> "pang"
,:test equal

#?(mconcat '((1 2)(3 6)(9)))
=> (1 2 3 6 9)
,:test equal

;;; ANY

#?(mappend (any t)(any nil))
=> T
#?(mappend (mempty)(any t))
=> T

#?(mconcat (the (list any) '(nil nil nil t)))
=> T
#?(mappend (the any (mempty))(mempty)) => NIL

;;; ALL

#?(mappend (mempty)(all t))
=> T

#?(mappend (mempty)(all nil))
=> NIL

#?(mconcat (the (list all) '(t t t)))
=> T
#?(mconcat (the (list all) '(t t nil)))
=> NIL

;;; ORDERING
#?(mappend :lt :gt)
=> :LT

#?(mappend :gt :lt) => :gt
#?(mappend (mempty) :lt) => :lt
#?(mappend (mempty) :gt) => :GT

#?(defun length-compare(x y)
    (declare(type string x y))
    (mappend (compare (length x)(length y))
	     (compare x y)))
=> LENGTH-COMPARE
,:before(fmakunbound 'length-compare)

#?(length-compare "zen" "ants") => :LT
#?(length-compare "zen" "ant") => :GT

#?(defun length-compare2(x y)
    (declare(type string x y))
    (flet((vowels(x)
	    (count-if (curry find _ "aeiou")
		      x)))
      (declare(ftype (function(string)fixnum)vowels))
      (mappend* (compare (length x)(length y))
		(compare (vowels x)(vowels y))
		(compare x y))))
=> length-compare2
,:before (fmakunbound 'length-compare2)

#?(length-compare2 "zen" "anna") => :LT
#?(length-compare2 "zen" "ana") => :LT
#?(length-compare2 "zen" "ann") => :GT

;;; MAYBE
#?(mappend nothing (just "andy"))
=> (JUST "andy")
,:test equal

#?(mappend (just :lt)nothing)
=> (JUST :LT)
,:test equal

#?(mappend (just (sum 3))(just (sum 4)))
=> (JUST 7)
,:test equal


(requirements-about MEMPTY)

;;;; Description:

#+syntax
(MEMPTY) ; => result

;;;; Arguments and Values:

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAPPEND)

;;;; Description:

#+syntax
(MAPPEND #:g1 #:g2) ; => result

;;;; Arguments and Values:

; g1 := 

; g2 := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MCONCAT)

;;;; Description:

#+syntax
(MCONCAT #:g4) ; => result

;;;; Arguments and Values:

; g4 := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MONOID-P)

;;;; Description:

#+syntax
(MONOID-P #:arg) ; => result

;;;; Arguments and Values:

; arg := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

