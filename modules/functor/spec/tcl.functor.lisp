(tcl:defpackage :tcl.functor.spec
  (:use :tcl :jingoh :tcl.functor #:tcl.data #:tcl.io))
(in-package :tcl.functor.spec)
(setup :tcl.functor)

(requirements-about FMAP)

;;;; Description:

#+syntax
(FMAP #:g0 #:g1) ; => result

;;;; Arguments and Values:

; g0 := FUNCTION

; g1 := FUNCTOR

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
;; list
#?(fmap #'1+ (list 1 2 3)) => (2 3 4)
,:test equal
#?(fmap #'1+ (list)) => NIL

;; vector
#?(fmap #'1+ (vector 1 2 3)) => #(2 3 4)
,:test equalp
#?(fmap #'1+ (vector)) => #()
,:test equalp

;; string
#?(fmap #'char-upcase "hoge") => "HOGE"
,:test equal
#?(fmap #'char-upcase "") => ""
,:test equal

;; array
#?(fmap #'1+ #2A((1 2)(3 4))) => #2A((2 3)(4 5))
,:test equalp
#?(fmap #'1+ #0A0) => #0A1
,:test equalp

;; hash-table
#?(let((ht(the hash-table(alexandria:plist-hash-table '(:a 1 :b 2)))))
    (fmap #'1+ ht))
:satisfies #`(& (hash-table-p $result)
		(eql 2 (gethash :a $result))
		(eql 3 (gethash :b $result)))

#?(fmap #'1+ (make-hash-table))
:satisfies #`(& (hash-table-p $result)
		(eql 0 (hash-table-count $result)))

;; function
#?(fmap #'1+ #'1+)
:satisfies #`(& (functionp $result)
		(eql 3 (funcall $result 1)))
    
;; maybe
#?(fmap #'1+ (just 0)) => (just 1)
,:test equal
#?(fmap #'1+ nothing) => NOTHING

;; io
#?(fmap #'reverse (get-line))
:satisfies #`(with-input-from-string(*standard-input* "hoge")
	       (& (functionp $result)
		  (typep $result 'io-action)
		  (equal "egoh" (funcall $result))))

(requirements-about FUNCTOR-P)

;;;; Description:

#+syntax
(FUNCTOR-P #:arg) ; => result

;;;; Arguments and Values:

; arg := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

