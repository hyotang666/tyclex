(defpackage :tcl.applicative.spec
  (:use :tcl :jingoh :tcl.applicative #:tcl.io #:tcl.data))
(in-package :tcl.applicative.spec)
(setup :tcl.applicative)

(requirements-about PURE)

;;;; Description:

#+syntax
(PURE #:g0) ; => result

;;;; Arguments and Values:

; g0 := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about <*>)

;;;; Description:

#+syntax
(<*> #:g2 #:g3) ; => result

;;;; Arguments and Values:

; g2 := 

; g3 := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about APPLICATIVE-P)

;;;; Description:

#+syntax
(APPLICATIVE-P #:arg) ; => result

;;;; Arguments and Values:

; arg := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about <*>*)

;;;; Description:

#+syntax
(<*>* &rest body) ; => result

;;;; Arguments and Values:

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
;; list
#?(<*> (list (curry * 0 _)
	     (curry + 100 _)
	     (curry expt _ 2))
       '(1 2 3))
=> (0 0 0 101 102 103 1 4 9)
,:test equal

#?(<*>* (list (curry + _ _)
	      (curry * _ _))
	'(1 2)
	'(3 4))
=> (4 5 5 6 3 4 6 8)
,:test equal

#?(<$> (curry concatenate 'string _ _)
       '("ha" "heh" "hmm")
       '("?" "!" "."))
=> ("ha?" "ha!" "ha." "heh?" "heh!" "heh." "hmm?" "hmm!" "hmm.")
,:test equal

#?(<$> (curry * _ _)
       '(2 5 10)
       '(8 10 11))
=> (16 20 22 40 50 55 80 100 110)
,:test equal

#?(remove-if-not (curry > _ 50)
		 (<$> (curry * _ _)
		      '(2 5 10)
		      '(8 10 11)))
=> (55 80 100 110)
,:test equal

;; function
#?(funcall (<$> (curry + _ _)
		(curry + _ 3)
		(curry * _ 100))
	   5)
=> 508

#?(funcall (<$> (curry list _ _ _)
		(curry + _ 3)
		(curry * _ 2)
		(curry / _ 2))
	   5)
=> (8 10 5/2)
,:test equal

;; io
#?(<$> (curry concatenate 'string _ _)
       (get-line)
       (get-line))
:satisfies #`(with-input-from-string(*standard-input* (format nil "one~%two"))
	       (& (functionp $result)
		  (equal "onetwo"
			 (funcall $result))))

#?(lambda()
    (let((a (funcall (<$> (curry concatenate 'string _ _)
			  (get-line)
			  (get-line)))))
      (put-string-line (concatenate 'string
				    "The two lines concatenated turn out to be: "
				    a))))
:satisfies
#`(with-input-from-string(*standard-input*(format nil "one~%two"))
    (& (functionp $result)
       (equal #.(format nil "The two lines concatenated turn out to be: onetwo~%")
	      (with-output-to-string(*standard-output*)
		(funcall(funcall $result))))))

;; maybe
#?(<*> (just (curry + 3 _)) (just 9))
:satisfies #`(equal $result (just 12))

#?(<*> (just (curry + 3 _)) nothing)
=> NOTHING

#?(<*> (just (curry uiop:strcat _ "hahaha")) nothing)
=> NOTHING

#?(<*> nothing (just "woot"))
=> NOTHING

#?(<*> (pure (curry + 3 _)) (just 9))
:satisfies #`(equal $result (just 12))

#?(<*> (<*> (pure (curry + _ _))
	    (just 3))
       (just 5))
=> (JUST 8)
,:test equal

#?(<*> (<*> (pure (curry + _ _))
	    nothing)
       (just 5))
=> NOTHING

#?(<*>* (pure (curry + _ _))
	(just 3)
	(just 5))
=> (JUST 8)
,:test equal

#?(<$> (curry concatenate 'string _ _)
       (just "johntra")
       (just "volta"))
=> (just "johntravolta")
,:test equal
