(defpackage :tyclex.type-matcher.spec
  (:import-from :tyclex.type-matcher #:find-diverging-class)
  (:use :cl :jingoh :tyclex.type-matcher #:tyclex.dsl.defdata))
(in-package :tyclex.type-matcher.spec)
(setup :tyclex.type-matcher)

(requirements-about TYPE-MATCH-P)

;;;; Description:

#+syntax
(TYPE-MATCH-P t1 t2 &optional reccursivep) ; => result

;;;; Arguments and Values:

; t1 := 

; t2 := 

; reccursivep := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:

#?(type-match-p 'fixnum 'float) => NIL
#?(type-match-p 'float 'fixnum) => NIL

;; vector
#?(type-match-p 'vector 'vector) => T
#?(type-match-p 'vector '(vector *)) => T
#?(type-match-p '(vector *) 'vector) => T
#?(type-match-p '(vector *) '(vector *)) => T
#?(type-match-p '(vector fixnum) '(vector character)) => NIL

#?(type-match-p 'vector '(?A ?B)) :satisfies identity
#?(type-match-p '(?A ?B) 'vector) :satisfies identity

#?(type-match-p 'vector 'sequence) => T
#?(type-match-p 'sequence 'vector) => NIL
;#?(type-match-p 'vector '(sequence *)) => T
#?(type-match-p '(sequence *) 'vector) => NIL
#?(type-match-p '(vector *) 'sequence) => T
#?(type-match-p 'sequence '(vector *)) => NIL
;#?(type-match-p '(vector *) '(sequence *)) => T
;#?(type-match-p '(sequence *) '(vector *)) => NIL

;#?(type-match-p 'vector 'array) => NIL
#?(type-match-p 'array 'vector) => NIL
;#?(type-match-p 'vector '(array *)) => NIL
#?(type-match-p '(array *) 'vector) => NIL
;#?(type-match-p '(vector *) 'array) => NIL
#?(type-match-p 'array '(vector *)) => NIL
;#?(type-match-p '(vector *) '(array *)) => NIL
#?(type-match-p '(array *) '(vector *)) => NIL

#?(type-match-p 'vector 'string) => NIL
;#?(type-match-p 'string 'vector) => NIL
#?(type-match-p 'vector '(string *)) => NIL
;#?(type-match-p '(string *) 'vector) => NIL
#?(type-match-p '(vector *) 'string) => NIL
;#?(type-match-p 'string '(vector *)) => NIL
#?(type-match-p '(vector *) '(string *)) => NIL
;#?(type-match-p '(string *) '(vector *)) => NIL

;; string
#?(type-match-p 'string 'string) => T
#?(type-match-p 'string '(string *)) => T
#?(type-match-p '(string *) 'string) => T
#?(type-match-p '(string *) '(string *)) => T
#?(type-match-p '(string fixnum) '(string character)) => NIL

#?(type-match-p 'string '(?A ?B)) :satisfies identity
#?(type-match-p '(?A ?B) 'string) :satisfies identity

#?(type-match-p 'string 'sequence) => T
;#?(type-match-p 'sequence 'string) => T
;#?(type-match-p 'string '(sequence *)) => T
;#?(type-match-p '(sequence *) 'string) => T
#?(type-match-p '(string *) 'sequence) => T
;#?(type-match-p 'sequence '(string *)) => T
;#?(type-match-p '(string *) '(sequence *)) => T
;#?(type-match-p '(sequence *) '(string *)) => T

;#?(type-match-p 'string 'array) => NIL
#?(type-match-p 'array 'string) => NIL
;#?(type-match-p 'string '(array *)) => NIL
#?(type-match-p '(array *) 'string) => NIL
;#?(type-match-p '(string *) 'array) => NIL
#?(type-match-p 'array '(string *)) => NIL
;#?(type-match-p '(string *) '(array *)) => NIL
#?(type-match-p '(array *) '(string *)) => NIL

;; list
#?(type-match-p 'null 'list) => T
#?(type-match-p 'list 'null) => T
#?(type-match-p 'null 'cons) => T
#?(type-match-p 'cons 'null) => T
#?(type-match-p 'null '(list fixnum)) => T
#?(type-match-p '(list fixnum) 'null) => T
#?(type-match-p 'null '(cons fixnum)) => T
#?(type-match-p '(cons fixnum) 'null) => T
#?(type-match-p '(cons fixnum) '(list fixnum)) => T
#?(type-match-p '(list fixnum) '(cons fixnum)) => T
#?(type-match-p '(list fixnum) '(list character)) => NIL

#?(type-match-p 'null 'symbol) => T
;#?(type-match-p 'symbol 'null) => T

(requirements-about FIND-DIVERGING-CLASS)

;;;; Description:

#+syntax
(FIND-DIVERGING-CLASS class type) ; => result

;;;; Arguments and Values:

; class := class-name, otherwise error.
#?(find-diverging-class 'bit 'fixnum) :signals error

; type := type-specifier, otherwise unspecified.
#?(find-diverging-class 'fixnum "not type specifier") => unspecified
#?(find-diverging-class 'fixnum 'not-type-specifier) => unspecified

; result := great common type specifier.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
#?(find-diverging-class 'warning 'error) => CONDITION
#?(find-diverging-class 'error 'warning) => CONDITION
#?(find-diverging-class 'vector 'string) => VECTOR
#?(find-diverging-class 'string 'vector) => VECTOR
#?(find-diverging-class 'list 'string) => SEQUENCE
#?(find-diverging-class 'string 'list) => SEQUENCE
#?(find-diverging-class 'null 'list) => LIST
#?(find-diverging-class 'null 'symbol) => SYMBOL
#?(find-diverging-class 'null 'vector) => SEQUENCE
#?(find-diverging-class 'vector 'null) => SEQUENCE
#?(find-diverging-class 'fixnum 'float) => REAL
#?(find-diverging-class 'float 'fixnum) => REAL
#?(find-diverging-class 'ratio 'float) => REAL
#?(find-diverging-class 'float 'ratio) => REAL

#?(find-diverging-class 'fixnum 'bit) => FIXNUM
#?(find-diverging-class 'fixnum 'function) => T

#?(find-diverging-class 'integer '(integer 0 4)) => INTEGER
#?(find-diverging-class 'fixnum '(integer 0 4)) => FIXNUM
#?(find-diverging-class 'fixnum `(integer 0 ,(1+ most-positive-fixnum))) => INTEGER
#?(find-diverging-class 'fixnum '(complex float)) => NUMBER
#?(find-diverging-class 'list '(string 3)) => SEQUENCE
#?(find-diverging-class 'vector '(string 3)) => VECTOR
#?(find-diverging-class 'vector '(array * 3)) => ARRAY
#?(find-diverging-class 'vector '(array *)) => (ARRAY *)
,:test equal

#?(find-diverging-class 'fixnum '(and integer (not (satisfies evenp)))) => INTEGER
#?(find-diverging-class 'fixnum '(or symbol fixnum)) => (OR SYMBOL FIXNUM)
,:test equal
#?(find-diverging-class 'string '(or symbol fixnum)) => T
#?(find-diverging-class 'symbol '(or keyword fixnum)) => T
#?(find-diverging-class 'list '(not list)) => T
#?(find-diverging-class 'vector '(not list)) => (NOT LIST)
,:test equal
#?(find-diverging-class 'symbol '(not list)) => T

#?(find-diverging-class 'symbol '(eql :hoge)) => SYMBOL
#?(find-diverging-class 'symbol '(member a b c)) => SYMBOL
#?(find-diverging-class 'symbol '(member 1 2 3)) => T
#?(find-diverging-class 'fixnum '(satisfies evenp)) => T

