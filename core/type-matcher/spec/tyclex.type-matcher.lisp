(defpackage :tyclex.type-matcher.spec
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
#?(type-match-p 'sequence 'vector) => T
#?(type-match-p 'vector '(sequence *)) => T
#?(type-match-p '(sequence *) 'vector) => T
#?(type-match-p '(vector *) 'sequence) => T
#?(type-match-p 'sequence '(vector *)) => T
#?(type-match-p '(vector *) '(sequence *)) => T
#?(type-match-p '(sequence *) '(vector *)) => T

#?(type-match-p 'vector 'array) => NIL
#?(type-match-p 'array 'vector) => NIL
#?(type-match-p 'vector '(array *)) => NIL
#?(type-match-p '(array *) 'vector) => NIL
#?(type-match-p '(vector *) 'array) => NIL
#?(type-match-p 'array '(vector *)) => NIL
#?(type-match-p '(vector *) '(array *)) => NIL
#?(type-match-p '(array *) '(vector *)) => NIL

#?(type-match-p 'vector 'string) => NIL
#?(type-match-p 'string 'vector) => NIL
#?(type-match-p 'vector '(string *)) => NIL
#?(type-match-p '(string *) 'vector) => NIL
#?(type-match-p '(vector *) 'string) => NIL
#?(type-match-p 'string '(vector *)) => NIL
#?(type-match-p '(vector *) '(string *)) => NIL
#?(type-match-p '(string *) '(vector *)) => NIL

;; string
#?(type-match-p 'string 'string) => T
#?(type-match-p 'string '(string *)) => T
#?(type-match-p '(string *) 'string) => T
#?(type-match-p '(string *) '(string *)) => T
#?(type-match-p '(string fixnum) '(string character)) => NIL

#?(type-match-p 'string '(?A ?B)) :satisfies identity
#?(type-match-p '(?A ?B) 'string) :satisfies identity

#?(type-match-p 'string 'sequence) => T
#?(type-match-p 'sequence 'string) => T
#?(type-match-p 'string '(sequence *)) => T
#?(type-match-p '(sequence *) 'string) => T
#?(type-match-p '(string *) 'sequence) => T
#?(type-match-p 'sequence '(string *)) => T
#?(type-match-p '(string *) '(sequence *)) => T
#?(type-match-p '(sequence *) '(string *)) => T

#?(type-match-p 'string 'array) => NIL
#?(type-match-p 'array 'string) => NIL
#?(type-match-p 'string '(array *)) => NIL
#?(type-match-p '(array *) 'string) => NIL
#?(type-match-p '(string *) 'array) => NIL
#?(type-match-p 'array '(string *)) => NIL
#?(type-match-p '(string *) '(array *)) => NIL
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
#?(type-match-p 'symbol 'null) => T
