(defpackage :tyclex.dsl.define-type-class.spec
  (:import-from :tyclex.dsl.define-type-class #:find-diverging-class)
  (:use :cl :jingoh :tyclex.dsl.define-type-class))
(in-package :tyclex.dsl.define-type-class.spec)
(setup :tyclex.dsl.define-type-class)

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

#?(find-diverging-class 'fixnum '(and integer (not evenp))) => INTEGER
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

(requirements-about DEFINE-TYPE-CLASS)

;;;; Description:

#+syntax
(DEFINE-TYPE-CLASS (name &rest type-var+) (&rest var-constraint*) signature+ &rest rest) ; => result

;;;; Arguments and Values:

; name := SYMBOL otherwise error.
#?(define-type-class("not symbol")()()) :signals error
,:lazy t

; type-var := SYMBOL otherwise error.
#?(define-type-class(dummy "not symbol")()()) :signals error
,:lazy t

; var-constraint := (type-class-name type-var)
; type-class-name := symbol otherwise error.
#?(define-type-class(dummy)("not symbol")()) :signals error
,:lazy t
; It must names already defined other type class, otherwise error.
#?(define-type-class(dummy)(not-exist)()) :signals error
,:lazy t

; signature := (interface arg-signatures type-pattern)

; interface := SYMBOL, otherwise error.
#?(define-type-class(dummy)()
    (("not symbol" () fixnum)))
:signals error
,:lazy t

; arg-signatures := (type-pattern*), otherwise error.
#?(define-type-class(dummy)()
    ((interface "not list" fixnum)))
:signals error
,:lazy t

; rest := (:default interface lambda-list body+)

; result := `NAME`

;;;; Affected By:
; State of TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Side-Effects:
; Modify TYCLEX.OBJECTS.TYPE-CLASS::*TYPE-CLASSES*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about COMPUTE-RETURN-TYPE)

;;;; Description:

#+syntax
(COMPUTE-RETURN-TYPE var &optional env) ; => result

;;;; Arguments and Values:

; var := S-Expression

; env := Environment-object

; result := type-pattern

;;;; Affected By:
; Lisp environment.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
;; Setups
#?(tyclex.dsl.defdata:defdata maybe(a)
    :nothing
    (just a))
=> MAYBE
,:before (fmakunbound 'just)

#?(defmacro env(&environment env)
    `',env)
=> ENV
,:before (fmakunbound 'env)

#?(define-type-class(yes-no a)()
    ((yes-no(a)boolean)))
=> YES-NO
,:before (mapc #'fmakunbound '(yes-no yes-no-p))

#?(tyclex.dsl.definstance:definstance(yes-no fixnum)
    ((yes-no(a)
       `(not(zerop ,a)))))
=> YES-NO

#?(tyclex.dsl.defio:defio(put-string((string string))null)
    (write-string string)(force-output)(values))
=> PUT-STRING
,:before (fmakunbound 'put-string)

;; Constants.
#?(compute-return-type ''symbol) => SYMBOL
#?(compute-return-type 0) => FIXNUM
#?(compute-return-type (1+ most-positive-fixnum)) => BIGNUM
#?(compute-return-type 1.3) => SINGLE-FLOAT
#?(compute-return-type 1.3d2) => DOUBLE-FLOAT
#?(compute-return-type 1/3) => RATIO
#?(compute-return-type #C(1 2)) => COMPLEX
#?(compute-return-type "string") => STRING
#?(compute-return-type #(vector)) => VECTOR
#?(compute-return-type #2A((1 2)(3 4))) => ARRAY
#?(compute-return-type #P"") => PATHNAME
#?(compute-return-type *standard-output*) => STREAM
#?(compute-return-type #'car) => FUNCTION
#?(compute-return-type nil) => NULL
#?(compute-return-type ''(1 2 3)) => (LIST FIXNUM)
,:test equal
#?(compute-return-type ''(1 . "")) => (CONS FIXNUM STRING)
,:test equal
#?(compute-return-type ''(1 "" #\a)) => (CONS FIXNUM (CONS STRING (CONS CHARACTER NULL)))
,:test equal
#?(compute-return-type ''(1 "" . #\a)) => (CONS FIXNUM (CONS STRING CHARACTER))
,:test equal
#?(compute-return-type '(the alias :key)) => ALIAS
#?(compute-return-type :nothing) => (MAYBE *)
,:test equal
#?(compute-return-type (just 0)) => (MAYBE FIXNUM)
,:test equal

;; Free variables.
#?(compute-return-type '*standard-output*) => STREAM
; Return type will be `T`, unless `DECLARE`ed.
#?(let((var 0))
    (declare(ignore var))
    (compute-return-type 'var (env)))
=> T
,:lazy t
#?(let((var 0))
    (declare (type fixnum var)
	     (ignore var))
    (compute-return-type 'var (env)))
=> FIXNUM
,:lazy t

;; Interface call.
#?(compute-return-type '(yes-no 0)) => BOOLEAN

;; IO call.
#?(compute-return-type '(put-string "string")) => (TYCLEX.OBJECTS.IO-ACTION:IO NULL)
,:test equal

;; ADT-CONSTRUCTOR call.
#?(compute-return-type '(just 0)) => (MAYBE FIXNUM)
,:test equal
#?(compute-return-type ':nothing) => (MAYBE *)
,:test equal

;; Curried function call.
#?(compute-return-type '(tyclex.curry:curry + _ _)) => (FUNCTION * FUNCTION)
,:test equal
#?(compute-return-type '(tyclex.curry:curry + 1 _)) => (FUNCTION * NUMBER)
,:test equal

;; Standard form.
#?(compute-return-type '(coerce original 'return)) => RETURN
#?(compute-return-type '(map 'return sequence)) => RETURN

#?(compute-return-type '(list 1 2 3)) => LIST

;; Special-operator form.
;; (progn progv let let* flet labels lambda setq locally eval-when)
#?(compute-return-type '(progn (vector 1 2 3)(list 1 2 3))) => LIST

;; (the)
#?(compute-return-type '(the fixnum (hoge))) => FIXNUM

;; (unwind-protect multiple-value-prog1 multiple-value-call load-time-value)
#?(compute-return-type '(unwind-protect(+ 1 2 3)
			  (clean-up)))
=> FIXNUM

;; (tagbody)
#?(compute-return-type '(tagbody :tag (+ 1 2 3))) => NULL

;; (function)
#?(compute-return-type '#'(lambda()(vector 1 2 3)(list 1 2 3))) => (FUNCTION * LIST)
,:test equal
#?(compute-return-type '#'+) => (FUNCTION(&REST NUMBER)(VALUES NUMBER &OPTIONAL))
,:test equal

;; (if)
; When both then clause and else clause are exists, great common type is returned.
#?(compute-return-type '(if(pred)
			  0
			  1.5))
=> REAL
; When one of clause's return type is NIL, (it means ERROR.)
; such clause's return type is ignored.
#?(compute-return-type '(if(pred)
			  (error "")
			  0))
=> FIXNUM
#?(compute-return-type '(if(pred)
			  0
			  (error "")))
=> FIXNUM
; When both are NIL, NIL is returned.
#?(compute-return-type '(if(pred)
			  (error 'condition-a)
			  (error 'condition-b)))
=> NIL

; (go throw catch)
; Return T.
#?(compute-return-type '(go :tag)) => T

; (block)
; Return great common type of last form and each `RETURN-FROM` form.
#?(compute-return-type '(block block
			       (return-from block 0)
			       1.5))
=> REAL

(requirements-about INFINITE-EXPANSION-DETECTER)

;;;; Description:

#+syntax
(INFINITE-EXPANSION-DETECTER expander form env) ; => result

;;;; Arguments and Values:

; expander := 

; form := 

; env := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

