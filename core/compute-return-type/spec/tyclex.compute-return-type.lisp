(defpackage :tyclex.compute-return-type.spec
  (:use :cl :jingoh :tyclex.compute-return-type))
(in-package :tyclex.compute-return-type.spec)
(setup :tyclex.compute-return-type)

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
#?(defmacro env(&environment env)
    `',env)
=> ENV
,:before (fmakunbound 'env)

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
#?(compute-return-type ''(1 2 3)) => (TYCLEX.OBJECTS.NEWTYPE:LIST FIXNUM)
,:test equal
#?(compute-return-type ''(1 . "")) => (CONS FIXNUM STRING)
,:test equal
#?(compute-return-type ''(1 "" #\a)) => (CONS FIXNUM (CONS STRING (CONS CHARACTER NULL)))
,:test equal
#?(compute-return-type ''(1 "" . #\a)) => (CONS FIXNUM (CONS STRING CHARACTER))
,:test equal
#?(compute-return-type '(the keyword :key)) => KEYWORD

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
#?(compute-return-type '#'+) => (FUNCTION(&REST NUMBER) NUMBER)
,:test equal
; With FUNCTION-TYPEed.
#?(tyclex.curry:function-type name (args) return)
=> NAME
#?(compute-return-type '(name)) => RETURN
#?(tyclex.curry:function-type name (args) (values return &optional))
=> NAME
#?(compute-return-type '(name)) => RETURN

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

;;;; Corner cases.
;;; Espaecially in sbcl.
#?(compute-return-type (expander:expand '(make-hash-table)))
=> hash-table
