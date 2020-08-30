(defpackage :tyclex.curry.spec
  (:use :cl :jingoh :tyclex.curry))
(in-package :tyclex.curry.spec)
(setup :tyclex.curry)

(requirements-about CURRY)
;;;; Description:
; CURRYied function

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:
; none

(requirements-about CURRY)

;;;; Description:
; Currying function and apply argment patially.
#?(curry + _ _)
:satisfies (lambda($result)
	     (& ; result is curried function.
	       (typep $result 'curry)
	       ; When one argument applied, curried function returned.
	       (typep (funcall $result 1) 'curry)
	       ; When all of arguments are applied, body form is evaluated.
	       (= 3 (funcall (funcall $result 1)
			     2))
	       ; We can apply arguments at once.
	       (= 3 (funcall $result 1 2))
	       ))

#+syntax
(CURRY op &rest args) ; => result

;;;; Arguments and Values:

; op := Function name, otherwise error
#?(curry #'+ _ _) :signals error
; Lambda form is not valid
#?(curry (lambda(x y)(+ x y)) _ _) :signals error

; args := Arguments for `OP`, when there is `_`, result `CURRY`ied function awaits it.
#?(curry + 1 2 _)
:satisfies (lambda($result)
	     (& (typep $result 'curry)
		(= 6 (funcall $result 3))))

; result := `CURRY`ied function.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FUNCTION-TYPE-OF)

;;;; Description:
; Alter `INTROSPECT-ENVIRONMENT:FUNCTION-TYPE`.

#+syntax
(FUNCTION-TYPE-OF name) ; => result

;;;; Arguments and Values:

; name := Symbol, otherwise condition.
#?(function-type-of "not symbol")
:signals condition
; When NAME is not set, return NIL.
#?(function-type-of '#:dummy) => NIL

; result := ftype function form, when `NAME` is `FUNCTION-TYPE`ed.

;;;; Affected By:
; Lisp environment.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FUNCTION-TYPE)

;;;; Description:
; Alter DECLAIM FTYPE.
#?(function-type dummy (fixnum) float)
:expanded-to (progn (setf (get 'dummy 'ftype)'(function(fixnum)float))
		    'dummy)

#+syntax
(FUNCTION-TYPE name args return) ; => result

;;;; Arguments and Values:

; name := Symbol, otherwise error.
#?(function-type "not symbol" * *) :signals error
,:lazy t

; args := [ * | list ], otherwise error.
#?(function-type dummy "not valid" *) :signals error
; For ftype form second element.

; return := type-specifier, otherwise unspecified.
#?(function-type dummy * "not type specifier") :signals error
#?(function-type dummy * not-type-specifier) => unspecified

; result := NAME

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify lisp environment.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DECURRY)

;;;; Description:
; Make efficient curry form.
#?(decurry (macroexpand-1 '(curry + _ _)) '(1 2))
=> (+ 1 2)
,:test equal
#?(decurry (macroexpand-1 '(curry + _ _ _)) '(1 2))
=> (let ((instance
           (make-instance 'curry
                          :arity 1
                          :return-type '(values number &optional))))
     (labels ((curry (&optional (var nil var-p))
                (if var-p
	            (+ 1 2 var)
	            instance)))
       '(curry + _ _ _)
       (c2mop:set-funcallable-instance-function instance #'curry)
       instance))
,:test jingoh::sexp=

#+syntax
(DECURRY form actual-args) ; => result

;;;; Arguments and Values:

; form := Expanded curry form.

; actual-args := argument list.

; result := form.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about RECURRY)

;;;; Description:
; Make efficient curry form.
#?(recurry '(curry + _ _ _) '(1 2))
=> (curry + 1 2 _)
,:test equal

#+syntax
(RECURRY curry-form actual-args) ; => result

;;;; Arguments and Values:

; curry-form := form.

; actual-args := argument list.

; result := form.

;;;; Affected By:
; none.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CURRY-FORM-P)

;;;; Description:
; T when form looks like curry form.
#?(curry-form-p '(curry + _ _)) => T
#?(curry-form-p 'curry) => NIL
#?(curry-form-p '(not-curry)) => NIL
#?(curry-form-p '(curry "invlid")) => T

#+syntax
(CURRY-FORM-P form) ; => result

;;;; Arguments and Values:

; form := form

; result := BOOLEAN.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about EXPANDED-CURRY-FORM-P)

;;;; Description:
; T when form looks like expanded curry form.
#?(expanded-curry-form-p (macroexpand-1 '(curry + _ _)))
=> T

#+syntax
(EXPANDED-CURRY-FORM-P form) ; => result

;;;; Arguments and Values:

; form := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about EXPANDED-CURRY-FORM-ARITY)

;;;; Description:
; Get arity from expanded curry form.
#?(expanded-curry-form-arity (macroexpand-1 '(curry + _ _)))
=> 2

#+syntax
(EXPANDED-CURRY-FORM-ARITY form) ; => result

;;;; Arguments and Values:

; form := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about EXPANDED-CURRY-FORM-RETURN-TYPE)

;;;; Description:
#?(expanded-curry-form-return-type (macroexpand-1 '(curry + _)))
=> number

#+syntax
(EXPANDED-CURRY-FORM-RETURN-TYPE form) ; => result

;;;; Arguments and Values:

; form := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CANONICALIZE-RETURN-TYPE)

;;;; Description:
#?(canonicalize-return-type '(values number &optional))
=> NUMBER

#+syntax
(CANONICALIZE-RETURN-TYPE return-type) ; => result

;;;; Arguments and Values:

; return-type := type-specifier

; result := type-specifier

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about N-ARG-PROMISED-CURRY :doc-type function)

;;;; Description:

#+syntax (N-ARG-PROMISED-CURRY n expanded) ; => result

;;;; Arguments and Values:

; n := (integer 0 *)
#?(N-ARG-PROMISED-CURRY "not integer" (MACROEXPAND-1 '(CURRY + _))) :signals condition
#?(N-ARG-PROMISED-CURRY -1 (MACROEXPAND-1 '(CURRY + _))) :signals condition

; expanded := curry-expanded-form, otherwise unspecified.
#?(n-arg-promised-curry 1 '(not curry expanded form)) => unspecified

; result := form which generate function.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

#?(N-ARG-PROMISED-CURRY 1 (MACROEXPAND-1 '(CURRY + _)))
=> (LAMBDA (x) (+ x))
, :test jingoh.tester:sexp=

#?(N-ARG-PROMISED-CURRY 1 (MACROEXPAND-1 '(CURRY + _ _)))
=> (LET ((curry-instance
          (MAKE-INSTANCE 'CURRY :ARITY 1 :RETURN-TYPE
                         '(VALUES NUMBER &OPTIONAL))))
     (LABELS ((curry (required &OPTIONAL (optional NIL optionalp))
                (IF optionalp
                    (+ required optional)
                    curry-instance)))
       '(CURRY + _ _)
       (SB-MOP:SET-FUNCALLABLE-INSTANCE-FUNCTION
        curry-instance
        #'curry)
       #'curry))
, :test jingoh.tester:sexp=

#?(N-ARG-PROMISED-CURRY 2 (MACROEXPAND-1 '(CURRY + _ _)))
=> (LAMBDA (x y) (+ x y))
, :test jingoh.tester:sexp=

#?(N-ARG-PROMISED-CURRY 1 (MACROEXPAND-1 '(CURRY + _ _ _)))
=> (LET ((curry-instance
          (MAKE-INSTANCE 'CURRY :ARITY 2 :RETURN-TYPE
                         '(VALUES NUMBER &OPTIONAL))))
     (LABELS ((curry
                  (x
                   &OPTIONAL (y NIL y-p)
                   (z NIL z-p))
                (IF y-p
                    (IF z-p
                        (+ x y z)
                        (LET ((curry-instance
                               (MAKE-INSTANCE 'CURRY :ARITY 1 :RETURN-TYPE
                                              '(VALUES NUMBER &OPTIONAL))))
                          (LABELS ((curry
                                       (&OPTIONAL (z NIL z-p))
                                     (IF z-p
                                         (+ x y z)
                                         curry-instance)))
                            '(CURRY + _ _ _)
                            (SB-MOP:SET-FUNCALLABLE-INSTANCE-FUNCTION
                             curry-instance
                             #'curry)
                            curry-instance)))
                    curry-instance)))
       '(CURRY + _ _ _)
       (SB-MOP:SET-FUNCALLABLE-INSTANCE-FUNCTION
        curry-instance
        #'curry)
       #'curry))
, :test jingoh.tester:sexp=
