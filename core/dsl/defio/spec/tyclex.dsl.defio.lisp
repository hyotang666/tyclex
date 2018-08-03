(defpackage :tyclex.dsl.defio.spec
  (:use :cl :jingoh :tyclex.dsl.defio))
(in-package :tyclex.dsl.defio.spec)
(setup :tyclex.dsl.defio)

(requirements-about DEFIO)

;;;; Description:
; Define IO function.
#?(defio(put-string((string string))null)
    (write-string string)(force-output)(values))
:expanded-to
(LOCALLY
  #+sbcl(DECLARE (SB-EXT:MUFFLE-CONDITIONS SB-EXT:COMPILER-NOTE))
  (PROCLAIM'(FTYPE(FUNCTION(STRING)(TYCLEX.OBJECTS.IO-ACTION:IO NULL))PUT-STRING))
  (TYCLEX.OBJECTS.IO-ACTION:ADD-IO 'PUT-STRING :TYPE '(TYCLEX.OBJECTS.IO-ACTION:IO NULL))
  (DEFUN PUT-STRING(STRING)
    (MAKE-INSTANCE 'TYCLEX.OBJECTS.IO-ACTION:IO-ACTION
		   :INSTANCE (LAMBDA()
			       (WRITE-STRING STRING)
			       (FORCE-OUTPUT)
			       (VALUES))
		   :TYPE '(TYCLEX.OBJECTS.IO-ACTION:IO NULL))))

#+syntax
(DEFIO (name signature return) &body body) ; => result

;;;; Arguments and Values:

; name := Symbol, otherwise error.
#?(defio("not symbol"((string string))null)
    (write-string string)(force-output)(values))
:signals error

; signature := ([var type]*)
; var := symbol, otherwise error.
#?(defio(put-string(("not symbol" string))null)
    (write-string string)(force-output)(values))
:signals error

; type := type specifier, otherwise unspecified.
#?(defio(put-string((string not-type-specifier))null)
    (write-string string)(force-output)(values))
=> unspecified

; return := type specifier, otherwise unspecified.
#?(defio(put-string((string string))not-type-specifier)
    (write-string string)(force-output)(values))
=> unspecified

; body := S-Expressions

; result := `NAME`.

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify lisp environment.

;;;; Notes:

;;;; Exceptional-Situations:

