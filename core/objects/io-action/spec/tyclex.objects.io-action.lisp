(defpackage :tyclex.objects.io-action.spec
  (:use :cl :jingoh :tyclex.objects.io-action))
(in-package :tyclex.objects.io-action.spec)
(setup :tyclex.objects.io-action)

(requirements-about IO-ACTION)
;;;; Description:
;;;; Compound Type Specifier Kind:

;;;; Compound Type Specifier Syntax:

;;;; Compound Type Specifier Arguments:

;;;; Compound Type Specifier Description:

(requirements-about IO-TYPE)

;;;; Description:

#+syntax
(IO-TYPE &rest sb-pcl::args) ; => result

;;;; Argument Precedence Order:
; sb-pcl::object

;;;; Method signature:
#+signature(IO-TYPE (IO-ACTION IO-ACTION))

;;;; Arguments and Values:

; args := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about IO)

;;;; Description:

#+syntax
(IO tyclex.newtype::arg) ; => result

;;;; Arguments and Values:

; arg := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ACTION)
;;;; Description:
;;;; Compound Type Specifier Kind:

;;;; Compound Type Specifier Syntax:

;;;; Compound Type Specifier Arguments:

;;;; Compound Type Specifier Description:

(requirements-about MAKE-ACTION)

;;;; Description:

#+syntax
(MAKE-ACTION &key ((:type #:type) (error "required"))) ; => result

;;;; Arguments and Values:

; type := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ACTION-TYPE)

;;;; Description:

#+syntax
(ACTION-TYPE sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-IO)

;;;; Description:

#+syntax
(ADD-IO name &rest args) ; => result

;;;; Arguments and Values:

; name := 

; args := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REMOVE-IO)

;;;; Description:

#+syntax
(REMOVE-IO name) ; => result

;;;; Arguments and Values:

; name := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about IO-BOUNDP)

;;;; Description:

#+syntax
(IO-BOUNDP symbol) ; => result

;;;; Arguments and Values:

; symbol := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about IO-MAKUNBOUND)

;;;; Description:

#+syntax
(IO-MAKUNBOUND symbol) ; => result

;;;; Arguments and Values:

; symbol := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

