(in-package :cl-user)
(defpackage :tyclex.conditions
  (:use :cl)
  (:export
    #:tyclex-condition
    #:tyclex-error

    #:slot-uninitialized
    #:missing
    #:redefinition-warning
    ))
(in-package :tyclex.conditions)

(define-condition tyclex-condition(condition)()
  (:documentation "Root condition for tyclex system."))
(define-condition tyclex-error(error tyclex-condition)()
  (:documentation "Root error for tyclex system."))

(define-condition slot-uninitialized(tyclex-error cell-error)()
  (:documentation "Signaled when slot is not initialized.")
  (:report (lambda(condition stream)
	     (format stream "~S uninitialized."
		     (cell-error-name condition)))))

(define-condition missing(tyclex-error cell-error)()
  (:documentation "Signaled when object is not found.")
  (:report (lambda(condition stream)
	     (format stream "~:(~A~) ~S."
		     (type-of condition)
		     (cell-error-name condition)))))

(define-condition redefinition-warning(style-warning cell-error tyclex-condition)()
  (:documentation "Signaled when object is redefined.")
  (:report (lambda(condition stream)
	     (format stream "~S is redefined."(cell-error-name condition)))))

