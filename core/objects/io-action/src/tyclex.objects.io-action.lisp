(in-package :cl-user)

(defpackage :tyclex.objects.io-action
  (:use :cl)
  (:export ;; type-name
           #:io-action
           ;; reader
           #:io-type
           ;; Helpers
           #:io-action-construct-form-p
           #:io-action-construct-form-return-type
           #:io-action-construct-form-function-form)
  (:export ;; type-name
           #:io)
  (:export ;; type-name
           #:action
           ;; constructor
           #:make-action
           ;; reader
           #:action-type
           #:action-body
           #:action-lambda-list
           ;; helpers
           #:add-io
           #:remove-io
           #:io-boundp
           #:io-makunbound
           #:get-io
           #:io-form-p))

(in-package :tyclex.objects.io-action)

;;;; Conditions

(define-condition missing-io (tyclex.conditions:missing) ())

;;;; IO-ACTION in order to distinguish from plain function.

(defclass io-action ()
  ((instance :initarg :instance :reader action-of)
   (type :initarg :type :type (cons (eql io) (cons * null)) :reader io-type))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((c io-action) &key)
  (c2mop:set-funcallable-instance-function c (action-of c)))

;;;; IO-ACTION-CONSTRUCT-FORM-P

(declaim
 (ftype (function (list) (values boolean &optional))
        io-action-construct-form-p))

(defun io-action-construct-form-p (thing)
  (and (listp thing) (every #'equal thing '(make-instance 'io-action))))

;;;; IO-ACTION-CONSTRUCT-FORM-RETURN-TYPE

(declaim
 (ftype (function (symbol) (values t &optional))
        io-action-construct-form-return-type))

(defun io-action-construct-form-return-type (io-action-construct-form)
  (introspect-environment:constant-form-value
    (getf io-action-construct-form :type)))

;;;; IO-ACTION-CONSTRUCT-FORM-FUNCTION-FORM

(declaim
 (ftype (function (symbol) (values t &optional))
        io-action-construct-form-function-form))

(defun io-action-construct-form-function-form (io-action-construct-form)
  (getf io-action-construct-form :instance))

;;;; ACTION data structure.

(defstruct (action (:copier nil) (:predicate nil))
  (type (error 'tyclex.conditions:slot-uninitialized :name 'type)
        :type (cons (eql io) (cons * null))
        :read-only t)
  (body (error 'tyclex.conditions:slot-uninitialized :name 'body)
        :type cons
        :read-only t)
  (lambda-list (error 'tyclex.conditions:slot-uninitialized :name 'lambda-list)
               :type list
               :read-only t))

(defvar *io-functions* (make-hash-table :test #'eq))

;;;; Trivial helpers
;;;; GET-IO

(declaim
 (ftype (function (symbol &optional boolean)
         (values (or null action) &optional))
        get-io))

(defun get-io (name &optional errorp)
  (or (values (gethash name *io-functions*))
      (when errorp
        (error 'missing-io :name name))))

(defun (setf get-io) (new-value name &optional errorp)
  (declare (ignore errorp))
  (check-type name (and symbol (not (or keyword boolean))))
  (check-type new-value action)
  (when (get-io name nil)
    (warn 'tyclex.conditions:redefinition-warning :name name))
  (setf (gethash name *io-functions*) new-value))

;;;; ADD-IO

(declaim
 (ftype (function (symbol &rest (or symbol list)) (values action &optional))
        add-io))

(defun add-io (name &rest args) (setf (get-io name) (apply #'make-action args)))

;;;; REMOVE-IO

(declaim (ftype (function (*) (values boolean &optional)) remove-io))

(defun remove-io (name) (remhash name *io-functions*))

;;;; IO-MAKUNBOUND

(declaim (ftype (function (symbol) (values symbol &optional)) io-makunbound))

(defun io-makunbound (symbol)
  (check-type symbol symbol)
  (if (get-io symbol nil)
      (progn (remove-io symbol) (fmakunbound symbol))
      symbol))

;;;; IO-BOUNDP

(declaim
 (ftype (function (symbol) (values (or null action) &optional)) io-boundp))

(defun io-boundp (symbol) (get-io symbol nil))

;;;; IO-FORM-P

(declaim (ftype (function (*) (values (or null action) &optional)) io-form-p))

(defun io-form-p (thing)
  (and (consp thing) (symbolp (car thing)) (io-boundp (car thing))))
