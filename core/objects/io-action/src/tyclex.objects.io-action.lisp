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

(defun io-action-construct-form-p (thing)
  (and (listp thing) (every #'equal thing '(make-instance 'io-action))))

(defun io-action-construct-form-return-type (io-action-construct-form)
  (introspect-environment:constant-form-value
    (getf io-action-construct-form :type)))

(defun io-action-construct-form-function-form (io-action-construct-form)
  (getf io-action-construct-form :instance))

;;;; ACTION data structure.

(defstruct (action (:copier nil) (:predicate nil))
  (type (error 'tyclex.conditions:slot-uninitialized :name 'type) :type
   (cons (eql io) (cons * null)) :read-only t)
  (body (error 'tyclex.conditions:slot-uninitialized :name 'body) :type cons
   :read-only t)
  (lambda-list (error 'tyclex.conditions:slot-uninitialized :name 'lambda-list)
   :type list :read-only t))

(defvar *io-functions* (make-hash-table :test #'eq))

;; Trivial helpers

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

(defun add-io (name &rest args) (setf (get-io name) (apply #'make-action args)))

(defun remove-io (name) (remhash name *io-functions*))

(defun io-makunbound (symbol)
  (check-type symbol symbol)
  (if (get-io symbol nil)
      (progn (remove-io symbol) (fmakunbound symbol))
      symbol))

(defun io-boundp (symbol) (check-type symbol symbol) (get-io symbol nil))

(defun io-form-p (thing)
  (and (consp thing) (symbolp (car thing)) (io-boundp (car thing))))