(in-package :cl-user)
(defpackage :tyclex.objects.io-action
  (:use :cl)
  (:export
    ;; type-name
    #:io-action
    ;; reader
    #:io-type
    ;; Helpers
    #:io-action-construct-form-p #:io-action-construct-form-return-type
    )
  (:export
    ;; type-name
    #:io
    )
  (:export
    ;; type-name
    #:action
    ;; constructor
    #:make-action
    ;; reader
    #:action-type #:action-body #:action-lambda-list
    ;; helpers
    #:add-io #:remove-io #:io-boundp #:io-makunbound #:find-io #:io-form-p
    )
  )
(in-package :tyclex.objects.io-action)

;;;; IO-ACTION in order to distinguish from plain function.
(defclass io-action ()
  ((instance :initarg :instance :reader action-of)
   (type :initarg :type :type (cons (eql io)(cons * null)) :reader io-type))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((c io-action) &key)
  (c2mop:set-funcallable-instance-function c (action-of c)))

(defun io-action-construct-form-p(thing)
  (and (listp thing)
       (every #'equal thing '(make-instance 'io-action))))

(defun io-action-construct-form-return-type(io-action-construct-form)
  (getf io-action-construct-form :type))

;;; IO type constructor.
(tyclex.newtype:define-newtype io (a)
  (declare(ignore a))
  'io-action)

;;;; ACTION data structure.
(defstruct(action (:copier nil)(:predicate nil))
  (type (error "required") :type (cons (eql io)(cons * null)) :read-only t)
  (body (error "required") :type cons :read-only t)
  (lambda-list (error "required") :type list :read-only t))

(defvar *io-functions* (make-hash-table :test #'eq))

;; Trivial helpers

(defun add-io(name &rest args)
  (setf(gethash name *io-functions*)(apply #'make-action args)))

(defun remove-io(name)
  (check-type name (and symbol (not (or keyword boolean))))
  (remhash name *io-functions*))

(defun find-io(name &optional errorp)
  (or (gethash name *io-functions*)
      (when errorp
	(error "Missing io named ~S" name))))

(defun io-boundp(symbol)
  (check-type symbol symbol)
  (values(gethash symbol *io-functions*)))

(defun io-makunbound(symbol)
  (remove-io symbol)
  (fmakunbound symbol))

(defun io-form-p(thing)
  (and (listp thing)
       (symbolp (car thing))
       (io-boundp (car thing))))
