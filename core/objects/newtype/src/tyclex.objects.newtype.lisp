(in-package :cl-user)

(defpackage :tyclex.objects.newtype
  (:use :cl)
  (:shadow #:list)
  (:export #:list)
  (:export ;; object.
           ;; As type name.
           #:newtype
           ;; Constructor
           #:make-newtype
           ;; Readers
           #:newtype-name
           #:newtype-lambda-list
           ;; Predicate.
           #:newtypep)
  (:export ;; Helpers
           #:add-newtype
           #:remove-newtype
           #:find-newtype
           #:newtype-type-specifier-p)
  (:export ;; Conditions
           #:missing-newtype))

(in-package :tyclex.objects.newtype)

;;;; CONDITIONS

(define-condition missing-newtype (tyclex.conditions::missing) ())

;;;; LIST
;;; as function.

(declaim (ftype (function (&rest t) (values list &optional)) list))

(setf (symbol-function 'list) #'cl:list)

;;; as type.

(deftype list (&optional a) (declare (ignore a)) 'cl:list)

;;;; Backend database.

(defvar *newtypes* (make-hash-table :test #'eq))

;;;; NEWTYPE object.

(defstruct (newtype (:predicate newtypep))
  (name (error 'tyclex.conditions:slot-uninitialized :name 'name)
        :type (and symbol (not (or keyword boolean)))
        :read-only t)
  (lambda-list nil :type list :read-only t))

;;;; ADD-NEWTYPE

(declaim
 (ftype (function
         ((and symbol (not (or keyword boolean))) &key (:lambda-list list))
         (values newtype &optional))
        add-newtype))

(defun add-newtype (name &key lambda-list)
  (setf (gethash name *newtypes*)
          (make-newtype :name name :lambda-list lambda-list)))

;;;; REMOVE-NEWTYPE

(declaim (ftype (function (symbol) (values boolean &optional)) remove-newtype))

(defun remove-newtype (symbol) (remhash symbol *newtypes*))

;;;; FIND-NEWTYPE

(declaim
 (ftype (function (t &optional boolean) (values (or null newtype) &optional))
        find-newtype))

(defun find-newtype (name &optional (errorp t))
  (or (and (newtypep name) name)
      (values (gethash name *newtypes*))
      (when errorp
        (error 'missing-newtype :name name))))

;;;; NEWTYPE-TYPE-SPECIFIER-P

(declaim
 (ftype (function (*) (values (or null newtype) &optional))
        newtype-type-specifier-p))

(defun newtype-type-specifier-p (type-specifier)
  (labels ((ensure-symbol (thing)
             (if (atom thing)
                 thing
                 (ensure-symbol (car thing)))))
    (find-newtype (ensure-symbol type-specifier) nil)))
