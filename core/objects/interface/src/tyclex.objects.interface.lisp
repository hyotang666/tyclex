(in-package :cl-user)

(defpackage :tyclex.objects.interface
  (:use :cl)
  (:export ;; as type name
           #:interface
           ;; as constructor
           #:make-interface
           ;; predicate
           #:interfacep
           ;; readers
           #:interface-lambda-list
           #:interface-return-type
           #:interface-type-class
           #:interface-default
           ;; accessor
           #:interface-instances
           ;; helpers
           #:find-interface
           #:add-interface
           #:remove-interface
           #:augment-instances
           #:interface-boundp
           #:interface-form-p
           #:interface-makunbound))

(in-package :tyclex.objects.interface)

;;;; Condition

(define-condition missing-interface (tyclex.conditions:missing) ())

(define-condition signature-conflicted (style-warning tyclex.conditions:tyclex-condition)
  ((cell :initarg :cell :reader conflicted-cell)
   (old :initarg :old :reader conflicted-old))
  (:report
   (lambda (condition stream)
     (format stream "~:(~A~): ~S ~S" (type-of condition)
             (conflicted-cell condition) (conflicted-old condition)))))

;;;; INTERFACE OBJECT

(defstruct (interface (:copier nil) (:predicate interfacep) (:conc-name nil))
  "Represents type class interface"
  (lambda-list nil :type list :read-only t)
  (return-type nil :type (or list symbol) :read-only t)
  (type-class (error 'tyclex.conditions:slot-uninitialized :name 'type-class)
              :type symbol
              :read-only t)
  (instances nil :type list)
  (default nil :type list :read-only t))

(defvar *interfaces* (make-hash-table :test #'eq))

;; helper
;;;; FIND-ITNERFACE

(deftype interface-designator ()
  '(or interface (and symbol (not (or keyword boolean)))))

(declaim
 (ftype (function (interface-designator &optional boolean)
         (values (or null interface) &optional))
        find-interface))

(defun find-interface (interface &optional (errorp t))
  (if (interfacep interface)
      interface
      (or (gethash interface *interfaces*)
          (when errorp
            (error 'missing-interface :name interface)))))

;;;; ADD-INTERFACE

(declaim
 (ftype (function
         ((and symbol (not (or keyword boolean))) &rest (or symbol list)))
        add-interface))

(defun add-interface (interface &rest args)
  (setf (gethash interface *interfaces*) (apply #'make-interface args)))

;;;; REMOVE-INTERFACE

(declaim
 (ftype (function ((and symbol (not (or keyword boolean))))
         (values boolean &optional))
        remove-interface))

(defun remove-interface (interface) (remhash interface *interfaces*))

;;;; AUGMENT-INSTANCES

(declaim
 (ftype (function
         (interface-designator tyclex.objects.instance:instance &key
          (:test function))
         (values list &optional))
        augment-instances))

(defun augment-instances (interface cell &key (test #'eql))
  (let ((exists? (find cell (interface-instances interface) :test test)))
    (if (not exists?)
        (push cell (interface-instances interface))
        (progn
         (warn 'signature-conflicted :cell cell :old exists?)
         (setf (interface-instances interface)
                 (cons cell
                       (delete cell (interface-instances interface)
                               :test test
                               :count 1)))))))

;;;; INTERFACE-BOUNDP

(declaim
 (ftype (function ((and symbol (not (or keyword boolean))))
         (values (or null interface) &optional))
        interface-boundp))

(defun interface-boundp (symbol) (find-interface symbol nil))

;;;; INTERFACE-FORM-P

(declaim
 (ftype (function (*) (values (or null interface) &optional)) interface-form-p))

(defun interface-form-p (thing)
  (and (listp thing) (symbolp (car thing)) (find-interface (car thing) nil)))

;;;; INTERFACE-MAKUNBOUND

(declaim
 (ftype (function (symbol) (values symbol &optional)) interface-makunbound))

(defun interface-makunbound (symbol)
  (when (find-interface symbol nil)
    (remove-interface symbol)
    (fmakunbound symbol)))

;;;; EASY READERS

(declaim
 (ftype (function (interface-designator) (values list &optional))
        interface-lambda-list
        interface-default
        interface-instances)
 (ftype (function (interface-designator) (values symbol &optional))
        interface-type-class)
 (ftype (function (interface-designator) (values (or symbol list) &optional))
        interface-return-type))

(defun interface-lambda-list (interface)
  (lambda-list (find-interface interface)))

(defun interface-return-type (interface)
  (return-type (find-interface interface)))

(defun interface-type-class (interface) (type-class (find-interface interface)))

(defun interface-default (interface) (default (find-interface interface)))

;;;; INSTANCE-TABLE

(defun interface-instances (interface) (instances (find-interface interface)))

(defun (setf interface-instances) (new interface)
  (setf (instances (find-interface interface)) new))
