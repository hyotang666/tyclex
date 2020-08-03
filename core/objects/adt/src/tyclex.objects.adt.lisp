(in-package :cl-user)

(defpackage :tyclex.objects.adt
  (:use :cl)
  (:import-from :tyclex.objects.adt-constructor
                #:first-atom
                #:adt-constructor-makunbound
                #:get-adt-constructor
                #:adt-constructor-arg-types
                #:adt-constructor-type-of)
  (:import-from :tyclex.objects.io-action #:io-action #:io-type)
  (:export ;; type-name
           #:adt
           ;; constructor
           #:make-adt
           ;; readers
           #:adt-constructors
           #:adt-lambda-list
           ;; helpers
           #:add-adt
           #:remove-adt
           #:get-adt
           #:adt-type-specifier-p
           #:adt-value-p
           #:data-type-of
           #:class-name-of
           #:cons-type-specifier
           ;; conditions
           #:missing-adt))

(in-package :tyclex.objects.adt)

;;;; Conditions.

(define-condition missing-adt (tyclex.conditions:missing) ())

;;;; object ADT
;;;; constructor MAKE-ADT
;;;; slot-readers ADT-CONSTRUCTORS, ADT-LAMBDA-LIST

(defstruct (adt (:copier nil) (:predicate nil))
  (constructors
   (error 'tyclex.conditions:slot-uninitialized :name 'constructors) :type list
   :read-only t)
  (lambda-list (error 'tyclex.conditions:slot-uninitialized :name 'lambda-list)
   :type list :read-only t)) ; underlying database.

(defvar *adts* (make-hash-table :test #'eq))

;; Trivial helpers

(defun get-adt (name &optional (errorp t))
  (or (gethash (first-atom name) *adts*)
      (when errorp
        (error 'missing-adt :name name))))

(defun (setf get-adt) (new-value name &optional errorp)
  (declare (ignore errorp))
  (check-type name (and symbol (not (or keyword boolean))))
  (check-type new-value adt)
  (when (get-adt name nil)
    (warn 'tyclex.conditions:redefinition-warning :name name))
  (setf (gethash name *adts*) new-value))

(defun add-adt (name &rest args) (setf (get-adt name) (apply #'make-adt args)))

(push 'tyclex.conditions:redefinition-warning uiop:*uninteresting-conditions*)

(defun remove-adt (name)
  (check-type name symbol)
  (let ((adt (get-adt name nil)))
    (when adt
      (mapc #'adt-constructor-makunbound (adt-constructors adt))
      (remhash name *adts*))))

(defun adt-type-specifier-p (thing)
  (let ((adt (get-adt thing nil)))
    (when adt
      (let* ((form
              (cdr
                (lflatten
                  (if (symbolp thing)
                      nil
                      thing))))
             (lambda-list (adt-lambda-list adt)))
        (<= (length form) (length lambda-list))))))

(defun lflatten (thing)
  (labels ((rec (list &optional acc)
             (typecase (car list)
               (null (apply #'append (cdr list) acc))
               (atom (apply #'append (cons list acc)))
               (t (rec (car list) (cons (cdr list) acc))))))
    (if (atom thing)
        thing
        (rec thing))))

(defun adt-value-p (thing)
  (let ((adt-constructor (get-adt-constructor thing nil)))
    (when adt-constructor
      (values
        (if (symbolp thing) ; nullary constructor.
            (tyclex.unifier:make-empty-environment)
            (tyclex.unifier:ignore-unification-failure
             (tyclex.unifier:unify (adt-constructor-arg-types adt-constructor)
                                   (mapcar #'data-type-of (cdr thing)))))
        adt-constructor))))

(defun data-type-of (thing)
  (multiple-value-bind (env adt-constructor)
      (adt-value-p thing)
    (if env
        (with-accessors ((arg-types adt-constructor-arg-types)
                         (type-of adt-constructor-type-of))
            adt-constructor
          (if (null arg-types)
              type-of
              (cons (car type-of)
                    (mapcar
                      (lambda (elt)
                        (tyclex.unifier:find-variable-value elt env))
                      (adt-lambda-list (get-adt type-of))))))
        (typecase thing
          (io-action (io-type thing))
          (function
           (let ((name (millet:function-name thing)))
             (if name
                 (introspect-environment:function-type name)
                 'function)))
          (t (class-name-of thing))))))

(defun class-name-of (thing)
  (let ((name (class-name (class-of thing))))
    ;; Canonicalize.
    (cond ((subtypep name 'string) (setf name 'string))
          ((subtypep name 'stream) (setf name 'stream))
          ((subtypep name 'vector)
           (if (subtypep name 'bit-vector)
               (setf name 'bit-vector)
               (setf name 'vector)))
          ((subtypep name 'array) (setf name 'array))
          ((subtypep name 'character) ; especially ccl needs.
           (setf name 'character))
          ((subtypep name 'symbol) ; especially ccl needs.
           (unless (eq 'null name)
             (setf name 'symbol))))
    ;; Return value.
    name))

(defun cons-type-specifier (types)
  (if (atom types)
      (if (null types)
          'null
          types)
      `(cons ,(car types) ,(cons-type-specifier (cdr types)))))
