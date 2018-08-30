(in-package :cl-user)
(defpackage :tyclex.objects.adt
  (:use :cl)
  (:import-from :tyclex.objects.adt-constructor
		#:first-atom #:adt-constructor-makunbound)
  (:export
    ;; type-name
    #:adt
    ;; constructor
    #:make-adt
    ;; readers
    #:adt-constructors #:adt-lambda-list
    ;; helpers
    #:add-adt #:remove-adt #:get-adt #:adt-type-specifier-p
    ;; conditions
    #:missing-adt
    )
  )
(in-package :tyclex.objects.adt)

;;;; Conditions.
(define-condition missing-adt(tyclex.conditions:missing)())

;;;; object ADT
;;;; constructor MAKE-ADT
;;;; slot-readers ADT-CONSTRUCTORS, ADT-LAMBDA-LIST
(defstruct (adt (:copier nil)(:predicate nil))
  (constructors (error 'tyclex.conditions:slot-uninitialized :name 'constructors)
		:type list	:read-only t)
  (lambda-list	(error 'tyclex.conditions:slot-uninitialized :name 'lambda-list)
		:type list	:read-only t))

; underlying database.
(defvar *adts* (make-hash-table :test #'eq))

;; Trivial helpers
(defun get-adt(name &optional(errorp t))
  (or (gethash (First-atom name) *adts*)
      (when errorp
	(error 'missing-adt :name name))))

(defun (setf get-adt)(new-value name &optional errorp)
  (declare(ignore errorp))
  (check-type name (and symbol (not (or keyword boolean))))
  (check-type new-value adt)
  (when(get-adt name nil)
    (warn 'tyclex.conditions:redefinition-warning :name name))
  (setf (gethash name *adts*)new-value))

(defun add-adt(name &rest args)
  (setf (get-adt name)
	(apply #'make-adt args)))

(defun remove-adt(name)
  (check-type name symbol)
  (let((adt(get-adt name nil)))
    (when adt
      (mapc #'Adt-constructor-makunbound (adt-constructors adt))
      (remhash name *adts*))))

(defun adt-type-specifier-p(thing)
  (let((adt(get-adt thing nil)))
    (when adt
      (let*((form(cdr(lflatten (if (symbolp thing)
				 ()
				 thing))))
	    (lambda-list(adt-lambda-list adt)))
	(<= (length form)(length lambda-list))))))

(defun lflatten(list)
  (labels((rec(list &optional acc)
	    (typecase(car list)
	      (null (apply #'append acc))
	      (atom (apply #'append (cons list acc)))
	      (t (rec (car list)(cons (cdr list)acc))))))
    (rec list)))
