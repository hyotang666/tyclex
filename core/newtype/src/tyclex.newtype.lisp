(in-package :cl-user)
(defpackage :tyclex.newtype
  (:use :cl)
  (:shadow #:list)
  (:export #:list)
  (:export ; main api
    #:define-newtype
    #:denew
    )
  (:export ; for hackers
    #:newtype-type-specifier-p #:add-newtype
    ))
(in-package :tyclex.newtype)

(setf(symbol-function 'list)#'cl:list)

(deftype list(&optional a)
  (declare(ignore a))
  'cl:list)
(declaim(ftype(function(&rest t)list)list))

(defvar *newtypes* (make-hash-table :test #'eq))

(defmacro define-newtype(name lambda-list &body body)
  ;; trivial-syntax-check.
  (assert(typep name '(and symbol (not (or keyword boolean)))))
  (assert(listp lambda-list))
  (assert(typep lambda-list '(or null
				 (cons * null)
				 (cons (eql &optional)(cons * null)))))
  ;; body
  (let((arg(gensym "ARG")))
    `(PROGN (ADD-NEWTYPE ',name)
	    (DEFTYPE ,name ,lambda-list ,@body)
	    (DEFMACRO,name(,arg)
	      `(THE ,',(enough-type-specifier name lambda-list arg) ,,arg)))))

(defun enough-type-specifier(name lambda-list var)
  (if(null lambda-list)
    name
    `(,name (uiop:symbol-call "TYCLEX.COMPUTE-RETURN-TYPE" "COMPUTE-RETURN-TYPE" ,var))))

(setf (symbol-function 'denew)#'third)

(defun newtype-type-specifier-p(type-specifier)
  (labels((ensure-symbol(thing)
	    (if(atom thing)
	      thing
	      (ensure-symbol(car thing)))))
    (values(gethash (ensure-symbol type-specifier) *newtypes*))))

(defun add-newtype(name)
  (check-type name (and symbol (not (or keyword boolean))))
  (setf (gethash name *newtypes*)t))
