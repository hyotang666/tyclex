(in-package :cl-user)

(defpackage :tyclex.dsl.defdata
  (:use :cl)
  (:import-from :tyclex.objects.type-class
                #:find-type-class
                #:type-class-constraints
                #:type-class-interfaces)
  (:import-from :tyclex.objects.adt
                #:adt-value-p
                #:cons-type-specifier
                #:add-adt
                #:get-adt
                #:adt-lambda-list
                #:adt-constructors)
  (:import-from :tyclex.objects.adt-constructor
                #:add-adt-constructor
                #:get-adt-constructor
                #:adt-constructor-arg-types
                #:adt-constructor-type-of)
  (:import-from :tyclex.objects.interface #:interface-default)
  (:import-from :tyclex.objects.io-action #:io-action #:io-type)
  (:import-from :tyclex.dsl.definstance #:definstance)
  (:export ;; Main API
           #:defdata
           ;; Helpers
           #:data-order))

(in-package :tyclex.dsl.defdata)

;;;; DEFDATA

(defmacro defdata (name&options lambda-list &rest constructor*)
  ;; binding
  (destructuring-bind
      (name &key deriving)
      (alexandria:ensure-list name&options)
    ;; trivial syntax check.
    (check-type name valid-name)
    (assert (every #'find-type-class deriving))
    (assert (every (lambda (x) (typep x 'valid-name)) lambda-list))
    (assert (notany (lambda (x) (find x lambda-list-keywords :test #'eq))
                    lambda-list))
    (dolist (constructor constructor*)
      (assert (symbolp (alexandria:ensure-car constructor))))
    ;; body
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(<deftype> name lambda-list constructor*)
       ,(<add-adt> name lambda-list constructor*)
       ,@(mapcan
           (lambda (constructor) (<constructors> name lambda-list constructor))
           constructor*)
       ,@(loop :for c :in constructor*
               :collect (<add-adt-constructor> c lambda-list name))
       ,@(mapcan #'<pattern-matcher> constructor*)
       ,@(loop :for tc :in deriving
               :append (<derivings> tc name))
       ',name)))

;;; VALID-NAME

(deftype valid-name () '(and symbol (not (or keyword boolean))))

;;; <deftype>

(defun <deftype> (name lambda-list constructor*)
  (labels ((type-name (args constructor)
             (cond ((symbolp constructor) `'(eql ,constructor))
                   (args (comma-type-specifier args constructor))
                   ((list-constructor-p constructor)
                    `',(cons-type-specifier
                         `((eql ,(car constructor)) ,@(cdr constructor))))
                   (t
                     `',(cons-type-specifier
                          `((eql ,(car constructor))
                            ,@(loop :for clause :in (cdr constructor)
                                    :collect (typecase clause
                                               (symbol t)
                                               (list
                                                (getf clause :type t))))))))))
    `(deftype ,name (&optional ,@lambda-list)
       (list 'or
             ,@(mapcar
                 (lambda (constructor) (type-name lambda-list constructor))
                 constructor*)))))

(defun comma-type-specifier (args constructor)
  ``(cons (eql ,',(car constructor))
          ,,(labels ((rec (types)
                       (if (endp types)
                           ''null
                           `(list 'cons
                                  ,(let ((arg (car types)))
                                     (cond ((find arg args :test #'eq) arg)
                                           ((millet:type-specifier-p arg)
                                            `',arg)
                                           (t
                                             `(list ',(car arg) ,@(cdr arg)))))
                                  ,(rec (cdr types))))))
              (rec (cdr constructor)))))

(defun list-constructor-p (constructor)
  (every #'millet:type-specifier-p (cdr constructor)))

;;; <add-adt>

(defun <add-adt> (name lambda-list constructor*)
  `(add-adt ',name
            :constructors ',(mapcar #'alexandria:ensure-car constructor*)
            :lambda-list ',(mapcar
                             (lambda (elt)
                               (if (millet:type-specifier-p elt)
                                   elt
                                   (tyclex.unifier:envar elt)))
                             lambda-list)))

;;; <constructors>

(defun <constructors> (name args constructor)
  (labels ((arg-types (lambda-list arg-types &optional acc)
             (if (endp arg-types)
                 (nreverse acc)
                 (if (find (car arg-types) lambda-list :test #'eq)
                     (arg-types lambda-list (cdr arg-types) (push t acc))
                     (arg-types lambda-list (cdr arg-types)
                                (push (car arg-types) acc))))))
    (cond
     ((symbolp constructor)
      (unless (keywordp constructor)
        `((alexandria:define-constant ,constructor ',constructor))))
     ((or args (list-constructor-p constructor))
      (let ((lambda-list
             (alexandria:make-gensym-list (length (cdr constructor)))))
        `(,@(if args
                `((declaim
                   (ftype (function ,(arg-types args (cdr constructor))
                           ,(constructor-return-type name))
                          ,(constructor-name constructor))))
                `((declaim
                   (ftype (function ,(cdr constructor) ,name)
                          ,(constructor-name constructor)))))
          (defun ,(constructor-name constructor) ,lambda-list
            (list ',(car constructor) ,@lambda-list)))))
     (t
       `((defstruct
             (,(car constructor) :named (:type list) (:conc-name nil)
              (:copier nil) (:predicate nil)
              (:constructor ,(constructor-name constructor)))
           ,@(cdr constructor)))))))

(defun constructor-return-type (name) (list name '*))

(defun constructor-name (constructor) (intern (symbol-name (car constructor))))

;;; <add-adt-constructor>

(defun <add-adt-constructor> (constructor lambda-list name)
  `(add-adt-constructor ',(alexandria:ensure-car constructor)
                        :type-of ',(if lambda-list
                                       (constructor-return-type name)
                                       name)
                        :arg-types ',(arg-types constructor lambda-list)))

(defun arg-types (constructor args)
  (cond ((symbolp constructor) nil)
        ((or args (list-constructor-p constructor))
         (sublis
           (mapcar (lambda (elt) (cons elt (tyclex.unifier:envar elt))) args)
           (cdr constructor)))
        (t
         (mapcar
           (lambda (slot)
             (if (symbolp slot)
                 t
                 (getf slot :type t)))
           (cdr constructor)))))

;;; <pattern-matcher>

(defun <pattern-matcher> (constructor)
  (when (listp constructor)
    `((trivia:defpattern ,(constructor-name constructor)
          (&rest args)
        `(list (eq ',',(car constructor)) ,@args)))))

;;; <derivings>

(defun <derivings> (type-class-name type)
  (labels ((rec (type-class-names &optional acc)
             (if (endp type-class-names)
                 acc ; order is not issue.
                 (body (car type-class-names) (cdr type-class-names) acc)))
           (body (type-class-name rest acc)
             (rec (append (type-class-constraints type-class-name) rest)
                  (loop :for interface
                             :in (type-class-interfaces type-class-name)
                        :collect (or (interface-default interface)
                                     (error "Default instance is not found. ~S"
                                            interface))
                          :into result
                        :finally (return
                                  (cons
                                    `(definstance (,type-class-name ,type)
                                       ,result)
                                    acc))))))
    (rec (list type-class-name))))

(declaim (ftype (function ((satisfies adt-value-p)) fixnum) data-order))

(defun data-order (thing)
  (position (alexandria:ensure-car thing)
            (adt-constructors
              (get-adt
                (adt-constructor-type-of (nth-value 1 (adt-value-p thing)))))
            :test #'eq))

;;;; PRETTY-PRINTER

(set-pprint-dispatch '(cons (member defdata)) (pprint-dispatch '(deftype)))
