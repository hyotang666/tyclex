(in-package :cl-user)

(defpackage :tyclex.newtype
  (:use :cl)
  (:import-from :tyclex.objects.newtype #:add-newtype)
  (:import-from :tyclex.compute-return-type #:compute-return-type)
  (:import-from :tyclex.unifier
                #:unify
                #:find-variable-value
                #:enwild
                #:dewild
                #:make-variable-list)
  (:export ; main api
           #:define-newtype
           #:denew))

(in-package :tyclex.newtype)

(defmacro define-newtype (name lambda-list &body body)
  ;; trivial-syntax-check.
  (assert (typep name '(and symbol (not (or keyword boolean)))))
  (assert (listp lambda-list))
  ;; body
  (let ((arg (gensym "ARG")) (env (gensym "ENV")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (add-newtype ',name)
       (deftype ,name ,lambda-list ,@body)
       ,(<rewinder> name lambda-list)
       (defmacro ,name (,arg &environment ,env)
         ,@(if (null lambda-list)
               `((declare (ignore ,env)) `(the ,',name ,,arg))
               `(`(the
                   ,(,(intern (format nil "REWIND-~A" name))
                     (compute-return-type (expander:expand ,arg ,env) ,env))
                   ,,arg)))))))

(defun denew (thing)
  (if (typep thing '(cons (eql the) *))
      (third thing)
      thing))

(defun <rewinder> (name lambda-list)
  (alexandria:with-gensyms (type-specifier)
    `(defun ,(intern (format nil "REWIND-~A" name)) (,type-specifier)
       ,@(if (null lambda-list)
             `((declare (ignore ,type-specifier)) ',name)
             (let ((required (lambda-fiddle:required-lambda-vars lambda-list)))
               (if (null required)
                   `((declare (ignore ,type-specifier)) ',name)
                   (let* ((variables (make-variable-list (length required)))
                          (expanded (millet:type-expand `(,name ,@variables))))
                     `(`(,',name
                         ,@(if (equal ',expanded ,type-specifier)
                               `(,,type-specifier)
                               (loop :for variable :in ',variables
                                     :collect (or (find-variable-value variable
                                                                       (unify
                                                                         ',(enwild
                                                                             expanded)
                                                                         (enwild
                                                                           ,type-specifier)))
                                                  '*)
                                       :into args
                                     :finally (return (dewild args)))))))))))))

;;;; PRETTY-PRINTER

(set-pprint-dispatch '(cons (member define-newtype))
                     (pprint-dispatch '(deftype)))
