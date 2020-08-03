(in-package :cl-user)

(defpackage :tyclex.expander
  (:import-from :tyclex.curry
                #:expanded-curry-form-p
                #:decurry
                #:curry-form-p
                #:recurry
                #:first-promised-curry)
  (:import-from :tyclex.objects.io-action
                #:action-body
                #:action-lambda-list
                #:get-io
                #:io-form-p
                #:io-action-construct-form-p
                #:io-action-construct-form-function-form)
  (:use :cl)
  (:export))

(in-package :tyclex.expander)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cons-type-specifier (list)
    (typecase list
      (null '*)
      (atom
       (if (eq '* list)
           list
           `(eql ,list)))
      (cons
       `(cons ,(cons-type-specifier (car list))
              ,(cons-type-specifier (cdr list)))))))

(defun |funcall-expander| (form env)
  (destructuring-bind
      (op function . args)
      form
    (let ((the
           (when (and (listp function) (eq 'the (car function)))
             (setf function (third function)) ; as canonicalize.
             (second function))))
      (when (curry-form-p function)
        (return-from |funcall-expander|
          (expander:expand (recurry function args) env)))
      (setf function (expander:expand function env))
      (when (and (listp function) (eq 'the (car function)))
        (setf the (second function)
              function (third function)))
      (cond
       ((expanded-curry-form-p function)
        (expander:expand (decurry function args) env))
       ((io-form-p function)
        (let ((io (get-io (car function))))
          (if (null (cdr function))
              (pretty-body (expander:expand* (action-body io) env) the)
              (expander:expand
                `(destructuring-bind
                     ,(action-lambda-list io)
                     (list ,@(cdr function))
                   ,@(action-body io))
                env))))
       ((io-action-construct-form-p function)
        (|funcall-expander|
          `(,(car form) ,(io-action-construct-form-function-form function)
            ,@args)
          env))
       ((typep function '(cons (eql function) (cons symbol null)))
        `(,(cadr function) ,@(expander:expand* args env)))
       ((typep function '#.(cons-type-specifier '#'(lambda ())))
        (destructuring-bind
            (function (lambda lambda-list . body))
            function
          (declare (ignore function lambda))
          (if (expander::intersectionp lambda-list lambda-list-keywords
                                       :test #'eq)
              `(,op #'(lambda ,lambda-list ,@body)
                ,@(expander:expand* args env))
              (let ((binds (mapcar #'list lambda-list args)))
                (if binds
                    (expander:expand
                      `(let ,binds
                         ,@body)
                      env)
                    (if (cdr body)
                        `(locally ,@body)
                        (car body)))))))
       ((typep function '#.(cons-type-specifier '((lambda ()))))
        (destructuring-bind
            ((lambda lambda-list . body) . actual-args)
            function
          (declare (ignore lambda))
          (if (expander::intersectionp lambda-list lambda-list-keywords
                                       :test #'eq)
              `((lambda ,(expander:expand-params lambda-list env)
                  ,@(expander:expand* body env))
                ,@(expander:expand* actual-args env))
              (let ((binds (mapcar #'list lambda-list actual-args)))
                (if binds
                    (multiple-value-bind (binds decls prebody main)
                        (expander:parse-bubble-let
                          `(let ,binds
                             ,@body))
                      (expander:expand
                        `(let ,binds
                           ,@decls
                           ,@prebody
                           (,op ,main ,@args))
                        env))
                    (if (cdr body)
                        (expander:expand `(,op (locally ,@body) ,@args) env)
                        (expander:expand `(,op ,(car body) ,@args))))))))
       ((typep function '#.(cons-type-specifier '(constantly *)))
        (let ((arg-forms
               (remove-if (lambda (x) (constantp x env))
                          (expander:expand* args env))))
          (if arg-forms
              `(progn ,@arg-forms ,(cadr function))
              (cadr function))))
       ((typep function '#.(cons-type-specifier '(let ())))
        (multiple-value-bind (binds decls prebody main)
            (expander:parse-bubble-let function)
          (expander:expand
            `(let ,binds
               ,@decls
               ,@prebody
               (,op ,main ,@args))
            env)))
       (t `(,op ,function ,@(expander:expand* args env)))))))

(defun pretty-body (form the)
  (let ((return-type
         (when the
           (typecase the
             ((cons (eql io) *) (second the))
             ((cons (eql function) *) (third the))))))
    (if (cdr form)
        (if return-type
            `(the ,return-type (progn ,@form))
            `(locally ,@form))
        (if return-type
            `(the ,return-type ,(car form))
            (car form)))))

(defun |mapcar-expander| (form env)
  (destructuring-bind
      (op fun . args)
      form
    (setf fun (expander:expand fun env))
    (setf args (expander:expand* args env))
    (cond
     ((expanded-curry-form-p fun)
      `(,op ,(first-promised-curry fun) ,@(expander:expand* args env)))
     ((loop :for form :in args
            :thereis (and (constantp form env)
                          (null
                            (introspect-environment:constant-form-value form
                                                                        env))))
      (let ((args (remove-if (lambda (x) (constantp x env)) args)))
        (if args
            (if (expander:pure-fun-form-p fun env)
                `(progn ,@args nil)
                `(progn ,fun ,@args nil))
            (if (expander:pure-fun-form-p fun env)
                nil
                `(progn ,fun nil)))))
     (t `(,op ,fun ,@args)))))

(handler-bind ((expander:expander-conflict #'expander:use-next))
  (expander:defexpandtable :tyclex (:use optimize)
                           (:add |funcall-expander| funcall)
                           (:add |mapcar-expander| mapcar mapcan maplist
                            mapcon)))
