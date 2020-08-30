(in-package :cl-user)

(defpackage :tyclex.curry
  (:use :cl)
  (:export ;; Main API
           #:curry
           ;; Helpers
           #:function-type-of
           #:function-type
           #:canonicalize-return-type
           ;; Decurry
           #:decurry
           #:recurry
           #:curry-form-p
           #:expanded-curry-form-p
           #:expanded-curry-form-arity
           #:expanded-curry-form-return-type
           #:first-promised-curry))

(in-package :tyclex.curry)

;;;; CURRY data structure.

(defclass curry ()
  ((arity :initarg :arity :reader arity)
   (return-type :initarg :return-type :reader return-type))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((c curry) &key function)
  (c2mop:set-funcallable-instance-function c function))

;;;; CURRY

(defmacro curry (&whole whole op &rest args)
  ;; Trivial syntax check.
  (check-type op symbol)
  (assert args)
  ;; body
  (<section-form> op args whole))

;;; <Section-Form>

(defun <section-form> (op args whole)
  (let* ((gensyms (underscore-gensyms args))
         (optional-lambda-list (optional-lambda-list gensyms)))
    (if gensyms
        (<curry-form> (<section-body-form> op args gensyms)
                      optional-lambda-list
                      (or (third (function-type-of op))
                          (third (introspect-environment:function-type op)))
                      whole)
        `(,op ,@args))))

(defun underscore-gensyms (args)
  (alexandria:make-gensym-list (count-if #'underscorep args)))

(defun underscorep (thing) (and (symbolp thing) (string= "_" thing)))

(defun optional-lambda-list (lambda-list)
  (mapcar (lambda (x) `(,x nil ,(gensym (format nil "~A-P" x)))) lambda-list))

;;; <Section-Body-Form>

(defun <section-body-form> (op args gensyms)
  (labels ((rec (args gensyms &optional acc)
             (if (endp args)
                 (nreverse acc)
                 (body (car args) (cdr args) gensyms acc)))
           (body (arg rest gensyms acc)
             (if (underscorep arg)
                 (rec rest (cdr gensyms) (push (car gensyms) acc))
                 (rec rest gensyms (push arg acc)))))
    `((,op ,@(rec args gensyms)))))

;;; <Curry-Form>

(defun <curry-form> (body optional-lambda-list return-type whole)
  (let ((curry (gensym "CURRY")))
    (labels ((entry-point (list)
               (if (endp list)
                   (<body-form> body)
                   `(labels ((,curry (&optional ,@list)
                               (if ,(caddar list)
                                   ,(rec (cdr list))
                                   (make-instance 'curry
                                                  :function #',curry
                                                  :arity ,(length list)
                                                  :return-type ',return-type))))
                      ',whole ; for decurry.
                      (make-instance 'curry
                                     :function #',curry
                                     :arity ,(length list)
                                     :return-type ',return-type))))
             (rec (list)
               (if (endp list)
                   (<body-form> body)
                   `(if ,(caddar list)
                        ,(rec (cdr list))
                        ,(entry-point list))))
             (<body-form> (body)
               (if (cdr body)
                   `(locally ,@body)
                   (car body))))
      (entry-point optional-lambda-list))))

;;;; DECURRY

(declaim (ftype (function (list list) (values list &optional)) decurry))

(defun decurry (form actual-args)
  (labels ((rec (if count)
             (if (zerop count)
                 (if (eq 'if (car if))
                     (fourth if)
                     if)
                 (rec (third if) (1- count)))))
    (expander:walk-sublis
      (loop :for var :in (cdr (second (first (second form))))
            :for arg :in actual-args
            :collect `(,(car var) ,arg))
      (rec (third (first (second form))) (length actual-args)))))

(declaim (ftype (function (list list) (values list &optional)) recurry))

(defun recurry (curry-form actual-args)
  (if (null actual-args)
      curry-form
      (let* ((underscore-num (count-if #'underscorep (cddr curry-form)))
             (arg-num (length actual-args))
             (diff (- underscore-num arg-num)))
        (flet ((underscore-to-actual-arg (whole gensyms)
                 (loop :for elt :in whole
                       :when (underscorep elt)
                         :collect (or (pop gensyms) elt)
                       :else
                         :collect elt)))
          (if (minusp diff)
              (error "Too much args. ~S ~S" curry-form actual-args)
              (underscore-to-actual-arg curry-form actual-args))))))

(declaim (ftype (function (*) (values boolean &optional)) curry-form-p))

(defun curry-form-p (form) (typep form '(cons (eql curry) *)))

(declaim
 (ftype (function (*) (values boolean &optional)) expanded-curry-form-p))

(defun expanded-curry-form-p (form)
  (and (listp form)
       (eq 'labels (car form))
       (let ((body (cddr form)))
         (and (every #'listp body)
              (= 2 (length body))
              (eq 'make-instance (caadr body))
              (equal (cadadr body) ''curry)))))

(declaim
 (ftype (function (list) (values (or fixnum null) &optional))
        expanded-curry-form-arity))

(defun expanded-curry-form-arity (form)
  (let* ((make-instance (fourth form)) (arity (getf make-instance :arity)))
    (introspect-environment:constant-form-value arity)))

(declaim
 (ftype (function (list) (values (or symbol list) &optional))
        expanded-curry-form-return-type))

(defun expanded-curry-form-return-type (form)
  (let* ((make-instance (fourth form))
         (return-type (getf make-instance :return-type)))
    (canonicalize-return-type
      (introspect-environment:constant-form-value return-type))))

(declaim (ftype (function (list) (values list &optional)) first-promised-curry))

(defun first-promised-curry (expanded)
  (destructuring-bind
      (op ((name (key first . rest) body)) origin main)
      expanded
    `(,op ((,name (,(car first) ,key ,@rest) ,(third body))) ,origin
      ,(getf main :function))))

;;;; CANONICALIZE-RETURN-TYPE

(declaim
 (ftype (function ((or list atom)) (values (or list atom) &optional))
        canonicalize-return-type))

(defun canonicalize-return-type (return-type)
  (flet ((ensure-t (thing)
           (sublis
             '((* . t) (simple-vector . vector) (simple-array . array)
               (simple-string . string) (base-string . string))
             thing)))
    (if (typep return-type '(cons (eql values) t))
        (if (typep return-type '(cons * (cons (eql &optional) t)))
            (ensure-t (caddr return-type))
            (ensure-t (cadr return-type)))
        (ensure-t return-type))))

;;;; FUNCTION-TYPE

(defmacro function-type (name args return)
  ;; trivial syntax checks.
  (check-type name (and symbol (not (or keyword boolean))))
  (check-type args (or (eql *) list))
  (check-type return (or symbol list))
  ;; body
  `(progn (setf (get ',name 'ftype) '(function ,args ,return)) ',name))

(declaim
 (ftype (function (symbol)
         (values (or null (cons (eql function) (cons * (cons * null))))
                 &optional))
        function-type-of))

(defun function-type-of (name) (get name 'ftype))