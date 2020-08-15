(in-package :cl-user)

(defpackage :tyclex.dsl.defio
  (:use :cl)
  (:import-from :tyclex.newtype #:define-newtype)
  (:import-from :tyclex.objects.io-action #:io #:io-action #:add-io)
  (:export #:defio))

(in-package :tyclex.dsl.defio)

(define-newtype io
    (a)
  (declare (ignore a))
  'io-action)

;;;; DEFIO

(defmacro defio ((name signature return) &body body)
  (multiple-value-bind (vars types)
      (parse-signature signature)
    `(locally
      ;; SBCL emits compiler note about return type `IO-ACTION`.
      ;; note: type assertion too complex to check:
      #+sbcl
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (proclaim '(ftype (function ,types (io ,return)) ,name))
      (add-io ',name :type '(io ,return) :body ',body :lambda-list ',vars)
      (defun ,name ,vars
        (make-instance 'io-action
                       :instance (lambda () ,@body)
                       :type '(io ,return))))))

(defun parse-signature (signature)
  (labels ((rec (signature &optional vars types)
             (if (endp signature)
                 (do-return vars types)
                 (body (car signature) (cdr signature) vars types)))
           (do-return (vars types)
             (values (nreverse vars) (nreverse types)))
           (body (sig sig-rest vars types)
             (if (find sig lambda-list-keywords :test #'eq)
                 (rec sig-rest (push sig vars) (push sig types))
                 (rec sig-rest (push (car sig) vars) (push (cadr sig) types)))))
    (rec signature)))

;;;; PRETTY-PRINTER

(defun pprint-defio (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     "~:<~W~^ ~3I~@_~/tyclex.dsl.defio::pprint-args/~^~1I ~:_~@{~W~^ ~_~}~:>")
    stream exp))

(defun pprint-args (stream exp &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch))
        #+sbcl
        (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
    (set-pprint-dispatch '(cons (member function)) nil)
    (write exp :stream stream)))

(set-pprint-dispatch '(cons (member defio)) 'pprint-defio)
