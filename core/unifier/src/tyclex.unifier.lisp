(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :tyclex.unifier)
    (rename-package :unify :tyclex.unifier)
    (let ((method
           (find-method #'print-object nil
                        (list
                          (find-class
                            (find-symbol "ENVIRONMENT" "TYCLEX.UNIFIER"))
                          t)
                        nil)))
      (when method
        (remove-method #'print-object method)))
    (let ((asdf::*asdf-session* nil))
      (declare (ignorable asdf::*asdf-session*)) ; for CCL.
      (asdf:load-system :cl-unification :force t))))

(in-package :tyclex.unifier)

(handler-bind ((package-error #'continue)) ; for ECL
  (export
    '(envar patternize enwild dewild ignore-unification-failure
            find-value-variable replace-bind substitute-pattern
            make-variable-list subst-wildcard-to-var subst-var-to-wildcard
            gensymed-var-p)))

;;;; ENVAR

(defun envar (thing)
  (trestrul:asubst-if (lambda (x) (intern (format nil "?~A" x)))
                      (lambda (x)
                        (and (typep x '(and symbol (not (or keyword boolean))))
                             (not (char= #\? (char (string x) 0)))))
                      thing))

;;;; PATTERNIZE

(defun patternize (thing)
  (if (millet:type-specifier-p thing)
      thing
      (if (listp thing)
          (trestrul:mapleaf #'patternize thing)
          (envar thing))))

;;;; ENWILD

(defun enwild (type-spec) (sublis '((* . _) (t . _)) type-spec))

;;;; DEWILD

(defun dewild (pattern) (subst t '_ pattern))

;;;; IGNORE-UNIFICATION-FAILURE

(defmacro ignore-unification-failure (form)
  `(handler-case ,form
     (unification-failure ()
       nil)))

;;;; FIND-VALUE-VARIABLE

(defun find-value-variable (value env &key (test #'eql))
  (declare (type environment env))
  (labels ((find-value-var (frames)
             (unless (endp frames)
               (or (let ((binding
                          (find value (frame-bindings (car frames))
                                :test test
                                :key #'binding-value)))
                     (when binding
                       (binding-variable binding)))
                   (find-value-var (cdr frames))))))
    (find-value-var (environment-frames env))))

;;;; REPLACE-BIND

(defun replace-bind (variable value env)
  (setf (binding-value (find-variable-binding variable env)) value)
  env)

;;;; SUBSTITUTE-PATTERN

(defun substitute-pattern (pattern environment)
  (let ((type-spec
         (dewild
           (trestrul:asubst-if
             (lambda (var)
               (let ((return-type (find-variable-value var environment)))
                 (typecase return-type
                   ((cons (eql values) t) (cadr return-type))
                   (null var)
                   (t return-type))))
             #'variablep pattern))))
    (typecase type-spec
      ((cons (eql function) (cons * null)) `(function * ,(cadr type-spec)))
      (otherwise type-spec))))

;;;; MAKE-VARIABLE-LIST

(defun make-variable-list (fixnum)
  (loop :repeat fixnum
        :collect (intern (format nil "?~A" (gensym)))))

;;;; SUBST-WILDCARD-TO-VAR

(defun subst-wildcard-to-var (thing)
  (trestrul:asubst-if (lambda (x) (declare (ignore x)) (envar (gensym)))
                      #'variable-any-p thing))

;;;; SUBST-VAR-TO-WILDCARD

(defun subst-var-to-wildcard (thing) (subst-if '_ #'gensymed-var-p thing))

;;;; GENSYMED-VAR-P

(defun gensymed-var-p (x)
  (and (symbolp x)
       (let ((name (symbol-name x)))
         (and (string= "?G" name :end2 2)
              (loop :for i :upfrom 2 :below (length name)
                    :always (digit-char-p (char name i)))))))
