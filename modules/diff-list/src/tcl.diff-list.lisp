(in-package :cl-user)
(common-lisp:defpackage :tcl.diff-list
  (:use :tcl #:tcl.monoid)
  (:export
    #:diff-list
    #:to-diff-list #:from-diff-list
    ))
(in-package :tcl.diff-list)

(define-newtype diff-list(a)
  `(function((list ,a))(list ,a)))

(declaim(ftype(function((list *))(diff-list *))to-diff-list))
(defun to-diff-list(list)
  (curry append list _))

(defun from-diff-list(diff-list)
  (funcall diff-list nil))

(definstance(monoid (diff-list *))
  ((mempty()
     (alexandria:with-gensyms(xs)
       `(diff-list (lambda(,xs)(append nil ,xs)))))
   (mappend(f g)
     (alexandria:with-gensyms(xs)
       `(diff-list (lambda(,xs)(function ,f (funcall ,g ,xs))))))))
