(tcl:defpackage :tcl.monad-plus
  (:use :tcl #:tcl.monad)
  (:shadowing-import-from :tcl.monad #:return #:do)
  (:export ;; type-class name
           #:monad+
           ;; interfaces
           #:mzero
           #:mplus
           ;; predicate
           #:monad+-p
           ;; helpers
           #:guard
           #:lc))

(in-package :tcl.monad-plus)

(define-type-class (monad+ m) ((monad m))
  ((mzero () (m a))
   (mplus ((m a) (m a)) (m a))))

;;;; Helpers

(defmacro guard (bool)
  `(if ,bool
       (return nil)
       (mzero)))

(defmacro lc (&rest expression*)
  `(do
    ,@(labels ((rec (list &optional acc)
                 (if (endp list)
                     (nreverse acc)
                     (if (and (symbolp (second list))
                              (string= '#:<- (second list)))
                         (rec (cdddr list)
                              (list* (third list) (second list) (first list)
                                     acc))
                         (rec (cdr list) (cons `(guard ,(car list)) acc))))))
        (rec (cddr expression*)))
    (return ,(car expression*))))

;;;; Instances

(definstance (monad+ list)
  ((mzero () nil)
   (mplus (a b) `(append ,a ,b))))