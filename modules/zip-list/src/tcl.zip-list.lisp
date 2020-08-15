(tcl:defpackage :tcl.zip-list
  (:use #:tcl #:tcl.applicative #:tcl.functor)
  (:export ;; type-name
           #:zip-list))

(in-package :tcl.zip-list)

(define-newtype zip-list (&optional a) (declare (ignore a)) 'list)

(definstance (functor zip-list)
  ((fmap (f zl) `(zip-list (fmap ,f ,(denew zl))))))

(definstance (applicative zip-list)
  ((pure (x) `(series:series ,x))
   (<*> (fs xs)
     (alexandria:with-gensyms (fn)
       `(zip-list
         (let ((,fn ,fs))
           (series:collect
            (series:map-fn t #'funcall (series:scan ,fn)
                           (series:scan ,xs)))))))))