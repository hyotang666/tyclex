(tcl:defpackage :tcl.writer
  (:use #:tcl #:tcl.monad #:tcl.monoid)
  (:shadowing-import-from :tcl.monad #:do #:return)
  (:export
    ;; newtype
    #:writer
    ))
(in-package :tcl.writer)

(define-newtype writer(&optional w a)
  `(cons ,a ,w))

(definstance(monad (writer w) :constraints (monoid w))
  ((return(x)
     `(writer (cons ,x (mempty))))
   (>>=(writer f)
     (alexandria:with-gensyms(x v y v%)
       `(destructuring-bind(,x . ,v),writer
	  (destructuring-bind(,y . ,v%)(funcall ,f ,x)
	    (writer(cons ,y (mappend ,v ,v%)))))))))

