(in-package :cl-user)
(tcl:defpackage :tcl.state
  (:use #:tcl #:tcl.monad)
  (:shadowing-import-from #:tcl.monad #:do #:return)
  (:export
    ;; newtype
    #:state
    ))
(in-package :tcl.state)

(define-newtype state(&optional state a)
  `(function(,state)(cons ,a ,state)))

(definstance(monad (state s))
  ((return(x)
     (alexandria:with-gensyms(s)
       `(state (lambda(,s)
		 (cons ,x ,s)))))
   (>>=(h f)
     (alexandria:with-gensyms(s a new g)
       `(state (lambda(,s)
		 (destructuring-bind(,a . ,new)(funcall ,h ,s)
		   (let((,g(funcall ,f ,a)))
		     (funcall ,g ,new)))))))))
