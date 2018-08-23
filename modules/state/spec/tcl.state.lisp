(in-package :cl-user)
(tcl:defpackage :tcl.state.spec
  (:shadow #:pop #:push #:get)
  (:use :tcl :jingoh :tcl.state))
(in-package :tcl.state.spec)
(setup :tcl.state)

(requirements-about state)

#?(deftype stack()'(list integer))
=> STACK

#?(declaim(ftype(function()(state stack integer))pop))
=> implementation-dependent

#?(defun pop()
    (state(lambda(cons)
	    (destructuring-bind(x . xs)cons
	      (cons x xs)))))
=> POP
,:before (fmakunbound 'pop)

#?(declaim(ftype(function(integer)(state stack null))push))
=> implementation-dependent

#?(defun push(a)
    (state(lambda(xs)
	    (cons nil (cons a xs)))))
=> PUSH
:before (fmakunbound 'push)

#?(declaim(ftype(function()(state stack integer))stack-manip))
=> implementation-dependent

#?(defun stack-manip()
    (tcl.monad:do (push 3)
		  a <- (pop)
		  (pop)))
=> STACK-MANIP
,:before (fmakunbound 'stack-manip)

#?(funcall (stack-manip) '(5 8 2 1))
=> (5 8 2 1)
,:test equal

#?(funcall (tcl.monad:do (push 3)
			 (pop)
			 (pop))
	   '(5 8 2 1))
=> (5 8 2 1)
,:test equal

#?(funcall (tcl.monad:do a <- (pop)
			 (if(= 5 a)
			   (push 5)
			   (tcl.monad:do (push 3)
					 (push 8))))
	   '(9 0 2 1 0))
=> (NIL 8 3 0 2 1 0)
,:test equal

#?(declaim(ftype(function()state)get))
=> implementation-dependent

#?(defun get()
    (state (lambda(s)
	     (cons s s))))
=> GET
,:before (fmakunbound 'get)

#?(defun put(new-state)
    (state(lambda(s)
	    (declare(ignore s))
	    (cons nil new-state))))
=> PUT
,:before (fmakunbound 'put)

#?(tcl.monad:do stack-now <- (get)
		(if (equal '(1 2 3) stack-now)
		  (put '(8 3 1))
		  (put '(9 2 1))))
:satisfies #`(& (functionp $result)
		(equal '(nil 9 2 1)(funcall $result '(1 2)))
		(equal '(nil 8 3 1)(funcall $result '(1 2 3))))
