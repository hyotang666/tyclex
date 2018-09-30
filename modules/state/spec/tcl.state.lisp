(in-package :cl-user)
(tcl:defpackage :tcl.state.spec
  (:shadow #:pop #:push #:get #:random #:random-state)
  (:use :tcl :jingoh :tcl.state))
(in-package :tcl.state.spec)
(setup :tcl.state)

(requirements-about state)

#?(deftype stack()'(list integer))
=> STACK

#?(defun pop()
    (state(lambda(cons)
	    (destructuring-bind(x . xs)cons
	      (cons x xs)))))
=> POP
,:before (fmakunbound 'pop)

#?(defun push(a)
    (state(lambda(xs)
	    (cons nil (cons a xs)))))
=> PUSH
,:before (fmakunbound 'push)

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

#?(defun random(random-state)
    (let((*random-state*(make-random-state random-state)))
      (cons (cl:random most-positive-fixnum) *random-state*)))
=> RANDOM
,:before (fmakunbound 'random)

#?(defun random-state()
    (state #'random))
=> RANDOM-STATE
,:before (fmakunbound 'random-state)

#?(setf (symbol-function 'three-coins)
	(tcl.monad:do a <- (random-state)
		      b <- (random-state)
		      c <- (random-state)
		      (tcl.monad:return (list a b c))))
:be-the function

#?(three-coins *random-state*)
:satisfies #`(& (consp $result)
		(listp (car $result))
		(= 3 (length (car $result)))
		(every #'integerp (car $result))
		(random-state-p (cdr $result)))

#?(funcall (tcl.monad:lift-m (tyclex:curry + 100 _)(pop))
	   '(1 2 3 4))
=> (101 2 3 4)
,:test equal
