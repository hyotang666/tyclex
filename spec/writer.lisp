(in-package :vs-haskell.spec)
(named-readtables:in-readtable jingoh.reader:syntax)

(requirements-about WRITER)

#?(ehcl::function-type log-number(integer)((writer (list string))integer))
=> implementation-dependent
,:before (fmakunbound 'log-number)
,:lazy t

#?(defun log-number(x)
    (writer(cons x (list (uiop:strcat "Got number: "(princ-to-string x))))))
=> LOG-NUMBER

#?(domonad a <- (log-number 3)
	   b <- (log-number 5)
	   (.return (* a b)))
=> (15 "Got number: 3" "Got number: 5")
,:test equal
,:around(let(ehcl::*expand-verbose*)
	  (call-body))
,:lazy t

#?(defmacro to-diff-list(list)
    `(diff-list(ehcl::curry append ,list _)))
=> TO-DIFF-LIST
,:before (fmakunbound 'to-diff-list)

#?(defun from-diff-list(diff-list)
    (funcall diff-list nil))
=> FROM-DIFF-LIST
,:before (fmakunbound 'from-diff-list)

;;;; 14.2
#?(let((f(ehcl::curry * 5 _))
       (g(ehcl::curry + 3 _)))
    (declare(type function f g))
    (funcall (fmap f g) 8))
=> 55

#?(let((f(<$> (ehcl::curry + _ _)
	      (ehcl::curry * 2 _)
	      (ehcl::curry + 10 _))))
    (funcall f 3))
=> 19

#?(funcall (domonad a <- (ehcl::curry * _ 2)
		    b <- (ehcl::curry + _ 10)
		    (.return (+ a b)))
	   3)
=> 19
,:around(let(ehcl::*expand-verbose*)
	  (call-body))
,:lazy t

;;;; STATE-MONAD

#?(deftype stack()
    '(list integer))
=> STACK

#?(defun .pop()
    (state(trivia:lambda-ematch((cons x xs)(cons x xs)))))
=> .POP
,:before (fmakunbound '.pop)
#?(ehcl::function-type .pop()(state stack integer))
=> implementation-dependent

#?(defun .push(a)
    (state(lambda(xs)
	    (cons nil (cons a xs)))))
=> .PUSH
,:before (fmakunbound '.push)
#?(ehcl::function-type .push(integer)(state stack null))
=> implementation-dependent

#?(defun stack-manip()
    (domonad (.push 3)
	     (.pop)
	     (.pop)))
=> STACK-MANIP
,:before (fmakunbound 'stack-manip)

#?(funcall(stack-manip)'(5 8 2 1))
=> (5 8 2 1)
,:test equal

#?(defun stack-stuff()
    (domonad a <- (.pop)
	     (if(eql a 5)
	       (.push 5)
	       (domonad (.push 3)
			(.push 8)))))
=> STACK-STUFF
,:before (fmakunbound 'stack-stuff)

#?(funcall(stack-stuff)'(9 0 2 1 0))
=> (NIL 8 3 0 2 1 0)
,:test equal
