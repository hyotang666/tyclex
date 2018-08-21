(tcl:defpackage :tcl.monad-plus.spec
  (:shadowing-import-from :tcl.monad #:return #:do)
  (:use :tcl :jingoh #:tcl.monad-plus))
(in-package :tcl.monad-plus.spec)
(setup :tcl.monad-plus)

(requirements-about monad+)

;;;; LC
#?(lc (cons n c) || n <- '(1 2) c <- '(#\a #\b))
=> ((1 . #\a)(1 . #\b)(2 . #\a)(2 . #\b))
,:test equal

#?(lc x || x <- (the list(loop :for i :upfrom 1 :to 50 :collect i))
      (find #\7 (princ-to-string x)))
=> (7 17 27 37 47)
,:test equal

;;;; KNIGHT
#?(deftype knight-pos()
    '(cons fixnum fixnum))
=> KNIGHT-POS

#?(defun move-knight(pos)
    (destructuring-bind(c . r)pos
      (do (cons c% r%) <- (list (cons (+ 2 c)(1- r))
				(cons (+ 2 c)(1+ r))
				(cons (- c 2)(- r 1))
				(cons (- c 2)(1+ r))
				(cons (1+ c)(- r 2))
				(cons (1+ c)(+ r 2))
				(cons (1- c)(- r 2))
				(cons (1- c)(+ r 2)))
	(guard(and (find c% '(1 2 3 4 5 6 7 8))
		   (find r% '(1 2 3 4 5 6 7 8))))
	(return (cons c% r%)))))
=> MOVE-KNIGHT
,:before (fmakunbound 'move-knight)

#?(declaim(ftype(function(knight-pos)(list knight-pos))move-knight))
=> implementation-dependent
,:lazy t

#?(move-knight '(6 . 2))
=> ((8 . 1)(8 . 3)(4 . 1)(4 . 3)(7 . 4)(5 . 4))
,:test equal

#?(move-knight '(8 . 1))
=> ((6 . 2)(7 . 3))
,:test equal

#?(defun in3(start)
    (do first <- (move-knight start)
      second <- (move-knight first)
      (move-knight second)))
=> IN3
,:before (fmakunbound 'in3)

#?(defun reach-in3-p(start end)
    (find end (in3 start) :test #'equal))
=> REACH-IN3-P
,:before (fmakunbound 'reach-in3-p)

#?(reach-in3-p '(6 . 2)'(6 . 1))
:satisfies identity

#?(reach-in3-p '(6 . 2)'(7 . 3))
=> NIL
