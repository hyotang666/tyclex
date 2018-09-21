(tcl:defpackage :tcl.writer.spec
  (:shadowing-import-from :tcl.monad #:do #:return)
  (:use #:tcl :jingoh #:tcl.writer #:tcl.monad #:tcl.monoid))
(in-package :tcl.writer.spec)
(setup :tcl.writer)

(requirements-about writer)

#?(declaim(ftype (function (fixnum)(writer (list string)fixnum))
		 log-number))
=> implementation-dependent

#?(defun log-number(x)
    (writer (cons x (list (format nil "Got number: ~S"x)))))
=> LOG-NUMBER
,:before (fmakunbound 'log-number)

#?(defun mult-with-log()
    (do a <- (log-number 3)
      b <- (log-number 5)
      (return (* a b))))
=> MULT-WITH-LOG
,:before (fmakunbound 'mult-with-log)

#?(mult-with-log)
=> (15 "Got number: 3" "Got number: 5")
,:test equal

#?(deftype food () 'string)
=> FOOD
#?(deftype price()'(sum integer))
=> PRICE

#?(declaim(ftype(function(food)(cons food price))add-drink))
=> implementation-dependent

#?(defun add-drink(x)
    (trivia:match x
      ("beans" (cons "milk" (sum 25)))
      ("jerky" (cons "whisky" (sum 99)))
      (_ (cons "beer" (sum 30)))))
=> ADD-DRINK
,:before (fmakunbound 'add-drink)

#?(>>= (cons "beans" (sum 10))
       #'add-drink)
=> ("milk" . 35)
,:test equal

#?(>>= (cons "jerky" (sum 25))
       #'add-drink)
=> ("whisky" . 124)
,:test equal

#?(>>= (cons "dogmeat" (sum 5))
       #'add-drink)
=> ("beer" . 35)
,:test equal

#?(>>=* (cons "dogmeat" (sum 5))
	#'add-drink
	#'add-drink)
=> ("beer" . 65)
,:test equal
