(tcl:defpackage :tcl.writer.spec
  (:shadowing-import-from :tcl.monad #:do #:return)
  (:use #:tcl :jingoh #:tcl.writer #:tcl.monad))
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
