(defpackage :tcl.data
  (:use :cl #:tyclex.dsl.defdata)
  (:export
    #:maybe ; type-name
    #:just #:nothing ; constructors
    )
  )
(in-package :tcl.data)

(defdata maybe (a)
  (just a)
  nothing)
