(defpackage :tcl.ord
  (:use :cl :tyclex)
  (:shadowing-import-from :tyclex #:list)
  (:export
    #:ord #:ord-p
    )
  )
(in-package :tcl.ord)

(define-type-class(ord a)()
  ((ord(a)number))
  (:default ord(x)
    `(data-order ,x)))

(definstance(ord number)
  ((ord(a)
     a)))

(definstance(ord character)
  ((ord(a)
     `(char-code ,a))))

