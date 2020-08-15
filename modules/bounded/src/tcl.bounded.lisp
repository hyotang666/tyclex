(tcl:defpackage :tcl.bounded
  (:use :tcl)
  (:export))

(in-package :tcl.bounded)

(define-type-class (bounded a) ()
  ((min-bound (a) t)
   (max-bound (a) t))
  (:default min-bound (x) `(car (adt-constructors (find-adt ,x))))
  (:default max-bound (x) `(car (last (adt-constructors (find-adt ,x))))))
