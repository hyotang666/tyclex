(tcl:defpackage :tcl.eq
  (:use :tcl :tcl.data)
  (:export
    #:eq-p
    #:== #:/==
    )
  )
(in-package :tcl.eq)

(define-type-class(eq a)()
  ((==(a a) boolean)
   (/==(a a) boolean))
  (:default == (x y)
    `(not (/== ,x ,y)))
  (:default /== (x y)
    `(not (== ,x ,y))))

(definstance(eq symbol)
  ((==(a b)
     `(eq ,a ,b))))

(definstance(eq number)
  ((==(a b)
     `(= ,a ,b))))

(definstance(eq character)
  ((==(a b)
     `(char= ,a ,b))))

(definstance(eq string)
  ((==(a b)
     `(equal ,a ,b))))

(definstance(eq list)
  ((==(a b)
     `(equal ,a ,b))))

(definstance(eq vector)
  ((==(a b)
     `(equalp ,a ,b))))

(definstance(eq bit-vector)
  ((==(a b)
     `(equal ,a ,b))))

(definstance(eq pathname)
  ((==(a b)
     `(equal ,a ,b))))

(definstance(eq hash-table)
  ((==(a b)
     `(equalp ,a ,b))))

(definstance(eq array)
  ((==(a b)
     `(equalp ,a ,b))))

(definstance(eq (maybe *))
  ((==(a b)
     (let((x(gensym"X"))
	  (y(gensym"Y")))
       `(trivia:match*(,a ,b)
	  ((nothing nothing)T)
	  (((just ,x)(just ,y))(eval `(== ,,x ,,y))))))))
