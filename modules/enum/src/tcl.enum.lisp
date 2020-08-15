(tcl:defpackage :tcl.enum
  (:use :tcl)
  (:export ;; type-class
           #:enum
           ;; interfaces
           #:succ
           #:pred
           ;; predicate
           #:enum-p))

(in-package :tcl.enum)

(define-type-class (enum a) ()
  ((succ (a) a)
   (pred (a) a))
  (:default succ (x)
    (let ((v (gensym "V")))
      `(let ((,v ,x))
         (nth (1+ (data-order ,v))
              (adt-constructors
                (find-adt
                 (adt-constructor-type-of (find-adt-constructor ,v))))))))
  (:default pred (x)
    (let ((v (gensym "V")))
      `(let ((,v ,x))
         (nth (1- (data-order ,v))
              (adt-constructors
                (find-adt
                 (adt-constructor-type-of (find-adt-constructor ,v)))))))))

(definstance (enum number)
  ((succ (a) `(1+ ,a))
   (pred (a) `(1- ,a))))

(definstance (enum character)
  ((succ (a) `(code-char (1+ (char-code ,a))))
   (pred (a) `(code-char (1- (char-code ,a))))))