(tcl:defpackage :tcl.monoid
  (:use :tcl #:tcl.data)
  (:export
    ;; type-class
    #:monoid
    ;; interfaces
    #:mempty #:mappend #:mconcat
    ;; predicate
    #:monoid-p
    ;; Helpers
    #:mappend*
    ;; Newtypes
    #:product #:sum #:any #:all #:1st
    ))
(in-package :tcl.monoid)

(define-type-class(monoid m)()
  ((mempty()m)
   (mappend(m m)m)
   (mconcat((list m))m))
  (:default mconcat(ms)
    `(reduce (lambda(a m)(mappend a m))
	     ,ms
	     :from-end t
	     :initial-value (mempty))))

;;;; Helpers
(defmacro mappend* (&body form*)
  (labels((rec(form*)
	    (if(endp(cdr form*))
	      (car form*)
	      `(mappend ,(car form*)
			,(rec (cdr form*))))))
    (rec form*)))

;;;; Newtypes
(define-newtype product(a) a)

(define-newtype sum(a) a)

(define-newtype any() 'boolean)

(define-newtype all() 'boolean)

(define-newtype 1st(&optional a)
  (declare(ignore a))
  '(maybe *))

;;;; Instances
(definstance(monoid list)
  ((mempty()nil)
   (mappend(a b)
     `(append ,a ,b))))

(definstance(monoid vector)
  ((mempty()`(vector))
   (mappend(a b)
     `(concatenate 'vector ,a ,b))))

(definstance(monoid string)
  ((mempty()"")
   (mappend(a b)
     `(concatenate 'string ,a ,b))))

(definstance(monoid bit-vector)
  ((mempty()`(make-array 0 :element-type 'bit))
   (mappend(a b)
     `(concatenate 'bit-vector ,a ,b))))

(definstance(monoid (product number))
  ((mempty()`(product 1))
   (mappend(a b)
     `(product (* ,a ,b)))))

(definstance(monoid (sum number))
  ((mempty()`(sum 0))
   (mappend(a b)
     `(sum (+ ,a ,b)))))

(definstance(monoid any)
  ((mempty()`(any nil))
   (mappend(a b)
     `(or ,a ,b))))

(definstance(monoid all)
  ((mempty()`(all t))
   (mappend(a b)
     `(and ,a ,b))))

(definstance(monoid ordering)
  ((mempty():eq)
   (mappend(a b &environment env)
     (when(constantp a env)
       (setf a (introspect-environment:constant-form-value a env)))
     (when(constantp b env)
       (setf b (introspect-environment:constant-form-value b env)))
     (trivia:match*(a b)
       ((:lt _):lt)
       ((:eq y)y)
       ((:gt _):gt)
       ((_ _) `(trivia:ematch*(,a ,b)
		 ((:lt _):lt)
		 ((:eq y)y)
		 ((:gt _):gt)))))))

(definstance(monoid (maybe a) :constraints (monoid a))
  ((mempty()nothing)
   (mappend(a b)
     (trivia:match*(a b)
       ((nothing m)m)
       ((m nothing)m)
       (((just m1)(just m2))`(just (mappend ,m1 ,m2)))
       ((_ _) (alexandria:with-gensyms(m m1 m2)
		`(trivia:ematch*(,a ,b)
		   ((nothing ,m),m)
		   ((,m nothing),m)
		   (((just ,m1)(just ,m2))
		    (just(eval `(mappend ,,m1 ,,m2)))))))))))

(definstance(monoid 1st)
  ((mempty()'(1st nothing))
   (mappend(a b)
     (trivia:match*((denew a)(denew b))
       (((just x)_)`(1st(just ,x)))
       ((nothing x)x)
       ((_ _) (alexandria:with-gensyms(x)
		`(trivia:ematch*(,a ,b)
		   (((just ,x)_)(1st(just ,x)))
		   ((nothing ,x),x))))))))

