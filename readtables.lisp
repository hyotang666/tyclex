(in-package #:vs-haskell)

(defun |#`-reader|(stream character number)
  (declare(ignore character number))
  (let((form(read stream t t t)))
    (ecase(car form)
      (and `(alexandria:conjoin ,@(cdr form)))
      (or `(alexandria:disjoin ,@(cdr form)))
      (+ `(alexandria:compose ,@(cdr form)))
      (% (make-form (cdr form))))))

(defun make-form(form)
  (flet((underscorep(x)
          (and (symbolp x)
               (string= '#:_ x))))
    (let((position(position-if #'underscorep form)))
      (if position
        (progn (assert (not (find-if #'underscorep form :start (1+ position))))
               (if (= 1 position)
                 `(alexandria:rcurry ,(car form) ,@(cddr form))
                 `(alexandria:rcurry (alexandria:curry ,(car form)
                                                       ,@(subseq form 1 position))
                                     ,@(nthcdr (1+ position)form))))
        `(alexandria:curry ,@form)))))

(defun |#%-reader|(stream character number)
  (declare(ignore character number))
  (let((form(read stream t t t)))
    (rotatef(car form)(cadr form))
    form))

(defun |#[-reader|(stream character integer)
  (declare(ignore character integer))
  (let((form(read-delimited-list #\] stream t)))
    (cons 'list (rplaca form `(eq ',(car form))))))

(named-readtables:defreadtable :higher-order-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\% #'|#%-reader|)
  (:dispatch-macro-char #\# #\` #'|#`-reader|))

(named-readtables:defreadtable :pattern-match-constructor
  (:merge :standard)
  (:dispatch-macro-char #\# #\[ #'|#[-reader|)
  (:syntax-from :standard #\) #\]))

(named-readtables:defreadtable :cl-vs-haskell
  (:merge :standard :pattern-match-constructor :higher-order-syntax))
