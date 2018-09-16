(in-package :cl-user)
(defpackage :tyclex.newtype
  (:use :cl)
  (:import-from :tyclex.objects.newtype #:add-newtype)
  (:import-from :tyclex.compute-return-type #:compute-return-type)
  (:export ; main api
    #:define-newtype
    #:denew
    )
  )
(in-package :tyclex.newtype)

(defmacro define-newtype(name lambda-list &body body)
  ;; trivial-syntax-check.
  (assert(typep name '(and symbol (not (or keyword boolean)))))
  (assert(listp lambda-list))
  ;; body
  (let((arg(gensym "ARG")))
    `(PROGN (ADD-NEWTYPE ',name)
	    (DEFTYPE ,name ,lambda-list ,@body)
	    (DEFMACRO,name(,arg)
	      ,(if(null lambda-list)
		 ``(THE ,',name ,,arg)
		 `(LET((WILD-CARDS(enough-wildcard ',lambda-list)))
		    (IF(EQ 'CONS (ALEXANDRIA:ENSURE-CAR(MILLET:TYPE-EXPAND `(,',name ,@wild-cards))))
		      `(THE (,',name ,@wild-cards),,arg)
		      `(THE (,',name ,(Compute-return-type ,arg)) ,,arg))))))))

(defun enough-wildcard(lambda-list)
  (loop :repeat (length (remove-if (lambda(x)(find x lambda-list-keywords :test #'eq))
				   lambda-list))
	:collect '*))

(defun denew(thing)
  (if(typep thing '(cons (eql the)*))
    (third thing)
    thing))

