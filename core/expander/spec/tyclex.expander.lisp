(defpackage :tyclex.expander.spec
  (:use :cl :jingoh :tyclex.expander))
(in-package :tyclex.expander.spec)
(setup :tyclex.expander)

(requirements-about funcall-expander :test equal)

; Imediately calling of curried function.
#?(expander:call 'funcall '(funcall(tyclex.curry:curry + 10 _)5) :tyclex)
=> (+ 10 5)

; Nested.
#?(expander:call 'funcall '(funcall (funcall (tyclex.curry:curry + _ _) 1) 2) :tyclex)
=> (+ 1 2)

; Less arguments.
#?(expander:call 'funcall '(funcall (tyclex.curry:curry + _ _) 5) :tyclex)
:equivalents (macroexpand-1 '(tyclex.curry:curry + 5 _))
,:test jingoh.tester:sexp=

; Imediately calling of symbol, is not optimized.
#?(expander:call 'funcall '(funcall 'reverse var):tyclex)
=> (funcall 'reverse var)

; Imefiately calling of #'symbol
#?(expander:call 'funcall '(funcall #'reverse var) :tyclex)
=> (reverse var)

#?(expander:call 'funcall '(funcall (lambda(x)(print x)) :dummy) :tyclex)
=> (let((x :dummy))
     (print x))

#?(expander:call 'funcall '(funcall (lambda(x)(print x)) x) :tyclex)
=> (let((x x))
     (print x))

#?(expander:call 'funcall '(funcall (lambda(x)(print x) x) x) :tyclex)
=> (let((x x))
     (print x)
     x)

#?(expander:call 'funcall '(funcall (lambda(&optional x)(print x))) :tyclex)
=> (funcall #'(lambda(&optional x)(print x)))

#?(expander:call 'funcall '(funcall (let((a 1))
				      (lambda(x)(+ a x)))
				    2)
		 :tyclex)
=> (let((a 1))
     (let((x 2))
       (+ a x)))
,:test jingoh.tester:sexp=

#?(expander:call 'funcall '(funcall (constantly 1) 2 3 4) :tyclex)
=> 1
