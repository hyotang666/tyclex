(in-package :vs-haskell.spec)
(named-readtables:in-readtable jingoh.reader:syntax)

(requirements-about SUBTYPE?)

; NOTE! in `SUBTYPE?`, `T` means wildcard.

#?(millet:type-expand 'birds) => INTEGER
;; id1
#?(ehcl::subtype? 'birds 'birds) => T
#?(ehcl::subtype? 'birds 'zip-list) => NIL
#?(ehcl::subtype? 'zip-list 'birds) => NIL
;; id2, id3
#?(ehcl::subtype? 'bit '(mod 3)) => T
#?(ehcl::subtype? '(mod 3)'bit) => NIL
#?(ehcl::subtype? 'zip-list 'pair) => NIL
#?(ehcl::subtype? 'pair 'zip-list) => NIL
#?(ehcl::subtype? 'list 'zip-list) => NIL
#?(ehcl::subtype? 'zip-list 'list) => T
;; id4
#|id5|# #?(ehcl::subtype? '(mod 3) 'integer) => T
#|id6|# #?(ehcl::subtype? 'integer '(mod 3)) => NIL
#|id7|# #?(ehcl::subtype? 'bit 'integer) => T
#|id8|# #?(ehcl::subtype? 'integer 'bit) => NIL
#|id9|# #?(ehcl::subtype? 'fixnum '(maybe fixnum)) => NIL
        #?(ehcl::subtype? 'fixnum 'maybe) => NIL
        #?(ehcl::subtype? '(maybe fixnum) 'maybe) => T
#?(ehcl::subtype? t 'fixnum) => T
#?(ehcl::subtype? 'fixnum t) => T
#|id10|# #?(ehcl::subtype? T '(function T T)) => T
#|id11|# #?(ehcl::subtype? 'function '(function T T)) :satisfies identity
#?(ehcl::subtype? 'pair '(pair ?A)) => T
#?(ehcl::subtype? '(pair ?A) 'pair) => T
#?(ehcl::subtype? '(pair ?A) '(list ?A)) => T
#?(ehcl::subtype? '(list ?A) '(pair ?A)) => NIL
,:around(let(vs-haskell::*subtype-verbose*)(call-body))
#?(ehcl::subtype? '(pair sequence) '(function ?A)) => NIL
,:around(let(vs-haskell::*subtype-verbose*)(call-body))
#++
(defun subtype?(t1 t2)
  (if(millet:type-specifier-p t1)
    (if(millet:type-specifier-p t2)
      (if(subtypep t1 t2)
	(let((expanded?1 (nth-value 1(millet:type-expand t1))))
	  (multiple-value-bind(expanded expanded?2)(millet:type-expand t2)
	    (if expanded?1
	      (if expanded?2
		(matrix-case:matrix-etypecase(t1 t2)
		  ((symbol symbol)(eq t1 t2)) ; id 1
		  ((symbol list)(eq t1 (car t2))) ; id2
		  ((list symbol)(eq (car t1)t2)) ; id3
		  ((list list)(loop :for e1 :in t1
				    :for e2 :in t2
				    :always (subtype? e1 e2)))) ; id4
		T) ; id5
	      (if expanded?2
		(handler-case(string= t2 expanded) ; id6
		  (error()t))
		T)))) ; id7
	nil) ; id8
      (if(adt-p t2)
	(eq (alexandria:ensure-car t1)(alexandria:ensure-car t2)) ; id9
	(or (eql t t1) ; id10
	    (with-subtype-verbose(type-unify:unify t1 (patternize t2)))))) ; id11
    (if(millet:type-specifier-p t2)
      (if(adt-p t1)
	(eq (alexandria:ensure-car t1)(alexandria:ensure-car t2)) ; id12
	(or (eql t t2) ; id13
	    (with-subtype-verbose(type-unify:unify (patternize t1)t2)))) ; id14
      (if(adt-p t1)
	(if(adt-p t2)
	  (progn #++(dev:log t "(SUBTYPE? ~S ~S)" t1 t2)
		 (eq (alexandria:ensure-car t1)(alexandria:ensure-car t2)) ; id15
		 )
	  (progn #++(dev:log t "2(subtype? ~S ~S)"t1 t2)
		 (with-subtype-verbose(type-unify:unify t1 t2)))) ; id16
	(if(adt-p t2)
	  (progn #++(dev:log t "3(subtype? ~S ~S)"t1 t2)
		 (with-subtype-verbose(type-unify:unify t1 t2))) ; id17
	  (with-subtype-verbose(type-unify:unify (patternize t1)(patternize t2)))))))) ; id18
