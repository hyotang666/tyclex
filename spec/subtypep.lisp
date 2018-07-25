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
#?(ehcl::subtype? 'zip-list 'list) => NIL
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
#?(ehcl::subtype? '(pair ?A) '(list ?A)) => NIL
#?(ehcl::subtype? '(list ?A) '(pair ?A)) => NIL
,:around(let(vs-haskell::*subtype-verbose*)(call-body))
#?(ehcl::subtype? '(pair sequence) '(function ?A)) => NIL
,:around(let(vs-haskell::*subtype-verbose*)(call-body))
#?(ehcl::subtype? '(list T)'(cons T)) => T
#?(ehcl::subtype? '(cons t)'(list t)) => T
#?(ehcl::subtype? '(list t) '(list ?A)) => T
#?(ehcl::subtype? '(list ?A) '(list fixnum)) => T
