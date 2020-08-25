# TYCLEX
People say "To learn language X, implement lisp with it".
But I am Common Lisper, so I say "To learn language X, implement it with Common Lisp".

This repository is the fruit of my haskell study.

Please do not be excited.
Actually, I did not implement haskell.
`TYCLEX` is standing in TYpe CLass EXtension for common lisp.
Yes, it is just extension, not haskell itself.

Haskell is amazing language, but I did not feel like to be haskeller.
Already my study is over.
It means this repository is frozen.

Additionaly, TYCLEX may not practical one.
If you need such library, searching another one is strongly recommended.

## About system (also package)
### TYCLEX
This is the core.
Providing the system for defining type class.

#### Hierarchical systems.

```
:components
(;; Bottom
 (:system "tyclex.curry")
 (:system "tyclex.unifier")
 (:system "tyclex.objects.newtype")
 (:system "tyclex.conditions")

 ;; Level.1
 (:system "tyclex.objects.instance" :depends-on ("tyclex.conditions"))
 (:system "tyclex.objects.interface" :depends-on ("tyclex.conditions"))
 (:system "tyclex.objects.io-action" :depends-on ("tyclex.conditions"))
 (:system "tyclex.objects.adt-constructor" :depends-on ("tyclex.conditions"))

 ;; Level.2
 (:system "tyclex.expander" :depends-on ("tyclex.objects.io-action"))
 (:system "tyclex.objects.adt" :depends-on ("tyclex.objects.adt-constructor" "tyclex.objects.io-action"))
 (:system "tyclex.objects.type-class" :depends-on ("tyclex.objects.interface"))

 ;; Level.3
 (:system "tyclex.objects" :depends-on ("tyclex.objects.adt" "tyclex.objects.type-class"))
 (:system "tyclex.type-matcher" :depends-on ("tyclex.objects.adt"))
 (:system "tyclex.dsl.definstance" :depends-on ("tyclex.objects.type-class"))

 ;; Level.4
 (:system "tyclex.dsl.defdata" :depends-on ("tyclex.dsl.definstance"))
 (:system "tyclex.compute-return-type" :depends-on ("tyclex.type-matcher"))

 ;; Level.5
 (:system "tyclex.dsl.define-type-class :depends-on ("tyclex.compure-return-type"))
 (:system "tyclex.newtype" :depends-on ("tyclex.compute-return-type"))

 ;; Level.6
 (:system "tyclex.dsl.defio" :depends-on ("tyclex.newtype"))

 ;; Level.7
 (:system "tyclex.dsl" :depends-on ("tyclex.dsl.defio"))

 ;; Top
 (:system "tyclex" :depends-on ("tyclex.dsl")))
```

### TCL
TYCLEX exports symbol `LIST`.
For conveniency, TCL is provided.
It responds to resolve symbol confliction.
Use TCL instead of CL.

### TCL-USER
TYCLEX is just core system.
It does not provide any type class.
Some basic type classes are provided as modules.
If you like "Buttery included", TCL-USER is what you need.
Please be care about implicit symbol shadowing, e.g. `DO` is shadowed.

#### Hierarchical systems.
```
:components
(;; Bottom
 (:module "tcl.bounded")
 (:module "tcl.enum")
 (:module "tcl.io")
 (:module "tcl.ord")
 (:module "tcl.eq")
 (:module "tcl.functor")
 (:module "tcl.monad")
 (:module "tcl.monoid")

 ;; Level.1
 (:module "tcl.compare" :depends-on ("tcl.ord"))
 (:module "tcl.applicative" :depends-on ("tcl.functor"))
 (:module "tcl.monad-plus" :depends-on ("tcl.monad"))
 (:module "tcl.state" :depends-on ("tcl.monad"))
 (:module "tcl.diff-list" :depends-on ("tcl.monoid"))
 (:module "tcl.writer" :depends-on ("tcl.monad" "tcl.monoid"))

 ;; Level.2
 (:module "tcl.zip-list" :depends-on ("tcl.applicative"))
)
```
