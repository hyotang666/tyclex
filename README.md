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
