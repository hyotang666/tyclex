; vim: ft=lisp et
(in-package :asdf)
(defsystem "ehcl.curry"
  :depends-on
  (
   "closer-mop" ; wrapper for meta object protocols.
   "alexandria" ; public domain utilities.
   )
  :components
  ((:file "curried-function")))
