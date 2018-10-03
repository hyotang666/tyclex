(tcl:defpackage :tcl-user
  (:use :tcl #:tcl.applicative #:tcl.compare #:tcl.eq #:tcl.io #:tcl.monad-plus #:tcl.ord
	#:tcl.bounded #:tcl.enum #:tcl.functor #:tcl.monad #:tcl.monoid #:tcl.zip-list)
  (:shadowing-import-from :tcl.monad #:return #:do)
  (:export))
(in-package :tcl-user)

