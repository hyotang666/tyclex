(in-package :cl-user)

(macrolet ((def (&rest options)
             `(uiop:define-package :tcl-user
                (:shadowing-import-from :tyclex.objects.newtype #:list)
                (:shadowing-import-from :tcl.monad #:return #:do)
                ,@(loop :for key :in options
                        :collect `(,key #:tcl #:tcl.data #:tcl.applicative
                                   #:tcl.compare #:tcl.eq #:tcl.io
                                   #:tcl.monad-plus #:tcl.ord #:tcl.bounded
                                   #:tcl.enum #:tcl.functor #:tcl.monad
                                   #:tcl.monoid #:tcl.zip-list #:tcl.writer
                                   #:tcl.diff-list #:tcl.state)))))
  (def :use :reexport))