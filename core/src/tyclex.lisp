(macrolet ((def (&rest options)
             `(uiop:define-package :tyclex
                (:shadowing-import-from #:tyclex.objects.newtype #:list)
                ,@(loop :for key :in options
                        :collect `(,key #:tyclex.curry #:tyclex.newtype
                                   #:tyclex.objects #:tyclex.dsl
                                   #:tyclex.type-matcher
                                   #:tyclex.compute-return-type
                                   ,@(when (eq :use key)
                                       (list :cl)))))))
  (def :use :reexport))
