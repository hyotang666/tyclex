(macrolet((def(&rest options)
	    `(uiop:define-package :tyclex
               ,@(loop :for key :in options
		       :collect `(,key #:tyclex.curry
				       #:tyclex.newtype
				       #:tyclex.objects
				       #:tyclex.dsl
				       #:tyclex.type-matcher
				       )))))
  (def :use :reexport))
