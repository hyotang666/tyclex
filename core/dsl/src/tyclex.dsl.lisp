(macrolet((def(&rest options)
	    `(uiop:define-package :tyclex.dsl
               ,@(loop :for key :in options
		       :collect `(,key #:tyclex.dsl.defio
				       #:tyclex.dsl.defdata
				       #:tyclex.dsl.definstance
				       #:tyclex.dsl.define-type-class)))))
  (def :use :reexport))
