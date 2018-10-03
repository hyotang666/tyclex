(in-package :cl-user)
(macrolet((def(&rest options)
	    `(uiop:define-package :tyclex.dsl
               ,@(loop :for key :in options
		       :collect `(,key #:tyclex.dsl.defio
				       #:tyclex.dsl.defdata
				       #:tyclex.dsl.definstance
				       #:tyclex.dsl.define-type-class
				       ,@(when(eq :use key)
					   (list :cl))
				       )))))
  (def :use :reexport))
