(in-package :cl-user)
(macrolet((def(&rest options)
	    `(uiop:define-package :tyclex.objects
               ,@(loop :for key :in options
		       :collect `(,key #:tyclex.objects.instance
				       #:tyclex.objects.interface
				       #:tyclex.objects.type-class
				       #:tyclex.objects.io-action
				       #:tyclex.objects.adt
				       #:tyelex.objects.adt-constructor
				       ,@(when (eq :use key)
					   (list :cl))
				       )))))
  (def :use :reexport))
