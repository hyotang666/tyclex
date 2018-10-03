(in-package :cl-user)
(macrolet((def(&rest options)
	    `(uiop:define-package :tyclex.objects
               (:shadowing-import-from :tyclex.objects.newtype #:list)
               ,@(loop :for key :in options
		       :collect `(,key #:tyclex.objects.instance
				       #:tyclex.objects.interface
				       #:tyclex.objects.type-class
				       #:tyclex.objects.io-action
				       #:tyclex.objects.adt
				       #:tyclex.objects.adt-constructor
				       #:tyclex.objects.newtype
				       ,@(when (eq :use key)
					   (list :cl))
				       )))))
  (def :use :reexport))
