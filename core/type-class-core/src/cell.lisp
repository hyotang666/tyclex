(in-package :tyclex.type-class-core)

;;;; CELL
(defstruct(cell (:predicate nil)
		(:copier nil)
		(:conc-name nil))
  "Represents instance table cell."
  (signature  (error "SIGNATURE is required.")  :type list :read-only t)
  (instances  (error "INSTANCES is required.")  :type list :read-only t)
  (types      (error "TYPES is required.")      :type list :read-only t)
  (constraints(error "CONSTRAINTS is required."):type list :read-only t))

