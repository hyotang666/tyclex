(defpackage :tyclex.type-class-core
  (:use :cl)
  (:export ; instance table cell.
    ;; as type name
    #:cell
    ;; as constructor
    #:make-cell
    ;; as readers
    #:signature #:instances #:types #:constraints
    )
  (:export ; type class instance.
    ;; as type name
    #:instance
    ;; as constructor
    #:make-type-class-instance
    ;; predicate
    #:instancep
    ;; readers
    #:instance-lambda-list #:instance-return-type #:instance-type-class #:instance-default
    ;; accessor
    #:instance-table
    ;; helpers
    #:find-instance #:add-instance #:remove-instance #:augment-table
    )
  (:export ; type class
    ;; as type name
    #:type-class
    ;; as constructor
    #:make-type-class
    ;; predicate
    #:type-class-p
    ;; readers
    #:type-class-name #:type-class-vars #:type-class-instances
    ;; accessors
    #:type-class-member #:type-class-constraints
    ;; helpers
    #:find-type-class #:add-type-class #:remove-type-class
    )
  )
(in-package :tyclex.type-class-core)

