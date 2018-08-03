# CONCEPTS
`TYCLEX.OBJECTS.TYPE-CLASS` provides data object for type-class and operators for it.
Object instance itself is associated with symbol as name.
## Objects TYPE-CLASS
`TYPE-CLASS` represents type class.
### Slot VARS
`VARS` contains type variables for type-class.
It is used to make `INSTANCE-SIGNATURE`.
### Slot INTERFACES
`INTERFACES` contains interface names.
It is used to implement :DERIVING, as trait, and to compute constraints definitions to achive type context.
### Slot MEMBER
`MEMBER` contains type patterns which implements concrete instance.
It is used to implement type class predicate.
### Slot CONSTRAINTS
`CONSTRAINTS` contains type class names.
Is is used to achive type context.

