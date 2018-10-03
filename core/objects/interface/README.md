# CONCEPTS
`TYCLEX.OBJECTS.INTERFACE` provides data object for each type class interface, and operators for it.
Object instance itself is associated with symbol whicn macro name.
## Object INTERFACE.
`INTERFACE` represents type class instance interface.
### Slot LAMBDA-LIST
`LAMBDA-LIST` contains type patterns.
It is used to compute instance call return type, and making instance signature.
### Slot RETURN-TYPE
`RETURN-TYPE` contains type patterns.
It is used to compute instance call return type.
### Slot TYPE-CLASS
`TYPE-CLASS` contains type-class name.
It is used to type context, especially to get constraints of type-class which the `INTERFACE` belongs to.
### Slot INSTANCES
`INSTANCES` contains `INSTANCE` objects.
It is used as repository of concrete implementation of the `INTERFACE`.
### Slot DEFAULT
`DEFAULT` contains one element of `CL:MACROLET` first argument.
Is is used default isntance, when `INSTANCE-DEFINITIONS` lacks definition of the `INTERFACE`.
