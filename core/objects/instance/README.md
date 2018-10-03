# CONCEPTS
`TYCLEX.OBJECTS.INSTANCE` responds to provide data object called `INSTANCE` and operators for it.
Object itself is contained as elements of `INTERFACE-INSTANCES`.
## Object INSTANCE.
`INSTANCE` represents concrete instance for type class.
### Slot SIGNATURE
`SIGNATURE` contains type patterns.
It is used as key to find instance with `INTERFACE` arugments return types.
### Slot DEFINITIONS
`DEFINITIONS` contains `CL:MACROLET` first argument.
It is used to achive type context.
### Slot TYPES
`TYPES` contains type patterns.
It is used as foreign key to find instance to achive type context with `INSTANCE` `DEFINITIONS` `CONSTRAINTS`.
### Slot CONSTRAINTS
`CONSTRAINTS` contains type class names.
It is used to achive type context.
Each constraint is used as foreigh key to match `TYPES`.
