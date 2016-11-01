# Registers

|Num|Name|Use                           |
|---|----|------------------------------|
| 0 |zero| zero                         |
| 1 | at | ---                          |
| 2 | v0 | scratch 0                    |
| 3 | v1 | scratch 1                    |
| 4 | a0 | scratch 2                    |
|   | a1 | general use                  |
|   | a2 | general use                  |
|   | a3 | general use                  |
|   | t0 | general use                  |
|   | t1 | general use                  |
|   | t2 | general use                  |
|   | t3 | general use                  |
|   | t4 | general use                  |
|   | t5 | general use                  |
|   | t6 | general use                  |
|   | t7 | general use                  |
|   | s0 | general use                  |
|   | s1 | general use                  |
|   | s2 | general use                  |
|   | s3 | general use                  |
|   | s4 | general use                  |
|   | s5 | general use                  |
|   | s6 | general use                  |
|   | s7 | general use                  |
|   | t8 | general use                  |
|   | t9 | general use                  |
|   | k0 | kernel 0                     |
|   | k1 | kernel 1                     |
|   | gp | global pointer               |
|   | sp | stack pointer                |
|   | fp | frame pointer                |
|   | ra | scratch 3                    |
|---|--- |------------------------------|
|   | pc |                              |
|   | lo |                              |
|   | hi |                              |

## Instructions

### `Comment String`

No code.

### Op-Assignment `(o0 :: Operand) :=  (o :: Operation)` // Carlos

`case o of`

 -  `Id (o1 :: Operand)` -> `case o1 of`

    -   `R Name`          -> '\\i j -> add i j $zero'

    -   `T Int`           -> '\\i j -> add i j $zero'  

    -   `FP`              -> '\\i -> add i $fp $zero'

    -   `C (v::Constant)` -> '\\i   -> li i v'



### Array read `Operand :=# (Int32, Operand)` // Carlos

### Array write `(Int32, Operand) :#= Operand` // Carlos

### Pointer read `Operand :=* Operand` // Carlos

### Pointer write `Operand :*= Operand` // Carlos

### Push parameter `Param Operand`

### Call `Call Function`

### Call assign `Operand :<- Function`

### Cleanup after function call `Cleanup Word32`

### Reserve space in the stack `Prolog Word32`

### Free space in the stack `Epilog Word32`


## Terminators // Moisés

### Mandatory branc `Br Label`

### Conditional branch 1 `IfBr Operand Label Label`

### Conditional branch 2 `CondBr Rel Operand Operand Label`

### `Return (Maybe Operand)`

### `Exit`

## Read/Write // Moisés
