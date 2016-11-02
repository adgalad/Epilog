## Registers

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

## Stack Representation

```
+----------------------------+ Direcciones altas / Fondo de la pila
|            . . .           |
+----------------------------+
|    espacio del llamador    |
+----------------------------+
|            . . .           |  
+----------------------------+  -\
|            argn            |   |
+----------------------------+   |
|            . . .           |   |
+----------------------------+   > Argumentos   
|            arg2            |   |
+----------------------------+   |
|            arg1            |   |
+----------------------------+  _/
|           ret val          |
+----------------------------+
|          ret addr          |
+----------------------------+
|        frame pointer       | <---- $fp
+----------------------------+  -\
|             v1  (-4($fp) ) |   |
+----------------------------+   |
|             v2             |   |
+----------------------------+   > Variables locales   
|            . . .           |   |
+----------------------------+   |
|             vn             |   |
+----------------------------+  _/
|                            | <---- $sp
+----------------------------+
|                            |
+----------------------------+
|                            |
+----------------------------+ Direcciones bajas / Tope de la pila
               .            
               .            
               .            
```

## Instructions

### `Comment (s :: String)`

`# s`

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


## Terminators

### Mandatory branc `Br Label`

`j label`

### Conditional branch 1 `IfBr (o :: Operand) (l1 :: Label) (l2 :: Label)`

 -  `R (n :: Name)`  ->  `\\r -> bne $r $zero, l1; j l2'

### Conditional branch 2 `CondBr (r :: Rel) (o1 :: Operand) (o2 :: Operand) (l :: Label)`

`case r of`

 -  LTF -> `\\f1 f2 -> c.lt.s $f1, $f2 ; bc1t l`

 -  LEF -> `\\f1 f2 -> c.le.s $f1, $f2 ; bc1t l`

 -  GTF -> `\\f1 f2 -> c.lt.s $f2, $f1 ; bc1t l`

 -  GEF -> `\\f1 f2 -> c.le.s $f2, $f1 ; bc1t l`

 -  EQF -> `\\f1 f2 -> c.eq.s $f1, $f2 ; bc1t l`

 -  NEF -> `\\f1 f2 -> c.eq.s $f1, $f2 ; bc1f l`



 -  LTI -> `\\r1 r2 -> slt $v0, $r1, $r2; bne $v0, $zero, l`

 -  LEI -> `\\r1 r2 -> slt $v0, $r2, $r1; beq $v0, $zero, l`

 -  GTI -> `\\r1 r2 -> slt $v0, $r2, $r1; bne $v0, $zero, l`

 -  GEI -> `\\r1 r2 -> slt $v0, $r1, $r2; beq $v0, $zero, l`

 -  EQI -> `\\r1 r2 -> beq $r1, $r2, l`

 -  NEI -> `\\r1 r2 -> bne $r1, $r2, l`

 -  FAI -> `\\r1 r2 -> `div $r1, $r2; mfhi $v0; bne $v0, $zero, l`

 -  NFI -> `\\r1 r2 -> `div $r1, $r2; mfhi $v0; beq $v0, $zero, l`


### `Return` (Cambia la generación de IR)

```
lw $ra, -4($fp) # revisar índices
jr $ra
```

### `Exit`

```
li  $v0, 10           
syscall
```

## Read/Write // Moisés
