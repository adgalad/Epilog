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

### Op-Assignment `(o0 :: Operand) :=  (o :: Operation)`

`case o of`

 -  `Id (o1 :: Operand)` -> `case o1 of`

    -   `R Name`          -> '\\i j -> add i j $zero'

    -   `T Int`           -> '\\i j -> add i j $zero'  

    -   `FP`              -> '\\i   -> add i $fp $zero'

    -   `C (v::Constant)` -> `case v of`

        - `FC _` -> '\\i  -> l.s i v'

        - `_`    -> '\\i  -> li i v'

 -  `U op a` -> '\\r -> case op of` 
    
    -   NegI     -> 'neg   r a'

    -   NegF     -> 'neg.s r a'

    -   BNot     -> 'xori  r a -1'

 -  `B op a b` -> '\\r -> case op of'
    
    -  `AddI`    ->  `case b of` 

            - `C (v::Constant)` -> 'addi r a v'

            - `_              ` -> 'add r a b'

    -  `SubI`    ->  'sub r a b'

    -  `MulI`    ->  'mult a b; mflo'

    -  `DivI`    ->  'div a b; mflo r'

    -  `RemI`    ->  'div a b; mfhi r'

    -  `AddF`    ->  'add.s r a b'

    -  `SubF`    ->  'sub.s r a b'

    -  `MulF`    ->  'mul.s r a b'

    -  `DivF`    ->  'div.s r a b'
    
    -  `BSL`     ->  'sll r a b'

    -  `BSR`     ->  'sra r a b'

    -  `BAnd`    ->  'and r a b'

    -  `BOr`     ->  'or  r a b'

    -  `BXor`    ->  'xor r a b'



### Array read `Operand :=# (Int32, Operand)`

 - `t := a[i] (type :: Type)` ->  `case type of`
    
    - `bool, char` -> 'lb   t, i(a)'

    - `int, ptr`   -> 'lw   t, i(a)'

    - `float`      -> 'l.s, t, i(a)' 

### Array write `(Int32, Operand) :#= Operand`

 - `t[i] := a (type :: Type)` ->  `case type of`
    
    - `bool, char` -> 'sb  a, i(t)'

    - `int, ptr`   -> 'sw  a, i(t)'

    - `float`      -> 's.s a, i(t)'

### Pointer read `Operand :=* Operand`

 - `t := *a (type :: Type)` ->  `case type of`
    
    - `bool, char` -> 'lb t, 0(a)'

    - `int, ptr`   -> 'lw t, 0(a)' 

    - `float`      -> 'l.s t, 0(a)'


### Pointer write `Operand :*= Operand`

 - `*t := a (type :: Type)` ->  `case type of`
    
    - `bool, char` -> 'sb a, 0(t) '

    - `int, ptr`   -> 'sw a, 0(t)'

    - `float`      -> 's.s a, 0(t)'

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
