## Registers

|Num|Name|Use                           |
|---|----|------------------------------|
| 0 |zero| zero                         |
| 1 | at | ---                          |
| 2 | v0 | scratch 0                    |
| 3 | v1 | scratch 1                    |
| 4 | a0 | scratch 2                    |
| 5 | a1 | general use                  |
| 6 | a2 | general use                  |
| 7 | a3 | general use                  |
| 8 | t0 | general use                  |
| 9 | t1 | general use                  |
| 10| t2 | general use                  |
| 11| t3 | general use                  |
| 12| t4 | general use                  |
| 13| t5 | general use                  |
| 14| t6 | general use                  |
| 15| t7 | general use                  |
| 16| s0 | general use                  |
| 17| s1 | general use                  |
| 18| s2 | general use                  |
| 19| s3 | general use                  |
| 20| s4 | general use                  |
| 21| s5 | general use                  |
| 22| s6 | general use                  |
| 23| s7 | general use                  |
| 24| t8 | general use                  |
| 25| t9 | general use                  |
| 26| k0 | kernel 0                     |
| 27| k1 | kernel 1                     |
| 28| gp | global pointer               |
| 29| sp | stack pointer                |
| 30| fp | frame pointer                |
| 31| ra | return address               |
|---|--- |------------------------------|
|   | pc |                              |
|   | lo |                              |
|   | hi |                              |

\newpage
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
+----------------------------+   } Argumentos
|            arg1            |   |
+----------------------------+   |
|            arg0 (12($fp) ) |   |
+----------------------------+  _/
|           ret val          |
+----------------------------+
|          ret addr          |
+----------------------------+
|        frame pointer       | <---- $fp
+----------------------------+  -\
|             v0  (-4($fp) ) |   |
+----------------------------+   |
|             v1             |   |
+----------------------------+   } Variables locales
|            . . .           |   |
+----------------------------+   |
|             vn             | <-|-- $sp
+----------------------------+  _/
|                            |
+----------------------------+
|                            |
+----------------------------+ Direcciones bajas / Tope de la pila
               .
               .
               .
```

\newpage
## Instructions

### `Comment (s :: String)`

`# s`

### Op-Assignment `(o0 :: Operand) :=  (o :: Operation)`

`case o of`

 -  `Id (o1 :: Operand)` -> `case o1 of`

    -   `R Name`          -> `\\i j -> move i, j`

    -   `T Int`           -> `\\i j -> move i, j`

    -   `FP`              -> `\\i   -> move i, $fp`

    -   `C (v::Constant)` -> `case v of`

        - `FC _` -> `\\i  -> l.s i v`

        - `_`    -> `\\i  -> li i v`

 -  `U op a` -> `\\r -> case op of`

    -   NegI     -> `neg   r, a`

    -   NegF     -> `neg.s r, a`

    -   BNot     -> `xori  r, a -1`

 -  `B op a, b` -> `\\r -> case op of`

    -  `AddI`    ->  `case b of`

            - `C (v::Constant)` -> `addi r, a v`

            - `_              ` -> `add r, a, b`

    -  `SubI`    ->  `sub r, a, b`

    -  `MulI`    ->
      ```
        mult a, b
        mfhi r
        beq r $zero %l
        exception
      %l:
        mflo r
      ```

    -  `DivI`    ->  `div a, b; mflo r`

    -  `RemI`    ->  `div a, b; mfhi r`

    -  `AddF`    ->  `add.s r, a, b`

    -  `SubF`    ->  `sub.s r, a, b`

    -  `MulF`    ->  `mul.s r, a, b`

    -  `DivF`    ->  `div.s r, a, b`

    -  `BSL`     ->  `sll r, a, b`

    -  `BSR`     ->  `srl r, a, b`

    -  `BAnd`    ->  `and r, a, b`

    -  `BOr`     ->  `or  r, a, b`

    -  `BXor`    ->  `xor r, a, b`



### Array read `Operand :=# (Int32, Operand)`

 - `t := a[i] (type :: Type)` ->  `case type of`

    - `float`      -> `l.s, t, i(a)`

    - `_`   -> `lw   t, i(a)`


### Array write `(Int32, Operand) :#= Operand`

 -  `t[i] := a (type :: Type)` ->  `case type of`

    - `float` -> `s.s a, i(t)`

    - `_`     -> `sw  a, i(t)`


### Pointer read `Operand :=* Operand`

 - `t := *a (type :: Type)` ->  `case type of`

    - `float` -> `l.s t, 0(a)`

    - `_`   -> `lw t, 0(a)`



### Pointer write `Operand :*= Operand`

 - `*t := a (type :: Type)` ->  `case type of`

    - `float`      -> `s.s a, 0(t)`

    - `_`   -> `sw a, 0(t)`


### Push parameter `Param (o:: Operand)`

```
addi $sp, $sp, -4
sw   $o, 0($sp)
```

### Call `Call (f :: Function)`

```
addi $sp, $sp, -12
sw   $fp, 0($sp)
jal  f
```

### Call assign `(o :: Operand) :<- (f :: Function)`

```
addi $sp, $sp, -12
sw   $fp, 0($sp)
jal  f

lw   $o, 8($fp)
```

### Cleanup after function call `Cleanup (n :: Word32)`

```
lw   $fp, 0($fp)
addi $sp, $sp, (n + 12)
```

### Reserve space in the stack `Prolog (n :: Word32)`

```
move $fp, $sp
addi $sp, $sp, (-n)
```

### Free space in the stack `Epilog (n :: Word32)`

```
sw   $ra, 4($fp)
move $sp, $fp
```

### `Answer (o :: Operand)`

```
sw $o, 8($fp)
```

## Terminators

### Mandatory branc `Br Label`

`j label`

### Conditional branch 1 `IfBr (o :: Operand) (l1 :: Label) (l2 :: Label)`

 -  `R (n :: Name)`  ->  `\\r -> bne $r $zero, l1; j l2`

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

 -  FAI -> `\\r1 r2 -> div $r2, $r1; mfhi $v0; bne $v0, $zero, l`

 -  NFI -> `\\r1 r2 -> div $r2, $r1; mfhi $v0; beq $v0, $zero, l`

### `Return`

```
j   ($sp)
```

### `Exit`

```
li  $v0, 10
syscall
```

## Read/Write
