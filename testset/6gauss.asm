	.data
	.align 2
_base_header: .space 8
	.align 2
_last_used: .space 4
_true: .asciiz "true\n"
_false: .asciiz "false\n"
_str2: .asciiz "\tel numero cero para salir, o\n"
_str3: .asciiz "\tun numero entero N entre uno y diez para resolver un sistema de N ecuaciones.\n"
_str13: .asciiz "\n"
_str12: .asciiz " = "
_str4: .asciiz "> "
_str9: .asciiz "? "
_str5: .asciiz "Adios.\n"
_str14: .asciiz "El sistema de ecuaciones dado no tiene solucion pues su matriz es singular.\n"
_str1: .asciiz "Introduzca\n"
_str6: .asciiz "Introduzca los coeficientes de las ecuaciones, por filas\n"
_str10: .asciiz "La solucion al sistema de ecuaciones dado es:\n"
_str0: .asciiz "Sistemas de Ecuaciones por Eliminacion de Gauss.\n"
_str11: .asciiz "X_"
_str8: .asciiz "_"
_str7: .asciiz "c_"
	.align 2
Coef: .space 440
	.align 2
Solvable: .space 4
	.align 2
X: .space 40
	.text
	.globl main
# _entry
main:
#   call "main"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_main
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   Exit
	li $v0, 10
	syscall
# abs
proc_abs:
#   ; Procedure at (93,1)
# Procedure at (93,1)
#   prolog 0
	# Prolog 8
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 8

#   fvar X_1 12 4
#   CondBr {rel = LTF, op0 = RF "X_1", op1 = C (FC 0.0), trueDest = Label {lblstr = "YesGuard", lblnum = 5}, falseDest = Label {lblstr = "NextGuard", lblnum = 6}}
	li.s $f0, 0.0
	l.s $f1, 12($fp)
	c.lt.s $f1, $f0
	bc1t YesGuard
	j NextGuard
YesGuard:
#   _tf0 := negf f.X_1
	l.s $f1, 12($fp)
	li.s $f0, 0.0
	sub.s $f2, $f0, $f1
#   answer _tf0
	s.s $f2, 8($fp)
	s.s $f2, -4($fp)
#   Br {dest = Label {lblstr = "Return", lblnum = 2}}
	j Return
NextGuard:
#   Br {dest = Label {lblstr = "YesGuard_1", lblnum = 7}}
	j YesGuard_1
YesGuard_1:
#   answer f.X_1
	l.s $f1, 12($fp)
	s.s $f1, 8($fp)
#   Br {dest = Label {lblstr = "Return", lblnum = 2}}
	j Return
IfExit:
#   answer #f0.0
	li.s $f0, 0.0
	s.s $f0, 8($fp)
#   Br {dest = Label {lblstr = "Return", lblnum = 2}}
	j Return
Return:
#   ; Epilog for procedure abs
# Epilog for procedure abs
#   epilog 0
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# backSubstitution
proc_backSubstitution:
#   ; Procedure at (118,1)
# Procedure at (118,1)
#   prolog 16
	# Prolog 160
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 160

#   var N 12 4
#   _t1 := subi N #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   _t2 := muli _t1 #i44
	mul $a1, $a2, 44
#   _t3 := muli N #i4
	lw $a3, 12($fp)
	mul $t0, $a3, 4
#   _t4 := addi _t2 _t3
	add $a3, $a1, $t0
#   _tf5 := Coef[_t4]
	l.s $f1, Coef($a3)
#   _t6 := subi N #i1
	lw $t1, 12($fp)
	sub $t2, $t1, 1
#   _t7 := muli _t6 #i44
	mul $t1, $t2, 44
#   _t8 := subi N #i1
	lw $t3, 12($fp)
	sub $t4, $t3, 1
#   _t9 := muli _t8 #i4
	mul $t3, $t4, 4
#   _t10 := addi _t7 _t9
	add $t5, $t1, $t3
#   _tf11 := Coef[_t10]
	l.s $f2, Coef($t5)
#   _tf12 := divf _tf5 _tf11
	div.s $f3, $f1, $f2
#   _t13 := subi N #i1
	lw $t6, 12($fp)
	sub $t7, $t6, 1
#   _t14 := muli _t13 #i4
	mul $t6, $t7, 4
#   X[_t14] := _tf12
	s.s $f3, X($t6)
#   var L -4 4
#   var I -8 4
#   var J -12 4
#   fvar Sum -16 4
#   L := #i2
	li $s0, 2
	sw $a1, -20($fp)
	sw $a2, -24($fp)
	sw $a3, -28($fp)
	sw $t0, -32($fp)
	sw $t1, -36($fp)
	sw $t2, -40($fp)
	sw $t3, -44($fp)
	sw $t4, -48($fp)
	sw $t5, -52($fp)
	sw $t6, -56($fp)
	sw $t7, -60($fp)
	sw $s0, -4($fp)
	s.s $f1, -64($fp)
	s.s $f2, -68($fp)
	s.s $f3, -72($fp)
#   Br {dest = Label {lblstr = "ForHeader", lblnum = 10}}
	j ForHeader
ForHeader:
#   CondBr {rel = LEI, op0 = R "L", op1 = R "N", trueDest = Label {lblstr = "ForBody", lblnum = 11}, falseDest = Label {lblstr = "ForExit", lblnum = 12}}
	lw $a1, 12($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody
	j ForExit
ForBody:
#   _t15 := subi N L
	lw $a1, 12($fp)
	lw $a2, -4($fp)
	sub $a3, $a1, $a2
#   I := _t15
#   f.Sum := #f0.0
	li.s $f1, 0.0
#   _t16 := subi N #i1
	sub $a2, $a1, 1
#   J := I
	sw $a2, -76($fp)
	sw $a3, -12($fp)
	sw $a3, -8($fp)
	sw $a3, -80($fp)
	s.s $f1, -16($fp)
#   Br {dest = Label {lblstr = "ForHeader_1", lblnum = 13}}
	j ForHeader_1
ForHeader_1:
#   CondBr {rel = LEI, op0 = R "J", op1 = T 16, trueDest = Label {lblstr = "ForBody_1", lblnum = 14}, falseDest = Label {lblstr = "ForExit_1", lblnum = 15}}
	lw $a1, -76($fp)
	lw $a2, -12($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_1
	j ForExit_1
ForBody_1:
#   _t17 := muli I #i44
	lw $a1, -8($fp)
	mul $a2, $a1, 44
#   _t18 := muli J #i4
	lw $a1, -12($fp)
	mul $a3, $a1, 4
#   _t19 := addi _t17 _t18
	add $a1, $a2, $a3
#   _tf20 := Coef[_t19]
	l.s $f1, Coef($a1)
#   _t21 := muli J #i4
	lw $t0, -12($fp)
	mul $t1, $t0, 4
#   _tf22 := X[_t21]
	l.s $f2, X($t1)
#   _tf23 := mulf _tf20 _tf22
	mul.s $f3, $f1, $f2
#   _tf24 := addf f.Sum _tf23
	l.s $f4, -16($fp)
	add.s $f5, $f4, $f3
#   f.Sum := _tf24
#   J := addi J #i1
	add $t0, $t0, 1
	sw $a1, -84($fp)
	sw $a2, -88($fp)
	sw $a3, -92($fp)
	sw $t0, -12($fp)
	sw $t1, -96($fp)
	s.s $f1, -100($fp)
	s.s $f2, -104($fp)
	s.s $f3, -108($fp)
	s.s $f5, -16($fp)
	s.s $f5, -112($fp)
#   Br {dest = Label {lblstr = "ForHeader_1", lblnum = 13}}
	j ForHeader_1
ForExit_1:
#   _t25 := muli I #i44
	lw $a1, -8($fp)
	mul $a2, $a1, 44
#   _t26 := muli N #i4
	lw $a1, 12($fp)
	mul $a3, $a1, 4
#   _t27 := addi _t25 _t26
	add $a1, $a2, $a3
#   _tf28 := Coef[_t27]
	l.s $f1, Coef($a1)
#   _tf29 := subf _tf28 f.Sum
	l.s $f2, -16($fp)
	sub.s $f3, $f1, $f2
#   _t30 := muli I #i44
	lw $t0, -8($fp)
	mul $t1, $t0, 44
#   _t31 := muli I #i4
	mul $t2, $t0, 4
#   _t32 := addi _t30 _t31
	add $t0, $t1, $t2
#   _tf33 := Coef[_t32]
	l.s $f2, Coef($t0)
#   _tf34 := divf _tf29 _tf33
	div.s $f4, $f3, $f2
#   _t35 := muli I #i4
	lw $t3, -8($fp)
	mul $t4, $t3, 4
#   X[_t35] := _tf34
	s.s $f4, X($t4)
#   L := addi L #i1
	lw $t3, -4($fp)
	add $t3, $t3, 1
	sw $a1, -116($fp)
	sw $a2, -120($fp)
	sw $a3, -124($fp)
	sw $t0, -128($fp)
	sw $t1, -132($fp)
	sw $t2, -136($fp)
	sw $t3, -4($fp)
	sw $t4, -140($fp)
	s.s $f1, -144($fp)
	s.s $f2, -148($fp)
	s.s $f3, -152($fp)
	s.s $f4, -156($fp)
#   Br {dest = Label {lblstr = "ForHeader", lblnum = 10}}
	j ForHeader
ForExit:
#   Br {dest = Label {lblstr = "Return_1", lblnum = 8}}
	j Return_1
Return_1:
#   ; Epilog for procedure backSubstitution
# Epilog for procedure backSubstitution
#   epilog 16
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# elimination
proc_elimination:
#   ; Procedure at (68,1)
# Procedure at (68,1)
#   prolog 20
	# Prolog 164
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 164

#   var N_1 12 4
#   var K -4 4
#   var IMax -8 4
#   var I_1 -12 4
#   var J_1 -16 4
#   fvar C -20 4
#   _t36 := subi N_1 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   K := #i0
	li $a1, 0
	sw $a1, -4($fp)
	sw $a2, -24($fp)
#   Br {dest = Label {lblstr = "ForHeader_2", lblnum = 18}}
	j ForHeader_2
ForHeader_2:
#   CondBr {rel = LEI, op0 = R "K", op1 = T 36, trueDest = Label {lblstr = "ForBody_2", lblnum = 19}, falseDest = Label {lblstr = "ForExit_2", lblnum = 20}}
	lw $a1, -24($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_2
	j ForExit_2
ForBody_2:
#   IfBr {cond = R "Solvable", trueDest = Label {lblstr = "YesGuard_2", lblnum = 22}, falseDest = Label {lblstr = "IfExit_1", lblnum = 21}}
	lw $a1, Solvable+0
	bne $a1, $0, YesGuard_2
	j IfExit_1
YesGuard_2:
#   _t37 := subi N_1 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   param _t37
	sw $a2, -28($fp)
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   param K
	lw $a1, -4($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   _t38 := call "maxPivot"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_maxPivot
	lw $a1, 8($fp)
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   IMax := _t38
#   _t39 := muli IMax #i44
	mul $a2, $a1, 44
#   _t40 := muli K #i4
	lw $a3, -4($fp)
	mul $t0, $a3, 4
#   _t41 := addi _t39 _t40
	add $a3, $a2, $t0
#   _tf42 := Coef[_t41]
	l.s $f1, Coef($a3)
	sw $a1, -8($fp)
	sw $a1, -32($fp)
	sw $a2, -36($fp)
	sw $a3, -40($fp)
	sw $t0, -44($fp)
	s.s $f1, -48($fp)
#   CondBr {rel = LTF, op0 = TF 42, op1 = C (FC 1.0e-5), trueDest = Label {lblstr = "YesGuard_3", lblnum = 24}, falseDest = Label {lblstr = "NextGuard_1", lblnum = 25}}
	li.s $f0, 1.0e-5
	c.lt.s $f1, $f0
	bc1t YesGuard_3
	j NextGuard_1
YesGuard_3:
#   Solvable := #False
	li $a1, 0
	sw $a1, Solvable+0
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 23}}
	j IfExit_2
NextGuard_1:
#   Br {dest = Label {lblstr = "YesGuard_4", lblnum = 26}}
	j YesGuard_4
YesGuard_4:
#   param N_1
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param IMax
	lw $a1, -8($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param K
	lw $a1, -4($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "swapRows"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_swapRows
#   cleanup 12
	lw $fp, 0($fp)
	add $sp, $sp, 24
#   _t43 := addi K #i1
	lw $a1, -4($fp)
	add $a2, $a1, 1
#   _t44 := subi N_1 #i1
	lw $a1, 12($fp)
	sub $a3, $a1, 1
#   I_1 := _t43
	sw $a2, -12($fp)
	sw $a2, -52($fp)
	sw $a3, -56($fp)
#   Br {dest = Label {lblstr = "ForHeader_3", lblnum = 27}}
	j ForHeader_3
ForHeader_3:
#   CondBr {rel = LEI, op0 = R "I_1", op1 = T 44, trueDest = Label {lblstr = "ForBody_3", lblnum = 28}, falseDest = Label {lblstr = "ForExit_3", lblnum = 29}}
	lw $a1, -56($fp)
	lw $a2, -12($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_3
	j ForExit_3
ForBody_3:
#   _t45 := muli I_1 #i44
	lw $a1, -12($fp)
	mul $a2, $a1, 44
#   _t46 := muli K #i4
	lw $a1, -4($fp)
	mul $a3, $a1, 4
#   _t47 := addi _t45 _t46
	add $a1, $a2, $a3
#   _tf48 := Coef[_t47]
	l.s $f1, Coef($a1)
#   _t49 := muli K #i44
	lw $t0, -4($fp)
	mul $t1, $t0, 44
#   _t50 := muli K #i4
	mul $t2, $t0, 4
#   _t51 := addi _t49 _t50
	add $t0, $t1, $t2
#   _tf52 := Coef[_t51]
	l.s $f2, Coef($t0)
#   _tf53 := divf _tf48 _tf52
	div.s $f3, $f1, $f2
#   f.C := _tf53
#   _t54 := addi K #i1
	lw $t3, -4($fp)
	add $t4, $t3, 1
#   J_1 := _t54
	sw $a1, -60($fp)
	sw $a2, -64($fp)
	sw $a3, -68($fp)
	sw $t0, -72($fp)
	sw $t1, -76($fp)
	sw $t2, -80($fp)
	sw $t4, -16($fp)
	sw $t4, -84($fp)
	s.s $f1, -88($fp)
	s.s $f2, -92($fp)
	s.s $f3, -20($fp)
	s.s $f3, -96($fp)
#   Br {dest = Label {lblstr = "ForHeader_4", lblnum = 30}}
	j ForHeader_4
ForHeader_4:
#   CondBr {rel = LEI, op0 = R "J_1", op1 = R "N_1", trueDest = Label {lblstr = "ForBody_4", lblnum = 31}, falseDest = Label {lblstr = "ForExit_4", lblnum = 32}}
	lw $a1, 12($fp)
	lw $a2, -16($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_4
	j ForExit_4
ForBody_4:
#   _t55 := muli I_1 #i44
	lw $a1, -12($fp)
	mul $a2, $a1, 44
#   _t56 := muli J_1 #i4
	lw $a1, -16($fp)
	mul $a3, $a1, 4
#   _t57 := addi _t55 _t56
	add $a1, $a2, $a3
#   _tf58 := Coef[_t57]
	l.s $f1, Coef($a1)
#   _t59 := muli K #i44
	lw $t0, -4($fp)
	mul $t1, $t0, 44
#   _t60 := muli J_1 #i4
	lw $t0, -16($fp)
	mul $t2, $t0, 4
#   _t61 := addi _t59 _t60
	add $t0, $t1, $t2
#   _tf62 := Coef[_t61]
	l.s $f2, Coef($t0)
#   _tf63 := mulf _tf62 f.C
	l.s $f3, -20($fp)
	mul.s $f4, $f2, $f3
#   _tf64 := subf _tf58 _tf63
	sub.s $f3, $f1, $f4
#   _t65 := muli I_1 #i44
	lw $t3, -12($fp)
	mul $t4, $t3, 44
#   _t66 := muli J_1 #i4
	lw $t3, -16($fp)
	mul $t5, $t3, 4
#   _t67 := addi _t65 _t66
	add $t3, $t4, $t5
#   Coef[_t67] := _tf64
	s.s $f3, Coef($t3)
#   J_1 := addi J_1 #i1
	lw $t6, -16($fp)
	add $t6, $t6, 1
	sw $a1, -100($fp)
	sw $a2, -104($fp)
	sw $a3, -108($fp)
	sw $t0, -112($fp)
	sw $t1, -116($fp)
	sw $t2, -120($fp)
	sw $t3, -124($fp)
	sw $t4, -128($fp)
	sw $t5, -132($fp)
	sw $t6, -16($fp)
	s.s $f1, -136($fp)
	s.s $f2, -140($fp)
	s.s $f3, -144($fp)
	s.s $f4, -148($fp)
#   Br {dest = Label {lblstr = "ForHeader_4", lblnum = 30}}
	j ForHeader_4
ForExit_4:
#   _t68 := muli I_1 #i44
	lw $a1, -12($fp)
	mul $a2, $a1, 44
#   _t69 := muli K #i4
	lw $a1, -4($fp)
	mul $a3, $a1, 4
#   _t70 := addi _t68 _t69
	add $a1, $a2, $a3
#   Coef[_t70] := #f0.0
	li.s $f0, 0.0
	s.s $f0, Coef($a1)
#   I_1 := addi I_1 #i1
	lw $t0, -12($fp)
	add $t0, $t0, 1
	sw $a1, -152($fp)
	sw $a2, -156($fp)
	sw $a3, -160($fp)
	sw $t0, -12($fp)
#   Br {dest = Label {lblstr = "ForHeader_3", lblnum = 27}}
	j ForHeader_3
ForExit_3:
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 23}}
	j IfExit_2
IfExit_2:
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 21}}
	j IfExit_1
IfExit_1:
#   K := addi K #i1
	lw $a1, -4($fp)
	add $a1, $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "ForHeader_2", lblnum = 18}}
	j ForHeader_2
ForExit_2:
#   Br {dest = Label {lblstr = "Return_2", lblnum = 16}}
	j Return_2
Return_2:
#   ; Epilog for procedure elimination
# Epilog for procedure elimination
#   epilog 20
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# gauss
proc_gauss:
#   ; Procedure at (47,1)
# Procedure at (47,1)
#   prolog 8
	# Prolog 32
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 32

#   var N_2 12 4
#   var Eq -4 4
#   var Var -8 4
#   param &_str6
	add $sp, $sp, -4
	la $v0, _str6
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   _t71 := subi N_2 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   Eq := #i0
	li $a1, 0
	sw $a1, -4($fp)
	sw $a2, -12($fp)
#   Br {dest = Label {lblstr = "ForHeader_5", lblnum = 35}}
	j ForHeader_5
ForHeader_5:
#   CondBr {rel = LEI, op0 = R "Eq", op1 = T 71, trueDest = Label {lblstr = "ForBody_5", lblnum = 36}, falseDest = Label {lblstr = "ForExit_5", lblnum = 37}}
	lw $a1, -12($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_5
	j ForExit_5
ForBody_5:
#   Var := #i0
	li $a1, 0
	sw $a1, -8($fp)
#   Br {dest = Label {lblstr = "ForHeader_6", lblnum = 38}}
	j ForHeader_6
ForHeader_6:
#   CondBr {rel = LEI, op0 = R "Var", op1 = R "N_2", trueDest = Label {lblstr = "ForBody_6", lblnum = 39}, falseDest = Label {lblstr = "ForExit_6", lblnum = 40}}
	lw $a1, 12($fp)
	lw $a2, -8($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_6
	j ForExit_6
ForBody_6:
#   param &_str7
	add $sp, $sp, -4
	la $v0, _str7
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param Eq
	lw $a1, -4($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "_writeInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeInteger
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param &_str8
	add $sp, $sp, -4
	la $v0, _str8
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param Var
	lw $a1, -8($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "_writeInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeInteger
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param &_str9
	add $sp, $sp, -4
	la $v0, _str9
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   _t72 := muli Eq #i44
	lw $a1, -4($fp)
	mul $a2, $a1, 44
#   _t73 := muli Var #i4
	lw $a1, -8($fp)
	mul $a3, $a1, 4
#   _t74 := addi _t72 _t73
	add $a1, $a2, $a3
#   _tf75 := call "_readFloat"
	sw $a1, -16($fp)
	sw $a2, -20($fp)
	sw $a3, -24($fp)
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readFloat
	l.s $f1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   Coef[_t74] := _tf75
	lw $a1, -16($fp)
	s.s $f1, Coef($a1)
#   Var := addi Var #i1
	lw $a1, -8($fp)
	add $a1, $a1, 1
	sw $a1, -8($fp)
	s.s $f1, -28($fp)
#   Br {dest = Label {lblstr = "ForHeader_6", lblnum = 38}}
	j ForHeader_6
ForExit_6:
#   Eq := addi Eq #i1
	lw $a1, -4($fp)
	add $a1, $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "ForHeader_5", lblnum = 35}}
	j ForHeader_5
ForExit_5:
#   Solvable := #True
	li $a1, 1
#   param N_2
	sw $a1, Solvable+0
	lw $a2, 12($fp)
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   call "elimination"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_elimination
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   IfBr {cond = R "Solvable", trueDest = Label {lblstr = "YesGuard_5", lblnum = 42}, falseDest = Label {lblstr = "IfExit_3", lblnum = 41}}
	lw $a1, Solvable+0
	bne $a1, $0, YesGuard_5
	j IfExit_3
YesGuard_5:
#   param N_2
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "backSubstitution"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_backSubstitution
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "IfExit_3", lblnum = 41}}
	j IfExit_3
IfExit_3:
#   param N_2
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "showSolution"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_showSolution
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "Return_3", lblnum = 33}}
	j Return_3
Return_3:
#   ; Epilog for procedure gauss
# Epilog for procedure gauss
#   epilog 8
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# main
proc_main:
#   ; Procedure at (24,1)
# Procedure at (24,1)
#   prolog 8
	# Prolog 8
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 8

#   var Done -4 4
#   Done := #False
	li $a1, 0
#   var N_3 -8 4
#   param &_str0
	sw $a1, -4($fp)
	add $sp, $sp, -4
	la $v0, _str0
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 45}}
	j WhileHeader
WhileHeader:
#   IfBr {cond = R "Done", trueDest = Label {lblstr = "WhileExit", lblnum = 46}, falseDest = Label {lblstr = "YesGuard_6", lblnum = 47}}
	lw $a1, -4($fp)
	bne $a1, $0, WhileExit
	j YesGuard_6
YesGuard_6:
#   param &_str1
	add $sp, $sp, -4
	la $v0, _str1
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param &_str2
	add $sp, $sp, -4
	la $v0, _str2
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param &_str3
	add $sp, $sp, -4
	la $v0, _str3
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param &_str4
	add $sp, $sp, -4
	la $v0, _str4
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   N_3 := call "_readInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
	sw $a1, -8($fp)
#   CondBr {rel = EQI, op0 = R "N_3", op1 = C (IC 0), trueDest = Label {lblstr = "YesGuard_7", lblnum = 49}, falseDest = Label {lblstr = "NextGuard_2", lblnum = 50}}
	li $v0, 0
	beq $a1, $v0, YesGuard_7
	j NextGuard_2
YesGuard_7:
#   param &_str5
	add $sp, $sp, -4
	la $v0, _str5
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Done := #True
	li $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "IfExit_4", lblnum = 48}}
	j IfExit_4
NextGuard_2:
#   CondBr {rel = LEI, op0 = C (IC 1), op1 = R "N_3", trueDest = Label {lblstr = "JC_andalso", lblnum = 52}, falseDest = Label {lblstr = "IfExit_4", lblnum = 48}}
	li $v0, 1
	lw $a1, -8($fp)
	slt $v0, $a1, $v0
	beq $v0, $0, JC_andalso
	j IfExit_4
JC_andalso:
#   CondBr {rel = LTI, op0 = R "N_3", op1 = C (IC 11), trueDest = Label {lblstr = "YesGuard_8", lblnum = 51}, falseDest = Label {lblstr = "IfExit_4", lblnum = 48}}
	li $v0, 11
	lw $a1, -8($fp)
	slt $v0, $a1, $v0
	bne $v0, $0, YesGuard_8
	j IfExit_4
YesGuard_8:
#   param N_3
	lw $a1, -8($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "gauss"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_gauss
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "IfExit_4", lblnum = 48}}
	j IfExit_4
IfExit_4:
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 45}}
	j WhileHeader
WhileExit:
#   Br {dest = Label {lblstr = "Return_4", lblnum = 43}}
	j Return_4
Return_4:
#   ; Epilog for procedure main
# Epilog for procedure main
#   epilog 8
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# maxPivot
proc_maxPivot:
#   ; Procedure at (98,1)
# Procedure at (98,1)
#   prolog 16
	# Prolog 68
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 68

#   var K_1 12 4
#   var N_4 16 4
#   var Best -4 4
#   Best := K_1
	lw $a1, 12($fp)
#   fvar BestVal -8 4
#   _t76 := muli K_1 #i44
	mul $a2, $a1, 44
#   _t77 := muli K_1 #i4
	mul $a3, $a1, 4
#   _t78 := addi _t76 _t77
	add $t0, $a2, $a3
#   _tf79 := Coef[_t78]
	l.s $f1, Coef($t0)
#   f.BestVal := _tf79
#   var I_2 -12 4
#   _t80 := addi K_1 #i1
	add $t1, $a1, 1
#   _t81 := subi N_4 #i1
	lw $t2, 16($fp)
	sub $t3, $t2, 1
#   I_2 := _t80
	sw $a1, -4($fp)
	sw $a1, 12($fp)
	sw $a2, -20($fp)
	sw $a3, -24($fp)
	sw $t0, -28($fp)
	sw $t1, -12($fp)
	sw $t1, -32($fp)
	sw $t3, -36($fp)
	s.s $f1, -8($fp)
	s.s $f1, -40($fp)
#   Br {dest = Label {lblstr = "ForHeader_7", lblnum = 55}}
	j ForHeader_7
ForHeader_7:
#   CondBr {rel = LEI, op0 = R "I_2", op1 = T 81, trueDest = Label {lblstr = "ForBody_7", lblnum = 56}, falseDest = Label {lblstr = "ForExit_7", lblnum = 57}}
	lw $a1, -36($fp)
	lw $a2, -12($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_7
	j ForExit_7
ForBody_7:
#   fvar Val -16 4
#   _t82 := muli I_2 #i44
	lw $a1, -12($fp)
	mul $a2, $a1, 44
#   _t83 := muli K_1 #i4
	lw $a1, 12($fp)
	mul $a3, $a1, 4
#   _t84 := addi _t82 _t83
	add $a1, $a2, $a3
#   _tf85 := Coef[_t84]
	l.s $f1, Coef($a1)
#   param _tf85
	sw $a1, -44($fp)
	sw $a2, -48($fp)
	sw $a3, -52($fp)
	s.s $f1, -56($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   _tf86 := call "abs"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_abs
	l.s $f2, 8($fp)
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   f.Val := _tf86
	s.s $f2, -16($fp)
	s.s $f2, -60($fp)
#   CondBr {rel = GTF, op0 = RF "Val", op1 = RF "BestVal", trueDest = Label {lblstr = "YesGuard_9", lblnum = 59}, falseDest = Label {lblstr = "IfExit_5", lblnum = 58}}
	l.s $f1, -8($fp)
	c.lt.s $f1, $f2
	bc1t YesGuard_9
	j IfExit_5
YesGuard_9:
#   Best := I_2
	lw $a1, -12($fp)
#   f.BestVal := f.Val
	l.s $f1, -16($fp)
	sw $a1, -4($fp)
	sw $a1, -12($fp)
	s.s $f1, -8($fp)
	s.s $f1, -16($fp)
#   Br {dest = Label {lblstr = "IfExit_5", lblnum = 58}}
	j IfExit_5
IfExit_5:
#   I_2 := addi I_2 #i1
	lw $a1, -12($fp)
	add $a1, $a1, 1
	sw $a1, -12($fp)
#   Br {dest = Label {lblstr = "ForHeader_7", lblnum = 55}}
	j ForHeader_7
ForExit_7:
#   answer Best
	lw $a1, -4($fp)
	sw $a1, 8($fp)
#   Br {dest = Label {lblstr = "Return_5", lblnum = 53}}
	j Return_5
Return_5:
#   ; Epilog for procedure maxPivot
# Epilog for procedure maxPivot
#   epilog 16
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# showSolution
proc_showSolution:
#   ; Procedure at (131,1)
# Procedure at (131,1)
#   prolog 4
	# Prolog 20
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 20

#   var N_5 12 4
#   IfBr {cond = R "Solvable", trueDest = Label {lblstr = "YesGuard_10", lblnum = 63}, falseDest = Label {lblstr = "NextGuard_3", lblnum = 64}}
	lw $a1, Solvable+0
	bne $a1, $0, YesGuard_10
	j NextGuard_3
YesGuard_10:
#   param &_str10
	add $sp, $sp, -4
	la $v0, _str10
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   var I_3 -4 4
#   _t87 := subi N_5 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   I_3 := #i0
	li $a1, 0
	sw $a1, -4($fp)
	sw $a2, -8($fp)
#   Br {dest = Label {lblstr = "ForHeader_8", lblnum = 65}}
	j ForHeader_8
ForHeader_8:
#   CondBr {rel = LEI, op0 = R "I_3", op1 = T 87, trueDest = Label {lblstr = "ForBody_8", lblnum = 66}, falseDest = Label {lblstr = "ForExit_8", lblnum = 67}}
	lw $a1, -8($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_8
	j ForExit_8
ForBody_8:
#   param &_str11
	add $sp, $sp, -4
	la $v0, _str11
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param I_3
	lw $a1, -4($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "_writeInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeInteger
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param &_str12
	add $sp, $sp, -4
	la $v0, _str12
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   _t88 := muli I_3 #i4
	lw $a1, -4($fp)
	mul $a2, $a1, 4
#   _tf89 := X[_t88]
	l.s $f1, X($a2)
#   param _tf89
	sw $a2, -12($fp)
	s.s $f1, -16($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   call "_writeFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeFloat
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param &_str13
	add $sp, $sp, -4
	la $v0, _str13
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   I_3 := addi I_3 #i1
	lw $a1, -4($fp)
	add $a1, $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "ForHeader_8", lblnum = 65}}
	j ForHeader_8
ForExit_8:
#   Br {dest = Label {lblstr = "IfExit_6", lblnum = 62}}
	j IfExit_6
NextGuard_3:
#   IfBr {cond = R "Solvable", trueDest = Label {lblstr = "IfExit_6", lblnum = 62}, falseDest = Label {lblstr = "YesGuard_11", lblnum = 68}}
	lw $a1, Solvable+0
	bne $a1, $0, IfExit_6
	j YesGuard_11
YesGuard_11:
#   param &_str14
	add $sp, $sp, -4
	la $v0, _str14
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "IfExit_6", lblnum = 62}}
	j IfExit_6
IfExit_6:
#   Br {dest = Label {lblstr = "Return_6", lblnum = 60}}
	j Return_6
Return_6:
#   ; Epilog for procedure showSolution
# Epilog for procedure showSolution
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# swapRows
proc_swapRows:
#   ; Procedure at (110,1)
# Procedure at (110,1)
#   prolog 8
	# Prolog 80
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 80

#   var A 12 4
#   var B 16 4
#   var N_6 20 4
#   CondBr {rel = NEI, op0 = R "A", op1 = R "B", trueDest = Label {lblstr = "YesGuard_12", lblnum = 72}, falseDest = Label {lblstr = "IfExit_7", lblnum = 71}}
	lw $a1, 16($fp)
	lw $a2, 12($fp)
	bne $a2, $a1, YesGuard_12
	j IfExit_7
YesGuard_12:
#   var I_4 -4 4
#   fvar T -8 4
#   _t90 := subi N_6 #i1
	lw $a1, 20($fp)
	sub $a2, $a1, 1
#   I_4 := #i0
	li $a1, 0
	sw $a1, -4($fp)
	sw $a2, -12($fp)
#   Br {dest = Label {lblstr = "ForHeader_9", lblnum = 73}}
	j ForHeader_9
ForHeader_9:
#   CondBr {rel = LEI, op0 = R "I_4", op1 = T 90, trueDest = Label {lblstr = "ForBody_9", lblnum = 74}, falseDest = Label {lblstr = "ForExit_9", lblnum = 75}}
	lw $a1, -12($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_9
	j ForExit_9
ForBody_9:
#   _t91 := muli A #i44
	lw $a1, 12($fp)
	mul $a2, $a1, 44
#   _t92 := muli I_4 #i4
	lw $a1, -4($fp)
	mul $a3, $a1, 4
#   _t93 := addi _t91 _t92
	add $a1, $a2, $a3
#   _tf94 := Coef[_t93]
	l.s $f1, Coef($a1)
#   f.T := _tf94
#   _t95 := muli B #i44
	lw $t0, 16($fp)
	mul $t1, $t0, 44
#   _t96 := muli I_4 #i4
	lw $t0, -4($fp)
	mul $t2, $t0, 4
#   _t97 := addi _t95 _t96
	add $t0, $t1, $t2
#   _tf98 := Coef[_t97]
	l.s $f2, Coef($t0)
#   _t99 := muli A #i44
	lw $t3, 12($fp)
	mul $t4, $t3, 44
#   _t100 := muli I_4 #i4
	lw $t3, -4($fp)
	mul $t5, $t3, 4
#   _t101 := addi _t99 _t100
	add $t3, $t4, $t5
#   Coef[_t101] := _tf98
	s.s $f2, Coef($t3)
#   _t102 := muli B #i44
	lw $t6, 16($fp)
	mul $t7, $t6, 44
#   _t103 := muli I_4 #i4
	lw $t6, -4($fp)
	mul $s0, $t6, 4
#   _t104 := addi _t102 _t103
	add $t6, $t7, $s0
#   Coef[_t104] := f.T
	s.s $f1, Coef($t6)
#   I_4 := addi I_4 #i1
	lw $s1, -4($fp)
	add $s1, $s1, 1
	sw $a1, -16($fp)
	sw $a2, -20($fp)
	sw $a3, -24($fp)
	sw $t0, -28($fp)
	sw $t1, -32($fp)
	sw $t2, -36($fp)
	sw $t3, -40($fp)
	sw $t4, -44($fp)
	sw $t5, -48($fp)
	sw $t6, -52($fp)
	sw $t7, -56($fp)
	sw $s0, -60($fp)
	sw $s1, -4($fp)
	s.s $f1, -8($fp)
	s.s $f1, -64($fp)
	s.s $f2, -68($fp)
#   Br {dest = Label {lblstr = "ForHeader_9", lblnum = 73}}
	j ForHeader_9
ForExit_9:
#   Br {dest = Label {lblstr = "IfExit_7", lblnum = 71}}
	j IfExit_7
IfExit_7:
#   Br {dest = Label {lblstr = "Return_7", lblnum = 69}}
	j Return_7
Return_7:
#   ; Epilog for procedure swapRows
# Epilog for procedure swapRows
#   epilog 8
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
