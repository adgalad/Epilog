	.data
	.align 2
_base_header: .space 8
	.align 2
_last_used: .space 4
_true: .asciiz "true\n"
_false: .asciiz "false\n"
_str14: .asciiz "\t Desviacion Estandar: "
_str16: .asciiz "\t Maximo: "
_str15: .asciiz "\t Minimo: "
_str11: .asciiz "\t Promedio: "
_str9: .asciiz "\t Se tomaron "
_str13: .asciiz "\t Varianza: "
_str2: .asciiz "\tun numero no positivo para salir, o\n"
_str3: .asciiz "\tun numero positivo N para calcular los estadisticos de una secuencia de N numeros.\n"
_str12: .asciiz "\n"
_str10: .asciiz " muestras.\n"
_str7: .asciiz " numeros en punto flotante.\n1> "
_str4: .asciiz "> "
_str5: .asciiz "Adios.\n"
_str0: .asciiz "Calculo de estadisticos basicos.\n"
_str1: .asciiz "Introduzca\n"
_str6: .asciiz "Introduzca "
_str8: .asciiz "Resultados:\n"
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
#   ; Procedure at (99,1)
# Procedure at (99,1)
#   prolog 0
	# Prolog 8
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 8

#   fvar X 12 4
#   CondBr {rel = GEF, op0 = RF "X", op1 = C (FC 0.0), trueDest = Label {lblstr = "YesGuard", lblnum = 5}, falseDest = Label {lblstr = "NextGuard", lblnum = 6}}
	li.s $f0, 0.0
	l.s $f1, 12($fp)
	c.le.s $f0, $f1
	bc1t YesGuard
	j NextGuard
YesGuard:
#   answer f.X
	l.s $f1, 12($fp)
	s.s $f1, 8($fp)
#   Br {dest = Label {lblstr = "Return", lblnum = 2}}
	j Return
NextGuard:
#   CondBr {rel = LTF, op0 = RF "X", op1 = C (FC 0.0), trueDest = Label {lblstr = "YesGuard_1", lblnum = 7}, falseDest = Label {lblstr = "IfExit", lblnum = 4}}
	li.s $f0, 0.0
	l.s $f1, 12($fp)
	c.lt.s $f1, $f0
	bc1t YesGuard_1
	j IfExit
YesGuard_1:
#   _tf0 := negf f.X
	l.s $f1, 12($fp)
	li.s $f0, 0.0
	sub.s $f2, $f0, $f1
#   answer _tf0
	s.s $f2, 8($fp)
	s.s $f2, -4($fp)
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
# main
proc_main:
#   ; Procedure at (25,1)
# Procedure at (25,1)
#   prolog 8
	# Prolog 8
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 8

#   var Done -4 4
#   Done := #False
	li $a1, 0
#   var N -8 4
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
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 10}}
	j WhileHeader
WhileHeader:
#   IfBr {cond = R "Done", trueDest = Label {lblstr = "WhileExit", lblnum = 11}, falseDest = Label {lblstr = "YesGuard_2", lblnum = 12}}
	lw $a1, -4($fp)
	bne $a1, $0, WhileExit
	j YesGuard_2
YesGuard_2:
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
#   N := call "_readInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
	sw $a1, -8($fp)
#   CondBr {rel = LEI, op0 = R "N", op1 = C (IC 0), trueDest = Label {lblstr = "YesGuard_3", lblnum = 14}, falseDest = Label {lblstr = "NextGuard_1", lblnum = 15}}
	li $v0, 0
	slt $v0, $v0, $a1
	beq $v0, $0, YesGuard_3
	j NextGuard_1
YesGuard_3:
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
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 13}}
	j IfExit_1
NextGuard_1:
#   CondBr {rel = GTI, op0 = R "N", op1 = C (IC 0), trueDest = Label {lblstr = "YesGuard_4", lblnum = 16}, falseDest = Label {lblstr = "IfExit_1", lblnum = 13}}
	li $v0, 0
	lw $a1, -8($fp)
	slt $v0, $v0, $a1
	bne $v0, $0, YesGuard_4
	j IfExit_1
YesGuard_4:
#   param N
	lw $a1, -8($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "stats"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_stats
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 13}}
	j IfExit_1
IfExit_1:
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 10}}
	j WhileHeader
WhileExit:
#   Br {dest = Label {lblstr = "Return_1", lblnum = 8}}
	j Return_1
Return_1:
#   ; Epilog for procedure main
# Epilog for procedure main
#   epilog 8
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# sqrt
proc_sqrt:
#   ; Procedure at (90,1)
# Procedure at (90,1)
#   prolog 4
	# Prolog 36
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 36

#   fvar X_1 12 4
#   fvar Sqrt -4 4
#   f.Sqrt := #f1.0
	li.s $f1, 1.0
	s.s $f1, -4($fp)
#   Br {dest = Label {lblstr = "WhileHeader_1", lblnum = 19}}
	j WhileHeader_1
WhileHeader_1:
#   _tf1 := mulf f.Sqrt f.Sqrt
	l.s $f1, -4($fp)
	mul.s $f2, $f1, $f1
#   _tf2 := subf f.X_1 _tf1
	l.s $f1, 12($fp)
	sub.s $f3, $f1, $f2
#   _tf3 := divf _tf2 f.X_1
	div.s $f4, $f3, $f1
#   param _tf3
	s.s $f2, -8($fp)
	s.s $f3, -12($fp)
	s.s $f4, -16($fp)
	add $sp, $sp, -4
	s.s $f4, 0($sp)
#   _tf4 := call "abs"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_abs
	l.s $f1, 8($fp)
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
	s.s $f1, -20($fp)
#   CondBr {rel = GTF, op0 = TF 4, op1 = C (FC 1.0e-6), trueDest = Label {lblstr = "YesGuard_5", lblnum = 21}, falseDest = Label {lblstr = "WhileExit_1", lblnum = 20}}
	li.s $f0, 1.0e-6
	c.lt.s $f0, $f1
	bc1t YesGuard_5
	j WhileExit_1
YesGuard_5:
#   _tf5 := divf f.X_1 f.Sqrt
	l.s $f1, 12($fp)
	l.s $f2, -4($fp)
	div.s $f3, $f1, $f2
#   _tf6 := addf f.Sqrt _tf5
	add.s $f1, $f2, $f3
#   _tf7 := divf _tf6 #f2.0
	li.s $f0, 2.0
	div.s $f2, $f1, $f0
#   f.Sqrt := _tf7
	s.s $f1, -24($fp)
	s.s $f2, -4($fp)
	s.s $f2, -28($fp)
	s.s $f3, -32($fp)
#   Br {dest = Label {lblstr = "WhileHeader_1", lblnum = 19}}
	j WhileHeader_1
WhileExit_1:
#   answer f.Sqrt
	l.s $f1, -4($fp)
	s.s $f1, 8($fp)
#   Br {dest = Label {lblstr = "Return_2", lblnum = 17}}
	j Return_2
Return_2:
#   ; Epilog for procedure sqrt
# Epilog for procedure sqrt
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# stats
proc_stats:
#   ; Procedure at (46,1)
# Procedure at (46,1)
#   prolog 32
	# Prolog 104
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 104

#   var N_1 12 4
#   var I -4 4
#   fvar X_2 -8 4
#   fvar Sum -12 4
#   fvar SquareSum -16 4
#   fvar Max -20 4
#   fvar Min -24 4
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
#   param N_1
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "_writeInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeInteger
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
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
#   f.X_2 := call "_readFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readFloat
	l.s $f1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   f.Sum := f.X_2
#   _tf8 := mulf f.X_2 f.X_2
	mul.s $f2, $f1, $f1
#   f.SquareSum := _tf8
#   f.Max := f.X_2
#   f.Min := f.X_2
#   I := #i2
	li $a1, 2
	sw $a1, -4($fp)
	s.s $f1, -24($fp)
	s.s $f1, -20($fp)
	s.s $f1, -12($fp)
	s.s $f1, -8($fp)
	s.s $f2, -16($fp)
	s.s $f2, -36($fp)
#   Br {dest = Label {lblstr = "ForHeader", lblnum = 24}}
	j ForHeader
ForHeader:
#   CondBr {rel = LEI, op0 = R "I", op1 = R "N_1", trueDest = Label {lblstr = "ForBody", lblnum = 25}, falseDest = Label {lblstr = "ForExit", lblnum = 26}}
	lw $a1, 12($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody
	j ForExit
ForBody:
#   param I
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
#   f.X_2 := call "_readFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readFloat
	l.s $f1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   _tf9 := addf f.Sum f.X_2
	l.s $f2, -12($fp)
	add.s $f3, $f2, $f1
#   f.Sum := _tf9
#   _tf10 := mulf f.X_2 f.X_2
	mul.s $f2, $f1, $f1
#   _tf11 := addf f.SquareSum _tf10
	l.s $f4, -16($fp)
	add.s $f5, $f4, $f2
#   f.SquareSum := _tf11
	s.s $f1, -8($fp)
	s.s $f2, -40($fp)
	s.s $f3, -12($fp)
	s.s $f3, -44($fp)
	s.s $f5, -16($fp)
	s.s $f5, -48($fp)
#   CondBr {rel = GTF, op0 = RF "X_2", op1 = RF "Max", trueDest = Label {lblstr = "YesGuard_6", lblnum = 28}, falseDest = Label {lblstr = "NextGuard_2", lblnum = 29}}
	l.s $f4, -20($fp)
	c.lt.s $f4, $f1
	bc1t YesGuard_6
	j NextGuard_2
YesGuard_6:
#   f.Max := f.X_2
	l.s $f1, -8($fp)
	s.s $f1, -20($fp)
	s.s $f1, -8($fp)
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 27}}
	j IfExit_2
NextGuard_2:
#   CondBr {rel = LTF, op0 = RF "X_2", op1 = RF "Min", trueDest = Label {lblstr = "YesGuard_7", lblnum = 30}, falseDest = Label {lblstr = "IfExit_2", lblnum = 27}}
	l.s $f1, -24($fp)
	l.s $f2, -8($fp)
	c.lt.s $f2, $f1
	bc1t YesGuard_7
	j IfExit_2
YesGuard_7:
#   f.Min := f.X_2
	l.s $f1, -8($fp)
	s.s $f1, -24($fp)
	s.s $f1, -8($fp)
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 27}}
	j IfExit_2
IfExit_2:
#   I := addi I #i1
	lw $a1, -4($fp)
	add $a1, $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "ForHeader", lblnum = 24}}
	j ForHeader
ForExit:
#   fvar Avg -28 4
#   _tf12 := itof N_1
	lw $a1, 12($fp)
	mtc1 $a1, $f1
	cvt.s.w $f1, $f1
#   _tf13 := divf f.Sum _tf12
	l.s $f2, -12($fp)
	div.s $f3, $f2, $f1
#   f.Avg := _tf13
#   fvar Vrnz -32 4
#   _tf15 := #f2.0
	li.s $f2, 2.0
#   _tf14 := mulf _tf15 f.Avg
	mul.s $f4, $f2, $f3
#   _tf16 := mulf _tf14 f.Sum
	l.s $f5, -12($fp)
	mul.s $f6, $f4, $f5
#   _tf17 := subf f.SquareSum _tf16
	l.s $f5, -16($fp)
	sub.s $f7, $f5, $f6
#   _tf18 := mulf f.Avg f.Avg
	mul.s $f5, $f3, $f3
#   _tf19 := itof N_1
	mtc1 $a1, $f8
	cvt.s.w $f8, $f8
#   _tf20 := mulf _tf18 _tf19
	mul.s $f9, $f5, $f8
#   _tf21 := addf _tf17 _tf20
	add.s $f10, $f7, $f9
#   f.Vrnz := _tf21
#   _tf22 := itof N_1
	mtc1 $a1, $f11
	cvt.s.w $f11, $f11
#   _tf23 := divf f.Vrnz _tf22
	div.s $f13, $f10, $f11
#   f.Vrnz := _tf23
#   param &_str8
	s.s $f1, -52($fp)
	s.s $f2, -56($fp)
	s.s $f3, -28($fp)
	s.s $f3, -60($fp)
	s.s $f4, -64($fp)
	s.s $f5, -68($fp)
	s.s $f6, -72($fp)
	s.s $f7, -76($fp)
	s.s $f8, -80($fp)
	s.s $f9, -84($fp)
	s.s $f10, -88($fp)
	s.s $f11, -92($fp)
	s.s $f13, -32($fp)
	s.s $f13, -96($fp)
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
#   param N_1
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "_writeInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeInteger
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
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
#   param f.Avg
	l.s $f1, -28($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   call "_writeFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeFloat
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
#   param f.Vrnz
	l.s $f1, -32($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   call "_writeFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeFloat
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
#   param f.Vrnz
	l.s $f1, -32($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   _tf24 := call "sqrt"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_sqrt
	l.s $f1, 8($fp)
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param _tf24
	s.s $f1, -100($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   call "_writeFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeFloat
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
#   param &_str15
	add $sp, $sp, -4
	la $v0, _str15
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param f.Min
	l.s $f1, -24($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   call "_writeFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeFloat
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
#   param &_str16
	add $sp, $sp, -4
	la $v0, _str16
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param f.Max
	l.s $f1, -20($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   call "_writeFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeFloat
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
#   Br {dest = Label {lblstr = "Return_3", lblnum = 22}}
	j Return_3
Return_3:
#   ; Epilog for procedure stats
# Epilog for procedure stats
#   epilog 32
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
