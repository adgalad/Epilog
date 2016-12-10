	.data
	.align 2
_base_header: .space 8
	.align 2
_last_used: .space 4
_true: .asciiz "true\n"
_false: .asciiz "false\n"
_str6: .asciiz "\tun numero en punto flotante negativo para salir, o\n"
_str7: .asciiz "\tun numero en punto flotante no negativo para usarlo como radicando.\n"
_str2: .asciiz "\tun numero entero no positivo para salir, o\n"
_str3: .asciiz "\tun numero entero positivo para usarlo como indice.\n"
_str9: .asciiz "\n"
_str8: .asciiz "< "
_str4: .asciiz "> "
_str5: .asciiz "Adios.\n"
_str0: .asciiz "Calculo de Raiz Enesima por el Metodo de Heron.\n"
_str1: .asciiz "Introduzca\n"
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
#   ; Procedure at (59,1)
# Procedure at (59,1)
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
# heron
proc_heron:
#   ; Procedure at (47,1)
# Procedure at (47,1)
#   prolog 16
	# Prolog 72
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 72

#   fvar X_1 12 4
#   var N 16 4
#   fvar Rt -4 4
#   f.Rt := #f1.0
	li.s $f1, 1.0
#   fvar Nf -8 4
#   _tf1 := itof N
	lw $a1, 16($fp)
	mtc1 $a1, $f2
	cvt.s.w $f2, $f2
#   f.Nf := _tf1
	s.s $f1, -4($fp)
	s.s $f2, -8($fp)
	s.s $f2, -20($fp)
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 10}}
	j WhileHeader
WhileHeader:
#   param N
	lw $a1, 16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param f.Rt
	l.s $f1, -4($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   _tf2 := call "pow"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_pow
	l.s $f1, 8($fp)
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   _tf3 := subf f.X_1 _tf2
	l.s $f2, 12($fp)
	sub.s $f3, $f2, $f1
#   _tf4 := divf _tf3 f.X_1
	div.s $f4, $f3, $f2
#   param _tf4
	s.s $f1, -24($fp)
	s.s $f3, -28($fp)
	s.s $f4, -32($fp)
	add $sp, $sp, -4
	s.s $f4, 0($sp)
#   _tf5 := call "abs"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_abs
	l.s $f2, 8($fp)
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
	s.s $f2, -36($fp)
#   CondBr {rel = GTF, op0 = TF 5, op1 = C (FC 1.0e-6), trueDest = Label {lblstr = "YesGuard_2", lblnum = 12}, falseDest = Label {lblstr = "WhileExit", lblnum = 11}}
	li.s $f0, 1.0e-6
	c.lt.s $f0, $f2
	bc1t YesGuard_2
	j WhileExit
YesGuard_2:
#   fvar B -12 4
#   _t6 := subi N #i1
	lw $a1, 16($fp)
	sub $a2, $a1, 1
#   param _t6
	sw $a2, -40($fp)
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   param f.Rt
	l.s $f1, -4($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   _tf7 := call "pow"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_pow
	l.s $f1, 8($fp)
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   f.B := _tf7
#   fvar A -16 4
#   _tf8 := subf f.Nf #f1.0
	l.s $f2, -8($fp)
	li.s $f0, 1.0
	sub.s $f3, $f2, $f0
#   _tf9 := mulf _tf8 f.Rt
	l.s $f2, -4($fp)
	mul.s $f4, $f3, $f2
#   f.A := _tf9
#   _tf10 := divf f.X_1 f.B
	l.s $f2, 12($fp)
	div.s $f5, $f2, $f1
#   _tf11 := addf f.A _tf10
	add.s $f2, $f4, $f5
#   _tf12 := divf _tf11 f.Nf
	l.s $f6, -8($fp)
	div.s $f7, $f2, $f6
#   f.Rt := _tf12
	s.s $f1, -12($fp)
	s.s $f1, -44($fp)
	s.s $f2, -48($fp)
	s.s $f3, -52($fp)
	s.s $f4, -16($fp)
	s.s $f4, -56($fp)
	s.s $f5, -60($fp)
	s.s $f7, -4($fp)
	s.s $f7, -64($fp)
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 10}}
	j WhileHeader
WhileExit:
#   answer f.Rt
	l.s $f1, -4($fp)
	s.s $f1, 8($fp)
#   Br {dest = Label {lblstr = "Return_1", lblnum = 8}}
	j Return_1
Return_1:
#   ; Epilog for procedure heron
# Epilog for procedure heron
#   epilog 16
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# main
proc_main:
#   ; Procedure at (13,1)
# Procedure at (13,1)
#   prolog 12
	# Prolog 16
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 16

#   var Done -4 4
#   Done := #False
	li $a1, 0
#   var N_1 -8 4
#   fvar X_2 -12 4
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
#   Br {dest = Label {lblstr = "WhileHeader_1", lblnum = 15}}
	j WhileHeader_1
WhileHeader_1:
#   IfBr {cond = R "Done", trueDest = Label {lblstr = "WhileExit_1", lblnum = 16}, falseDest = Label {lblstr = "YesGuard_3", lblnum = 17}}
	lw $a1, -4($fp)
	bne $a1, $0, WhileExit_1
	j YesGuard_3
YesGuard_3:
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
#   N_1 := call "_readInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
	sw $a1, -8($fp)
#   CondBr {rel = LEI, op0 = R "N_1", op1 = C (IC 0), trueDest = Label {lblstr = "YesGuard_4", lblnum = 19}, falseDest = Label {lblstr = "NextGuard_1", lblnum = 20}}
	li $v0, 0
	slt $v0, $v0, $a1
	beq $v0, $0, YesGuard_4
	j NextGuard_1
YesGuard_4:
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
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 18}}
	j IfExit_1
NextGuard_1:
#   CondBr {rel = GTI, op0 = R "N_1", op1 = C (IC 0), trueDest = Label {lblstr = "YesGuard_5", lblnum = 21}, falseDest = Label {lblstr = "IfExit_1", lblnum = 18}}
	li $v0, 0
	lw $a1, -8($fp)
	slt $v0, $v0, $a1
	bne $v0, $0, YesGuard_5
	j IfExit_1
YesGuard_5:
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
	s.s $f1, -12($fp)
#   CondBr {rel = LTF, op0 = RF "X_2", op1 = C (FC 0.0), trueDest = Label {lblstr = "YesGuard_6", lblnum = 23}, falseDest = Label {lblstr = "NextGuard_2", lblnum = 24}}
	li.s $f0, 0.0
	c.lt.s $f1, $f0
	bc1t YesGuard_6
	j NextGuard_2
YesGuard_6:
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
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 22}}
	j IfExit_2
NextGuard_2:
#   CondBr {rel = GEF, op0 = RF "X_2", op1 = C (FC 0.0), trueDest = Label {lblstr = "YesGuard_7", lblnum = 25}, falseDest = Label {lblstr = "IfExit_2", lblnum = 22}}
	li.s $f0, 0.0
	l.s $f1, -12($fp)
	c.le.s $f0, $f1
	bc1t YesGuard_7
	j IfExit_2
YesGuard_7:
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
#   param N_1
	lw $a1, -8($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param f.X_2
	l.s $f1, -12($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   _tf13 := call "heron"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_heron
	l.s $f1, 8($fp)
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   param _tf13
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
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 22}}
	j IfExit_2
IfExit_2:
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 18}}
	j IfExit_1
IfExit_1:
#   Br {dest = Label {lblstr = "WhileHeader_1", lblnum = 15}}
	j WhileHeader_1
WhileExit_1:
#   Br {dest = Label {lblstr = "Return_2", lblnum = 13}}
	j Return_2
Return_2:
#   ; Epilog for procedure main
# Epilog for procedure main
#   epilog 12
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# pow
proc_pow:
#   ; Procedure at (64,1)
# Procedure at (64,1)
#   prolog 8
	# Prolog 20
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 20

#   fvar X_3 12 4
#   var N_2 16 4
#   var I -4 4
#   fvar R -8 4
#   f.R := f.X_3
	l.s $f1, 12($fp)
#   I := #i2
	li $a1, 2
	sw $a1, -4($fp)
	s.s $f1, -8($fp)
	s.s $f1, 12($fp)
#   Br {dest = Label {lblstr = "ForHeader", lblnum = 28}}
	j ForHeader
ForHeader:
#   CondBr {rel = LEI, op0 = R "I", op1 = R "N_2", trueDest = Label {lblstr = "ForBody", lblnum = 29}, falseDest = Label {lblstr = "ForExit", lblnum = 30}}
	lw $a1, 16($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody
	j ForExit
ForBody:
#   _tf14 := mulf f.R f.X_3
	l.s $f1, -8($fp)
	l.s $f2, 12($fp)
	mul.s $f3, $f1, $f2
#   f.R := _tf14
#   I := addi I #i1
	lw $a1, -4($fp)
	add $a1, $a1, 1
	sw $a1, -4($fp)
	s.s $f3, -8($fp)
	s.s $f3, -12($fp)
#   Br {dest = Label {lblstr = "ForHeader", lblnum = 28}}
	j ForHeader
ForExit:
#   answer f.R
	l.s $f1, -8($fp)
	s.s $f1, 8($fp)
#   Br {dest = Label {lblstr = "Return_3", lblnum = 26}}
	j Return_3
Return_3:
#   ; Epilog for procedure pow
# Epilog for procedure pow
#   epilog 8
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
