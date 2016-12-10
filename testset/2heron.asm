	.data
	.align 2
_base_header: .space 8
	.align 2
_last_used: .space 4
_true: .asciiz "true\n"
_false: .asciiz "false\n"
_str2: .asciiz "\tun numero en punto flotante negativo para salir, o\n"
_str3: .asciiz "\tun numero en punto flotante no negativo para calcular su raiz cuadrada mediante el metodo de Heron.\n"
_str7: .asciiz "\n"
_str6: .asciiz "< "
_str4: .asciiz "> "
_str5: .asciiz "Adios.\n"
_str0: .asciiz "Calculo de Raiz Cuadrada por el Metodo de Heron.\n"
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
#   ; Procedure at (44,1)
# Procedure at (44,1)
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
#   ; Procedure at (34,1)
# Procedure at (34,1)
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
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 10}}
	j WhileHeader
WhileHeader:
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
#   CondBr {rel = GTF, op0 = TF 4, op1 = C (FC 1.0e-6), trueDest = Label {lblstr = "YesGuard_2", lblnum = 12}, falseDest = Label {lblstr = "WhileExit", lblnum = 11}}
	li.s $f0, 1.0e-6
	c.lt.s $f0, $f1
	bc1t YesGuard_2
	j WhileExit
YesGuard_2:
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
#   param f.Sqrt
	s.s $f1, -24($fp)
	s.s $f2, -4($fp)
	s.s $f2, -28($fp)
	s.s $f3, -32($fp)
	add $sp, $sp, -4
	s.s $f2, 0($sp)
#   call "_writeFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeFloat
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
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 10}}
	j WhileHeader
WhileExit:
#   answer f.Sqrt
	l.s $f1, -4($fp)
	s.s $f1, 8($fp)
#   Br {dest = Label {lblstr = "Return_1", lblnum = 8}}
	j Return_1
Return_1:
#   ; Epilog for procedure heron
# Epilog for procedure heron
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# main
proc_main:
#   ; Procedure at (13,1)
# Procedure at (13,1)
#   prolog 8
	# Prolog 12
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 12

#   var Done -4 4
#   Done := #False
	li $a1, 0
#   fvar X_2 -8 4
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
#   f.X_2 := call "_readFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readFloat
	l.s $f1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
	s.s $f1, -8($fp)
#   CondBr {rel = LTF, op0 = RF "X_2", op1 = C (FC 0.0), trueDest = Label {lblstr = "YesGuard_4", lblnum = 19}, falseDest = Label {lblstr = "NextGuard_1", lblnum = 20}}
	li.s $f0, 0.0
	c.lt.s $f1, $f0
	bc1t YesGuard_4
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
#   CondBr {rel = GEF, op0 = RF "X_2", op1 = C (FC 0.0), trueDest = Label {lblstr = "YesGuard_5", lblnum = 21}, falseDest = Label {lblstr = "IfExit_1", lblnum = 18}}
	li.s $f0, 0.0
	l.s $f1, -8($fp)
	c.le.s $f0, $f1
	bc1t YesGuard_5
	j IfExit_1
YesGuard_5:
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
#   param f.X_2
	l.s $f1, -8($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   _tf8 := call "heron"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_heron
	l.s $f1, 8($fp)
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param _tf8
	s.s $f1, -12($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   call "_writeFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeFloat
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
#   epilog 8
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
