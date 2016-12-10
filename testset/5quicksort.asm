	.data
	.align 2
_base_header: .space 8
	.align 2
_last_used: .space 4
_true: .asciiz "true\n"
_false: .asciiz "false\n"
_str5: .asciiz "\t(C)   para ordenar Caracteres.\n"
_str4: .asciiz "\t(F)   para ordenar numeros en punto Flotante, o\n"
_str3: .asciiz "\t(I/E) para ordenar Enteros,\n"
_str2: .asciiz "\t(T)   para Terminar,\n"
_str19: .asciiz "\n"
_str21: .asciiz ": "
_str6: .asciiz "> "
_str7: .asciiz "Adios.\n"
_str20: .asciiz "Caracteres ordenados:\n"
_str13: .asciiz "Cuantos caracteres desea ordenar? > "
_str9: .asciiz "Cuantos enteros desea ordenar? > "
_str11: .asciiz "Cuantos numeros en punto flotante desea ordenar? > "
_str15: .asciiz "Enteros ordenados:\n"
_str17: .asciiz "Flotantes ordenados:\n"
_str1: .asciiz "Introduzca una letra:\n"
_str0: .asciiz "Multi Quicksort 'In Situ'.\n"
_str12: .asciiz "Ordenar Caracteres\n"
_str8: .asciiz "Ordenar Enteros\n"
_str10: .asciiz "Ordenar Numeros en Punto Flotante\n"
_str18: .asciiz "c"
_str16: .asciiz "f"
_str14: .asciiz "i"
	.align 2
Cs: .space 400
	.align 2
Fs: .space 400
	.align 2
Is: .space 400
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
# displayC
proc_displayC:
#   ; Procedure at (184,1)
# Procedure at (184,1)
#   prolog 4
	# Prolog 20
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 20

#   var N 12 4
#   var I -4 4
#   _t0 := subi N #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   I := #i0
	li $a1, 0
	sw $a1, -4($fp)
	sw $a2, -8($fp)
#   Br {dest = Label {lblstr = "ForHeader", lblnum = 4}}
	j ForHeader
ForHeader:
#   CondBr {rel = LEI, op0 = R "I", op1 = T 0, trueDest = Label {lblstr = "ForBody", lblnum = 5}, falseDest = Label {lblstr = "ForExit", lblnum = 6}}
	lw $a1, -8($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody
	j ForExit
ForBody:
#   param &_str18
	add $sp, $sp, -4
	la $v0, _str18
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
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
#   param &_str21
	add $sp, $sp, -4
	la $v0, _str21
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   _t1 := muli I #i4
	lw $a1, -4($fp)
	mul $a2, $a1, 4
#   _t2 := Cs[_t1]
	lw $a1, Cs($a2)
#   param _t2
	sw $a1, -12($fp)
	sw $a2, -16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "_writeChar"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeChar
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param &_str19
	add $sp, $sp, -4
	la $v0, _str19
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   I := addi I #i1
	lw $a1, -4($fp)
	add $a1, $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "ForHeader", lblnum = 4}}
	j ForHeader
ForExit:
#   Br {dest = Label {lblstr = "Return", lblnum = 2}}
	j Return
Return:
#   ; Epilog for procedure displayC
# Epilog for procedure displayC
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# displayF
proc_displayF:
#   ; Procedure at (179,1)
# Procedure at (179,1)
#   prolog 4
	# Prolog 20
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 20

#   var N_1 12 4
#   var I_1 -4 4
#   _t3 := subi N_1 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   I_1 := #i0
	li $a1, 0
	sw $a1, -4($fp)
	sw $a2, -8($fp)
#   Br {dest = Label {lblstr = "ForHeader_1", lblnum = 9}}
	j ForHeader_1
ForHeader_1:
#   CondBr {rel = LEI, op0 = R "I_1", op1 = T 3, trueDest = Label {lblstr = "ForBody_1", lblnum = 10}, falseDest = Label {lblstr = "ForExit_1", lblnum = 11}}
	lw $a1, -8($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_1
	j ForExit_1
ForBody_1:
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
#   param I_1
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
#   param &_str21
	add $sp, $sp, -4
	la $v0, _str21
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   _t4 := muli I_1 #i4
	lw $a1, -4($fp)
	mul $a2, $a1, 4
#   _tf5 := Fs[_t4]
	l.s $f1, Fs($a2)
#   param _tf5
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
#   param &_str19
	add $sp, $sp, -4
	la $v0, _str19
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   I_1 := addi I_1 #i1
	lw $a1, -4($fp)
	add $a1, $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "ForHeader_1", lblnum = 9}}
	j ForHeader_1
ForExit_1:
#   Br {dest = Label {lblstr = "Return_1", lblnum = 7}}
	j Return_1
Return_1:
#   ; Epilog for procedure displayF
# Epilog for procedure displayF
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# displayI
proc_displayI:
#   ; Procedure at (174,1)
# Procedure at (174,1)
#   prolog 4
	# Prolog 20
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 20

#   var N_2 12 4
#   var I_2 -4 4
#   _t6 := subi N_2 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   I_2 := #i0
	li $a1, 0
	sw $a1, -4($fp)
	sw $a2, -8($fp)
#   Br {dest = Label {lblstr = "ForHeader_2", lblnum = 14}}
	j ForHeader_2
ForHeader_2:
#   CondBr {rel = LEI, op0 = R "I_2", op1 = T 6, trueDest = Label {lblstr = "ForBody_2", lblnum = 15}, falseDest = Label {lblstr = "ForExit_2", lblnum = 16}}
	lw $a1, -8($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_2
	j ForExit_2
ForBody_2:
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
#   param I_2
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
#   param &_str21
	add $sp, $sp, -4
	la $v0, _str21
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   _t7 := muli I_2 #i4
	lw $a1, -4($fp)
	mul $a2, $a1, 4
#   _t8 := Is[_t7]
	lw $a1, Is($a2)
#   param _t8
	sw $a1, -12($fp)
	sw $a2, -16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "_writeInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeInteger
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param &_str19
	add $sp, $sp, -4
	la $v0, _str19
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   I_2 := addi I_2 #i1
	lw $a1, -4($fp)
	add $a1, $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "ForHeader_2", lblnum = 14}}
	j ForHeader_2
ForExit_2:
#   Br {dest = Label {lblstr = "Return_2", lblnum = 12}}
	j Return_2
Return_2:
#   ; Epilog for procedure displayI
# Epilog for procedure displayI
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# main
proc_main:
#   ; Procedure at (33,1)
# Procedure at (33,1)
#   prolog 16
	# Prolog 16
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 16

#   var Done -4 4
#   Done := #False
	li $a1, 0
#   var C -8 4
#   var Enter -12 4
#   var N_3 -16 4
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
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 19}}
	j WhileHeader
WhileHeader:
#   IfBr {cond = R "Done", trueDest = Label {lblstr = "WhileExit", lblnum = 20}, falseDest = Label {lblstr = "YesGuard", lblnum = 21}}
	lw $a1, -4($fp)
	bne $a1, $0, WhileExit
	j YesGuard
YesGuard:
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
#   C := call "_readChar"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readChar
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   Enter := call "_readChar"
	sw $a1, -8($fp)
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readChar
	lw $a2, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
	sw $a2, -12($fp)
#   CondBr {rel = EQI, op0 = R "C", op1 = C (CC 84), trueDest = Label {lblstr = "YesGuard_1", lblnum = 23}, falseDest = Label {lblstr = "JC_orelse", lblnum = 25}}
	li $v0, 84
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_1
	j JC_orelse
JC_orelse:
#   CondBr {rel = EQI, op0 = R "C", op1 = C (CC 116), trueDest = Label {lblstr = "YesGuard_1", lblnum = 23}, falseDest = Label {lblstr = "NextGuard", lblnum = 24}}
	li $v0, 116
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_1
	j NextGuard
YesGuard_1:
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
#   Done := #True
	li $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "IfExit", lblnum = 22}}
	j IfExit
NextGuard:
#   CondBr {rel = EQI, op0 = R "C", op1 = C (CC 73), trueDest = Label {lblstr = "YesGuard_2", lblnum = 26}, falseDest = Label {lblstr = "JC_orelse_3", lblnum = 30}}
	li $v0, 73
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_2
	j JC_orelse_3
JC_orelse_3:
#   CondBr {rel = EQI, op0 = R "C", op1 = C (CC 105), trueDest = Label {lblstr = "YesGuard_2", lblnum = 26}, falseDest = Label {lblstr = "JC_orelse_2", lblnum = 29}}
	li $v0, 105
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_2
	j JC_orelse_2
JC_orelse_2:
#   CondBr {rel = EQI, op0 = R "C", op1 = C (CC 69), trueDest = Label {lblstr = "YesGuard_2", lblnum = 26}, falseDest = Label {lblstr = "JC_orelse_1", lblnum = 28}}
	li $v0, 69
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_2
	j JC_orelse_1
JC_orelse_1:
#   CondBr {rel = EQI, op0 = R "C", op1 = C (CC 101), trueDest = Label {lblstr = "YesGuard_2", lblnum = 26}, falseDest = Label {lblstr = "NextGuard_1", lblnum = 27}}
	li $v0, 101
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_2
	j NextGuard_1
YesGuard_2:
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
#   N_3 := call "_readInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   param N_3
	sw $a1, -16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "sortI"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_sortI
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "IfExit", lblnum = 22}}
	j IfExit
NextGuard_1:
#   CondBr {rel = EQI, op0 = R "C", op1 = C (CC 70), trueDest = Label {lblstr = "YesGuard_3", lblnum = 31}, falseDest = Label {lblstr = "JC_orelse_4", lblnum = 33}}
	li $v0, 70
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_3
	j JC_orelse_4
JC_orelse_4:
#   CondBr {rel = EQI, op0 = R "C", op1 = C (CC 102), trueDest = Label {lblstr = "YesGuard_3", lblnum = 31}, falseDest = Label {lblstr = "NextGuard_2", lblnum = 32}}
	li $v0, 102
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_3
	j NextGuard_2
YesGuard_3:
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
#   N_3 := call "_readInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   param N_3
	sw $a1, -16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "sortF"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_sortF
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "IfExit", lblnum = 22}}
	j IfExit
NextGuard_2:
#   CondBr {rel = EQI, op0 = R "C", op1 = C (CC 67), trueDest = Label {lblstr = "YesGuard_4", lblnum = 34}, falseDest = Label {lblstr = "JC_orelse_5", lblnum = 35}}
	li $v0, 67
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_4
	j JC_orelse_5
JC_orelse_5:
#   CondBr {rel = EQI, op0 = R "C", op1 = C (CC 99), trueDest = Label {lblstr = "YesGuard_4", lblnum = 34}, falseDest = Label {lblstr = "IfExit", lblnum = 22}}
	li $v0, 99
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_4
	j IfExit
YesGuard_4:
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
#   N_3 := call "_readInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   param N_3
	sw $a1, -16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "sortC"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_sortC
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "IfExit", lblnum = 22}}
	j IfExit
IfExit:
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 19}}
	j WhileHeader
WhileExit:
#   Br {dest = Label {lblstr = "Return_3", lblnum = 17}}
	j Return_3
Return_3:
#   ; Epilog for procedure main
# Epilog for procedure main
#   epilog 16
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# partitionC
proc_partitionC:
#   ; Procedure at (161,1)
# Procedure at (161,1)
#   prolog 12
	# Prolog 76
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 76

#   var L 12 4
#   var H 16 4
#   var Pivot -4 4
#   _t9 := muli H #i4
	lw $a1, 16($fp)
	mul $a2, $a1, 4
#   _t10 := Cs[_t9]
	lw $a1, Cs($a2)
#   Pivot := _t10
#   var I_3 -8 4
#   I_3 := L
	lw $a3, 12($fp)
#   var J -12 4
#   _t11 := subi H #i1
	lw $t0, 16($fp)
	sub $t1, $t0, 1
#   J := L
	sw $a1, -4($fp)
	sw $a1, -16($fp)
	sw $a2, -20($fp)
	sw $a3, -12($fp)
	sw $a3, -8($fp)
	sw $a3, 12($fp)
	sw $t1, -24($fp)
#   Br {dest = Label {lblstr = "ForHeader_3", lblnum = 38}}
	j ForHeader_3
ForHeader_3:
#   CondBr {rel = LEI, op0 = R "J", op1 = T 11, trueDest = Label {lblstr = "ForBody_3", lblnum = 39}, falseDest = Label {lblstr = "ForExit_3", lblnum = 40}}
	lw $a1, -24($fp)
	lw $a2, -12($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_3
	j ForExit_3
ForBody_3:
#   _t12 := muli J #i4
	lw $a1, -12($fp)
	mul $a2, $a1, 4
#   _t13 := Cs[_t12]
	lw $a1, Cs($a2)
	sw $a1, -28($fp)
	sw $a2, -32($fp)
#   CondBr {rel = LEI, op0 = T 13, op1 = R "Pivot", trueDest = Label {lblstr = "YesGuard_5", lblnum = 42}, falseDest = Label {lblstr = "IfExit_1", lblnum = 41}}
	lw $a3, -4($fp)
	slt $v0, $a3, $a1
	beq $v0, $0, YesGuard_5
	j IfExit_1
YesGuard_5:
#   _t14 := muli I_3 #i4
	lw $a1, -8($fp)
	mul $a2, $a1, 4
#   _t15 := &Cs + _t14
	la $v0, Cs
	add $a1, $v0, $a2
#   _t16 := muli J #i4
	lw $a3, -12($fp)
	mul $t0, $a3, 4
#   _t17 := &Cs + _t16
	la $v0, Cs
	add $a3, $v0, $t0
#   param _t17
	sw $a1, -36($fp)
	sw $a2, -40($fp)
	sw $a3, -44($fp)
	sw $t0, -48($fp)
	add $sp, $sp, -4
	sw $a3, 0($sp)
#   param _t15
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "swapC"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_swapC
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   _t18 := addi I_3 #i1
	lw $a1, -8($fp)
	add $a2, $a1, 1
#   I_3 := _t18
	sw $a2, -8($fp)
	sw $a2, -52($fp)
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 41}}
	j IfExit_1
IfExit_1:
#   J := addi J #i1
	lw $a1, -12($fp)
	add $a1, $a1, 1
	sw $a1, -12($fp)
#   Br {dest = Label {lblstr = "ForHeader_3", lblnum = 38}}
	j ForHeader_3
ForExit_3:
#   _t19 := muli I_3 #i4
	lw $a1, -8($fp)
	mul $a2, $a1, 4
#   _t20 := &Cs + _t19
	la $v0, Cs
	add $a1, $v0, $a2
#   _t21 := muli H #i4
	lw $a3, 16($fp)
	mul $t0, $a3, 4
#   _t22 := &Cs + _t21
	la $v0, Cs
	add $a3, $v0, $t0
#   param _t22
	sw $a1, -56($fp)
	sw $a2, -60($fp)
	sw $a3, -64($fp)
	sw $t0, -68($fp)
	add $sp, $sp, -4
	sw $a3, 0($sp)
#   param _t20
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "swapC"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_swapC
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   answer I_3
	lw $a1, -8($fp)
	sw $a1, 8($fp)
#   Br {dest = Label {lblstr = "Return_4", lblnum = 36}}
	j Return_4
Return_4:
#   ; Epilog for procedure partitionC
# Epilog for procedure partitionC
#   epilog 12
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# partitionF
proc_partitionF:
#   ; Procedure at (149,1)
# Procedure at (149,1)
#   prolog 12
	# Prolog 76
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 76

#   var L_1 12 4
#   var H_1 16 4
#   fvar Pivot_1 -4 4
#   _t23 := muli H_1 #i4
	lw $a1, 16($fp)
	mul $a2, $a1, 4
#   _tf24 := Fs[_t23]
	l.s $f1, Fs($a2)
#   f.Pivot_1 := _tf24
#   var I_4 -8 4
#   I_4 := L_1
	lw $a1, 12($fp)
#   var J_1 -12 4
#   _t25 := subi H_1 #i1
	lw $a3, 16($fp)
	sub $t0, $a3, 1
#   J_1 := L_1
	sw $a1, -12($fp)
	sw $a1, -8($fp)
	sw $a1, 12($fp)
	sw $a2, -16($fp)
	sw $t0, -20($fp)
	s.s $f1, -4($fp)
	s.s $f1, -24($fp)
#   Br {dest = Label {lblstr = "ForHeader_4", lblnum = 45}}
	j ForHeader_4
ForHeader_4:
#   CondBr {rel = LEI, op0 = R "J_1", op1 = T 25, trueDest = Label {lblstr = "ForBody_4", lblnum = 46}, falseDest = Label {lblstr = "ForExit_4", lblnum = 47}}
	lw $a1, -20($fp)
	lw $a2, -12($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_4
	j ForExit_4
ForBody_4:
#   _t26 := muli J_1 #i4
	lw $a1, -12($fp)
	mul $a2, $a1, 4
#   _tf27 := Fs[_t26]
	l.s $f1, Fs($a2)
	sw $a2, -28($fp)
	s.s $f1, -32($fp)
#   CondBr {rel = LEF, op0 = TF 27, op1 = RF "Pivot_1", trueDest = Label {lblstr = "YesGuard_6", lblnum = 49}, falseDest = Label {lblstr = "IfExit_2", lblnum = 48}}
	l.s $f2, -4($fp)
	c.le.s $f1, $f2
	bc1t YesGuard_6
	j IfExit_2
YesGuard_6:
#   _t28 := muli I_4 #i4
	lw $a1, -8($fp)
	mul $a2, $a1, 4
#   _t29 := &Fs + _t28
	la $v0, Fs
	add $a1, $v0, $a2
#   _t30 := muli J_1 #i4
	lw $a3, -12($fp)
	mul $t0, $a3, 4
#   _t31 := &Fs + _t30
	la $v0, Fs
	add $a3, $v0, $t0
#   param _t31
	sw $a1, -36($fp)
	sw $a2, -40($fp)
	sw $a3, -44($fp)
	sw $t0, -48($fp)
	add $sp, $sp, -4
	sw $a3, 0($sp)
#   param _t29
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "swapF"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_swapF
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   _t32 := addi I_4 #i1
	lw $a1, -8($fp)
	add $a2, $a1, 1
#   I_4 := _t32
	sw $a2, -8($fp)
	sw $a2, -52($fp)
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 48}}
	j IfExit_2
IfExit_2:
#   J_1 := addi J_1 #i1
	lw $a1, -12($fp)
	add $a1, $a1, 1
	sw $a1, -12($fp)
#   Br {dest = Label {lblstr = "ForHeader_4", lblnum = 45}}
	j ForHeader_4
ForExit_4:
#   _t33 := muli I_4 #i4
	lw $a1, -8($fp)
	mul $a2, $a1, 4
#   _t34 := &Fs + _t33
	la $v0, Fs
	add $a1, $v0, $a2
#   _t35 := muli H_1 #i4
	lw $a3, 16($fp)
	mul $t0, $a3, 4
#   _t36 := &Fs + _t35
	la $v0, Fs
	add $a3, $v0, $t0
#   param _t36
	sw $a1, -56($fp)
	sw $a2, -60($fp)
	sw $a3, -64($fp)
	sw $t0, -68($fp)
	add $sp, $sp, -4
	sw $a3, 0($sp)
#   param _t34
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "swapF"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_swapF
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   answer I_4
	lw $a1, -8($fp)
	sw $a1, 8($fp)
#   Br {dest = Label {lblstr = "Return_5", lblnum = 43}}
	j Return_5
Return_5:
#   ; Epilog for procedure partitionF
# Epilog for procedure partitionF
#   epilog 12
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# partitionI
proc_partitionI:
#   ; Procedure at (137,1)
# Procedure at (137,1)
#   prolog 12
	# Prolog 76
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 76

#   var L_2 12 4
#   var H_2 16 4
#   var Pivot_2 -4 4
#   _t37 := muli H_2 #i4
	lw $a1, 16($fp)
	mul $a2, $a1, 4
#   _t38 := Is[_t37]
	lw $a1, Is($a2)
#   Pivot_2 := _t38
#   var I_5 -8 4
#   I_5 := L_2
	lw $a3, 12($fp)
#   var J_2 -12 4
#   _t39 := subi H_2 #i1
	lw $t0, 16($fp)
	sub $t1, $t0, 1
#   J_2 := L_2
	sw $a1, -4($fp)
	sw $a1, -16($fp)
	sw $a2, -20($fp)
	sw $a3, -12($fp)
	sw $a3, -8($fp)
	sw $a3, 12($fp)
	sw $t1, -24($fp)
#   Br {dest = Label {lblstr = "ForHeader_5", lblnum = 52}}
	j ForHeader_5
ForHeader_5:
#   CondBr {rel = LEI, op0 = R "J_2", op1 = T 39, trueDest = Label {lblstr = "ForBody_5", lblnum = 53}, falseDest = Label {lblstr = "ForExit_5", lblnum = 54}}
	lw $a1, -24($fp)
	lw $a2, -12($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_5
	j ForExit_5
ForBody_5:
#   _t40 := muli J_2 #i4
	lw $a1, -12($fp)
	mul $a2, $a1, 4
#   _t41 := Is[_t40]
	lw $a1, Is($a2)
	sw $a1, -28($fp)
	sw $a2, -32($fp)
#   CondBr {rel = LEI, op0 = T 41, op1 = R "Pivot_2", trueDest = Label {lblstr = "YesGuard_7", lblnum = 56}, falseDest = Label {lblstr = "IfExit_3", lblnum = 55}}
	lw $a3, -4($fp)
	slt $v0, $a3, $a1
	beq $v0, $0, YesGuard_7
	j IfExit_3
YesGuard_7:
#   _t42 := muli I_5 #i4
	lw $a1, -8($fp)
	mul $a2, $a1, 4
#   _t43 := &Is + _t42
	la $v0, Is
	add $a1, $v0, $a2
#   _t44 := muli J_2 #i4
	lw $a3, -12($fp)
	mul $t0, $a3, 4
#   _t45 := &Is + _t44
	la $v0, Is
	add $a3, $v0, $t0
#   param _t45
	sw $a1, -36($fp)
	sw $a2, -40($fp)
	sw $a3, -44($fp)
	sw $t0, -48($fp)
	add $sp, $sp, -4
	sw $a3, 0($sp)
#   param _t43
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "swapI"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_swapI
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   _t46 := addi I_5 #i1
	lw $a1, -8($fp)
	add $a2, $a1, 1
#   I_5 := _t46
	sw $a2, -8($fp)
	sw $a2, -52($fp)
#   Br {dest = Label {lblstr = "IfExit_3", lblnum = 55}}
	j IfExit_3
IfExit_3:
#   J_2 := addi J_2 #i1
	lw $a1, -12($fp)
	add $a1, $a1, 1
	sw $a1, -12($fp)
#   Br {dest = Label {lblstr = "ForHeader_5", lblnum = 52}}
	j ForHeader_5
ForExit_5:
#   _t47 := muli I_5 #i4
	lw $a1, -8($fp)
	mul $a2, $a1, 4
#   _t48 := &Is + _t47
	la $v0, Is
	add $a1, $v0, $a2
#   _t49 := muli H_2 #i4
	lw $a3, 16($fp)
	mul $t0, $a3, 4
#   _t50 := &Is + _t49
	la $v0, Is
	add $a3, $v0, $t0
#   param _t50
	sw $a1, -56($fp)
	sw $a2, -60($fp)
	sw $a3, -64($fp)
	sw $t0, -68($fp)
	add $sp, $sp, -4
	sw $a3, 0($sp)
#   param _t48
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "swapI"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_swapI
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   answer I_5
	lw $a1, -8($fp)
	sw $a1, 8($fp)
#   Br {dest = Label {lblstr = "Return_6", lblnum = 50}}
	j Return_6
Return_6:
#   ; Epilog for procedure partitionI
# Epilog for procedure partitionI
#   epilog 12
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# quicksortC
proc_quicksortC:
#   ; Procedure at (126,1)
# Procedure at (126,1)
#   prolog 4
	# Prolog 24
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 24

#   var L_3 12 4
#   var H_3 16 4
#   CondBr {rel = LTI, op0 = R "L_3", op1 = R "H_3", trueDest = Label {lblstr = "YesGuard_8", lblnum = 60}, falseDest = Label {lblstr = "IfExit_4", lblnum = 59}}
	lw $a1, 16($fp)
	lw $a2, 12($fp)
	slt $v0, $a2, $a1
	bne $v0, $0, YesGuard_8
	j IfExit_4
YesGuard_8:
#   var P -4 4
#   param H_3
	lw $a1, 16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param L_3
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   _t51 := call "partitionC"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_partitionC
	lw $a1, 8($fp)
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   P := _t51
#   _t52 := subi P #i1
	sub $a2, $a1, 1
#   param _t52
	sw $a1, -4($fp)
	sw $a1, -8($fp)
	sw $a2, -12($fp)
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   param L_3
	lw $a3, 12($fp)
	add $sp, $sp, -4
	sw $a3, 0($sp)
#   call "quicksortC"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_quicksortC
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   _t53 := addi P #i1
	lw $a1, -4($fp)
	add $a2, $a1, 1
#   param H_3
	sw $a2, -16($fp)
	lw $a1, 16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param _t53
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   call "quicksortC"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_quicksortC
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   Br {dest = Label {lblstr = "IfExit_4", lblnum = 59}}
	j IfExit_4
IfExit_4:
#   Br {dest = Label {lblstr = "Return_7", lblnum = 57}}
	j Return_7
Return_7:
#   ; Epilog for procedure quicksortC
# Epilog for procedure quicksortC
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# quicksortF
proc_quicksortF:
#   ; Procedure at (120,1)
# Procedure at (120,1)
#   prolog 4
	# Prolog 24
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 24

#   var L_4 12 4
#   var H_4 16 4
#   CondBr {rel = LTI, op0 = R "L_4", op1 = R "H_4", trueDest = Label {lblstr = "YesGuard_9", lblnum = 64}, falseDest = Label {lblstr = "IfExit_5", lblnum = 63}}
	lw $a1, 16($fp)
	lw $a2, 12($fp)
	slt $v0, $a2, $a1
	bne $v0, $0, YesGuard_9
	j IfExit_5
YesGuard_9:
#   var P_1 -4 4
#   param H_4
	lw $a1, 16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param L_4
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   _t54 := call "partitionF"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_partitionF
	lw $a1, 8($fp)
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   P_1 := _t54
#   _t55 := subi P_1 #i1
	sub $a2, $a1, 1
#   param _t55
	sw $a1, -4($fp)
	sw $a1, -8($fp)
	sw $a2, -12($fp)
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   param L_4
	lw $a3, 12($fp)
	add $sp, $sp, -4
	sw $a3, 0($sp)
#   call "quicksortF"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_quicksortF
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   _t56 := addi P_1 #i1
	lw $a1, -4($fp)
	add $a2, $a1, 1
#   param H_4
	sw $a2, -16($fp)
	lw $a1, 16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param _t56
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   call "quicksortF"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_quicksortF
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   Br {dest = Label {lblstr = "IfExit_5", lblnum = 63}}
	j IfExit_5
IfExit_5:
#   Br {dest = Label {lblstr = "Return_8", lblnum = 61}}
	j Return_8
Return_8:
#   ; Epilog for procedure quicksortF
# Epilog for procedure quicksortF
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# quicksortI
proc_quicksortI:
#   ; Procedure at (114,1)
# Procedure at (114,1)
#   prolog 4
	# Prolog 24
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 24

#   var L_5 12 4
#   var H_5 16 4
#   CondBr {rel = LTI, op0 = R "L_5", op1 = R "H_5", trueDest = Label {lblstr = "YesGuard_10", lblnum = 68}, falseDest = Label {lblstr = "IfExit_6", lblnum = 67}}
	lw $a1, 16($fp)
	lw $a2, 12($fp)
	slt $v0, $a2, $a1
	bne $v0, $0, YesGuard_10
	j IfExit_6
YesGuard_10:
#   var P_2 -4 4
#   param H_5
	lw $a1, 16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param L_5
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   _t57 := call "partitionI"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_partitionI
	lw $a1, 8($fp)
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   P_2 := _t57
#   _t58 := subi P_2 #i1
	sub $a2, $a1, 1
#   param _t58
	sw $a1, -4($fp)
	sw $a1, -8($fp)
	sw $a2, -12($fp)
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   param L_5
	lw $a3, 12($fp)
	add $sp, $sp, -4
	sw $a3, 0($sp)
#   call "quicksortI"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_quicksortI
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   _t59 := addi P_2 #i1
	lw $a1, -4($fp)
	add $a2, $a1, 1
#   param H_5
	sw $a2, -16($fp)
	lw $a1, 16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param _t59
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   call "quicksortI"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_quicksortI
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   Br {dest = Label {lblstr = "IfExit_6", lblnum = 67}}
	j IfExit_6
IfExit_6:
#   Br {dest = Label {lblstr = "Return_9", lblnum = 65}}
	j Return_9
Return_9:
#   ; Epilog for procedure quicksortI
# Epilog for procedure quicksortI
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# sortC
proc_sortC:
#   ; Procedure at (99,1)
# Procedure at (99,1)
#   prolog 4
	# Prolog 24
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 24

#   var N_4 12 4
#   var I_6 -4 4
#   _t60 := subi N_4 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   I_6 := #i0
	li $a1, 0
	sw $a1, -4($fp)
	sw $a2, -8($fp)
#   Br {dest = Label {lblstr = "ForHeader_6", lblnum = 71}}
	j ForHeader_6
ForHeader_6:
#   CondBr {rel = LEI, op0 = R "I_6", op1 = T 60, trueDest = Label {lblstr = "ForBody_6", lblnum = 72}, falseDest = Label {lblstr = "ForExit_6", lblnum = 73}}
	lw $a1, -8($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_6
	j ForExit_6
ForBody_6:
#   param &_str18
	add $sp, $sp, -4
	la $v0, _str18
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param I_6
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
#   _t61 := muli I_6 #i4
	lw $a1, -4($fp)
	mul $a2, $a1, 4
#   _t62 := call "_readChar"
	sw $a2, -12($fp)
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readChar
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   Cs[_t61] := _t62
	lw $a2, -12($fp)
	sw $a1, Cs($a2)
#   param &_str19
	sw $a1, -16($fp)
	add $sp, $sp, -4
	la $v0, _str19
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   I_6 := addi I_6 #i1
	lw $a1, -4($fp)
	add $a1, $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "ForHeader_6", lblnum = 71}}
	j ForHeader_6
ForExit_6:
#   _t63 := subi N_4 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   param _t63
	sw $a2, -20($fp)
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   param #i0
	li $v0, 0
	add $sp, $sp, -4
	sw $v0, 0($sp)
#   call "quicksortC"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_quicksortC
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   param &_str20
	add $sp, $sp, -4
	la $v0, _str20
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param N_4
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "displayC"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_displayC
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "Return_10", lblnum = 69}}
	j Return_10
Return_10:
#   ; Epilog for procedure sortC
# Epilog for procedure sortC
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# sortF
proc_sortF:
#   ; Procedure at (89,1)
# Procedure at (89,1)
#   prolog 4
	# Prolog 24
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 24

#   var N_5 12 4
#   var I_7 -4 4
#   _t64 := subi N_5 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   I_7 := #i0
	li $a1, 0
	sw $a1, -4($fp)
	sw $a2, -8($fp)
#   Br {dest = Label {lblstr = "ForHeader_7", lblnum = 76}}
	j ForHeader_7
ForHeader_7:
#   CondBr {rel = LEI, op0 = R "I_7", op1 = T 64, trueDest = Label {lblstr = "ForBody_7", lblnum = 77}, falseDest = Label {lblstr = "ForExit_7", lblnum = 78}}
	lw $a1, -8($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_7
	j ForExit_7
ForBody_7:
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
#   param I_7
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
#   _t65 := muli I_7 #i4
	lw $a1, -4($fp)
	mul $a2, $a1, 4
#   _tf66 := call "_readFloat"
	sw $a2, -12($fp)
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readFloat
	l.s $f1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   Fs[_t65] := _tf66
	lw $a1, -12($fp)
	s.s $f1, Fs($a1)
#   I_7 := addi I_7 #i1
	lw $a1, -4($fp)
	add $a1, $a1, 1
	sw $a1, -4($fp)
	s.s $f1, -16($fp)
#   Br {dest = Label {lblstr = "ForHeader_7", lblnum = 76}}
	j ForHeader_7
ForExit_7:
#   _t67 := subi N_5 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   param _t67
	sw $a2, -20($fp)
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   param #i0
	li $v0, 0
	add $sp, $sp, -4
	sw $v0, 0($sp)
#   call "quicksortF"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_quicksortF
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   param &_str17
	add $sp, $sp, -4
	la $v0, _str17
	sw $v0, 0($sp)
#   call "_writeStr"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeStr
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param N_5
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "displayF"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_displayF
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "Return_11", lblnum = 74}}
	j Return_11
Return_11:
#   ; Epilog for procedure sortF
# Epilog for procedure sortF
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# sortI
proc_sortI:
#   ; Procedure at (79,1)
# Procedure at (79,1)
#   prolog 4
	# Prolog 24
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 24

#   var N_6 12 4
#   var I_8 -4 4
#   _t68 := subi N_6 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   I_8 := #i0
	li $a1, 0
	sw $a1, -4($fp)
	sw $a2, -8($fp)
#   Br {dest = Label {lblstr = "ForHeader_8", lblnum = 81}}
	j ForHeader_8
ForHeader_8:
#   CondBr {rel = LEI, op0 = R "I_8", op1 = T 68, trueDest = Label {lblstr = "ForBody_8", lblnum = 82}, falseDest = Label {lblstr = "ForExit_8", lblnum = 83}}
	lw $a1, -8($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_8
	j ForExit_8
ForBody_8:
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
#   param I_8
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
#   _t69 := muli I_8 #i4
	lw $a1, -4($fp)
	mul $a2, $a1, 4
#   _t70 := call "_readInteger"
	sw $a2, -12($fp)
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   Is[_t69] := _t70
	lw $a2, -12($fp)
	sw $a1, Is($a2)
#   I_8 := addi I_8 #i1
	lw $a2, -4($fp)
	add $a2, $a2, 1
	sw $a1, -16($fp)
	sw $a2, -4($fp)
#   Br {dest = Label {lblstr = "ForHeader_8", lblnum = 81}}
	j ForHeader_8
ForExit_8:
#   _t71 := subi N_6 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   param _t71
	sw $a2, -20($fp)
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   param #i0
	li $v0, 0
	add $sp, $sp, -4
	sw $v0, 0($sp)
#   call "quicksortI"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_quicksortI
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
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
#   param N_6
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "displayI"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_displayI
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "Return_12", lblnum = 79}}
	j Return_12
Return_12:
#   ; Epilog for procedure sortI
# Epilog for procedure sortI
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# swapC
proc_swapC:
#   ; Procedure at (195,1)
# Procedure at (195,1)
#   prolog 4
	# Prolog 20
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 20

#   ref A 12 4
#   ref B 16 4
#   var T -4 4
#   _t72 := *A
	lw $a1, 12($fp)
	lw $a2, 0($a1)
#   T := _t72
#   _t73 := *B
	lw $a1, 16($fp)
	lw $a3, 0($a1)
#   *A := _t73
	lw $a1, 12($fp)
	sw $a2, -4($fp)
	sw $a2, -8($fp)
	sw $a3, -12($fp)
	sw $a3, 0($a1)
#   *B := T
	lw $a1, -4($fp)
	lw $a2, 16($fp)
	sw $a1, 0($a2)
#   Br {dest = Label {lblstr = "Return_13", lblnum = 84}}
	j Return_13
Return_13:
#   ; Epilog for procedure swapC
# Epilog for procedure swapC
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# swapF
proc_swapF:
#   ; Procedure at (193,1)
# Procedure at (193,1)
#   prolog 4
	# Prolog 20
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 20

#   ref A_1 12 4
#   ref B_1 16 4
#   fvar T_1 -4 4
#   _tf74 := *A_1
	lw $a1, 12($fp)
	l.s $f1, 0($a1)
#   f.T_1 := _tf74
#   _tf75 := *B_1
	lw $a1, 16($fp)
	l.s $f2, 0($a1)
#   *A_1 := _tf75
	lw $a1, 12($fp)
	s.s $f1, -4($fp)
	s.s $f1, -8($fp)
	s.s $f2, -12($fp)
	s.s $f2, 0($a1)
#   *B_1 := f.T_1
	l.s $f1, -4($fp)
	lw $a1, 16($fp)
	s.s $f1, 0($a1)
#   Br {dest = Label {lblstr = "Return_14", lblnum = 86}}
	j Return_14
Return_14:
#   ; Epilog for procedure swapF
# Epilog for procedure swapF
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# swapI
proc_swapI:
#   ; Procedure at (191,1)
# Procedure at (191,1)
#   prolog 4
	# Prolog 20
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 20

#   ref A_2 12 4
#   ref B_2 16 4
#   var T_2 -4 4
#   _t76 := *A_2
	lw $a1, 12($fp)
	lw $a2, 0($a1)
#   T_2 := _t76
#   _t77 := *B_2
	lw $a1, 16($fp)
	lw $a3, 0($a1)
#   *A_2 := _t77
	lw $a1, 12($fp)
	sw $a2, -4($fp)
	sw $a2, -8($fp)
	sw $a3, -12($fp)
	sw $a3, 0($a1)
#   *B_2 := T_2
	lw $a1, -4($fp)
	lw $a2, 16($fp)
	sw $a1, 0($a2)
#   Br {dest = Label {lblstr = "Return_15", lblnum = 88}}
	j Return_15
Return_15:
#   ; Epilog for procedure swapI
# Epilog for procedure swapI
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
