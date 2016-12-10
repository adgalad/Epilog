	.data
	.align 2
_base_header: .space 8
	.align 2
_last_used: .space 4
_true: .asciiz "true\n"
_false: .asciiz "false\n"
_str2: .asciiz "\tel numero cero (0) para salir,\n"
_str4: .asciiz "\tel numero dos (2) para determinar paridad, u\n"
_str3: .asciiz "\tel numero uno (1) para determinar imparidad,\n"
_str5: .asciiz "\totro numero para mostrar este mensaje de nuevo.\n"
_str10: .asciiz " es impar.\n"
_str13: .asciiz " es par.\n"
_str11: .asciiz " no es impar.\n"
_str14: .asciiz " no es par.\n"
_str6: .asciiz "> "
_str7: .asciiz "Adios.\n"
_str9: .asciiz "El numero "
_str0: .asciiz "Estudio de paridad/imparidad.\n"
_str1: .asciiz "Introduzca\n"
_str8: .asciiz "Introduzca un numero entero para determinar su imparidad\n> "
_str12: .asciiz "Introduzca un numero entero para determinar su paridad\n> "
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
# even
proc_even:
#   ; Procedure at (47,1)
# Procedure at (47,1)
#   prolog 0
	# Prolog 12
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 12

#   var N 12 4
#   CondBr {rel = EQI, op0 = R "N", op1 = C (IC 0), trueDest = Label {lblstr = "YesGuard", lblnum = 5}, falseDest = Label {lblstr = "NextGuard", lblnum = 6}}
	li $v0, 0
	lw $a1, 12($fp)
	beq $a1, $v0, YesGuard
	j NextGuard
YesGuard:
#   answer #True
	li $v0, 1
	sw $v0, 8($fp)
#   Br {dest = Label {lblstr = "Return", lblnum = 2}}
	j Return
NextGuard:
#   CondBr {rel = EQI, op0 = R "N", op1 = C (IC 1), trueDest = Label {lblstr = "YesGuard_1", lblnum = 7}, falseDest = Label {lblstr = "NextGuard_1", lblnum = 8}}
	li $v0, 1
	lw $a1, 12($fp)
	beq $a1, $v0, YesGuard_1
	j NextGuard_1
YesGuard_1:
#   answer #False
	li $v0, 0
	sw $v0, 8($fp)
#   Br {dest = Label {lblstr = "Return", lblnum = 2}}
	j Return
NextGuard_1:
#   Br {dest = Label {lblstr = "YesGuard_2", lblnum = 9}}
	j YesGuard_2
YesGuard_2:
#   _t0 := subi N #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   param _t0
	sw $a2, -4($fp)
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   _t1 := call "odd"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_odd
	lw $a1, 8($fp)
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   answer _t1
	sw $a1, 8($fp)
	sw $a1, -8($fp)
#   Br {dest = Label {lblstr = "Return", lblnum = 2}}
	j Return
IfExit:
#   answer #False
	li $v0, 0
	sw $v0, 8($fp)
#   Br {dest = Label {lblstr = "Return", lblnum = 2}}
	j Return
Return:
#   ; Epilog for procedure even
# Epilog for procedure even
#   epilog 0
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# main
proc_main:
#   ; Procedure at (14,1)
# Procedure at (14,1)
#   prolog 12
	# Prolog 20
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 20

#   var Done -4 4
#   Done := #False
	li $a1, 0
#   var Choice -8 4
#   var N_1 -12 4
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
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 12}}
	j WhileHeader
WhileHeader:
#   IfBr {cond = R "Done", trueDest = Label {lblstr = "WhileExit", lblnum = 13}, falseDest = Label {lblstr = "YesGuard_3", lblnum = 14}}
	lw $a1, -4($fp)
	bne $a1, $0, WhileExit
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
#   Choice := call "_readInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
	sw $a1, -8($fp)
#   CondBr {rel = EQI, op0 = R "Choice", op1 = C (IC 0), trueDest = Label {lblstr = "YesGuard_4", lblnum = 16}, falseDest = Label {lblstr = "NextGuard_2", lblnum = 17}}
	li $v0, 0
	beq $a1, $v0, YesGuard_4
	j NextGuard_2
YesGuard_4:
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
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 15}}
	j IfExit_1
NextGuard_2:
#   CondBr {rel = EQI, op0 = R "Choice", op1 = C (IC 1), trueDest = Label {lblstr = "YesGuard_5", lblnum = 18}, falseDest = Label {lblstr = "NextGuard_3", lblnum = 19}}
	li $v0, 1
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_5
	j NextGuard_3
YesGuard_5:
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
#   N_1 := call "_readInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   param N_1
	sw $a1, -12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   _t2 := call "odd"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_odd
	lw $a2, 8($fp)
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
	sw $a2, -16($fp)
#   IfBr {cond = T 2, trueDest = Label {lblstr = "YesGuard_6", lblnum = 21}, falseDest = Label {lblstr = "NextGuard_4", lblnum = 22}}
	bne $a2, $0, YesGuard_6
	j NextGuard_4
YesGuard_6:
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
	lw $a1, -12($fp)
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
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 20}}
	j IfExit_2
NextGuard_4:
#   Br {dest = Label {lblstr = "YesGuard_7", lblnum = 23}}
	j YesGuard_7
YesGuard_7:
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
	lw $a1, -12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "_writeInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeInteger
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
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 20}}
	j IfExit_2
IfExit_2:
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 15}}
	j IfExit_1
NextGuard_3:
#   CondBr {rel = EQI, op0 = R "Choice", op1 = C (IC 2), trueDest = Label {lblstr = "YesGuard_8", lblnum = 24}, falseDest = Label {lblstr = "IfExit_1", lblnum = 15}}
	li $v0, 2
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_8
	j IfExit_1
YesGuard_8:
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
#   N_1 := call "_readInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   param N_1
	sw $a1, -12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   _t3 := call "even"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_even
	lw $a2, 8($fp)
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
	sw $a2, -20($fp)
#   IfBr {cond = T 3, trueDest = Label {lblstr = "YesGuard_9", lblnum = 26}, falseDest = Label {lblstr = "NextGuard_5", lblnum = 27}}
	bne $a2, $0, YesGuard_9
	j NextGuard_5
YesGuard_9:
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
	lw $a1, -12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "_writeInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeInteger
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
#   Br {dest = Label {lblstr = "IfExit_3", lblnum = 25}}
	j IfExit_3
NextGuard_5:
#   Br {dest = Label {lblstr = "YesGuard_10", lblnum = 28}}
	j YesGuard_10
YesGuard_10:
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
	lw $a1, -12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "_writeInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeInteger
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
#   Br {dest = Label {lblstr = "IfExit_3", lblnum = 25}}
	j IfExit_3
IfExit_3:
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 15}}
	j IfExit_1
IfExit_1:
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 12}}
	j WhileHeader
WhileExit:
#   Br {dest = Label {lblstr = "Return_1", lblnum = 10}}
	j Return_1
Return_1:
#   ; Epilog for procedure main
# Epilog for procedure main
#   epilog 12
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# odd
proc_odd:
#   ; Procedure at (53,1)
# Procedure at (53,1)
#   prolog 0
	# Prolog 12
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 12

#   var N_2 12 4
#   CondBr {rel = EQI, op0 = R "N_2", op1 = C (IC 0), trueDest = Label {lblstr = "YesGuard_11", lblnum = 32}, falseDest = Label {lblstr = "NextGuard_6", lblnum = 33}}
	li $v0, 0
	lw $a1, 12($fp)
	beq $a1, $v0, YesGuard_11
	j NextGuard_6
YesGuard_11:
#   answer #False
	li $v0, 0
	sw $v0, 8($fp)
#   Br {dest = Label {lblstr = "Return_2", lblnum = 29}}
	j Return_2
NextGuard_6:
#   CondBr {rel = EQI, op0 = R "N_2", op1 = C (IC 1), trueDest = Label {lblstr = "YesGuard_12", lblnum = 34}, falseDest = Label {lblstr = "NextGuard_7", lblnum = 35}}
	li $v0, 1
	lw $a1, 12($fp)
	beq $a1, $v0, YesGuard_12
	j NextGuard_7
YesGuard_12:
#   answer #True
	li $v0, 1
	sw $v0, 8($fp)
#   Br {dest = Label {lblstr = "Return_2", lblnum = 29}}
	j Return_2
NextGuard_7:
#   Br {dest = Label {lblstr = "YesGuard_13", lblnum = 36}}
	j YesGuard_13
YesGuard_13:
#   _t4 := subi N_2 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   param _t4
	sw $a2, -4($fp)
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   _t5 := call "even"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_even
	lw $a1, 8($fp)
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   answer _t5
	sw $a1, 8($fp)
	sw $a1, -8($fp)
#   Br {dest = Label {lblstr = "Return_2", lblnum = 29}}
	j Return_2
IfExit_4:
#   answer #False
	li $v0, 0
	sw $v0, 8($fp)
#   Br {dest = Label {lblstr = "Return_2", lblnum = 29}}
	j Return_2
Return_2:
#   ; Epilog for procedure odd
# Epilog for procedure odd
#   epilog 0
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
