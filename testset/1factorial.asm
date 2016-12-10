	.data
	.align 2
_base_header: .space 8
	.align 2
_last_used: .space 4
_true: .asciiz "true\n"
_false: .asciiz "false\n"
_str2: .asciiz "\tel numero 0 para salir,\n"
_str3: .asciiz "\tel numero 1 para utilizar el metodo recursivo, o\n"
_str4: .asciiz "\tel numero 2 para utilizar el metodo iterativo.\n"
_str10: .asciiz "\n"
_str12: .asciiz "< "
_str5: .asciiz "> "
_str6: .asciiz "Adios.\n"
_str0: .asciiz "Factorial Iterativo/Recursivo.\n"
_str11: .asciiz "Ha ingresado un numero negativo.\n"
_str1: .asciiz "Introduzca\n"
_str7: .asciiz "Introduzca x, (entero, no negativo) para calcular x! con el metodo "
_str9: .asciiz "iterativo\n"
_str8: .asciiz "recursivo\n"
_str13: .asciiz "wut?"
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
# itfact
proc_itfact:
#   ; Procedure at (52,1)
# Procedure at (52,1)
#   prolog 8
	# Prolog 16
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 16

#   var N 12 4
#   var I -4 4
#   var R -8 4
#   R := #i1
	li $a1, 1
#   I := #i1
	li $a2, 1
	sw $a1, -8($fp)
	sw $a2, -4($fp)
#   Br {dest = Label {lblstr = "ForHeader", lblnum = 4}}
	j ForHeader
ForHeader:
#   CondBr {rel = LEI, op0 = R "I", op1 = R "N", trueDest = Label {lblstr = "ForBody", lblnum = 5}, falseDest = Label {lblstr = "ForExit", lblnum = 6}}
	lw $a1, 12($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody
	j ForExit
ForBody:
#   _t0 := muli R I
	lw $a1, -8($fp)
	lw $a2, -4($fp)
	mul $a3, $a1, $a2
#   R := _t0
#   I := addi I #i1
	add $a2, $a2, 1
	sw $a2, -4($fp)
	sw $a3, -8($fp)
	sw $a3, -12($fp)
#   Br {dest = Label {lblstr = "ForHeader", lblnum = 4}}
	j ForHeader
ForExit:
#   answer R
	lw $a1, -8($fp)
	sw $a1, 8($fp)
#   Br {dest = Label {lblstr = "Return", lblnum = 2}}
	j Return
Return:
#   ; Epilog for procedure itfact
# Epilog for procedure itfact
#   epilog 8
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# main
proc_main:
#   ; Procedure at (15,1)
# Procedure at (15,1)
#   prolog 12
	# Prolog 20
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 20

#   var Done -4 4
#   Done := #False
	li $a1, 0
#   var Metodo -8 4
#   var X -12 4
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
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 9}}
	j WhileHeader
WhileHeader:
#   IfBr {cond = R "Done", trueDest = Label {lblstr = "WhileExit", lblnum = 10}, falseDest = Label {lblstr = "YesGuard", lblnum = 11}}
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
#   Metodo := call "_readInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
	sw $a1, -8($fp)
#   CondBr {rel = EQI, op0 = C (IC 0), op1 = R "Metodo", trueDest = Label {lblstr = "YesGuard_1", lblnum = 13}, falseDest = Label {lblstr = "NextGuard", lblnum = 14}}
	li $v0, 0
	beq $v0, $a1, YesGuard_1
	j NextGuard
YesGuard_1:
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
#   Done := #True
	li $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "IfExit", lblnum = 12}}
	j IfExit
NextGuard:
#   CondBr {rel = EQI, op0 = R "Metodo", op1 = C (IC 1), trueDest = Label {lblstr = "YesGuard_2", lblnum = 15}, falseDest = Label {lblstr = "JC_orelse", lblnum = 16}}
	li $v0, 1
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_2
	j JC_orelse
JC_orelse:
#   CondBr {rel = EQI, op0 = R "Metodo", op1 = C (IC 2), trueDest = Label {lblstr = "YesGuard_2", lblnum = 15}, falseDest = Label {lblstr = "IfExit", lblnum = 12}}
	li $v0, 2
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_2
	j IfExit
YesGuard_2:
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
#   CondBr {rel = EQI, op0 = R "Metodo", op1 = C (IC 1), trueDest = Label {lblstr = "YesGuard_3", lblnum = 18}, falseDest = Label {lblstr = "NextGuard_1", lblnum = 19}}
	li $v0, 1
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_3
	j NextGuard_1
YesGuard_3:
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
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 17}}
	j IfExit_1
NextGuard_1:
#   CondBr {rel = EQI, op0 = R "Metodo", op1 = C (IC 2), trueDest = Label {lblstr = "YesGuard_4", lblnum = 20}, falseDest = Label {lblstr = "IfExit_1", lblnum = 17}}
	li $v0, 2
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_4
	j IfExit_1
YesGuard_4:
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
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 17}}
	j IfExit_1
IfExit_1:
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
#   X := call "_readInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   param &_str10
	sw $a1, -12($fp)
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
#   CondBr {rel = LTI, op0 = R "X", op1 = C (IC 0), trueDest = Label {lblstr = "YesGuard_5", lblnum = 22}, falseDest = Label {lblstr = "NextGuard_2", lblnum = 23}}
	li $v0, 0
	lw $a1, -12($fp)
	slt $v0, $a1, $v0
	bne $v0, $0, YesGuard_5
	j NextGuard_2
YesGuard_5:
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
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 21}}
	j IfExit_2
NextGuard_2:
#   CondBr {rel = EQI, op0 = R "Metodo", op1 = C (IC 1), trueDest = Label {lblstr = "YesGuard_6", lblnum = 24}, falseDest = Label {lblstr = "NextGuard_3", lblnum = 25}}
	li $v0, 1
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_6
	j NextGuard_3
YesGuard_6:
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
#   param X
	lw $a1, -12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   _t1 := call "recfact"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_recfact
	lw $a1, 8($fp)
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   X := _t1
#   param X
	sw $a1, -12($fp)
	sw $a1, -16($fp)
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
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 21}}
	j IfExit_2
NextGuard_3:
#   CondBr {rel = EQI, op0 = R "Metodo", op1 = C (IC 2), trueDest = Label {lblstr = "YesGuard_7", lblnum = 26}, falseDest = Label {lblstr = "NextGuard_4", lblnum = 27}}
	li $v0, 2
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_7
	j NextGuard_4
YesGuard_7:
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
#   param X
	lw $a1, -12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   _t2 := call "itfact"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_itfact
	lw $a1, 8($fp)
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   X := _t2
#   param X
	sw $a1, -12($fp)
	sw $a1, -20($fp)
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
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 21}}
	j IfExit_2
NextGuard_4:
#   Br {dest = Label {lblstr = "YesGuard_8", lblnum = 28}}
	j YesGuard_8
YesGuard_8:
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
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 21}}
	j IfExit_2
IfExit_2:
#   Br {dest = Label {lblstr = "IfExit", lblnum = 12}}
	j IfExit
IfExit:
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 9}}
	j WhileHeader
WhileExit:
#   Br {dest = Label {lblstr = "Return_1", lblnum = 7}}
	j Return_1
Return_1:
#   ; Epilog for procedure main
# Epilog for procedure main
#   epilog 12
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# recfact
proc_recfact:
#   ; Procedure at (60,1)
# Procedure at (60,1)
#   prolog 0
	# Prolog 16
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 16

#   var N_1 12 4
#   CondBr {rel = EQI, op0 = R "N_1", op1 = C (IC 0), trueDest = Label {lblstr = "YesGuard_9", lblnum = 32}, falseDest = Label {lblstr = "NextGuard_5", lblnum = 33}}
	li $v0, 0
	lw $a1, 12($fp)
	beq $a1, $v0, YesGuard_9
	j NextGuard_5
YesGuard_9:
#   answer #i1
	li $v0, 1
	sw $v0, 8($fp)
#   Br {dest = Label {lblstr = "Return_2", lblnum = 29}}
	j Return_2
NextGuard_5:
#   CondBr {rel = NEI, op0 = R "N_1", op1 = C (IC 0), trueDest = Label {lblstr = "YesGuard_10", lblnum = 34}, falseDest = Label {lblstr = "IfExit_3", lblnum = 31}}
	li $v0, 0
	lw $a1, 12($fp)
	bne $a1, $v0, YesGuard_10
	j IfExit_3
YesGuard_10:
#   _t3 := subi N_1 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   param _t3
	sw $a2, -4($fp)
	add $sp, $sp, -4
	sw $a2, 0($sp)
#   _t4 := call "recfact"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_recfact
	lw $a1, 8($fp)
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   _t5 := muli N_1 _t4
	lw $a2, 12($fp)
	mul $a3, $a2, $a1
#   answer _t5
	sw $a3, 8($fp)
	sw $a1, -8($fp)
	sw $a3, -12($fp)
#   Br {dest = Label {lblstr = "Return_2", lblnum = 29}}
	j Return_2
IfExit_3:
#   answer #i0
	li $v0, 0
	sw $v0, 8($fp)
#   Br {dest = Label {lblstr = "Return_2", lblnum = 29}}
	j Return_2
Return_2:
#   ; Epilog for procedure recfact
# Epilog for procedure recfact
#   epilog 0
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
