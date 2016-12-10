	.data
	.align 2
_base_header: .space 8
	.align 2
_last_used: .space 4
_true: .asciiz "true\n"
_false: .asciiz "false\n"
_str8: .asciiz "\t(B)ooleano, (C)aracter, (I/E)ntero, (F)lotante\n"
_str2: .asciiz "\tel numero cero para salir, o\n"
_str3: .asciiz "\tun numero entero N entre uno y veinte para ordenar N peroles.\n"
_str7: .asciiz "\n"
_str15: .asciiz ") "
_str14: .asciiz ": ("
_str4: .asciiz "> "
_str5: .asciiz "Adios.\n"
_str1: .asciiz "Introduzca\n"
_str6: .asciiz "Introduzca la letra correspondiente al tipo que desea para el perol #"
_str9: .asciiz "Introduzca un Booleano > "
_str10: .asciiz "Introduzca un Caracter > "
_str11: .asciiz "Introduzca un Entero > "
_str12: .asciiz "Introduzca un Flotante > "
_str0: .asciiz "Orientacion a Peroles.\n"
_str13: .asciiz "Resultado:\n"
	.align 2
Perolero: .space 160
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
# main
proc_main:
#   ; Procedure at (46,1)
# Procedure at (46,1)
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
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 4}}
	j WhileHeader
WhileHeader:
#   IfBr {cond = R "Done", trueDest = Label {lblstr = "WhileExit", lblnum = 5}, falseDest = Label {lblstr = "YesGuard", lblnum = 6}}
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
#   N := call "_readInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
	sw $a1, -8($fp)
#   CondBr {rel = EQI, op0 = R "N", op1 = C (IC 0), trueDest = Label {lblstr = "YesGuard_1", lblnum = 8}, falseDest = Label {lblstr = "NextGuard", lblnum = 9}}
	li $v0, 0
	beq $a1, $v0, YesGuard_1
	j NextGuard
YesGuard_1:
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
#   Br {dest = Label {lblstr = "IfExit", lblnum = 7}}
	j IfExit
NextGuard:
#   CondBr {rel = LEI, op0 = C (IC 1), op1 = R "N", trueDest = Label {lblstr = "JC_andalso", lblnum = 11}, falseDest = Label {lblstr = "IfExit", lblnum = 7}}
	li $v0, 1
	lw $a1, -8($fp)
	slt $v0, $a1, $v0
	beq $v0, $0, JC_andalso
	j IfExit
JC_andalso:
#   CondBr {rel = LTI, op0 = R "N", op1 = C (IC 21), trueDest = Label {lblstr = "YesGuard_2", lblnum = 10}, falseDest = Label {lblstr = "IfExit", lblnum = 7}}
	li $v0, 21
	lw $a1, -8($fp)
	slt $v0, $a1, $v0
	bne $v0, $0, YesGuard_2
	j IfExit
YesGuard_2:
#   param N
	lw $a1, -8($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "peroles"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_peroles
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "IfExit", lblnum = 7}}
	j IfExit
IfExit:
#   Br {dest = Label {lblstr = "WhileHeader", lblnum = 4}}
	j WhileHeader
WhileExit:
#   Br {dest = Label {lblstr = "Return", lblnum = 2}}
	j Return
Return:
#   ; Epilog for procedure main
# Epilog for procedure main
#   epilog 8
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# mauritius
proc_mauritius:
#   ; Procedure at (112,1)
# Procedure at (112,1)
#   prolog 16
	# Prolog 84
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 84

#   var N_1 12 4
#   var BC -4 4
#   BC := #i0
	li $a1, 0
#   var CI -8 4
#   _t0 := subi N_1 #i1
	lw $a2, 12($fp)
	sub $a3, $a2, 1
#   CI := _t0
#   var IF -12 4
#   _t1 := subi N_1 #i1
	sub $t0, $a2, 1
#   IF := _t1
#   var J -16 4
#   J := #i0
	li $a2, 0
	sw $a1, -4($fp)
	sw $a2, -16($fp)
	sw $a3, -8($fp)
	sw $a3, -20($fp)
	sw $t0, -12($fp)
	sw $t0, -24($fp)
#   Br {dest = Label {lblstr = "WhileHeader_1", lblnum = 14}}
	j WhileHeader_1
WhileHeader_1:
#   CondBr {rel = LEI, op0 = R "J", op1 = R "CI", trueDest = Label {lblstr = "YesGuard_3", lblnum = 16}, falseDest = Label {lblstr = "WhileExit_1", lblnum = 15}}
	lw $a1, -8($fp)
	lw $a2, -16($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, YesGuard_3
	j WhileExit_1
YesGuard_3:
#   _t2 := muli J #i8
	lw $a1, -16($fp)
	mul $a2, $a1, 8
#   _t3 := Perolero[_t2]
	lw $a1, Perolero($a2)
	sw $a1, -28($fp)
	sw $a2, -32($fp)
#   CondBr {rel = EQI, op0 = T 3, op1 = C (CC 66), trueDest = Label {lblstr = "YesGuard_4", lblnum = 18}, falseDest = Label {lblstr = "NextGuard_1", lblnum = 19}}
	li $v0, 66
	beq $a1, $v0, YesGuard_4
	j NextGuard_1
YesGuard_4:
#   param BC
	lw $a1, -4($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param J
	lw $a1, -16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "swap"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_swap
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   _t4 := addi BC #i1
	lw $a1, -4($fp)
	add $a2, $a1, 1
#   BC := _t4
#   _t5 := addi J #i1
	lw $a1, -16($fp)
	add $a3, $a1, 1
#   J := _t5
	sw $a2, -4($fp)
	sw $a2, -36($fp)
	sw $a3, -16($fp)
	sw $a3, -40($fp)
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 17}}
	j IfExit_1
NextGuard_1:
#   _t6 := muli J #i8
	lw $a1, -16($fp)
	mul $a2, $a1, 8
#   _t7 := Perolero[_t6]
	lw $a1, Perolero($a2)
	sw $a1, -44($fp)
	sw $a2, -48($fp)
#   CondBr {rel = EQI, op0 = T 7, op1 = C (CC 67), trueDest = Label {lblstr = "YesGuard_5", lblnum = 20}, falseDest = Label {lblstr = "NextGuard_2", lblnum = 21}}
	li $v0, 67
	beq $a1, $v0, YesGuard_5
	j NextGuard_2
YesGuard_5:
#   _t8 := addi J #i1
	lw $a1, -16($fp)
	add $a2, $a1, 1
#   J := _t8
	sw $a2, -16($fp)
	sw $a2, -52($fp)
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 17}}
	j IfExit_1
NextGuard_2:
#   _t9 := muli J #i8
	lw $a1, -16($fp)
	mul $a2, $a1, 8
#   _t10 := Perolero[_t9]
	lw $a1, Perolero($a2)
	sw $a1, -56($fp)
	sw $a2, -60($fp)
#   CondBr {rel = EQI, op0 = T 10, op1 = C (CC 73), trueDest = Label {lblstr = "YesGuard_6", lblnum = 22}, falseDest = Label {lblstr = "NextGuard_3", lblnum = 23}}
	li $v0, 73
	beq $a1, $v0, YesGuard_6
	j NextGuard_3
YesGuard_6:
#   param CI
	lw $a1, -8($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param J
	lw $a1, -16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "swap"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_swap
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   _t11 := subi CI #i1
	lw $a1, -8($fp)
	sub $a2, $a1, 1
#   CI := _t11
	sw $a2, -8($fp)
	sw $a2, -64($fp)
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 17}}
	j IfExit_1
NextGuard_3:
#   _t12 := muli J #i8
	lw $a1, -16($fp)
	mul $a2, $a1, 8
#   _t13 := Perolero[_t12]
	lw $a1, Perolero($a2)
	sw $a1, -68($fp)
	sw $a2, -72($fp)
#   CondBr {rel = EQI, op0 = T 13, op1 = C (CC 70), trueDest = Label {lblstr = "YesGuard_7", lblnum = 24}, falseDest = Label {lblstr = "IfExit_1", lblnum = 17}}
	li $v0, 70
	beq $a1, $v0, YesGuard_7
	j IfExit_1
YesGuard_7:
#   CondBr {rel = NEI, op0 = R "CI", op1 = R "IF", trueDest = Label {lblstr = "JC_andalso_1", lblnum = 27}, falseDest = Label {lblstr = "IfExit_2", lblnum = 25}}
	lw $a1, -12($fp)
	lw $a2, -8($fp)
	bne $a2, $a1, JC_andalso_1
	j IfExit_2
JC_andalso_1:
#   CondBr {rel = NEI, op0 = R "CI", op1 = R "J", trueDest = Label {lblstr = "YesGuard_8", lblnum = 26}, falseDest = Label {lblstr = "IfExit_2", lblnum = 25}}
	lw $a1, -16($fp)
	lw $a2, -8($fp)
	bne $a2, $a1, YesGuard_8
	j IfExit_2
YesGuard_8:
#   param IF
	lw $a1, -12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param CI
	lw $a1, -8($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "swap"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_swap
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   Br {dest = Label {lblstr = "IfExit_2", lblnum = 25}}
	j IfExit_2
IfExit_2:
#   param IF
	lw $a1, -12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   param J
	lw $a1, -16($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "swap"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_swap
#   cleanup 8
	lw $fp, 0($fp)
	add $sp, $sp, 20
#   _t14 := subi CI #i1
	lw $a1, -8($fp)
	sub $a2, $a1, 1
#   CI := _t14
#   _t15 := subi IF #i1
	lw $a1, -12($fp)
	sub $a3, $a1, 1
#   IF := _t15
	sw $a2, -8($fp)
	sw $a2, -76($fp)
	sw $a3, -12($fp)
	sw $a3, -80($fp)
#   Br {dest = Label {lblstr = "IfExit_1", lblnum = 17}}
	j IfExit_1
IfExit_1:
#   Br {dest = Label {lblstr = "WhileHeader_1", lblnum = 14}}
	j WhileHeader_1
WhileExit_1:
#   Br {dest = Label {lblstr = "Return_1", lblnum = 12}}
	j Return_1
Return_1:
#   ; Epilog for procedure mauritius
# Epilog for procedure mauritius
#   epilog 16
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# peroles
proc_peroles:
#   ; Procedure at (69,1)
# Procedure at (69,1)
#   prolog 12
	# Prolog 84
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 84

#   var N_2 12 4
#   var J_1 -4 4
#   _t16 := subi N_2 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   J_1 := #i0
	li $a1, 0
	sw $a1, -4($fp)
	sw $a2, -16($fp)
#   Br {dest = Label {lblstr = "ForHeader", lblnum = 30}}
	j ForHeader
ForHeader:
#   CondBr {rel = LEI, op0 = R "J_1", op1 = T 16, trueDest = Label {lblstr = "ForBody", lblnum = 31}, falseDest = Label {lblstr = "ForExit", lblnum = 32}}
	lw $a1, -16($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody
	j ForExit
ForBody:
#   var Done_1 -8 4
#   Done_1 := #False
	li $a1, 0
#   var K -12 4
	sw $a1, -8($fp)
#   Br {dest = Label {lblstr = "WhileHeader_2", lblnum = 33}}
	j WhileHeader_2
WhileHeader_2:
#   IfBr {cond = R "Done_1", trueDest = Label {lblstr = "WhileExit_2", lblnum = 34}, falseDest = Label {lblstr = "YesGuard_9", lblnum = 35}}
	lw $a1, -8($fp)
	bne $a1, $0, WhileExit_2
	j YesGuard_9
YesGuard_9:
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
#   param J_1
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
#   K := call "_readChar"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readChar
	lw $a1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   param &_str7
	sw $a1, -12($fp)
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
#   CondBr {rel = EQI, op0 = R "K", op1 = C (CC 66), trueDest = Label {lblstr = "YesGuard_10", lblnum = 37}, falseDest = Label {lblstr = "JC_orelse", lblnum = 39}}
	li $v0, 66
	lw $a1, -12($fp)
	beq $a1, $v0, YesGuard_10
	j JC_orelse
JC_orelse:
#   CondBr {rel = EQI, op0 = R "K", op1 = C (CC 98), trueDest = Label {lblstr = "YesGuard_10", lblnum = 37}, falseDest = Label {lblstr = "NextGuard_4", lblnum = 38}}
	li $v0, 98
	lw $a1, -12($fp)
	beq $a1, $v0, YesGuard_10
	j NextGuard_4
YesGuard_10:
#   Done_1 := #True
	li $a1, 1
#   _t17 := muli J_1 #i8
	lw $a2, -4($fp)
	mul $a3, $a2, 8
#   Perolero[_t17] := #c66
	li $v0, 66
	sw $v0, Perolero($a3)
#   param &_str9
	sw $a1, -8($fp)
	sw $a3, -20($fp)
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
#   _t18 := muli J_1 #i8
	lw $a1, -4($fp)
	mul $a2, $a1, 8
#   _t19 := addi _t18 #i4
	add $a1, $a2, 4
#   _t20 := call "_readBoolean"
	sw $a1, -24($fp)
	sw $a2, -28($fp)
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readBoolean
	lw $a3, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   Perolero[_t19] := _t20
	lw $a1, -24($fp)
	sw $a3, Perolero($a1)
	sw $a3, -32($fp)
#   Br {dest = Label {lblstr = "IfExit_3", lblnum = 36}}
	j IfExit_3
NextGuard_4:
#   CondBr {rel = EQI, op0 = R "K", op1 = C (CC 67), trueDest = Label {lblstr = "YesGuard_11", lblnum = 40}, falseDest = Label {lblstr = "JC_orelse_1", lblnum = 42}}
	li $v0, 67
	lw $a1, -12($fp)
	beq $a1, $v0, YesGuard_11
	j JC_orelse_1
JC_orelse_1:
#   CondBr {rel = EQI, op0 = R "K", op1 = C (CC 99), trueDest = Label {lblstr = "YesGuard_11", lblnum = 40}, falseDest = Label {lblstr = "NextGuard_5", lblnum = 41}}
	li $v0, 99
	lw $a1, -12($fp)
	beq $a1, $v0, YesGuard_11
	j NextGuard_5
YesGuard_11:
#   Done_1 := #True
	li $a1, 1
#   _t21 := muli J_1 #i8
	lw $a2, -4($fp)
	mul $a3, $a2, 8
#   Perolero[_t21] := #c67
	li $v0, 67
	sw $v0, Perolero($a3)
#   param &_str10
	sw $a1, -8($fp)
	sw $a3, -36($fp)
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
#   _t22 := muli J_1 #i8
	lw $a1, -4($fp)
	mul $a2, $a1, 8
#   _t23 := addi _t22 #i4
	add $a1, $a2, 4
#   _t24 := call "_readChar"
	sw $a1, -40($fp)
	sw $a2, -44($fp)
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readChar
	lw $a3, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   Perolero[_t23] := _t24
	lw $a1, -40($fp)
	sw $a3, Perolero($a1)
#   param &_str7
	sw $a3, -48($fp)
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
#   Br {dest = Label {lblstr = "IfExit_3", lblnum = 36}}
	j IfExit_3
NextGuard_5:
#   CondBr {rel = EQI, op0 = R "K", op1 = C (CC 73), trueDest = Label {lblstr = "YesGuard_12", lblnum = 43}, falseDest = Label {lblstr = "JC_orelse_4", lblnum = 47}}
	li $v0, 73
	lw $a1, -12($fp)
	beq $a1, $v0, YesGuard_12
	j JC_orelse_4
JC_orelse_4:
#   CondBr {rel = EQI, op0 = R "K", op1 = C (CC 105), trueDest = Label {lblstr = "YesGuard_12", lblnum = 43}, falseDest = Label {lblstr = "JC_orelse_3", lblnum = 46}}
	li $v0, 105
	lw $a1, -12($fp)
	beq $a1, $v0, YesGuard_12
	j JC_orelse_3
JC_orelse_3:
#   CondBr {rel = EQI, op0 = R "K", op1 = C (CC 69), trueDest = Label {lblstr = "YesGuard_12", lblnum = 43}, falseDest = Label {lblstr = "JC_orelse_2", lblnum = 45}}
	li $v0, 69
	lw $a1, -12($fp)
	beq $a1, $v0, YesGuard_12
	j JC_orelse_2
JC_orelse_2:
#   CondBr {rel = EQI, op0 = R "K", op1 = C (CC 101), trueDest = Label {lblstr = "YesGuard_12", lblnum = 43}, falseDest = Label {lblstr = "NextGuard_6", lblnum = 44}}
	li $v0, 101
	lw $a1, -12($fp)
	beq $a1, $v0, YesGuard_12
	j NextGuard_6
YesGuard_12:
#   Done_1 := #True
	li $a1, 1
#   _t25 := muli J_1 #i8
	lw $a2, -4($fp)
	mul $a3, $a2, 8
#   Perolero[_t25] := #c73
	li $v0, 73
	sw $v0, Perolero($a3)
#   param &_str11
	sw $a1, -8($fp)
	sw $a3, -52($fp)
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
#   _t26 := muli J_1 #i8
	lw $a1, -4($fp)
	mul $a2, $a1, 8
#   _t27 := addi _t26 #i4
	add $a1, $a2, 4
#   _t28 := call "_readInteger"
	sw $a1, -56($fp)
	sw $a2, -60($fp)
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readInteger
	lw $a3, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   Perolero[_t27] := _t28
	lw $a1, -56($fp)
	sw $a3, Perolero($a1)
	sw $a3, -64($fp)
#   Br {dest = Label {lblstr = "IfExit_3", lblnum = 36}}
	j IfExit_3
NextGuard_6:
#   CondBr {rel = EQI, op0 = R "K", op1 = C (CC 70), trueDest = Label {lblstr = "YesGuard_13", lblnum = 48}, falseDest = Label {lblstr = "JC_orelse_5", lblnum = 49}}
	li $v0, 70
	lw $a1, -12($fp)
	beq $a1, $v0, YesGuard_13
	j JC_orelse_5
JC_orelse_5:
#   CondBr {rel = EQI, op0 = R "K", op1 = C (CC 102), trueDest = Label {lblstr = "YesGuard_13", lblnum = 48}, falseDest = Label {lblstr = "IfExit_3", lblnum = 36}}
	li $v0, 102
	lw $a1, -12($fp)
	beq $a1, $v0, YesGuard_13
	j IfExit_3
YesGuard_13:
#   Done_1 := #True
	li $a1, 1
#   _t29 := muli J_1 #i8
	lw $a2, -4($fp)
	mul $a3, $a2, 8
#   Perolero[_t29] := #c70
	li $v0, 70
	sw $v0, Perolero($a3)
#   param &_str12
	sw $a1, -8($fp)
	sw $a3, -68($fp)
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
#   _t30 := muli J_1 #i8
	lw $a1, -4($fp)
	mul $a2, $a1, 8
#   _t31 := addi _t30 #i4
	add $a1, $a2, 4
#   _tf32 := call "_readFloat"
	sw $a1, -72($fp)
	sw $a2, -76($fp)
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__readFloat
	l.s $f1, 8($fp)
#   cleanup 0
	lw $fp, 0($fp)
	add $sp, $sp, 12
#   Perolero[_t31] := _tf32
	lw $a1, -72($fp)
	s.s $f1, Perolero($a1)
	s.s $f1, -80($fp)
#   Br {dest = Label {lblstr = "IfExit_3", lblnum = 36}}
	j IfExit_3
IfExit_3:
#   Br {dest = Label {lblstr = "WhileHeader_2", lblnum = 33}}
	j WhileHeader_2
WhileExit_2:
#   J_1 := addi J_1 #i1
	lw $a1, -4($fp)
	add $a1, $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "ForHeader", lblnum = 30}}
	j ForHeader
ForExit:
#   param N_2
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "mauritius"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_mauritius
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   param N_2
	lw $a1, 12($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "printOut"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_printOut
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "Return_2", lblnum = 28}}
	j Return_2
Return_2:
#   ; Epilog for procedure peroles
# Epilog for procedure peroles
#   epilog 12
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# printOne
proc_printOne:
#   ; Procedure at (176,1)
# Procedure at (176,1)
#   prolog 0
	# Prolog 92
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 92

#   var J_2 12 4
#   param J_2
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
#   _t33 := muli J_2 #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t34 := Perolero[_t33]
	lw $a1, Perolero($a2)
#   param _t34
	sw $a1, -4($fp)
	sw $a2, -8($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "_writeChar"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeChar
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
#   _t35 := muli J_2 #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t36 := Perolero[_t35]
	lw $a1, Perolero($a2)
	sw $a1, -12($fp)
	sw $a2, -16($fp)
#   CondBr {rel = EQI, op0 = T 36, op1 = C (CC 66), trueDest = Label {lblstr = "YesGuard_14", lblnum = 53}, falseDest = Label {lblstr = "NextGuard_7", lblnum = 54}}
	li $v0, 66
	beq $a1, $v0, YesGuard_14
	j NextGuard_7
YesGuard_14:
#   _t37 := muli J_2 #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t38 := addi _t37 #i4
	add $a1, $a2, 4
#   _t39 := Perolero[_t38]
	lw $a3, Perolero($a1)
#   param _t39
	sw $a1, -20($fp)
	sw $a2, -24($fp)
	sw $a3, -28($fp)
	add $sp, $sp, -4
	sw $a3, 0($sp)
#   call "_writeBoolean"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeBoolean
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "IfExit_4", lblnum = 52}}
	j IfExit_4
NextGuard_7:
#   _t40 := muli J_2 #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t41 := Perolero[_t40]
	lw $a1, Perolero($a2)
	sw $a1, -32($fp)
	sw $a2, -36($fp)
#   CondBr {rel = EQI, op0 = T 41, op1 = C (CC 67), trueDest = Label {lblstr = "YesGuard_15", lblnum = 55}, falseDest = Label {lblstr = "NextGuard_8", lblnum = 56}}
	li $v0, 67
	beq $a1, $v0, YesGuard_15
	j NextGuard_8
YesGuard_15:
#   _t42 := muli J_2 #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t43 := addi _t42 #i4
	add $a1, $a2, 4
#   _t44 := Perolero[_t43]
	lw $a3, Perolero($a1)
#   param _t44
	sw $a1, -40($fp)
	sw $a2, -44($fp)
	sw $a3, -48($fp)
	add $sp, $sp, -4
	sw $a3, 0($sp)
#   call "_writeChar"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeChar
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "IfExit_4", lblnum = 52}}
	j IfExit_4
NextGuard_8:
#   _t45 := muli J_2 #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t46 := Perolero[_t45]
	lw $a1, Perolero($a2)
	sw $a1, -52($fp)
	sw $a2, -56($fp)
#   CondBr {rel = EQI, op0 = T 46, op1 = C (CC 73), trueDest = Label {lblstr = "YesGuard_16", lblnum = 57}, falseDest = Label {lblstr = "NextGuard_9", lblnum = 58}}
	li $v0, 73
	beq $a1, $v0, YesGuard_16
	j NextGuard_9
YesGuard_16:
#   _t47 := muli J_2 #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t48 := addi _t47 #i4
	add $a1, $a2, 4
#   _t49 := Perolero[_t48]
	lw $a3, Perolero($a1)
#   param _t49
	sw $a1, -60($fp)
	sw $a2, -64($fp)
	sw $a3, -68($fp)
	add $sp, $sp, -4
	sw $a3, 0($sp)
#   call "_writeInteger"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeInteger
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "IfExit_4", lblnum = 52}}
	j IfExit_4
NextGuard_9:
#   _t50 := muli J_2 #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t51 := Perolero[_t50]
	lw $a1, Perolero($a2)
	sw $a1, -72($fp)
	sw $a2, -76($fp)
#   CondBr {rel = EQI, op0 = T 51, op1 = C (CC 70), trueDest = Label {lblstr = "YesGuard_17", lblnum = 59}, falseDest = Label {lblstr = "IfExit_4", lblnum = 52}}
	li $v0, 70
	beq $a1, $v0, YesGuard_17
	j IfExit_4
YesGuard_17:
#   _t52 := muli J_2 #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t53 := addi _t52 #i4
	add $a1, $a2, 4
#   _tf54 := Perolero[_t53]
	l.s $f1, Perolero($a1)
#   param _tf54
	sw $a1, -80($fp)
	sw $a2, -84($fp)
	s.s $f1, -88($fp)
	add $sp, $sp, -4
	s.s $f1, 0($sp)
#   call "_writeFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeFloat
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   Br {dest = Label {lblstr = "IfExit_4", lblnum = 52}}
	j IfExit_4
IfExit_4:
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
#   Br {dest = Label {lblstr = "Return_3", lblnum = 50}}
	j Return_3
Return_3:
#   ; Epilog for procedure printOne
# Epilog for procedure printOne
#   epilog 0
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# printOut
proc_printOut:
#   ; Procedure at (169,1)
# Procedure at (169,1)
#   prolog 4
	# Prolog 12
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 12

#   var N_3 12 4
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
#   var J_3 -4 4
#   _t55 := subi N_3 #i1
	lw $a1, 12($fp)
	sub $a2, $a1, 1
#   J_3 := #i0
	li $a1, 0
	sw $a1, -4($fp)
	sw $a2, -8($fp)
#   Br {dest = Label {lblstr = "ForHeader_1", lblnum = 62}}
	j ForHeader_1
ForHeader_1:
#   CondBr {rel = LEI, op0 = R "J_3", op1 = T 55, trueDest = Label {lblstr = "ForBody_1", lblnum = 63}, falseDest = Label {lblstr = "ForExit_1", lblnum = 64}}
	lw $a1, -8($fp)
	lw $a2, -4($fp)
	slt $v0, $a1, $a2
	beq $v0, $0, ForBody_1
	j ForExit_1
ForBody_1:
#   param J_3
	lw $a1, -4($fp)
	add $sp, $sp, -4
	sw $a1, 0($sp)
#   call "printOne"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc_printOne
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   J_3 := addi J_3 #i1
	lw $a1, -4($fp)
	add $a1, $a1, 1
	sw $a1, -4($fp)
#   Br {dest = Label {lblstr = "ForHeader_1", lblnum = 62}}
	j ForHeader_1
ForExit_1:
#   Br {dest = Label {lblstr = "Return_4", lblnum = 60}}
	j Return_4
Return_4:
#   ; Epilog for procedure printOut
# Epilog for procedure printOut
#   epilog 4
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
# swap
proc_swap:
#   ; Procedure at (140,1)
# Procedure at (140,1)
#   prolog 8
	# Prolog 276
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 276

#   var P 12 4
#   var Q 16 4
#   var T -8 8
#   _t56 := muli P #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t57 := Perolero[_t56]
	lw $a1, Perolero($a2)
#   T := _t57
	sw $a1, -8($fp)
	sw $a1, -12($fp)
	sw $a2, -16($fp)
#   CondBr {rel = EQI, op0 = R "T", op1 = C (CC 66), trueDest = Label {lblstr = "YesGuard_18", lblnum = 68}, falseDest = Label {lblstr = "NextGuard_10", lblnum = 69}}
	li $v0, 66
	beq $a1, $v0, YesGuard_18
	j NextGuard_10
YesGuard_18:
#   _t58 := muli P #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t59 := addi _t58 #i4
	add $a1, $a2, 4
#   _t60 := Perolero[_t59]
	lw $a3, Perolero($a1)
#   T[#i4] := _t60
	sw $a1, -20($fp)
	sw $a2, -24($fp)
	sw $a3, -28($fp)
	sw $a3, -4($fp)
#   Br {dest = Label {lblstr = "IfExit_5", lblnum = 67}}
	j IfExit_5
NextGuard_10:
#   CondBr {rel = EQI, op0 = R "T", op1 = C (CC 67), trueDest = Label {lblstr = "YesGuard_19", lblnum = 70}, falseDest = Label {lblstr = "NextGuard_11", lblnum = 71}}
	li $v0, 67
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_19
	j NextGuard_11
YesGuard_19:
#   _t61 := muli P #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t62 := addi _t61 #i4
	add $a1, $a2, 4
#   _t63 := Perolero[_t62]
	lw $a3, Perolero($a1)
#   T[#i4] := _t63
	sw $a1, -32($fp)
	sw $a2, -36($fp)
	sw $a3, -40($fp)
	sw $a3, -4($fp)
#   Br {dest = Label {lblstr = "IfExit_5", lblnum = 67}}
	j IfExit_5
NextGuard_11:
#   CondBr {rel = EQI, op0 = R "T", op1 = C (CC 73), trueDest = Label {lblstr = "YesGuard_20", lblnum = 72}, falseDest = Label {lblstr = "NextGuard_12", lblnum = 73}}
	li $v0, 73
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_20
	j NextGuard_12
YesGuard_20:
#   _t64 := muli P #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t65 := addi _t64 #i4
	add $a1, $a2, 4
#   _t66 := Perolero[_t65]
	lw $a3, Perolero($a1)
#   T[#i4] := _t66
	sw $a1, -44($fp)
	sw $a2, -48($fp)
	sw $a3, -52($fp)
	sw $a3, -4($fp)
#   Br {dest = Label {lblstr = "IfExit_5", lblnum = 67}}
	j IfExit_5
NextGuard_12:
#   CondBr {rel = EQI, op0 = R "T", op1 = C (CC 70), trueDest = Label {lblstr = "YesGuard_21", lblnum = 74}, falseDest = Label {lblstr = "IfExit_5", lblnum = 67}}
	li $v0, 70
	lw $a1, -8($fp)
	beq $a1, $v0, YesGuard_21
	j IfExit_5
YesGuard_21:
#   _t67 := muli P #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t68 := addi _t67 #i4
	add $a1, $a2, 4
#   _tf69 := Perolero[_t68]
	l.s $f1, Perolero($a1)
#   T[#i4] := _tf69
	sw $a1, -56($fp)
	sw $a2, -60($fp)
	s.s $f1, -64($fp)
	s.s $f1, -4($fp)
#   Br {dest = Label {lblstr = "IfExit_5", lblnum = 67}}
	j IfExit_5
IfExit_5:
#   _t70 := muli Q #i8
	lw $a1, 16($fp)
	mul $a2, $a1, 8
#   _t71 := Perolero[_t70]
	lw $a1, Perolero($a2)
#   _t72 := muli P #i8
	lw $a3, 12($fp)
	mul $t0, $a3, 8
#   Perolero[_t72] := _t71
	sw $a1, Perolero($t0)
#   _t73 := muli P #i8
	mul $t1, $a3, 8
#   _t74 := Perolero[_t73]
	lw $a3, Perolero($t1)
	sw $a1, -68($fp)
	sw $a2, -72($fp)
	sw $a3, -76($fp)
	sw $t0, -80($fp)
	sw $t1, -84($fp)
#   CondBr {rel = EQI, op0 = T 74, op1 = C (CC 66), trueDest = Label {lblstr = "YesGuard_22", lblnum = 76}, falseDest = Label {lblstr = "NextGuard_13", lblnum = 77}}
	li $v0, 66
	beq $a3, $v0, YesGuard_22
	j NextGuard_13
YesGuard_22:
#   _t75 := muli Q #i8
	lw $a1, 16($fp)
	mul $a2, $a1, 8
#   _t76 := addi _t75 #i4
	add $a1, $a2, 4
#   _t77 := Perolero[_t76]
	lw $a3, Perolero($a1)
#   _t78 := muli P #i8
	lw $t0, 12($fp)
	mul $t1, $t0, 8
#   _t79 := addi _t78 #i4
	add $t0, $t1, 4
#   Perolero[_t79] := _t77
	sw $a3, Perolero($t0)
	sw $a1, -88($fp)
	sw $a2, -92($fp)
	sw $a3, -96($fp)
	sw $t0, -100($fp)
	sw $t1, -104($fp)
#   Br {dest = Label {lblstr = "IfExit_6", lblnum = 75}}
	j IfExit_6
NextGuard_13:
#   _t80 := muli P #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t81 := Perolero[_t80]
	lw $a1, Perolero($a2)
	sw $a1, -108($fp)
	sw $a2, -112($fp)
#   CondBr {rel = EQI, op0 = T 81, op1 = C (CC 67), trueDest = Label {lblstr = "YesGuard_23", lblnum = 78}, falseDest = Label {lblstr = "NextGuard_14", lblnum = 79}}
	li $v0, 67
	beq $a1, $v0, YesGuard_23
	j NextGuard_14
YesGuard_23:
#   _t82 := muli Q #i8
	lw $a1, 16($fp)
	mul $a2, $a1, 8
#   _t83 := addi _t82 #i4
	add $a1, $a2, 4
#   _t84 := Perolero[_t83]
	lw $a3, Perolero($a1)
#   _t85 := muli P #i8
	lw $t0, 12($fp)
	mul $t1, $t0, 8
#   _t86 := addi _t85 #i4
	add $t0, $t1, 4
#   Perolero[_t86] := _t84
	sw $a3, Perolero($t0)
	sw $a1, -116($fp)
	sw $a2, -120($fp)
	sw $a3, -124($fp)
	sw $t0, -128($fp)
	sw $t1, -132($fp)
#   Br {dest = Label {lblstr = "IfExit_6", lblnum = 75}}
	j IfExit_6
NextGuard_14:
#   _t87 := muli P #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t88 := Perolero[_t87]
	lw $a1, Perolero($a2)
	sw $a1, -136($fp)
	sw $a2, -140($fp)
#   CondBr {rel = EQI, op0 = T 88, op1 = C (CC 73), trueDest = Label {lblstr = "YesGuard_24", lblnum = 80}, falseDest = Label {lblstr = "NextGuard_15", lblnum = 81}}
	li $v0, 73
	beq $a1, $v0, YesGuard_24
	j NextGuard_15
YesGuard_24:
#   _t89 := muli Q #i8
	lw $a1, 16($fp)
	mul $a2, $a1, 8
#   _t90 := addi _t89 #i4
	add $a1, $a2, 4
#   _t91 := Perolero[_t90]
	lw $a3, Perolero($a1)
#   _t92 := muli P #i8
	lw $t0, 12($fp)
	mul $t1, $t0, 8
#   _t93 := addi _t92 #i4
	add $t0, $t1, 4
#   Perolero[_t93] := _t91
	sw $a3, Perolero($t0)
	sw $a1, -144($fp)
	sw $a2, -148($fp)
	sw $a3, -152($fp)
	sw $t0, -156($fp)
	sw $t1, -160($fp)
#   Br {dest = Label {lblstr = "IfExit_6", lblnum = 75}}
	j IfExit_6
NextGuard_15:
#   _t94 := muli P #i8
	lw $a1, 12($fp)
	mul $a2, $a1, 8
#   _t95 := Perolero[_t94]
	lw $a1, Perolero($a2)
	sw $a1, -164($fp)
	sw $a2, -168($fp)
#   CondBr {rel = EQI, op0 = T 95, op1 = C (CC 70), trueDest = Label {lblstr = "YesGuard_25", lblnum = 82}, falseDest = Label {lblstr = "IfExit_6", lblnum = 75}}
	li $v0, 70
	beq $a1, $v0, YesGuard_25
	j IfExit_6
YesGuard_25:
#   _t96 := muli Q #i8
	lw $a1, 16($fp)
	mul $a2, $a1, 8
#   _t97 := addi _t96 #i4
	add $a1, $a2, 4
#   _tf98 := Perolero[_t97]
	l.s $f1, Perolero($a1)
#   _t99 := muli P #i8
	lw $a3, 12($fp)
	mul $t0, $a3, 8
#   _t100 := addi _t99 #i4
	add $a3, $t0, 4
#   Perolero[_t100] := _tf98
	s.s $f1, Perolero($a3)
	sw $a1, -172($fp)
	sw $a2, -176($fp)
	sw $a3, -180($fp)
	sw $t0, -184($fp)
	s.s $f1, -188($fp)
#   Br {dest = Label {lblstr = "IfExit_6", lblnum = 75}}
	j IfExit_6
IfExit_6:
#   _t101 := muli Q #i8
	lw $a1, 16($fp)
	mul $a2, $a1, 8
#   Perolero[_t101] := T
	lw $a1, -8($fp)
	sw $a1, Perolero($a2)
#   _t102 := muli Q #i8
	lw $a1, 16($fp)
	mul $a3, $a1, 8
#   _t103 := Perolero[_t102]
	lw $a1, Perolero($a3)
	sw $a1, -192($fp)
	sw $a2, -196($fp)
	sw $a3, -200($fp)
#   CondBr {rel = EQI, op0 = T 103, op1 = C (CC 66), trueDest = Label {lblstr = "YesGuard_26", lblnum = 84}, falseDest = Label {lblstr = "NextGuard_16", lblnum = 85}}
	li $v0, 66
	beq $a1, $v0, YesGuard_26
	j NextGuard_16
YesGuard_26:
#   _t104 := T[#i4]
	lw $a1, -4($fp)
#   _t105 := muli Q #i8
	lw $a2, 16($fp)
	mul $a3, $a2, 8
#   _t106 := addi _t105 #i4
	add $a2, $a3, 4
#   Perolero[_t106] := _t104
	sw $a1, Perolero($a2)
	sw $a1, -204($fp)
	sw $a2, -208($fp)
	sw $a3, -212($fp)
#   Br {dest = Label {lblstr = "IfExit_7", lblnum = 83}}
	j IfExit_7
NextGuard_16:
#   _t107 := muli Q #i8
	lw $a1, 16($fp)
	mul $a2, $a1, 8
#   _t108 := Perolero[_t107]
	lw $a1, Perolero($a2)
	sw $a1, -216($fp)
	sw $a2, -220($fp)
#   CondBr {rel = EQI, op0 = T 108, op1 = C (CC 67), trueDest = Label {lblstr = "YesGuard_27", lblnum = 86}, falseDest = Label {lblstr = "NextGuard_17", lblnum = 87}}
	li $v0, 67
	beq $a1, $v0, YesGuard_27
	j NextGuard_17
YesGuard_27:
#   _t109 := T[#i4]
	lw $a1, -4($fp)
#   _t110 := muli Q #i8
	lw $a2, 16($fp)
	mul $a3, $a2, 8
#   _t111 := addi _t110 #i4
	add $a2, $a3, 4
#   Perolero[_t111] := _t109
	sw $a1, Perolero($a2)
	sw $a1, -224($fp)
	sw $a2, -228($fp)
	sw $a3, -232($fp)
#   Br {dest = Label {lblstr = "IfExit_7", lblnum = 83}}
	j IfExit_7
NextGuard_17:
#   _t112 := muli Q #i8
	lw $a1, 16($fp)
	mul $a2, $a1, 8
#   _t113 := Perolero[_t112]
	lw $a1, Perolero($a2)
	sw $a1, -236($fp)
	sw $a2, -240($fp)
#   CondBr {rel = EQI, op0 = T 113, op1 = C (CC 73), trueDest = Label {lblstr = "YesGuard_28", lblnum = 88}, falseDest = Label {lblstr = "NextGuard_18", lblnum = 89}}
	li $v0, 73
	beq $a1, $v0, YesGuard_28
	j NextGuard_18
YesGuard_28:
#   _t114 := T[#i4]
	lw $a1, -4($fp)
#   _t115 := muli Q #i8
	lw $a2, 16($fp)
	mul $a3, $a2, 8
#   _t116 := addi _t115 #i4
	add $a2, $a3, 4
#   Perolero[_t116] := _t114
	sw $a1, Perolero($a2)
	sw $a1, -244($fp)
	sw $a2, -248($fp)
	sw $a3, -252($fp)
#   Br {dest = Label {lblstr = "IfExit_7", lblnum = 83}}
	j IfExit_7
NextGuard_18:
#   _t117 := muli Q #i8
	lw $a1, 16($fp)
	mul $a2, $a1, 8
#   _t118 := Perolero[_t117]
	lw $a1, Perolero($a2)
	sw $a1, -256($fp)
	sw $a2, -260($fp)
#   CondBr {rel = EQI, op0 = T 118, op1 = C (CC 70), trueDest = Label {lblstr = "YesGuard_29", lblnum = 90}, falseDest = Label {lblstr = "IfExit_7", lblnum = 83}}
	li $v0, 70
	beq $a1, $v0, YesGuard_29
	j IfExit_7
YesGuard_29:
#   _tf119 := T[#i4]
	l.s $f1, -4($fp)
#   _t120 := muli Q #i8
	lw $a1, 16($fp)
	mul $a2, $a1, 8
#   _t121 := addi _t120 #i4
	add $a1, $a2, 4
#   Perolero[_t121] := _tf119
	s.s $f1, Perolero($a1)
	sw $a1, -264($fp)
	sw $a2, -268($fp)
	s.s $f1, -272($fp)
#   Br {dest = Label {lblstr = "IfExit_7", lblnum = 83}}
	j IfExit_7
IfExit_7:
#   Br {dest = Label {lblstr = "Return_5", lblnum = 65}}
	j Return_5
Return_5:
#   ; Epilog for procedure swap
# Epilog for procedure swap
#   epilog 8
	move $sp, $fp
#   Return
	lw $ra, 4($fp)
	jr $ra
