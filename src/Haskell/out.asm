	.data
	.align 2
_base_header: .space 8
	.align 2
_last_used: .space 4
_true: .asciiz "true\n"
_false: .asciiz "false\n"
_str0: .asciiz "\n"
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
	li $v0, 10
	syscall
# main
proc_main:
#   ; Procedure at (2,1)
# Procedure at (2,1)
#   prolog 8
	move $fp, $sp
	sw $ra, 4($fp)
	sub $sp, $sp, 8
#   var A0 -4 4
#   A0 := #i0
	li $a1, 0
#   var B -8 4
#   f.B := #f3.0
	li.s $f1, 3.0
#   A0 := #i1
	li $a1, 1
	j ForHeader
ForHeader:
	li $v0, 10
	slt $v0, $v0, $a1
	beq $v0, $0, ForBody
	j ForExit
ForBody:
#   param &_str0
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
#   _tf0 := addf f.B #f100.0
	li.s $f0, 100.0
	add.s $f2, $f1, $f0
#   param _tf0
	add $sp, $sp, -4
	s.s $f2, 0($sp)
#   call "_writeFloat"
	sub $sp, $sp, 12
	sw $fp, 0($sp)
	jal proc__writeFloat
#   cleanup 4
	lw $fp, 0($fp)
	add $sp, $sp, 16
#   A0 := addi A0 #i1
	add $a1, $a1, 1
	j ForHeader
ForExit:
	j Return
Return:
#   ; Epilog for procedure main
# Epilog for procedure main
#   epilog 8
	move $sp, $fp
	lw $ra, 4($fp)
	jr $ra
