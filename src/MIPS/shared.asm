    .data
_true: .asciiz "true\n"
_false: .asciiz "false\n"

    .text
main:
    addi $sp, $sp, -16

    sw $fp, 0($sp)
    # sw $, 4($sp)        # Callee stores return address
    # sw $, 8($sp)        # Don't assign return value
    li $v0, 1
    sw $v0, 12($sp)

    jal _writeBoolean

    lw $fp, 12($sp)
    addi $sp, $sp, 16

    li $v0, 10            # Exit
    syscall


# writeBoolean
_writeBoolean:
    move $fp, $sp         # Save the frame pointer
    sw $ra, 4($fp)        # Store the return address

    li $v0, 4             # Syscall for strings
    lw $v1, 12($fp)       # Load the argument
    beq $v1, $zero, _wB0  # Check it
    la $a0, _true
    j _wBend
_wB0:
    la $a0, _false
_wBend:
    syscall
    lw $ra, 4($fp)
    jr $ra


# writeFloat:
_writeFloat:
    nop


# writeInteger:
_writeInteger:
    nop


# writeChar:
_writeChar:
    nop


# writeStr:
_writeStr:
    nop


# readBoolean:
_readBoolean:
    nop


# readFloat:
_readFloat:
    nop


# readInteger:
_readInteger:
    nop


# readChar:
_readChar:
    nop


# ekam:
_ekam:
    nop
