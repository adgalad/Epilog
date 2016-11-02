    .data
_true: .asciiz "true\n"
_false: .asciiz "false\n"

_string: .asciiz "Hola Carlos\n"

    .text
main:
    addi $sp, $sp, -16

    sw   $fp, 0($sp)
    # sw $, 4($sp)          # Callee stores return address
    # sw $, 8($sp)          # Don't assign return value
    la   $v0, _string       # 0x4048f5c3
    sw   $v0, 12($sp)

    jal  _writeStr

    lw   $fp, 12($sp)
    addi $sp, $sp, 16

    li   $v0, 10            # Exit
    syscall


# writeBoolean
_writeBoolean:
    move $fp, $sp          # Save the frame pointer
    sw   $ra, 4($fp)       # Store the return address
    li   $v0, 4            # Syscall for strings
    lw   $v1, 12($fp)      # Load the argument
    beq  $v1, $zero, _wB0  # Check it
    la   $a0, _true
    j   _wBend
_wB0:
    la  $a0, _false
_wBend:
    syscall
    lw  $ra, 4($fp)
    jr  $ra


# writeFloat:
_writeFloat:
    move $fp, $sp
    sw   $ra, 4($fp)

    li   $v0, 2
    l.s  $f12, 12($fp)
    syscall

    lw   $ra, 4($fp)
    jr   $ra


# writeInteger:
_writeInteger:
    move $fp, $sp
    sw   $ra, 4($fp)

    li   $v0, 1
    lw   $a0, 12($fp)
    syscall

    lw   $ra, 4($fp)
    jr   $ra


# writeChar:
_writeChar:
    move $fp, $sp
    sw   $ra, 4($fp)

    li   $v0, 11
    lw   $a0, 12($fp)
    syscall

    lw   $ra, 4($fp)
    jr   $ra


# writeStr:
_writeStr:
    move $fp, $sp
    sw   $ra, 4($fp)

    li   $v0, 4
    lw   $a0, 12($fp)
    syscall

    lw   $ra, 4($fp)
    jr   $ra


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


# make:
_make:
    nop


# ekam:
_ekam:
    nop
