    .data
_true: .asciiz "true\n"
_false: .asciiz "false\n"

_string: .asciiz "Hola Carlos\n"

    .text
main:
    addi $sp, $sp, -16

    la   $v0, _string       # 0x4048f5c3
    sw   $v0, 12($sp)
    # sw $, 8($sp)          # Don't assign return value
    # sw $, 4($sp)          # Callee stores return address
    sw   $fp, 0($sp)

    jal  _writeStr

    lw   $fp, 0($fp)
    addi $sp, $sp, 16




    addi $sp, $sp, -12
    sw   $fp, 0($sp)
    jal  _readChar

    lw   $s0, 8($fp)



    addi $sp, $sp, -4
    sw   $s0, 0($sp)
    addi $sp, $sp, -12
    sw   $fp, 0($sp)
    jal  _writeChar

    lw   $fp, 0($fp)
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
    move $fp, $sp
    sw   $ra, 4($fp)
    li   $v0, 5            # Read Integer syscall
    syscall

    beq  $v0, $zero, _rBz
    li   $v0, 1
_rBz:
    sw   $v0, 8($fp)

    lw   $ra, 4($fp)
    jr   $ra


# readFloat:
_readFloat:
    move $fp, $sp
    sw   $ra, 4($fp)
    li   $v0, 6            # Read Float syscall
    syscall

    s.s  $f0, 8($fp)

    lw   $ra, 4($fp)
    jr   $ra


# readInteger:
_readInteger:
    move $fp, $sp
    sw   $ra, 4($fp)
    li   $v0, 5            # Read Integer syscall
    syscall

    sw   $v0, 8($fp)

    lw   $ra, 4($fp)
    jr   $ra


# readChar:
_readChar:
    move $fp, $sp
    sw   $ra, 4($fp)
    li   $v0, 12           # Read Char syscall
    syscall

    sw   $v0, 8($fp)

    lw   $ra, 4($fp)
    jr   $ra

# readChar:
_readChar:
    move $fp, $sp
    sw   $ra, 4($fp)
    li   $v0, 12           # Read Char syscall
    syscall

    sw   $v0, 8($fp)

    lw   $ra, 4($fp)
    jr   $ra

# toInt:
_toInt:
    move $fp, $sp
    sw   $ra, 4($fp)

    l.s  $SCRATCHF, 12($fp)
    cvt.w.s $SCRATCHF, $SCRATCHF
    s.s  $SCRATCHF, 8($fp)

    lw   $ra, 4($fp)
    jr   $ra

# toFloat:
_toFloat:
    move $fp, $sp
    sw   $ra, 4($fp)

    l.s  $SCRATCHF, 12($fp)
    cvt.s.w $SCRATCHF, $SCRATCHF
    s.s  $SCRATCHF, 8($fp)

    lw   $ra, 4($fp)
    jr   $ra
