
  .data
_base_header: .word 0, 0    # base_header {next, size}
_last_used: .word 0
  .text


main:

li   $t0, 400000
addi $sp, $sp, -4
sw   $t0, 0($sp)
addi $sp, $sp, -12
sw   $fp, 0($sp)

jal  _make

lw   $s2, 8($fp)
lw   $fp, 0($fp)
li $v0, 34
add $a0, $s2, $0
syscall
li $v0, 11
li $a0, '\n'
syscall

##############################################################

li   $t0, 4
addi $sp, $sp, -4
sw   $t0, 0($sp)
addi $sp, $sp, -12
sw   $fp, 0($sp)

jal  _make

lw   $s3, 8($fp)
lw   $fp, 0($fp)
li $v0, 34
add $a0, $s3, $0
syscall
li $v0, 11
li $a0, '\n'
syscall

############################################################

addi $sp, $sp, -4
sw   $s2, 0($sp)
addi $sp, $sp, -12
sw   $fp, 0($sp)

jal  _ekam

lw   $fp, 0($fp)

##############################################################

li   $t0, 4
addi $sp, $sp, -4
sw   $t0, 0($sp)
addi $sp, $sp, -12
sw   $fp, 0($sp)

jal  _make

lw   $s4, 8($fp)
lw   $fp, 0($fp)
li $v0, 34
add $a0, $s4, $0
syscall
li $v0, 11
li $a0, '\n'
syscall


li $v0, 10
syscall

##############################################################
##############################################################
##################        MAKE       #########################
##############################################################
##############################################################
_make:
  # a0: nbytes       <--- malloc argument
  # a1: &base_header
  # v1: nunits

move $fp, $sp          # Save the frame pointer
sw   $ra, 4($fp)       # Store the return address

lw   $a0, 12($fp)      # get argument nbytes
la   $a1, _base_header # get base header location

addi $v1, $a0, 7      # nbytes + sizeof(Header) - 1   -- header size is 8
div  $v1, $v1, 8      # v1 = v1 / sizeof(header)
addi $v1, $v1, 1      # v1 = v1 + 1   <-- nunits

lw $s0, _last_used    # s0       = last_used

bne $s0, $zero, _malloc_body   # if last_used == null
  sw  $a1, _base_header        #   base_header.next = &base
  sw  $a1, _last_used          #   last_used = &base
  move $s0, $a1                #   s0 = last_used

_malloc_body:

# t0: previous
# t1: current
move $t0, $s0           # previous = last_used
lw   $t1, 0($t0)        # current  = last_used->next

_malloc_loop:

# t2: current->size
lw $t2, 4($t1)

blt $t2, $v1, _malloc_not_found # if current->size >= nunits

  bne $t2, $v1, _malloc_else # if current->size == nunits
    lw $t3, 0($t1)           #    t3 = current->next
    sw $t3, 0($t0)           #    previous->next = current->next
    j _malloc_return

_malloc_else:                # else
    sub $t3, $t2, $v1        #    t3 = current->size - nunits
    sw  $t3, 4($t1)          #    current->size = t3
    mul $t3, $t3, 8          #    t3 = current->size * sizeof(Header)
    add $t1, $t1, $t3        #    current += current->size
    sw  $v1, 4($t1)          #    current->size = nunits

_malloc_return:

sw   $t0, _last_used       # last_used = previous
addi $t1, $t1, 8
sw   $t1, 8($fp)           # return  current + sizeof(Header)
lw   $ra, 4($fp)
jr   $ra

_malloc_not_found:
bne $t1, $s0, _malloc_loop_increment
# call more memory function
  addi $sp, $sp, -16
  sw   $v1, 12($sp)
  sw   $fp, 0($sp)

  jal  _more_mem

  lw   $t1, 8($fp)
  lw   $fp, 0($fp)
  addi $sp, $sp, 16

_malloc_loop_increment:
move $t0, $t1         # previous = current
lw   $t1, 0($t1)      # current  = current->next
j _malloc_loop



##############################################################
##############################################################
##################        MOAR_M       #######################
##############################################################
##############################################################
_more_mem:
  # a3: nunits       <-- argument
  # v0: memory reserved by sbrk

move $fp, $sp          # Save the frame pointer
sw   $ra, 4($fp)       # Store the return address

lw  $a3, 12($fp)       # get argument nunits

bge $a3, 128, _mm_isBigger  # if nunits < TOLETE
  li  $a3, 128              #    nunits = TOLETE

_mm_isBigger:

mul  $a0, $a3, 8     # a0 = nunits * sizeof(Header)
li   $v0, 9          # sbrk -> syscall 9
syscall              # v0 = sbrk(a0)

sw   $a3, 4($v0)     # mem->size = nunints     (mem <- v0)
addi $v0, $v0, 8     # mem += sizeof(header)

#call free($v0)
  addi $sp, $sp, -16
  sw   $v0, 12($sp)
  sw   $fp, 0($sp)

  jal  _ekam

  lw   $fp, 0($fp)
  addi $sp, $sp, 16

lw $v0, _last_used
sw $v0, 8($fp)              # return last_used
lw $ra, 4($fp)
jr $ra

##############################################################
##############################################################
##################        EKAM         #######################
##############################################################
##############################################################

_ekam:
  # a2: pointer (insertp)  <-- argument
  # t3: current
  # t4: current->next

move $fp, $sp          # Save the frame pointer
sw   $ra, 4($fp)       # Store the return address

lw $a2, 12($fp)        # Get argument pointer
sub $a2, $a2, 8        # insertp = ptr - sizeof(Header)

# t3: current = last_used
lw $t3, _last_used

_free_loop:
  lw $t4, 0($t3)               # t4 = current->next

  blt $t3, $a2, _free_loop_or  # if current >= insertp
    j _free_loop_body

  _free_loop_or:
  blt $a2, $t4, _free_loop_end # if insertp >= current->next

    _free_loop_body:
    blt $t3, $t4, _free_loop_increment
    bge $t3, $a2, _free_loop_or2
    j _free_loop_end

    _free_loop_or2:
    bge $a2, $t4, _free_loop_increment
    j _free_loop_end

    _free_loop_increment:
    move $t3, $t4
    j _free_loop

_free_loop_end:
lw  $t5, 4($a2)           # t5 = insertp->size
mul $t7, $t5, 8           # t7 = insertp->size * size(Header)
add $t6, $t7, $a2         # t6 = insertp + insertp->size

bne $t6, $t4, _free_else  # if insertp + insertp->size == current->next
  lw $t7, 0($t4)          # t7 = current->next->next
  sw $t7, 0($a2)          # insertp->next = current->next->next

  lw $t7, 4($t4)          # t7 = current->next->size
  add $t7, $t7, $t5       # t7 = current->next->size + insertp->size
  sw $t7, 4($a2)          # insertp->size += current->next->size

  j _free_if

_free_else:

sw $t4, 0($a2)            # insertp->next = current->next

_free_if:
lw  $t5, 4($t3)           # t5 = current->size
mul $t7, $t5, 8           # t7 = current->size * sizeof(Header)
add $t6, $t7, $t3         # t6 = currentp + currentp->size

bne $t6, $a2, _free_else2 # if current + current->size == insertp->next
  lw $t7, 0($a2)          # t7 = insertp->next
  sw $t7, 0($t3)          # current->next = insertp->next

  lw  $t7, 4($a2)         # t7 = insertp->size
  add $t7, $t7, $t5       # t7 = insertp->size + current->size
  sw  $t7, 4($t3)         # current->size += insertp->size
  j _free_end

_free_else2:
sw $a2, 0($t3)            # current->next = insertp

_free_end:
sw $t3, _last_used        # last_used = current

lw $ra, 4($fp)
jr $ra
