.text
.globl fact
fact:
  addi sp, sp, -12
  sw ra, 8(sp)
  sw s0, 4(sp)
  addi s0, sp, 12
  sw a0, -8(s0)
  lw t0, -8(s0)
  li t1, 1
  bgt t0, t1, L_else2
  li t0, 1
  mv a0, t0
  j L_exit_fact1
L_else2:
  lw t0, -8(s0)
  addi sp, sp, -24
  sw t0, 0(sp)
  sw t1, 4(sp)
  sw t2, 8(sp)
  sw t3, 12(sp)
  sw t4, 16(sp)
  sw t5, 20(sp)
  lw t0, -8(s0)
  li t1, 1
  sub t0, t0, t1
  addi sp, sp, -4
  sw t0, 0(sp)
  call fact
  addi sp, sp, 4
  lw t0, 0(sp)
  lw t1, 4(sp)
  lw t2, 8(sp)
  lw t3, 12(sp)
  lw t4, 16(sp)
  lw t5, 20(sp)
  addi sp, sp, 24
  mv t1, a0
  mul t0, t0, t1
  mv a0, t0
  j L_exit_fact1
L_if_end3:
L_exit_fact1:
  addi sp, s0, -12
  lw ra, 8(sp)
  lw s0, 4(sp)
  addi sp, sp, 12
  ret
main:
  addi sp, sp, -8
  sw ra, 4(sp)
  sw s0, 0(sp)
  addi s0, sp, 8
  addi sp, sp, -24
  sw t0, 0(sp)
  sw t1, 4(sp)
  sw t2, 8(sp)
  sw t3, 12(sp)
  sw t4, 16(sp)
  sw t5, 20(sp)
  li t0, 5
  addi sp, sp, -4
  sw t0, 0(sp)
  call fact
  addi sp, sp, 4
  lw t0, 0(sp)
  lw t1, 4(sp)
  lw t2, 8(sp)
  lw t3, 12(sp)
  lw t4, 16(sp)
  lw t5, 20(sp)
  addi sp, sp, 24
  mv t0, a0
  mv a0, t0
  j L_exit_main4
L_exit_main4:
  addi sp, s0, -8
  lw ra, 4(sp)
  lw s0, 0(sp)
  addi sp, sp, 8
  ret
