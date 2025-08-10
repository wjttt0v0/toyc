main:
  addi sp, sp, -8
  sw ra, 4(sp)
  sw s0, 0(sp)
  mv s0, sp
  li t0, 0
  mv a0, t0
  j L_exit_main1
L_exit_main1:
  mv sp, s0
  lw ra, 4(sp)
  lw s0, 0(sp)
  addi sp, sp, 8
  ret

