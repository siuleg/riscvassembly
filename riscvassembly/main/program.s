	# Put your program into this file
	#
	# The "program" is a function that takes
	# no arguments and that does not return
	# anything.
	#
	# The program function does not need to
	# return. It may be a forever loop.
	# If the program function returns,
	# it gets called again (and again and again-again).
	#
	# Do not change the attributes of the
	# program function below.
	#
	# You may "declare" other functions
	# using the pattern stubbed out for
	# you below.
	#
	# You need to follow the ESP Risc-V ABI:
	#
	# * The Stack Pointer is in register sp.
	#   Every function needs to move the stack
	#   pointer up (to lower addresses) in the
	#   beginning of the function and move it
	#   down in the end. The move needs to make
	#   room for all temporary variables the function
	#   needs. The stack pointer may only be moved
	#   in multiples of 32 bytes for alignment purposes.
	# 
	# * The return address is put into register
	#   ra by the branch-and-link instruction call.
	#   The callee function needs to start by storing
	#   the return address on the stack and end by
	#   restoring the return address from the stack.
	#   The place needed to store the return address
	#   on the stack needs to be accounted for
	#   when moving the stack pointer.
	#
	# * A function returns by executing a relative jump
	#   instruction jr on the return address:
	#
	#   jr		ra
	#
	#   This can be abbreviated to
	#
	#   ret
	#
	#   but the abbreviation is less understandable.
	#
	# * In addition to the stack pointer, every function
	#   needs to maintain the frame pointer. The frame pointer
	#   sits in register s0. It is equal to the stack pointer
	#   of the calling function. See below for an example on
	#   how to maintain the frame pointer. The frame pointer
	#   gets saved on the stack, then set to the stack pointer
	#   of the calling function and finally retrieved from
	#   the stack again. The place needed to store the
	#   frame pointer needs to be accounted for when moving the
	#   stack pointer.
	#   
	# * When calling a function, the first arguments of the
	#   function are passed in registers a0 through a7. All
	#   other arguments are passed on the stack. That latter
	#   case is rare. Each register is 32 bit wide. 64 bit
	#   values are passed in even-odd register pairs. The even
	#   register contains the least significant bits (as Risc-V
	#   is little endian).
	#
	# * The value returned by a function is passed in register
	#   a0 when it is up to 32 bit wide. 64 bit return values
	#   use the even-odd register pair a0-a1.
	#
	# * Register x0 always contains the value 0.
	#
	# * A callee function can use the registers a0 through a7
	#   as temporaries, after having saved them on the stack
	#   if they are arguments that are still needed. None of
	#   the registers a0 through a7 are callee-saved though. This
	#   means a caller function cannot assume to find the
	#   same contents in these registers once a function it
	#   called returns.
	#
	# * A callee function can also use the temporary registers
	#   t0 through t6. They are caller-saved as well. This means
	#   that the calling function cannot assume to find
	#   the same contents in these registers once a function
	#   it called returns.
	#
	# * Students should not use the registers gp, tp, s0 (the
	#   frame pointer, other than saving it and restoring it),
	#   s1 and s2 through s11.
	#
	# * Besides register x0, which always contains the value 0,
	#   students should not use any register with name x<n>, like
	#   x1, x17 etc.
	#
	# * Not all Risc-V processors have floating-point units and
	#   floating-point instructions. Students should not use
	#   any floating-point registers and floating-point instructions.
	#
	# * Not all Risc-V processors have multiplication and division
	#   units. Students should not use any multiplication or
	#   division instructions.
	#
	# The following functions can be called for (very basic)
	# input and output on the UART simulated over USB:
	#
	# * The function
	#
	#   unsigned char get_character();
	#
	#   takes no arguments and returns one byte containing
	#   an ASCII character typed in by the user.
	#
	#   The function returns the byte in register a0, which
	#   is a 32 bit register. Only the 8 least significant
	#   bits of that byte are meaningful. The upper 24 bits
	#   can contain any value.
	#
	#   The function can be called with
	#
	#   call	get_character
	#
	# * The function
	#
	#   void put_character(unsigned char);
	#
	#   takes one byte in argument and returns nothing.
	#   It outputs the byte as an ASCII character on the screen.
	#
	#   The argument of the function is passed to put_character()
	#   in the lower 8 bits of register a0. The upper 24 bits of
	#   register a0 must be zero.
	#
	# * The two functions get_character() and put_character()
	#   of course abide by the ABI described above.
	#
	# The ESP32-C3 has no full operating system. Only one program
	# can run at the time. Nothing can be typed in or output if
	# a program does not call get_character() or put_character()
	# sufficiently often. On a full-grown computer with a real
	# operating system, this is different: the operating system can
	# still get user input and refresh the screen with output
	# while a program is computing.
	#
	# In order to avoid any issue with programs that get "stuck"
	# (in a long computation or just a buggy forever loop), the ESP32
	# hardware implements a watch-dog. This is special hardware that
	# reboots the microcontroller if none of the two functions
	# get_character() and put_character() has been called for 8 seconds.
	#
	# Students are advised to check for loops that run for too long
	# when they see their device rebooting without a warning.
	#
	# All address computations for the code segment are done by the
	# assembler. When writing assembly, we only use symbolic constants,
	# defined as labels.
	#
	# Any number of labels can be used in the program, but all labels
	# need to be unique in the assembly file. It is customary to
	# name labels functionname_l1, functionname_l2 and so on.
	# 
	#
	# The following Assembly (pseudo-) instructions are available on
	# Risc-V.
	#
	# In this list, the [m:n] notation is used, for example for [31:12].
	# This notation means: take the (m-n+1) bits numbered m through n, both sides
	# included. For example, [31:12] means: take the 20 bits numbered 31 through 12.
	#
	# In a 32 bit word, the least significant bit has number 0, the most significant
	# bit has number 31.
	#
	# In the list below, M stands for memory.
	#
	# The case of the instructions is not important. On Linux, lower-case instructions
	# are mostly used.
	#
	# For load and store operations, instead of writing for example
	#
	#  SW   sp,ra,28
	#
	# the mnemonic
	#
	#  SW   ra,28(sp)
	#
	#  is mostly used. It means: store ra to the address in the stack pointer, offset
	#  by 28 bytes.
	#
	#
	#  LB   rd,rs1,imm    Load Byte       rd <- M[rs1+imm][0:7]       sign-extended
	#  LH   rd,rs1,imm    Load Half-Word  rd <- M[rs1+imm][0:15]      sign-extended
	#  LW   rd,rs1,imm    Load Word       rd <- M[rs1+imm][0:31]
	#  LBU  rd,rs1,imm    Load Byte       rd <- M[rs1+imm][0:7]       zero-extended
	#  LHU  rd,rs1,imm    Load Half-Word  rd <- M[rs1+imm][0:15]      zero-extended
	#  SB   rs1,rs2,imm   Store Byte      M[rs1+imm][0:7] <- rs2[0:7]
	#  SH   rs1,rs2,imm   Store Half-Word M[rs1+imm][0:15] <- rs2[0:15]
	#  SW   rs1,rs2,imm   Store Word      M[rs1+imm][0:31] <- rs2[0:31]
	#  SLL  rd,rs1,rs2    Shift Left      rd <- rs1 << rs2
	#  SLLI rd,rs1,imm    Shift Left Imme rd <- rs1 << imm[0:4]
	#  SRL  rd,rs1,rs2    Shift Right     rd <- rs1 >> rs2            logical, unsigned
	#  SRLI rd,rs1,imm    Shift Right Imm rd <- rs1 >> imm[0:4]       logical, unsigned
	#  SRA  rd,rs1,rs2    Shift Right     rd <- rs1 >> rs2            arithmetical, signed
	#  SRAI rd,rs1,imm    Shift Right Imm rd <- rs1 >> imm[0:4]       arithmetical, signed
	#  ADD  rd,rs1,rs2    Addition        rd <- rs1 + rs2
	#  ADDI rd,rs1,imm    Addition Immedi rd <- rs1 + imm
	#  SUB  rd,rs1,rs2    Subtraction     rd <- rs1 - rs2
	#  LUI  rd,imm        Load-Upper Imme rd <- imm << 12
	#  AUIPC rd,imm       Add Up Im to PC rd <- PC + (imm << 12)
	#  XOR  rd,rs1,rs2    XOR             rd <- rs1 ^ rs2
	#  XORI rd,rs1,imm    XOR immediate   rd <- rs1 ^ imm
	#  OR   rd,rs1,rs2    OR              rd <- rs1 | rs2
	#  ORI  rd,rs1,imm    OR immediate    rd <- rs1 | imm
	#  AND  rd,rs1,rs2    AND             rd <- rs1 & rs2
	#  ANDI rd,rs1,imm    AND immediate   rd <- rs1 & imm
	#  SLT  rd,rs1,rs2    Set Less Than   rd <- (rs1 < rs2) ? 1 : 0   signed comparison
	#  SLTI rd,rs1,imm    Set LT Immedia  rd <- (rs1 < imm) ? 1 : 0   signed comparison
	#  SLTU rd,rs1,rs2    Set LT Unsigned rd <- (rs1 < rs2) ? 1 : 0   unsigned comparison
	#  SLTIU rd,rs1,imm   Set LT Imm Uns  rd <- (rs1 < imm) ? 1 : 0   unsigned comparison
	#  BEQ  rs1,rs2,imm   Branch Equal    if (rs1 == rs2) PC += imm else PC += 4
	#  BNE  rs1,rs2,imm   Branch Not Equ  if (rs1 != rs2) PC += imm else PC += 4
	#  BLT  rs1,rs2,imm   Branch LT       if (rs1 < rs2) PC += imm else PC += 4    signed comparison
	#  BGE  rs1,rs2,imm   Branch GE       if (rs1 >= rs2) PC += imm else PC += 4   signed comparison
	#  BLTU rs1,rs2,imm   Branch LT Unsi  if (rs1 < rs2) PC += imm else PC += 4    unsigned comparison
	#  BGEU rs1,rs2,imm   Branch GE Unsi  if (rs1 >= rs2) PC += imm else PC += 4   unsigned comparison
	#  JAL  rd,imm        Jump and Link   rd <- PC+4 then PC += imm
	#  JALR rd,rs1,imm    Jump & Link Reg rd <- PC+4 then PC = rs1 + imm
	#
	#  LA   rd,symbol     Load Address    Pseudo-instruction   AUIPC rd, symbol[31:12]
	#                                                          ADDI  rd, rd, symbol[11:0]
	#  NOP                No op           Pseudo-instruction   ADDI x0, x0, 0
	#  LI   rd,imm        Load immediate  Pseudo-instruction   ADDI rd, x0, imm
	#  MV   rd,rs         Move register   Pseudo-instruction   ADDI rd, rs, imm
	#  NOT  rd,rs         Logical not     Pseudo-instruction   XORI rd, rs, -1
	#  NEG  rd,rs         Negate          Pseudo-instruction   SUB  rd, x0, rs
	#  J    offset        Jump            Pseudo-instruction   JAL x0, offset
	#  JR   rs            Jump Register   Pseudo-instruction   JALR x0, rs, 0
	#  RET                Return          Pseudo-instruction   JALR x0, x1, 0    x1 = ra
	#  CALL offset        Call            Pseudo-instruction   AUIPC x1, offset[31:12]
	#                                                          JALR  x1, x1, offset[11:0]
	#
	#  
	#
	#
	.option nopic              # We do not write PIC code
	.attribute arch, "rv32i2p0_m2p0_a2p0_f2p0_d2p0_c2p0" # Requirements for our code
	.attribute unaligned_access, 0  # We do not use unaligned accesses 
	.attribute stack_align, 16      # We align the stack
	.text                       # This is where the program starts
	.align	1                   # Align this function
	.globl	program             # Label program is a globally callable function
	.type	program, @function  # Label program is a function
program:                            # Label for the program function. The function starts here
	addi	sp,sp,-32	    # Move stack pointer up (to lower addresses)
	sw	ra,28(sp)           # Store return address on stack
	sw	s0,24(sp)           # Store frame pointer on stack
	addi	s0,sp,32            # Set frame pointer to stack pointer of caller

	# Your code goes here
	
	lw	ra,28(sp)           # Restore return address
	lw	s0,24(sp)           # Restore frame pointer
	addi	sp,sp,32            # Move stack pointer down (to higher addresses)
	jr	ra                  # Return from function
	.size	program, .-program  # Indicate what the size of the function is 


increment:                          # Example function increment   int increment(int x) { return x + 1 ; }
	addi	sp,sp,-32
	sw	ra,28(sp)
	sw	s0,24(sp)
	addi	s0,sp,32
	addi	a0,a0,1
	lw	ra,28(sp)
	lw	s0,24(sp)
	addi	sp,sp,32
	jr	ra
	.size	increment, .-increment

	
