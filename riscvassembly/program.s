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
        #   jr          ra
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
        #   call        get_character
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
        .align  1                   # Align this function
        .globl  program             # Label program is a globally callable function
        .type   program, @function  # Label program is a function
program:                            # Label for the program function. The function starts here
        addi    sp,sp,-64           # Move stack pointer up (to lower addresses)
        sw      ra,60(sp)           # Store return address on stack
        sw      s0,56(sp)           # Store frame pointer on stack
        addi    s0,sp,64            # Set frame pointer to stack pointer of caller

	li	a0,47806
	call	print_hexadecimal
	li	a0,'\n'
	call	put_character
	
        lw      ra,60(sp)           # Restore return address
        lw      s0,56(sp)           # Restore frame pointer
        addi    sp,sp,64            # Move stack pointer down (to higher addresses)
        jr      ra                  # Return from function
        .size   program, .-program  # Indicate what the size of the function is

	
multiply_unsigned:                  # Multiply register a0 with a1, put result in a0
        addi    sp,sp,-32
        sw      ra,28(sp)
        sw      s0,24(sp)
        addi    s0,sp,32
	mv	a2,x0               # a2 is temporary for the result, we kill the arguments
	mv	a3,x0		    # a3 is temporary for the counter i
multiply_unsigned_loop:
	slti	a4,a3,32	    # a4 = (i < 32)
	beq	a4,x0,multiply_unsigned_done
	andi	a4,a0,1
	beq	a4,x0,multiply_unsigned_after_if
	add	a2,a2,a1
multiply_unsigned_after_if:
	srli	a0,a0,1
	slli	a1,a1,1
	addi	a3,a3,1
	j	multiply_unsigned_loop
multiply_unsigned_done:
	mv	a0,a2
        lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   multiply_unsigned, .-multiply_unsigned


multiply_signed:                    # Multiply register a0 with a1, put result in a0
        addi    sp,sp,-32
        sw      ra,28(sp)
        sw      s0,24(sp)
        addi    s0,sp,32
	bge	a0,x0,multiply_signed_if_one_else
	neg	a2,a0
	li	a4,1
	j	multiply_signed_if_one_done
multiply_signed_if_one_else:
	mv	a2,a0
	mv	a4,x0
multiply_signed_if_one_done:
	bge	a1,x0,multiply_signed_if_two_else
	neg	a3,a1
	li	a5,1
	j	multiply_signed_if_two_done
multiply_signed_if_two_else:
	mv	a3,a1
	mv	a5,x0
multiply_signed_if_two_done:
	mv	a0,a2
	mv	a1,a3
	sw      a4,20(sp)
	sw      a5,16(sp)
	call	multiply_unsigned
	lw      a4,20(sp)
	lw      a5,16(sp)
	xor	a4,a4,a5
	beq	a4,x0,multiply_signed_if_three_done
	neg	a0,a0
multiply_signed_if_three_done:
        lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   multiply_signed, .-multiply_signed


divide_unsigned:                    # a0 is a pointer to quotient, a1 a pointer to remainder	      
        addi    sp,sp,-32           # Divide a2 by a3, put result in variables pointed to by a0 and a1
        sw      ra,28(sp)           # Does not return anything
        sw      s0,24(sp)
        addi    s0,sp,32
	bne	a3,x0,divide_unsigned_if_one_done
	mv	a4,x0
	mv	a5,a2
	j	divide_unsigned_done
divide_unsigned_if_one_done:
	mv	a4,x0
	mv	a5,x0
	li	a6,32
divide_unsigned_loop:
	beq	a6,x0,divide_unsigned_done
	addi	a6,a6,-1
	slli	a5,a5,1
	mv	a7,a2
	srl	a7,a7,a6
	andi	a7,a7,1
	or	a5,a5,a7
	blt	a5,a3,divide_unsigned_loop
	sub	a5,a5,a3
	li	a7,1
	sll	a7,a7,a6
	or	a4,a4,a7
	j	divide_unsigned_loop
divide_unsigned_done:
	sw	a4,0(a0)
	sw	a5,0(a1)
        lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   divide_unsigned, .-divide_unsigned


divide_signed:                      # a0 is a pointer to quotient, a1 a pointer to remainder	      
        addi    sp,sp,-32	    # Divide a2 by a3, put result in variables pointed to by a0 and a1
        sw      ra,28(sp)           # Does not return anything
        sw      s0,24(sp)
        addi    s0,sp,32
	bge	a2,x0,divide_signed_if_one_else
	neg	a2,a2
	li	a4,1
	j	divide_signed_if_one_done
divide_signed_if_one_else:
	mv	a4,x0
divide_signed_if_one_done:
	bge	a3,x0,divide_signed_if_two_else
	neg	a3,a3
	li	a5,1
	j	divide_signed_if_two_done
divide_signed_if_two_else:
	mv	a5,x0
divide_signed_if_two_done:
	xor	a4,a4,a5
	sw	a4,20(sp)
	call	divide_unsigned
	lw	a4,20(sp)
	beq	a4,x0,divide_signed_done
	lw	a4,0(a0)
	neg	a4,a4
	sw	a4,0(a0)
	lw	a4,0(a1)
	neg	a4,a4
	sw	a4,0(a1)
divide_signed_done:	
        lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   divide_signed, .-divide_signed



print_string:                       # a0 is a pointer to a string. Print that string.
        addi    sp,sp,-32           # Does not return anything
        sw      ra,28(sp)
        sw      s0,24(sp)
        addi    s0,sp,32 
	sw	a0,20(sp)
print_string_loop:
	lw	a0,20(sp)
	lb	a1,0(a0)
	beq	a1,x0,print_string_end
	mv	a0,a1
	call	put_character
	lw	a0,20(sp)
	addi	a0,a0,1
	sw	a0,20(sp)
	j	print_string_loop
print_string_end:	
        lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   print_string, .-print_string

print_hexadecimal:                  # Print the unsigned value in a0
	addi    sp,sp,-32           # as a hexadecimal number with
	sw      ra,28(sp)           # leading 0x and leading zeros
	sw      s0,24(sp)
	addi    s0,sp,32
	sw	a0,20(sp)
	li	a0, '0'
	call	put_character
	li	a0, 'x'
	call	put_character
	li	a1, 32
print_hexadecimal_loop:	
	li	a2, 4
	blt	a1,a2,print_hexadecimal_done
	addi	a1,a1,-4
	lw	a0,20(sp)
	srl	a0,a0,a1
	andi	a0,a0,15
	li	a3,10
	blt	a0,a3,print_hexadecimal_low
	li	a4,'A'
	addi	a0,a0,-10
	j	print_hexadecimal_end_if
print_hexadecimal_low:
	li	a4,'0'
print_hexadecimal_end_if:
	add	a0,a0,a4
	sw	a1,16(sp)
	call	put_character
	lw	a1,16(sp)
	j	print_hexadecimal_loop
print_hexadecimal_done:	
        lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   print_hexadecimal, .-print_hexadecimal

	

print_unsigned:                     # Print the unsigned value in a0
        addi    sp,sp,-64           # Does not return anything
        sw      ra,60(sp)
        sw      s0,56(sp)
        addi    s0,sp,64
	bne	a0,x0,print_unsigned_if_one_done
	li	a0,'0'
	call	put_character
	j	print_unsigned_done
print_unsigned_if_one_done:
	mv	a1,x0
print_unsigned_while_one_loop:	
	beq	a0,x0,print_unsigned_while_one_done
	sw	a1,52(sp)
	mv	a2,a0
	li	a3,10
	addi	a0,sp,48
	addi	a1,sp,44
	call	divide_unsigned
	lw	a1,52(sp)
	lw	a0,48(sp)
	lw	a2,44(sp)
	add	a3,sp,a1
	sb	a2,20(a3)
	addi	a1,a1,1
	j	print_unsigned_while_one_loop
print_unsigned_while_one_done:
	beq	a1,x0,print_unsigned_done
	addi	a1,a1,-1
	add	a0,sp,a1
	lb	a0,20(a0)
	addi	a0,a0,'0'
	sw	a1,52(sp)
	call	put_character
	lw	a1,52(sp)
	j	print_unsigned_while_one_done
print_unsigned_done:	
        lw      ra,60(sp)
        lw      s0,56(sp)
        addi    sp,sp,64
        jr      ra
        .size   print_unsigned, .-print_unsigned


print_signed:                       # Print the signed value in a0
        addi    sp,sp,-32           # Does not return anything
        sw      ra,28(sp)
        sw      s0,24(sp)
        addi    s0,sp,32
	bge	a0,x0,print_signed_if_done
	neg	a0,a0
	sw	a0,20(sp)
	li	a0,'-'
	call	put_character
	lw	a0,20(sp)
print_signed_if_done:
	call	print_unsigned
        lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   print_signed, .-print_signed


input_string:                       # a0 is a pointer to a string, a1 is a maximum length
        addi    sp,sp,-32           # get a string from the user and put it into the string
        sw      ra,28(sp)           # if a1 is at least 1, always put '\0' at the end
        sw      s0,24(sp)           # does not return anything
        addi    s0,sp,32
	beq	a1,x0,input_string_done
	li	a2,1
	bne	a1,a2,input_string_main
	sb	x0,0(a0)
	j	input_string_done
input_string_main:
	addi	a1,a1,-1
	mv	a2,x0
input_string_loop:
	bge	a2,a1,input_string_loop_done
	sw	a0,20(sp)
	sw	a1,16(sp)
	sw	a2,12(sp)
	call	get_character
	mv	a3,a0
	sw	a3,8(sp)
	call	put_character
	lw	a3,8(sp)
	lw	a2,12(sp)
	lw	a1,16(sp)
	lw	a0,20(sp)
	andi	a3,a3,255
	bne	a3,x0,input_string_if_one_else
	addi	a2,a2,-1
	j	input_string_if_one_done
input_string_if_one_else:
	li	a4,'\n'
	beq	a3,a4,input_string_loop_done
	add	a4,a0,a2
	sb	a3,0(a4)
input_string_if_one_done:
	addi	a2,a2,1
	j	input_string_loop
input_string_loop_done:	
	add	a4,a0,a2
	sb	x0,0(a4)
input_string_done:	
        lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   input_string, .-input_string


input_unsigned:                     # Does not take any arguments
        addi    sp,sp,-32           # Get an unsigned integer from the user, put it in a0
        sw      ra,28(sp)           # Return 0 if the user entered something invalid
        sw      s0,24(sp)
        addi    s0,sp,32
	mv	a0,x0
input_unsigned_loop:	
	sw	a0,20(sp)
	call	get_character
	mv	a1,a0
	sw	a1,16(sp)
	call	put_character
	lw	a1,16(sp)
	lw	a0,20(sp)
	andi	a1,a1,255
	beq	a1,x0,input_unsigned_loop
	li	a2,'\n'
	beq	a1,a2,input_unsigned_done
	li	a2,'0'
	blt	a1,a2,input_unsigned_invalid
	li	a2,'9'
	blt	a2,a1,input_unsigned_invalid
	li	a2,'0'
	sub	a1,a1,a2
	sw	a1,16(sp)
	sw	a0,20(sp)
	li	a1,10
	call	multiply_unsigned
	mv	a2,a0
	lw	a0,20(sp)
	lw	a1,16(sp)
	add	a0,a2,a1
	j	input_unsigned_loop
input_unsigned_invalid:	
	mv	a0,x0
input_unsigned_done:	
        lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   input_unsigned, .-input_unsigned


input_signed:                       # Does not take any arguments
        addi    sp,sp,-32           # Get a signed integer from the user, put it in a0
        sw      ra,28(sp)           # Return 0 if the user entered something invalid
        sw      s0,24(sp)
        addi    s0,sp,32
	mv	a0,x0
	mv	a3,x0
	li	a4,1
input_signed_loop:	
	sw	a0,20(sp)
	sw	a3,12(sp)
	sw	a4,8(sp)
	call	get_character
	mv	a1,a0
	sw	a1,16(sp)
	call	put_character
	lw	a4,8(sp)
	lw	a3,12(sp)
	lw	a1,16(sp)
	lw	a0,20(sp)
	andi	a1,a1,255
	beq	a1,x0,input_signed_loop
	li	a2,'\n'
	beq	a1,a2,input_signed_done
	beq	a4,x0,input_signed_not_first
	mv	a4,x0
	li	a2,'-'
	bne	a1,a2,input_signed_not_first
	li	a3,1
	j	input_signed_loop
input_signed_not_first:	
	li	a2,'0'
	blt	a1,a2,input_signed_invalid
	li	a2,'9'
	blt	a2,a1,input_signed_invalid
	li	a2,'0'
	sub	a1,a1,a2
	sw	a3,12(sp)
	sw	a4,8(sp)
	sw	a1,16(sp)
	sw	a0,20(sp)
	li	a1,10
	call	multiply_signed
	mv	a2,a0
	lw	a0,20(sp)
	lw	a1,16(sp)
	lw	a3,12(sp)
	lw	a4,8(sp)
	add	a0,a2,a1
	j	input_signed_loop
input_signed_invalid:	
	mv	a0,x0
input_signed_done:
	beq	a3,x0,input_signed_outta_here
	neg	a0,a0
input_signed_outta_here:
        lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   input_signed, .-input_signed


search:                             # a0 is a base pointer to an integer array
        addi    sp,sp,-32           # a1 is the size of the array
        sw      ra,28(sp)           # a2 is the element we are looking for
        sw      s0,24(sp)           # a0 contains the index of the element equal to a2 or -1 if not found
        addi    s0,sp,32
	mv	a3,a0               # saved base address of array
	li	a0,-1               # result if we dont find anything
	mv	a4,x0               # a4 is i
search_loop:
	bge	a4,a1,search_loop_done
	slli	a5,a4,2             # i * 4 because 4 bytes in an integer
	add	a5,a3,a5            # a3 + i * 4 is address of i-th element
	lw	a6,0(a5)            # load the i-th element from memory
	bne	a6,a2,search_loop_not_found
	mv	a0,a4
	j	search_loop_done
search_loop_not_found:	
	addi	a4,a4,1
	j	search_loop
search_loop_done:	
	lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   search, .-search

copy_integer_array:                 # Copies all elements of an integer array into another
        addi    sp,sp,-32           # integer array
        sw      ra,28(sp)           # a0 is base address of the destination
        sw      s0,24(sp)           # a1 is base address of the source
        addi    s0,sp,32            # a2 is size
	mv	a3,x0               # i : a3
copy_integer_array_loop:
	bge	a3,a2,copy_integer_array_loop_done
	lw	a4,0(a1)
	sw	a4,0(a0)
	addi	a1,a1,4
	addi	a0,a0,4
	addi	a3,a3,1
	j	copy_integer_array_loop
copy_integer_array_loop_done:	
	lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   copy_integer_array, .-copy_integer_array

	
merge_sorted_integer_arrays:        # Merges two integer arrays sorted in ascending order
        addi    sp,sp,-32           # into one integer array sorted in ascending order
        sw      ra,28(sp)           # a0 is base address of the destination
        sw      s0,24(sp)           # a1 is base address of first array, a2 its size
        addi    s0,sp,32            # a3 is base address of second array, a4 its size
	mv	a5,x0               # i : a5
	mv	a6,x0               # j : a6
	mv	a7,x0               # k : a7
merge_sorted_integer_arrays_loop:
	bge	a5,a2,merge_sorted_integer_arrays_loop_done
	bge	a6,a4,merge_sorted_integer_arrays_loop_done
	sw	a2,20(sp)
	sw	a4,16(sp)
	slli	a2,a5,2
	add	a2,a1,a2
	lw	a2,0(a2)            # u
	slli	a4,a6,2
	add	a4,a3,a6
	lw	a4,0(a4)            # v
	bge	a2,a4,merge_sorted_integer_arrays_if_else_else
	slli	a4,a7,2
	add	a4,a0,a4
	sw	a2,0(a4)
	addi	a5,a5,1
	j	merge_sorted_integer_arrays_if_else_done
merge_sorted_integer_arrays_if_else_else:
	slli	a2,a7,2
	add	a2,a0,a2
	sw	a4,0(a2)
	addi	a6,a6,1
merge_sorted_integer_arrays_if_else_done:	
	addi	a7,a7,1
	lw	a4,16(sp)
	lw	a2,20(sp)
	j	merge_sorted_integer_arrays_loop
merge_sorted_integer_arrays_loop_done:	
	bge	a5,a2,merge_sorted_integer_arrays_loop_two_done
	sw	a2,20(sp)         # Make room, save a2 onto stack
	sw	a4,16(sp)         # Make room, save a4 onto stack
	slli	a2,a5,2           # Multiply i by 4
	add	a2,a1,a2          # Add offset to base address
	lw	a2,0(a2)          # Load a[i]
	slli	a4,a7,2           # Multiply k by 4
	add	a4,a0,a4          # Add offset to base address
	sw	a2,0(a4)          # Store to dst[k]
	lw	a4,16(sp)         # Recover a4 from stack
	lw	a2,20(sp)	  # Recover a2 from stack
	addi	a5,a5,1           # i++
	addi	a7,a7,1           # k++
	j	merge_sorted_integer_arrays_loop_done
merge_sorted_integer_arrays_loop_two_done:
	bge	a6,a4,merge_sorted_integer_arrays_loop_three_done
	sw	a2,20(sp)         # Make room, save a2 onto stack
	sw	a4,16(sp)         # Make room, save a4 onto stack
	slli	a2,a6,2           # Multiply j by 4
	add	a2,a3,a2          # Add offset to base address
	lw	a2,0(a2)          # Load b[j]
	slli	a4,a7,2           # Multiply k by 4
	add	a4,a0,a4          # Add offset to base address
	sw	a2,0(a4)          # Store to dst[k]
	lw	a4,16(sp)         # Recover a4 from stack
	lw	a2,20(sp)	  # Recover a2 from stack
	addi	a6,a6,1           # j++
	addi	a7,a7,1           # k++
	j	merge_sorted_integer_arrays_loop_two_done
merge_sorted_integer_arrays_loop_three_done:	
	lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   merge_sorted_integer_arrays, .-merge_sorted_integer_arrays
	

merge_sort_integer_array:           # Sorts an integer array in ascending order 
        addi    sp,sp,-32           # using merge sort
        sw      ra,28(sp)           # a0 is base address of the array, a1 its size
        sw      s0,24(sp)           
        addi    s0,sp,32	
	blt	a1,x0,merge_sort_integer_array_outta_here
	beq	a1,x0,merge_sort_integer_array_outta_here
	li	a2,1
	beq	a1,a2,merge_sort_integer_array_outta_here
	srai	a2,a1,1             # a2 : left_n
	sub	a3,a1,a2            # a3 : right_n
	sw	a0,20(sp)           # Live: a0, a1, a2, a3
	sw	a1,16(sp)
	sw	a2,12(sp)
	sw	a3,8(sp)
	mv	a1,a2
	call	merge_sort_integer_array
	lw	a3,8(sp)
	lw	a2,12(sp)
	lw	a1,16(sp)
	lw	a0,20(sp)
	slli	a4,a2,2
	add	a4,a0,a4
	sw	a0,20(sp)           # Live: a0, a1, a2, a3, a4
	sw	a1,16(sp)           # a0: base address full array, a1 its size
	sw	a2,12(sp)           # a2: left_n, a3: right_n
	sw	a3,8(sp)            # a4: base address right array
	sw	a4,4(sp)
	mv	a0,a4
	mv	a1,a3
	call	merge_sort_integer_array
	lw	a4,4(sp)
	lw	a3,8(sp)
	lw	a2,12(sp)
	lw	a1,16(sp)
	lw	a0,20(sp)
	mv	a5,a1               # Compute 32 * ceil((4 * n) / 32)
	slli	a5,a5,2             # Rounding up required for stack alignment
	addi	a5,a5,31
	srai	a5,a5,5
	slli	a5,a5,5
	sw	sp,-32(s0)
	sub	sp,sp,a5
	sw	a0,-28(s0)
	sw	a1,-24(s0)
	mv 	a1,a0
	mv	a0,sp
	mv	a5,a3
	mv	a3,a4
	mv	a4,a5
	call	merge_sorted_integer_arrays
	lw	a0,-28(s0)
	lw	a1,-24(s0)
	mv	a2,a1
	mv	a1,sp
	call	copy_integer_array
	lw	sp,-32(s0)
merge_sort_integer_array_outta_here:
	lw      ra,28(sp)
        lw      s0,24(sp)
        addi    sp,sp,32
        jr      ra
        .size   merge_sort_integer_array, .-merge_sort_integer_array

	
		
	# Put all strings in the end of the program in order not to mess with the alignment of the functions
str_enter_the:
	.asciz "Please enter the "
	.size   str_enter_the, .-str_enter_the

str_enter_th_integer:
	.asciz "-th integer: "
	.size   str_enter_th_integer, .-str_enter_th_integer

str_the_unsorted_array_is:
	.asciz "The unsorted array is:\n"
	.size   str_the_unsorted_array_is, .-str_the_unsorted_array_is

str_the_sorted_array_is:
	.asciz "The sorted array is:\n"
	.size   str_the_sorted_array_is, .-str_the_sorted_array_is
	
str_newline:
	.asciz "\n"
	.size   str_newline, .-str_newline
	
