// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
//
// This program only needs to handle arguments that satisfy
// R0 >= 0, R1 >= 0, and R0*R1 < 32768.

// Put your code here.

// load R0 into the iter variable
@R0
D=M
@iter
M=D

// set the acc variable to 0
@acc
M=0

(LOOP)
  @iter
  D=M
  @STOP
  D;JEQ // if iter is 0, go to END
  @iter
  M=M-1 // iter = iter - 1

  @R1 // The multiplicative factor
  D=M
  @acc
  M=D+M // acc = acc + factor

  @LOOP // restart the loop
  0;JMP

(STOP)
  @acc
  D=M
  @R2
  M=D // Load the value of acc into R2

(END)
  @END
  0;JMP
