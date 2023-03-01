// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed.
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

@color // sets A to color (some register the assembler picks for us), sets M to RAM[color]
M=-1   // RAM[color] = -1

(INPUT)
  @color
  D=M
  @NONZERO
  D;JNE
  @ZERO
  D;JEQ

(NONZERO)
  @KBD   // sets A to KBD (a register number), M to RAM[KBD]
  D=M    // copies RAM[KBD] to D
  @FILL  // sets A to FILL (address of label)
  D;JNE  // evaluate D; jump if D is not equal to zero
  @NONZERO
  0;JMP

(ZERO)
  @KBD   // sets A to KBD (a register number), M to RAM[KBD]
  D=M    // copies RAM[KBD] to D
  @FILL  // sets A to FILL (address of label)
  D;JEQ  // evaluate D; jump if D is equal to zero
  @ZERO
  0;JMP

(FILL)
  @iter
  M=0   // iter = 0

(LOOP)
  @iter
  D=M   // load iter into D

  @8192
  D=D-A // D = iter - 8192

  @SWAP
  D;JEQ // if D = 0, jump to SWAP

  @iter
  D=M   // D = iter

  @SCREEN
  D=A+D // D = address of next screen register to paint

  @currscreen
  M=D   // currscreen = address of next screen register to paint

  @color
  D=M   // D = color

  @currscreen
  A=M   // A = currscreen
  M=D   // memory[currscreen] = color

  @iter
  M=M+1 // increment iter

  @LOOP
  0;JMP

(SWAP)
  @color
  M=!M
  @INPUT
  0;JMP
