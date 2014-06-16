; **********************************************
; ************* SoftROM EEPROM tool ************
; **********************************************
;
; Assemble with "The Black Smurf Assembler"
; freely available from https://github.com/Edilbert/BSA
;
; Copyright (c) 2014 Nils Eilers <nils.eilers@gmx.de>
; This work is free. You can redistribute it and/or modify it under the
; terms of the Do What The Fuck You Want to Public License, Version 2,
; as published by Sam Hocevar. See the COPYING file for more details.

; ************
; * Keycodes *
; ************

CR         =  13     ; Carriage Return
HOME       =  19     ; Home
DEL        =  20     ; Delete
CLR        = 147     ; Clear

COLUMNS    =  80     ; CBM 8000 series

Tracks     =  77     ; Tracks per side

; ****************************
; * Some Zero Page Variables *
; ****************************

BP        = $16      ; Buffer Pointer
BAMP      = $18      ; BAM    Pointer
TEMP      = $1a      ; Miscellenious
SP        = $1b      ; SCREEN Pointer

; ************************
; * CBM Kernal Variables *
; ************************

STATUS    = $96      ; Status byte
STKEY     = $9b      ; Stop key pressed?
BLNSW     = $a7      ; Blink switch
BLNCT     = $a8      ; Blink count
BLNON     = $aa      ; Blink On
FA        = $d4

; ********************************
; * Location for status messages *
; ********************************

Screen_Command = $8000 + 18 * 80

; *****************************************
; * Put the buffer after the program code *
; *****************************************

Buffer  = [EOP & $ff00] + $100

; ***************************************************
; * The CBM 8000 Kernal has no jump table for these *
; ***************************************************

TALK    = $f0d2
LISTEN  = $f0d5
SECOND  = $f143
TKSA    = $f193
CIOUT   = $f19e
UNTLK   = $f1ae
UNLSN   = $f1b9
ACPTR   = $f1c0
BSOUT   = $ffd2
GETIN   = $ffe4

; ***************
; * Print Macro *
; ***************

MACRO MAC_Print(lab)
         LDY #<lab
         LDA #>lab
         LDX #?lab
         JSR PrintText
ENDMAC

; **********************************
; * BASIC header for program start *
; **********************************

START   = $0401

* = START ; *** BASIC ***  CBM / PET series

          .LOAD START     ; LOAD address
          .STORE START,EOP-START,"softrom.prg"

Link      .WORD EndLink

; **********
  Linenumber
; **********

          .WORD 2014 

; **********
  SysCommand
; **********

          .BYTE $9e       ; SYS token 
StartML   .BYTE "(1065)"
          .BYTE ':',$8f   ; REM token
          .BYTE " SOFTROM EEPROM TOOL"
LineEnd   .BYTE 0 
EndLink   .WORD 0

          JMP Main


; ****
  STOP
; ****
          LDA STKEY
          CMP #$ef
          RTS

; **********
  Error_Beep
; **********

          LDA #7
          JMP BSOUT

; *********
  PrintText
; *********

          STY SP
          STA SP+1
          LDY #0
PrTe_10   LDA (SP),Y
          JSR BSOUT
          INY
          DEX
          BNE PrTe_10
          RTS

; ********************
  Flush_Keyboard_Queue
; ********************

          JSR GETIN
          BNE Flush_Keyboard_Queue
          RTS  



; ****
  Main
; ****

          JSR Detect_BASIC_version

EXIT      RTS
EOP       ; END-OF-PROGRAM



; ********************
  Detect_BASIC_version
; ********************
; TODO: detect BASIC version and patch vectors
          RTS
