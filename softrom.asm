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

TAB        =   9     ; Tab
CR         =  13     ; Carriage Return
HOME       =  19     ; Home
DEL        =  20     ; Delete
CLR        = 147     ; Clear
CDOWN      =  17     ; Cursor Down
CRIGHT     =  29     ; Cursor Right
CUP        = 145     ; Cursor Up
CLEFT      = 157     ; Cursor Left

; ****************
; * Screen codes *
; ****************

C8296 = 0

#if C8296
HOR_BAR    = 102     ; -
VER_BAR    =  97     ; |
UL         = 104     ; upper left
UR         = 106     ; upper right
LL         =  98     ; lower left
LR         = 100     ; lower right
TL         = 101     ; T left
TR         = 103     ; T right
TU         = 105     ; T upper
TB         =  99     ; T bottom

#else

HOR_BAR    =  64     ; -
VER_BAR    =  93     ; |
UL         = 112     ; upper left
UR         = 110     ; upper right
LL         = 109     ; lower left
LR         = 125     ; lower right
TL         = 107     ; T left
TR         = 115     ; T right
#endif

COLUMNS    =  80     ; CBM 8000 series
ROWS       =  25

Tracks     =  77     ; Tracks per side

; ****************************
; * Some Zero Page Variables *
; ****************************

BP        = $16      ; Buffer Pointer
STP       = $18      ; String Pointer
TEMP      = $1a      ; Miscellenious
SP        = $1b      ; SCREEN Pointer
FNLEN     = $d1      ; Length of Filename
SA        = $d3      ; Secondary Address
FA        = $d4      ; First Address
FNADR     = $da      ; Pointer to Filename

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

Screen = $8000

; *****************************************
; * Put the buffer after the program code *
; *****************************************

Buffer  = [EOP & $ff00] + $100
PosDiskName =  8
PosDiskID   = 26

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

MACRO MAC_Plot(Row,Col,Char)
          LDA #Char
          LDX #Row
          LDY #Col
          JSR PlotAt
ENDMAC 

MACRO MAC_PutString(Row,Col,Text)
          LDX #Row
          LDY #Col
          JSR GotoXY
          LDX #<Text
          LDY #>Text
          JSR PutString
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
          .BYTE " SOFTROM EEPROM TOOL 0.1"
LineEnd   .BYTE 0 
EndLink   .WORD 0

          JMP Main

NUMBER    .BYTE "00000 "
UnitText  .BYTE 'unit:',0
DriveText .BYTE 'drive:',0
Cols      .BYTE 80
CursorRow .BYTE  0
CursorCol .BYTE  0
Entries   .BYTE  0
FirstLine .BYTE  0
LastLine  .BYTE 42
Pages     .BYTE  2
ScreenLo  .FILL 25 (0)
ScreenHi  .FILL 25 (0)
EntryLo   .FILL 42 (0)
EntryHi   .FILL 42 (0)
Page      .BYTE  0
Offset    .BYTE  0
Unit      .BYTE  8
Drive     .BYTE  '0'
LV0       .BYTE  0
LV1       .BYTE  0
Reverse   .BYTE  0
Select    .BYTE  2
FirstFile .BYTE  0  ; First file on display
SOD       .WORD  0  ; Start Of Directory
EOD       .WORD  0  ; End Of Directory

DiskStatus = $8000 + 121


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

; ***********
  FormatByte
; ***********

; Convert binary number in (A) to
; three decimal digits in (Y),(X) and (A)

          LDY #'0'      ; 100
          LDX #'0'-1    ;  10
          SEC  
asts_01   INX  
          SBC #10
          BCS asts_01
          ADC #$3a 
          CPX #$3a 
          BCC asts_rt  ; X < 10
          PHA  
          TXA  
          SBC #10      ; X -= 10
          TAX  
          PLA  
          INY          ; Y = 1
asts_rt   RTS  


; **************
  FormatInteger
; **************

          LDY #$2f           ; X = low  byte
          SEC                ; A = high byte
FORINT_01 INY
          STX LV0
          STA LV1
          TXA  
          SBC #<10000
          TAX  
          LDA LV1
          SBC #>10000
          BCS FORINT_01
          STY NUMBER
          LDX LV0
          LDA LV1
          LDY #$2f 
          SEC  
FORINT_02 INY
          STX LV0
          STA LV1
          TXA  
          SBC #<1000
          TAX  
          LDA LV1
          SBC #>1000
          BCS FORINT_02
          STY NUMBER+1
          LDX LV0
          LDA LV1
          LDY #$2f 
          SEC  
FORINT_03 INY
          STX LV0
          STA LV1
          TXA  
          SBC #100 
          TAX  
          LDA LV1
          SBC #0
          BCS FORINT_03
          STY NUMBER+2
          LDA LV0
          LDY #$2f 
          SEC  
FORINT_04 INY
          SBC #10
          BCS FORINT_04
          STY NUMBER+3
          ADC #$3a 
          STA NUMBER+4
          LDX #0
          LDA #' ' 
FORINT_05 LDY NUMBER,X
          CPY #'0' 
          BNE FORINT_06
          STA NUMBER,X
          INX  
          CPX #4
          BCC FORINT_05
FORINT_06 RTS


; ********
  ShowUnit
; ********

          LDX #0
          LDY Select
          BNE ShUn10
          LDX #$80
ShUn10    STX Reverse
          LDX #1
          LDY #6
          JSR GotoXY
          LDA Unit
          JSR FormatByte
          CPX #'0'
          BEQ ShUn20
          PHA
          TXA
          JSR PutCharR
          PLA
ShUn20    JSR PutCharR
          LDA #' '
          JMP PutChar
          
; *********
  ShowDrive
; *********

          LDX #0
          LDY Select
          DEY
          BNE ShDr10
          LDX #$80
ShDr10    STX Reverse
          LDA Drive
          LDX #1
          LDY #15
          JMP PlotAtR


; ************
  ShowDiskName
; ************

          LDX #1
          LDY #18
          JSR GotoXY
          LDA #' '
          STA Buffer + PosDiskName + 16 ; delete quote
          LDA #0
          STA Buffer + PosDiskName + 20 ; mark string end
          LDX #<[Buffer + PosDiskName]
          LDY #>[Buffer + PosDiskName]
          JMP PutString

; **************
  FormatFilename
; **************

          LDA BP
          STA STP
          LDA BP+1
          STA STP+1
          LDY #2
          LDA (BP),Y      ; Blocks low
          TAX
          INY
          LDA (BP),Y      ; Blocks high
          JSR FormatInteger
          LDY #0
FoFi10    LDA NUMBER+2,Y
          STA (BP),Y
          INY
          CPY #4
          BCC FoFi10
          LDA (BP),Y
          CMP #' '
          BNE FoFi99
FoFi20    INC STP
          LDA (STP),Y
          CMP #' '
          BEQ FoFi20
          CMP #$22
          BEQ FoFi20
          STA (BP),Y
FoFi40    INY
          CPY #$20
          BCS FoFi99
FoFi50    LDA (STP),Y
          STA (BP),Y
          BNE FoFi40
          INC Entries
FoFi99    RTS
          

; *************
  FormatEntries
; *************

          LDA #0
          STA Entries
          LDA #$20        ; 1st. file
          STA BP
          LDA #>Buffer
          STA BP+1
FoEn10    LDY #4          ; after link and #
          LDA (BP),Y
          CMP #' '        ; for filename must be blank
          BNE FoEn99
          JSR FormatFilename
          JSR GetLoadAddress
          LDX Entries
          DEX
          JSR ShowEntry
          CLC
          LDA BP
          ADC #$20
          STA BP
          BCC FoEn20
          INC BP+1
FoEn20    LDA EOD         ; check for End Of Directory
          CMP BP
          LDA EOD+1
          SBC BP+1
          BCS FoEn10         
FoEn99    RTS


; *******
  PET2SCR
; *******

          CMP #$41
          BCC P2C99
          CMP #$5b
          BCS P2C10
          AND #$1f    ; a - z
          RTS
P2C10     CMP #$c1
          BCC P2C99
          CMP #$ca
          BCS P2C99
          AND #$7f    ; A - Z
P2C99     RTS

; *********
  ShowEntry
; *********

          CPX LastLine
          BCS ShEn99
          LDY #0              ; not selected
          TXA                 ; entry # in (X)
          CLC
          ADC #2
          CMP Select
          BNE ShEn05
          LDY #$80            ; selected
ShEn05    STY Reverse
          LDA EntryLo,X
          STA SP
          LDA EntryHi,X
          STA SP+1
          LDY #0
ShEn10    LDA (BP),Y
          BEQ ShEn99
          CMP #$22            ; hide quote
          BNE ShEn20
          LDA #' '
ShEn20    JSR PET2SCR
          ORA Reverse
          STA (SP),Y
          INY
          CPY #32
          BCC ShEn10
ShEn99    RTS


; ***********
  ShowEntries
; ***********

          LDA SOD
          STA BP
          LDA SOD+1
          STA BP+1
          LDX #0
ShoE10    JSR ShowEntry
          CLC
          LDA BP
          ADC #$20
          STA BP
          BCC ShoE20
          INC BP+1
ShoE20    INX
          CPX LastLine
          BCS ShoE99
          TXA
          ADC FirstLine  ; Carry is clear 
          CMP Entries
          BCC ShoE10
ShoE99    RTS


; ***********
  RightSelect
; ***********

          LDA Select
          BEQ IncSelect  ; Unit -> Drive
          CMP #2
          BCC RiSe99     ; Drive
          CLC
          ADC #19
          CMP Entries
          BCS RiSe99
          ADC #2
          STA Select
RiSe99    RTS
          


; **********
  LeftSelect
; **********

          LDA Select
          CMP #1
          BEQ DecSelect  ; Unit <- Drive
          CMP #23
          BCC LeSe99
          SBC #21
          STA Select
LeSe99    RTS
          
          
; *********
  IncSelect
; *********

          LDX Select
          BEQ InSe30          ; Unit
          DEX
          BEQ InSe30          ; Drive
          CPX LastLine
          BCC InSe10
          CLC                 ; Scroll display
          LDA FirstLine
          ADC LastLine
          CMP Entries
          BCS InSe20          ; At end alreay
          INC FirstLine
          LDA SOD             ; Advance SOD
          ADC #$20
          STA SOD
          BCC InSe05
          INC SOD+1
InSe05    RTS
InSe10    DEX
          CPX Entries
          BCC InSe30          ; in range
InSe20    LDX #1
          STX Select          ; wrap around
InSe30    INC Select
InSe99    RTS
          
          
          
; *********
  DecSelect
; *********

          LDX Select
          CPX #2              ; First file
          BNE DeSe20
          LDA FirstLine       ; Scroll display
          BEQ DeSe20
          DEC FirstLine
          SEC
          LDA SOD
          SBC #$20
          STA SOD
          BCS DeSe10
          DEC SOD+1
DeSe10    RTS
DeSe20    DEC Select
          BPL DeSe99
          LDX Entries
          INX
          CPX LastLine
          BCC DeSe30
          LDX LastLine
DeSe30    STX Select          ; wrap around
DeSe99    RTS
          
          
; *************
  ShowSelection
; *************

          JSR ShowUnit
          JSR ShowDrive
          JMP ShowEntries


; ********
  IncValue
; ********

          LDX Select
          BNE InVa10
          LDA Unit
          CMP #11
          BCS InVa99
          INC Unit
          BNE InVa20
InVa10    DEX
          BNE InVa99
          LDA Drive
          CMP #'9'
          BCS InVa99
          INC Drive
InVa20    JSR Reload
InVa99    RTS

; ********
  DecValue
; ********

          LDX Select
          BNE DeVa10
          LDA Unit
          CMP #9
          BCC DeVa99
          DEC Unit
          BNE DeVa20
DeVa10    DEX
          BNE DeVa99
          LDA Drive
          CMP #'1'
          BCC DeVa99
          DEC Drive
DeVa20    JSR Reload
DeVa99    RTS


; **********
  HomeSelect
; **********

          LDA #0
          STA Select
          STA FirstLine
          LDA #$20
          STA SOD
          LDA #>Buffer
          STA SOD+1
          RTS

; ********
  MainLoop
; ********

          JSR GETIN
          BEQ MainLoop
          CMP #3          ; STOP key
          BEQ MaLo99    
          CMP #TAB
          BEQ MaLo85
          CMP #CDOWN
          BEQ MaLo85
MaLo05    CMP #CUP
          BNE MaLo10
          JSR DecSelect
          JMP MaLo90
MaLo10    CMP #CRIGHT
          BNE MaLo15
          JSR RightSelect
          JMP MaLo90
MaLo15    CMP #CLEFT
          BNE MaLo20
          JSR LeftSelect
          JMP MaLo90
MaLo20    CMP #'+'
          BNE MaLo25
          JSR IncValue
          JMP MaLo90
MaLo25    CMP #'-'
          BNE MaLo30
          JSR DecValue
          JMP MaLo90
MaLo30    CMP #HOME
          BNE MaLo35
          JSR HomeSelect
          JMP MaLo90
MaLo35    ;BRK
          JMP MainLoop
MaLo85    JSR IncSelect
MaLo90    JSR ShowSelection
          JMP MainLoop
MaLo99    RTS


; ******
  Reload
; ******

          LDA #0
          STA Entries
          JSR PaintMask
          JSR ShowUnit
          JSR ShowDrive
          JSR LoadDirectory
          JSR ShowDiskName
          JSR FormatEntries
          LDA Entries
          BEQ Relo10
          LDA #2
          STA Select
          JSR ShowEntries
          JSR ShowUnit
Relo10    RTS


; ****
  Init
; ****

          LDA #8
          STA Unit
          LDA #'0'
          STA Drive
          LDA #0
          STA CursorRow
          STA CursorCol
          STA Entries
          STA Offset
          STA Reverse
          STA Select
          STA FirstLine
          RTS

; ****
  Main
; ****

          JSR Detect_BASIC_version
          JSR Detect_Screen_Width
          JSR SetupScreen
          JSR SetupEntries
          JSR Init
          JSR Reload
          JSR MainLoop
          LDY #21
          LDA #13
Main10    JSR BSOUT
          DEY
          BNE Main10
          JSR Flush_Keyboard_Queue
EXIT      RTS



; ********************
  Detect_BASIC_version
; ********************
; TODO: detect BASIC version and patch vectors
          RTS

; *******************
  Detect_Screen_Width
; *******************
; TODO: detect Screen Width
          LDA #80
          STA Cols
          LDA #2
          STA Pages
          LDA #42
          STA LastLine
          RTS


; ***********
  SetupScreen
; ***********

          LDA #<Screen
          LDX #>Screen
          LDY #0
SeSe10    STA ScreenLo,Y
          TXA
          STA ScreenHi,Y
          CLC
          LDA ScreenLo,Y
          ADC Cols
          BCC SeSe20
          INX
SeSe20    INY
          CPY #ROWS
          BCC SeSe10
          RTS


; ************
  SetupEntries
; ************

          LDY #0
SeEn10    LDA ScreenLo+3,Y
          LDX ScreenHi+3,Y
          ORA #1
          STA EntryLo,Y
          PHA
          TXA
          STA EntryHi,Y
          PLA
          CLC
          ADC #40
          STA EntryLo+21,Y
          BCC SeEn20
          INX
SeEn20    TXA
          STA EntryHi+21,Y
          INY
          CPY #21
          BCC SeEn10
          RTS


; ********
  Hor_Line
; ********

         CLC
         LDA ScreenLo,Y   ; Row # (0-24) in Y
         ADC Offset
         STA SP
         LDA ScreenHi,Y
         ADC #0
         STA SP+1
         LDY #1
         LDA #HOR_BAR
HoLi10   STA (SP),Y
         INY
         CPY #39
         BCC HoLi10
         RTS

; ********
  Ver_Line
; ********

         CLC
         TYA
         ADC Offset
         TAY
         LDX #1           ; Col # (0-39) in Y
VeLi10   LDA ScreenLo,X
         STA SP
         LDA ScreenHi,X
         STA SP+1
         LDA #VER_BAR
         STA (SP),Y
         INX
         CPX #24
         BCC VeLi10
         RTS


; ******
  GotoXY
; ******

          STX CursorRow
          STY CursorCol
          RTS

; *******
  PutChar
; *******

          PHA                ; A = Char
          LDX CursorRow
          CLC
          LDA ScreenLo,X
          ADC CursorCol
          STA SP
          LDA ScreenHi,X
          ADC #0
          STA SP+1
          PLA
          LDY #0
          STA (SP),Y
          INC CursorCol
          RTS


; ********
  PutCharR
; ********

          PHA                ; A = Char
          LDX CursorRow
          CLC
          LDA ScreenLo,X
          ADC CursorCol
          STA SP
          LDA ScreenHi,X
          ADC #0
          STA SP+1
          PLA
          LDY #0
          JSR PET2SCR
          ORA Reverse
          STA (SP),Y
          INC CursorCol
          RTS


; *********
  PutString
; *********

          STX STP
          STY STP+1
          LDY #0
          LDA (STP),Y
          BEQ PuSt99
          JSR PET2SCR
          JSR PutChar
PuSt10    INY
          CPY #40
          BCS PuSt99      ; safety exit
          LDA (STP),Y
          BEQ PuSt99
          JSR PET2SCR
          STA (SP),Y
          INC CursorCol
          BNE PuSt10
PuSt99    RTS

; ******
  PlotAt
; ******

         PHA              ; X = Row
         CLC              ; Y = Col
         TYA              ; A = Char
         ADC Offset
         TAY
         LDA ScreenLo,X
         STA SP
         LDA ScreenHi,X
         STA SP+1
         PLA
         STA (SP),Y
         RTS

; *******
  PlotAtR
; *******

         PHA              ; X = Row
         CLC              ; Y = Col
         TYA              ; A = Char
         ADC Offset
         TAY
         LDA ScreenLo,X
         STA SP
         LDA ScreenHi,X
         STA SP+1
         PLA
         ORA Reverse
         STA (SP),Y
         RTS

; *********
  PaintPage
; *********

          LDY #0
          JSR Hor_Line
          LDY #2
          JSR Hor_Line
          LDY #24
          JSR Hor_Line
          LDY #0
          JSR Ver_Line
          LDY #39
          JSR Ver_Line
          MAC_Plot( 0, 0,UL)
          MAC_Plot( 0,39,UR)
          MAC_Plot(24, 0,LL)
          MAC_Plot(24,39,LR)
          MAC_Plot( 2, 0,TL)
          MAC_Plot( 2,39,TR)
          RTS

; *********
  PaintMask
; *********

          LDA #142         ; Screen Mode
          JSR BSOUT
          LDA #CLR
          JSR BSOUT        ; clear screen
          JSR BSOUT        ; amd remove window settings
          LDA #14
          STA $e84c        ; text char set
          LDA Pages
          STA Page
          LDA #0
          STA Offset
PaMa10    JSR PaintPage
          CLC
          LDA Offset
          ADC #40
          STA Offset
          DEC Page
          BNE PaMa10
          LDA #0
          STA Offset
          MAC_PutString(1,1,UnitText)
          MAC_PutString(1,9,DriveText)
          RTS



; *************
  GetDiskStatus
; *************

          LDA Unit
          STA FA
          JSR TALK 
          LDA #$6f 
          JSR TKSA 
          LDY #0
gss_10    JSR ACPTR
          CMP #' ' 
          BCC gss_20
          JSR PET2SCR
          STA DiskStatus,Y
          INY  
          CPY #40
          BCC gss_10
gss_20    JSR UNTLK
          RTS  


; *************
  LoadDirectory
; *************

          LDA #<Buffer        ; Initialize buffer pointer BP
          STA BP              ; and End Of Directory EOD
          STA EOD
          LDA #$20
          STA SOD
          LDA #>Buffer
          STA BP+1
          STA EOD+1
          STA SOD+1
          LDY #0
          TYA
LoDi05    STA Buffer,Y
          INY
          BNE LoDi05
          LDA Unit            ; Send LOAD "$n" to Unit
          STA FA
          JSR LISTEN
          LDA #$f0
          JSR SECOND
          LDA #'$'
          JSR CIOUT
          LDA Drive
          JSR CIOUT
          JSR UNLSN
          JSR GetDiskStatus
          LDA DiskStatus
          CMP #'0'
          BNE LoDi99
          JSR TALK
          LDA #$60
          JSR TKSA
          LDA #$bd
          AND STATUS
          STA STATUS
          LDY #0
LoDi10    JSR ACPTR
          STA (BP),Y
          BIT STATUS
          BVS LoDi20
          INY
          BNE LoDi10
          INC BP+1
          BPL LoDi10
LoDi20    JSR UNTLK
          STY EOD
          LDA BP+1
          STA EOD+1
LoDi99    RTS

; **************
  GetLoadAddress
; **************

          LDY #$1e
          LDA #0
          STA (BP),Y          ; new end of string
          LDY #$16            ; position of file type
          LDA (BP),Y
          CMP #'P'            ; PRG files only
          BNE GLA99
          LDA Unit            ; send LOAD "Filenamen" to Unit
          STA FA
          JSR LISTEN
          LDA #$f0
          JSR SECOND
          LDY #4              ; start of ilename
GLA10     LDA (BP),Y
          CMP #$22            ; quote marks end of filename
          BEQ GLA20
          JSR CIOUT
          INY
          CPY #20
          BCC GLA10
GLA20     JSR UNLSN
          JSR GetDiskStatus
          LDA DiskStatus
          CMP #'0'
          BNE GLA99
          JSR TALK
          LDA #$60
          JSR TKSA
          JSR ACPTR
          JSR ASCII_Hex
          LDY #$1d             ; load address low
          STA (BP),Y
          DEY
          TXA
          STA (BP),Y
          JSR ACPTR
          JSR ASCII_Hex
          DEY                  ; load address high
          STA (BP),Y
          DEY
          TXA
          STA (BP),Y
          JSR UNTLK
GLA99     RTS

; **********
  ASCII_Hex
; **********

; Input:  (A)
; Output: (X) = High nibble (A) = Low nibble
         PHA  
         LSR A
         LSR A
         LSR A
         LSR A
         ORA #'0' 
         CMP #$3a 
         BCC Hex_11
         ADC #6
Hex_11   TAX  
         PLA  
         AND #15
         ORA #'0' 
         CMP #$3a 
         BCC Hex_12
         ADC #6
Hex_12   RTS  


EOP       ; END-OF-PROGRAM

