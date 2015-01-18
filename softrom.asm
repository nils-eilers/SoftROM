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

ROWS       =  25

; ****************************
; * Some Zero Page Variables *
; ****************************

BP        = $5e      ; Buffer Pointer
STP       = $60      ; String Pointer
SP        = $62      ; SCREEN Pointer
TP        = $64      ; Temporary Pointer
SelPtr    = $66      ; Selection pointer

; ************************
; * CBM Kernal Variables *
; ************************

STATUS    = $96      ; Status byte
STKEY     = $9b      ; Stop key pressed?
BLNSW     = $a7      ; Blink switch
BLNCT     = $a8      ; Blink count
BLNON     = $aa      ; Blink On
EAL       = $c9      ; used for LOAD, SAVE and TIM
FNLEN     = $d1      ; Filename length
SA        = $d3      ; Secondary address
FA        = $d4      ; First sddress
FNADR     = $da      ; Filename address

DOS_Filename = $0342

; *************
; * Constants *
; *************

Screen = $8000
EntriesPerPage = 20

; *****************************************
; * Put the buffer after the program code *
; *****************************************

Buffer  = [EOP & $ff00] + $100
PosDiskName =  8
PosDiskID   = 26

; ***************************************************
; * The CBM 8000 Kernal has no jump table for these *
; ***************************************************

READY   = $b403 ; $b3ff ; $b406 omitting prompt
TALK    = $f0d2
LISTEN  = $f0d5
SECOND  = $f143
TKSA    = $f193
CIOUT   = $f19e
UNTLK   = $f1ae
UNLSN   = $f1b9
ACPTR   = $f1c0
BSOUT   = $ffd2
CHROUT  = $ffd2
GETIN   = $ffe4

Kernal_STOP = $f343

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

; ***************************************************
; The directory is loaded into RAM beyond the program
; SOD = Start of directory
; EOD = End   of directory
; ***************************************************

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

          .WORD 2015

; **********
  SysCommand
; **********

            .BYTE $9e       ; SYS token
StartML     .BYTE "(1065)"
            .BYTE ':',$8f   ; REM token
            .BYTE " SOFTROM EEPROM TOOL 0.2"
LineEnd     .BYTE 0
EndLink     .WORD 0

          JMP Main

NUMBER      .BYTE "00000 "
UnitText    .PET  'unit:',0
DriveText   .PET  'drive:',0
HelpText    .BYTE "<H> = HELP",0
ProgramText .BYTE " SOFTROM EEPROM TOOL (C) NILS EILERS & BS ",0
Msg_READY   .BYTE "READY.",0
Cols        .BYTE 80
Count       .BYTE  0
CursorRow   .BYTE  0
CursorCol   .BYTE  0
Entries     .BYTE  0
FirstLine   .BYTE  0
LastLine    .BYTE 20
HelpScreen  .BYTE  0
Pages       .BYTE  2
ScreenLo    .FILL 25 (0)
ScreenHi    .FILL 25 (0)
EntryLo     .FILL 40 (0)
EntryHi     .FILL 40 (0)
Page        .BYTE  0
Offset      .BYTE  0
Unit        .BYTE  8
Drive       .BYTE  '0'
LV0         .BYTE  0
LV1         .BYTE  0
Reverse     .BYTE  0
Select      .BYTE  2
FirstFile   .BYTE  0  ; First file on display
SOD         .WORD  0  ; Start Of Directory
EOD         .WORD  0  ; End Of Directory
TargetLo    .BYTE  $00
TargetHi    .BYTE  $90
SelectionLo .BYTE  0
SelectionHi .BYTE  0
SelFilename .BYTE 16

DiskStatus = $8000 + 201

HelpLen   = 30
HelpLines =  7
HelpWin     .BYTE "UP/DN  MOVE SELECTION BAR    ",0
            .BYTE "+ / -  CHANGE UNIT OR DRIVE #",0
            .BYTE "HOME   RESET THE SCREEN      ",0
            .BYTE "RETURN FLASH EEPROM FROM FILE",0
            .BYTE "STOP   QUIT THE PROGRAM      ",0
            .BYTE "H      SHOW THIS HELP WINDOW ",0
            .BYTE "ESC    CLOSE HELP WINDOW     ",0


; ********
  ShowHelp
; ********

          LDA #0
          STA Offset
          LDX #53
          STX Count
          LDX #8
          LDY #24
          JSR HorLine
          LDX #16
          LDY #24
          JSR HorLine
          LDX #16
          STX Count
          LDX #8
          LDY #23
          JSR VerLine
          LDX #8
          LDY #53
          JSR VerLine
          MAC_Plot( 8,23,UL)
          MAC_Plot( 8,53,UR)
          MAC_Plot(16,23,LL)
          MAC_Plot(16,53,LR)
          LDA #HelpLines
          STA Count
          LDX #9
          LDY #24
          JSR GotoXY
          LDX #<HelpWin
          LDY #>HelpWin
ShHe10    JSR PutString
          LDX CursorRow
          INX
          LDY #24
          JSR GotoXY
          CLC
          LDA STP
          ADC #HelpLen
          TAX
          LDA STP+1
          ADC #0
          TAY
          DEC Count
          BNE ShHe10
          LDA #$ff
          STA HelpScreen
          RTS



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
          LDX #2
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
          LDX #2
          LDY #15
          JMP PlotAtR


; ************
  ShowDiskName
; ************

          LDX #2
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

; Input: (X)  = entry # (unchanged)
;        (BP) = buffer pointer
; Used:  (A),(Y)

          CPX LastLine
          BCS ShEn99
          LDY #0              ; no reverse
          TXA                 ; entry # in (X)
          CLC
          ADC #2
          CMP Select
          BNE ShEn10
          LDA EntryLo,X
          STA SelectionLo
          LDA EntryHi,X
          STA SelectionHi
          LDA BP
          STA SelPtr
          LDA BP+1
          STA SelPtr+1
          LDY #$80            ; selected
ShEn10    STY Reverse
          LDA EntryLo,X
          STA SP
          LDA EntryHi,X
          STA SP+1
          LDY #0
ShEn20    LDA (BP),Y
          BEQ ShEn99
          CMP #$22            ; hide quote
          BNE ShEn30
          LDA #' '
ShEn30    JSR PET2SCR
          ORA Reverse
          STA (SP),Y
          INY
          CPY #32
          BCC ShEn20
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
          ADC #18
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
          SBC #20
          STA Select
LeSe99    RTS


; *********
  IncSelect
; *********

; Increment selection
;    0 : unit
;    1 : drive
;  > 1 : filename

          LDX Select
          BEQ InSe40          ; Unit
          DEX
          BEQ InSe40          ; Drive
          CPX LastLine
          BCC InSe20
          CLC                 ; Scroll display
          LDA FirstLine
          ADC LastLine
          CMP Entries
          BCS InSe30          ; At end alreay
          INC FirstLine
          LDA SOD             ; Advance SOD
          ADC #$20
          STA SOD
          BCC InSe10
          INC SOD+1
InSe10    RTS
InSe20    CPX Entries
          BCC InSe40          ; in range
InSe30    LDX #1
          STX Select          ; wrap around
InSe40    INC Select
InSe99    RTS



; *********
  DecSelect
; *********

; Decrement selection
;    0 : unit
;    1 : drive
;  > 1 : filename

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
          CPX LastLine
          BCC DeSe30
          LDX LastLine
DeSe30    INX
          STX Select          ; wrap around
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
          BIT HelpScreen
          BPL MaLo02
          PHA
          JSR Repaint
          PLA
MaLo02    CMP #3          ; STOP key
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
MaLo35    CMP #'H'
          BNE MaLo40
          JSR ShowHelp
          JMP MainLoop
MaLo40    CMP #13
          BNE MaLo45
          JMP Flash
MaLo45    JMP MainLoop
MaLo85    JSR IncSelect
MaLo90    JSR ShowSelection
          JMP MainLoop
MaLo99    RTS


; *******
  Repaint
; *******

          LDA #0
          STA HelpScreen
          JSR PaintMask
          JSR ShowUnit
          JSR ShowDrive
          JSR ShowDiskName
          LDA Entries
          BEQ Repa10
          LDA #2
          STA Select
          JSR ShowEntries
          JSR ShowUnit
Repa10    RTS


; ******
  Reload
; ******

          LDA #0
          STA Entries
          JSR LoadDirectory
          JSR FormatEntries
          JSR Repaint
          RTS


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
          JSR Repaint
          JSR SetupEntries
          JSR Init
          JSR Reload
          JSR MainLoop
          JSR Flush_Keyboard_Queue
          LDA #<Msg_READY
          LDY #>Msg_READY
          JMP READY



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
          LDA #40
          STA LastLine
          RTS


; ***********
  SetupScreen
; ***********

          LDA #CLR
          JSR CHROUT          ; clear screen
          LDA #130
          JSR CHROUT          ; legacy character set
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
SeEn10    LDA ScreenLo+4,Y
          LDX ScreenHi+4,Y
          ORA #1
          STA EntryLo,Y
          PHA
          TXA
          STA EntryHi,Y
          PLA
          CLC
          ADC #40
          STA EntryLo+EntriesPerPage,Y
          BCC SeEn20
          INX
SeEn20    TXA
          STA EntryHi+EntriesPerPage,Y
          INY
          CPY #EntriesPerPage
          BCC SeEn10
          RTS


; ********
  HorLine
; ********

; Draw a horizontal line
; (X)   = Row
; (Y)   = Start column
; Count = Last  column + 1

         CLC
         LDA ScreenLo,X
         ADC Offset
         STA SP
         LDA ScreenHi,X
         ADC #0
         STA SP+1
         LDA #HOR_BAR
HoLi10   STA (SP),Y
         INY
         CPY Count
         BCC HoLi10
         RTS

; ********
  VerLine
; ********

; Draw a vertical line
; (Y)   = Column
; (X)   = Start row
; Count = Last  row + 1

         CLC
         TYA
         ADC Offset
         TAY
VeLi10   LDA ScreenLo,X
         STA SP
         LDA ScreenHi,X
         STA SP+1
         LDA #VER_BAR
         STA (SP),Y
         INX
         CPX Count
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
          JSR PutCharR
PuSt10    INY
          CPY #80
          BCS PuSt99      ; safety exit
          LDA (STP),Y
          BEQ PuSt99
          JSR PET2SCR
          ORA Reverse
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

          LDX #39
          STX Count
          LDX #1
          LDY #1
          JSR HorLine
          LDX #3
          LDY #1
          JSR HorLine
          LDX #24
          LDY #1
          JSR HorLine
          LDX #24
          STX Count
          LDX #1
          LDY #0
          JSR VerLine
          LDX #1
          LDY #39
          JSR VerLine
          MAC_Plot( 1, 0,UL)
          MAC_Plot( 1,39,UR)
          MAC_Plot(24, 0,LL)
          MAC_Plot(24,39,LR)
          MAC_Plot( 3, 0,TL)
          MAC_Plot( 3,39,TR)
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
          MAC_PutString(2,1,UnitText)
          MAC_PutString(2,9,DriveText)
          LDA #$80
          STA Reverse
          MAC_PutString(0,19,ProgramText)
          LDA #0
          STA Reverse
          MAC_PutString(0,69,HelpText)
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
LoDi99    JMP Close_Disk_File

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
          LDA Unit            ; send LOAD "Filename" to Unit
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
GLA99     JMP Close_Disk_File

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

; *********
  Load_File
; *********

          JSR Send_Filename
          JSR TALK            ; send primary address
          LDA #$60
          JSR TKSA            ; send secondary address
          JSR ACPTR           ; read first byte
          TAY                 ; flash start address
          LDA STATUS          ; check time out bit
          BEQ LoFi_22         ; no time out -> continue
;         JMP Display_File_Not_Found
LoFi_22   JSR ACPTR
          STA EAL+1           ; flash start address
          LDA #0
          STA EAL
          STA STATUS
LoFi_30   JSR ACPTR           ; read next byte
          STA (EAL),Y         ; store byte
LoFi_40   LDA (EAL),Y         ; this comparison is false for an
          CMP (EAL),Y         ; unfinished write cycle to EEPROM
          BNE LoFi_40
          INY                 ; increment write address
          BNE LoFi_60
          INC EAL+1
LoFi_60   LDA STATUS          ; Get EOF marker in bit 6
          BEQ LoFi_30         ; repeat until EOF or ERROR
          STY EAL             ; store end address+1 in EAL
          JSR UNTLK

; ***************
  Close_Disk_File
; ***************

          JSR LISTEN
          LDA #$e0
          JSR SECOND
          JMP UNLSN

; *************
  Send_Filename
; *************

          JSR LISTEN
          LDA #$f0
          JSR SECOND
          LDY #0
SeFi_10   LDA (FNADR),Y
          JSR CIOUT
          INY
          CPY FNLEN
          BCC SeFi_10
SeFi_20   JMP UNLSN

; **************
  Setup_Filename
; **************

          CLC
          LDA SelPtr
          ADC #4
          STA TP
          LDA SelPtr+1
          ADC #0
          STA TP+1
          LDY #0
SpFi_10   LDA (TP),Y
          CMP #$22
          BEQ SpFi_20
          STA DOS_Filename,Y
          INY
          CPY #16
          BCC SpFi_10
SpFi_20   STY FNLEN
          LDA #<DOS_Filename
          STA FNADR
          LDA #>DOS_Filename
          STA FNADR+1
          RTS


; *****
  Flash
; *****

          JSR Setup_Filename
          JSR Load_File
;         JSR Verify_File
          RTS


EOP       ; END-OF-PROGRAM
