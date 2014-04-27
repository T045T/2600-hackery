; move a sprite with the joystick

	processor 6502
	include vcs.h
	org $F000

LowSwordOffset = 7
MidSwordOffset = 3
HighSwordOffset = -1

P0YPosFromBot = $80;
P0LinesLeft = $81;
P0MissileLine = $82

P1YPosFromBot = $83
P1LinesLeft = $84
P1MissileLine = $85

;; these will hold the values that are pushed to the corresponding registers on the next scanline, so the memory fetch and related calculations don't cause us to miss the start of the line
GRP0Next = $86
GRP1Next = $87
PF0Next = $88
PF1Next = $89
PF2Next = $8A
ENAM0Next = $8B
ENAM1Next = $8C

P0ArrowPos = $8D
P1ArrowPos = $8E

P0Sprite = $8F
P1Sprite = $91

P0Status = $93		; Left nibble (D4-D7) is P0, right (D0-D3) P1
	;;  D1D0 : Current animation frame (4 frames each) - always back to 0 for standing, make sword shorter for the other frames
	;;  D2   : Jumping?
	;;  D3 : 0 if facing right, 1 if left (aligned to simply dump P0Status into REFP0)
	;;  D5D4 : Counter for stance:
	;;   0 0 : Low - if Jumping, DIVE!
	;;   0 1 : Med - if Jumping, KICK!
	;;   1 0 : High
	;;  D6: SwordThrown (if 1, don't touch P0MissileLine or HMM0 for Joystick events, sword is taken care of by physics - haha, like we have physics)
	;;  D7: ResetPlayer (if 1, reset Player to his edge of the screen)
P1Status = $94

CurrentLine = $95
PF0Base = $96
PF1Base = $98
PF2Base = $9A
CurrentScreen = $9C		; Incremented when moving one screen to the right, decremented when moving to the left
				; Levels are symmetrical, so if CurrentScreen is negative, use NOT(CurrentScreen)+1 as screen index

;generic start up stuff...
Start
	SEI
	CLD
	LDX #$FF
	TXS
	LDA #0
ClearMem
	STA 0,X
	DEX
	BNE ClearMem
	LDA #$00
	STA COLUBK	;start with black background
	LDA #$82
	STA COLUPF
	LDA #66
	STA COLUP0
	LDA #$1E
	STA COLUP1
	LDA #1
	STA CTRLPF
;Setting some variables...
	LDA #40
	STA P0YPosFromBot	;Initial Y Position
	STA P1YPosFromBot

	LDA #$30
	STA NUSIZ0	; Missile is 8 color clocks wide, player normal
	STA NUSIZ1	; Missile is 8 color clocks wide, player normal

	; Reset Player and missile position
;;	LDX #12
;;	STA WSYNC
;;	STA RESP0		; (4) Player 0 to left edge of screen

;; P1Reset
;;	DEX			; 2
;;	BNE P1Reset		; 2 (3)
;;	NOP
;;	;; The Loop above should take 13*5 - 1 = 64 cycles, plus the 4 from RESP0
;;	;; makes 68. This means we're at color clock 68*3 = 204
;;	STA RESM1
;;	STA RESP1		;Reset Player 1 too, close to the right edge

	LDA #%10000000		; Reset P0
	STA P0Status
	LDA #%10001000		; Reset P1, and have him reflected
	STA P1Status

;VSYNC time
MainLoop
	LDA #2
	STA VSYNC
	STA WSYNC
	STA WSYNC
	STA WSYNC
	LDA #43
	STA TIM64T
	LDA #0
	STA VSYNC


;Main Computations; check down, up, left, right
;general idea is to do a BIT compare to see if
;a certain direction is pressed, and skip the value
;change if so

;
;Not the most efficient code, but gets the job done,
;including diagonal movement
;

; for up and down, we INC or DEC
; the Y Position

	LDA #%00010000	;Down?
	BIT SWCHA
	BNE P0SkipMoveDown
	INC P0YPosFromBot
P0SkipMoveDown

	LDA #%00100000	;Up?
	BIT SWCHA
	BNE P0SkipMoveUp
	DEC P0YPosFromBot
P0SkipMoveUp

; for left and right, we're gonna
; set the horizontal speed, and then do
; a single HMOVE.  We'll use X to hold the
; horizontal speed, then store it in the
; appropriate register


;assume horiz speed will be zero
	LDX #0

	LDA #%01000000	;Left?
	BIT SWCHA
	BNE P0SkipMoveLeft
	LDX #$10	;a 1 in the left nibble means go left
P0SkipMoveLeft

	LDA #%10000000	;Right?
	BIT SWCHA
	BNE P0SkipMoveRight
	LDX #$F0	;a -1 in the left nibble means go right...
P0SkipMoveRight
			;(in 4 bits, using "two's complement
			; notation", binary 1111 = decimal -1
			; (which we write there as hex F --
			; confused?))


	STX HMP0	;set the move for Player 0
	STX HMM0	; ... and Missile (sword) 0


	;; Now, check P1

	LDA #%00000001	;Down?
	BIT SWCHA
	BNE P1SkipMoveDown
	INC P1YPosFromBot
P1SkipMoveDown

	LDA #%00000010	;Up?
	BIT SWCHA
	BNE P1SkipMoveUp
	DEC P1YPosFromBot
P1SkipMoveUp

; for left and right, we're gonna
; set the horizontal speed, and then do
; a single HMOVE.  We'll use X to hold the
; horizontal speed, then store it in the
; appropriate register


;assume horiz speed will be zero
	LDX #0

	LDA #%00000100	;Left?
	BIT SWCHA
	BNE P1SkipMoveLeft
	LDX #$10	;a 1 in the left nibble means go left
P1SkipMoveLeft

	LDA #%00001000	;Right?
	BIT SWCHA
	BNE P1SkipMoveRight
	LDX #$F0	;a -1 in the left nibble means go right...
P1SkipMoveRight
			;(in 4 bits, using "two's complement
			; notation", binary 1111 = decimal -1
			; (which we write there as hex F --
			; confused?))


	STX HMP1	;set the move for Player 0
	STX HMM1	; ... and Missile (sword) 0

; while we're at it, change the color of the background
; if the button is pressed (making sure D6 of VBLANK has
; appropriately set above) We'll set the background color
; to the vertical position, since that will be changing
; a lot but we can still control it.

	LDA INPT4		;read button input
	BMI ButtonNotPressed	;skip if button not pressed
	LDA #%00010000
	BIT P0Status
	BNE IsMid
	LDA #%00100000
	BIT P0Status
	BNE IsHigh
IsLow				; Go from low to mid...
	LDA P0Status
	AND #%11001111
	ADC #%00010000
	JMP ChangedStance
IsMid				; From mid to high...
	LDA P0Status
	AND #%11001111
	ADC #%00100000
	JMP ChangedStance
IsHigh				; And back to low.
	LDA P0Status
	AND #%11001111
	ADC #%00000000
ChangedStance
	STA P0Status
	;; LDA P0YPosFromBot		;must be pressed, get YPos
	;; STA COLUBK		;load into bgcolor
ButtonNotPressed

	STA WSYNC
	STA HMOVE

CheckPlayerStatus
	LDA P0Status		; Set reflection bit for both players according to their status byte
	STA REFP0
	LDA P1Status
	STA REFP1
P0PosStart
	LDA #%00010000
	BIT P0Status
	BNE P0MidPos
	LDA #%00100000
	BIT P0Status
	BNE P0HighPos
P0LowPos
	LDX #<FencerLow
	STX P0Sprite
	LDX #>FencerLow
	STX P0Sprite+1
	LDA P0YPosFromBot
	SBC #LowSwordOffset
	JMP P0PosDone
P0MidPos
	LDX #<FencerMid
	STX P0Sprite
	LDX #>FencerMid
	STX P0Sprite+1
	LDA P0YPosFromBot
	SBC #MidSwordOffset
	JMP P0PosDone
P0HighPos
	LDX #<FencerHigh
	STX P0Sprite
	LDX #>FencerHigh
	STX P0Sprite+1
	LDA P0YPosFromBot
	SBC #HighSwordOffset
P0PosDone
	STA P0MissileLine
	CLC
P1PosStart
	LDA #%00010000
	BIT P1Status
	BNE P1MidPos
	LDA #%00100000
	BIT P1Status
	BNE P1HighPos
P1LowPos
	LDX #<FencerLow
	STX P1Sprite
	LDX #>FencerLow
	STX P1Sprite+1
	LDA P1YPosFromBot
	SBC #LowSwordOffset
	JMP P1PosDone
P1MidPos
	LDX #<FencerMid
	STX P1Sprite
	LDX #>FencerMid
	STX P1Sprite+1
	LDA P1YPosFromBot
	SBC #MidSwordOffset
	JMP P1PosDone
P1HighPos
	LDX #<FencerHigh
	STX P1Sprite
	LDX #>FencerHigh
	STX P1Sprite+1
	LDA P1YPosFromBot
	SBC #HighSwordOffset
P1PosDone
	STA P1MissileLine
	CLC
P0SwordThrown
P1SwordThrown			;TODO!

P0Reset
	LDA #%10000000
	BIT P0Status
	BEQ P0ResetDone
	STA WSYNC
	STA RESP0
P0ResetDone
	LDA P0Status
	AND #%01111111		; Clear reset bit
	STA P0Status
P1Reset
	LDA #%10000000
	BIT P0Status
	BEQ P1ResetDone
	LDX #12
	STA WSYNC
	NOP			; 2
	NOP			; 2
P1ResetLoop
	DEX			; 2
	BNE P1ResetLoop		; 2 (3)
P1ResetDone
	LDA P0Status
	AND #%11110111		; Clear and write P1 reset bit
	STA P0Status

	;; Reset the swords every frame to account for possible turning around

P0SwordReset
	LDA #%01000000
	BIT P0Status
	BNE P0SwordSkip		; If sword has been thrown, don't position it with the player
	STA HMCLR
	LDX #2
	STX RESMP0
	LDA #0
	STA RESMP0
	LDA #%00001000
	BIT P0Status
	BNE P0Mirrored
	LDA #$C0
	JMP P0SwordDone
P0Mirrored
	LDA #$70
	STA HMM0
	STA WSYNC
	STA HMOVE
	LDA #$50
	LDX #4
	NOP
P0HMOVE_Delay			; Don't change HMM0 for at least 24 cycles
	DEX
	BNE P0HMOVE_Delay
P0SwordDone
	STA HMM0
	LDA #0
	STA RESMP0
P0SwordSkip

P1SwordReset
	LDA #%01000000
	BIT P1Status
	BNE P1SwordSkip		; If sword has been thrown, don't position it with the player
	LDX #2
	STX RESMP1
	LDA #0
	STA RESMP1
	LDA #%00001000
	BIT P1Status
	BNE P1Mirrored
	LDA #$C0
	JMP P1SwordDone
P1Mirrored
	LDA #$70
	STA HMM1
	STA WSYNC
	STA HMOVE
	LDA #$50
	LDX #4
P1HMOVE_Delay			; Don't change HMM0 for at least 24 cycles
	DEX
	BNE P1HMOVE_Delay
	LDX #0
	STX HMM0		; The mirror-HMOVE already applied the HMM0 value calculated above
P1SwordDone
	STA HMM1
	LDA #0
	STA RESMP1
P1SwordSkip

	STA WSYNC
	STA HMOVE

	;; For Testing only!
	LDA #<PF0Center
	STA PF0Base
	LDA #>PF0Center
	STA PF0Base+1

	LDA #<PF1Center
	STA PF1Base
	LDA #>PF1Center
	STA PF1Base+1

	LDA #<PF2Center
	STA PF2Base
	LDA #>PF2Center
	STA PF2Base+1

WaitForVblankEnd
	LDA INTIM
	BNE WaitForVblankEnd
	LDX #95			; Halved because we're now using a two-line kernel
	STX CurrentLine
	STA WSYNC
	STA VBLANK


	;; Main scanline loop!
	;; Assumptions:
	;; A = draw P0 sword this line?
	;; Y = draw P1 sword this line?
	;; X = Don't care

	;; What's going on:
	;; The "Scanloop", or Kernel, always takes 2 scanlines
	;; (2*76 = 152 cycles) to run - to do this, branches
	;; need to be balanced, sometimes with creative use
	;; of instructions other than NOP. These instructions have a "NOP"
	;; comment to mark them.
	;;
	;; What the Kernel actually does in the two scanlines is two things:
	;; 1. Set the relevant registers (GRP[0,1], ENAM[0,1], PF[0,1,2]) to
	;;    what they should be for the 2 scanlines the loop runs
	;; 2. Decide whether to load and potentially load from ROM the register
	;;    values for the next Kernel iteration

	;; Cycle counts are in the form of [X] + Y, where
	;; X: Cycle where PC arrives at this instruction
	;; Y: Number of cycles this instruction takes

ScanLoop

	;; Entry point is at 3 cycles, since the jump from the end of the loop
	;; happens right at the end of the line
	STA ENAM0		; [3] + 3
	STY ENAM1		; [6] + 3
	LDA GRP0Next		; [9] + 3
	STA GRP0		; [12] + 3

	LDA GRP1Next		; [15] + 3
	STA GRP1		; [18] + 3

	;; All player and missile registers set after 21 cycles

	;; PF0 and PF2 are set at the end of the loop, and PF1
	;; doesn't start drawing until cycle 28, so set it here
	;; (68 color cycles HBLANK + 4 Bits * 4 color cycles from PF0
	;;  => color cycle 84, 84 / 3 = 28 )
	LDA PF1Next		; [21] + 3
	STA PF1			; [24] + 3

	LDX CurrentLine		; [27] + 3

	;; Assumptions:
	;; X holds current scanline (counted from bottom, starting at 95)

	;; What's going on:
	;; P[0,1]YPosFromBot hold the lines where the two player sprites
	;; start. If we're on that line, put 14 into P[0,1]LinesLeft, since
	;; the sprites are 14 lines high. The following routine will draw
	;; one line of each player and decrement the lines left.
	;; To save cycles, P0LinesLeft is put into the Y register by the end
	;; of this routine, so DrawPlayers won't have to load it
ActivatePlayers			; [30]
	LDY #14			; [30] + 2 Only need to do this once
CheckActivateP1			; arrive at [32]
	CPX P1YPosFromBot	; [32] + 3
	BNE SkipActivateP1	; [35] + 2 (3)
	STY P1LinesLeft		; [37] + 3
	JMP CheckActivateP0	; [40] + 3
SkipActivateP1			; [38] // Need to balance the shorter side of the branch
	NOP			; [38] + 2
	STA $2D			; [40] + 3 // NOP

CheckActivateP0			; [43]
	CPX P0YPosFromBot	; [43] + 3
	BNE SkipActivateP0	; [46] + 2 (3 if taken)
	STY P0LinesLeft		; [48] + 3
	JMP DrawPlayers		; [51] + 3 // Y contains lines left
SkipActivateP0			; [49] // Need to balance the shorter side of the branch
	NOP			; [49] + 2 // NOP
	LDY P0LinesLeft		; [51] + 3 // Player 0 is active, so we need to load lines left into Y for sprite drawing
EndActivatePlayers

	;; Assumptions:
	;; X holds current scanline (counted from Bottom, starting at 95)
	;; Y holds lines left to draw of P0Sprite (i.e. P0LinesLeft)

	;; What's going on:
	;; Check whether P[0,1]LinesLeft > 0 - if it is, there's lines left to
	;; draw for the corresponding player. If it's 0, put 0 into GRP[0,1]
DrawPlayers			; [54]
	BEQ DontDrawP0		; [54] + 2 (3 if taken)
IsP0_On
	DEY			; [56] + 2 // Decrement y to get a valid offset
	LDA (P0Sprite),Y	; [58] + 5
	STA GRP0Next		; [63] + 3
	STY P0LinesLeft		; [66] + 3
	JMP DrawP0Missile	; [69] + 3
DontDrawP0			; [57] // Balance shorter side of branch
	DEC $2D			; [57] + 5 // NOP
	NOP			; [62] + 2 // NOP
	STY P0LinesLeft		; [64] + 3 // Store new P0LinesLeft value (0)
	LDA #0			; [67] + 2 // Set GRP0Next to 0 (we're done
	STA GRP0Next		; [69] + 3 // drawing P0 for this scanline)

DrawP0Missile			; [72]
	CPX P0MissileLine	; [72] + 3
	PHP			; [75] + 3
	;; The status word after CPX actually has the correct bit
	;; set to write to ENAM0/1, so push it to the stack as "ENAM0Next"

	LDY P1LinesLeft		; [78] + 3
	BEQ DontDrawP1		; [81] + 2 (3)
IsP1_On
	DEY			; [83] + 2
	LDA (P1Sprite),Y	; [85] + 5
	STA GRP1Next		; [90] + 3
	STY P1LinesLeft		; [93] + 3
	JMP DrawP1Missile	; [96] + 3
DontDrawP1			; [84]
	DEC $2D			; [84] + 5 // NOP
	DEC $2D			; [89] + 5 // NOP
	LDA #0			; [94] + 2 // Set GRP1Next to 0 (we're done
	STA GRP1Next		; [96] + 3 // drawing P1 for this scanline)

DrawP1Missile			; [99]
	CPX P1MissileLine	; [99] + 3
	PHP			; [102] + 3
EndDrawPlayers

	;; Assumptions:
	;; The top of the stack points at the next line's value for
	;; ENAM1, the next value on the stack after that is ENAM0
	;;
	;; What's going on:
	;; Here, we load the values for the 3 playfield registers from ROM,
	;; store them in RAM and set up the Registers (A and Y) so they can be
	;; used at the top of the loop.
	;; We also set PF0, since there's no time to do it at the top

DrawPF				; [105]
	LDY.w CurrentLine	; [105] + 4 // Load current line into Y
				;           // (it's also in X, but the addressing mode we need below only works with Y...)
	;; the .w above changes the instruction from zero-width (3 cycles)
	;; to absolute addressing (4 cycles), so we arrive at the end
	;; right at cycle 152
	DEC CurrentLine		; [109] + 5
	BEQ StartOverscan	; [114] + 2 (3) // If we're at line 0, the playfield is done, go into overscan
	LDA (PF0Base),Y		; [116] + 5
	TAX			; [121] + 2 // Store PF0 value in X, we can't set it yet, because it has yet to be drawn in the right screen half
	LDA (PF1Base),Y		; [123] + 5
	STA PF1Next		; [128] + 3
	LDA (PF2Base),Y		; [131] + 5

	;; PF2 is drawn during cycles 38-60, and 114-136
	;; In other words, the TIA is just done drawing it when we set
	;; it here for the next two-line interval :)

	STA PF2			; [136] + 3

	;; Get the ENAM0 and ENAM1 values from the stack
	;; ENAM1 was pushed last and goes into the Y
	;; register, ENAM0 stays in the accumulator
	PLA			; [139] + 4
	TAY			; [143] + 2
	PLA			; [145] + 4

	;; This is the latest in a scanline we can set PF0,
	;; and it appears to work without disturbing the current line's
	;; drawing
	STX PF0			; [149] + 3
	JMP ScanLoop		; [152] + 3
EndScanLoop

StartOverscan
	LDA #2
	STA WSYNC
	STA VBLANK
	LDY #31
	PLA			; These are here to make sure the stack doesn't overflow
	PLA
OverScanWait
	STA WSYNC
	DEY
	BNE OverScanWait
	JMP  MainLoop

	org $FD00
FencerLow ; 14 Lines - Upside-Down because it's easier to draw that way
	.byte %00100100  ;  X  X  ;
	.byte %00100110  ;  X  XX ;
	.byte %00110010  ;  XX  X ;
	.byte %00110110  ;  XX XX ;
	.byte %00111100  ;  XXXX  ;
	.byte %00111000  ;  XXX   ;
	.byte %00110001  ;  XX   X;
	.byte %00110010  ;  XX  X ;
	.byte %00110100  ;  XX X  ;
	.byte %00111000  ;  XXX   ;
	.byte %01110000  ; XXX    ;
	.byte %10100000  ;X X     ;
	.byte %10110000  ;X XX    ;
	.byte %10110000  ;X XX    ;

FencerHigh
	.byte %00100100  ;  X  X  ;
	.byte %00100110  ;  X  XX ;
	.byte %00110010  ;  XX  X ;
	.byte %00110110  ;  XX XX ;
	.byte %00111100  ;  XXXX  ;
	.byte %00111000  ;  XXX   ;
	.byte %00110000  ;  XX    ;
	.byte %00110000  ;  XX    ;
	.byte %00110000  ;  XX    ;
	.byte %00110000  ;  XX    ;
	.byte %01111000  ; XXXX   ;
	.byte %10100100  ;X X  X  ;
	.byte %10110010  ;X XX  X ;
	.byte %10110001  ;X XX   X;

FencerMid
	.byte %00100100  ;  X  X  ;
	.byte %00100110  ;  X  XX ;
	.byte %00110010  ;  XX  X ;
	.byte %00110110  ;  XX XX ;
	.byte %00111100  ;  XXXX  ;
	.byte %00111000  ;  XXX   ;
	.byte %00110000  ;  XX    ;
	.byte %00110000  ;  XX    ;
	.byte %00110110  ;  XX XX ;
	.byte %00111001  ;  XXX  X;
	.byte %01110000  ; XXX    ;
	.byte %10100000  ;X X     ;
	.byte %10110000  ;X XX    ;
	.byte %10110000  ;X XX    ;

PF0Center
	.byte %11110000
	.byte %11100000
	.byte %11110000
	.byte %11100000
	.byte %11110000
	.byte %11100000
	.byte %11110000
	.byte %11100000
	.byte %11110000
	.byte %11100000
	.byte %11110000
	.byte %11100000
	.byte %00000000
	.byte %11100000
	.byte %00000000
	.byte %11100000
	.byte %00000000
	.byte %11100000
	.byte %00000000
	.byte %11100000
	.byte %00000000
	.byte %11100000
	.byte %00000000
	.byte %11100000
	.byte %00000000
	.byte %11100000
	.byte %00000000
	.byte %11100000
	.byte %00000000
	.byte %11100000
	.byte %00000000
	.byte %11100000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %00000000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000

	org $FE00
PF1Center
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100
	.byte %00111100

PF2Center
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110
	.byte %11000110

	org $FFFC
	.word Start		; NMI
	.word Start		; RESET
	.word Start		; IRQ
