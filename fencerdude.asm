; move a sprite with the joystick

	processor 6502
	include vcs.h
	org $F000

LowSwordOffset = 8
MidSwordOffset = 4
HighSwordOffset = 0

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

P0Status = $93
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

	LDA #%00010000	; Down?
	BIT SWCHA
	BNE P0SkipMoveDown
	INC P0YPosFromBot
P0SkipMoveDown

	LDA #%00100000	; Up?
	BIT SWCHA
	BNE P0SkipMoveUp
	DEC P0YPosFromBot
P0SkipMoveUp

	;; for left and right, we're gonna
	;; set the horizontal speed, and then do
	;; a single HMOVE.  We'll use X to hold the
	;; horizontal speed, then store it in the
	;; appropriate register

	;; assume horiz speed will be zero
	LDX #0

	LDA #%01000000	; Left?
	BIT SWCHA
	BNE P0SkipMoveLeft
	LDX #$10	; a 1 in the left nibble means go left
P0SkipMoveLeft

	LDA #%10000000	; Right?
	BIT SWCHA
	BNE P0SkipMoveRight
	LDX #$F0	; a -1 in the left nibble means go right...
P0SkipMoveRight
			;(in 4 bits, using "two's complement
			; notation", binary 1111 = decimal -1
			; (which we write there as hex F --
			; confused?))


	STX HMP0	; set the move for Player 0
	STX HMM0	; ... and Missile (sword) 0


	;; Now, check P1

	LDA #%00000001	; Down?
	BIT SWCHA
	BNE P1SkipMoveDown
	INC P1YPosFromBot
P1SkipMoveDown

	LDA #%00000010	; Up?
	BIT SWCHA
	BNE P1SkipMoveUp
	DEC P1YPosFromBot
P1SkipMoveUp

	LDX #0

	LDA #%00000100	; Left?
	BIT SWCHA
	BNE P1SkipMoveLeft
	LDX #$10	; a 1 in the left nibble means go left
P1SkipMoveLeft

	LDA #%00001000	; Right?
	BIT SWCHA
	BNE P1SkipMoveRight
	LDX #$F0	; a -1 in the left nibble means go right...
P1SkipMoveRight
			;(in 4 bits, using "two's complement
			; notation", binary 1111 = decimal -1
			; (which we write there as hex F --
			; confused?))


	STX HMP1	; set the move for Player 0
	STX HMM1	; ... and Missile (sword) 0


	;; Use the button input for stance switching (mostly to test it)
	;; Pressing the button will cycle through low-med-high stances
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

	;; Skipdraw (as per Thomas Jentzsch - http://www.randomterrain.com/atari-2600-memories-tutorial-andrew-davie-23.html)

P0SkipDraw
	TXA			; [30] + 2
	SEC			; [32] + 2
	SBC P0YPosFromBot	; [34] + 3
	ADC #15			; [37] + 2
	BCC SkipP0		; [39] + 2 (3)
	TAY			; [41] + 2
	LDA (P0Sprite),Y	; [43] + 5
	STA GRP0Next		; [48] + 3
	JMP P1SkipDraw		; [51] + 3
SkipP0				; [42]
	DEC $2D			; [42] + 5 // NOP
	DEC $2D			; [47] + 5 // NOP
	SEC			; [52] + 2 // Only got here because carrry bit was cleared, so re-set it for P1SkipDraw
P1SkipDraw			; [54]
	TXA			; [54] + 2
	SBC P1YPosFromBot	; [56] + 3
	ADC #15			; [59] + 2
	BCC SkipP1		; [61] + 2 (3)
	TAY			; [63] + 2
	LDA (P1Sprite),Y	; [65] + 5
	STA GRP1Next		; [70] + 3
	JMP EndSkipDraw		; [73] + 3
SkipP1				; [64]
	DEC $2D			; [64] + 5 // NOP
	DEC $2D			; [69] + 5 // NOP
	NOP			; [74] + 2
EndSkipDraw			; [76]

P0Missile
	CPX P0MissileLine	; [76] + 3
	PHP			; [79] + 3
P1Missile
	CPX P1MissileLine	; [82] + 3
	PHP			; [85] + 3

				; [90]
	DEC $2D			; YARRR! Here be booty! 17 cycles! \o/
	DEC $2D
	DEC $2D
	NOP

	;; End Skipdraw
	
	;; Assumptions:
	;; X holds current scanline (counted from bottom, starting at 95)

	;; What's going on:
	;; P[0,1]YPosFromBot hold the lines where the two player sprites
	;; start. If we're on that line, put 14 into P[0,1]LinesLeft, since
	;; the sprites are 14 lines high. The following routine will draw
	;; one line of each player and decrement the lines left.
	;; To save cycles, P0LinesLeft is put into the Y register by the end
	;; of this routine, so DrawPlayers won't have to load it

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

	;; Code from http://www.biglist.com/lists/stella/archives/200403/msg00260.html
	;; Thanks to R. Mundschau!
	;;
	;; Positions an object horizontally
	;; Inputs: A = Desired position.
	;; X = Desired object to be positioned (0-5).
	;; scanlines: If control comes on or before cycle 73 then 1 scanline is consumed.
	;; If control comes after cycle 73 then 2 scanlines are consumed.
	;; Outputs: X = unchanged
	;; A = Fine Adjustment value.
	;; Y = the "remainder" of the division by 15 minus an additional 15.
	;; control is returned on cycle 6 of the next scanline.
PosObject SUBROUTINE

	STA WSYNC		; 00 Sync to start of scanline.
	SEC			; 02 Set the carry flag so no borrow will be applied during the division.
divideby15
	SBC #15			; 04 ; Waste the necessary amount of time dividing X-pos by 15!
	BCS divideby15		; 06/07 - 11/16/21/26/31/36/41/46/51/56/61/66

	TAY			; 08 ; At this point the value in A is -1 to -15. In this code I use a table
				; to quickly convert that value to the fine adjust value needed.
	LDA fineAdjustTable,Y	; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
				; In your own code you may wish to consume only 4.
	STA HMP0,X		; 17 Store the fine adjustment value.

	STA RESP0,X		; 21/ 26/31/36/41/46/51/56/61/66/71 - Set the rough position.

	STA WSYNC
	RTS


	org $FD00
FencerLow ; 14 Lines - Upside-Down because it's easier to draw that way
	.byte %00000000
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
	.byte %00000000
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
	.byte %00000000
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

	org $FF00
fineAdjustBegin		; table for fine adjustment of X positioning
	.byte %01110000	; Left 7
	.byte %01100000 ; Left 6
	.byte %01010000 ; Left 5
	.byte %01000000 ; Left 4
	.byte %00110000 ; Left 3
	.byte %00100000 ; Left 2
	.byte %00010000 ; Left 1
	.byte %00000000 ; No movement.
	.byte %11110000 ; Right 1
	.byte %11100000 ; Right 2
	.byte %11010000 ; Right 3
	.byte %11000000 ; Right 4
	.byte %10110000 ; Right 5
	.byte %10100000 ; Right 6
	.byte %10010000 ; Right 7
fineAdjustTable = fineAdjustBegin - %11110001 ; NOTE: %11110001 = -15

	org $FFFC
	.word Start		; NMI
	.word Start		; RESET
	.word Start		; IRQ
