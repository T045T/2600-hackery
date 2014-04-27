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

P0Status = $93 		; Left nibble (D4-D7) is P0, right (D0-D3) P1
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
CurrentScreen = $9C 		; Incremented when moving one screen to the right, decremented when moving to the left
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
;; 	LDX #12
;; 	STA WSYNC
;; 	STA RESP0 		; (4) Player 0 to left edge of screen

;; P1Reset
;; 	DEX			; 2
;; 	BNE P1Reset		; 2 (3)
;; 	NOP
;; 	;; The Loop above should take 13*5 - 1 = 64 cycles, plus the 4 from RESP0
;; 	;; makes 68. This means we're at color clock 68*3 = 204
;; 	STA RESM1
;; 	STA RESP1		;Reset Player 1 too, close to the right edge
	
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
	AND #%01111111 		; Clear reset bit
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
	LDX #95 		; Halved because we're now using a two-line kernel
	STX CurrentLine
	STA WSYNC
	STA VBLANK  	


;main scanline loop...

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
	
	;; PF0 and PF2 are set at the end of the loop
	LDA PF1Next		; [21] + 3
	STA PF1			; [24] + 3

	LDX CurrentLine		; [27] + 3

	;; From here on out, there are 12 "ghost cycles" from the timer I removed
	;; but I don't want to update all cycle counts right now
	
; here the idea is that P0LinesLeft
; is zero if the line isn't being drawn now,
; otherwise it's however many lines we have to go

	LDY #14			; [38] + 2 Only need to do this once
CheckActivateP1			; arrive at [51]
	CPX P1YPosFromBot	; [51] + 3
	BNE SkipActivateP1	; [54] + 2 (3)
	STY P1LinesLeft		; [56] + 3
	JMP CheckActivateP0	; [59] + 3
SkipActivateP1
	NOP			; [57] + 2
	STA $2D			; [59] + 3

CheckActivateP0
	CPX P0YPosFromBot	; [40] + 3
	BNE SkipActivateP0	; [43] + 2 (3 if taken)
	STY P0LinesLeft		; [45] + 3
	JMP StartSpriteDrawing	; [48] + 3
SkipActivateP0
	NOP 			; [46] + 2
	LDY P0LinesLeft		; [48] + 3 - storing ACC to nowhere

StartSpriteDrawing		; [62]
;turn player graphics off then see if there's a line of sprite to draw

	;; if P0LinesLeft is non zero,
	;; we're drawing it

	;; Y contains P0LinesLeft

	BEQ FinishP0		; [73] + 2 (3 if taken)
IsP0_On
	DEY			; [75] + 2
	LDA (P0Sprite),Y	; [77] + 5
	STA GRP0Next		; [82] + 3
	STY P0LinesLeft		; [85] + 3
	JMP DrawP1Sprite	; [88] + 3
FinishP0			; [76]
	DEC $2D			; [76] + 5
	NOP
	STY P0LinesLeft		; [81] + 3
	LDA #0			; [86] + 2
	STA GRP0Next		; [88] + 3
	
DrawP1Sprite			; [91]
	CPX P0MissileLine	; [91] + 3
	PHP			; [94] + 3 -
	;; The status word is actually a valid input
	;; to ENAM0/1, so that's cool

	LDY P1LinesLeft 	; [97] + 3
	BEQ FinishP1		; [100] + 2 (3)
IsP1_On
	DEY			; [102] + 2
	LDA (P1Sprite),Y	; [104] + 5
	STA GRP1Next		; [109] + 3
	STY P1LinesLeft		; [112] + 3
	JMP PlayersDone		; [115] + 3
FinishP1			; [103]
	DEC $2D			; [103] + 5
	DEC $2D			; [108] + 5
	LDA #0			; [113] + 2
	STA GRP1Next		; [115] + 3

PlayersDone			; [118] - 6 Ghost cycles
	CPX P1MissileLine	; [112] + 3
	PHP			; [115] + 3

	LDY.w CurrentLine		; [118] + 3
	DEC CurrentLine		; [121] + 2
	BEQ StartOverscan	; [123] + 2 (3)
	LDA (PF0Base),Y		; [125] + 5
	TAX
	LDA (PF1Base),Y		; [133] + 5
	STA PF1Next		; [138] + 3
	LDA (PF2Base),Y		; [141] + 5

	STA PF2			; [146] + 3
		
	PLA			; 4
	TAY
	PLA			; 4
	STX PF0			; [160] + 3
	JMP ScanLoop		; [156, apparently] + 3

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
	
	org $FFFC
	.word Start		; NMI
	.word Start		; RESET
	.word Start		; IRQ
