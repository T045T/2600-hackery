; move a sprite with the joystick
	
	processor 6502
	include vcs.h
	org $F000

LowSwordOffset = 7
MediumSwordOffset = 7
HighSwordOffset = 7
	
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
	LDA #66
	STA COLUP0
;Setting some variables...
	LDA #80
	STA P0YPosFromBot	;Initial Y Position

	LDA #$30	
	STA NUSIZ0	; Missile is 8 color clocks wide, player normal
	STA NUSIZ0	; Missile is 8 color clocks wide, player normal

	; Reset Player and missile position
	STA WSYNC
	STA RESP0 		;Player 0 to left edge of screen
	LDA #2
	STA RESMP0		; Player 0 missile (sword) on top of Player 0
	LDA #0
	STA RESMP0		; Activate missile graphic

	 LDA #$C0
	 STA HMM0
	 STA WSYNC
	 STA HMOVE


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
	BNE SkipMoveDown
	INC P0YPosFromBot
	INC P0YPosFromBot
SkipMoveDown

	LDA #%00100000	;Up?
	BIT SWCHA 
	BNE SkipMoveUp
	DEC P0YPosFromBot
	DEC P0YPosFromBot
SkipMoveUp

; for left and right, we're gonna 
; set the horizontal speed, and then do
; a single HMOVE.  We'll use X to hold the
; horizontal speed, then store it in the 
; appropriate register


;assume horiz speed will be zero
	LDX #0	

	LDA #%01000000	;Left?
	BIT SWCHA 
	BNE SkipMoveLeft
	LDX #$10	;a 1 in the left nibble means go left
SkipMoveLeft
	
	LDA #%10000000	;Right?
	BIT SWCHA 
	BNE SkipMoveRight
	LDX #$F0	;a -1 in the left nibble means go right...
SkipMoveRight
			;(in 4 bits, using "two's complement 
			; notation", binary 1111 = decimal -1
			; (which we write there as hex F --
			; confused?))


	STX HMP0	;set the move for Player 0
	STX HMM0	; ... and Missile (sword) 0


; while we're at it, change the color of the background
; if the button is pressed (making sure D6 of VBLANK has
; appropriately set above) We'll set the background color
; to the vertical position, since that will be changing 
; a lot but we can still control it.

	LDA INPT4		;read button input
	BMI ButtonNotPressed	;skip if button not pressed
	LDA P0YPosFromBot		;must be pressed, get YPos
	STA COLUBK		;load into bgcolor
ButtonNotPressed

	STA WSYNC	
	STA HMOVE 	
	
WaitForVblankEnd
	LDA INTIM	
	BNE WaitForVblankEnd	
	LDY #191 	
	STA WSYNC
	STA VBLANK  	


;main scanline loop...
;
;(this probably ends the "new code" section of today's
; lesson...)


ScanLoop 
	STA WSYNC 	

	LDA GRP0Next
	STA GRP0
	LDA ENAM0Next
	STA ENAM0
	
; here the idea is that P0LinesLeft
; is zero if the line isn't being drawn now,
; otherwise it's however many lines we have to go

CheckActivateFencer
	CPY P0YPosFromBot
	BNE SkipActivateFencer
	LDA #14
	STA P0LinesLeft
SkipActivateFencer

;turn player graphics off then see if there's a line of sprite to draw
	LDA #0		
	STA GRP0Next
;
;if the P0LinesLeft is non zero,
;we're drawing it
;
	LDX P0LinesLeft 
	BEQ FinishFencer
IsFencerOn	
	LDA FencerLow-1,X		
	STA GRP0Next
	DEC P0LinesLeft
	CPX LowSwordOffset 	; X still holds *old* P0LinesLeft, the one for the current line
	BNE DeactivateSword	; Don't activate Missile Register if not equal
ActivateSword
	LDA #2
	STA ENAM0Next
	JMP FinishFencer
DeactivateSword
	LDA #0
	STA ENAM0Next
FinishFencer

	DEY		
	BNE ScanLoop	

	LDA #2		
	STA WSYNC  	
	STA VBLANK 	
	LDX #30		
OverScanWait
	STA WSYNC
	DEX
	BNE OverScanWait
	JMP  MainLoop      

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
	
	org $FFFC
	.word Start		; NMI
	.word Start		; RESET
	.word Start		; IRQ
