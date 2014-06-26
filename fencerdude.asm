; move a sprite with the joystick

	processor 6502
	include vcs.h

	MAC DO_WITH_BITMASK
.INDEX SET {2}
	IF .INDEX == 0
	{1} #%00000001
	ENDIF
	IF .INDEX == 1
	{1} #%00000010
	ENDIF
	IF .INDEX == 2
	{1} #%00000100
	ENDIF
	IF .INDEX == 3
	{1} #%00001000
	ENDIF
	IF .INDEX == 4
	{1} #%00010000
	ENDIF
	IF .INDEX == 5
	{1} #%00100000
	ENDIF
	IF .INDEX == 6
	{1} #%01000000
	ENDIF
	IF .INDEX == 7
	{1} #%10000000
	ENDIF
	ENDM

	MAC do_with_bitmask_inv
.INDEX SET {2}
	if .INDEX == 0
	{1} #%11111110
	endif
	if .INDEX == 1
	{1} #%11111101
	endif
	if .INDEX == 2
	{1} #%11111011
	endif
	if .INDEX == 3
	{1} #%11110111
	endif
	if .INDEX == 4
	{1} #%11101111
	endif
	if .INDEX == 5
	{1} #%11011111
	endif
	if .INDEX == 6
	{1} #%10111111
	endif
	if .INDEX == 7
	{1} #%01111111
	endif
	ENDM
	
;;; Usage: unlessbit [bit number] [variable] [label to jump to if bit set]
	MAC unlessbit
	DO_WITH_BITMASK LDA, {1}
	BIT {2}
	BEQ {3}
	ENDM

;;; Usage: ifbit [bit number] [variable] [label to jump to if bit set]
	MAC ifbit
	DO_WITH_BITMASK LDA, {1}
	BIT {2}
	BNE {3}
	ENDM
	
;;; Usage: setbit [bit number] [variable]
	MAC setbit
	LDA {2}
	DO_WITH_BITMASK ORA, {1}
	STA {2}
	ENDM

;;; Usage: clearbit [bit number] [variable]
	MAC clearbit
	LDA {2}
	do_with_bitmask_inv AND, {1}
	STA {2}
	ENDM


	SEG.U variables
	ORG $80
;;; Bit Names:
SWCHA_P1Up = 0
SWCHA_P1Down = 1
SWCHA_P1Left = 2
SWCHA_P1Right = 3
SWCHA_P0Up = 4
SWCHA_P0Down = 5
SWCHA_P0Left = 6
SWCHA_P0Right = 7
	
LowSwordOffset = 8
MidSwordOffset = 4
HighSwordOffset = 0

P0InitialX = 16
P1InitialX = 144
	
P0YFromBot	.byte
P0LinesLeft 	.byte
P0SwordYFromBot	.byte
P0XPos 		.byte
P0SwordX 	.byte

P1YFromBot 	.byte
P1LinesLeft 	.byte
P1SwordYFromBot	.byte
P1XPos 		.byte
P1SwordX 	.byte

P0Status1 	.byte
P1Status1 	.byte

			;;  D1D0 : Current animation frame (4 frames each) - always back to 0 for standing, make sword shorter for the other frames
Status1_Jumping = 2	;;  D2   : Jumping?
Status1_Leftfacing = 3	;;  D3 : 0 if facing right, 1 if left (aligned to simply dump P0Status1 into REFP0)
			;;  D5D4 : Counter for stance:
Status1_JumpKicking = 4	;;   0 0 : Low - if Jumping, DIVE!
Status1_MidStance = 4	;;   0 1 : Med - if Jumping, KICK!
Status1_HighStance = 5	;;   1 0 : High
Status1_SwordThrown = 6	;;  D6: SwordThrown (if 1, don't touch P0SwordYFromBot or HMM0 for Joystick events, sword is taken care of by physics - haha, like we have physics)
Status1_Reset = 7	;;  D7: ResetPlayer (if 1, reset Player to his edge of the screen)
			;;  From left to right, i.e. foo = #%D7D6D5D4D3D2D1D0

P0Status2	.byte
P1Status2	.byte

Status2_Stance_Debounce = 0


;;; Graphics variables

;;; these hold the values that are pushed to the corresponding registers on the next scanline, so the memory fetch
;;; and related calculations don't cause us to miss the start of the line
GRP0Next 	.byte
GRP1Next 	.byte
PF0Next 	.byte
PF1Next 	.byte
PF2Next 	.byte
ENAM0Next 	.byte
ENAM1Next 	.byte

;;; These hold pointers to the sprites, stored so the pointer location is the bottom of the sprite
P0Sprite 	.word
P1Sprite 	.word

PF0Base 	.word
PF1Base 	.word
PF2Base 	.word

CurrentLine 	.byte
CurrentScreen 	.byte		; Incremented when moving one screen to the right, decremented when moving to the left
				; Levels are symmetrical, so if CurrentScreen is negative, use NOT(CurrentScreen)+1 as screen index


	SEG code
	ORG $F000
	
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
	LDA #1			; Activate playfield Reflection
	STA CTRLPF
;Setting some variables...
	LDA #40
	STA P0YFromBot	;Initial Y Position
	STA P1YFromBot

	LDA #P0InitialX
	STA P0XPos
	LDA #P1InitialX
	STA P1XPos		; Initial X Positions
	
	LDA #$30
	STA NUSIZ0	; Missile is 8 color clocks wide, player normal
	STA NUSIZ1	; Missile is 8 color clocks wide, player normal

	LDA #%10000000		; Reset P0
	STA P0Status1
	LDA #%10001000		; Reset P1, and have him reflected
	STA P1Status1


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


;;; Controls:
;;; Just check for each direction whether the Joystick has been pushed in that direction and manipulate
;;; Player coordinates accordingly. 

	ifbit SWCHA_P0Up, SWCHA, P0SkipMoveUp ; Bits for pushed directions in SWCHA are *un*set
	INC P0YFromBot
P0SkipMoveUp

	ifbit SWCHA_P0Left, SWCHA, P0SkipMoveLeft
	setbit Status1_Leftfacing, P0Status1 
	LDA P0XPos
	CMP #9
	BEQ P0SkipMoveLeft
	DEC P0XPos
P0SkipMoveLeft

	ifbit SWCHA_P0Right, SWCHA, P0SkipMoveRight
	clearbit Status1_Leftfacing, P0Status1
	LDA P0XPos
	CMP #161
	BEQ P0SkipMoveRight
	INC P0XPos
P0SkipMoveRight

	;; Now, check P1
	ifbit SWCHA_P1Up, SWCHA, P1SkipMoveUp
	INC P1YFromBot
P1SkipMoveUp

	ifbit SWCHA_P1Left, SWCHA, P1SkipMoveLeft
	setbit Status1_Leftfacing, P1Status1
	LDA P1XPos
	CMP #9
	BEQ P1SkipMoveLeft
	DEC P1XPos
P1SkipMoveLeft
	
	ifbit SWCHA_P1Right, SWCHA, P1SkipMoveRight
	clearbit Status1_Leftfacing, P1Status1
	LDA P1XPos
	CMP #161
	BEQ P1SkipMoveRight
	INC P1XPos
P1SkipMoveRight

	CLC			; Clear Carry bit, so it doesn't confuse any of the following calculations

	
;;; Use the Joystick for stance switching (pushing down cycles through stances)

	ifbit SWCHA_P0Down, SWCHA, P0SkipStanceSwitch
	ifbit Status2_Stance_Debounce, P0Status2, P0StanceSwitchDone
	ifbit Status1_MidStance, P0Status1, P0IsMid
	ifbit Status1_HighStance, P0Status1, P0IsHigh
P0IsLow				; Go from low to mid...
	LDA P0Status1
	AND #%11001111		; Clear both stance bits
	ORA #%00010000		; Only set the "Mid" stance bit
	JMP P0ChangedStance
P0IsMid				; From mid to high...
	LDA P0Status1
	AND #%11001111
	ORA #%00100000
	JMP P0ChangedStance
P0IsHigh			; And back to low.
	LDA P0Status1
	AND #%11001111
P0ChangedStance
	STA P0Status1
	setbit Status2_Stance_Debounce, P0Status2
	JMP P0StanceSwitchDone
P0SkipStanceSwitch
	clearbit Status2_Stance_Debounce, P0Status2
P0StanceSwitchDone

	;; Use the Joystick for stance switching (pushing down cycles through stances)
	ifbit SWCHA_P1Down, SWCHA, P1SkipStanceSwitch
	ifbit Status2_Stance_Debounce, P1Status2, P1StanceSwitchDone
	ifbit Status1_MidStance, P1Status1, P1IsMid
	ifbit Status1_HighStance, P1Status1, P1IsHigh
P1IsLow				; Go from low to mid...
	LDA P1Status1
	AND #%11001111
	ORA #%00010000
	JMP P1ChangedStance
P1IsMid				; From mid to high...
	LDA P1Status1
	AND #%11001111
	ORA #%00100000
	JMP P1ChangedStance
P1IsHigh				; And back to low.
	LDA P1Status1
	AND #%11001111
	ORA #%00000000
P1ChangedStance
	STA P1Status1
	setbit Status2_Stance_Debounce, P1Status2
	JMP P1StanceSwitchDone
P1SkipStanceSwitch
	clearbit Status2_Stance_Debounce, P1Status2
P1StanceSwitchDone
	
CheckPlayerStatus
	LDA P0Status1		; Set reflection bit for both players according to their status byte
	STA REFP0
	LDA P1Status1
	STA REFP1
P0PosStart
	ifbit Status1_MidStance, P0Status1, P0MidPos
	ifbit Status1_HighStance, P0Status1, P0HighPos
P0LowPos
	LDX #<FencerLow
	STX P0Sprite
	LDX #>FencerLow
	STX P0Sprite+1
	LDA P0YFromBot
	SBC #LowSwordOffset
	JMP P0PosDone
P0MidPos
	LDX #<FencerMid
	STX P0Sprite
	LDX #>FencerMid
	STX P0Sprite+1
	LDA P0YFromBot
	SBC #MidSwordOffset
	JMP P0PosDone
P0HighPos
	LDX #<FencerHigh
	STX P0Sprite
	LDX #>FencerHigh
	STX P0Sprite+1
	LDA P0YFromBot
	SBC #HighSwordOffset
P0PosDone
	TAX
	ifbit Status1_SwordThrown, P0Status1, P1PosStart	   ; If the sword has been thrown, skip. Otherwise...
	STX P0SwordYFromBot				   ; set it to the appropriate line
	CLC
P1PosStart
	ifbit Status1_MidStance, P1Status1, P1MidPos
	ifbit Status1_HighStance, P1Status1, P1HighPos
P1LowPos
	LDX #<FencerLow
	STX P1Sprite
	LDX #>FencerLow
	STX P1Sprite+1
	LDA P1YFromBot
	SBC #LowSwordOffset
	JMP P1PosDone
P1MidPos
	LDX #<FencerMid
	STX P1Sprite
	LDX #>FencerMid
	STX P1Sprite+1
	LDA P1YFromBot
	SBC #MidSwordOffset
	JMP P1PosDone
P1HighPos
	LDX #<FencerHigh
	STX P1Sprite
	LDX #>FencerHigh
	STX P1Sprite+1
	LDA P1YFromBot
	SBC #HighSwordOffset
P1PosDone
	TAX
	ifbit Status1_SwordThrown, P1Status1, P0Reset        ; If the sword has been thrown, skip. Otherwise...
	STX P1SwordYFromBot				   ; set it to the appropriate line
	CLC

P0Reset
	unlessbit Status1_Reset, P0Status1, P0ResetDone
	LDA #P0InitialX
	STA P0XPos
P0ResetDone
	clearbit Status1_Reset, P0Status1
P1Reset
	unlessbit Status1_Reset, P1Status1, P1ResetDone
	LDA #P1InitialX
	STA P1XPos
P1ResetDone
	clearbit Status1_Reset, P1Status1


;;; Reset the swords every frame to account for possible turning around

P0SwordReset
	ifbit Status1_SwordThrown, P0Status1, P0SwordSkip	; If sword has been thrown, don't position it with the player
	LDX P0XPos
	ifbit Status1_Leftfacing, P0Status1, P0Mirrored
	TXA
	ADC #9
	TAX
	JMP P0SwordDone
P0Mirrored
	TXA
	SBC #6
	CLC			; Clear Carry bit so the subtraction above doesn't confuse P1's sword
	TAX
P0SwordDone
	STX P0SwordX
P0SwordSkip

P1SwordReset
	ifbit Status1_SwordThrown, P1Status1, P1SwordSkip	; If sword has been thrown, don't position it with the player
	LDX P1XPos
	ifbit Status1_Leftfacing, P1Status1, P1Mirrored
	TXA
	ADC #9
	TAX
	JMP P1SwordDone
P1Mirrored
	TXA
	SBC #6
	CLC			; Clear Carry bit so the subtraction above doesn't confuse anything down the line
	TAX
P1SwordDone
	STX P1SwordX
P1SwordSkip


;;; Move all the objects to their positions
PositionP0
	LDA P0XPos
	LDX #0
	JSR PosPlayer
PositionP1
	LDA P1XPos
	LDX #1
	JSR PosPlayer
PositionM0
	LDA P0SwordX
	LDX #0
	JSR PosMissile
PositionM1
	LDA P1SwordX
	LDX #1
	JSR PosMissile

	STA WSYNC
	STA HMOVE
	
	LDA #0			; Make sure no old values from the bottom of the screen linger
	STA GRP0Next		; in GRP0Next and GRP1Next
	STA GRP1Next

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


	LDY #0
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
	SBC P0YFromBot	; [34] + 3
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
	SBC P1YFromBot	; [56] + 3
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
	CPX P0SwordYFromBot	; [76] + 3
	PHP			; [79] + 3
P1Missile
	CPX P1SwordYFromBot	; [82] + 3
	PHP			; [85] + 3

				; [90]
	LDA #0
	STA ENAM0		; Disable Missiles (Swords) during the second line, so they're
	STA ENAM1		; nice and thin
	DEC $2D			; YARRR! Here be booty! 9 cycles! \o/
	NOP
	NOP
	

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
	LDA #35			; 30 Lines of overscan - 30*76 / 64 = 35.something
	STA TIM64T
	PLA			; These are here to make sure the stack doesn't overflow
	PLA			; (the pushed ENAMn values reside there)
OverScanWait
	LDA INTIM
	BNE OverScanWait
	STA WSYNC
	JMP  MainLoop

	;; Code from BattleZone ( http://www.computerarcheology.com/wiki/wiki/Atari2600/BattleZone/Code )
	;; Commented Version from http://www.qotile.net/minidig/disassembly/unfinished.zip
	;;
	;; Positions an object horizontally
	;; Inputs: A = Desired position. (left screen edge is 9)
	;; X = Desired object to be positioned (Player / Missile 0 or 1, use appropriate subroutine for each)
	;; scanlines: If control comes on or before cycle 73 then 1 scanline is consumed.
	;; If control comes after cycle 73 then 2 scanlines are consumed.
	;; Outputs: X = unchanged
	;; A = Y = Fine Adjustment value.
PosPlayer SUBROUTINE
	CMP #$11                 ; Desired position >= $11
	BCS PlayerPositionOk           ; Y:
	SBC #$05                 ; Correct troubles with early RESP
	BCS PlayerPositionOk           ;
	ADC #$A5                 ;
PlayerPositionOk
	STA WSYNC                ;
.wait
	SBC #$0F                 ;
	BCS .wait                ; RESP loop
	
	EOR #$07                 ;
	ASL                      ;
	ASL                      ;
	ASL                      ;
	ASL                      ;
	TAY                      ; Y-> correct HMXX value
	STA RESP0,X           ; Position it!
	STA HMP0,X
	STA WSYNC             ;
	RTS                      ; done, that's all!

PosMissile SUBROUTINE
	CMP #$10                 ; Desired position >= $11
	BCS MissilePositionOk    ; Y:
	SBC #$04                 ; Correct troubles with early RESP
	BCS MissilePositionOk    ;
	ADC #$A5                 ;
MissilePositionOk
	STA WSYNC                ;
.wait
	SBC #$0F                 ;
	BCS .wait                ; RESP loop
	
	EOR #$07                 ;
	ASL                      ;
	ASL                      ;
	ASL                      ;
	ASL                      ;
	TAY                      ; Y-> correct HMXX value
	STA RESM0,X           ; Position it!
	STA HMM0,X
	STA    WSYNC             ;
	RTS                      ; done, that's all!
	
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
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000

	org $FE00
PF1Center
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000

PF2Center
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000

	org $FFFC
	.word Start		; NMI
	.word Start		; RESET
	.word Start		; IRQ
