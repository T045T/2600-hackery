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

JUMP_HEIGHT = 14

P0InitialX = 16
P1InitialX = 144

P0YFromBot      .byte
P0LinesLeft     .byte
P0SwordYFromBot .byte
P0XPos          .byte
P0SwordX        .byte
P0XVel          .byte
P0XPosBuf       .byte
P0YVel          .byte
P0YPosBuf       .byte

P1YFromBot      .byte
P1LinesLeft     .byte
P1SwordYFromBot .byte
P1XPos          .byte
P1SwordX        .byte
P1XVel          .byte
P1XPosBuf       .byte
P1YVel          .byte
P1YPosBuf       .byte

P0Status1       .byte
P1Status1       .byte

                        ;;  D1D0 : Current animation frame (4 frames each) - always back to 0 for standing, make sword shorter for the other frames
Status1_Jumping = 2     ;;  D2   : Jumping?
Status1_Leftfacing = 3  ;;  D3 : 0 if facing right, 1 if left (aligned to simply dump P0Status1 into REFP0)
                        ;;  D5D4 : Counter for stance:
Status1_JumpKicking = 4 ;;   0 0 : Low - if Jumping, DIVE!
Status1_MidStance = 4   ;;   0 1 : Med - if Jumping, KICK!
Status1_HighStance = 5  ;;   1 0 : High
Status1_SwordThrown = 6 ;;  D6: SwordThrown (if 1, don't touch P0SwordYFromBot or HMM0 for Joystick events, sword is taken care of by physics - haha, like we have physics)
Status1_Reset = 7       ;;  D7: ResetPlayer (if 1, reset Player to his edge of the screen)
                        ;;  From left to right, i.e. foo = #%D7D6D5D4D3D2D1D0

P0Status2       .byte
P1Status2       .byte
Temp            .byte
LongTemp        .word           ;used as the second part of a 16bit address when we need to temporarily store one!

Status2_Stance_Debounce = 0
Status2_Jump_Debounce = 1


;;; Graphics variables

;;; these hold the values that are pushed to the corresponding registers on the next scanline, so the memory fetch
;;; and related calculations don't cause us to miss the start of the line
GRP0Next        .byte
GRP1Next        .byte
PF0Next         .byte
PF1Next         .byte
PF2Next         .byte
ENAM0Next       .byte
ENAM1Next       .byte

;;; These hold pointers to the sprites, stored so the pointer location is the bottom of the sprite
P0Sprite        .word
P1Sprite        .word

PF0Base         .word
PF1Base         .word
PF2Base         .word

CurrentLine     .byte
CurrentScreen   .byte           ; Incremented when moving one screen to the right, decremented when moving to the left
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
        STA COLUBK      ;start with black background
        LDA #$82
        STA COLUPF
        LDA #66
        STA COLUP0
        LDA #$1E
        STA COLUP1
        LDA #1                  ; Activate playfield Reflection
        STA CTRLPF
;Setting some variables...
        LDA #40
        STA P0YFromBot  ;Initial Y Position
        STA P1YFromBot

        LDA #P0InitialX
        STA P0XPos
        LDA #P1InitialX
        STA P1XPos              ; Initial X Positions

        LDA #$30
        STA NUSIZ0      ; Missile is 8 color clocks wide, player normal
        STA NUSIZ1      ; Missile is 8 color clocks wide, player normal

        LDA #%10000000          ; Reset P0
        STA P0Status1
        LDA #%10001000          ; Reset P1, and have him reflected
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

P0Gravity
        LDA P0XPos
        JSR CheckPFPixel        ; Is there a Playfield pixel one scanline below the player?
        LDA P0YFromBot
        CLC
        SBC #14
        TAY
        LDA (LongTemp),Y        ; Longtemp contains the base address of the current playfield sprite
        BIT Temp                ; Check the bit our player is above for a collision
        BNE P0NoFall            ; If there is one, there's ground, don't fall.
        DEC P0YVel              ; If not, increase falling speed
        JMP P0GravityDone
P0NoFall
        LDA #0
        STA P0YVel              ; We hit some sort of ground, stop moving down
        clearbit Status1_Jumping, P0Status1
P0Rise
        LDA P0YFromBot
        CLC
        SBC #13
        TAY
        LDA (LongTemp),Y
        BIT Temp
        BEQ P0GravityDone       ; If not, we're done
        INC P0YFromBot          ; If so, raise player by one scanline
        JMP P0Rise              ;
P0GravityDone
P1Gravity
        LDA P1XPos
        JSR CheckPFPixel        ; Is there a Playfield pixel one scanline below the player?
        LDA P1YFromBot
        CLC
        SBC #14
        TAY
        LDA (LongTemp),Y
        BIT Temp
        BNE P1NoFall            ; If so, there's ground, don't fall.
        DEC P1YVel              ; If not, increase falling speed
        JMP P1GravityDone
P1NoFall
        LDA #0
        STA P1YVel              ; We hit some sort of ground, stop moving down
        clearbit Status1_Jumping, P1Status1
P1Rise
        LDA P1YFromBot
        CLC
        SBC #13
        TAY
        LDA (LongTemp),Y
        BIT Temp
        BEQ P1GravityDone       ; If not, we're done
        INC P1YFromBot          ; If so, raise player by one scanline
        JMP P1Rise              ;
P1GravityDone
        CLC

;;; Controls:
;;; Just check for each direction whether the Joystick has been pushed in that direction and manipulate
;;; Player coordinates accordingly.

        ifbit SWCHA_P0Up, SWCHA, P0SkipMoveUp ; Bits for pushed directions in SWCHA are *un*set
        ifbit Status1_Jumping, P0Status1, P0SkipMoveUp
        LDA #JUMP_HEIGHT
        STA P0YVel
        setbit Status1_Jumping, P0Status1
P0SkipMoveUp

        ifbit SWCHA_P0Left, SWCHA, P0SlowLeft
        setbit Status1_Leftfacing, P0Status1
P0MoveLeft
        LDA #248
        CMP P0XVel              ; Is Xvel already -8?
        BEQ P0SkipMoveLeft      ; If so, don't change it
        DEC P0XVel
        JMP P0SkipMoveLeft
P0SlowLeft                      ; If left wasn't pressed, slow down
        LDA P0XVel
        BPL P0SkipMoveLeft      ; Only slow down until at 0
        INC P0XVel
P0SkipMoveLeft

        ifbit SWCHA_P0Right, SWCHA, P0SlowRight
        clearbit Status1_Leftfacing, P0Status1
P0MoveRight
        LDA #8
        CMP P0XVel              ; Is XVel already 8?
        BEQ P0SkipMoveRight     ; If so, we're done
        INC P0XVel              ; If not, increase it!
        JMP P0SkipMoveRight
P0SlowRight
        LDA P0XVel
        BMI P0SkipMoveRight
        BEQ P0SkipMoveRight
        DEC P0XVel
P0SkipMoveRight

        ;; Now, check P1
        ifbit SWCHA_P1Up, SWCHA, P1SkipMoveUp
        ifbit Status1_Jumping, P1Status1, P1SkipMoveUp
        LDA #JUMP_HEIGHT
        STA P1YVel
        setbit Status1_Jumping, P1Status1
P1SkipMoveUp

        ifbit SWCHA_P1Left, SWCHA, P1SlowLeft
        setbit Status1_Leftfacing, P1Status1
P1MoveLeft
        LDA #248
        CMP P1XVel              ; Is Xvel already -8?
        BEQ P1SkipMoveLeft      ; If so, don't change it
        DEC P1XVel
        JMP P1SkipMoveLeft
P1SlowLeft                      ; If left wasn't pressed, slow down
        LDA P1XVel
        BPL P1SkipMoveLeft      ; Only slow down until at 0
        INC P1XVel
P1SkipMoveLeft

        ifbit SWCHA_P1Right, SWCHA, P1SlowRight
        clearbit Status1_Leftfacing, P1Status1
P1MoveRight
        LDA #8
        CMP P1XVel              ; Is XVel already 8?
        BEQ P1SkipMoveRight     ; If so, we're done
        INC P1XVel              ; If not, increase it!
        JMP P1SkipMoveRight
P1SlowRight
        LDA P1XVel
        BMI P1SkipMoveRight
        BEQ P1SkipMoveRight
        DEC P1XVel
P1SkipMoveRight

        CLC                     ; Clear Carry bit, so it doesn't confuse any of the following calculations


;;; Use the Joystick for stance switching (pushing down cycles through stances)

        ifbit SWCHA_P0Down, SWCHA, P0SkipStanceSwitch
        ifbit Status2_Stance_Debounce, P0Status2, P0StanceSwitchDone
        ifbit Status1_MidStance, P0Status1, P0IsMid
        ifbit Status1_HighStance, P0Status1, P0IsHigh
P0IsLow                         ; Go from low to mid...
        LDA P0Status1
        AND #%11001111          ; Clear both stance bits
        ORA #%00010000          ; Only set the "Mid" stance bit
        JMP P0ChangedStance
P0IsMid                         ; From mid to high...
        LDA P0Status1
        AND #%11001111
        ORA #%00100000
        JMP P0ChangedStance
P0IsHigh                        ; And back to low.
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
P1IsLow                         ; Go from low to mid...
        LDA P1Status1
        AND #%11001111
        ORA #%00010000
        JMP P1ChangedStance
P1IsMid                         ; From mid to high...
        LDA P1Status1
        AND #%11001111
        ORA #%00100000
        JMP P1ChangedStance
P1IsHigh                                ; And back to low.
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

;;; Use the Vel and PosBuf variables to calculate the new screen positions for the two players
;;;
;;; Vel holds the velocity of the corresponding player in X or Y direction - the unit for velocity is 1/4 pixels
;;; ("Pixels" meaning color clocks in X direction, Kernel units - 2 scanlines - in Y direction)
;;;
;;; PosBuf holds the intermediate position value, since storing the entire position in quarter pixels would entail 16 bit arithmetic
;;; As soon as PosBuf grows above 4 (or -4), multiples of 4 are converted to "real" pixels, added to the Pos variable,
;;; and removed from PosBuf
CalculatePlayerMovement
StartP0XVel
        CLC
        LDA P0XPosBuf
        ADC P0XVel
        STA P0XPosBuf
        CMP #4
        BCS P0XStartCalc        ; If P0XPosBuf >= 4, calculate stuff
P0XCheckLessThanMinus4
        CMP #252
        BEQ P0XStartCalc        ; If P0XPosBuf is exactly -4 (252 unsigned), calculate stuff
        BCS EndP0XVel           ; If P0XPosBuf is bigger than 252 (i.e. -4), don't to anything
P0XStartCalc
        STA Temp                ; Divide by 4 and sign extend
        ASL                     ; Store leftmost bit in Carry register
        ROR Temp                ; ROR rotates the carry register into the leftmost bit
        LDA P0XPosBuf
        ASL                     ; Put leftmost bit back into the Carry register
        ROR Temp                ; Temp now contains the signed value of P0XPosBuf / 4
        LDA P0XPos
        CLC
        ADC Temp
        STA P0XPos
        LDA Temp
        ASL
        ASL
        STA Temp
        LDA P0XPosBuf
        SEC
        SBC Temp
        STA P0XPosBuf
EndP0XVel
StartP0YVel
        CLC
        LDA P0YPosBuf
        ADC P0YVel
        STA P0YPosBuf
        CMP #0
        BEQ EndP0YVel           ; If PosBuf == 0, don't move the player
        BPL P0YStartCalc
P0YCheckLessThanMinus4
        CMP #252
        BEQ P0YStartCalc
        BCS EndP0YVel           ; If P0YPosBuf is bigger than 252 (i.e. -4), don't to anything
P0YStartCalc
        STA Temp                ; Divide by 4 and sign extend
        ASL                     ; Store leftmost bit in Carry register
        ROR Temp                ; ROR rotates the carry register into the leftmost bit
        LDA P0YPosBuf
        ASL                     ; Put leftmost bit back into the Carry register
        ROR Temp                ; Temp now contains the signed value of P0YPosBuf / 4
        LDA P0YFromBot
        CLC
        ADC Temp
        STA P0YFromBot
        LDA Temp
        ASL
        ASL
        STA Temp
        LDA P0YPosBuf
        SEC
        SBC Temp
        STA P0YPosBuf
EndP0YVel
StartP1XVel
        CLC
        LDA P1XPosBuf
        ADC P1XVel
        STA P1XPosBuf
        CMP #4
        BCS P1XStartCalc        ; If P1XPosBuf >= 4, calculate stuff
P1XCheckLessThanMinus4
        CMP #252
        BEQ P1XStartCalc        ; If P1XPosBuf is exactly -4 (252 unsigned), calculate stuff
        BCS EndP1XVel           ; If P1XPosBuf is bigger than 252 (i.e. -4), don't to anything
P1XStartCalc
        STA Temp                ; Divide by 4 and sign extend
        ASL                     ; Store leftmost bit in Carry register
        ROR Temp                ; ROR rotates the carry register into the leftmost bit
        LDA P1XPosBuf
        ASL                     ; Put leftmost bit back into the Carry register
        ROR Temp                ; Temp now contains the signed value of P1XPosBuf / 4
        LDA P1XPos
        CLC
        ADC Temp
        STA P1XPos
        LDA Temp
        ASL
        ASL
        STA Temp
        LDA P1XPosBuf
        SEC
        SBC Temp
        STA P1XPosBuf
EndP1XVel
StartP1YVel
        CLC
        LDA P1YPosBuf
        ADC P1YVel
        STA P1YPosBuf
        CMP #0
        BEQ EndP1YVel           ; If PosBuf == 0, don't move the player
        BPL P1YStartCalc
P1YCheckLessThanMinus4
        CMP #252
        BEQ P1YStartCalc
        BCS EndP1YVel           ; If P1YPosBuf is bigger than 252 (i.e. -4), don't to anything
P1YStartCalc
        STA Temp                ; Divide by 4 and sign extend
        ASL                     ; Store leftmost bit in Carry register
        ROR Temp                ; ROR rotates the carry register into the leftmost bit
        LDA P1YPosBuf
        ASL                     ; Put leftmost bit back into the Carry register
        ROR Temp                ; Temp now contains the signed value of P1YPosBuf / 4
        LDA P1YFromBot
        CLC
        ADC Temp
        STA P1YFromBot
        LDA Temp
        ASL
        ASL
        STA Temp
        LDA P1YPosBuf
        SEC
        SBC Temp
        STA P1YPosBuf
EndP1YVel

        ;; Make sure Players don't leave the screen edges!
        LDA #9
        CMP P0XPos              ; If 9 < P0XPos...
        BCC P0XRightBound       ; We're good. Else...
        STA P0XPos              ; Make XPos 9
P0XRightBound
        LDA #161
        CMP P0XPos
        BCS P0BoundsDone
        STA P0XPos
P0BoundsDone
        LDA #9
        CMP P1XPos              ; If 9 < P1XPos...
        BCC P1XRightBound       ; We're good. Else...
        STA P1XPos              ; Make XPos 9
P1XRightBound
        LDA #161
        CMP P1XPos
        BCS P1BoundsDone
        STA P1XPos
P1BoundsDone
EndCalculatePlayerMovement
        CLC

CheckPlayerStatus
        LDA P0Status1           ; Set reflection bit for both players according to their status byte
        STA REFP0
        LDA P1Status1
        STA REFP1
P0PosStart
        ifbit Status1_Jumping, P0Status1, P0Jumping
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
        ifbit Status1_SwordThrown, P0Status1, P1PosStart           ; If the sword has been thrown, skip. Otherwise...
        STX P0SwordYFromBot                                ; set it to the appropriate line
        CLC
        JMP P1PosStart
P0Jumping
        LDX #<FencerJump
        STX P0Sprite
        LDX #>FencerJump
        STX P0Sprite+1
        ifbit Status1_SwordThrown, P0Status1, P1PosStart
        LDX #255
        STX P0SwordYFromBot
P1PosStart
        ifbit Status1_Jumping, P1Status1, P1Jumping
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
        STX P1SwordYFromBot                                ; set it to the appropriate line
        CLC
        JMP P0Reset
P1Jumping
        LDX #<FencerJump
        STX P1Sprite
        LDX #>FencerJump
        STX P1Sprite+1
        ifbit Status1_SwordThrown, P1Status1, P0Reset
        LDX #255
        STX P1SwordYFromBot
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
        ifbit Status1_SwordThrown, P0Status1, P0SwordSkip       ; If sword has been thrown, don't position it with the player
        LDX P0XPos
        ifbit Status1_Leftfacing, P0Status1, P0Mirrored
        TXA
        ADC #9
        TAX
        JMP P0SwordDone
P0Mirrored
        TXA
        SBC #6
        CLC                     ; Clear Carry bit so the subtraction above doesn't confuse P1's sword
        TAX
P0SwordDone
        STX P0SwordX
P0SwordSkip

P1SwordReset
        ifbit Status1_SwordThrown, P1Status1, P1SwordSkip       ; If sword has been thrown, don't position it with the player
        LDX P1XPos
        ifbit Status1_Leftfacing, P1Status1, P1Mirrored
        TXA
        ADC #9
        TAX
        JMP P1SwordDone
P1Mirrored
        TXA
        SBC #6
        CLC                     ; Clear Carry bit so the subtraction above doesn't confuse anything down the line
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

        LDA #0                  ; Make sure no old values from the bottom of the screen linger
        STA GRP0Next            ; in GRP0Next and GRP1Next
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

;;; Load playfield register values for the first line
        LDY #95                 ; Halved because we're now using a two-line kernel
        STY CurrentLine
        LDA (PF0Base),Y
        STA PF0
        LDA (PF1Base),Y
        STA PF1Next
        LDA (PF2Base),Y
        STA PF2
        LDA #0                  ; Set A and Y to 0, since were never ever going to draw the swords
        TAY                     ; in the very first line
WaitForVblankEnd
        LDA INTIM
        BNE WaitForVblankEnd
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
        STA ENAM0               ; [3] + 3
        STY ENAM1               ; [6] + 3
        LDA GRP0Next            ; [9] + 3
        STA GRP0                ; [12] + 3

        LDA GRP1Next            ; [15] + 3
        STA GRP1                ; [18] + 3

        ;; All player and missile registers set after 21 cycles

        ;; PF0 and PF2 are set at the end of the loop, and PF1
        ;; doesn't start drawing until cycle 28, so set it here
        ;; (68 color cycles HBLANK + 4 Bits * 4 color cycles from PF0
        ;;  => color cycle 84, 84 / 3 = 28 )
        LDA PF1Next             ; [21] + 3
        STA PF1                 ; [24] + 3

        LDX CurrentLine         ; [27] + 3

        ;; Skipdraw (as per Thomas Jentzsch - http://www.randomterrain.com/atari-2600-memories-tutorial-andrew-davie-23.html)

P0SkipDraw
        TXA                     ; [30] + 2
        SEC                     ; [32] + 2
        SBC P0YFromBot  ; [34] + 3
        ADC #15                 ; [37] + 2
        BCC SkipP0              ; [39] + 2 (3)
        TAY                     ; [41] + 2
        LDA (P0Sprite),Y        ; [43] + 5
        STA GRP0Next            ; [48] + 3
        JMP P1SkipDraw          ; [51] + 3
SkipP0                          ; [42]
        DEC $2D                 ; [42] + 5 // NOP
        DEC $2D                 ; [47] + 5 // NOP
        SEC                     ; [52] + 2 // Only got here because carrry bit was cleared, so re-set it for P1SkipDraw
P1SkipDraw                      ; [54]
        TXA                     ; [54] + 2
        SBC P1YFromBot  ; [56] + 3
        ADC #15                 ; [59] + 2
        BCC SkipP1              ; [61] + 2 (3)
        TAY                     ; [63] + 2
        LDA (P1Sprite),Y        ; [65] + 5
        STA GRP1Next            ; [70] + 3
        JMP EndSkipDraw         ; [73] + 3
SkipP1                          ; [64]
        DEC $2D                 ; [64] + 5 // NOP
        DEC $2D                 ; [69] + 5 // NOP
        NOP                     ; [74] + 2
EndSkipDraw                     ; [76]

P0Missile
        CPX P0SwordYFromBot     ; [76] + 3
        PHP                     ; [79] + 3
P1Missile
        CPX P1SwordYFromBot     ; [82] + 3
        PHP                     ; [85] + 3

                                ; [90]
        LDA #0
        STA ENAM0               ; Disable Missiles (Swords) during the second line, so they're
        STA ENAM1               ; nice and thin
        DEC $2D                 ; YARRR! Here be booty! 9 cycles! \o/
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

DrawPF                          ; [105]
        LDY.w CurrentLine       ; [105] + 4 // Load current line into Y
                                ;           // (it's also in X, but the addressing mode we need below only works with Y...)
        ;; the .w above changes the instruction from zero-width (3 cycles)
        ;; to absolute addressing (4 cycles), so we arrive at the end
        ;; right at cycle 152
        DEC CurrentLine         ; [109] + 5
        BEQ StartOverscan       ; [114] + 2 (3) // If we're at line 0, the playfield is done, go into overscan
        LDA (PF0Base),Y         ; [116] + 5
        TAX                     ; [121] + 2 // Store PF0 value in X, we can't set it yet, because it has yet to be drawn in the right screen half
        LDA (PF1Base),Y         ; [123] + 5
        STA PF1Next             ; [128] + 3
        LDA (PF2Base),Y         ; [131] + 5

        ;; PF2 is drawn during cycles 38-60, and 114-136
        ;; In other words, the TIA is just done drawing it when we set
        ;; it here for the next two-line interval :)

        STA PF2                 ; [136] + 3

        ;; Get the ENAM0 and ENAM1 values from the stack
        ;; ENAM1 was pushed last and goes into the Y
        ;; register, ENAM0 stays in the accumulator
        PLA                     ; [139] + 4
        TAY                     ; [143] + 2
        PLA                     ; [145] + 4

        ;; This is the latest in a scanline we can set PF0,
        ;; and it appears to work without disturbing the current line's
        ;; drawing
        STX PF0                 ; [149] + 3
        JMP ScanLoop            ; [152] + 3
EndScanLoop

StartOverscan
        LDA #2
        STA WSYNC
        STA VBLANK
        LDA #35                 ; 30 Lines of overscan - 30*76 / 64 = 35.something
        STA TIM64T
        PLA                     ; These are here to make sure the stack doesn't overflow
        PLA                     ; (the pushed ENAMn values reside there)
OverScanWait
        LDA INTIM
        BNE OverScanWait
        STA WSYNC
        JMP  MainLoop

;;; Collision code goes here (should go with control code actually, so that there's no stuttering)
;;;   Maybe move control code down here?
;;; Basic idea: (P0XPos - 9) >> 2 => Playfield pixel position (same for P0XPos - 1 and P1XPos)
;;; Build table(s) with bit indices (2x40 Bytes)
;;; Another set of tables with PF memory locations (2x10 Bytes), addressed by
;;; playfield pixel pos >> 2 (flexible enough for both reflected and unreflected)
;;; ==> Fetch data from Playfield memory (at P0YFromBot - 1)

;;; Subroutine to check inputs
;;; Inputs: X = player ID (0 or 1) - used as an offset to all parameters that change
;;;         Temp = the 4 control bits (up, down, left, right) for the given player
;;; TODO(nberg): Check that the layout of status bytes and other memory supports using an offset to switch
;;;              between players!

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

;;; Fetches the bit mask and sprite pointer for a given playfield location.
;;; Input:
;;; A : X coordinate of the playfield location
;;;
;;; Output:
;;; Pixel Mask in Temp, Pointer to the Correct Playfield Sprite in LongTemp
CheckPFPixel SUBROUTINE
        ;; P[0,1]XPos is 2 more than we expect - since PosPlayer and PosMissile
        ;; take coordinates between 9 and 161, but we want zero-based indices for
        ;; the 160 possible X values, hence the coordinates we're working with in
        ;; here should be between 7 and 159, hence we subtract 2
        SBC #2

        LSR
        LSR                     ; divide X coordinate by 4 - playfield pixels are 4 Color Clocks wide
        TAX
        LDA PFBitMasks,X      ; Get the bit mask for the playfield pixel we're looking at
        STA Temp                ; Bit mask goes into Temp
        TXA
        LSR
        LSR                     ; divide by 4 again to determine the playfield register id
        TAX
        LDA PFRegisterIndices,X ; Load the Zero-Page address of the correct PF Sprite pointer into A
        TAX
        LDA #0,X                ; Load the location of the current Sprite (LSB) into A
        STA LongTemp            ; Store into LongTemp so we can use Zero Page Indirect addressing mode
        INX
        LDA #0,X                ; Load the MSB into A
        STA LongTemp + 1        ; Store MSB into LongTemp + 1
        RTS


        ;;; Output remaining ROM space
        echo "------", [$FFFA - *]d, "bytes between End of Code and End of Cartridge"

        org $FD00
PFRegisterIndices
        .byte PF0Base                   ; 1
        .byte PF1Base                   ; 2
        .byte PF1Base                   ; 3
        .byte PF2Base                   ; 4
        .byte PF2Base                   ; 5
        .byte PF2Base                   ; 6
        .byte PF2Base                   ; 7
        .byte PF1Base                   ; 8
        .byte PF1Base                   ; 9
        .byte PF0Base                   ; 10

;;; The bit indices of the 40 Playfield pixels. Since the Playfield is stored in multiple registers,
;;; the indices are weird
PFBitMasks
        .byte %00010000        ; 1
        .byte %00100000        ; 2
        .byte %01000000        ; 3
        .byte %10000000        ; 4
        .byte %10000000        ; 5
        .byte %01000000        ; 6
        .byte %00100000        ; 7
        .byte %00010000        ; 8
        .byte %00001000        ; 9
        .byte %00000100        ; 10
        .byte %00000010        ; 11
        .byte %00000001        ; 12
        .byte %00000001        ; 13
        .byte %00000010        ; 14
        .byte %00000100        ; 15
        .byte %00001000        ; 16
        .byte %00010000        ; 17
        .byte %00100000        ; 18
        .byte %01000000        ; 19
        .byte %10000000        ; 20
        .byte %10000000        ; 21
        .byte %01000000        ; 22
        .byte %00100000        ; 23
        .byte %00010000        ; 24
        .byte %00001000        ; 25
        .byte %00000100        ; 26
        .byte %00000010        ; 27
        .byte %00000001        ; 28
        .byte %00000001        ; 29
        .byte %00000010        ; 30
        .byte %00000100        ; 31
        .byte %00001000        ; 32
        .byte %00010000        ; 33
        .byte %00100000        ; 34
        .byte %01000000        ; 35
        .byte %10000000        ; 36
        .byte %10000000        ; 37
        .byte %01000000        ; 38
        .byte %00100000        ; 39
        .byte %00010000        ; 40

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

FencerJump
        .byte %00000000
        .byte %00011000  ;   XX   ;
        .byte %01111110  ; XXXXXX ;
        .byte %01111110  ; XXXXXX ;
        .byte %11111111  ;XXXXXXXX;
        .byte %11111111  ;XXXXXXXX;
        .byte %01111110  ; XXXXXX ;
        .byte %01111110  ; XXXXXX ;
        .byte %00011000  ;   XX   ;
        .byte %00000000  ;        ;
        .byte %00000000  ;        ;
        .byte %00000000  ;        ;
        .byte %00000000  ;        ;
        .byte %00000000  ;        ;
        .byte %00000000  ;        ;

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

;;; Output remaining ROM space
        echo "------", [$FFFA - *]d, "bytes between End of Sprites and End of Cartridge"

;;; Bureaucracy!
        org $FFFC
        .word Start             ; NMI
        .word Start             ; RESET
        .word Start             ; IRQ
