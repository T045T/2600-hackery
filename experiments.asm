
  processor 6502
  include "vcs.h"
  include "macro.h"

schedule = #$ff
PF_REFL        = $1  ; mirror playfield
PF_DUP         = $0  ; repeat playfield
TIMER_VBLANK   = $2a ;  ~2688 cycles
TIMER_SCREEN   = $13 ; ~77824 cycles
TIMER_OVERSCAN = $14 ;  ~1280 cycles

  seg.u vars
  org $80

scanline .word 0
xoff .word 20
yoff .word 20
sprite .word      ; pointer to player sprite

  seg code
  org $f000

fencer_high:
  .byte %10110001  ;X XX   X;
  .byte %10110010  ;X XX  X ;
  .byte %10100100  ;X X  X  ;
  .byte %01111000  ; XXXX   ;
  .byte %00110000  ;  XX    ;
  .byte %00110000  ;  XX    ;
  .byte %00110000  ;  XX    ;
  .byte %00110000  ;  XX    ;
  .byte %00111000  ;  XXX   ;
  .byte %00111100  ;  XXXX  ;
  .byte %00110110  ;  XX XX ;
  .byte %00110010  ;  XX  X ;
  .byte %00100110  ;  X  XX ;
  .byte %00100100  ;  X  X  ;
fencer_mid:
  .byte %10110000  ;X XX    ;
  .byte %10110000  ;X XX    ;
  .byte %10100000  ;X X     ;
  .byte %01110000  ; XXX    ;
  .byte %00111001  ;  XXX  X;
  .byte %00110110  ;  XX XX ;
  .byte %00110000  ;  XX    ;
  .byte %00110000  ;  XX    ;
  .byte %00111000  ;  XXX   ;
  .byte %00111100  ;  XXXX  ;
  .byte %00110110  ;  XX XX ;
  .byte %00110010  ;  XX  X ;
  .byte %00100110  ;  X  XX ;
  .byte %00100100  ;  X  X  ;
fencer_low:
  .byte %10110000  ;X XX    ;
  .byte %10110000  ;X XX    ;
  .byte %10100000  ;X X     ;
  .byte %01110000  ; XXX    ;
  .byte %00111000  ;  XXX   ;
  .byte %00110100  ;  XX X  ;
  .byte %00110010  ;  XX  X ;
  .byte %00110001  ;  XX   X;
  .byte %00111000  ;  XXX   ;
  .byte %00111100  ;  XXXX  ;
  .byte %00110110  ;  XX XX ;
  .byte %00110010  ;  XX  X ;
  .byte %00100110  ;  X  XX ;
  .byte %00100100  ;  X  X  ;

reset:
  sei              ; interrupts
  cld              ; decimal mode
  ldx #$ff
  txs              ; stack
  lda #0
  ldx #0
memclear:
  sta 0,x
  inx
  bne memclear

  ; set controller DDR as input
  lda #$00
  sta SWACNT

  ; playfield setup
  lda #$84
  sta COLUBK
  lda #$2e
  sta COLUP0
  ; set up sprite pointer
  lda #<fencer_low
  sta sprite
  lda #>fencer_low
  sta sprite+1

  lda PF_REFL
  sta CTRLPF

  ;//////
.new_frame:
  ldy #0
  ldx #255

  ; read controller status
  lda #%10000000
  bit SWCHA
  bne .notright
  inc xoff
.notright:
  lda #%01000000
  bit SWCHA
  bne .notleft
  dec xoff
.notleft:
  lda #%00100000
  bit SWCHA
  bne .notdown
  dec yoff
.notdown:
  lda #%00010000
  bit SWCHA
  bne .notup
  inc yoff
.notup:

; hack to get around 16-bit operations
; extremely wasteful
  sta WSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC

.loop:
  stx scanline


  ldx xoff
.sleeploop:
;    SLEEP 20
;    dex
;    cpx #0
;  bmi .sleeploop

  ldx scanline

  sta RESP0

  ;draw player
  txa
  sbc yoff
  bpl .noplayer
  lda yoff
  sbc #14
  bpl .noplayer
  ; we have to draw a sprite
  lda (<sprite),y
  iny
  sta GRP0
  jmp .player_done
.noplayer:
  lda #0
  sta GRP0
.player_done:
  dex
  sta WSYNC

  cpx #0
  bne .loop
  lda #2
;  sta VBLANK
  sta VSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  lda #0
  sta VSYNC
;  sta VBLANK
  jmp .new_frame
  ;//////


  org $fffa

irqvec:
  .word reset  ; NMI
  .word reset  ; RESET
  .word reset  ; IRQ

