    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with register mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start an uninitialized segment at $80 for var declaration.
;; We have memory from $80 to $FF to work with, minus a few at
;; the end if we use the stack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80
P0XPos byte        ; sprite X coordinate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code segment starting at $F000.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

Reset:
    CLEAN_START    ; macro to clean memory and TIA

    ldx #$80       ; blue background color
    stx COLUBK

    ldx #$D0       ; green playfield floor color
    stx COLUPF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #10
    sta P0XPos     ; initialize player X coordinate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by configuring VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
    lda #2
    sta VBLANK     ; turn VBLANK on
    sta VSYNC      ; turn VSYNC on

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 3 vertical lines of VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 3
        sta WSYNC  ; first three VSYNC scanlines
    REPEND
    lda #0
    sta VSYNC      ; turn VSYNC off

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set player horizontal position while in VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda P0XPos     ; load register A with desired X position
    and #$7F       ; AND position with $7F to fix range
    sta WSYNC      ; wait for next scanline
    sta HMCLR      ; clear old horizontal position values

    sec            ; set carry flag before subtraction
DivideLoop:
    sbc #15        ; subtract 15 from the accumulator
    bcs DivideLoop ; loop while carry flag is still set

    eor #7         ; adjust the remainder in A between -8 and 7
    asl            ; shift left by 4, as HMP0 uses only 4 bits
    asl
    asl
    asl
    sta HMP0       ; set fine position
    sta RESP0      ; reset 15-step brute position
    sta WSYNC      ; wait for next scanline
    sta HMOVE      ; apply the fine position offset

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the remaining 35 lines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 35
        sta WSYNC
    REPEND

    lda #0
    sta VBLANK     ; turn VBLANK off

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 160
        sta WSYNC  ; wait for 160 empty scanlines
    REPEND

    ldy #17         ; counter to draw 17 rows of player0 bitmap
DrawBitmap:
    lda P0Bitmap,Y ; load player bitmap slice of data
    sta GRP0       ; set graphics for player 0 slice

    lda P0Color,Y  ; load player color from lookup table
    sta COLUP0     ; set color for player 0 slice

    sta WSYNC      ; wait for next scanline

    dey
    bne DrawBitmap ; repeat next scanline until finished

    lda #0
    sta GRP0       ; disable P0 bitmap graphics

    lda #$FF       ; enable grass playfield
    sta PF0
    sta PF1
    sta PF2

    REPEAT 15
        sta WSYNC  ; wait for remaining 15 empty scanlines
    REPEND

    lda #0         ; disable grass playfield
    sta PF0
    sta PF1
    sta PF2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output 30 more VBLANK overscan lines to complete our frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Overscan:
    lda #2
    sta VBLANK     ; turn VBLANK on again for overscan
    REPEAT 30
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joystick input test for P0 up/down/left/right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000
    bit SWCHA
    bne CheckP0Down
    inc P0XPos

CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
    dec P0XPos

CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
    dec P0XPos

CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne NoInput
    inc P0XPos

NoInput:
    ; fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop to next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup table for the player graphics bitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Bitmap:
    byte #%00000000
    byte #%00010100
    byte #%00010100
    byte #%00010100
    byte #%00010100
    byte #%00010100
    byte #%00011100
    byte #%01011101
    byte #%01011101
    byte #%01011101
    byte #%01011101
    byte #%01111111
    byte #%00111110
    byte #%00010000
    byte #%00011100
    byte #%00011100
    byte #%00011100

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup table for the player colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Color:
    byte #$00
    byte #$F6
    byte #$F2
    byte #$F2
    byte #$F2
    byte #$F2
    byte #$F2
    byte #$C2
    byte #$C2
    byte #$C2
    byte #$C2
    byte #$C2
    byte #$C2
    byte #$3E
    byte #$3E
    byte #$3E
    byte #$24

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    word Reset
    word Reset
