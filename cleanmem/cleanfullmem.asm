    processor 6502

    seg code
    org $F000       ; define the ROM code origin at $F000

Start:
    sei             ; disable interrupts
    cld             ; disable the BCD decimal math mode
    ldx #$FF        ; loads the X register with value #$FF
    txs             ; transfer X register to S(tack) Pointer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Clear the Zero Page region (from $00 to $FF)
; That means the entire TIA register space and also RAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0          ; A = 0
    ldx #$FF        ; X = #$FF
    sta $FF         ; store zero at memory position $FF before the loop starts

MemLoop:
    dex             ; decrements X
    sta $0,X        ; store zero at address $0 + X (does not modify flags)
    bne MemLoop     ; loop until X == 0 (until z-flag is set by previous DEX)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fill  ROM size to exactly 4KB
; Also tells 6502 where our program should start (at $FFFC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC       ; moves/jumps origin to ROM address $FFFC
    .word Start     ; puts 2 bytes at position $FFFC (where program starts)
    .word Start     ; puts interrupt vector at position $FFFE (unused in VCS)
