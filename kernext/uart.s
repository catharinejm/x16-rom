;; -*- no-whitespace-cleanup: () -*-
.include "io.inc"
.include "regs.inc"
.include "via2.inc"

.segment "ZPKEXT"
vars_start = *
SIZE: .res 3
BYTE: .res 1
PTR:  .res 2
BANK: .res 1
SPACELEFT: .res 1
vars_size = *-vars_start

;; Waits
.macro spin_wait_clobber_x cycles
.assert (cycles - 6) >= 0 && (cycles - 6) % 5 == 0, error, "spin_wait_clobber_x cycles must be 5n + 6"
.local iters
iters = cycles / 5
    ldx #iters ;; 2c
.local @loop
@loop:
    dex       ;; 2c
    bpl @loop ;; 3c (taken) 2c (not taken)
.endmacro

.segment "KERNEXT"
uart_read_file:
    php
    sei ;; disable interrupts
    lda ram_bank
    pha
    lda #1
    sta ram_bank

    ;; Read 3 bytes into SIZE
    lda #3
    ldx #<SIZE
    ldy #>SIZE
    jsr uart_read_bytes

@read_bank:
    ldy #$a0
    sta PTR+1
@read_loop:
    lda SIZE+1
    bne @read_next_page
    lda SIZE+2
    beq @read_tail
    dec SIZE+2
@read_next_page:
    dec SIZE+1
    lda #0
    ldx #0
    ldy PTR+1
    jsr uart_read_bytes
    inc PTR+1
    lda #$c0
    cmp PTR+1
    bne @read_loop
    inc ram_bank
    beq @return ;; wrapped -- TODO handle < 256 banks?
    bra @read_bank

@read_tail:
    lda SIZE
    ldx #0
    ldy PTR+1
    jsr uart_read_bytes

@return:
    lda ram_bank
    dec ;; TODO handle < 256 banks?
    plx
    stx ram_bank
    ldx PTR
    ldy PTR+1
    plp
    rts


;; .A = nbytes (0 = 256), .X = buflo, .Y = bufhi
uart_read_bytes:
    sta SPACELEFT
    stx PTR
    sty PTR+1

    ;; Y contains offset into PTR
    ldy #0

    lda #SERIALKBD::RTSPIN
    trb VIA2::regb ;; pulse RTS low...
    tsb VIA2::regb ;; and back high

    lda #SERIALKBD::RXPIN
    ; ldy #25 ;; 25 iterations of 11cyc = 275 = 34.375us ~= 4 baud widths at 115200 baud (34.722us)
@wait_for_start:
    bit VIA2::regb      ;; 4cyc
    bne @wait_for_start ;; 3cyc (taken)
.assert >* = >@wait_for_start, error, "jumping across page"

    ;; ~10 cycles may have passed between RX going low and arriving here

    nop
    nop
    nop
    nop
    nop
    nop

    ;; Now we're 12-23 cycles into the start bit

    bit VIA2::regb      ;; 4c
    bne @wait_for_start ;; 2c (not taken)

    ;; 18-29 cycles in

    ;; Pulse RTS again if we can take another bit after this
    ;; Doing it early so there's no pause between bytes from the FTDI

    ;; 22c if there's space for another byte, 23c if not
    lda #SERIALKBD::RTSPIN ;; 2c
    dec SPACELEFT          ;; 5c
    bne :+                 ;; 3c taken, 2c not
    and #0                 ;; 2c
:   trb VIA2::regb         ;; 6c
    tsb VIA2::regb         ;; 6c


.macro read_bit
    lda #SERIALKBD::RXPIN ;; 2c
    and VIA2::regb        ;; 4c
    lsr                   ;; 2c
    ror BYTE              ;; 6c
.endmacro

    ;; Bit 0 is 22/23 cycles into the next wait
    ;; but only spin 43cyc more (65 total, not 70) b/c there's a ~10 cycle drift as we read
    spin_wait_clobber_x 41 ;; 41c
    lda #SERIALKBD::RXPIN  ;; 2c
    read_bit               ;; 14c

;; Bits 1-7 are 14cyc into the next wait
.repeat 7
    spin_wait_clobber_x 56 ;; 56cyc
    ;; 69 cyc
    read_bit     ;; 14cyc
;; 14 cyc into next spin
.endrepeat
;; 8 cycle drift?

    lda BYTE       ;; 3c
    sta (PTR),Y    ;; 6c
    iny            ;; 2c
    lda SPACELEFT  ;; 3c (already decremented)
    beq @return    ;; 2c (not taken)

    ;; 30 cyc into stop bit spin
    ;; Spin 36 more
    spin_wait 36 ;; 36 cycles

    lda #SERIALKBD::RXPIN
:   bit VIA2::regb
    beq :-   ;; wait till high stop bit comes in just in case
    ; ldy #8   ;; only wait upto 88 cycles for start bit, it will come immediately if there's more data
    jmp @wait_for_start

@return:
    rts
