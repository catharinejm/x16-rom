;; -*- no-whitespace-cleanup: (); indent-tabs-mode: () -*-
.setcpu "65C02"

.include "io.inc"
.include "regs.inc"
.include "via2.inc"

.export uart_read_file, uart_prep_for_read, uart_is_file_ready

.segment "ZPKEXT": zeropage
vars_start = *
PTR:  .res 2
LASTBANK: .res 1
SIZE: .res 3
;; TODO: Could BYTE and SPACELEFT go on the stack? Unsure of tradeoff
;; between stack overflow vs using up zp
BYTE: .res 1
SPACELEFT: .res 1
vars_size = *-vars_start

.segment "KVEXTB0"
FILE_ID: .res 1

;; Waits
.macro spin_wait_clobber_x cycles
.assert (cycles - 6) >= 0 && (cycles - 6) .mod 5 = 0, error, "spin_wait_clobber_x cycles must be 5n + 6"
.local @iters,@loop
@iters = (cycles - 6) / 5
    ldx #@iters ;; 2c
@loop:
    dex       ;; 2c
    bpl @loop ;; 3c (taken) 2c (not taken)
.assert >* = >@loop, error, "spin_wait_clobber_x across page boundary"
.endmacro

.segment "KERNEXT"

;; .A = number of $7f bytes already read
uart_read_file:
    php
    sei ;; disable interrupts
    ldx ram_bank
    phx
    ldx #1
    stx ram_bank

    ;; Read and discard (16 - .A) remaining $7f bytes
    clc
    sbc #$10
    eor #$ff
    sta SIZE ;; just for debugging
    ldx #0
    ldy #$a0
    jsr uart_read_bytes

    ;; If in emulator and debug is on, verify we actually got #$7fs
    ldy $9fbe
    cpy #$31
    bne @nodebug
    ldy $9fbf
    cpy #$36
    bne @nodebug
    ldy $9fb0
    beq @nodebug
    ldx SIZE
    dex
@chkloop:
    lda $a000,x
    cmp #$7f
    beq :+
    stp ;; should launch debugger in emulator
:   dex
    bpl @chkloop
@nodebug:

    ;; Read 3 bytes into SIZE
    lda #3
    ldx #<SIZE
    ldy #>SIZE
    jsr uart_read_bytes

@read_bank:
    ldy #$a0
    sty PTR+1
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
    beq @return
    ldx #0
    ldy PTR+1
    jsr uart_read_bytes
    sty PTR

@return:
    lda PTR+1
    cmp #$a0
    bne @1
    lda PTR
    bne @1
    lda #$c0
    sta PTR+1
    dec ram_bank
@1:
    lda ram_bank
    sta LASTBANK
    stz ram_bank
    inc FILE_ID
    plx
    stx ram_bank
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
@wait_for_start:
    bit VIA2::regb      ;; 4cyc
    bne @wait_for_start ;; 3cyc (taken)
.assert >* = >@wait_for_start, error, "jumping across page"

    ;; ~10 cycles may have passed between RX going low and arriving here

    nop
    nop

    ;; Now we're 4-14 cycles into the start bit

    bit VIA2::regb      ;; 4c
    bne @wait_for_start ;; 2c (not taken)

    ;; 10-20 cycles in

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
    lda #SERIALKBD::RXPIN  ;; 2c
    read_bit               ;; 14c

    ;; We're maybe 4 cycles behind here

;; Bits 1-7 are 14cyc into the next wait
.repeat 7
    spin_wait_clobber_x 21 ;; 21cyc
    ;; 35 cyc
    read_bit     ;; 14cyc
;; 14 cyc into next spin
.endrepeat

    lda BYTE       ;; 3c
    sta (PTR),Y    ;; 6c
    iny            ;; 2c
    lda SPACELEFT  ;; 3c (already decremented)
    beq @return    ;; 2c (not taken)

    lda #SERIALKBD::RXPIN
:   bit VIA2::regb
    beq :-   ;; wait till high stop bit comes in just in case
    jmp @wait_for_start

@return:
    rts

;; Returns .A = next FILE_ID
uart_prep_for_read:
    php
    phx
    sei
    ldx ram_bank
    stz ram_bank
    lda FILE_ID
    ina
    stx ram_bank
    plx
    plp
    rts

;; .A = expected file id
;; Returns: if file is ready: .A=LASTBANK, .X=PTR, .Y=PTR+1, .C=0
;;          otherwise: .C=1
uart_is_file_ready:
    phx
    ldx ram_bank
    phx
    stz ram_bank
    cmp FILE_ID
    beq :+
    plx
    stx ram_bank
    plx
    sec
    rts

:   plx
    stx ram_bank
    plx
    lda LASTBANK
    ldx PTR
    ldy PTR+1
    clc
    rts
