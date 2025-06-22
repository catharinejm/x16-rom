.setcpu "65C02"

.include "io.inc"
.include "banks.inc"

.export serialkbd_init, serialkbd_fetch

.import ps2data_kbd, ps2data_kbd_count

;; Named from the X16's perspective. E.g. RTSPIN is connected to CTS on the FTDI
RXPIN  = $01 ;; PB0
RTSPIN = $10 ;; PB4
;CTSPIN = $40 ;; PB6 - used to signal data is available from PC, not real CTS
CTSIRQ = $10 ;; CB1 IRQ flag bit

.scope VIA2
    base = $9f10
    regb = base + $00
    rega = base + $01
    ddrb = base + $02
    ddra = base + $03
    t1cl = base + $04
    t1ch = base + $05
    t1ll = base + $06
    t1lh = base + $07
    t2l  = base + $08
    t2h  = base + $09
    sr   = base + $0a
    acr  = base + $0b
    pcr  = base + $0c
    ifr  = base + $0d
    ier  = base + $0e
.endscope


.segment "KVARSB0"
WTPTR: .byte 0
RDPTR: .byte 0
BUFFER: .res 16

.segment "SERIALKBD"

serialkbd_init:
    KVARS_START

    stz RDPTR
    stz WTPTR

    lda #$7f
    sta VIA2::ier
    sta VIA2::ifr

    lda #RTSPIN
    sta VIA2::ddrb ;; Set RTS as output
    sta VIA2::regb ;; set RTS high

    stz VIA2::acr
    stz VIA2::pcr

@done:
    KVARS_END
    rts

;; TODO think about how to handle zero-page addrs, scratch vs saved, maybe some 16-bit regs
SPACELEFT = $22
fill_buffer:
    ;; If the CTS IRQ flag hasn't set, there's no data
    lda #CTSIRQ
    bit VIA2::ifr
    bne :+
    rts
:   sta VIA2::ifr ;; Clear CTS IRQ flag

    ; php
    ; phy
    ; phx
    ; pha
    lda SPACELEFT
    pha
    ; sei ;; disable interrupts


;; TODO not necessary to php or sei when called from irq handler, but
;; can't be avoided if this procedure could be called anywhere. Unless
;; maybe interrupts disabled is a prerequiste for calling?

    ldx WTPTR ;; X contains WTPTR throughout, and must be set before jumping to @return

    lda RDPTR
    clc ;; CLEAR carry b/c we want RDPTR-WTPTR-1
    sbc WTPTR
    and #$0f
    bne :+
    jmp @return ;; no space, so return
:   sta SPACELEFT

    lda #RTSPIN
    trb VIA2::regb ;; pulse RTS low...
    tsb VIA2::regb ;; and back high

    lda #RXPIN
    ldy #25 ;; 25 iterations of 11cyc = 275 = 34.375us ~= 4 baud widths at 115200 baud (34.722us)
@wait_for_start:
    bit VIA2::regb      ;; 4cyc
    beq @start_read     ;; 2cyc (not taken)
    dey                 ;; 2cyc
    bne @wait_for_start ;; 3cyc (taken)
.assert >* = >@wait_for_start, error, "jumping across page"
    jmp @return

@start_read:

    ;; 11 cycles may have passed between RX going low and arriving here

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
    lda #RTSPIN      ;; 2c
    dec SPACELEFT    ;; 5c
    bne :+           ;; 3c taken, 2c not
    and #0           ;; 2c
:   trb VIA2::regb   ;; 6c
    tsb VIA2::regb   ;; 6c


;; Waits iters*5 cycles + 20
.macro spin_wait iters
    ldy #iters + 1  ;; 2c
    jsr _spin_wait  ;; iters*5+16
    nop             ;; 2c
.endmacro

;; A must equal #RXPIN
.macro read_bit
    and VIA2::regb  ;; 4c
    lsr             ;; 2c
    ror BUFFER,X    ;; 7c
    lda #RXPIN      ;; 2c
.endmacro

;; Bit 0 is 22/23 cycles into the next wait
;; but only spin 43cyc more (65 total, not 70) b/c there's a ~10 cycle drift as we read
    spin_wait 4 ;; 40c
    lda #RXPIN  ;; 2c
    read_bit    ;; 15c

;; Bits 1-7 are 15cyc into the next wait
.repeat 7
    spin_wait 7 ;; 55cyc
    ;; 70 cyc
    read_bit    ;; 15cyc
    ;; 15 cyc into next spin
.endrepeat
    ;; 8 cycle drift

    inx            ;; 2c
    txa            ;; 2c
    and #$0f       ;; 2c
    tax            ;; 2c
    lda SPACELEFT  ;; 3c (already decremented)
    beq @return    ;; 2c (not taken)

    ;; 28 cyc into stop bit spin
    ;; Spin 36 more (64 total) b/c we've drifted from -5 to +5 by now
    spin_wait 4 ;; 40 cycles

    lda #RXPIN
:   bit VIA2::regb
    beq :-   ;; wait till high stop bit comes in just in case
    ldy #8   ;; only wait upto 88 cycles for start bit, it will come immediately if there's more data
    jmp @wait_for_start

@return:
    stx WTPTR

    pla
    sta SPACELEFT
    ; pla
    ; plx
    ; ply
    ; plp
    rts

;; Waits (Y-1)*5 cycles + 10
_spin_wait:
    dey           ;; 2c
    bne _spin_wait ;; 3c (taken) 2c (not taken)
.assert >* = >_spin_wait, error, "spin_wait across page"
    rts           ;; 6c

read_byte:
    php
    phx
    sei ;; Disable interrupts so RDPTR and WTPTR don't change during execution
    ldx RDPTR
    cpx WTPTR
    bne :+
    lda #0
    bra @done
:   lda BUFFER,X
    tax
    lda RDPTR
    ina
    and #$0f
    sta RDPTR
    txa
@done:
    plx
    plp
    cmp #0
    clc
    rts

serialkbd_fetch:
    KVARS_START
    jsr fill_buffer

    ;; let real PS/2 keyboard take precedence
    lda ps2data_kbd_count
    beq @use_serial

    lda ps2data_kbd
    bne @done

@use_serial:
    jsr read_byte
    beq @done ;; No uart key

    sta ps2data_kbd
    inc ps2data_kbd_count

@done:
    KVARS_END
    rts
