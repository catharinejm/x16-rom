.include "io.inc"
.include "banks.inc"

.export kernext_call

.segment "KERNEXT"
kernext_call:
    ldx rom_bank
    phx
    sta kernext_target
    lda #BANK_KERNEXT
    jmp kernext_jsr
    

.segment "KEXTCALL"
kernext_jsr:
    sta rom_bank
    jsr $c0ff
kernext_target := *-2
    plx
    stx rom_bank
    rts
