;; -*- no-whitespace-cleanup: () -*-
.setcpu "65C02"

.include "io.inc"
.include "banks.inc"
.include "kernext.inc"
.include "via2.inc"

.export serialkbd_init

.segment "SERIALKBD"

serialkbd_init:
    php
    sei
    KVARS_START
    stz serialkbd_RDPTR
    stz serialkbd_WTPTR

    lda #$7f
    sta VIA2::ier
    sta VIA2::ifr

    lda #SERIALKBD::RTSPIN
    sta VIA2::ddrb ;; Set RTS as output
    sta VIA2::regb ;; set RTS high

    stz VIA2::acr
    stz VIA2::pcr

@done:
    KVARS_END
    plp
    rts
