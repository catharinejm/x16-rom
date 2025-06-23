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

.scope SERIALKBD
    ;; Named from the X16's perspective. E.g. RTSPIN is connected to CTS on the FTDI
    RXPIN  = $01 ;; PB0
    RTSPIN = $10 ;; PB4
    ; CTSPIN = $40 ;; PB6 - used to signal data is available from PC, not real CTS
    CTSIRQ = $10 ;; CB1 IRQ flag bit
.endscope
