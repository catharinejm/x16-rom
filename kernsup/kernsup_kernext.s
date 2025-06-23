.include "banks.inc"

.macro bridge symbol
	.local address
	.segment "KSUP_VEC16"
address = *
	.segment "KSUP_CODE16"
symbol:
	jsr kxjsrfar
	.word address
	.byte BANK_KERNAL
	rts
	.segment "KSUP_VEC16"
	jmp symbol
.endmacro

.setcpu "65c02"

.segment "KSUP_CODE16"

; KERNEXT bank's entry into jsrfar
.setcpu "65c02"
	ram_bank = 0
	rom_bank = 1
.export kxjsrfar
kxjsrfar:
.include "jsrfar.inc"


.segment "KSUP_VEC16"

	xjsrfar = kxjsrfar
.include "kernsup.inc"

	.byte 0, 0, 0, 0 ; signature
