.setcpu "65C02"

.import serialkbd_init, serialkbd_fetch, serialkbd_fill_buffer, serialkbd_read_byte

.segment "JMPTBL"

    jmp serialkbd_init        ;C000
    jmp serialkbd_fetch       ;C003
    jmp serialkbd_fill_buffer ;C006
    jmp serialkbd_read_byte   ;C009
