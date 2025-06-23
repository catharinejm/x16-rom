.setcpu "65C02"

.import serialkbd_fetch, serialkbd_fill_buffer, serialkbd_read_byte

.segment "JMPTBL"

    jmp serialkbd_fetch       ;C000
    jmp serialkbd_fill_buffer ;C003
    jmp serialkbd_read_byte   ;C006
