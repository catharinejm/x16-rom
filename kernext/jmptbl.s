.setcpu "65C02"

.import serialkbd_init, serialkbd_fetch, serialkbd_fill_buffer, serialkbd_read_byte
.import uart_prep_for_read, uart_is_file_ready

.segment "JMPTBL"

    jmp serialkbd_init        ;C000
    jmp serialkbd_fetch       ;C003
    jmp serialkbd_fill_buffer ;C006
    jmp serialkbd_read_byte   ;C009
    jmp uart_prep_for_read    ;C00C
    jmp uart_is_file_ready    ;C00F
