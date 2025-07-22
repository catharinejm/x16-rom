.setcpu "65C02"

.import serialkbd_init, serialkbd_fetch_in_kvars, serialkbd_fetch, serialkbd_fill_buffer, serialkbd_read_byte
.import uart_prep_for_read, uart_is_file_ready

.segment "JMPTBL"

    jmp serialkbd_init        ;C000
    jmp serialkbd_fetch_in_kvars ;C003
    jmp serialkbd_fetch       ;C006
    jmp serialkbd_fill_buffer ;C009
    jmp serialkbd_read_byte   ;C00C
    jmp uart_prep_for_read    ;C00F
    jmp uart_is_file_ready    ;C012
