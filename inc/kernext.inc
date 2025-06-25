;; -*- no-whitespace-cleanup: () -*-
.import __KVEXTB0_START__

serialkbd_init        := $C000
serialkbd_fetch       := $C003
serialkbd_fill_buffer := $C006
serialkbd_read_byte   := $C009
uart_prep_for_read    := $C00C
uart_is_file_ready    := $C00F
