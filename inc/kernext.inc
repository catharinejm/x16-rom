;; -*- no-whitespace-cleanup: () -*-
.import __KVEXTB0_START__

serialkbd_init        := $C000
serialkbd_fetch_in_kvars := $C003
serialkbd_fetch       := $C006
serialkbd_fill_buffer := $C009
serialkbd_read_byte   := $C00C
uart_prep_for_read    := $C00F
uart_is_file_ready    := $C012
