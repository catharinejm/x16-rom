;; -*- no-whitespace-cleanup: () -*-
.import __KVEXTB0_START__

serialkbd_fetch       := $C000
serialkbd_fill_buffer := $C003
serialkbd_read_byte   := $C006

serialkbd_BUFFER      := __KVEXTB0_START__
serialkbd_RDPTR       := __KVEXTB0_START__ + $10
serialkbd_WTPTR       := __KVEXTB0_START__ + $11
