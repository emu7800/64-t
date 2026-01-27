            .setcpu "6502"

            .include "cbm_kernal.inc"
            ;.include "c64.inc"

            .export rs232_userport_funcs

serial_config    = $5f   ; Four (4) byte string for serial port configuration
modem_file_no    = 128
modem_device_no  = 2

            .code

rs232_userport_funcs:
            jmp setup
            jmp open_modem_device
            jmp close_modem_device
            jmp getchar_from_modem_device
            jmp putchar_to_modem_device


setup:
            rts

open_modem_device:
            jsr close_modem_device
            lda #4                 ; filename length
            ldx #<serial_config
            ldy #>serial_config
            jsr SETNAM
            lda #modem_file_no
            ldx #modem_device_no
            ldy #3                 ; secondary address
            jsr SETLFS
            jsr OPEN
            jsr CLRCHN
            ldx #modem_file_no
            jsr CHKOUT
            lda #0
            jsr CHROUT
            jmp CLRCHN


close_modem_device:
            lda #modem_file_no
            jmp CLOSE


getchar_from_modem_device:
            jsr CLRCHN
            ldx #modem_file_no
            jsr CHKIN
            jmp GETIN


putchar_to_modem_device:
            pha
            jsr CLRCHN
            ldx #modem_file_no
            jsr CHKOUT
            pla
            jmp CHROUT
