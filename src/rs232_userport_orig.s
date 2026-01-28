            .setcpu "6502"

            .include "cbm_kernal.inc"

            .export rs232_userport_funcs_setup   := setup
            .export rs232_userport_funcs_enable  := open_modem_device
            .export rs232_userport_funcs_disable := close_modem_device
            .export rs232_userport_funcs_getchar := getchar_from_modem_device
            .export rs232_userport_funcs_putchar := putchar_to_modem_device

modem_file_no    = 128
modem_device_no  = 2

            .code

setup:      ; x: baud_rate: 0=300, 1=1200, 2=2400; y: 0=ntsc, 0~=pal
            txa
            asl a
            cpy #0
            beq :+
            clc
            adc #baud_offset
:           tax
            lda baud,x
            sta serial_config+2
            lda baud+1,x
            sta serial_config+3
            rts

open_modem_device:
            jsr close_modem_device
            lda #4                 ; filename length
            ldx #<serial_config
            ldy #>serial_config
            jsr SETNAM
            lda #modem_file_no
            ldx #modem_device_no
            ldy #3
            jsr SETLFS
            jsr OPEN
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


            .rodata

ntsc_clock_freq = 1022727
pal_clock_freq  =  985248

baud:
ntsc_baud:
            .word ntsc_clock_freq/ 300/2 - 100  ; 300
            .word ntsc_clock_freq/1200/2 - 100  ; 1200
            .word ntsc_clock_freq/2400/2 - 100  ; 2400

pal_baud:
            .word pal_clock_freq / 300/2 - 100  ; 300
            .word pal_clock_freq /1200/2 - 100  ; 1200
            .word pal_clock_freq /2400/2 - 100  ; 2400

baud_offset = pal_baud - ntsc_baud


            .data

serial_config:
            .byte 0     ; 1 stopbit, 8 bit wordsize
            .byte 0     ; full duplex, no parity
            .byte 0, 0  ; baud rate lo, hi
