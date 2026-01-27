;
; '64 Terminal - Next Generation (NG)
;
; Telecommunications program for the Commodore 64
;
; - Smooth scrolling display!
; - Set-up menu/function key modes
; - 24K Receive Buffer with Review
;   and VIC printer dump
; - Improved baud rate settings
; - Requires modem, VIC printer optional
;
; Original by Dr. Jim Rothwell, Midwest Micro Inc., circa 1983
;
; Reverse engineering and Next Generation improvements
; by Mike Murphy (mike@emu7800.net), circa 2025.
; Intended for preservation and educational purposes only.
;
; To build, invoke 'make' in this directory without arguments.
;

            .setcpu "6502"

            .include "cbm_kernal.inc"
            .include "c64.inc"

            .import __LOADADDR__
            .import __BSS_LOAD__
            .import __BSS_SIZE__
            .import finescroll_funcs
            .import rs232_userport_funcs


stop_char               = 3     ; PETSCII stop
BS                      = 8     ; ASCII backspace
enable_logoshift        = 9     ; PETSCII enable keyboard LOGO+SHIFT combination
CR                      = 13    ; PETSCII/ASCII carriage return
toggle_charset          = 14    ; PETSCII toggle character set
cursor_down             = 17    ; PETSCII cursor down
cursor_home             = 19    ; PETSCII cursor home
delete                  = 20    ; PETSCII delete $14
space                   = 32    ; PETSCII/ASCII space
quote                   = 34    ; PETSCII/ASCII quote
achar                   = 65    ; PETSCII a
nchar                   = 78    ; PETSCII n
ychar                   = 89    ; PETSCII y
zchar                   = 90    ; PETSCII z
Achar                   = 97    ; PETSCII A
DEL                     = 127   ; ASCII delete
F1                      = 133   ; PETSCII F1
F3                      = 134   ; PETSCII F3
F5                      = 135   ; PETSCII F5
F7                      = 136   ; PETSCII F7
F2                      = 137   ; PETSCII F2
F4                      = 138   ; PETSCII F4
F6                      = 139   ; PETSCII F6
F8                      = 140   ; PETSCII F8
shift_return            = 141   ; PETSCII SHIFT+RETURN
cls                     = 147   ; PETSCII clear screen
insert                  = 148   ; PETSCII insert
cursor_left             = 157   ; PETSCII cursor left
underscore              = 164   ; PETSCII underscore


show_cursor_flag        = $5e   ; bit7: 0=dont show cursor, 1=show cursor
serial_config           = $5f   ; Four (4) byte string for serial port configuration
rcv_flag                = $69   ; bit7: 0=off, 1=on
rcv_buffer_ptr          = $6a
petscii_flag            = $70   ; bit7: 0=ascii, 1=petscii
rcv_buffer_ptr_lo       = rcv_buffer_ptr
rcv_buffer_ptr_hi       = rcv_buffer_ptr+1
ptr                     = $6c
ptr_lo                  = ptr
ptr_hi                  = ptr+1
rcv_buffer_full_flag    = $6e   ; bit7: 0=not full, 1=full
rcv_notadded_flag       = $6f   ; bit7: added to buffer since rcv on: 0=byte added, 1=byte not added
NDX                     = $c6   ; Number of Characters in Keyboard Buffer (Queue)
PNTR                    = $d3   ; Cursor column on current logical line (0-79)
TBLX                    = $d6   ; Current cursor physical line number (0-24)
COLOR                   = $0286 ; Current Foreground Color for Text (values 0-f)


sprite_data_base_addr   = $2000
offset_rcv              = 0  ; RCF sprite
offset_dpx              = 1  ; DPX sprite
offset_64t              = 3  ; 64T sprite
offset_ermi             = 4  ; ERMI sprite
offset_nal              = 5  ; NAL sprite


sprite_data_ptr_offset  = $3f8  ; Start of Sprite Shape Data Pointers
lo_screen_nybble        = $1
hi_screen_nybble        = $a
lo_screen_addr          = $0400*lo_screen_nybble ; = $0400 video matrix addr
hi_screen_addr          = $0400*hi_screen_nybble ; = $2800 video matrix addr


black                   = 0
white                   = 1
blue                    = 6
gray                    = 15


printer_file_no         = 4
printer_device_no       = 4


; C64 kernal addresses not in cbm_kernal.inc
USKINDIC                = $f6bc ; Update Stop key indicator, at memory address $0091, A and X registers used

            .segment "LOADADDR"
            .word   __LOADADDR__

            .segment "EXEHDR"
startofexeheader:
            ; 0 SYSnnnn:REM  * * 64 TERMINAL NG * *
            .word endofbasicprogram
            .byte $00, $00 ; 0
            .byte $9e      ; SYS
            .byte $30 + .lobyte ((start .mod 10000) / 1000)
            .byte $30 + .lobyte ((start .mod 1000) / 100)
            .byte $30 + .lobyte ((start .mod 100) / 10)
            .byte $30 + .lobyte ((start .mod 10) / 1)
            .byte $3a      ; :
            .byte $8f      ; REM
            .asciiz "  * * 64 terminal ng * *"

endofbasicprogram:
            .byte 0, 0

            .code
start:
            jsr clear_screen
            lda #toggle_charset
            jsr CHROUT
            lda #enable_logoshift
            jsr CHROUT
            lda #%00010111          ; scroll 7, 24 rows
            sta VIC_CTRL1
            lda #$80
            sta color_flag
            jsr config_colors
            jsr output_cursor_home_and_cursor_down
            jsr output_space_and_cursor_left_to_screen
            lda #0
            sta show_cursor_flag
            jsr print_title_message_to_screen
reset:
            jsr config_colors
            jsr init_sprites
            lda #$80
            sta show_cursor_flag
            sta rcv_notadded_flag
            jsr accept_presets_menu
            jsr init_rcv_buffer_ptr
            jsr CLALL
            jsr setup_modem_device
            jsr open_modem_device
main:
            jsr CLRCHN

get_byte_from_keyboard:
            jsr GETIN
            bne :+
            jmp get_char_from_modem
:           cmp #128
            bcc @toggle_rcv
            cmp #129
            beq :+
            cmp #142
            bcc @toggle_rcv
            cmp #cls
            beq @toggle_rcv
            cmp #insert
            beq @toggle_rcv
            cmp #160
            bcs @toggle_rcv
:           jmp main
@toggle_rcv:
            cmp #F2
            bne @toggle_dpx
            bit rcv_buffer_full_flag
            bpl :+
            jmp main               ; dont bother since buffer full
:           lda #$80
            eor rcv_flag
            sta rcv_flag
            sta rcv_notadded_flag
@set_rcv_flag:
            lda #CR
            jsr output_char_to_screen
            jsr indicate_rcv_flag
            jmp main
@toggle_dpx:
            cmp #F1
            bne :+
            lda #0
            sta show_cursor_flag
            ldy #<help_text
            lda #>help_text
            jsr output_string_to_screen
            lda #$80
            sta show_cursor_flag
            jmp main
:           cmp #F4
            bne :+
            lda #$80
            eor dpx_flag
            sta dpx_flag
            jsr indicate_dpx_config
            jmp main
:           cmp #F6
            bne :+
            jmp reset
:           cmp #F7
            bne :+
            jmp dump_rcv_buffer_to_screen
:           cmp #F8
            bne :+
            jmp dump_rcv_buffer_to_printer
:           cmp #insert
            bne :+
            lda #DEL
            jmp output_char_to_modem_with_dpx_echo
:           cmp #cls
            bne :+
            jsr output_clear_and_cursor_down_to_screen
            jmp main
:           cmp #shift_return
            bne :+
            and #$7f
:           cmp #CR
            beq output_char_to_modem_with_dpx_echo
            cmp #delete
            bne :+
            bit petscii_flag
            bmi @dont_convert_delete
            lda #BS
@dont_convert_delete:
            jmp output_char_to_modem_with_dpx_echo
:           cmp #space
            bcs output_char_to_modem_with_dpx_echo
            jmp main


output_char_to_modem_with_dpx_echo:
            pha
            bit dpx_flag
            bpl output_char_to_modem
            jsr output_char_to_screen
            ; Expects character to output to be on top of the stack
output_char_to_modem:
            pla
            cmp #BS
            beq @done
            cmp #DEL
            beq @done
            bit petscii_flag
            bmi @done
@convert_to_ascii:
            and #$7f
            cmp #achar
            bcc @done
            cmp #zchar+1
            bcs @done
            clc
            adc #32
@done:
            jsr putchar_to_modem_device


get_char_from_modem:
            jsr getchar_from_modem_device
            cmp #0
            beq @done
@handle_char:
            bit petscii_flag
            bmi @output_then_done
@handle_ascii_char:
            and #$7f
            cmp #BS
            beq @output_then_done
            cmp #DEL
            beq @output_then_done
            cmp #CR
            beq @output_then_done
            cmp #space
            bcc @done
@handle_printable_ascii_char:
            cmp #Achar
            bcc :+
            sec
            sbc #32
            bne @output_then_done
:           cmp #achar
            bcc @output_then_done
            cmp #zchar+1
            bcs @output_then_done
            clc
            adc #32
@output_then_done:
            jsr output_char_to_screen
@done:
            jmp main


dump_rcv_buffer_to_printer:
            lda #0
            sta rcv_flag
            sta rcv_notadded_flag
            jsr indicate_rcv_flag
            lda #CR
            jsr output_char_to_screen
            jsr CLRCHN
            jsr close_modem_device
            jsr open_printer_device
            ldx #printer_device_no
            jsr CHKOUT
            jsr READST
            bpl @status_ok
            ldy #<err_printer_offline_message
            lda #>err_printer_offline_message
            jsr output_string_to_screen
            jmp @done

@status_ok:
            lda #%00000111         ;  scroll 7, 25 rows
            sta VIC_CTRL1
            lda #<startof_rcv_buffer_addr
            sta ptr_lo
            lda #>startof_rcv_buffer_addr
            sta ptr_hi
            lda #CR
            jsr CHROUT

@get_nextbyte:
            ldy #0
            lda (ptr),y
            beq @clear_rcv_buffer_then_done
            jsr CHROUT
            jsr USKINDIC
            jsr STOP
            beq @clear_rcv_buffer_then_done ; runstop pressed
            inc ptr_lo
            bne @get_nextbyte
            inc ptr_hi
            bne @get_nextbyte

@clear_rcv_buffer_then_done:
            lda #CR
            jsr CHROUT
            jsr init_rcv_buffer_ptr

@done:
            jsr UNLSN
            jsr CLRCHN
            lda #CR
            jsr output_char_to_screen
            jsr open_modem_device
            lda #%00010111         ; scroll 7, 24 rows
            sta VIC_CTRL1
            jmp main


output_char_to_screen:
            bit rcv_flag
            bpl @skip_buffering
            jsr add_to_rcv_buffer
@skip_buffering:
            cmp #CR
            bne @is_not_cr
            jsr output_space_and_cursor_left_to_screen
@is_cr:
            lda #CR
            bne @chrout_and_finescroll_ifneeded
@is_not_cr:
            bit petscii_flag
            bmi @is_petscii
            cmp #BS
            beq @is_backspace
            cmp #DEL
            beq @is_backspace
            bne @is_not_backspace
@is_petscii:
            cmp #delete
            bne @is_not_backspace
@is_backspace:
            jsr output_backspace_to_screen
            jmp finescroll_oneline_ifneeded
@is_not_backspace:
            cmp #quote
            bne @chrout_and_finescroll_ifneeded
            jsr CHROUT
            lda #quote
            jsr CHROUT
            lda #cursor_left
@chrout_and_finescroll_ifneeded:
            jsr CHROUT


finescroll_oneline_ifneeded:
            bit show_cursor_flag
            bpl :+
            lda #underscore
            jsr CHROUT
            lda #cursor_left
            jsr CHROUT
:           jmp finescroll_oneline


clear_screen:
            jsr finescroll_clearscreen
            lda #0                 ; disable all sprites
            sta VIC_SPR_ENA
            rts


output_space_and_cursor_left_to_screen:
            lda #space
            jsr CHROUT
            lda #cursor_left
            jmp CHROUT


output_backspace_to_screen:
            lda #0
            sta move_cursor_up_oneline_flag
            lda PNTR
            bne @cursor_not_at_startof_line
            lda #$80
            sta move_cursor_up_oneline_flag
            lda TBLX
            cmp #3
            bcc @done
@cursor_not_at_startof_line:
            jsr output_space_and_cursor_left_to_screen
            lda #cursor_left
            jsr CHROUT
            bit move_cursor_up_oneline_flag
            bpl @done
            dec TBLX               ; move current cursor up one physical line
@done:
            rts


output_string_to_screen:
            sty ptr_lo
            sta ptr_hi
            ldy #0
@next:
            lda (ptr),y
            beq @done              ; strings are zero-terminated
            jsr output_char_to_screen
            iny
            bne @next
            inc ptr_hi
            bne @next
@done:
            rts


delay_onethirdsec:
            ldx #0
@loop1:
            ldy #0
@loop2:
            dey
            bne @loop2
            dex
            bne @loop1
            rts


print_title_message_to_screen:
            lda #0
            sta rcv_flag
            ldx #20
            stx TBLX

            ldy #<title_message
            lda #>title_message
            jsr output_string_to_screen

            jsr output_space_and_cursor_left_to_screen

            jsr delay_onethirdsec
            jsr delay_onethirdsec
            jsr delay_onethirdsec
            jsr delay_onethirdsec
            jsr delay_onethirdsec
            jsr delay_onethirdsec
            jsr delay_onethirdsec
            jsr delay_onethirdsec
            jsr delay_onethirdsec
            jmp delay_onethirdsec


accept_presets_menu:
            lda #0
            sta NDX
            sta rcv_flag
            sta petscii_flag
            jsr clear_screen
            jsr output_clear_and_cursor_down_to_screen
            lda #0
            sta dpx_flag
            lda #$80               ; set color
            sta ntsc_flag          ; set NTSC
            sta color_flag
            lda #0                 ; user specified baud, 8 bits, 1 stop bit
            sta serial_config
            lda #0
            sta serial_config+1    ; no parity
            ldx #2                 ; 1200 baud
            stx baud_selection
            dex
            txa
            asl a
            bit ntsc_flag
            bmi :+
            clc
            adc baud_offset
:           tax
            lda baud,x
            sta serial_config+2
            lda baud+1,x
            sta serial_config+3

presets_start:
            ldy #<presets
            lda #>presets
            jsr output_string_to_screen
@accept_q:
            jsr GETIN
            cmp #CR
            beq @accept_y
            cmp #ychar
            beq @accept_y
            cmp #nchar
            beq @tv_start
            bne @accept_q
@accept_y:
            lda #ychar
            jsr output_char_to_screen
            jmp @done
@tv_start:
            jsr delay_onethirdsec
            jsr output_clear_and_cursor_down_to_screen
            ldy #<presets_tv
            lda #>presets_tv
            jsr output_string_to_screen
@tv_q:
            jsr GETIN
            beq @tv_q
            cmp #$31
            bne @tv_n1
            jsr output_char_to_screen
            lda #$80
            sta ntsc_flag
            jmp @baud_start
@tv_n1:
            cmp #$32
            bne @tv_q
            jsr output_char_to_screen
            lda #0
            sta ntsc_flag
@baud_start:
            jsr delay_onethirdsec
            jsr output_clear_and_cursor_down_to_screen
            ldy #<presets_baud
            lda #>presets_baud
            jsr output_string_to_screen
@baud_q:
            jsr GETIN
            beq @baud_q
            cmp #$31
            bcc @baud_q
            cmp #$34
            bcs @baud_q
            pha
            jsr output_char_to_screen
            pla
            and #7
            tax
            stx baud_selection
            dex
            txa
            asl a
            bit ntsc_flag
            bmi :+
            clc
            adc baud_offset
:           tax
            lda baud,x
            sta serial_config+2
            lda baud+1,x
            sta serial_config+3
@set_8n1:
            lda serial_config
            and #%10011111         ; 8 bit
            and #%01111111         ; 1 stop bit
            sta serial_config
            lda serial_config+1
            and #%00011111         ; no parity
            sta serial_config+1
@charset_start:
            jsr delay_onethirdsec
            jsr output_clear_and_cursor_down_to_screen
            ldy #<presets_charset
            lda #>presets_charset
            jsr output_string_to_screen
@charset_q:
            jsr GETIN
            beq @charset_q
            cmp #$31
            bne @charset_n1
            jsr output_char_to_screen
            lda #0
            sta petscii_flag
            beq @tvtype_start
@charset_n1:
            cmp #$32
            bne @charset_q
            jsr output_char_to_screen
            lda #$80
            sta petscii_flag
@tvtype_start:
            jsr delay_onethirdsec
            jsr output_clear_and_cursor_down_to_screen
            ldy #<presets_tvtype
            lda #>presets_tvtype
            jsr output_string_to_screen
@tvtype_q:
            jsr GETIN
            beq @tvtype_q
            cmp #$31
            bne @tvtype_n1
            jsr output_char_to_screen
            lda #0                 ; set b/w
            sta color_flag
            beq @done
@tvtype_n1:
            cmp #$32
            bne @tvtype_q
            jsr output_char_to_screen
            lda #$80               ; set color
            sta color_flag
@done:
            jsr delay_onethirdsec
            jsr output_clear_and_cursor_down_to_screen
            jsr config_colors
            lda #$ff               ; turn on all sprites
            sta VIC_SPR_ENA
            jsr output_cursor_home_and_cursor_down
            lda #0
            sta show_cursor_flag
            ldy #<help_text
            lda #>help_text
            jsr output_string_to_screen
            lda #$80
            sta show_cursor_flag



config_colors:
            bit color_flag
            bmi :+
            lda #black
            beq :++
:           lda #blue
:           sta VIC_BORDERCOLOR
            sta VIC_BG_COLOR0
            lda #white
            sta COLOR
            jsr finescroll_settextcolor
            lda #gray
            sta VIC_SPR0_COLOR
            sta VIC_SPR1_COLOR
            sta VIC_SPR2_COLOR
            sta VIC_SPR3_COLOR
            sta VIC_SPR4_COLOR
            sta VIC_SPR5_COLOR
            rts


indicate_dpx_config:
            ldx #6
            ldy #2
            bit dpx_flag
            bmi @set_indication
@clear_indication:
            lda sprite_data_base_addr+offset_dpx*64,y
            and #%11000000
            sta sprite_data_base_addr+offset_dpx*64,y
            iny
            iny
            iny
            dex
            bpl @clear_indication
            rts
@set_indication:
            lda sprite_data_base_addr+offset_dpx*64,y
            ora #%00111100
            sta sprite_data_base_addr+offset_dpx*64,y
            iny
            iny
            iny
            dex
            bpl @set_indication
            rts


indicate_rcv_flag:
            ldx #6
            ldy #2
            bit rcv_flag
            bmi @set_indication
@clear_indication:
            lda sprite_data_base_addr+offset_rcv,y
            and #%11000000
            sta sprite_data_base_addr+offset_rcv,y
            iny
            iny
            iny
            dex
            bpl @clear_indication
            rts
@set_indication:
            lda sprite_data_base_addr+offset_rcv,y
            ora #%00111100
            sta sprite_data_base_addr+offset_rcv,y
            iny
            iny
            iny
            dex
            bpl @set_indication
            rts


finescroll_clearscreen:
            jmp finescroll_funcs+0
finescroll_settextcolor:
            jmp finescroll_funcs+3
finescroll_oneline:
            jmp finescroll_funcs+6


setup_modem_device:
            ldx #0
            ldy #0
            lda baud_selection
            cmp #1    ; 2400
            bne :+
            inx
            inx
            bne :++
:           cmp #2    ; 1200
            bne :+
            inx
:           bit ntsc_flag
            bmi :+
            iny
            ; x: baud_rate: 0=300, 1=1200, 2=2400; y: 0=ntsc, 1=pal
:           jmp rs232_userport_funcs+0
open_modem_device:
            jmp rs232_userport_funcs+3
close_modem_device:
            jmp rs232_userport_funcs+6
getchar_from_modem_device:
            jmp rs232_userport_funcs+9
putchar_to_modem_device:
            jmp rs232_userport_funcs+12


open_printer_device:
            lda #0                 ; filename length
            jsr SETNAM
            lda #printer_file_no
            ldx #printer_device_no
            ldy #7                 ; secondary address
            jsr SETLFS
            jmp OPEN


init_rcv_buffer_ptr:
            lda #<startof_rcv_buffer_addr
            sta rcv_buffer_ptr_lo
            lda #>startof_rcv_buffer_addr
            sta rcv_buffer_ptr_hi
            ldy #0                 ; buffer is zero-terminated
            tya
            sta (rcv_buffer_ptr),y
            sta rcv_flag
            sta rcv_notadded_flag
            sta rcv_buffer_full_flag
            jmp indicate_rcv_flag


output_clear_and_cursor_down_to_screen:
            lda #cls
            jsr CHROUT

output_cursor_home_and_cursor_down:
            lda #cursor_home
            jsr CHROUT
            lda #cursor_down
            jsr CHROUT
            jmp finescroll_oneline_ifneeded


add_to_rcv_buffer:
            ldy #0
            sta save_a
            cmp #BS
            beq remove_from_rcv_buffer
            cmp #DEL
            beq remove_from_rcv_buffer
            bit rcv_buffer_full_flag
            bmi @done
            sty rcv_notadded_flag  ; y expected to be zero, so indicate 'added'
            sta (rcv_buffer_ptr),y
            inc rcv_buffer_ptr_lo
            bne @done
            inc rcv_buffer_ptr_hi
            lda rcv_buffer_ptr_hi
            cmp #>endof_rcv_buffer_addr
            bcc @done
            dec rcv_buffer_ptr_hi
            dec rcv_buffer_ptr_lo
            lda #$80               ; set buffer full
            sta rcv_buffer_full_flag
            lda #0
            sta rcv_flag
            jsr indicate_rcv_flag
@done:
            lda #0
            sta (rcv_buffer_ptr),y
            lda save_a
            rts


remove_from_rcv_buffer:
            tya
            sta (rcv_buffer_ptr),y
            lda rcv_buffer_ptr_lo
            bne @decrement_then_done
            lda #>startof_rcv_buffer_addr
            cmp rcv_buffer_ptr_hi
            beq @done
            dec rcv_buffer_ptr_hi
@decrement_then_done:
            dec rcv_buffer_ptr_lo
@done:
            lda save_a
            rts


dump_rcv_buffer_to_screen:
            jsr CLRCHN
            lda #<startof_rcv_buffer_addr
            sta ptr_lo
            lda #>startof_rcv_buffer_addr
            sta ptr_hi
            lda #0
            sta rcv_flag
            sta rcv_notadded_flag
            jsr indicate_rcv_flag
            lda #CR
            jsr output_char_to_screen

@get_and_output_byte:
            ldy #0
            lda (ptr),y
            beq @done
            jsr output_char_to_screen
            jsr GETIN
            cmp #F7
            beq @done
            cmp #space
            bne @increment_ptr
@pause:
            jsr GETIN
            cmp #F7
            beq @done
            cmp #space
            bne @pause
@increment_ptr:
            inc ptr_lo
            bne @get_and_output_byte
            inc ptr_hi
            bne @get_and_output_byte
@done:
            lda #CR
            jsr output_char_to_screen
            jsr open_modem_device
            jmp main


init_sprites:
            ldx #$ff               ; set all sprites to double-width
            stx VIC_SPR_EXP_X
            lda #0                 ; set all sprites to single-height, single color
:           sta sprite_data_base_addr,x
            sta sprite_data_base_addr+$100,x
            dex
            bne :-
            sta VIC_SPR_EXP_Y
            sta VIC_SPR_MCOLOR
            ldx #20
:           lda sprite_data_rcv,x
            sta sprite_data_base_addr+offset_rcv*64,x
            lda sprite_data_dpx,x
            sta sprite_data_base_addr+offset_dpx*64,x
            lda sprite_data_64t,x
            sta sprite_data_base_addr+offset_64t*64,x
            lda sprite_data_ermi,x
            sta sprite_data_base_addr+offset_ermi*64,x
            lda sprite_data_nal,x
            sta sprite_data_base_addr+offset_nal*64,x
            dex
            bpl :-
            lda #54
            sta VIC_SPR0_Y
            sta VIC_SPR1_Y
            sta VIC_SPR2_Y
            sta VIC_SPR3_Y
            sta VIC_SPR4_Y
            lda #0
            sta VIC_SPR_HI_X
            lda #184
            sta VIC_SPR0_X
            lda #240
            sta VIC_SPR1_X
            lda #25
            sta VIC_SPR2_X
            lda #74
            sta VIC_SPR3_X
            lda #123
            sta VIC_SPR4_X
            ldx #(sprite_data_base_addr >> 6) + offset_rcv
            stx lo_screen_addr+sprite_data_ptr_offset
            stx hi_screen_addr+sprite_data_ptr_offset
            ldx #(sprite_data_base_addr >> 6) + offset_dpx
            stx lo_screen_addr+sprite_data_ptr_offset+1
            stx hi_screen_addr+sprite_data_ptr_offset+1
            ldx #(sprite_data_base_addr >> 6) + offset_64t
            stx lo_screen_addr+sprite_data_ptr_offset+2
            stx hi_screen_addr+sprite_data_ptr_offset+2
            ldx #(sprite_data_base_addr >> 6) + offset_ermi
            stx lo_screen_addr+sprite_data_ptr_offset+3
            stx hi_screen_addr+sprite_data_ptr_offset+3
            ldx #(sprite_data_base_addr >> 6) + offset_nal
            stx lo_screen_addr+sprite_data_ptr_offset+4
            stx hi_screen_addr+sprite_data_ptr_offset+4
            rts

            .rodata
sprite_data_64t:
                                ; ------------------------
            .byte $70, $40, $1f ;  111     1         11111
            .byte $80, $c0, $04 ; 1       11           1
            .byte $81, $40, $04 ; 1      1 1           1
            .byte $f2, $40, $04 ; 1111  1  1           1
            .byte $8b, $e0, $04 ; 1   1 11111          1
            .byte $88, $40, $04 ; 1   1    1           1
            .byte $70, $40, $04 ;  111     1           1
sprite_data_ermi:
                                ; ------------------------
            .byte $fb, $c8, $9c ; 11111 1111  1   1  111
            .byte $82, $2d, $88 ; 1     1   1 11 11   1
            .byte $82, $2a, $88 ; 1     1   1 1 1 1   1
            .byte $e3, $c8, $88 ; 111   1111  1   1   1
            .byte $82, $88, $88 ; 1     1 1   1   1   1
            .byte $82, $48, $88 ; 1     1  1  1   1   1
            .byte $fa, $28, $9c ; 11111 1   1 1   1  111
sprite_data_nal:
                                ; ------------------------
            .byte $89, $c8, $00 ; 1   1  111  1
            .byte $8a, $28, $00 ; 1   1 1   1 1
            .byte $ca, $28, $00 ; 11  1 1   1 1
            .byte $ab, $e8, $00 ; 1 1 1 11111 1
            .byte $9a, $28, $00 ; 1  11 1   1 1
            .byte $8a, $28, $00 ; 1   1 1   1 1
            .byte $8a, $2f, $80 ; 1   1 1   1 1111
sprite_data_rcv:
                                ; ------------------------
            .byte $f1, $c8, $80 ; 1111   111  1   1
            .byte $8a, $28, $80 ; 1   1 1   1 1   1
            .byte $8a, $08, $80 ; 1   1 1     1   1
            .byte $f2, $05, $00 ; 1111  1      1 1
            .byte $a2, $05, $00 ; 1 1   1      1 1
            .byte $92, $22, $00 ; 1  1  1   1   1
            .byte $89, $c2, $00 ; 1   1  111    1
sprite_data_dpx:
                                ; ------------------------
            .byte $e3, $c8, $80 ; 111   1111  1   1
            .byte $92, $28, $80 ; 1  1  1   1 1   1
            .byte $8a, $25, $00 ; 1   1 1   1  1 1
            .byte $8b, $e2, $00 ; 1   1 11111   1
            .byte $8a, $05, $00 ; 1   1 1      1 1
            .byte $92, $08, $80 ; 1  1  1     1   1
            .byte $e2, $08, $80 ; 111   1     1   1

title_message:
                  ;----------------------------------------
            .byte $91
            .byte "             64 TERMINAL NG"
            .byte $0d, $0d, $0d, $0d
            .byte "                   by"
            .byte $0d, $0d, $0d, $0d
            .byte "            Dr. Jim Rothwell"
            .byte $0d, $0d, $0d, $0d, $0d, $0d
            .byte "               (c) 1983"
            .byte $0d, $0d
            .byte "           Midwest Micro Inc."
            .byte $0d, 0

presets:
            .byte "***presets***"
            .byte $0d, $0d
            .byte "tv: NTSC"
            .byte $0d
            .byte "baud: 1200"
            .byte $0d
            .byte "wordsize: 8 bits"
            .byte $0d
            .byte "parity: none"
            .byte $0d
            .byte "stopbits: 1"
            .byte $0d
            .byte "charset: ascii"
            .byte $0d
            .byte "tvtype: color"
            .byte $0d, $0d
            .byte "accept presets? "
            .byte 0
presets_tv:
            .byte "***tv***"
            .byte $0d, $0d
            .byte "1. NTSC"
            .byte $0d
            .byte "2.  PAL"
            .byte $0d, $0d
            .byte "selection? "
            .byte 0
presets_baud:
            .byte "***baud***"
            .byte $0d, $0d
            .byte "1. 2400"
            .byte $0d
            .byte "2. 1200"
            .byte $0d
            .byte "3.  300"
            .byte $0d, $0d
            .byte "selection? "
            .byte 0
presets_charset:
            .byte "***charset***"
            .byte $0d, $0d
            .byte "1. ASCII"
            .byte $0d
            .byte "2. PETSCII"
            .byte $0d, $0d
            .byte "selection? "
            .byte 0
presets_tvtype:
            .byte "***tvtype***"
            .byte $0d, $0d
            .byte "1.   b/w"
            .byte $0d
            .byte "2. color"
            .byte $0d, $0d
            .byte "selection? "
            .byte 0
help_text:
            .byte $0d
            .byte "F1  This help text                    "
            .byte $0d
            .byte "F2  ReCeiVe buffer toggle             "
            .byte $0d
            .byte "F4  Half DuPleX toggle                "
            .byte $0d
            .byte "F6  Reset program                     "
            .byte $0d
            .byte "F7  Review RCV buffer toggle,         "
            .byte $0d
            .byte "    SPACE pauses review               "
            .byte $0d
            .byte "F8  Dump RCV buffer to printer,       "
            .byte $0d
            .byte "    RUNSTOP stops printing            "
            .byte $0d, $0d
            .byte 0

err_printer_offline_message:
            .byte $0d
            .byte "* * * ERROR: Printer Off-Line * * *"
            .byte $0d, 0

ntsc_clock_freq = 1022727
pal_clock_freq  =  985248

baud:
ntsc_baud:
            .word ntsc_clock_freq/2400/2 - 100  ; 2400
            .word ntsc_clock_freq/1200/2 - 100  ; 1200
            .word ntsc_clock_freq/ 300/2 - 100  ; 300
pal_baud:
            .word pal_clock_freq /2400/2 - 100  ; 2400
            .word pal_clock_freq /1200/2 - 100  ; 1200
            .word pal_clock_freq / 300/2 - 100  ; 300

baud_offset = pal_baud - ntsc_baud

            .bss
baud_selection:
            .byte 0
move_cursor_up_oneline_flag:
            .byte 0
save_a:     .byte 0

ntsc_flag:
            .byte 0  ; bit7: 0=PAL, 1=NTSC
dpx_flag:
            .byte 0  ; bit7: 0=full duplex, 1=half duplex
color_flag:
            .byte 0  ; bit7: 0=b/w, 1=color

; Receive buffer size: $6000 bytes => 24,576 bytes => 24K
startof_rcv_buffer_addr:
            .res $6000
endof_rcv_buffer_addr:
