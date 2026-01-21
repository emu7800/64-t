;
; '64 Terminal
;
; Telecommunications program for the Commodore 64
;
; - Smooth scrolling display!
; - Set-up menu/function key modes
; - 24K Receive Buffer with Review
;   and VIC printer dump
; - Fully programmable Baud, Duplex,
;   Parity, Wordsize, Stopbit, Linefeed
; - Requires modem, VIC printer optional
;
; By Dr. Jim Rothwell, Midwest Micro Inc, circa 1983
;
;
; Keys/functions:
;
; - F1/F2              RCV: Toggle receive buffering (on/off)
; - F3/F4              DPX: Toggle between full/half duplex (full/half)
; - F5/F6              FMT: Toggle format mode (eliminates extra spaces, off/on)
; - F7                 Copies receive buffer to screen (Review feature)
; - F8                 Dumps receive buffer to printer, RUNSTOP stops printing
; - RUNSTOP + RESTORE  Resets to the Accept Presets page
; - CTRL + RUNSTOP     Toggles the TxD line (not sure why useful)
; - F1 + RESTORE       Clears receive buffer
;
;
; Reverse engineered by Mike Murphy (mike@emu7800.net), circa 2023.
; Intended for preservation and educational purposes only.
;
; To build, invoke 'make' in this directory without arguments.
;

.setcpu "6502"
.include "cbm_kernal.inc"
.include "c64.inc"

ntsc_clock_freq         = 1023000  ; ntsc is actually 1022727, pal is 985248

ntsc_baud_110           = ntsc_clock_freq/110/2 - 100
ntsc_baud_150           = ntsc_clock_freq/150/2 - 100
ntsc_baud_300           = ntsc_clock_freq/300/2 - 100

stop_char               = 3     ; PETSCII stop
disable_logoshift       = 8     ; PETSCII disable keyboard LOGO+SHIFT combination
BS                      = 8     ; PETSCII/ASCII backspace
LF                      = 10    ; PETSCII/ASCII linefeed
CR                      = 13    ; PETSCII/ASCII carriage return
toggle_charset          = 14    ; PETSCII toggle character se
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
DEL                     = 127   ; PETSCII/ASCII delete
F1                      = 133   ; PETSCII F1
F3                      = 134   ; PETSCII F3
F5                      = 135   ; PETSCII F5
F7                      = 136   ; PETSCII F7
F2                      = 137   ; PETSCII F2
F4                      = 138   ; PETSCII F4
F6                      = 139   ; PETSCII F6
F8                      = 140   ; PETSCII F8
shift_return            = 141   ; PETSCII SHIFT+RETURN
cursor_up               = 145   ; PETSCII cursor up
clear_screen            = 147   ; PETSCII clear screen
insert                  = 148   ; PETSCII insert
cursor_left             = 157   ; PETSCII cursor left
underscore              = 164   ; PETSCII underscore

bit_opcode              = $2c
ldx_opcode              = $ae
sta_opcode              = $8d

accumlator_save         = $03
dpx_flag                = $1f   ; bit7: 0=full, 1=half
linefeed_flag           = $20   ; bit7: 0=off, 1=on
color_flag              = $21   ; bit7: 0=b/w, 1=color
FREETOP                 = $33   ; Pointer to the Bottom of the String Text Storage Area
FRESPC                  = $35   ; Temporary Pointer for Strings
MEMSIZ1                 = $37   ; Pointer to the Highest Address Used by BASIC
fmt_flag                = $5e   ; bit7: 1=off; bit0-7: 0=on: discard spaces, 1=on: dont discard spaces
last_keycode            = $5d
serial_config           = $5f   ; Four (4) byte string for serial port configuration
rs232_txd_flag          = $63   ; bit7: 1=toggled txd
rcv_flag                = $69   ; bit7: 0=off, 1=on
rcv_buffer_ptr          = $6a
rcv_buffer_ptr_lo       = rcv_buffer_ptr
rcv_buffer_ptr_hi       = rcv_buffer_ptr+1
ptr                     = $6c
ptr_lo                  = ptr
ptr_hi                  = ptr+1
rcv_buffer_full_flag    = $6e   ; bit7: 0=not full, 1=full
rcv_notadded_flag       = $6f   ; bit7: added to buffer since rcv on: 0=byte added, 1=byte not added
restore_key_flag        = $70   ; bit7: 0=not pressed, 1=pressed
NDX                     = $c6   ; Number of Characters in Keyboard Buffer (Queue)
SFDX                    = $cb   ; Matrix Coordinate of Current Key Pressed
PNTR                    = $d3   ; Cursor column on current logical line (0-79)
TBLX                    = $d6   ; Current cursor physical line number (0-24)
MEMSIZ2                 = $283  ; Pointer: O.S. End of Memory
COLOR                   = $286  ; Current Foreground Color for Text (values 0-f)
HIBASE                  = $288  ; Top page of screen memory
SHFLAG                  = $28d  ; Flag: SHIFT/CTRL/LOGO Keypress bit0=SHIFT, bit1=LOGO, bit2=CTRL


sprite_data_base_addr   = $2000
offset_rcv              = 0  ; RCF sprite
offset_dpx              = 1  ; DPX sprite
offset_fmt              = 2  ; FMT sprite
offset_64t              = 3  ; 64T sprite
offset_ermi             = 4  ; ERMI sprite
offset_nal              = 5  ; NAL sprite


; Receive buffer size: $6000 bytes => 24,576 bytes => 24K
startof_rcv_buffer_addr = $3000
endof_basic_addr        = $9000


sprite_data_ptr_offset  = $3f8  ; Start of Sprite Shape Data Pointers
lo_screen_nybble        = $1
hi_screen_nybble        = $a
lo_screen_addr          = $0400*lo_screen_nybble ; = $0400 video matrix addr
hi_screen_addr          = $0400*hi_screen_nybble ; = $2800 video matrix addr


modem_file_no           = 128
modem_device_no         = 2
printer_file_no         = 4
printer_device_no       = 4

; C64 kernal addresses not in cbm_kernal.inc
USKINDIC                = $f6bc ; Update Stop key indicator, at memory address $0091, A and X registers used

.segment "LOADADDR"
.import __LOADADDR__
.word   __LOADADDR__

.segment "EXEHDR"
startofexeheader:
           ; 0 SYSnnnn:REM   * * * 64 TERMINAL * * *
           .word endofbasicprogram
           .byte $00, $00 ; 0
           .byte $9e      ; SYS
           .byte $30 + .lobyte ((start .mod 10000) / 1000)
           .byte $30 + .lobyte ((start .mod 1000) / 100)
           .byte $30 + .lobyte ((start .mod 100) / 10)
           .byte $30 + .lobyte ((start .mod 10) / 1)
           .byte $3a      ; :
           .byte $8f      ; REM
           .asciiz "   * * * 64 terminal * * *"

endofbasicprogram:
           .byte 0, 0

.code
start:
           jsr copy_cart_signature_for_nono_routine
           jsr clear_both_screens
           lda #toggle_charset
           jsr CHROUT
           lda #disable_logoshift
           jsr CHROUT
           lda #>sprite_data_base_addr
           sta MEMSIZ2+1
           sta FRESPC+1
           sta FREETOP+1
           lda #>endof_basic_addr
           sta MEMSIZ1+1
           lda #%00010111          ; scroll 7, 24 rows
           sta VIC_CTRL1
           lda #6                  ; blue
           sta VIC_BORDERCOLOR
           sta VIC_BG_COLOR0
           lda #1                  ; white
           sta COLOR

           ; load nmihandler into NMI interrupt vector
           lda #<nmihandler
           sta NMIVec
           lda #>nmihandler
           sta NMIVec+1

           lda #$80                ; set to color mode
           sta color_flag

           jsr output_cursor_home_and_cursor_down
           jsr output_space_and_cursor_left_to_screen
           lda #0                  ; dont show cursor, set low screen
           sta show_cursor_flag
           sta screen_select_flag
           jsr print_title_message_to_screen
reset:
           jsr init_sprites
           lda #0
           sta restore_key_flag
           lda #$80                ; show cursor
           sta show_cursor_flag
           jsr accept_presets_menu
           jsr init_rcv_buffer_ptr
           jsr CLALL
           lda #$80
           sta rcv_notadded_flag
           lda #0
           sta rs232_txd_flag
           jsr open_printer_device
           jsr open_modem_device
main:
           jsr CLRCHN

           jsr USKINDIC
           jsr STOP
           bne get_byte_from_keyboard

           ; runstop already pressed, check for restore
           bit restore_key_flag
           bmi reset

           ; runstop already pressed, check for CTRL
           jsr $eb1e ; somewhere in the middle of KEYLOG:($028f)=$eadd, evaluate shift functions
           jsr USKINDIC
           lda SHFLAG
           cmp #4                 ; Is CTRL pressed?
           beq toggle_rs232_txd_and_set_txd_flag

get_byte_from_keyboard:
           jsr GETIN
           beq check_for_ctrlrunstop
           cmp #stop_char
           beq check_for_ctrlrunstop
           bit restore_key_flag
           bpl handle_char_from_kbd

           ; restore pressed
           pha
           jsr USKINDIC
           jsr STOP
           beq handle_runstop_restore

           pla
           jmp handle_char_from_kbd

           ; runstop pressed
handle_runstop_restore:
           pla
           jmp reset

check_for_ctrlrunstop:
           lda SHFLAG
           cmp #4                 ; Is CTRL pressed?
           bne clear_last_keycode_then_get_char_from_modem

           ; CTRL pressed
           jsr USKINDIC
           jsr STOP
           bne get_current_key_pressed ; runstop not pressed

           ; CTRL+runstop pressed, toggle RS232 TXD
toggle_rs232_txd_and_set_txd_flag:
           jsr toggle_rs232_txd
           lda #$80
           sta rs232_txd_flag
           bne main

get_current_key_pressed:
           .byte ldx_opcode
           .word SFDX             ; avoid zero-page addressing optimization
           cpx #64                ; no key pressed
           beq go_get_char_from_modem
           lda $eb81,x            ; keyboard decode table: keyboard1 unshifted
           and #$1f
           cmp last_keycode
           beq go_get_char_from_modem
           sta last_keycode
           pha
           jmp output_char_to_modem
clear_last_keycode_then_get_char_from_modem:
           lda #0
           sta rs232_txd_flag
           sta last_keycode
go_get_char_from_modem:
           jmp get_char_from_modem


handle_char_from_kbd:
           bit restore_key_flag
           bpl @next
           cmp #F1
           bne @next
           ; F1 + restore pressed
           jsr init_rcv_buffer_ptr
           lda #0
           sta restore_key_flag
           jmp main
@next:
           ldx #0
           stx restore_key_flag
           cmp #128
           bcc continue_handle_char_from_kbd
           cmp #129
           beq @done
           cmp #142
           bcc continue_handle_char_from_kbd
           cmp #clear_screen
           beq continue_handle_char_from_kbd
           cmp #insert
           beq continue_handle_char_from_kbd
           cmp #160
           bcs continue_handle_char_from_kbd
@done:
           jmp main

continue_handle_char_from_kbd:
           cmp #F1                ; turn off RCV
           bne @next2
           bit rcv_flag
           bmi @test_rcv_notadded_flag
           jmp main
@test_rcv_notadded_flag:
           bit rcv_notadded_flag
           bmi @skip_cr
           lda #CR
           jsr output_char_to_screen
@skip_cr:
           lda #0                 ; clear
           sta rcv_flag
           jsr indicate_rcv_flag
           jmp main
@next2:
           cmp #F2                ; turn on RCV
           bne @next3
           bit rcv_buffer_full_flag
           bpl @rcv_buffer_not_full
           jmp main               ; dont bother since buffer full
@rcv_buffer_not_full:
           bit rcv_flag
           bpl @set_rcv_flag
           jmp main
@set_rcv_flag:
           lda #CR
           jsr output_char_to_screen
           lda #$80
           sta rcv_flag
           sta rcv_notadded_flag
           jsr indicate_rcv_flag
           jmp main
@next3:
           cmp #F3                ; turn on DPX
           bne @next4
           lda #0                 ; full
           sta dpx_flag
           jsr indicate_dpx_config
           jmp main
@next4:
           cmp #F4                ; turn off DPX
           bne @next5
           lda #$80               ; half
           sta dpx_flag
           jsr indicate_dpx_config
           jmp main
@next5:
           cmp #F5                ; turn off FMT
           bne @next6
           lda #$80               ; off
           sta fmt_flag
           jsr indicate_fmt_flag
           jmp main
@next6:
           cmp #F6                ; turn on FMT
           bne @next7
           lda #0                 ; on
           sta fmt_flag
           jsr indicate_fmt_flag
           jmp main
@next7:
           cmp #F7
           bne @next8
           jmp dump_rcv_buffer_to_screen
@next8:
           cmp #F8
           bne @next9
           jmp dump_rcv_buffer_to_printer
@next9:
           cmp #insert
           bne @next10
           lda #DEL
           bne output_char_to_modem_with_dpx_echo
@next10:
           cmp #$ba               ; unknown
           bne @next11
           lda #$60               ; PETSCII horizontal line
           bne output_char_to_modem_with_dpx_echo
@next11:
           cmp #clear_screen
           bne @next12
           jsr output_clear_and_cursor_down_to_screen
           jmp main
@next12:
           cmp #shift_return
           bne @next13
           and #$7f
@next13:
           cmp #CR
           beq output_char_to_modem_with_dpx_echo

           cmp #delete
           bne @next14
           lda #BS
           bne output_char_to_modem_with_dpx_echo
@next14:
           cmp #space
           bcs output_char_to_modem_with_dpx_echo
           jmp main


output_char_to_modem_with_dpx_echo:
           pha

           ; Convert ASCII uppercase character to PETSCII uppercase only for local character echo (half duplex)
           bit case_flag
           bpl @next1

           cmp #achar
           bcc @next1
           cmp #zchar+1
           bcs @next1
           ora #$80
@next1:
           .byte bit_opcode
           .word dpx_flag          ; avoid zero-page addressing optimization
           bpl output_char_to_modem
           jsr output_char_to_screen


           ; Expects character to output to be on top of the stack
output_char_to_modem:
           jsr CLRCHN
           ldx #modem_file_no
           jsr CHKOUT
           pla
           cmp #BS
           beq @done
           cmp #DEL
           beq @done
           bit case_flag
           bpl @next1
           ; Convert PETSCII uppercase only character to ASCII upper case
           cmp #$c1               ; PETSCII uppercase only A
           bcc @next2
           cmp #$db               ; PETSCII uppercase only Z + 1
           bcs @next2
           and #$7f
           bne @next2             ; needed? lol
           jmp @next2

@next1:    ; Convert to ASCII
           cmp #achar
           bcc @next2
           cmp #zchar+1
           bcs @done
           adc #32

@next2:
           and #$7f
           cmp #CR
           bne @done
           jsr CHROUT
           .byte bit_opcode
           .word linefeed_flag     ; avoid zero-page addressing optimization
           bpl get_char_from_modem
           lda #LF
@done:
           jsr CHROUT


get_char_from_modem:
           jsr CLRCHN
           ldx #modem_file_no
           jsr CHKIN
           jsr GETIN
           pha
           jsr CLRCHN
           pla
           bne @handle_char
           jmp main

@handle_char:
           and #$7f
           cmp #BS
           beq output_byte_then_2main
           cmp #DEL
           beq output_byte_then_2main
           cmp #CR
           beq @handle_cr
           cmp #space
           bcs @handle_printable_char
           jmp main

@handle_cr:
           bit fmt_flag
           bmi @output_cr_then_2main
           lda #0                 ; set discard spaces
           sta fmt_flag
@output_cr_then_2main:
           lda #CR
           bne output_byte_then_2main

@handle_printable_char:
           bne @handle_nonspace_printable_char

           ; char is a space, discard if 'discard spaces' is on
           ldx fmt_flag
           beq get_char_from_modem

@handle_nonspace_printable_char:
           bit fmt_flag
           bmi convert_2petscii
           cmp #space
           bne set_nonspace_seen_fmt_flag
clear_nonspace_seen_fmt_flag:
           ldx #0                 ; set discard spaces
           .byte bit_opcode
set_nonspace_seen_fmt_flag:
           ldx #1                 ; set dont discard spaces
           stx fmt_flag
convert_2petscii:
           cmp #Achar
           bcc @convert_2petscii2
           sbc #32
           bne output_byte_then_2main
@convert_2petscii2:
           cmp #achar
           bcc output_byte_then_2main
           cmp #zchar+1
           bcs output_byte_then_2main
           adc #32
output_byte_then_2main:
           jsr output_char_to_screen
           jmp main


dump_rcv_buffer_to_printer:
           lda #0
           sta rcv_flag
           sta rcv_notadded_flag
           jsr indicate_rcv_flag
           lda #CR
           jsr output_char_to_screen
           jsr CLRCHN
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
           lda #modem_file_no
           jsr CLOSE
           jsr open_modem_device
           lda #%00010111         ; scroll 7, 24 rows
           sta VIC_CTRL1
           jmp main


clear_both_screens:
           lda #>hi_screen_addr
           sta HIBASE
           lda #clear_screen
           jsr CHROUT
           lda #>lo_screen_addr
           sta HIBASE
           lda #clear_screen
           jsr CHROUT
           lda #0                 ; select low screen
           sta screen_select_flag
           lda #(lo_screen_nybble << 4 | 6) ; 0001 0110  video matrix: 1*1k=$400, set low screen
           sta VIC_VIDEO_ADR
           lda #%00010111         ; scroll 7, 24 rows
           sta VIC_CTRL1
           lda #CR
           jsr CHROUT
           lda #0                 ; disable all sprites
           sta VIC_SPR_ENA
           rts


toggle_rs232_txd:
           lda CIA2
           and #%11111011         ; clear RS232 TXD line
           sta CIA2
           lda VIC_BORDERCOLOR
           ora #%00000001
           sta VIC_BORDERCOLOR
           jsr @delay_256x160
           lda CIA2
           ora #%00000100         ; set RS232 TXD line
           sta CIA2
           lda VIC_BORDERCOLOR
           and #%11111110
           sta VIC_BORDERCOLOR
           rts


@delay_256x160:
           ldx #160
@loop_outer:
           ldy #0
@loop_inner:
           dey
           bne @loop_inner
           dex
           bne @loop_outer
           rts


output_char_to_screen:
           bit rcv_flag
           bpl @next1
           jsr to_rcv_buffer
@next1:
           cmp #CR
           bne @next2
           jsr output_space_and_cursor_left_to_screen
           lda #CR
           bne @is_not_quote
@next2:
           cmp #BS
           beq @is_backspace
           cmp #DEL
           bne @is_not_backspace
@is_backspace:
           jsr output_backspace_to_screen
           jmp fine_scroll_one_line_if_needed

@is_not_backspace:
           cmp #quote
           bne @is_not_quote
           jsr CHROUT
           lda #quote
           jsr CHROUT
           lda #cursor_left
           jsr CHROUT
           jmp fine_scroll_one_line_if_needed

@is_not_quote:
           jsr CHROUT


fine_scroll_one_line_if_needed:
           bit show_cursor_flag
           bpl @next
           lda #underscore
           jsr CHROUT
           lda #cursor_left
           jsr CHROUT
@next:
           lda TBLX
           cmp #23
           bcs fine_scroll_one_line
           rts


show_cursor_flag:
           .byte $80                 ; bit7: 0=dont show cursor, 1=show cursor
screen_select_flag:
           .byte 0                   ; bit7: 0=low screen, 1=high screen


screen_copy_offset = (23-1)*40 - 3*$100

fine_scroll_one_line:
           stx save_x
           sty save_y

           ; copy to opposite screen one line up

           ldy #39
           lda #space
           bit screen_select_flag
           bmi @loop1_lo

           ; on low screen
@loop1_hi:
           sta hi_screen_addr+23*40,y
           dey
           bpl @loop1_hi

           ldy #0
@loop2_hi:
           lda lo_screen_addr+2*40+screen_copy_offset+2*$100,y
           sta hi_screen_addr+1*40+screen_copy_offset+2*$100,y
           lda lo_screen_addr+2*40+screen_copy_offset+$100,y
           sta hi_screen_addr+1*40+screen_copy_offset+$100,y
           lda lo_screen_addr+2*40+screen_copy_offset,y
           sta hi_screen_addr+1*40+screen_copy_offset,y

           lda #1        ; white
           sta $d800,y   ; color ram: set foreground color
           sta $d900,y   ; color ram: set foreground color
           sta $da00,y   ; color ram: set foreground color
           sta $db00,y   ; color ram: set foreground color
           iny
           bne @loop2_hi

           ldy #screen_copy_offset
@loop3_hi:
           lda lo_screen_addr+2*40,y
           sta hi_screen_addr+1*40,y
           dey
           bpl @loop3_hi
           bmi do_fine_scroll

           ; on high screen
@loop1_lo:
           sta lo_screen_addr+23*40,y
           dey
           bpl @loop1_lo

           ldy #0
@loop2_lo:
           lda hi_screen_addr+2*40+screen_copy_offset+2*$100,y
           sta lo_screen_addr+1*40+screen_copy_offset+2*$100,y
           lda hi_screen_addr+2*40+screen_copy_offset+$100,y
           sta lo_screen_addr+1*40+screen_copy_offset+$100,y
           lda hi_screen_addr+2*40+screen_copy_offset,y
           sta lo_screen_addr+1*40+screen_copy_offset,y
           iny
           bne @loop2_lo

           ldy #screen_copy_offset
@loop3_lo:
           lda hi_screen_addr+2*40,y
           sta lo_screen_addr+1*40,y
           dey
           bpl @loop3_lo


do_fine_scroll:
           ldy #7
@loop1:
           bit VIC_CTRL1
           bpl @loop1
           dec VIC_CTRL1             ; fine scroll one line

           ldx #200
@delay:
           dex
           bne @delay
           dey
           bne @loop1

@loop2:
           bit VIC_CTRL1
           bpl @loop2

           ; swap screens to implement carriage return

           bit screen_select_flag
           bmi @swap_to_lo_screen

@swap_to_hi_screen:
           lda #(hi_screen_nybble << 4 | 6) ; 1010 0110  video matrix: 10*1k=$2800
           sta VIC_VIDEO_ADR
           sta screen_select_flag
           lda #>hi_screen_addr
           sta HIBASE
           bne @done
@swap_to_lo_screen:
           lda #(lo_screen_nybble << 4 | 6) ; 0001 0110  video matrix: 1*1k=$400
           sta VIC_VIDEO_ADR
           lda #>lo_screen_addr
           sta HIBASE
           sta screen_select_flag
@done:
           lda #%00010111         ; scroll 7, 24 rows
           sta VIC_CTRL1
           ldy save_y
           ldx save_x
           lda #22
           sta TBLX
           lda #cursor_up
           jsr CHROUT
           lda #CR
           jsr CHROUT
           lda #underscore
           jsr CHROUT
           lda #cursor_left
           jsr CHROUT
           rts

save_y:     .byte $8F
save_x:     .byte $14


output_space_and_cursor_left_to_screen:
           lda #space
           jsr CHROUT
           lda #cursor_left
           jsr CHROUT
           rts


output_backspace_to_screen:
           lda #0
           sta @move_cursor_up_oneline_flag
           lda PNTR
           bne @cursor_not_at_startof_line
           lda #$80
           sta @move_cursor_up_oneline_flag
           lda TBLX
           cmp #3
           bcc @done
@cursor_not_at_startof_line:
           jsr output_space_and_cursor_left_to_screen
           lda #cursor_left
           jsr CHROUT
           bit @move_cursor_up_oneline_flag
           bpl @done
           dec TBLX               ; move current cursor up one physical line
@done:
           rts

@move_cursor_up_oneline_flag:
           .byte 0


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
           rts


title_message:
           .byte $91
           .byte "              64 TERMINAL"
           .byte $0d, $0d, $0d, $0d
           .byte "                   by"
           .byte $0d, $0d, $0d, $0d
           .byte "            Dr. Jim Rothwell"
           .byte $0d, $0d, $0d, $0d, $0d, $0d
           .byte "               (C) 1983"
           .byte $0d, $0d
           .byte "           Midwest Micro Inc."
           .byte $0d, 0

err_printer_offline_message:
           .byte $0d
           .byte "* * * ERROR: Printer Off-Line * * *"
           .byte $0d, 0


accept_presets_menu:
           lda #0
           sta NDX
           sta rcv_flag
           sta restore_key_flag
           sta case_flag
           jsr clear_both_screens
           jsr CLRCHN
           jsr output_clear_and_cursor_down_to_screen
           lda #0
           sta dpx_flag
           sta linefeed_flag
           lda #$80                ; set color_flag, clear fmt_flag
           sta color_flag
           sta fmt_flag

           lda #%00100000          ; user specified baud, 7 bits, 1 stop bit
           sta serial_config
           lda #%10100000          ; 3 lines, full duplex, mark parity
           sta serial_config+1
           lda #<ntsc_baud_300
           .byte sta_opcode
           .word serial_config+2   ; avoid zero-page addressing optimization
           lda #>ntsc_baud_300
           .byte sta_opcode
           .word serial_config+3   ; avoid zero-page addressing optimization
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
           beq @accept_n
           bne @accept_q
@accept_y:
           lda #ychar
           jsr output_char_to_screen
           jmp @done
@accept_n:
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
           dex
           lda baud_settings_lo,X
           sta serial_config+2
           lda baud_settings_hi,X
           sta serial_config+3
           jsr delay_onethirdsec

           jsr output_clear_and_cursor_down_to_screen
           ldy #<presets_linefeed
           lda #>presets_linefeed
           jsr output_string_to_screen
@linefeed_q:
           jsr GETIN
           beq @linefeed_q
           cmp #$31
           bne @linefeed_n
           jsr output_char_to_screen
           lda #0                 ; set linefeeds off
           sta linefeed_flag
           beq @parity_q1
@linefeed_n:
           cmp #$32
           bne @linefeed_q
           jsr output_char_to_screen
           lda #$80               ; set linefeeds on
           sta linefeed_flag
@parity_q1:
           jsr delay_onethirdsec
           jsr output_clear_and_cursor_down_to_screen
           ldy #<presets_parity
           lda #>presets_parity
           jsr output_string_to_screen
@parity_q:
           jsr GETIN
           beq @parity_q
           cmp #$31
           bcc @parity_q
           cmp #$36
           bcs @parity_q
           pha
           jsr output_char_to_screen
           pla
           and #7  ; mask it
           tax
           dex     ; decrement it
           txa
           asl     ; double it
           tax
           lda serial_config+1
           and #%00011111
           ora parity_settings,X
           sta serial_config+1
           jsr delay_onethirdsec

           jsr output_clear_and_cursor_down_to_screen
           ldy #<presets_stopbits
           lda #>presets_stopbits
           jsr output_string_to_screen
@stopbits_q:
           jsr GETIN
           beq @stopbits_q
           cmp #$31
           bcc @stopbits_q
           cmp #$33
           bcs @stopbits_q
           pha
           jsr output_char_to_screen
           pla
           and #7  ; mask it
           tax
           dex     ; decrement it
           txa
           asl     ; double it
           tax
           lda serial_config
           and #%01111111
           ora stopbit_settings,X
           sta serial_config
           jsr delay_onethirdsec
           jsr delay_onethirdsec

           jsr output_clear_and_cursor_down_to_screen
           ldy #<presets_wordsize
           lda #>presets_wordsize
           jsr output_string_to_screen
@wordsize_q:
           jsr GETIN
           beq @wordsize_q
           cmp #$31
           bcc @wordsize_q
           cmp #$33
           bcs @wordsize_q
           pha
           jsr output_char_to_screen
           pla
           and #7  ; mask it
           tax
           dex     ; decrement it
           txa
           asl     ; double it
           tax
           lda serial_config
           and #%10011111
           ora wordsize_settings,x
           sta serial_config
           jsr delay_onethirdsec

           jsr output_clear_and_cursor_down_to_screen
           ldy #<presets_casemode
           lda #>presets_casemode
           jsr output_string_to_screen
@casemode_q:
           jsr GETIN
           beq @casemode_q
           cmp #$31
           bne @casemode_n1
           jsr output_char_to_screen
           lda #0                 ; clear upper only
           sta case_flag
           beq @casemode_1
@casemode_n1:
           cmp #$32
           bne @casemode_q
           jsr output_char_to_screen
           lda #$80               ; set upper only
           sta case_flag
@casemode_1:
           jsr delay_onethirdsec

           jsr output_clear_and_cursor_down_to_screen
           ldy #<presets_tvvideo
           lda #>presets_tvvideo
           jsr output_string_to_screen

@tvvideo_q:
           jsr GETIN
           beq @tvvideo_q
           cmp #$31
           bne @tvvideo_n1
           jsr output_char_to_screen
           lda #0                 ; clear color mode (set b/w)
           sta color_flag
           beq @done
@tvvideo_n1:
           cmp #$32
           bne @tvvideo_q
           jsr output_char_to_screen
           lda #$80
           sta color_flag
@done:
           jsr delay_onethirdsec
           jsr output_clear_and_cursor_down_to_screen
           jsr config_color
           lda #$ff               ; turn on all sprites
           sta VIC_SPR_ENA
           jsr output_cursor_home_and_cursor_down
           rts


presets:
           .byte "***presets***"
           .byte $0d, $0d
           .byte "baud: 300"
           .byte $0d
           .byte "linefeed: off"
           .byte $0d
           .byte "parity: mark"
           .byte $0d
           .byte "stopbits: 1"
           .byte $0d
           .byte "wordsize: 7 bits"
           .byte $0d
           .byte "case mode: upper/lower"
           .byte $0d
           .byte "tv: color"
           .byte $0d, $0d
           .byte "accept presets? "
           .byte 0
presets_baud:
           .byte "***baud***"
           .byte $0d, $0d
           .byte "1.  110"
           .byte $0d
           .byte "2.  150"
           .byte $0d
           .byte "3.  300"
           .byte $0d, $0d
           .byte "selection? "
           .byte 0
presets_linefeed:
           .byte "***linefeed***"
           .byte $0d, $0d
           .byte "1. off"
           .byte $0d
           .byte "2.  on"
           .byte $0d, $0d
           .byte "selection? "
           .byte 0
presets_parity:
           .byte "***parity***"
           .byte $0d, $0d
           .byte "1.   off"
           .byte $0d
           .byte "2.   odd"
           .byte $0d
           .byte "3.  even"
           .byte $0d
           .byte "4.  mark"
           .byte $0d
           .byte "5. space"
           .byte $0d, $0d
           .byte "selection? "
           .byte 0
presets_stopbits:
           .byte "***stopbits***"
           .byte $0d, $0d
           .byte "1. 1"
           .byte $0d
           .byte "2. 2"
           .byte $0d, $0d
           .byte "selection? "
           .byte 0
presets_tvvideo:
           .byte "***tv/video***"
           .byte $0d, $0d
           .byte "1.   b/w"
           .byte $0d
           .byte "2. color"
           .byte $0d, $0d
           .byte "selection? "
           .byte 0
presets_wordsize:
           .byte "***wordsize***"
           .byte $0d, $0d
           .byte "1.    7"
           .byte $0d
           .byte "2.    8"
           .byte $0d, $0d
           .byte "selection? "
           .byte 0
presets_casemode:
           .byte "***case mode***"
           .byte $0d, $0d
           .byte "1.   upper/lower"
           .byte $0d
           .byte "2. caps only"
           .byte $0d, $0d
           .byte "selection? "
           .byte 0
case_flag:
           .byte 0                   ; bit7: 0=upper/lower, 1=upper only

baud_settings_lo:
           .byte <ntsc_baud_110      ; (1) 110 baud lo
           .byte <ntsc_baud_150      ; (2) 150 baud lo
           .byte <ntsc_baud_300      ; (3) 300 baud lo
baud_settings_hi:
           .byte >ntsc_baud_110      ; (1) 110 baud hi
           .byte >ntsc_baud_150      ; (2) 150 baud hi
           .byte >ntsc_baud_300      ; (3) 300 baud hi

           .byte $ff, $1f
           .byte $00

parity_settings:
           .byte $00, $00            ; 000 00000: (1) off
           .byte $20, $00            ; 001 00000: (2) odd
           .byte $60, $00            ; 011 00000; (3) even
           .byte $a0, $00            ; 101 00000; (4) mark  (off 7bit=1)
           .byte $e0, $7f            ; 111 00000; (5) space (off 7bit=0)

           .byte $ff

stopbit_settings:
           .byte $00, $00            ; 0 0000000: (1) 1 stop bit
           .byte $80, $00            ; 1 0000000: (2) 2 stop bits

           .byte $9f, $ff
wordsize_settings:
           .byte $20, $00            ; 0010 0000: (1) 7 bits
           .byte $00, $00            ; 0000 0000: (2) 8 bits


config_color:
           bit color_flag
           bmi set_blue
           lda #0
           beq set_colors
set_blue:  lda #6
set_colors:
           sta VIC_BORDERCOLOR
           sta VIC_BG_COLOR0
           lda #1                 ; white
           sta COLOR
           lda #$f                ; gray
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


indicate_fmt_flag:
           ldx #6
           ldy #2
           bit fmt_flag
           bpl @set_indication
@clear_indication:
           lda sprite_data_base_addr+offset_fmt*64,y
           and #%11000000
           sta sprite_data_base_addr+offset_fmt*64,y
           iny
           iny
           iny
           dex
           bpl @clear_indication
           rts
@set_indication:
           lda sprite_data_base_addr+offset_fmt*64,y
           ora #%00111100
           sta sprite_data_base_addr+offset_fmt*64,y
           iny
           iny
           iny
           dex
           bpl @set_indication
           rts


open_modem_device:
           lda #modem_file_no
           jsr CLOSE
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
           jsr CLRCHN
           rts


open_printer_device:
           lda #0                 ; filename length
           jsr SETNAM
           lda #printer_file_no
           ldx #printer_device_no
           ldy #7                 ; secondary address
           jsr SETLFS
           jsr OPEN
           rts


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
           jsr indicate_rcv_flag
           rts


output_clear_and_cursor_down_to_screen:
           lda #clear_screen
           jsr CHROUT

output_cursor_home_and_cursor_down:
           lda #cursor_home
           jsr CHROUT
           lda #cursor_down
           jsr CHROUT
           jmp fine_scroll_one_line_if_needed

to_rcv_buffer:
           ldy #0
           sta accumlator_save
           cmp #BS
           beq remove_from_rcv_buffer
           cmp #DEL
           bne add_to_rcv_buffer


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
           lda accumlator_save
           rts


add_to_rcv_buffer:
           bit rcv_buffer_full_flag
           bmi @done
           sty rcv_notadded_flag  ; y expected to be zero, so indicate 'added'
           sta (rcv_buffer_ptr),y
           inc rcv_buffer_ptr_lo
           bne @done
           inc rcv_buffer_ptr_hi
           lda rcv_buffer_ptr_hi
           cmp MEMSIZ1+1
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
           lda accumlator_save
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
           lda #modem_file_no
           jsr CLOSE
           jsr open_modem_device
           jmp main


sprite_data_dpx:
                               ; ------------------------
           .byte $e3, $c8, $80 ; 111   1111  1   1
           .byte $92, $28, $80 ; 1  1  1   1 1   1
           .byte $8a, $25, $00 ; 1   1 1   1  1 1
           .byte $8b, $e2, $00 ; 1   1 11111   1
           .byte $8a, $05, $00 ; 1   1 1      1 1
           .byte $92, $08, $80 ; 1  1  1     1   1
           .byte $e2, $08, $80 ; 111   1     1   1
sprite_data_fmt:
                               ; ------------------------
           .byte $fa, $2f, $80 ; 11111 1   1 11111
           .byte $83, $62, $00 ; 1     11 11   1
           .byte $82, $a2, $00 ; 1     1 1 1   1
           .byte $e2, $22, $00 ; 111   1   1   1
           .byte $82, $22, $00 ; 1     1   1   1
           .byte $82, $22, $00 ; 1     1   1   1
           .byte $82, $22, $00 ; 1     1   1   1
sprite_data_rcv:
                               ; ------------------------
           .byte $f1, $c8, $80 ; 1111   111  1   1
           .byte $8a, $28, $80 ; 1   1 1   1 1   1
           .byte $8a, $08, $80 ; 1   1 1     1   1
           .byte $f2, $05, $00 ; 1111  1      1 1
           .byte $a2, $05, $00 ; 1 1   1      1 1
           .byte $92, $22, $00 ; 1  1  1   1   1
           .byte $89, $c2, $00 ; 1   1  111    1
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


init_sprites:
           ldx #$ff               ; set all sprites to double-width
           stx VIC_SPR_EXP_X
           lda #0                 ; set all sprites to single-height, single color
@loop1:
           sta sprite_data_base_addr,x
           sta sprite_data_base_addr+$100,x
           dex
           bne @loop1

           sta VIC_SPR_EXP_Y
           sta VIC_SPR_MCOLOR
           ldx #20
@loop2:
           lda sprite_data_rcv,x
           sta sprite_data_base_addr+offset_rcv*64,x
           lda sprite_data_dpx,x
           sta sprite_data_base_addr+offset_dpx*64,x
           lda sprite_data_fmt,x
           sta sprite_data_base_addr+offset_fmt*64,x
           lda sprite_data_64t,x
           sta sprite_data_base_addr+offset_64t*64,x
           lda sprite_data_ermi,x
           sta sprite_data_base_addr+offset_ermi*64,x
           lda sprite_data_nal,x
           sta sprite_data_base_addr+offset_nal*64,x
           dex
           bpl @loop2
           lda #54
           sta VIC_SPR0_Y
           sta VIC_SPR1_Y
           sta VIC_SPR2_Y
           sta VIC_SPR3_Y
           sta VIC_SPR4_Y
           sta VIC_SPR5_Y
           lda #4
           sta VIC_SPR_HI_X
           lda #184
           sta VIC_SPR0_X
           lda #240
           sta VIC_SPR1_X
           lda #40
           sta VIC_SPR2_X
           lda #25
           sta VIC_SPR3_X
           lda #74
           sta VIC_SPR4_X
           lda #123
           sta VIC_SPR5_X
           ldx #<(sprite_data_base_addr/64)  ; set sprite #0 ptr
           stx lo_screen_addr+sprite_data_ptr_offset
           stx hi_screen_addr+sprite_data_ptr_offset
           inx                               ; set sprite #1 ptr
           stx lo_screen_addr+sprite_data_ptr_offset+1
           stx hi_screen_addr+sprite_data_ptr_offset+1
           inx                               ; set sprite #2 ptr
           stx lo_screen_addr+sprite_data_ptr_offset+2
           stx hi_screen_addr+sprite_data_ptr_offset+2
           inx                               ; set sprite #3 ptr
           stx lo_screen_addr+sprite_data_ptr_offset+3
           stx hi_screen_addr+sprite_data_ptr_offset+3
           inx                               ; set sprite #4 ptr
           stx lo_screen_addr+sprite_data_ptr_offset+4
           stx hi_screen_addr+sprite_data_ptr_offset+4
           inx                               ; set sprite #5 ptr
           stx lo_screen_addr+sprite_data_ptr_offset+5
           stx hi_screen_addr+sprite_data_ptr_offset+5
           rts


nmihandler:
           pha
           txa
           pha
           tya
           pha
           lda #%01111111         ; disable all NMI sources
           sta CIA2_ICR
           ldy CIA2_ICR
           bpl @skip_rs232        ; NMI has not been generated
           jmp $fe72              ; RS232 NMI routine
@skip_rs232:
           lda #$80
           sta restore_key_flag
           jmp $febc              ; return from NMI


copy_cart_signature_for_nono_routine:
           ldy #8
@loop:
           lda @cart_signature,Y
           sta $8000,Y
           dey
           bpl @loop
           rts

@cart_signature:
           .word that_is_a_nono
           .word that_is_a_nono
           .byte $c3, $c2, $cd, $38, $30     ; CBM80 in PETSCII


that_is_a_nono:
           jsr CLRCHN
           ldy #0
@loop:     lda @nono_msg,Y
           beq @spin_forever
           jsr CHROUT
           iny
           bne @loop
@spin_forever:
           beq @spin_forever

@nono_msg:
           .byte " that is a no-no !!"
           .byte $0d, 0
