           processor 6502
;
; '64 Terminal - Next Generation
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
; Original by Dr. Jim Rothwell, Midwest Micro Inc., circa 1983
;
;
; Keys/functions:
;
; - F1/F2   RCV: Toggle receive buffering (off/on)
; - F3/F4   DPX: Toggle half duplex (off/on)
; - F6      Resets to the Accept Presets page, clears receive buffer
; - F7      Review feature, copies receive buffer to screen. SPACE pauses, F7 again quits
; - F8      Dumps receive buffer to printer, RUNSTOP quits printing
;
;
; Reverse engineering and Next Generation improvements
; by Mike Murphy (mike@emu7800.net), circa 2023.
; Intended for preservation and educational purposes only.
;
; Build using dasm, the 8-bit macro assembler: https://dasm-assembler.github.io/:
; > dasm.exe .\64-tng.asm -f1 -v4 '-o64-tng.prg'
;

ntsc_clock_freq         = 1022727
pal_clock_freq          = 985248

ntsc_baud_2400          = ntsc_clock_freq/2400/2 - 100
ntsc_baud_1200          = ntsc_clock_freq/1200/2 - 100
ntsc_baud_300           = ntsc_clock_freq/ 300/2 - 100

pal_baud_2400           = pal_clock_freq/2400/2 - 100
pal_baud_1200           = pal_clock_freq/1200/2 - 100
pal_baud_300            = pal_clock_freq/ 300/2 - 100


stop_char               = 3     ; PETSCII stop
BS                      = 8     ; ASCII backspace
CR                      = 13    ; PETSCII/ASCII carriage return
toggle_charset          = 14    ; PETSCII toggle character se
cursor_down             = 17    ; PETSCII cursor down
cursor_home             = 19    ; PETSCII cursor home
delete                  = 20    ; PETSCII delete $14
quote                   = 34    ; PETSCII/ASCII quote
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
cursor_up               = 145   ; PETSCII cursor up
clear_screen            = 147   ; PETSCII clear screen
insert                  = 148   ; PETSCII insert
cursor_left             = 157   ; PETSCII cursor left
underscore              = 164   ; PETSCII underscore

accumlator_save         = $03
dpx_flag                = $1f   ; bit7: 0=full, 1=half
ntsc_flag               = $21   ; bit7: 0=b/w, 1=color
FREETOP                 = $33   ; Pointer to the Bottom of the String Text Storage Area
FRESPC                  = $35   ; Temporary Pointer for Strings
MEMSIZ1                 = $37   ; Pointer to the Highest Address Used by BASIC
last_keycode            = $5d
show_cursor_flag        = $5e   ; bit7: 0=dont show cursor, 1=show cursor
serial_config           = $5f   ; Four (4) byte string for serial port configuration
screen_select_flag      = $63   ; bit7: 0=low screen, 1=high screen
rcv_flag                = $69   ; bit7: 0=off, 1=on
rcv_buffer_ptr          = $6a
charset_flag            = $70   ; bit7: 0=ascii, 1=petscii
rcv_buffer_ptr_lo       = rcv_buffer_ptr
rcv_buffer_ptr_hi       = rcv_buffer_ptr+1
ptr                     = $6c
ptr_lo                  = ptr
ptr_hi                  = ptr+1
rcv_buffer_full_flag    = $6e   ; bit7: 0=not full, 1=full
rcv_notadded_flag       = $6f   ; bit7: added to buffer since rcv on: 0=byte added, 1=byte not added
NDX                     = $c6   ; Number of Characters in Keyboard Buffer (Queue)
SFDX                    = $cb   ; Matrix Coordinate of Current Key Pressed
PNTR                    = $d3   ; Cursor column on current logical line (0-79)
TBLX                    = $d6   ; Current cursor physical line number (0-24)
MEMSIZ2                 = $0283 ; Pointer: O.S. End of Memory
COLOR                   = $0286 ; Current Foreground Color for Text (values 0-f)
HIBASE                  = $0288 ; Top page of screen memory
SHFLAG                  = $028d ; Flag: SHIFT/CTRL/LOGO Keypress bit0=SHIFT, bit1=LOGO, bit2=CTRL
NMINV                   = $0318 ; Vector: Non-Maskable Interrupt


sprite_data_base_addr   = $2000
offset_rcv  = 0  ; RCV sprite
offset_dpx  = 1  ; DPX sprite
offset_64t  = 2  ; 64T sprite
offset_ermi = 3  ; ERMI sprite
offset_nal  = 4  ; NAL sprite


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

; From Mapping the Commodore 64, Sheldon Leemon
; https://github.com/Project-64/reloaded/blob/master/c64/mapc64/MAPC6412.TXT

SP0X     = $d000 ; Sprite 0 Horizontal Position
SP0Y     = $d001 ; Sprite 0 Vertical Position
SP1X     = $d002 ; Sprite 1 Horizontal Position
SP1Y     = $d003 ; Sprite 1 Vertical Position
SP2X     = $d004 ; Sprite 2 Horizontal Position
SP2Y     = $d005 ; Sprite 2 Vertical Position
SP3X     = $d006 ; Sprite 3 Horizontal Position
SP3Y     = $d007 ; Sprite 3 Vertical Position
SP4X     = $d008 ; Sprite 4 Horizontal Position
SP4Y     = $d009 ; Sprite 4 Vertical Position
SP5X     = $d00a ; Sprite 5 Horizontal Position
SP5Y     = $d00b ; Sprite 5 Vertical Position
MSIGX    = $d010 ; Most Significant Bits of Sprites 0-7 Horizontal Position
SCROLY   = $d011 ; Vertical fine scrolling and control register
                 ; bits0-2: Fine scroll display vertically by X scan lines (0-7)
                 ; bit3:    Select a 24-row or 25-row text display (1=25 rows, 0=24 rows)
                 ; bit4:    Blank the entire screen to the same color as the background (0=blank)
                 ; bit5:    Enable bitmap graphics mode (1=enable)
                 ; bit6:    Enable extended color text mode (1=enable)
                 ; bit7:    High bit (Bit 8) of raster compare register at 53266 ($D012)

SPENA    = $d015 ; Sprite enable register
YXPAND   = $d017 ; Sprite Vertical Expansion Register
VMCSB    = $d018 ; VIC-II Chip Memory Control Register
XXPAND   = $d01d ; Sprite Horizontal Expansion Register
SPMC     = $d01c ; Sprite Multicolor Registers
EXTCOL   = $d020 ; Border Color Register
BGCOL0   = $d021 ; Background Color
SP0COL   = $d027 ; Sprite Color Register 0
SP1COL   = $d028 ; Sprite Color Register 1
SP2COL   = $d029 ; Sprite Color Register 2
SP3COL   = $d02a ; Sprite Color Register 3
SP4COL   = $d02b ; Sprite Color Register 4
SP5COL   = $d02c ; Sprite Color Register 5
SP6COL   = $d02d ; Sprite Color Register 6
SP7COL   = $d02e ; Sprite Color Register 7

; $dd00-$dd0F CIA #2 Registers
CI2PRA   = $dd00 ; Data Port Register A
CI2ICR   = $dd0d ; Interrupt Control Register

; C64 kernal addresses
USKINDIC = $f6bc ; Update Stop key indicator, at memory address $0091, A and X registers used
UNLSTN   = $ffae ; Comand serial bus device to UNLISTEN
READST   = $ffb7 ; Read I/O status word
SETLFS   = $ffba ; Set logical file parameters
SETNAM   = $ffbd ; Set filename parameters
OPEN     = $ffc0 ; Open a logical file
CLOSE    = $ffc3 ; Close a logical file
CHKIN    = $ffc6 ; Define an input channel
CHKOUT   = $ffc9 ; Define an output channel
CLRCHN   = $ffcc ; Restore input and output device to defaults (keyboard, screen)
CHROUT   = $ffd2 ; Output a byte from the accumulator
STOP     = $ffe1 ; Check the STOP key
GETIN    = $ffe4 ; Get one byte from the input device into the accumulator
CLALL    = $ffe7 ; Close all logical I/O files

           org $0801

           ; 0 SYS2304:REM  * * 64 TERMINAL NG * *
           dc.w endofbootstrapper
           hex 00 00
           hex 9e 32 33 30 34 3a 8f
           dc "  * * 64 TERMINAL NG * *"
           hex 00
endofbootstrapper:
           hex 00 00

           ds.b start-*, 0

           org $0900
start:
           subroutine
           jsr clear_both_screens
           lda #toggle_charset
           jsr CHROUT
           lda #>sprite_data_base_addr
           sta MEMSIZ2+1
           sta FRESPC+1
           sta FREETOP+1
           lda #>endof_basic_addr
           sta MEMSIZ1+1
           lda #%00010111          ; scroll 7, 24 rows
           sta SCROLY
           lda #0                  ; black
           sta EXTCOL
           sta BGCOL0
           lda #1                  ; white
           sta COLOR

           lda #$80                ; set to ntsc mode
           sta ntsc_flag

           jsr output_cursor_home_and_cursor_down
           jsr output_space_and_cursor_left_to_screen
           lda #0                  ; dont show cursor, set low screen
           sta show_cursor_flag
           sta screen_select_flag
           jsr print_title_message_to_screen
reset:
           subroutine
           lda #1                  ; white
           sta COLOR
           jsr init_sprites
           lda #$80                ; show cursor
           sta show_cursor_flag
           jsr accept_presets_menu
           jsr init_rcv_buffer_ptr
           jsr CLALL
           lda #$80
           sta rcv_notadded_flag
           jsr open_printer_device
           jsr open_modem_device
main:
           subroutine
           jsr CLRCHN
           jsr GETIN
           beq main
           cmp #stop_char
           beq main
           ; Accept 0-128,130-141,147,148,160-255
           cmp #$80
           bcc .continue_handle_char_from_kbd
           cmp #$81
           beq .done
           cmp #$8e
           bcc .continue_handle_char_from_kbd
           cmp #clear_screen
           beq .continue_handle_char_from_kbd
           cmp #insert
           beq .continue_handle_char_from_kbd
           cmp #$a0
           bcs .continue_handle_char_from_kbd
.done:
           jmp main

.continue_handle_char_from_kbd:
           cmp #F1                ; turn off RCV
           bne .next2
           bit rcv_flag
           bmi .test_rcv_notadded_flag
           jmp main
.test_rcv_notadded_flag:
           bit rcv_notadded_flag
           bmi .skip_cr
           lda #CR
           jsr output_char_to_screen
.skip_cr:
           lda #0                 ; clear
           sta rcv_flag
           jsr indicate_rcv_flag
           jmp main
.next2:
           cmp #F2                ; turn on RCV
           bne .next3
           bit rcv_buffer_full_flag
           bpl .rcv_buffer_not_full
           jmp main               ; dont bother since buffer full
.rcv_buffer_not_full:
           bit rcv_flag
           bpl .set_rcv_flag
           jmp main
.set_rcv_flag:
           lda #CR
           jsr output_char_to_screen
           lda #$80
           sta rcv_flag
           sta rcv_notadded_flag
           jsr indicate_rcv_flag
           jmp main
.next3:
           cmp #F3                ; turn on DPX
           bne .next4
           lda #0                 ; full
           sta dpx_flag
           jsr indicate_dpx_config
           jmp main
.next4:
           cmp #F4                ; turn off DPX
           bne .next6
           lda #$80               ; half
           sta dpx_flag
           jsr indicate_dpx_config
           jmp main
.next6:    cmp #F6                ; reset
           bne .next7
           jmp reset
.next7:
           cmp #F7
           bne .next8
           jmp dump_rcv_buffer_to_screen
.next8:
           cmp #F8
           bne .next9
           jmp dump_rcv_buffer_to_printer
.next9:
           cmp #insert
           bne .next10
           lda #DEL
           jmp output_char_to_modem_with_dpx_echo
.next10:
           cmp #$ba               ; unknown
           bne .next11
           lda #$60               ; PETSCII horizontal line
           jmp output_char_to_modem_with_dpx_echo
.next11:
           cmp #clear_screen
           bne .next12
           jsr output_clear_and_cursor_down_to_screen
           jmp main
.next12:
           cmp #shift_return
           bne .next13
           and #$7f
.next13:
           cmp #CR
           beq output_char_to_modem_with_dpx_echo
           cmp #delete
           bne .next14
           lda #BS
           jmp output_char_to_modem_with_dpx_echo
.next14:
           cmp #" "
           bcs output_char_to_modem_with_dpx_echo
           jmp main


output_char_to_modem_with_dpx_echo:
           subroutine
           pha

           bit dpx_flag
           bpl output_char_to_modem
           jsr output_char_to_screen


           ; Expects character to output to be on top of the stack
output_char_to_modem:
           subroutine
           jsr CLRCHN
           ldx #modem_file_no
           jsr CHKOUT
           pla
           cmp #BS
           beq .done
           cmp #DEL
           beq .done
           bit charset_flag
           bmi .done
.convert_to_ascii:
           and #$7f
           cmp #"A"
           bcc .done
           cmp #"Z"+1
           bcs .done
           adc #32
.done:
           jsr CHROUT


get_char_from_modem:
           subroutine
           jsr CLRCHN
           ldx #modem_file_no
           jsr CHKIN
           jsr GETIN
           pha
           jsr CLRCHN
           pla
           bne .handle_char
           jmp main
.handle_char:
           cmp #CR
           beq .output_cr_then_done
           bit charset_flag
           bmi .handle_petscii_char
.handle_ascii_char:
           and #$7f
           cmp #BS
           beq .output_byte_to_screen_then_done
           cmp #DEL
           beq .output_byte_to_screen_then_done
           bne .handle_char2
.handle_petscii_char:
           cmp #delete
           beq .output_byte_to_screen_then_done
.handle_char2:
           cmp #" "
           bcs .handle_printable_char
           jmp main
.output_cr_then_done:
           lda #CR
           jmp .output_byte_to_screen_then_done
.handle_printable_char:
           bit charset_flag
           bmi .output_byte_to_screen_then_done
           cmp #"a"
           bcc .next
           sbc #32
           bne .output_byte_to_screen_then_done
.next:
           cmp #"A"
           bcc .output_byte_to_screen_then_done
           cmp #"Z"+1
           bcs .output_byte_to_screen_then_done
           adc #32
.output_byte_to_screen_then_done:
           jsr output_char_to_screen
           jmp main


dump_rcv_buffer_to_printer:
           subroutine
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
           bpl .status_ok
           ldy #<.err_printer_offline_message
           lda #>.err_printer_offline_message
           jsr output_string_to_screen
           jmp .done

.status_ok:
           lda #%00000111         ;  scroll 7, 25 rows
           sta SCROLY
           lda #<startof_rcv_buffer_addr
           sta ptr_lo
           lda #>startof_rcv_buffer_addr
           sta ptr_hi
           lda #CR
           jsr CHROUT

.get_nextbyte:
           ldy #0
           lda (ptr),y
           beq .clear_rcv_buffer_then_done
           jsr CHROUT
           jsr USKINDIC
           jsr STOP
           beq .clear_rcv_buffer_then_done ; runstop pressed
           inc ptr_lo
           bne .get_nextbyte
           inc ptr_hi
           bne .get_nextbyte

.clear_rcv_buffer_then_done:
           lda #CR
           jsr CHROUT
           jsr init_rcv_buffer_ptr

.done:
           jsr UNLSTN
           jsr CLRCHN
           lda #CR
           jsr output_char_to_screen
           lda #modem_file_no
           jsr CLOSE
           jsr open_modem_device
           lda #%00010111         ; scroll 7, 24 rows
           sta SCROLY
           jmp main

.err_printer_offline_message:
           hex 0d
           dc "* * * error: pRINTER oFF-lINE * * *"
           hex 0d 00

clear_both_screens:
           subroutine
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
           sta VMCSB
           lda #%00010111         ; scroll 7, 24 rows
           sta SCROLY
           lda #CR
           jsr CHROUT
           lda #0                 ; disable all sprites
           sta SPENA
           rts


delay_256x160:
           subroutine
           ldx #160
.loop_outer:
           ldy #0
.loop_inner:
           dey
           bne .loop_inner
           dex
           bne .loop_outer
           rts


output_char_to_screen:
           subroutine
           bit rcv_flag
           bpl .next1
           jsr to_rcv_buffer
.next1:
           cmp #CR
           bne .next2
           jsr output_space_and_cursor_left_to_screen
           lda #CR
           bne .is_not_quote
.next2:
           cmp #BS
           beq .is_backspace
           cmp #DEL
           bne .is_not_backspace
.is_backspace:
           jsr output_backspace_to_screen
           jmp fine_scroll_one_line_if_needed

.is_not_backspace:
           cmp #quote
           bne .is_not_quote
           jsr CHROUT
           lda #quote
           jsr CHROUT
           lda #cursor_left
           jsr CHROUT
           jmp fine_scroll_one_line_if_needed

.is_not_quote:
           jsr CHROUT


fine_scroll_one_line_if_needed:
           subroutine
           bit show_cursor_flag
           bpl .next
           lda #underscore
           jsr CHROUT
           lda #cursor_left
           jsr CHROUT
.next:
           lda TBLX
           cmp #23
           bcs fine_scroll_one_line
           rts


fine_scroll_one_line:
           subroutine
           stx .save_x
           sty .save_y

           ; copy to opposite screen one line up

           ldy #39
           lda #" "
           bit screen_select_flag
           bmi .loop1_lo

           ; on low screen
.loop1_hi:
           sta hi_screen_addr+23*40,y
           dey
           bpl .loop1_hi

screen_copy_offset = 22*40 - 3*$100

           ldy #0
.loop2_hi:
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
           bne .loop2_hi

           ldy #screen_copy_offset
.loop3_hi:
           lda lo_screen_addr+2*40,y
           sta hi_screen_addr+1*40,y
           dey
           bpl .loop3_hi
           bmi .do_fine_scroll

           ; on high screen
.loop1_lo:
           sta lo_screen_addr+23*40,y
           dey
           bpl .loop1_lo

           ldy #0

.loop2_lo:
           lda hi_screen_addr+2*40+screen_copy_offset+2*$100,y
           sta lo_screen_addr+1*40+screen_copy_offset+2*$100,y
           lda hi_screen_addr+2*40+screen_copy_offset+$100,y
           sta lo_screen_addr+1*40+screen_copy_offset+$100,y
           lda hi_screen_addr+2*40+screen_copy_offset,y
           sta lo_screen_addr+1*40+screen_copy_offset,y
           iny
           bne .loop2_lo

           ldy #screen_copy_offset
.loop3_lo:
           lda hi_screen_addr+2*40,y
           sta lo_screen_addr+1*40,y
           dey
           bpl .loop3_lo

.do_fine_scroll:
           ldy #7
.loop1:
           bit SCROLY
           bpl .loop1             ; wait until raster is off visible screen

           dec SCROLY             ; fine scroll one line

           ldx #200
.delay:
           dex
           bne .delay

           dey
           bne .loop1

.loop2:
           bit SCROLY
           bpl .loop2             ; wait until raster is off visible screen

           ; swap screens to implement carriage return

           bit screen_select_flag
           bmi .swap_to_lo_screen

.swap_to_hi_screen:
           lda #(hi_screen_nybble << 4 | 6) ; 1010 0110  video matrix: 10*1k=$2800
           sta VMCSB
           sta screen_select_flag
           lda #>hi_screen_addr
           sta HIBASE
           bne .done
.swap_to_lo_screen:
           lda #(lo_screen_nybble << 4 | 6) ; 0001 0110  video matrix: 1*1k=$400
           sta VMCSB
           lda #>lo_screen_addr
           sta HIBASE
           sta screen_select_flag
.done:
           lda #%00010111         ; scroll 7, 24 rows
           sta SCROLY
           ldy .save_y
           ldx .save_x
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
.save_y:   dc 0
.save_x:   dc 0

output_space_and_cursor_left_to_screen:
           subroutine
           lda #" "
           jsr CHROUT
           lda #cursor_left
           jsr CHROUT
           rts


output_backspace_to_screen:
           subroutine
           lda #0
           sta .move_cursor_up_oneline_flag
           lda PNTR
           bne .cursor_not_at_startof_line
           lda #$80
           sta .move_cursor_up_oneline_flag
           lda TBLX
           cmp #3
           bcc .done
.cursor_not_at_startof_line:
           jsr output_space_and_cursor_left_to_screen
           lda #cursor_left
           jsr CHROUT
           bit .move_cursor_up_oneline_flag
           bpl .done
           dec TBLX               ; move current cursor up one physical line
.done:
           rts
.move_cursor_up_oneline_flag:
           dc 0

output_string_to_screen:
           subroutine
           sty ptr_lo
           sta ptr_hi
           ldy #0
.next:
           lda (ptr),y
           beq .done              ; strings are zero-terminated
           jsr output_char_to_screen
           iny
           bne .next
           inc ptr_hi
           bne .next
.done:
           rts


delay_onethirdsec:
           subroutine
           ldx #0
.loop1:
           ldy #0
.loop2:
           dey
           bne .loop2
           dex
           bne .loop1
           rts


print_title_message_to_screen:
           subroutine
           lda #0
           sta rcv_flag
           ldx #20
           stx TBLX

           ldy #<.title_message
           lda #>.title_message
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
           jsr delay_onethirdsec
           rts

.title_message:
           hex 91
              ;----------------------------------------
           dc "             64 terminal ng"
           hex 0d 0d 0d 0d
           dc "      oRIGINAL BY dR. jIM rOTHWELL"
           hex 0d 0d
           dc "                C. 1983"
           hex 0d 0d 0d 0d 0d
           dc "       ng VERSION BY mIKE mURPHY"
           hex 0d 0d
           dc "            MIKE@EMU7800.NET"
           hex 0d 0d
           dc "                C. 2023"
           hex 0d 0d 0d 0d 0d 0d
           hex 00


accept_presets_menu:
           subroutine
           lda #0
           sta NDX
           sta rcv_flag
           sta charset_flag
           jsr clear_both_screens
           jsr CLRCHN
           jsr output_clear_and_cursor_down_to_screen
           lda #0
           sta dpx_flag
           lda #$80
           sta ntsc_flag

           lda #0                 ; user specified baud, 8 bits, 1 stop bit
           sta serial_config
           lda #0                 ; 3 lines, full duplex, mark parity
           sta serial_config+1
           lda #<ntsc_baud_1200
           sta serial_config+2
           lda #>ntsc_baud_1200
           sta serial_config+3

.presets_start:
           ldy #<.presets
           lda #>.presets
           jsr output_string_to_screen
.accept_q:
           jsr GETIN
           cmp #CR
           beq .accept_y
           cmp #"Y"
           beq .accept_y
           cmp #"N"
           beq .tv_start
           bne .accept_q
.accept_y:
           lda #"Y"
           jsr output_char_to_screen
           jmp .done
.tv_start:
           jsr delay_onethirdsec
           jsr output_clear_and_cursor_down_to_screen
           ldy #<.presets_tv
           lda #>.presets_tv
           jsr output_string_to_screen
.tv_q:
           jsr GETIN
           beq .tv_q
           cmp #"1"
           bne .tv_n1
           jsr output_char_to_screen
           lda #$80
           sta ntsc_flag
           bne .baud_start
.tv_n1:
           cmp #"2"
           bne .tv_q
           jsr output_char_to_screen
           lda #0
           sta ntsc_flag
.baud_start
           jsr delay_onethirdsec
           jsr output_clear_and_cursor_down_to_screen
           ldy #<.presets_baud
           lda #>.presets_baud
           jsr output_string_to_screen
.baud_q:
           jsr GETIN
           beq .baud_q
           cmp #"1"
           bcc .baud_q
           cmp #"4"
           bcs .baud_q
           pha
           jsr output_char_to_screen
           pla
           and #7
           tax
           dex
           bit ntsc_flag
           bmi .baud_ntsc
.baud_pal:
           lda .pal_baud_settings_lo,x
           sta serial_config+2
           lda .pal_baud_settings_hi,x
           sta serial_config+3
           jmp .wordsize_start
.baud_ntsc:
           lda .ntsc_baud_settings_lo,x
           sta serial_config+2
           lda .ntsc_baud_settings_hi,x
           sta serial_config+3

.wordsize_start:
           jsr delay_onethirdsec
           jsr output_clear_and_cursor_down_to_screen
           ldy #<.presets_wordsize
           lda #>.presets_wordsize
           jsr output_string_to_screen
.wordsize_q:
           jsr GETIN
           beq .wordsize_q
           cmp #"1"
           bcc .wordsize_q
           cmp #"3"
           bcs .wordsize_q
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
           ora .wordsize_settings,x
           sta serial_config
.parity_start:
           jsr delay_onethirdsec
           jsr output_clear_and_cursor_down_to_screen
           ldy #<.presets_parity
           lda #>.presets_parity
           jsr output_string_to_screen
.parity_q:
           jsr GETIN
           beq .parity_q
           cmp #"1"
           bcc .parity_q
           cmp #"6"
           bcs .parity_q
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
           ora .parity_settings,X
           sta serial_config+1
.stopbits_start:
           jsr delay_onethirdsec
           jsr output_clear_and_cursor_down_to_screen
           ldy #<.presets_stopbits
           lda #>.presets_stopbits
           jsr output_string_to_screen
.stopbits_q:
           jsr GETIN
           beq .stopbits_q
           cmp #"1"
           bcc .stopbits_q
           cmp #"3"
           bcs .stopbits_q
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
           ora .stopbit_settings,X
           sta serial_config
.charset_start: ; TODO: need to add PESCII support
           jsr delay_onethirdsec
           jsr output_clear_and_cursor_down_to_screen
           ldy #<.presets_charset
           lda #>.presets_charset
           jsr output_string_to_screen
.charset_q:
           jsr GETIN
           beq .charset_q
           cmp #"1"
           bne .charset_n1
           jsr output_char_to_screen
           lda #0
           sta charset_flag
           beq .done
.charset_n1:
           cmp #"2"
           bne .charset_q
           jsr output_char_to_screen
           lda #$80
           sta charset_flag
.done:
           jsr delay_onethirdsec
           jsr output_clear_and_cursor_down_to_screen
           jsr config_colors
           lda #$ff               ; turn on all sprites
           sta SPENA
           jsr output_cursor_home_and_cursor_down
           ldy #<.help_text
           lda #>.help_text
           jsr output_string_to_screen
           rts
.presets:
           dc "***PRESETS***"
           hex 0d 0d
           dc "TV: NTSC"
           hex 0d
           dc "BAUD: 1200"
           hex 0d
           dc "WORDSIZE: 8 BITS"
           hex 0d
           dc "PARITY: NONE"
           hex 0d
           dc "STOPBITS: 1"
           hex 0d
           dc "CHARSET: ASCII"
           hex 0d 0d
           dc "ACCEPT PRESETS? "
           dc 0
.presets_tv:
           dc "***TV***"
           hex 0d 0d
           dc "1. NTSC"
           hex 0d
           dc "2. PAL"
           hex 0d 0d
           dc "SELECTION? "
           dc 0
.presets_baud:
           dc "***BAUD***"
           hex 0d 0d
           dc "1. 2400"
           hex 0d
           dc "2. 1200"
           hex 0d
           dc "3.  300"
           hex 0d 0d
           dc "SELECTION? "
           dc 0
.presets_wordsize:
           dc "***WORDSIZE***"
           hex 0d 0d
           dc "1.    8"
           hex 0d
           dc "2.    7"
           hex 0d 0d
           dc "SELECTION? "
           dc 0
.presets_parity:
           dc "***PARITY***"
           hex 0d 0d
           dc "1.  NONE"
           hex 0d
           dc "2.   ODD"
           hex 0d
           dc "3.  EVEN"
           hex 0d
           dc "4.  MARK"
           hex 0d
           dc "5. SPACE"
           hex 0d 0d
           dc "SELECTION? "
           dc 0
.presets_stopbits:
           dc "***STOPBITS***"
           hex 0d 0d
           dc "1. 1"
           hex 0d
           dc "2. 2"
           hex 0d 0d
           dc "SELECTION? "
           dc 0
.presets_charset:
           dc "***CHARSET***"
           hex 0d 0d
           dc "1. ASCII"
           hex 0d
           dc "2. PETSCII"
           hex 0d 0d
           dc "SELECTION? "
           dc 0
.help_text:
           hex 0d
           dc "f1/f2  rEcEIvE BUFFER TOGGLE          "
           hex 0d
           dc "f3/f4  hALF dUpLEx TOGGLE             "
           hex 0d
           dc "f6     rESET PROGRAM     "
           hex 0d
           dc "f7     rEVIEW rcv BUFFER, SPACE PAUSES"
           hex 0d
           dc "         f7 AGAIN STOPS REVIEW        "
           hex 0d
           dc "f8     dUMP rcv BUFFER TO PRINTER,    "
           hex 0d
           dc "         runstop STOPS PRINTING       "
           hex 0d 0d
           hex 00


.ntsc_baud_settings_lo:
           dc <ntsc_baud_2400     ; (1) 2400 baud lo
           dc <ntsc_baud_1200     ; (2) 1200 baud lo
           dc <ntsc_baud_300      ; (3)  300 baud lo
.ntsc_baud_settings_hi:
           dc >ntsc_baud_2400     ; (1) 2400 baud hi
           dc >ntsc_baud_1200     ; (2) 1200 baud hi
           dc >ntsc_baud_300      ; (3)  300 baud hi

.pal_baud_settings_lo:
           dc <pal_baud_2400      ; (1) 2400 baud lo
           dc <pal_baud_1200      ; (2) 1200 baud lo
           dc <pal_baud_300       ; (3)  300 baud lo
.pal_baud_settings_hi:
           dc >pal_baud_2400      ; (1) 2400 baud hi
           dc >pal_baud_1200      ; (2) 1200 baud hi
           dc >pal_baud_300       ; (3)  300 baud hi

.parity_settings:
           hex 00 00              ; 000 00000: (1) none
           hex 20 00              ; 001 00000: (2) odd
           hex 60 00              ; 011 00000; (3) even
           hex a0 00              ; 101 00000; (4) mark  (off 7bit=1)
           hex e0 7f              ; 111 00000; (5) space (off 7bit=0)

.stopbit_settings:
           hex 00 00              ; 0 0000000: (1) 1 stop bit
           hex 80 00              ; 1 0000000: (2) 2 stop bits

           hex 9f ff
.wordsize_settings:
           hex 00 00              ; 0000 0000: (2) 8 bits
           hex 20 00              ; 0010 0000: (1) 7 bits

config_colors:
           subroutine
           lda #0                 ; black
           sta EXTCOL
           sta BGCOL0
           lda #1                 ; white
           sta COLOR
           lda #$f                ; gray
           sta SP0COL
           sta SP1COL
           sta SP2COL
           sta SP3COL
           sta SP4COL
           sta SP5COL
           rts


indicate_dpx_config:
           subroutine
           ldx #6
           ldy #2
           bit dpx_flag
           bmi .set_indication
.clear_indication:
           lda sprite_data_base_addr+offset_dpx*64,y
           and #%11000000
           sta sprite_data_base_addr+offset_dpx*64,y
           iny
           iny
           iny
           dex
           bpl .clear_indication
           rts
.set_indication:
           lda sprite_data_base_addr+offset_dpx*64,y
           ora #%00111100
           sta sprite_data_base_addr+offset_dpx*64,y
           iny
           iny
           iny
           dex
           bpl .set_indication
           rts

indicate_rcv_flag:
           subroutine
           ldx #6
           ldy #2
           bit rcv_flag
           bmi .set_indication
.clear_indication:
           lda sprite_data_base_addr+offset_rcv,y
           and #%11000000
           sta sprite_data_base_addr+offset_rcv,y
           iny
           iny
           iny
           dex
           bpl .clear_indication
           rts
.set_indication:
           lda sprite_data_base_addr+offset_rcv,y
           ora #%00111100
           sta sprite_data_base_addr+offset_rcv,y
           iny
           iny
           iny
           dex
           bpl .set_indication
           rts


open_modem_device:
           subroutine
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
           subroutine
           lda #0                 ; filename length
           jsr SETNAM
           lda #printer_file_no
           ldx #printer_device_no
           ldy #7                 ; secondary address
           jsr SETLFS
           jsr OPEN
           rts


init_rcv_buffer_ptr:
           subroutine
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
           subroutine
           lda #clear_screen
           jsr CHROUT

output_cursor_home_and_cursor_down:
           subroutine
           lda #cursor_home
           jsr CHROUT
           lda #cursor_down
           jsr CHROUT
           jmp fine_scroll_one_line_if_needed

to_rcv_buffer:
           subroutine
           ldy #0
           sta accumlator_save
           cmp #BS
           beq remove_from_rcv_buffer
           cmp #DEL
           bne add_to_rcv_buffer


remove_from_rcv_buffer:
           subroutine
           tya
           sta (rcv_buffer_ptr),y
           lda rcv_buffer_ptr_lo
           bne .decrement_then_done
           lda #>startof_rcv_buffer_addr
           cmp rcv_buffer_ptr_hi
           beq .done
           dec rcv_buffer_ptr_hi
.decrement_then_done:
           dec rcv_buffer_ptr_lo
.done:
           lda accumlator_save
           rts


add_to_rcv_buffer:
           subroutine
           bit rcv_buffer_full_flag
           bmi .done
           sty rcv_notadded_flag  ; y expected to be zero, so indicate 'added'
           sta (rcv_buffer_ptr),y
           inc rcv_buffer_ptr_lo
           bne .done
           inc rcv_buffer_ptr_hi
           lda rcv_buffer_ptr_hi
           cmp MEMSIZ1+1
           bcc .done
           dec rcv_buffer_ptr_hi
           dec rcv_buffer_ptr_lo
           lda #$80               ; set buffer full
           sta rcv_buffer_full_flag
           lda #0
           sta rcv_flag
           jsr indicate_rcv_flag
.done:
           lda #0
           sta (rcv_buffer_ptr),y
           lda accumlator_save
           rts


dump_rcv_buffer_to_screen:
           subroutine
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

.get_and_output_byte:
           ldy #0
           lda (ptr),y
           beq .done
           jsr output_char_to_screen
           jsr GETIN
           cmp #F7
           beq .done
           cmp #" "
           bne .increment_ptr
.pause:
           jsr GETIN
           cmp #F7
           beq .done
           cmp #" "
           bne .pause
.increment_ptr:
           inc ptr_lo
           bne .get_and_output_byte
           inc ptr_hi
           bne .get_and_output_byte
.done:
           lda #CR
           jsr output_char_to_screen
           lda #modem_file_no
           jsr CLOSE
           jsr open_modem_device
           jmp main


init_sprites:
           subroutine
           ldx #$ff               ; set all sprites to double-width
           stx XXPAND
           lda #0                 ; set all sprites to single-height, single color
.loop1:
           sta sprite_data_base_addr,x
           sta sprite_data_base_addr+$100,x
           dex
           bne .loop1

           sta YXPAND
           sta SPMC
           ldx #20
.loop2:
           lda .sprite_data_rcv,x
           sta sprite_data_base_addr+offset_rcv*64,x
           lda .sprite_data_dpx,x
           sta sprite_data_base_addr+offset_dpx*64,x
           lda .sprite_data_64t,x
           sta sprite_data_base_addr+offset_64t*64,x
           lda .sprite_data_ermi,x
           sta sprite_data_base_addr+offset_ermi*64,x
           lda .sprite_data_nal,x
           sta sprite_data_base_addr+offset_nal*64,x
           dex
           bpl .loop2
           lda #54
           sta SP0Y
           sta SP1Y
           sta SP2Y
           sta SP3Y
           sta SP4Y
           lda #0
           sta MSIGX
           lda #184
           sta SP0X
           lda #240
           sta SP1X
           lda #25
           sta SP2X
           lda #74
           sta SP3X
           lda #123
           sta SP4X
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

.sprite_data_64t:
                        ; ------------------------
           hex 70 40 1f ;  111     1         11111
           hex 80 c0 04 ; 1       11           1
           hex 81 40 04 ; 1      1 1           1
           hex f2 40 04 ; 1111  1  1           1
           hex 8b e0 04 ; 1   1 11111          1
           hex 88 40 04 ; 1   1    1           1
           hex 70 40 04 ;  111     1           1
.sprite_data_ermi:
                        ; ------------------------
           hex fb c8 9c ; 11111 1111  1   1  111
           hex 82 2d 88 ; 1     1   1 11 11   1
           hex 82 2a 88 ; 1     1   1 1 1 1   1
           hex e3 c8 88 ; 111   1111  1   1   1
           hex 82 88 88 ; 1     1 1   1   1   1
           hex 82 48 88 ; 1     1  1  1   1   1
           hex fa 28 9c ; 11111 1   1 1   1  111
.sprite_data_nal:
                        ; ------------------------
           hex 89 c8 00 ; 1   1  111  1
           hex 8a 28 00 ; 1   1 1   1 1
           hex ca 28 00 ; 11  1 1   1 1
           hex ab e8 00 ; 1 1 1 11111 1
           hex 9a 28 00 ; 1  11 1   1 1
           hex 8a 28 00 ; 1   1 1   1 1
           hex 8a 2f 80 ; 1   1 1   1 1111
.sprite_data_rcv:
                        ; ------------------------
           hex f1 c8 80 ; 1111   111  1   1
           hex 8a 28 80 ; 1   1 1   1 1   1
           hex 8a 08 80 ; 1   1 1     1   1
           hex f2 05 00 ; 1111  1      1 1
           hex a2 05 00 ; 1 1   1      1 1
           hex 92 22 00 ; 1  1  1   1   1
           hex 89 c2 00 ; 1   1  111    1
.sprite_data_dpx:
                        ; ------------------------
           hex e3 c8 80 ; 111   1111  1   1
           hex 92 28 80 ; 1  1  1   1 1   1
           hex 8a 25 00 ; 1   1 1   1  1 1
           hex 8b e2 00 ; 1   1 11111   1
           hex 8a 05 00 ; 1   1 1      1 1
           hex 92 08 80 ; 1  1  1     1   1
           hex e2 08 80 ; 111   1     1   1




           ds.b newmodem_start-*, 0
newmodem_start:
           org (((* >> 8) + 1) << 8)
           include "newmodem.asm"
           newmodem
