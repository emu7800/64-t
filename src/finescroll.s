            .setcpu "6502"

            .include "cbm_kernal.inc"
            .include "c64.inc"

            .export finescroll_funcs_clearscreen   := clearscreen
            .export finescroll_funcs_settextcolor  := settextcolor
            .export finescroll_funcs_scrolloneline := scrolloneline

screen_copy_offset = 22*40 - 3*$100

lo_screen_nybble = $1
hi_screen_nybble = $a
lo_screen_addr   = $0400*lo_screen_nybble ; = $0400 video matrix addr
hi_screen_addr   = $0400*hi_screen_nybble ; = $2800 video matrix addr

CR        = 13    ; PETSCII/ASCII carriage return
space     = 32    ; PETSCII/ASCII space
cursor_up = 145   ; PETSCII cursor up
cls       = 147   ; PETSCII clear screen

TBLX   = $d6   ; Current cursor physical line number (0-24)
HIBASE = $0288 ; Top page of screen memory

            .code

clearscreen:
            sta save_a
            stx save_x
            sty save_y

            lda #>hi_screen_addr
            sta HIBASE
            lda #cls
            jsr CHROUT
            lda #>lo_screen_addr
            sta HIBASE
            lda #cls
            jsr CHROUT
            lda #0                 ; select low screen
            sta screen_select_flag
            lda #(lo_screen_nybble << 4 | 6) ; 0001 0110  video matrix: 1*1k=$400, set low screen
            sta VIC_VIDEO_ADR
            lda #%00010111         ; scroll 7, 24 rows
            sta VIC_CTRL1
            lda #CR
            jsr CHROUT

            ldy save_y
            ldx save_x
            lda save_a
            rts

settextcolor:
            sta textcolor
            rts

scrolloneline:
            sta save_a
            stx save_x
            sty save_y

            lda TBLX
            cmp #23
            bcs :+
            jmp @return

:           ldy #39           ; copy to opposite screen one line up
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

            lda textcolor
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
            bmi @do_fine_scroll

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

@do_fine_scroll:
            ldy #7
:           bit VIC_CTRL1
            bpl :-                 ; wait until raster is off visible screen
            dec VIC_CTRL1          ; fine scroll one line
            ldx #200
:           dex
            bne :-
            dey
            bne :--
:           bit VIC_CTRL1
            bpl :-                 ; wait until raster is off visible screen

            bit screen_select_flag ; swap screens to implement carriage return
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

@done:      lda #%00010111         ; scroll 7, 24 rows
            sta VIC_CTRL1
            lda #22
            sta TBLX
            lda #cursor_up
            jsr CHROUT
            lda #CR
            jsr CHROUT

@return:
            ldy save_y
            ldx save_x
            lda save_a
            rts

            .bss

save_a:     .byte 0
save_x:     .byte 0
save_y:     .byte 0
screen_select_flag:
            .byte 0   ; bit7: 0=low screen, 1=high screen
textcolor:  .byte 0
