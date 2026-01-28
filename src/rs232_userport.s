; Replacement C64 NTSC/PAL RS-232 userport driver

; Based upon: newmodem.s - Towards 2400 - RS232 revisited by George Hug
; Transactor Magazine, volume 9, issue3, February 1989
; https://archive.org/details/transactor-magazines-v9-i03/page/n63/mode/2up
; and CCGMS Future: https://github.com/mist64/ccgmsterm
;
            .setcpu "6502"

            .export rs232_userport_funcs_setup   := setup
            .export rs232_userport_funcs_enable  := enable
            .export rs232_userport_funcs_disable := disable
            .export rs232_userport_funcs_getchar := getxfer
            .export rs232_userport_funcs_putchar := putxfer

bitci   = $a8     ; RS-232 Input bit count
ridata  = $aa     ; RS-232 Input byte buffer
bitts   = $b4     ; RS-232 Output bit count
nxtbit  = $b5     ; RS-232 Next bit to send
rodata  = $b6     ; RS-232 Output byte buffer

nminv   = $318    ; Vector: Non-maskable interrupt

ci2pra  = $dd00   ; CIA#2 data port register A
ci2prb  = $dd01   ; CIA#2 data port register B
ti2alo  = $dd04   ; CIA#2 timer A lo byte
ti2ahi  = $dd05   ; CIA#2 timer A hi byte
ti2blo  = $dd06   ; CIA#2 timer B lo byte
ti2bhi  = $dd07   ; CIA#2 timer B hi byte
ci2icr  = $dd0d   ; CIA#2 interrupt control register
ci2cra  = $dd0e   ; CIA#2 control register A
ci2crb  = $dd0f   ; CIA#2 control register B

            .if .def(__C64__)
ribuf   = $f7     ; RS-232: recv buffer ptr
ridbe   = $29b    ; RS-232: index to end of recv buffer
ridbs   = $29c    ; RS-232: index to start of recv buffer
rodbs   = $29d    ; RS-232: index to start of send buffer
rodbe   = $29e    ; RS-232: index to end of send buffer
enabl   = $2a1    ; RS-232: NMI interrupts enabled from ci2icr (bit4=wait for rcv edge, bit1=rcving data, bit0=xmiting data)
nmi     = nmi64
            .elseif .def(__C128__)
ribuf   = $c8     ; RS-232: recv buffer ptr
ridbe   = $a18    ; RS-232: index to end of recv buffer
ridbs   = $a19    ; RS-232: index to start of recv buffer
rodbs   = $a1a    ; RS-232: index to start of send buffer
rodbe   = $a1b    ; RS-232: index to end of send buffer
enabl   = $a0f    ; RS-232: NMI interrupts enabled from ci2icr (bit4=wait for rcv edge, bit1=rcving data, bit0=xmiting data)
nmi     = nmi128
            .else
            .error "__C64__ or __C128__ are not specified."
            .endif

            .code

setup:      ; x: baud_rate: 0=300, 1=1200, 2=2400; y: 0=ntsc, ~0=pal
            sei
            jsr setbaud
            lda #<nmi
            ldy #>nmi
            sta nminv
            sty nminv+1
            cli
            rts

setbaud:
            txa
            asl a
            cpy #0
            beq :+       ; 0=NTSC, ~0=PAL
            clc
            adc #boffset
:           tay
            lda bloc,y
            sta xmitlo
            lda bloc+1,y
            sta xmithi
            lda bloc+6,y
            sta strtlo
            lda bloc+7,y
            sta strthi
            lda bloc+12,y
            sta fulllo
            lda bloc+13,y
            sta fullhi
            rts

nmi64:
            pha
            txa
            pha
            tya
            pha
nmi128:
            cld
            ldx ti2bhi     ; sample timer b hi byte
            lda #%01111111 ; disable cia nmi's
            sta ci2icr
            lda ci2icr     ; read/clear flags
           ;bpl notcia     ; (restore key)
            cpx ti2bhi     ; timer b timeout? (since sampling timer hi byte above)
            ldy ci2prb     ; sample pin C for receive NMI
            bcs :+         ; no timeout
            ora #%00000010 ; set flag
            ora ci2icr     ; read flags again (changed since timeout)
:           and enabl      ; mask out non-enabled interrupts
            tax            ; bitmask of interrupts to be serviced
            lsr            ; check timer a
            bcc :+
            lda ci2pra     ; put output bit on pin M (PA2)
            and #%11111011
            ora nxtbit     ; output bit
            sta ci2pra
:           txa            ; check flag NMI
            and #%00010000 ; indicates new byte coming in
            beq nmion
strtlo=*+1
            lda #0         ; initialize timer b with start bit time
            sta ti2blo
strthi=*+1
            lda #0
            sta ti2bhi
            lda #$11
            sta ci2crb     ; start timer b, load latched value to counter
            lda #%00010010
            eor enabl
            sta enabl
            sta ci2icr     ; enable flag, timer b interrupts
fulllo=*+1
            lda #0         ; latch full bit value for next countdown
            sta ti2blo
fullhi=*+1
            lda #0
            sta ti2bhi
            lda #8
            sta bitci      ; input bits
            jmp chktxd

;notcia     ldy #0
;           jmp rstkey     ; or jmp norest

nmion:
            lda enabl      ; receive a bit
            sta ci2icr
            txa
            and #%00000010
            beq chktxd
            tya
            lsr
            ror ridata     ; input byte
            dec bitci      ; input bits
            bne txd
            lda ridata     ; input byte
            ldx ridbe      ; tail index to receive buffer
            sta ribuf,x    ; and store it
            inc ridbe      ; tail index to receive buffer
            lda #0
            sta ci2crb
            lda #%00010010
switch:
            ldy #%01111111
            sty ci2icr     ; twice
            sty ci2icr
            eor enabl
            sta enabl
            sta ci2icr
txd:
            txa
            lsr
chktxd:
            bcc @nmiflow
            dec bitts      ; output bits
            bmi @endbyte
            lda #4
            ror rodata     ; output byte
            bcs :+
            lda #0
:           sta nxtbit     ; output bit
@nmiflow:
            lda bitci      ; input bits
            and #8
            beq :+
            clc
:           pla
            tay
            pla
            tax
            pla
            rti
@endbyte:
            lda #0
            sta isbyte
            ldx #0         ; turn transmit interrupt off
            stx ci2cra
            lda #1
            bne switch

disable:    ; disables RS-232 input function without selecting another device for input (do before disk, tape, REU operation)
            pha
:           lda isbyte
            bne :-
            lda #%10000
            sta ci2icr
            lda #2
            and enabl
            bne :-
            sta enabl
            pla
            rts

enable:     ; enables RS-232 input function without selecting device #2 for input (do after disk, tape, REU operation)
            lda enable
            and #$12
            beq :+
            rts
:           sta ci2crb
            lda #$90
change:
            sta ci2icr
            php
            sei
            ldy #%01111111
            sty ci2icr
            sty ci2icr
            ora enabl
            sta enabl
            sta ci2icr
            plp
:           bit isbyte
            bne :-
            rts

getxfer:    ; get char from RS-232 recv buffer regardless of current input device (carry set if buffer empty)
            jsr $f04f      ; KERNAL code to set up user port
            ldx ridbs      ; head index to receive buffer
            cpx ridbe      ; tail index to receive buffer
            beq :+         ; skip (empty buffer, return with carry set)
            lda ribuf,x
            pha
            inc ridbs      ; head index to receive buffer
            clc
            pla
:           rts

putxfer:    ; put char to RS-232 send buffer regardless of current output device
            pha
            stx save_x
            sty save_y
            sta rodata     ; output byte
            lda #0
            sta nxtbit     ; output bit
            lda #9
            sta bitts      ; output bits
            lda #$ff
            sta isbyte
xmitlo=*+1
            lda #0
            sta ti2alo
xmithi=*+1
            lda #0
            sta ti2ahi
            lda #%00010001
            sta ci2cra
            lda #$81
            jsr change
            clc
save_x=*+1
            ldx #0
save_y=*+1
            ldy #0
            pla
            rts

            .rodata
bloc:
bntsc:
            .word 3408,  851, 425 ; NTSC transmit times
            .word 4915, 1090, 459 ; NTSC startbit times
            .word 3410,  845, 421 ; NTSC full bit times
bpal:
            .word 3283,  820, 409 ; PAL transmit times
            .word 4735, 1050, 442 ; PAL startbit times
            .word 3285,  814, 406 ; PAL full bit times
boffset = bpal - bntsc

            .data
isbyte:
            .byte 0
