; Based upon: newmodem.s - Towards 2400 - RS232 revisited by George Hug
; Transactor Magazine, volume 9, issue3, February 1989
; https://archive.org/details/transactor-magazines-v9-i03/page/n63/mode/2up
;
            .setcpu "6502"

            .export rs232_userport_funcs

status  = $90
savy    = $97     ; save register y location
dfltn   = $99     ; default input device (0=keyboard)
dflto   = $9a     ; default output device (3=screen)
sava    = $9e     ; save register a location
bitci   = $a8     ; RS-232 Input bit count
ridata  = $aa     ; RS-232 Input byte buffer
bitts   = $b4     ; RS-232 Output bit count
nxtbit  = $b5     ; RS-232 Next bit to send
rodata  = $b6     ; RS-232 Output byte buffer
fa      = $ba     ; current device number

; friendlier names
inbits	= bitci	  ; bit counter during RS232 input
inbyte	= ridata  ; byte buffer during RS232 input
outbits	= bitts	  ; bit counter and stop bit switch during RS232 output
outbit	= nxtbit  ; bit buffer (in bit #2) during RS232 output
outbyte	= rodata  ; byte buffer during RS232 output
rtail   = ridbe   ; RS-232: index to end of recv buffer
rhead   = ridbs   ; RS-232: index to start of recv buffer

nminv   = $318    ; Vector: Non-maskable interrupt
ichkin  = $31e    ; Vector: Kernal CHKIN
ibsout  = $326    ; Vector: Kernal CHROUT

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
robuf   = $f9     ; RS-232: send buffer ptr
baudof  = $299    ; RS-232: time required to send a bit (2 bytes: clock/baud/2-100)
ridbe   = $29b    ; RS-232: index to end of recv buffer
ridbs   = $29c    ; RS-232: index to start of recv buffer
rodbs   = $29d    ; RS-232: index to start of send buffer
rodbe   = $29e    ; RS-232: index to end of send buffer
enabl   = $2a1    ; RS-232: NMI interrupts enabled from ci2icr (bit4=wait for rcv edge, bit1=rcving data, bit0=xmiting data)
nmi    := nmi64
rstkey := $fe56   ; resume standard NMI handler, performing normal restore key handling (cart/basic warm start)
norest := $fe72   ; resume standard NMI handler, skipping normal restore key handling
return := $febc   ; return from NMI handler
oldout := $f1ca
oldchk := $f21b
findfn := $f30f
devnum := $f31f
nofile := $f701
            .elseif .def(__C128__)
ribuf   = $c8     ; RS-232: recv buffer ptr
robuf   = $ca     ; RS-232: send buffer ptr
baudof  = $a16    ; RS-232: time required to send a bit (2 bytes: clock/baud/2-100)
ridbe   = $a18    ; RS-232: index to end of recv buffer
ridbs   = $a19    ; RS-232: index to start of recv buffer
rodbs   = $a1a    ; RS-232: index to start of send buffer
rodbe   = $a1b    ; RS-232: index to end of send buffer
enabl   = $a0f    ; RS-232: NMI interrupts enabled from ci2icr (bit4=wait for rcv edge, bit1=rcving data, bit0=xmiting data)
nmi    := nmi128
rstkey := $fa4b   ; resume standard NMI handler, performing normal restore key handling (cart/basic warm start)
norest := $fa5f   ; resume standard NMI handler, skipping normal restore key handling
return := $ff33   ; return from NMI handler
oldout := $ef79
oldchk := $f10e
findfn := $f202
devnum := $f212
nofile := $f682
            .else
            .error "__C64__ or __C128__ are not specified."
            .endif

            .code
;--------------------------------------------
rs232_userport_funcs:
            .word setup
            .word enable  ; enables RS-232 input function without selecting device #2 for input (do after disk, tape, REU operation)
            .word disable ; disables RS-232 input function without selecting another device for input (do before disk, tape, REU operation)
            .word getxfer ; get char from RS-232 recv buffer regardless of current input device (carry set if buffer empty)
            .word putxfer ; put char to RS-232 send buffer regardless of current output device
;--------------------------------------------
setup:
        sei
        jsr setbaud
        lda #<nmi
        ldy #>nmi
        sta nminv
        sty nminv+1
        rts
;--------------------------------------------
; x: baud_rate: 0=300, 1=1200, 2=2400
; y: 0=ntsc, 1=pal
setbaud:
        txa
        asl a
        cpy #0
        beq @setntsc
@setpal:
        clc
        adc boffset
@setntsc:
        tay
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
;--------------------------------------------
nmi64:
        pha            ; new nmi handler
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
        ldy ci2prb     ; sample pin c for receive NMI
        bcs @mask      ; no timeout
        ora #%00000010 ; yes, set flag in acc
        ora ci2icr     ; read flags again (changed since timeout)
@mask:
        and enabl      ; mask out non-enabled interrupts
        tax            ; bitmask of interrupts to be serviced
        lsr            ; check timer a
        bcc @ckflag
        lda ci2pra     ; put output bit on pin M (PA2)
        and #%11111011
        ora nxtbit
        sta ci2pra
@ckflag:
        txa            ; check flag NMI
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
        lda #$12
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
        sta bitci
        jmp chktxd

;notcia ldy #0
;       jmp rstkey     ; or jmp norest

nmion:
        lda enabl      ; receive a bit
        sta ci2icr
        txa
        and #%00000010
        beq chktxd
        tya
        lsr
        ror ridata
        dec bitci
        bne txd

        lda ridata
        ldx ridbe      ; index to buffer
        sta ribuf,x    ; and store it
        inc ridbe
        lda #0
        sta ci2crb
        lda #%00010010
switch:
        ldy #%01111111
        sty ci2icr     ; twice
        sty ci2icr
        eor enabl
        sta enabl
        sta ci2icr     ; enable new config
txd:
        txa
        lsr
chktxd:
        bcc @nmiflow
        dec bitts
        bmi @endbyte
        lda #4
        ror rodata
        bcs :+
        lda #0
:
        sta nxtbit
@nmiflow:
        lda bitci
        and #8
        beq :+
        clc
:
        pla
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
;--------------------------------------------
disable:
        pha
:
        lda isbyte
        bne :-
        lda #%10000
        sta ci2icr
        lda #2
        and enabl
        bne :-
        sta enabl
        pla
        rts
;--------------------------------------------
enable:
        lda enable
        and #$12
        beq :+
:
        sta ci2crb
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
:
        bit isbyte
        bne :-
        rts
;--------------------------------------------
getxfer:
        jsr $f04f       ; KERNAL code to set up user port
        ldx ridbs
        cpx ridbe
        beq :+          ; skip (empty buffer, return with carry set)
        lda ribuf,x
        pha
        inc ridbs
        clc
        pla
:
        rts
;--------------------------------------------
putxfer:
        pha
        stx save_x
        sty save_y
        sta rodata
        lda #0
        sta nxtbit
        lda #9
        sta bitts
        lda #$ff
        sta isbyte
xmitlo=*+1
        lda #0
        sta ti2alo
xmithi=*+1
        lda #0
        sta ti2ahi
        lda #$11
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
;--------------------------------------------
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

            .bss
isbyte:
            .byte 0
