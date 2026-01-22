; newmodem.s - Towards 2400 - RS232 revisited by George Hug
; Transactor Magazine, volume 9, issue3, February 1989
; https://archive.org/details/transactor-magazines-v9-i03/page/n63/mode/2up
;
.setcpu "6502"
.export newmodem_start
.export restore_key_flag

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
newmodem_start:
xx00:   jmp setup
xx03:   jmp inable ; enables RS-232 input function without selecting device #2 for input (do after disk, tape, REU operation)
xx06:   jmp disabl ; disables RS-232 input function without selecting another device for input (do before disk, tape, REU operation)
xx09:   jmp rsget  ; get char from RS-232 recv buffer regardless of current input device (carry set if buffer empty)
xx0c:   jmp rsout  ; put char to RS-232 send buffer regardless of current output device
        nop
;--------------------------------------------
setup:
        lda #<nmi
        ldy #>nmi
        sta nminv
        sty nminv+1
        lda #<nchkin
        ldy #>nchkin
        sta ichkin
        sty ichkin+1
        lda #<nbsout
        ldy #>nbsout
        sta ibsout
        sty ibsout+1
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
        ldx ti2bhi     ; sample timer b hi byte (3060)
        lda #%01111111 ; disable cia nmi's
        sta ci2icr
        lda ci2icr     ; read/clear flags (3090)
        bpl notcia     ; (restore key)
        cpx ti2bhi     ; tb timeout since 3060? (3110)
        ldy ci2prb     ; (sample pin c)
        bcs @mask      ; no
        ora #%00000010 ; yes, set flag in acc.
        ora ci2icr     ; read/clear flags again
@mask:
        and enabl      ; mask out non-enabled
        tax            ; these must be serviced
        lsr            ; timer a? (bit 0)
        bcc @ckflag    ; no
        lda ci2pra     ; yes, put bit on pin M (PA2)
        and #%11111011
        ora nxtbit
        sta ci2pra
@ckflag:
        txa
        and #%00010000 ; *flag nmi? (bit 4)
        beq nmion      ; no
strtlo:
        lda #$42       ; yes, start-bit to tb  (argument overwritten)
        sta ti2blo
strthi:
        lda #$04       ;                       (argument overwritten)
        sta ti2bhi
        lda #$11       ; start tb counting
        sta ci2crb
        lda #$12       ; *flag nmi off, tb on
        eor enabl      ; update mask
        sta enabl
        sta ci2icr     ; enable new config
fulllo:
        lda #$4d       ; change reload latch   (argument overwritten)
        sta ti2blo     ;   to full-bit time
fullhi:
        lda #$03       ;                       (argument overwritten)
        sta ti2bhi
        lda #8         ; # of bits to receive
        sta bitci
        bne chktxd     ; branch always

notcia:
        ldy #$80
        sty restore_key_flag
        jmp return     ; or rstkey or norest

nmion:
        lda enabl      ; re-enable nmi's
        sta ci2icr
        txa
        and #%00000010 ; timer b? (bit 1)
        beq chktxd     ; no
        tya            ; yes, sample from '(sample pin c)' above
        lsr
        ror ridata     ; rs232 is lsb first
        dec bitci      ; byte finished?
        bne txd        ; no
        ldy ridbe      ; yes, byte to buffer
        lda ridata
        sta (ribuf),y  ; (no overrun test)
        inc ridbe
        lda #0         ; stop timer b
        sta ci2crb
        lda #%00010010 ; tb nmi off, *flag on
switch:
        ldy #%01111111 ; disable nmi's
        sty ci2icr     ; twice
        sty ci2icr
        eor enabl      ; update mask
        sta enabl
        sta ci2icr     ; enable new config
txd:
        txa
        lsr            ; timer a?
chktxd:
        bcc @exit      ; no
        dec bitts      ; yes, byte finished?
        bmi @char      ; yes
        lda #4         ; no, prep next bit
        ror rodata     ; (fill with stop bits)
        bcs @store
@low:
        lda #0
@store:
        sta nxtbit
@exit:
        jmp return     ; restore regs, rti
@char:
        ldy rodbs
        cpy rodbe      ; buffer empty?
        beq @txoff     ; yes
@getbuf:
        lda (robuf),y  ; no, prep next byte
        inc rodbs
        sta rodata
        lda #9         ; # bits to send
        sta bitts
        bne @low       ; always - do start bit
@txoff:
        lda #0         ; stop timer a
        stx ci2cra
        lda #1         ; disable ta nmi
        bne switch
;--------------------------------------------
disabl:
        pha            ; turns off modem port
@test:
        lda enabl
        and #%11       ; any current activity?
        bne @test      ; yes, test again
        lda #%10000    ; no, disable *flag nmi
        sta ci2icr
        lda #%10
        and enabl      ; currently receiving?
        bne @test      ; yes, start over
        sta enabl      ; all off, update mask
        pla
        rts
;--------------------------------------------
nbsout:
        pha            ; new bsout
        lda dflto
        cmp #2
        bne notmod
        pla
rsout:
        sta sava       ; output to modem
        sty savy
point:
        ldy rodbe
        sta (robuf),y  ; not official till pointer bumped
        iny
        cpy rodbs      ; buffer full?
        beq fulbuf     ; yes
        sty rodbe      ; no, bump pointer
strtup:
        lda enabl
        and #1         ; transmitting now?
        bne ret3       ; yes
        sta nxtbit     ; no, prep start bit
        lda #9
        sta bitts      ;   # bits to send
        ldy rodbs
        lda (robuf),y
        sta rodata     ;  and next byte
        inc rodbs
        lda baudof     ; full tx bit time to ta
        sta ti2alo
        lda baudof+1
        sta ti2ahi
        lda #%10001    ; start timer a
        sta ci2cra
        lda #%10000001 ; enable ta nmi
change:
        sta ci2icr     ; nmi clears flag if set
        php            ; save irq status
        sei            ; disable irq's
        ldy #%01111111 ; disable nmi's
        sty ci2icr     ; twice
        sty ci2icr
        ora enabl      ; update mask
        sta enabl
        sta ci2icr     ; enable new config
        plp            ; restore irq status
ret3:
        clc
        ldy savy
        lda sava
        rts
fulbuf:
        jsr strtup
        jmp point
notmod:
        pla            ; back to old bsout
        jmp oldout
;--------------------------------------------
nchkin:
        jsr findfn     ; new chkin
        bne nchkin_nosuch
        jsr devnum
        lda fa
        cmp #2
        bne nchkin_back
        sta dfltn

inable:
        sta sava       ; enable rs232 input
        sty savy

baud:
        lda baudof+1   ; set receive to same baud rate as xmit
        asl a          ; seems to be needed, missing on original listing
        and #$06
        tay
        lda strt24,y
        sta strtlo+1   ; overwrite values in nmi handler
        lda strt24+1,y
        sta strthi+1
        lda full24,y
        sta fulllo+1
        lda full24+1,y
        sta fullhi+1

        lda enabl
        and #%00010010 ; *flag or tb on?
        bne ret1       ; yes
        sta ci2crb     ; no stop tb
        lda #%10010000 ; turn on flag nmi
        jmp change

nchkin_nosuch:
        jmp nofile
nchkin_back:
        lda fa
        jmp oldchk
;--------------------------------------------
rsget:
        sta sava       ; input from modem
        sty savy
        ldy ridbs
        cpy ridbe      ; buffer empty?
        beq ret2       ; yes
        lda (ribuf),y  ; no, fetch character
        sta sava
        inc ridbs
ret1:
        clc            ; cc = char in acc.
ret2:
        ldy savy
        lda sava
        rts            ; cs = buffer was empty

.rodata
.if .def(__NTSC__)
strt24:  .word $01cb   ; 459   start bit times
strt12:  .word $0442   ; 1090
strt03:  .word $1333   ; 4915
full24:  .word $01a5   ; 421   full bit times
full12:  .word $034d   ; 845
full03:  .word $0d52   ; 3410
.elseif .def(__PAL__)
strt24:  .word $01ba   ; 442   start bit times
strt12:  .word $041a   ; 1050
strt03:  .word $127d   ; 4733
full24:  .word $0195   ; 405   full bit times
full12:  .word $032e   ; 814
full03:  .word $0cd4   ; 3284
.else
.error "__NTSC__ or __PAL__ are not specified."
.endif

.bss
restore_key_flag:
        .res 1
