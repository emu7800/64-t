        processor 6502

; Towards 2400 - RS232 revisited by George Hug
; Transactor Magazine, volume 9, issue3, February 1989
; https://archive.org/details/transactor-magazines-v9-i03/page/n63/mode/2up

;--------------------------------------------
;  "newmodem.src" - 64 mode.
;  @128 = changes for 128 mode.
;--------------------------------------------
ribuf   =$f7           ;@128 $c8
robuf   =$f9           ;@128 $ca
baudof  =$0299         ;@128 $0a16
ridbe   =$029b         ;@128 $0a18
ridbs   =$029c         ;@128 $0a19
rodbs   =$029d         ;@128 $0a1a
rodbe   =$029e         ;@128 $0a1b
enabl   =$02a1         ;@128 $0a0f
rstkey  =$fe56         ;@128 $fa4b
norest  =$fe72         ;@128 $fa5f
return  =$febc         ;@128 $ff33
oldout  =$f1ca         ;@128 $ef79
oldchk  =$f21b         ;@128 $f10e
findfn  =$f30f         ;@128 $f202
devnum  =$f31f         ;@128 $f212
nofile  =$f701         ;@128 $f682
;--------------------------------------------
        org $ce00      ;@128 $1a00
;--------------------------------------------
xx00    jmp setup
xx03    jmp inable
xx06    jmp disabl
xx09    jmp rsget
xx0c    jmp rsout
        nop
strt24  .word $01cb    ; 459 start-bit times
strt12  .word $0442    ;1090
strt03  .word $1333    ;4915
full24  .word $01a5    ; 421 full-bit times
full12  .word $034d    ; 845
full03  .word $0d52    ;3410
;--------------------------------------------
setup   lda #<nmi64    ;@128 #<nmi128
        ldy #>nmi64    ;@128 #>nmi128
        sta $0318
        sty $0319
        lda #<nchkin
        ldy #>nchkin
        sta $031e
        sty $031f
        lda #<nbsout
        ldy #>nbsout
        sta $0326
        sty $0327
        rts
;--------------------------------------------
nmi64   pha            ;new nmi handler
        txa
        pha
        tya
        pha
nmi128  cld
        ldx $dd07      ;sample timer b hi byte
        lda #$7f       ;disable cia nmi's
        sta $dd0d
        lda $dd0d      ;read/clear flags
        bpl notcia     ;(restore key)
        cpx $dd07      ;tb timeout since 3060?
        ldy $dd01      ;(sample pin c)
        bcs mask       ;no
        ora #$02       ;yes, set flag in acc.
        ora $dd0d      ;read/clear flags again
mask    and enabl      ;mask out non-enabled
        tax            ;these must be serviced
        lsr            ;timer a? (bit 0)
        bcc ckflag     ;no
        lda $dd00      ;yes, put bit on pin m
        and #$fb
        ora $b5
        sta $dd00
ckflag  txa
        and #$10       ;*flag nmi? (bit 4)
        beq nmion      ;no
strtlo  lda #$42       ;yes, start-bit to tb
        sta $dd06
strthi  lda #$04
        sta $dd07
        lda #$11       ;start tb counting
        sta $dd0f
        lda #$12       ;*flag nmi off, tb on
        eor enabl      ;update mask
        sta enabl
        sta $dd0d      ;enable new config.
fulllo  lda #$4d       ;change reload latch
        sta $dd06      ;  to full-bit time
fullhi  lda #$03
        sta $dd07
        lda #$08       ;# of bits to receive
        sta $a8
        bne chktxd     ;branch always
notcia  ldy #$00
        jmp rstkey     ;or jmp norest
nmion   lda enabl      ;re-enable nmi's
        sta $dd0d
        txa
        and #$02       ;timer b? (bit 1)
        beq chktxd     ;no
        tya            ;yes, sample from 3120
        lsr
        ror $aa        ;rs232 is lsb first
        sta (ribuf),y  ;(no overrun test)
        inc ridbe
        lda #$00       ;stop timer b
        sta $dd0f
        lda #$12       ;tb nmi off, *flag on
switch  ldy #$7f       ;disable nmi's
        sty $dd0d      ;twice
        sty $dd0d
        eor enabl      ;update mask
        sta enabl
        sta $dd0d      ;enable new config.
txd     txa
        lsr            ;timer a?
chktxd  bcc exit       ;no
        dec $b4        ;yes, byte finished?
        bmi char       ;yes
        lda #$04       ;no, prep next bit
        ror $b6        ;(fill with stop bits)
        bcs store
low     lda #$00
store   sta $b5
exit    jmp return     ;restore regs, rti
char    ldy rodbs
        cpy rodbe      ;buffer empty?
        beq txoff      ;yes
getbuf  lda (robuf),y  ;no, prep next byte
        inc rodbs
        sta $b6
        lda #$09       ;# bits to send
        sta $b4
        bne low        ;always - do start bit
txoff   lda #$00       ;stop timer a
        stx $dd0e
        lda #$01       ;disable ta nmi
        bne switch
;--------------------------------------------
disabl  pha            ;turns off modem port
test    lda enabl
        and #$03       ;any current activity?
        bne test       ;yes, test again
        lda #$10       ;no, disable *flag nmi
        sta $dd0d
        lda #$02
        and enabl      ;currently receiving?
        bne test       ;yes, start over
        sta enabl      ;all off, update mask
        pla
        rts
;--------------------------------------------
nbsout  pha            ;new bsout
        lda $9a
        cmp #$02
        bne notmod
        pla
rsout   sta $9e        ;output to modem
        sty $97
point   ldy rodbe
        sta (robuf),y  ;not official till 5120
        iny
        cpy rodbs      ;buffer full?
        beq fulbuf     ;yes
        sty rodbe      ;no, bump pointer
strtup  lda enabl
        and #$01       ;transmitting now?
        bne ret3       ;yes
        sta $b5        ;no, prep start bit
        lda #$09
        sta $b4        ;  # bits to send
        ldy rodbs
        lda (robuf),y
        sta $b6        ;  and next byte
        inc rodbs
        lda baudof     ;full tx bit time to ta
        sta $dd04
        lda baudof+1
        sta $dd05
        lda #$11       ;start timer a
        sta $dd0e
        lda #$81       ;enable ta nmi
change  sta $dd0d      ;nmi clears flag if set
        php            ;save irq status
        sei            ;disable irq's
        ldy #$7f       ;disable nmi's
        sty $dd0d      ;twice
        sty $dd0d
        ora enabl      ;update mask
        sta enabl
        sta $dd0d      ;enable new config.
        plp            ;restore irq status
ret3    clc
        ldy $97
        lda $9e
        rts
fulbuf  jsr strtup
        jmp point
notmod  pla            ;back to old bsout
        jmp oldout
;--------------------------------------------
nchkin  jsr findfn     ;new chkin
        bne nosuch
        jsr devnum
        lda $ba
        cmp #$02
        bne back
        sta $99
inable  sta $9e        ;enable rs232 input
        sta $97
baud    lda baudof+1   ;set receive to same
        and #$06       ;  baud rate as xmit
        tay
        lda strt24,y
        sta strtlo+1   ;overwrite value @ 3270
        lda strt24+1,y
        sta strthi+1
        lda full24,y
        sta fulllo+1
        lda full24+1,y
        sta fullhi+1
        lda enabl
        and #$12       ;*flag or tb on?
        bne ret1       ;yes
        sta $dd0f      ;no stop tb
        lda #$90       ;turn on flag nmi
        jmp change
nosuch  jmp nofile
back    lda $ba
        jmp oldchk
;--------------------------------------------
rsget   sta $9e        ;input from modem
        sty $97
        ldy ridbs
        cpy ridbe      ;buffer empty?
        beq ret2       ;yes
        lda (ribuf),y  ;no, fetch character
        sta $9e
        inc ridbs
ret1    clc            ;cc = char in acc.
ret2    ldy $97
        lda $9e
last    rts            ;cs = buffer was empty
