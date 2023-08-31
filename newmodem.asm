; Towards 2400 - RS232 revisited by George Hug
; Transactor Magazine, volume 9, issue3, February 1989
; https://archive.org/details/transactor-magazines-v9-i03/page/n63/mode/2up
;
; Ported into a DASM macro by Mike Murphy. See https://dasm-assembler.github.io/.
;
        MAC newmodem
        processor 6502

;--------------------------------------------
;  "newmodem.src" - 64 mode.
;  @128 = changes for 128 mode.
;--------------------------------------------
savy    =$97           ;save register y location
dfltn   =$99           ;default input device (0=keyboard)
dflto   =$9a           ;default output device (3=screen)
sava    =$9e           ;save register a location
bitci   =$a8           ;RS-232 Input bit count
ridata  =$aa           ;RS-232 Input byte buffer
bitts   =$b4           ;RS-232 Output bit count
nxtbit  =$b5           ;RS-232 Next bit to send
rodata  =$b6           ;RS-232 Output byte buffer
fa      =$ba           ;current device number
ribuf   =$f7           ;@128 $c8    RS-232: recv buffer ptr
robuf   =$f9           ;@128 $ca    RS-232: send buffer ptr
baudof  =$0299         ;@128 $0a16  RS-232: time required to send a bit (2 bytes:clock/baud/2-100)
ridbe   =$029b         ;@128 $0a18  RS-232: index to end of recv buffer
ridbs   =$029c         ;@128 $0a19  RS-232: index to start of recv buffer
rodbs   =$029d         ;@128 $0a1a  RS-232: index to start of send buffer
rodbe   =$029e         ;@128 $0a1b  RS-232: index to end of send buffer
enabl   =$02a1         ;@128 $0a0f  RS-232: NMI interrupts enabled from ci2icr (bit4=wait for rcv edge, bit1=rcving data, bit0=xmiting data)
nminv   =$0318         ;Vector: Non-maskable interrupt
ichkin  =$031e         ;Vector: Kernal CHKIN
ibsout  =$0326         ;Vector: Kernal CHROUT
ci2pra  =$dd00         ;CIA#2 data port register A
ci2prb  =$dd01         ;CIA#2 data port register B
ti2alo  =$dd04         ;CIA#2 timer A lo byte
ti2ahi  =$dd05         ;CIA#2 timer A hi byte
ti2blo  =$dd06         ;CIA#2 timer B lo byte
ti2bhi  =$dd07         ;CIA#2 timer B hi byte
ci2icr  =$dd0d         ;CIA#2 interrupt control register
ci2cra  =$dd0e         ;CIA#2 control register A
ci2crb  =$dd0f         ;CIA#2 control register B
rstkey  =$fe56         ;@128 $fa4b
norest  =$fe72         ;@128 $fa5f
return  =$febc         ;@128 $ff33
oldout  =$f1ca         ;@128 $ef79
oldchk  =$f21b         ;@128 $f10e
findfn  =$f30f         ;@128 $f202
devnum  =$f31f         ;@128 $f212
nofile  =$f701         ;@128 $f682
;--------------------------------------------
;org    =$ce00         ;@128 $1a00
;--------------------------------------------
xx00    jmp setup
xx03    jmp inable     ;enables RS-232 input function without selecting device #2 for input (do after disk, tape, REU operation)
xx06    jmp disabl     ;disables RS-232 input function without selecting another device for input (do before disk, tape, REU operation)
xx09    jmp rsget      ;get char from RS-232 recv buffer regardless of current input device (carry set if buffer empty)
xx0c    jmp rsout      ;put char to RS-232 send buffer regardless of current output device
        nop
strt24  .word $01cb    ; 459 start-bit times
strt12  .word $0442    ;1090
strt03  .word $1333    ;4915
full24  .word $01a5    ; 421 full-bit times
full12  .word $034d    ; 845
full03  .word $0d52    ;3410
;--------------------------------------------
setup:
        lda #<nmi64    ;@128 #<nmi128
        ldy #>nmi64    ;@128 #>nmi128
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
        pha            ;new nmi handler
        txa
        pha
        tya
        pha
nmi128:
        cld
        ldx ti2bhi     ;sample timer b hi byte
        lda #$7f       ;disable cia nmi's
        sta ci2icr
        lda ci2icr     ;read/clear flags
        bpl notcia     ;(restore key)
        cpx ti2bhi     ;tb timeout since 3060?
        ldy ci2prb     ;(sample pin c)
        bcs mask       ;no
        ora #$02       ;yes, set flag in acc.
        ora ci2icr     ;read/clear flags again
mask:
        and enabl      ;mask out non-enabled
        tax            ;these must be serviced
        lsr            ;timer a? (bit 0)
        bcc ckflag     ;no
        lda ci2pra     ;yes, put bit on pin m
        and #$fb
        ora nxtbit
        sta ci2pra
ckflag:
        txa
        and #$10       ;*flag nmi? (bit 4)
        beq nmion      ;no
strtlo:
        lda #$42       ;yes, start-bit to tb
        sta ti2blo
strthi:
        lda #$04
        sta ti2bhi
        lda #$11       ;start tb counting
        sta ci2crb
        lda #$12       ;*flag nmi off, tb on
        eor enabl      ;update mask
        sta enabl
        sta ci2icr     ;enable new config.
fulllo:
        lda #$4d       ;change reload latch
        sta ti2blo     ;  to full-bit time
fullhi:
        lda #$03
        sta ti2bhi
        lda #$08       ;# of bits to receive
        sta bitci
        bne chktxd     ;branch always
notcia:
        ldy #$00
        jmp rstkey     ;or jmp norest
nmion:
        lda enabl      ;re-enable nmi's
        sta ci2icr
        txa
        and #$02       ;timer b? (bit 1)
        beq chktxd     ;no
        tya            ;yes, sample from '(sample pin c)' above
        lsr
        ror ridata     ;rs232 is lsb first
        sta (ribuf),y  ;(no overrun test)
        inc ridbe
        lda #$00       ;stop timer b
        sta ci2crb
        lda #$12       ;tb nmi off, *flag on
switch:
        ldy #$7f       ;disable nmi's
        sty ci2icr     ;twice
        sty ci2icr
        eor enabl      ;update mask
        sta enabl
        sta ci2icr     ;enable new config.
txd:
        txa
        lsr            ;timer a?
chktxd:
        bcc exit       ;no
        dec bitts      ;yes, byte finished?
        bmi char       ;yes
        lda #$04       ;no, prep next bit
        ror rodata     ;(fill with stop bits)
        bcs store
low     lda #$00
store:
        sta nxtbit
exit:
        jmp return     ;restore regs, rti
char:
        ldy rodbs
        cpy rodbe      ;buffer empty?
        beq txoff      ;yes
getbuf:
        lda (robuf),y  ;no, prep next byte
        inc rodbs
        sta rodata
        lda #$09       ;# bits to send
        sta bitts
        bne low        ;always - do start bit
txoff:
        lda #$00       ;stop timer a
        stx ci2cra
        lda #$01       ;disable ta nmi
        bne switch
;--------------------------------------------
disabl:
        pha            ;turns off modem port
test:
        lda enabl
        and #$03       ;any current activity?
        bne test       ;yes, test again
        lda #$10       ;no, disable *flag nmi
        sta ci2icr
        lda #$02
        and enabl      ;currently receiving?
        bne test       ;yes, start over
        sta enabl      ;all off, update mask
        pla
        rts
;--------------------------------------------
nbsout:
        pha            ;new bsout
        lda dflto
        cmp #$02
        bne notmod
        pla
rsout:
        sta sava       ;output to modem
        sty savy
point:
        ldy rodbe
        sta (robuf),y  ;not official till 'bump pointer' below
        cpy rodbs      ;buffer full?
        beq fulbuf     ;yes
        sty rodbe      ;no, bump pointer
strtup:
        lda enabl
        and #$01       ;transmitting now?
        bne ret3       ;yes
        sta nxtbit     ;no, prep start bit
        lda #$09
        sta bitts      ;  # bits to send
        ldy rodbs
        lda (robuf),y
        sta rodata     ;  and next byte
        inc rodbs
        lda baudof     ;full tx bit time to ta
        sta ti2alo
        lda baudof+1
        sta ti2ahi
        lda #$11       ;start timer a
        sta ci2cra
        lda #$81       ;enable ta nmi
change:
        sta ci2icr     ;nmi clears flag if set
        php            ;save irq status
        sei            ;disable irq's
        ldy #$7f       ;disable nmi's
        sty ci2icr     ;twice
        sty ci2icr
        ora enabl      ;update mask
        sta enabl
        sta ci2icr     ;enable new config.
        plp            ;restore irq status
ret3:
        clc
        ldy savy
        lda sava
        rts
fulbuf:
        jsr strtup
        jmp point
notmod:
        pla            ;back to old bsout
        jmp oldout
;--------------------------------------------
nchkin:
        jsr findfn     ;new chkin
        bne nosuch
        jsr devnum
        lda fa
        cmp #$02
        bne back
        sta dfltn
inable:
        sta sava       ;enable rs232 input
        sty savy
baud:
        lda baudof+1   ;set receive to same

       ;and #$06       ;  baud rate as xmit
        and #3         ;fix
        asl a          ;fix

        tay
        lda strt24,y
        sta strtlo+1   ;overwrite values in nmi handler
        lda strt24+1,y
        sta strthi+1
        lda full24,y
        sta fulllo+1
        lda full24+1,y
        sta fullhi+1

        lda enabl
        and #$12       ;*flag or tb on?
        bne ret1       ;yes
        sta ci2crb     ;no stop tb
        lda #$90       ;turn on flag nmi
        jmp change
nosuch:
        jmp nofile
back:
        lda fa
        jmp oldchk
;--------------------------------------------
rsget:
        sta sava       ;input from modem
        sty savy
        ldy ridbs
        cpy ridbe      ;buffer empty?
        beq ret2       ;yes
        lda (ribuf),y  ;no, fetch character
        sta sava
        inc ridbs
ret1:
        clc            ;cc = char in acc.
ret2:
        ldy savy
        lda sava
        rts            ;cs = buffer was empty

        ENDM