        page    ,132
        name    serial
        title   SERIAL - buffered i/o resident extension
;************************************************************************
;*                                                                      *
;* SERIAL - Installable serial i/o subsystem. Interrupt driven input    *
;*              to an internat 2K byte buffer, with characters being    *
;*              remaved from it on a function call basis. Character     *
;*              output is initiated if the channel is clear, else the   *
;*              output driver waits for completion of the last char     *
;*              before transmitting the current one and exiting.        *
;*              The input buffer is a circular buffer, so the last      *
;*              2K characters are always to be found in it.             *
;*                                                                      *
;*      SERIAL is intended to replace the nermal polled interface       *
;*      to the serial channel to allow programs better throughput       *
;*      characteristics.                                                *
;*                                                                      *
;*  GIST Modifications: -added xon/xoff handling in both directions
;*                      -initilized to 9600 baud, no parity, 8 bits, 1 stop
;*                      -added fouth function call to get current length
;*  modified by L. Sherman
;*  Modified by F. Pellett, 05/08/87. (It still needs more work.)
;*
;*  set xbuflen to desired buffer length (2048) and
;*  xbufmax and xbufmin to values when to send xoff and xon (if xoff sent)
;*
;************************************************************************
;
; When an interrupt reaches the PIC, it issues an interrupt to the processor,
; returning vector 30H in response to the active interrupt on line 4 of
; the PIC. At this point, CS:IP will be loaded from [0000:0030]. We will
; supply a new vector to our receive service routine at this address.
;
; When int 14 is executed, CS:IP will be loaded from [0000:0050]. We will
; have taken the vector that was in that position, and stored it into
; SYSTEMVECTOR, replacing the old vector with one of our own. To get back to
; the normal dos code, we would have to do a far indirect jump to SYSTEMVECTOR.
;
; However, I find-that whenever the DOS is called to service the port,
; the interrupt line of the USART becomes disabled as a result of the
; way the service routine is handled.... The result is that the entirety
; of the functions of INT 14 have to be assumed by the SERIAL system, with
; no calls passed through to the dos int 14 handler.
;
; Once the SERIAL handler is installed, very simple programs can be built
; which use the serial port without need for exotic programming techniques
; to ensure servicing the serial port tn a fast polling environment.
;
;************************************************************************
;*                                                                      *
;* EQUATES AND SYMBOL DEFINITIONS                                       *
;*                                                                      *
;************************************************************************
;
; xbuflen-xbufmax must be greater than ~64 at 9600 baud because of a
; shortcoming on the ports card
;
xbuflen                 equ     1024            ; buffer length: from host
tolen                   equ     256             ; buffer length: to host
xbufmax                 equ     800             ; before sending xoff
xbufmin                 equ     50              ; before sending xon after xoff
int14vector             equ     word ptr 0050h  ;int 14 vector location
picbase                 equ     020h            ;pic base register
picmask                 equ     021h            ;pic int mask register
uartmask                equ     11101111b       ;single bit set for uart
commvector              equ     word ptr 0030h  ;address of hdw int service
;
xon                     equ     011h            ; XON control character
xoff                    equ     013h            ; XOFF control character
;
; the following equates apply to the 8250 serial communications device
; which is a hardware specific feature of the 1BM and most compatibles.
;
uart                    equ     word ptr 03f8h
uartdata                equ     word ptr 03f8h
uartintenable           equ     word ptr 03f9h
uartintident            equ     word ptr 03fah
uartlinecontrol         equ     word ptr 03fbh
uartmodemcontrol        equ     word ptr 03fch
uartlinestatus          equ     word ptr 03fdh
uartmodemstatus         equ     word ptr 03feh
uartdivisorlsb          equ     word ptr 03f8h
uartdivisormsb          equ     word ptr 03f9h


;        include \usr\include\gen.mac

;
; This is to get around an "anomaly" with the National Semiconductor
; chip when we need to set the THRE interrupt we need to do the following.
; Note: This will enable interrupts for a short time.
; This information was obtained from NS document 9/4/85.
;
SET_IER         MACRO   irv
                local   l1

                pushf                           ; Save current interrupt flag
                sti                             ; Enable interrupts (gag).

                mov     dx, uartlinestatus
l1:
                in      al, dx
                test    al, 00100000b           ; THRE
                jz      l1

                cli                             ; Disable them now
                mov     dx,uartintenable
                mov     al, irv
                out     dx,al                   ; Write it twice
                out     dx,al

                popf

                ENDM



;************************************************************************
;*                                                                      *
;* CODE BEGINS HERE                                                     *
;*                                                                      *
;************************************************************************

;**************************************************************

serial_TEXT        SEGMENT BYTE PUBLIC 'CODE'
serial_TEXT        ENDS
;CONST              SEGMENT WORD PUBLIC 'CONST'
;CONST              ENDS
;_BSS               SEGMENT WORD PUBLIC 'BSS'
;_BSS               ENDS
;_DATA              SEGMENT WORD PUBLIC 'DATA'
;_DATA              ENDS
;DGROUP             GROUP    CONST,_BSS,_DATA

;ASSUME          CS: serial_TEXT, DS: DGROUP, SS: DGROUP, ES: DGROUP

serial_data     segment
serial_data     ends

assume          cs:serial_TEXT, ds:serial_data, ss:serial_data, es:serial_data

PUBLIC             _serial

;**************************************************************
;


serial_text     segment
;                assume cs:serial_text
;                assume cs:serial_text
;                org     0100h

;                public _serial

_serial         proc    far
                push    bp
                mov     bp,sp
                push    di
                push    si

                push    ds


                mov     ax,seg serial_data
                mov     ds,ax

;
; This code module will intercept all int 14 functions:
;       1. the status function will return a char ready if the buffer
;              is not empty.
;       2. the read function will check the buffer for a character, and
;              if one is not present, will return a zero to the application
;              and WILL HAVE THE ZERO FLAG SET ON NO CHARACTER AVAILABLE.
;              This makes possible polled applications without devious
;              coding.
;
                jmp     serialinit              ;to jump to setup code
;
; When we reach this point, we are being calted by an application as if
; we were int 14.. we must accept its inputs, and return its return values.
;
door            label word                      ;this is the new int 14 entry

                push    ds                      ;gotta save DS cause we use it
                push    ax
                mov     ax,seg serial_data
                mov     ds,ax                   ;make memory references correct
                pop     ax

                cmp     dl,0                    ;make sure on base channel
                jne     systemprocess           ;if not on COM1, go to system

                cmp     ah,0                    ;is it a port setup?
                jne     int1
                jmp     serialsetup

int1:           cmp     ah,1                    ;is it a transmit command?
                jne     int2
                jmp     serialxmit

int2:           cmp     ah,2                    ;see if an input read
                jne     int3
                jmp     serialread              ;handle the read request

int3:           cmp     ah,3                    ;see if a status read
                jne     int4
                jmp     serialstatus

int4:           cmp     ah,4                    ;GIST - see if it a buffer
                                                ; length request
                jne     int5
                jmp     seriallen

int5:           cmp     ah,5                    ;GIST: abort current buffers

                jne     systemprocess           ; all others, hand off
                                                ; to regular interrup handler
                call    serialempty             ; empty out the serial buffer
                pop     ds
                iret


;
; when we got an interrupt, the flags, CS and IP were pushed onto the stack,
; and the interrupt enable flag IF was cleared.  Trap flag TF was also cleared.
;
systemprocess:  pop     ds                      ;get back data segment pointer
                jmp     dword ptr cs:[systemvector]
;

serialsetup:    push    dx
                push    bx
                push    ax

                mov     setupword,ax            ;save calling data

        cli                                     ;disallow interrupts

                mov     dx, uartlinecontrol     ;allow setting of baud rate
                and     al,01fh                 ;mask inactive bits
                or      al,80h                  ;and set DLAB bit.
                out     dx,al                   ;set up to write divisor

                mov     ax,setupword            ;get setup value back
                mov     cl,4                    ;bits to move offset number
                shr     al,cl                   ;move offset number to lsb
                mov     ah,0                    ;zero hi byte of offset
                and     al, 0feh                ;mask out bit 0 (parity)
                add     ax,offset baudtable     ;add base of table to offset
                mov     bx,ax                   ;put effective addr in ptr
                mov     ax,[bx]                 ;now have 16 bit divisor value

                mov     dx,uartdivisormsb       ;get divisor register address
                push    ax
                mov     al,ah
                out     dx,al                   ;write more significant byte
                pop     ax                      ;get back the 16 bit divisor
                mov     dx,uartdivisorlsb
                out     dx,al                   ;write less significant byte

                mov     ax,setupword            ;get setup word back
                mov     dx,uartlinecontrol      ;point to line control reg
                and     al,01fh                 ;mask unused bits
                out     dx,al                   ;and update.

                mov     al,0fh                  ;modem control lines
                mov     dx,uartmodemcontrol
                out     dx,al

                mov     al,01h                  ;received char interrupt only
                mov     dx,uartintenable
                out     dx,al

        sti                                     ;turn interrupts back on

                pop     ax
                pop     bx
                pop     dx
                pop     ds
                iret                            ;return to the application


;
; INT 1: Send a character to the host
serialxmit:     call    quetohost               ; queue char in al to host
                pop     ds
                iret                            ;return to the application

;
; INT 2: Read a character from the host
serialread:     cmp     buffercount,0           ;see if a character is ready
                jne     gotone                  ;if so, fetch it
                mov     al,0                    ;if not, return empty bucket
                or      al,al                   ;set the zero flag
                jmp     nocharread              ;to the application

gotone:         push    bx
                mov     ah,00000001b            ;dummy status byte
                mov     bx,readpointer          ;poaint bx to the read data buf
                mov     al,[bx]                 ;fetch the character
;
                dec     buffercount             ;another one bites the dust
                cmp     hostxoff,0              ;check if xoff was sent atredy
                je      noxon                   ; jmp if no xon to be sent
;                                                 because no previous xoff
; we get here when an xoff was sent before, and we might need to send
; an xon now to get things restarted:
                cmp     buffercount,xbufmin     ;tell host to resume sending
                jg      noxon
;
; this code sends an xon char to the host:
;
sendxon:
                push    ax
                mov     quehead, xon
                SET_IER         3
                mov     hostxoff,0              ; clear hostxoff flag
                pop     ax
;
noxon:          inc     readpointer             ; move to next read loc
                cmp     readpointer,offset buffertop
                jge     resetpointers
;
readexit:       or      ah,ah                   ;clear zero flag if set
                pop     bx                      ;restore index reg used
nocharread:     pop     ds
                ret     0002                    ;return to the caller perserving
                                                ;flags (so don't use iret)
;
resetpointers:  mov     readpointer,offset serialbuffer    ;wrap buffer pointer
                jmp     readexit
;
; the port status routine
;
serialstatus:   push    dx
                mov     dx,uartlinestatus       ;point to line status reg
                in      al,dx
                mov     ah,al

                mov     dx,uartmodemstatus
                in      al,dx

                cmp     buffercount,0
                je      nosetbit                ;no set ready bit if no chars

                or      ax,word ptr 0100h       ;set data ready bit

nosetbit:       pop     dx
                pop     ds
                iret

;     Interrupt processing routine: we arrive here if we have one of
;     two different interrupts: either RCA (Recieved character available)
;     (input has arrived from the host) or THRE (Transmit Holding Register
;     Empty)(it is ok to send data to the host now.)

serialinput     label word

                push    ax
                push    dx
                push    bx
                push    ds
;
; make the local code segment the data segment
;
                mov     ax,seg serial_data
                mov     ds,ax

                mov     dx,uartintident         ; find out which interrupt tt is
                in      al,dx                   ; get Int. Ident. Reg. contents

serialagain:
                cmp     al, 100b                ; is it RCA interrupt?
                jz      rcatrap                 ; jump if it ts
                cmp     al, 10b                 ; is it THRE interrupt?
                jz      sendtohost
                jmp     sendclear
;
;        Begin handling a THRE Interrupt here:
sendtohost:
                cmp     quehead, 0              ; Anything pending at head?
                je      nohead
                mov     al, quehead             ; Load special char for sending
                mov     quehead, 0              ; Zap the space

                jmp     short sendtoa

nohead:
                mov     bx,toread
                mov     al,[bx]                 ; fetch char to be sent out
                dec     tocount                 ; one less char in queue
                inc     toread
                cmp     toread,offset tohosttop ; time to circularly wrap?
                jl      sendtoa
                mov     toread,offset tohostbuf ; wrap pointer back to start

sendtoa:
                cmp     al, xoff
                je      turnoff
                cmp     tocount,0               ; is queue empty now?
                jg      sendtoit                ; just go send the char

;
; The following ten lines of code are gaggage to get around a "peculiarity"
; of the 8250 chip that when the IER is changed, the byte being shifted
; out tn the transmitter shift register falls on the floor. So, this code
; loops around (blah!) waiting for it to empty before changing the IER.
; Interrupts are enabied during this time. Known to be in WD8250.
;
turnoff:        push    ax                      ; SOK (Start of Kludge)
                pushf                           ; Wait for shift reg empty
                sti
                mov     dx,uartlinestatus
gag1:
                in      al, dx
                test    al, 01000000b
                jz      gag1
                popf
                pop     ax                      ; EQK (End of Kiudge)

                push    ax                      ; save char to be sent
                mov     al,1                    ; to disable THRE interrupt
                mov     dx, uartintenable
                out     dx,al
                pop     ax

;               mov     toread,offset tohostbuf         ; empty the queue
;               mov     towrite,offset tohostbuf        ;(tocount is already 0)

sendtoit:
                mov     dx,uartdata
                out     dx,al                   ; actually send the char
;
; This is to get around another "anomaly" with the INS8250-B chip having
; to do with lost THRE interrupts.
;
                mov     dx, uartlinestatus
                in      al, dx
                test    al, 00100000b
                jnz     sendtohost
                jmp     serialinxit

;
;
;        Otherwise, begin RCA (Received character available)
;        interrupt handling:
;
rcatrap:
                mov     dx,uart                 ;point to the uart rec reg
                in      al,dx                   ;get the character
                cmp     al,xoff                 ;GIST: see if its an xoff
                je      gotxoff                 ;if not, continue checking
                cmp     al,xon                  ; is it an XON?
                je      gotxon                  ;
;                cmp     al,0ch                  ; is it a FF? %%%
;                je      gotff
                jmp     regchar

gotxoff:        mov     pcxoff,1
                mov     al,01h                  ; disable THRE interrupt
                mov     dx,uartintenable        ; leaving only RCA active
                out     dx,al
                jmp     serialinxit

gotxon:         mov     pcxoff,0                ; no xoff pending anymore
                cmp     quehead,0               ; see if xon/xoff waiting
                jne     enablethre              ; if so, enable THRE
                cmp     tocount,0               ; enable THRE only if there
                je      serialinxit             ; is something to send

enablethre:
                SET_IER         3                  ; enable THRE and RCA
                jmp     serialinxit

gotff:          call    serialempty             ; dump the buffer from host
                jmp     regchar                 ;otherwise, process as usual
                                                ; just fall through


;
;  We have a regular character, to be stored into the queue of chars
;  that have arrived from the host.
;
regchar:
;  Below 2 lines are insurance code: if everything else works ok,
;  we'll never encounter this situtation.  However, if they don't,
;  then we would rather drop chars on the floor immedtately than to
;  cause a wrap-around in the circular queue and plot things twice.
                cmp     buffercount,xbuflen     ; see jf queue completely full
                jge     serialinxit             ; if so, drop char on floor

                mov     bx,writepointer         ;get the buffer write painter
                mov     [bx],al                 ;store the character
                inc     writepointer            ;move to the next loc

                cmp     writepointer,offset buffertop
                jl      incbuffer

                mov     writepointer,offset serialbuffer

incbuffer:      inc     buffercount             ; and increment the char count
                cmp     buffercount,xbufmax     ; send xoff tf nearty full
                jl      serialinxit             ; otherwise, exit

;  the buffer is nearly full (above cutoff point): send an xoff
                cmp     hostxoff,1              ; see if we already did xoff
                je      serialinxit

                mov     quehead, xoff           ; send an xoff to the host
                mov     hostxoff,1              ; set hostxoff flag
                SET_IER         3               ; enable RCA and THRE
;                jmp     serialinxit            ; fall thru

serialinxit:
; This condition (another pending interrupt at the same time) does occur
; rarely so this code is functional.
                mov     dx, uartintident
                in      al, dx
                test    al, 1
                jnz     serialfinish
                jmp     serialagain

serialfinish:
                mov     al,24h                  ; a nonspecific eoi
                out     20h,al                  ; issue an eoi to the pic

                pop     ds
                pop     bx
                pop     dx
                pop     aX
                iret

sendclear:
;   We hope the code below is never used, because those interrupts
;   aren't enabled (we think.) However, just in case we get one,
;   we perform the following steps to clear that interrupt:
                mov     dx,uartmodemstatus
                in      al,dx                   ; reset modem status int
                mov     dx,uartlinestatus
                in      al,dx                   ; reset line status int
                jmp     serialinxit

; GIST - routine returns current input buffer lenth

seriallen:      mov     ax,buffercount
                pop     ds
                iret

serialempty     PROC    near
;               reset all from-the-host queue pointers back to empty position
                mov     writepointer,offset serialbuffer
                mov     readpointer,offset serialbuffer
                mov     buffercount,0           ;clear count of chars in queue
                ret
serialempty     ENDP

quetohost       PROC    near                    ; queue byte in al to the hest
                mov     bx,towrite              ; get pointer into to-host queue
                mov     [bx],al                 ; store byte in host queue
                inc     towrite
                cmp     towrite,offset tohosttop
                jl      incto
                mov     towrite,offset tohostbuf

incto:          inc     tocount
                cmp     hostxoff,1              ; see if xoff pending
                je      nostart                 ; if so, don't start interrpt

                cmp     tocount,1               ; 1st char into the queue?
                jne     nostart                 ; then it was already enabled

                SET_IER         3               ; enable THRE and RCA ints
                out    dx,al                    ; Send the character

nostart:
                ret
                cmp     tocount,tolen
                jl      quedone

towait:
                sti                             ; Make sure interrupts enabled
                cmp     tocount,tolen           ; if queue to host ts full, THEN
                je      towait                  ; we'll wait till it empties
quedone:        ret

quetohost       ENDP

serial_text     ends

;************************************************************************
;*                                                                      *
;* MEMORY DEFINITIONS                                                   *
;*                                                                      *
;************************************************************************

serial_data           segment


                even
baudtable       dw      417h            ;110 baud
                dw      300h            ;150
                dw      180h            ;300
                dw      0c0h            ;600
                dw      060h            ;1200
                dw      030h            ;2400
                dw      018h            ;4800
                dw      00ch            ;9600

setupword       label word
                dw      ?
readpointer     label word
                dw      ?
writepointer    label word
                dw      ?
buffercount     label word
                dw      ?
toread          label word
                dw      ?
towrite         label word
                dw      ?
tocount         label word
                dw      ?
systemvector    label word
                dd      ?
comsavvector    label word
                dd      ?


                even
hostxoff        db      0             ; 1 if xoff has been sent to host

                even
pcxoff          db      0             ; 1 if this pc has received an xoff

                even
quehead         db      0             ; Non-zero means transmit this char first

                even
serialbuffer    label word
                db      xbuflen dup (?)
buffertop       label word

                even
tohostbuf       label word
                db      tolen dup (?)
tohosttop       label word

serial_data           ends

serial_text     segment

;************************************************************************
;*                                                                      *
;* INITIALIZATION CODE - WILL NOT BE SAVED WHEN TERMINATED              *
;*                                                                      *
;************************************************************************
;
; SERIALINIT initializes the buffer pointers and counter, grabs the old
; int 14 vector from 0000:0030 and replaces it with the origin address
; of this program, and unmasks the uart interrupt line on the PIC.
;
serialinit:     call    serialempty
                mov     toread,offset tohostbuf
                mov     towrite,offset tohostbuf
                mov     tocount,0

;
; get the old int 14 vector and steal it
;
                push    es
                mov     ax,0
                mov     es,ax

                mov     ax,es:int14vector       ;this is the offset
                mov     bx,es:int14vector+2     ;this is the segment
                pop     es
                mov     systemvector,ax         ; save original locations
                mov     systemvector+2,bx
;
; install our entry address as the new int 14 vector
;
                mov     ax,cs
                mov     dx,ax
                mov     bx,offset door
                push    es
                mov     ax,0
                mov     es,ax
                mov     es:int14vector,bx       ;this is the offset
                mov     es:int14vector+2,dx     ;this is the segment
                pop     es
;
; install our uart service routine address
;
                push    es
                mov     ax,0
                mov     es,ax
                mov     ax,es:commvector       ;this is the offset
                mov     bx,es:commvector+2     ;this is the segment
                pop     es
                mov     comsavvector,ax        ; save original locations
                mov     comsavvector+2,bx

                mov     ax,cs
                mov     dx,ax
                mov     bx,offset serialinput
                push    es
                mov     ax,0
                mov     es,ax
                mov     es:commvector,bx        ;is the offset
                mov     es:commvector+2,dx      ;is the code segment
                pop     es
;
; empty the usart receive buffer
;
                mov     dx,03f8h                ;address the usart
                in      al,dx
                in      al,dx                   ;empty the uart receive buffer
;
; turn the 8250 interrupts on and unmask the usart line to the pic
;
;
        cli

                mov     dx,uartlinecontrol
                mov     al,80h                  ;set dlab 1 for divisor load
                out     dx,al
;
                mov     dx,uartdivisorlsb
                mov     al,0ch                 ;lo byte of 9600 baud
                out     dx,al
;
                mov     dx,uartdivisormsb
                mov     al,0                    ;hi byte of 9600 baud
                out     dx,al
;
; Now set to 9600 baud... reset diab, and set up rest of usart
;
                mov     dx, uartlinecontrol
                mov     al ,03h                  ;8 bit no parity 1 stop
                out     dx,al
;
                mov     dx,uartmodemcontrol
                mov     al,0fh                  ;enable interrupt line
                out     dx,al
;
                mov     dx,uartintenable
                mov     al,01h                  ;interrupt on char input
                out     dx,al
;
; set up the pic
;
                in      al,picmask
                and     al,uartmask
                out     picmask,al
        sti

                pop     ds
                pop     si
                pop     di
                mov     sp,bp
                pop     bp
                ret


_serial         endp

serial_text     ends




serial_text     segment

                public _unserial


; restore original int 14 vector


_unserial       proc    far
                push    bp
                mov     bp,sp
                push    di
                push    si

                push    ds


                mov     ax,seg serial_data
                mov     ds,ax
                push    es
                mov     ax,0
                mov     es,ax

        cli

                mov     ax,systemvector         ; restore original locations
                mov     bx,systemvector+2
                mov     es:int14vector,ax       ;this is the offset
                mov     es:int14vector+2,bx     ;this is the segment
                mov     ax,comsavvector         ; restore original locations
                mov     bx,comsavvector+2
                mov     es:commvector,ax       ;this is the offset
                mov     es:commvector+2,bx     ;this is the segment

        sti

                pop     es

                pop     ds
                pop     si
                pop     di
                mov     sp,bp
                pop     bp
                ret


_unserial       endp

serial_text     ends


                end
