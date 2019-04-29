;
;   calling convention:
;
;       int getcpu();
;
;   returns:
;
;       tucked away neatly in your AX....
;
;       you get back   86 if an 8088/8086
;                     186 if an 80186/80188
;                     286 if an 80286
;                     386 if an 80386
;                      20 for a NEC V20/V30
;
;
;   REFERENCES:
;
;     getcpu() function is modified from cpu_type function
;     included in CHIPS.ASM file in CHIPS.LZH packet by
;     Pat Shea @Psi! CIS [76210,712]. You can find that packet
;     from garbo.uwasa.fi ftp-server (login: anonymous), from
;     some directory I don't currently remember. Only thing
;     I (A. Karttunen) changed from cpu_type is that now it
;     returns the processor type in AX, not in DX, and the
;     values are different. ndp_type I threw away, cause I
;     don't need it for anything. Also changed to be compilable
;     under Aztec C's assembler.
;
;     <And now changed for the Borland Turbo Assembler!>
;
;     _chips was made up of two PROC's, cpu_type and ndp_type.
;
;       cpu_type is based on uncopyrighted, published logic by
;         Clif (that's the way he spells it) Purkiser of Intel -
;         Santa Clara.
;
;       ndp_type is adopted from Ted Forgeron's article in PC
;         Tech Journal, Aug '87 p43.
;
;     In the event of subsequent republication of this function,
;       please carry forward reference to these two gentlemen as
;       original authors.
;
;       Copr. 1987      Pat Shea - Psi! (that Copr. is on there cuz my
;                                        lawyer sez I should, but feel
;                                        free to hack away!!!    pats.)
;
;       Update:   1/1/88 - changed this code slightly so that it is
;                          compilable using MASM 5.0, and the test.c
;                          file using MSC 5.0. <Albert Stein>
;
; This shit was for the Aztec-C 3.40's assembler, commented out:
;
;codeseg segment para public 'code'
;dataseg segment para public 'data'
;dataseg ends
;	assume  cs:codeseg,ds:dataseg,es:dataseg,ss:dataseg
;
        DOSSEG
        .MODEL  SMALL
        .CODE
	public  _getcpu
_getcpu proc    near
;
        push    DX              ; Save DX.
        pushf                   ; pump Ur flags register onto the stack
        xor     DX,DX           ; blow out Ur DX and AX to start off
        xor     AX,AX           ;   with a clean slate
        push    AX              ; put AX on the stack
        popf                    ; bring it back in Ur flags
        pushf                   ; try to set bits 12 thru 15 to a zero
        pop     AX              ; get back Ur flags word in AX
        and     AX, 0f000h      ; if bits 12 thru 15 are set then you got
        cmp     AX, 0f000h      ;   an Intel 8018x or a 808x or maybe even
        jz      dig             ;   a NEC V20/V30 ??? - gotta look more...
;
; OTHERWISE....
;   Here's the BIG one.... 'tells the difference between an 80286 and
;   an 80386 !!
;
        mov     AX, 07000h      ; try to set FLAG bits 12 thru 14
                                ;   - NT, IOPL
        push    AX              ; put it onto the stack
        popf                    ;   and try to pump 07000H into Ur flags
        pushf                   ; push Ur flags, again
        pop     AX              ;   and bring back AX for a compare
        and     AX,07000h       ; if Ur bits 12 thru 14 are set
        jnz     got386          ;   then Ur workin' with an 80386
        mov     AX, 286         ; save 286 in AX cuz it's an 80286
        jmp     SHORT CPUbye    ;   and bail out
;
got386: mov     AX, 386         ; save 386 in AX cuz it's an Intel 80386
        jmp     SHORT CPUbye    ;   and bail out

; here's we try to figger out whether it's an 80188/80186, an 8088/8086
;   or an NEC V20/V30 - 'couple of slick tricks from Clif Purkiser.....

dig:    mov     AX, 0ffffh      ; load up AX
        mov     CL, 33          ; HERE's the FIRST TRICK.... this will
                                ;   shift everything 33 times if it's
                                ;   8088/8086, or once for a 80188/80186!
        shl     AX, CL          ; on a shift of 33, all bits get zeroed
        jz      digmor          ;   out so if anything is left ON it's
                                ;   gotta be an 80188/80186
        mov     AX,186          ; save 186 in AX cuz it's an 80188/80186
        jmp     SHORT CPUbye    ;   and bail out

digmor: xor     AL,AL           ; clean out AL to set ZF
        mov     AL,40h          ; ANOTHER TRICK.... mul on an NEC duz NOT
        mul     AL              ;   effect the zero flag BUT on an Intel
        jz      gotNEC          ;   8088/8086, the zero flag gets thrown
        mov     AX,86           ; 86 into AX cuz it's an Intel 8088/8086
        jmp     SHORT CPUbye    ;   and bail out

gotNEC: mov     AX,20           ; it's an NEC V20/V30 so save 20 in AX

CPUbye: popf                    ; putchur flags back to where they were
        pop     dx
        ret                     ;   and go back to where you came from
                                ;   (i.e., ===>  calling function) with
                                ;    the CPU type tucked away in AX for
				;   future reference
_getcpu endp
;
;
;codeseg	ends
        end
