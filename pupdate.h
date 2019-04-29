;
; PUPDATE.H - Copyright (C) by A. Karttunen, July 1992, July 1993.
;
; Enable 386 instructions:
        .386
; Include the special macros:
        include pupmacro.h
;
; The main routine for updating the world in pixel mode.
; Implemented as a macro GEN_PUPDATE with various arguments, which
; define what is computed and how.
;
; These are just templates, which are replaced by real values with fillind
; function:
; These can be arbitrary values as long as BYWIDTH is not confused with N,
; SRC+BYWIDTH and possibly + or - N is never DST, nor SRC itself.
; And all the resulting index templates should be at least greater than
; 255., so that word instead of byte is allocated for index.
;
DST     EQU     1000h
SRC     EQU     9000h
BYWIDTH EQU     80
;
;
;dataseg segment para public 'data'
        .DATA
        extrn _alive:word, _born:word, _died:word, _maxcol:word, _maxrow:word
	extrn _cell_chars:byte ; This is a string.
	extrn _print_all:byte ; This is one byte flag.
	extrn _extent_checking:byte ; This is one byte flag.
	extrn __revbits:byte ; These are byte tables.
	extrn _tables:word  ; Actually this is a pointer.
        extrn _btables:dword
	extrn _vidcols:word, _vidlim:word, _vidlines:word, _vidseg:word
	extrn _vidstart:word, _vid2start:word, _orgvid2start:word
        extrn _vidwidth:word, _bytewidth:word, _bywidth:word
        extrn _silimit:word, _orgsilimit:word, _anychanges:word
	extrn _cga1kludge:word,_cga2kludge:word
	extrn _orgcga1kludge:word,_orgcga2kludge:word
	extrn _p_column:word
        extrn _minextrow:word,_maxextrow:word,_minextcol:word,_maxextcol:word
        extrn _collimit:word,_vcollimit:word
; Actually, alive, s46, c46, anychanges and colchanges should be defined as
;  doublewords in elo.c
;
;dataseg ends
;
; Here is the GEN_PUPDATE itself.
;
; Arguments are:
;
; FUNNAME     - Name of the updating function itself. Also, an array will be
;                created with the same name prefixed with the character '_'
;                and the initialization routine, prefixed with the letter 'i'.
;
; FREDKIN     - If 1 then Fredkin's self-replicating CA rules are used.
;               If 0, then use Conway's Life rules.
;
; I386        - If 1 then double word instructions and other 386 code is used,
;                so that 32 cells are computed each time in the inner loop,
;                instead of the default 16. (This must be 0 or 1!)
;
; EXTENTCHECK - If nonzero, then we keep the track of the outermost
;                changed cells.
;
; COUNTLIVING - If nonzero then the living cells are counted to _alive
;
; CHECKVIDLIM - Currently not used.
;
;
GEN_PUPDATE MACRO FUNNAME,FREDKIN,I386,EXTENTCHECK,COUNTLIVING,CHECKVIDLIM
        local   loop11,loop22,creep_in,output_cells,skip_output
	local   count_bits,break11,finis,back_to_loop,back2to_loop
	local   code1,code2,code3,code4,code5,code6,code7,code8
	local   code9,code10,code11,vcode,loppu,hamy,kumpala,kampala
        local   kundali,kundali2 ; Kundali the Snake.
	local   hotcode,cmpcode,savejmp,C_save1jmp,C_save2jmp,putcmp,skip123
	local   N,INDOFF
        local   back3to_loop,back123,back456,skip456,skip789,skipABC,skip_them
        local   skip_to_died,skip_all_cells
	local   do_the_kludge,setmaxextrow,setminextrow
;
I386CODE =      I386
N       EQU     (2+(2*I386CODE)) ;Width of word size used in bytes (4 in 386)
INDOFF  EQU     2                ;How many bytes the index templates are
;                                ; from the beginning of instruction?
;
; Pointers to those locations in code which keep the base addresses to the
;  old and new world:
;
;dataseg segment para public 'data'
        .DATA
        public  __&FUNNAME
__&FUNNAME DW    code11+INDOFF
        DW      code1+INDOFF+I386CODE
	DW      code2+INDOFF
	DW      code3+INDOFF
	DW      code4+INDOFF+I386CODE
        DW      code7+INDOFF+I386CODE
	DW      code8+INDOFF
	DW      code9+INDOFF
	DW      code10+INDOFF+I386CODE
	DW      code5+INDOFF
	DW      code6+INDOFF
        DW      0               ; End marker.
savejmp DW      0
;
;dataseg ends
;
; iFUNNAME(flag)
; int flag;
;
; If flag is non-zero, then jmp instruction is put back to hotcode location,
; or is kept intact there.
; If flag is zero, then jmp instruction is saved to savejmp (if not saved
; already there), and it's replaced by CMP R0,R4 instruction.
;
; I.e. when this is called with non-zero argument, the updating function
; will print all the cells, regardless whether they have changed from
; the previous generation.
; When this is called with zero argument, then the updating function will
; output only changed cells.
;
        .CODE
C_save1jmp DW   0
C_save2jmp DW   0
        public  _i&FUNNAME
_i&FUNNAME proc near
        push    bp
	mov     bp,sp
	push    ax
;
	cmp     word ptr [bp+4],0 ; If the argument
	jz      putcmp          ; is zero, then go to put the CMP instruction.
	mov     ax,savejmp      ; Otherwise, check whether there's already
	test    ax,ax           ;  JMP instruction saved to savejmp.
	jz      hamy ; If not it is still in its place, so do hamy things...
	mov     cs:hotcode,ax   ; Otherwise, put savejmp's contents to hotcode
	jmp     short loppu     ; And return.
putcmp:
        cmp     savejmp,0       ; If savejmp instruction is still zero ?
	jnz     skip123
	mov     ax,cs:hotcode   ; Then save the jump instruction
	mov     savejmp,ax      ;  there.
skip123:
        mov     ax,cs:cmpcode   ; Copy CMP instruction from cmpcode
if      I386CODE ; If 386 code, then put longprefix first
	mov     cs:byte ptr hotcode,longprefix
        mov     cs:hotcode+1,ax ; & CMP instruction after that.
else
	mov     cs:hotcode,ax   ;  to hotcode.
endif
loppu:
        pop     ax
	pop     bp
	ret
hamy: ; Copy original jump code + something to C_save2jmp:
        mov     ax,cs:hotcode
        add     ah,(skip_output-output_cells) ; Increment jump offset so that
        mov     CS:C_save2jmp,ax ; it skips the screen updating instruction...
        jmp     short loppu
cmpcode label   word
	cmp     R0W,R4W
_i&FUNNAME endp
;
; The updating function itself.
;
; Register usage:
;
;  SI - Index to the world, going column by column.
;  DI - "Correction constant" which when added to SI (see vcode) produces
;        the proper offset to CGA's perverted video memory.
;
; In the inner loop (loop11), when the Life algorithm is used:
;
;  R0 - Level 0 bits of the sum of the eight neighbours are collected to this.
;       (and in the end of the loop keeps the value of new central cells)
;  R1 - Level 1 bits (i.e. first level carries) of the sum of 8 neighbours.
;  R2 - Level 2 & 3 bits (i.e. second & third level carries ored together) of
;   the sum are collected to this, *inverted* (It is slightly faster that way).
;  R3 - Miscellaneous intermediate results.
;  R4 - In the end of the loop contains the old central cells.
;
; The code is modified rapidly, flexibly and plastically.
;
        public  _&FUNNAME
_&FUNNAME proc  near
;
;
; Push almost all general registers in alphabetical order.
if      I386CODE
        pushad                  ; With >=386 we can use this instruction.
        push    fs
else
        push    ax
        push    bp
	push    bx
	push    cx
        push    di
	push    dx
	push    si
endif
	push    es              ; Also ES segment register.
        push    ds              ; This must be pushed last.
;
        mov     ax,_vidseg      ; Initialize ES
	mov     es,ax           ;  to point to the video memory.
	mov     si,_p_column    ; Start from the value defined in _p_column
        call    _initCSvars     ; This keeps SI intact.
if      I386CODE
; In this case the offsets of btables should be equal...
        mov     ax,word ptr _btables+6 ; Get the segment of btable[1]
        mov     fs,ax           ; Put it into FS.
endif
        mov     ax,word ptr _btables+2 ; Get the segment of btable[0]
; (THIS MUST BE EQUAL to seg of btable[1] if I386CODE not in use)
        mov     ds,ax           ; Put it into DS.
if      EXTENTCHECK
        lea     ax,cs:do_the_kludge
	mov     CS:C_kludgeadr,ax ; Set this initially to point do_the_kludge
endif
	cmp     si,CS:C_vcollimit ; Is _p_column >= _vcollimit ?
        jb      loop11
        call    kundali         ; Then jump to do some selfmods (possibly...)
loop11:
        mov     ax,CS:C_vid2start
        mov     cs:vcode+INDOFF+I386CODE,ax ;
	mov     di,CS:C_cga2kludge
	mov     CS:C_di_save,di
        mov     di,CS:C_cga1kludge
;
if      EXTENTCHECK
        mov     word ptr CS:C_colchanges,0   ; Clear colchanges variable.
        mov     word ptr CS:C_colchanges+2,0 ; High word too!
endif
;
; First get the cells from first (actual) row:
code1   label   word
	_mov    R4,[SI+SRC+BYWIDTH]   ; Get the cells one bit below.
code2   label   word
	mov     R2L,[SI+SRC+BYWIDTH+N] ; Get the cells "SE" from central cells.
	_mov    R3,R4           ; Copy of R4 to R3.
	myrcl   R3,R2L          ; Rotate R3 left, bit-7 of R2L going righmost.
code3   label   word
	mov     R2L,[SI+SRC+BYWIDTH-1] ; Get the cells "SW" from central cells.
	ror     R2L,1           ; Get the bit-0 of those to carry.
	_mov    R2,R4           ; Copy of R4 to R2.
	myrcr   R2              ; Rotate R2 right, bit-0 of R2L going leftmost.
;
if      FREDKIN
        _mov    R0,R2
	_xor    R0,R3
else
        add3bit R2,R3,R4        ; Add those "SW" and "SE" cells.
        _push   R2              ; Push sum bits,
	_push   R3              ;  and carries to stack, for later use!
endif
;
; Then get the cells from zeroth row, the wrap row:
code4   label   word
	_mov    R4,[SI+SRC]     ; Get the cells in center.
if      (FREDKIN eq 0)
code5   label   word
	mov     R0L,[SI+SRC+N]  ; Get the cells from the column one right.
	_mov    R1,R4           ; Copy of R4 to R1.
	myrcl   R1,R0L          ; Rotate R1 left, bit-7 of R0L going righmost.
code6   label   word
	mov     R0L,[SI+SRC-1]  ; Get the cells from one column left of it.
	ror     R0L,1           ; Get the bit-0 of those to carry.
	_mov    R0,R4           ; Copy of R4 to R0.
	myrcr   R0              ; Rotate R0 right, bit-0 of R0L going leftmost.
        add3bit R0,R1,R2        ; Add those left and right cells.
endif
        add     si,CS:C_bywidth ; One row downward.
	jmp     creep_in
;
loop22:
;
; When coming to here it's subbozoed that R4 contains the old central
; cells, memory locations _s46 and _c46 keep the sum & carry bits of neighbours
; to left and right of it, and in the stack should be sum (second topmost)
; and carries (topmost) of the old neighbours 7 and 9.
;
; That applies for the Life algorithm. With Fredkin there is nothing in
; stack, R4 should contain the old central cells (now cells to the north),
; and R3 should contain the old SW & SE (now W & E) cells xored together.
;
if      FREDKIN
        _mov    R0,R3           ; Get old SW & SE cells.
else
        CS_prefix
	_mov    R0,C_s46        ; Start summing bits to R0
        CS_prefix
	_mov    R1,C_c46        ;  and first level carries to R1.
endif
creep_in:
if      FREDKIN
        _xor    R0,R4           ; Xor old central cell (now N cell)
else
	add3bit R0,R4,R2        ; Add also the old central cells, so now R0,R1
	_or     R1,R4           ;  contains sum of the neighbours 1+2+3.
endif

code7   label   word
	_mov    R4,[SI+SRC+BYWIDTH]   ; Get the cells one bit below.
code8   label   word
	mov     R2L,[SI+SRC+BYWIDTH+N] ; Get the cells "SE" from central cells.
	_mov    R3,R4           ; Copy of R4 to R3.
	myrcl   R3,R2L          ; Rotate R3 left, bit-7 of R2L going righmost.
;
code9   label   word
	mov     R2L,[SI+SRC+BYWIDTH-1] ; Get the cells "SW" from central cells.
	ror     R2L,1           ; Get the bit-0 of those to carry.
	_mov    R2,R4           ; Copy of R4 to R2.
	myrcr   R2              ; Rotate R2 right, bit-0 of R2L going leftmost.
;
if      FREDKIN
        _xor    R3,R2           ; Get "sum" of cells to left & right to R3.
	_xor    R0,R4           ; Sum the "S" cell to R0.
else
        addbits R2,R3           ; Add those "SW" and "SE" cells.
;
        CS_prefix
        _pop    C_c46           ; Get the carries & sum bits of the old (7+9)
        CS_prefix
	_pop    C_s46           ;  saved in stack in previous iteration.
;
        _push   R2              ; And save sum & carries of (SW+SE) to stack,
	_push   R3              ;  for the next iteration in this loop.
;
	addbits R2,R4
	_or     R3,R4           ; R2,R3 = Neighbours 7+8+9
        add3bit R0,R2,R4        ; R0,R2 = Neighbours (1+2+3)+(7+8+9)
        add2bit R1,R2           ; Add previous carries to level 1 carries.
	add2bit R1,R3           ; Also add carries from S79+S8
	_and    R2,R3           ; Add carries of that ^ to second level (R2)
        CS_prefix
	_mov    R3,C_s46        ; Get sum of cells left and right.
        add3bit R0,R3,R4        ; Add sum of 4+6 to R0.
        CS_prefix
	_mov    R4,C_c46        ; Get carries of cells left and right.
	add2bit R1,R3           ; Add carries of 4+6 that to 1st level carries.
	add2bit R1,R4           ; Also carries of that ^ to R1.
	_and    R2,R3           ; And or carries of those to
	_and    R2,R4           ;  second & third level carries (R2).
endif
;
code10  label   word
        _mov    R4,[SI+SRC]     ; Get the central cells themselves.
if      (FREDKIN eq 0)
	_or     R0,R4           ; Or them to sum bits.
	_and    R0,R2           ; Neighbourhood of >= 4 causes death.
	_and    R0,R1           ; Neighbourhood of >= 2 is a necessity.
endif
code11  label   word            ; lea is used instead of move, so we don't
        lea     R1W,[SI+DST]    ; need code12. (R1W is BX so this should work)
if      I386CODE                ; The destination table is in separate segment
        mov     R2D,FS:[R1W]    ; Get the cells of the generation two cycles
else
        _mov    R2,[R1W]        ; Get the cells of the generation two cycles
endif
	_xor    R2,R0           ; before this one, and xor new cells to them.
        CS_prefix
	_or     C_anychanges,R2 ; Or them to anychanges variable.
if      I386CODE                ; The destination table is in separate segment
        mov     FS:[R1W],R0D    ; Get the cells of the generation two cycles
else
	_mov    [R1W],R0        ; Store new cells to destination.
endif
hotcode label   word
	jmp     short output_cells ; This is later overwritten by that cmp:
if      I386CODE
        nop                     ; Must make room for the long prefix with this.
endif
;	cmp     R0,R4           ; If new cells different from the previous gen.
	jne     output_cells    ; Then goto output them.
back_to_loop:
if      COUNTLIVING
if      (EXTENTCHECK eq 0)
        _test   R0,R0           ; If R0 nonzero, i.e. there is some bits,
	jnz     count_bits      ;  then go to count them and add to _alive
endif
endif
back2to_loop:
        add     CS:vcode+INDOFF+I386CODE,DI ; Add correction constant to vcode.
        xchg    DI,CS:C_di_save ; And swap the "correction constants".
;
        add     SI,CS:C_bywidth ; One row downward.
        cmp     SI,CS:C_silimit
	jae     break11
	jmp     loop22
break11:;

if      (FREDKIN eq 0)          ; With Fredkin's algorithm nothing was pushed.
        add     sp,(N*2)        ; Pop sum & carries of the (7+9) off the stack
endif
if      EXTENTCHECK
	jmp     CS:[C_kludgeadr] ;Jmp to code which sets the min- or maxextcol
back3to_loop:                   ; (it is either do_the_kludge or skip789)
endif
        mov     si,CS:C_p_column
	add     si,N
	cmp     si,CS:C_collimit ; Is _p_column >= _collimit ?
	jae     finis           ; If it is, then the work is done.
	cmp     si,CS:C_vcollimit ; Is _p_column >= _vcollimit ?
        jb      kumpala
        call    kundali         ; Then jump to do some selfmods (possibly...)
;
kumpala:
	mov     CS:C_p_column,si
	jmp     loop11
finis:
        mov     ax,CS:C_save1jmp ; Check whether there was hotcode instr saved
        test    ax,ax
        jz      kampala         ; If not...
        mov     CS:hotcode,ax   ; Restore orig. hotcode instruction
        xor     CS:C_save1jmp,ax ; And clear the C_save1jmp location.
kampala:
        pop     ds              ; Must do this before calling _exportCSvars
        call    _exportCSvars
;
; Pop registers.
	pop     es
if      I386CODE
        pop     fs
        popad
else
        pop     si
	pop     dx
	pop     di
	pop     cx
	pop     bx
	pop     bp
	pop     ax
endif
;
        ret                     ; And return back
;
output_cells:
        cmp     SI,CS:C_vidlim  ;If going out of screen (below the lower edge)
        jae     skip_output     ; then skip the next instruction.
        ES_prefix
vcode   label   word
        _mov    [SI+1234h],R0   ; 1234 is overwritten with proper offset.
skip_output:
;
if      EXTENTCHECK
        _mov    R1,R4           ; Get the copy of old cells to R1
	_xor    R1,R0           ;  Xor new cells to them, to get the changes.
if      COUNTLIVING
	jz      back2to_loop    ; If no changes, then go to back to loop.
;       jz      skip_them
else
	jz      back_to_loop    ; If no changes, then jump back to loop.
endif
        CS_prefix
	_or     C_colchanges,R1 ; Or all the changes of one column together.
        cmp     si,CS:C_maxextrow ; If si is above the current value of
	ja      setmaxextrow    ;  _maxextrow, then update that.
back123:
        cmp     si,CS:C_minextrow ; If si is below the current value of
	jb      setminextrow    ;  _minextrow, then update that.
back456:
;
	mov     CS:C_maxextcol,SI ; _maxextcol keeps the maximum column value.
skip_them:
endif
;
if      COUNTLIVING
if      (EXTENTCHECK eq 0)
        _test   R0,R0
        jz      back2to_loop    ; If R0 nonzero, i.e. there is some bits,
; If relative jump out of range:
;       jnz     count_bits      ;  (so replaced by these
;       jmp     back2to_loop    ;                        two lines)
endif
else                            ; then fall thru this, to count the bits.
	jmp     back_to_loop
endif
;
count_bits:
if      COUNTLIVING
if      EXTENTCHECK
	_push   R1              ; Save R1 to stack if extent checking used.
        _mov    R2,R1           ; But save also to R2.
endif
        lea     bx,CS:C_cntbits ; BX is R1, so it's corrupted. And R0 is AX
if      EXTENTCHECK
        _and    R0,R2           ; Get the born cells to R0.
        jz      skip_to_died    ; If 0 cells born skip count the died cells.
endif
        CS_prefix
        xlatb                   ; AX. So get count of bits in R0L to R0L (AL)
	xchg    al,ah           ; Swap bytes of R0 (AX)
        CS_prefix
	xlatb                   ; Index with orig. high byte.
if      I386CODE
        mov     cx,ax           ; CX should be R2 and free for use.
	_ror    ax,10h          ; Swap the words in EAX.
        CS_prefix
        xlatb                   ; Do the same
	xchg    al,ah           ;  things with
        CS_prefix
	xlatb                   ;   the high bytes of EAX.
	add     ax,cx           ; Add bytes in "parallel" way.
	add     al,ah           ; Add halves together.
	cbw                     ; Clear AH. (Would XOR AH,AH be any faster?)
	cwde                    ; Clear upper word of the EAX.
if      EXTENTCHECK
        CS_prefix
	_add    C_born,R0       ; Add EAX (= R0) to _born count.
else
        CS_prefix
	_add    C_alive,R0      ; Add EAX (= R0) to _alive count.
endif
else   ;no I386CODE
	add     al,ah           ; Add bitcount of low byte to bitcount of high.
	xor     ah,ah           ; And clear high byte.
if      EXTENTCHECK
	add     CS:C_born,ax    ; Add that sum to born count. (low word)
	adc     CS:C_born+2,0   ; Add carry to high word.
else
	add     CS:C_alive,ax   ; Add that sum to alive count. (low word)
	adc     CS:C_alive+2,0  ; Add carry to high word.
endif
endif  ;I386CODE
;
; If extent checking used, then we must count dead cells also:
if      EXTENTCHECK
skip_to_died:
        _pop    R0              ; Get the old changed cells to R0
        _and    R0,R4           ; AND with the old cells to get the died ones
        jz      skip_all_cells  ; If 0 cells died skip counting.
        CS_prefix
        xlatb                   ;  to AX. So get count of bits in AL to AL.
	xchg    al,ah           ; Swap bytes of R0 (AX)
        CS_prefix
	xlatb                   ; Index with orig. high byte.
if      I386CODE
        mov     cx,ax           ; CX should be R2 and free for use.
	_ror    ax,10h          ; Swap the words in EAX.
        CS_prefix
        xlatb                   ; Do the same
	xchg    al,ah           ;  things with
        CS_prefix
	xlatb                   ;   the high bytes of EAX.
	add     ax,cx           ; Add bytes in "parallel" way.
	add     al,ah           ; Add halves together.
	cbw                     ; Clear AH. (Would XOR AH,AH be any faster?)
	cwde                    ; Clear upper word of the EAX.
        CS_prefix
	_add    C_died,R0        ; Add EAX (= R0) to _alive count.
else   ;no I386CODE
	add     al,ah           ; Add bitcount of low byte to bitcount of high.
	xor     ah,ah           ; And clear high byte.
	add     CS:C_died,ax    ; Add that sum to born count. (low word)
	adc     CS:C_died+2,0   ; Add carry to high word.
endif  ;I386CODE
skip_all_cells:
endif  ;EXTENTCHECKING
	jmp     back2to_loop
endif  ;COUNTLIVING
;
if      EXTENTCHECK
setmaxextrow:
	mov     CS:C_maxextrow,SI
        jmp     back123
setminextrow:
	mov     CS:C_minextrow,SI
        jmp     back456
;
do_the_kludge:
        cmp     CS:C_maxextcol,0
	jne     skip456
	jmp     back3to_loop    ; If maxextcol is still zero, then do nothing.
skip456:
	lea     ax,cs:skip789   ; Change kludgeadr to
	mov     CS:C_kludgeadr,ax ;   jump directly to skip789
	mov     ax,CS:C_maxextcol ; And move maxextcol to minextcol.
	mov     CS:C_minextcol,ax
	test    byte ptr CS:C_colchanges,80h ; Check whether the highest bit
; of the leftmost byte is set. (I.e. the leftmost bit). If it is...
	jz      skip789
	sub     CS:C_minextcol,N ; Then subtract _minextcol to previous column.
skip789:
        test    byte ptr CS:C_colchanges+(N-1),1 ; Check whether the lowest
; bit of the rightmost byte is set (I.e. the rightmost bit). If it is...
	jz      skipABC
	add     CS:C_maxextcol,N ; Then add _maxextcol to the next column.
skipABC:
	jmp     back3to_loop
endif
;
;
kundali:
        cmp     CS:C_save1jmp,0 ; Check whether we have already done this?
        jnz     kundali2        ; If yes, then don't do it twice...
; If it is, then do some hairy self modification again:
        mov     ax,CS:hotcode   ; Save the current instruction of hotcode
        mov     CS:C_save1jmp,ax ;  into C_save1jmp
        mov     ax,CS:C_save2jmp ; Get the incremented jump from C_save2jmp
        mov     CS:hotcode,ax   ; and insert it into hotcode (which now skips
; the video updating, because column is out of screen)
kundali2:
        ret                     ; Jump back.
;
if      FREDKIN
; If Fredkin's algorithm in use, then these are just locations needed
;  for consistency, although never executed.
code5   label   word
	mov     R0L,[SI+SRC-1]  ; Get the cells from one column left of it.
code6   label   word
	mov     R1L,[SI+SRC+N]  ; Get the cells from the column one right.
endif
;
_&FUNNAME endp
        ENDM
;
