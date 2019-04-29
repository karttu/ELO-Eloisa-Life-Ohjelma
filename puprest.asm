;
;       assume cs:codeseg,ds:dataseg,es:dataseg,ss:dataseg
;codeseg segment para public 'code'
        DOSSEG
        .MODEL  SMALL
;
        include pupdate.h
;
        .CODE
;
; fillind(ptr) - must be called in initialization, after cell tables are
;  allocated.
; Fill indexes in locations in update function.
; Kind of "internal linker".
;
; ptr = [bp+4]
;
        public  _fillind  
_fillind proc    near
        push    bp
        mov     bp,sp
        push    ax              ; Be paranoid and save all the used registers.
        push    bx
        push    dx
        push    si
        pushf
        cld                     ; Direction for lodsw is plusward.
        mov     si,[bp+4]       ; Get pointer to pointers from the stack.
fi_loop:
        lodsw                   ; AX <= [SI], SI += 2
        test    ax,ax           ; Terminator NULL encountered?
        jz      fi_out          ; In that case, go out.
        mov     bx,ax
        mov     ax,cs:[bx]      ; Get the index template from code pointer.
        cmp     ax,DST          ; If it's DST ?
        jne     fi_skip1
        mov     ax,word ptr _btables+4 ; Then overwrite the index with 
        mov     cs:[bx],ax      ;  the address of the destination table.
        jmp     fi_loop         ; Continue looping.
fi_skip1: ; Else it's SRC, possibly with some more displacements. (BYWIDTH+/-N)
        mov     dx,word ptr _btables ; Get the address of the source table.
        cmp     ax,SRC          ; Is index exactly SRC ?
        je      fi_skip3
        sub     ax,SRC          ; If not, then subtract SRC.
        cmp     ax,(BYWIDTH-1)
        jl      fi_skip2        ; If index is of form -N or +N
        sub     ax,BYWIDTH      ; If there is also BYWIDTH, then sub that too.
        add     ax,_bywidth     ; And add the real width in bytes.
fi_skip2:
        add     dx,ax           ; possible +N or -N offset which is added to DX
fi_skip3:
        mov     cs:[bx],dx      ; Overwrite the index.
        jmp     fi_loop         ; And continue.
;
fi_out:
        popf
        pop     si
        pop     dx
        pop     bx
        pop     ax
        pop     bp
;
        ret                     ; And return back
_fillind endp
;
; swapind(srcptr,dstptr) - swap source & destination addresses in indexes.
; must be called between every generation if btables are allocated to
; same segment. (I.e. when 386 code is not used).
; When 386 code is used, then this should be called always when algorithm
; has been changed.
;
; srcptr = [bp+4]
; dstptr = [bp+6]
;
        public  _swapind 
_swapind proc    near
        push    bp
        mov     bp,sp
        push    ax              ; Be paranoid and save all the used registers.
        push    bx
        push    di
        push    dx
        push    si
;
        mov     si,[bp+4]       ; Get the source pointer to pointers.
        mov     di,[bp+6]       ; Get the destination pointer to pointers.
        mov     bx,[si+(2*4)]   ; Get pointer to plain source address.
        mov     ax,cs:[bx]      ; Get the current source address.
        mov     bx,[si]         ; Get pointer to src pointers' dest. address.
        mov     dx,cs:[bx]      ; And its contents.
        sub     dx,ax           ; Now we have DST-SRC in DX.
        mov     bx,[di]         ; Get pointer to dst pointers' dest. address.
        mov     cs:[bx],ax      ; Put SRC there.
;
sw_loop:
        add     si,2            ; Increment pointers
        add     di,2            ;  to point the next pointers.
        mov     bx,[si]         ; Get pointer from source pointers.
        test    bx,bx           ; Terminator NULL encountered?
        jz      sw_out          ; In that case, go out.
        mov     ax,cs:[bx]      ; Get the contents of that source index.
        add     ax,dx           ; Add (DST-SRC) to SRC, to get DST.
        mov     bx,[di]         ; Get pointer from dest. pointers.
        mov     cs:[bx],ax      ; Store DST to there.
        jmp     sw_loop         ; And continue.
;
sw_out:
;
        pop     si
        pop     dx
        pop     di
        pop     bx
        pop     ax
        pop     bp
;
        ret                     ; And return back
_swapind endp
;
;
;
; cgapworld(btables[0 or 1])
; Print cells from past or present table to video, in CGA's 640x200 mode:
;
; (Note: this should work also with PC98's straightforward videomemory
;  if correction variables cga1kludge and cga2kludge are set to proper
;  values.)
;
        public  _cgapworld
_cgapworld proc  near
; Push almost all registers in alphabetical order (probably not all needed.)
        push    bp
        mov     bp,sp
        push    ax
        push    bx
        push    cx
        push    di
        push    ds
        push    dx
        push    es
        push    si
        pushf
;
        cld                     ; Set direction plusward for the movsw.
        mov     ax,_vidseg      ; Initialize ES
        mov     es,ax           ;  to point to the video memory.
        mov     si,_bywidth     ; Start from upper left corner (Skip wrap row)
;       mov     ax,si           ; get (bywidth-2)/2 to AX to get word count.
;       shr     ax,1
;       dec     ax
        xor     ax,ax           ; Clear AX.
        mov     cx,_bytewidth   ; CX = min(bytewidth,vidwidth);
        cmp     cx,_vidwidth
        jbe     kuukuu
        mov     ax,cx           ; AX will be the "correction constant"
        mov     cx,_vidwidth
        sub     ax,cx           ; AX = (bytewidth - vidwidth);
kuukuu:
        shr     cx,1            ; Divide by two to get word count.
        push    cx              ; Save it into stack for later use.
        inc     si              ;  Skip the wrap byte at left edge.
        mov     di,_orgvid2start ; Get the destination pointer.
        add     di,si           ; Add (bywidth+1)
;       mov     bx,_orgcga1kludge ; Get the first correction constant,
;       add     bx,2            ;  and make a minor correction to it ;->
;       mov     dx,_orgcga2kludge ; The other one...
;       add     dx,2
; Above four lines replaced with these five lines:
        add     ax,2
        mov     bx,ax
        mov     dx,bx
        add     bx,_orgcga1kludge ; Get the first correction constant
        add     dx,_orgcga2kludge ; The other one...
;
        mov     cx,_orgsilimit  ; CX = min(orgsilimit,vidlim);
        cmp     cx,_vidlim
        jbe     karmastuoli
        mov     cx,_vidlim
karmastuoli:
        lds     bp,[bp+4]       ;Get seg (of base addr) to DS and offset to BP
        add     si,bp           ; And add to SI.
        add     bp,cx           ; Now BP contains the correct limit.
luuppi:
        pop     cx              ; Get the word count from stack.
        push    cx              ; Push it immediately back, for the next time.
        cli                     ; Clear interrupts for MOVSW.
        rep     movsw           ; Move CX words from DS:[SI++] to ES:[DI++]
        sti                     ; Enable interrupts again.
;       add     di,ax           ; Now AX is included in BX & DX (so comm. out)
        add     di,bx           ; Add correction constant to dest. pointer.
;                               ; LEA DI,[BX+DI] would also work here.
        xchg    bx,dx           ; Swap the correction constants.
        add     si,ax           ; Add AX-corr. const. to SI
; Also, now that 2 is already included in AX:
;       add     si,2            ; Skip the wrap bytes in source table.
        cmp     si,bp           ; If still below the limit,
        jb      luuppi          ;  then loop back.
        pop     cx              ; Pop the word count from stack.
;
; Pop registers.
        popf
        pop     si
        pop     es
        pop     dx
        pop     ds
        pop     di
        pop     cx
        pop     bx
        pop     ax
        pop     bp
;
        ret                     ; And return back
;
_cgapworld endp
;
;
        public _pwrap1lr  ; Wrap left and right edges. Normal one.
_pwrap1lr proc near
;
        push    ax
        push    bx
        push    ds
        push    dx
        push    si
;
        mov     bx,_bywidth
        mov     dx,_orgsilimit  ; Get orgsilimit+btables[0] to DX
;       mov     si,_btables     ; Get pointer to past table. Replaced by this:
        lds     si,_btables     ; Get far pointer to past table.
        add     dx,si           ; Add base offset of btables[0] to DX.
        add     si,bx           ; Start from first actual row.
loopxx:
        mov     al,[bx+si-2]    ; Get the righmost byte in this row.
        mov     [si],al         ; And put it to column 0, i.e. wrap column.
        mov     al,[si+1]       ; Get the leftmost byte from column 1.
        mov     [bx+si-1],al    ; And put it to right wrap column.
        add     si,bx           ; Increment to next row.
        cmp     si,dx           ; Continue so long as si < btables+orgsilimit
        jb      loopxx
;
        pop     si
        pop     dx
        pop     ds
        pop     bx
        pop     ax
        ret
;
_pwrap1lr endp
;
;
        public _pwrap2lr  ; Wrap left and right edges. Twisted one.
_pwrap2lr proc near
;
        push    ax
        push    bx
        push    di
        push    ds
        push    dx
        push    si
;
        mov     bx,_bywidth
        mov     dx,_orgsilimit  ; Get orgsilimit+btables[0] to DX
        lds     si,_btables     ; Get far pointer to past table.
        add     dx,si           ; Add base offset of btables[0] to DX.
        mov     di,dx           ; Also to destination pointer.
        sub     di,bx        ; Set DI to -> left wrap byte of last actual row.
        add     si,bx           ; Start from first actual row.
loopyy:
        mov     al,[bx+si-2]    ; Get the righmost byte in this row.
        mov     [di],al         ; And put it to column 0, i.e. wrap column.
        mov     al,[si+1]       ; Get the leftmost byte from column 1.
        mov     [bx+di-1],al    ; And put it to right wrap column.
        add     si,bx           ; Increment SI to next row.
        sub     di,bx           ; Decrement DI to previous row.
        cmp     si,dx           ; Continue so long as si < btables+orgsilimit
        jb      loopyy
;
        pop     si
        pop     dx
        pop     ds
        pop     di
        pop     bx
        pop     ax
        ret
;
_pwrap2lr endp
;
        public _pwrap1tb  ; Wrap top and bottom rows. Normal one.
_pwrap1tb proc near
;
        push    ax
        push    bx
        push    cx
        push    di
        push    ds
        push    es
        push    si
        pushf
;
        cld                     ; Set direction plusward for rep movsw.
        mov     cx,_bywidth     ; Get the count to CX.
; Modification from near pointers (16 bit) to far pointers (32 bit, seg+off)
; 29-JUL-93. The following three lines were replaced...
;       mov     di,_btables     ; Set destination to zeroth, i.e. top wrap row
;       mov     si,di           ; And set
;       add     si,_orgsilimit  ;  source to be
; by these three lines:
        mov     si,_orgsilimit  ; 
        lds     di,_btables     ; Set destination to zeroth, i.e. top wrap row
        add     si,di           ; And set source to be
;
        sub     si,cx           ;   the last actual row.
        shr     cx,1            ; Divide CX by 2 to get word count.
        push    cx              ; Save it for later use.
;
        push    ds              ; ES = DS
        pop     es
        rep     movsw           ; Copy from DS:[SI] to ES:[DI]
;
        xchg    si,di           ; Exchange src & dst pointers.
        pop     cx              ; Get the old count from stack.
        rep     movsw           ; And wrap again, this time from top to bottom.
;
        popf
        pop     si
        pop     es
        pop     ds
        pop     di
        pop     cx
        pop     bx
        pop     ax
        ret
;
_pwrap1tb endp
;
;
        public _pwrap2tb  ; Wrap top and bottom rows. Twisted one.
_pwrap2tb proc near
;
        push    ax
        push    bx
        push    cx
        push    di
        push    ds
        push    es
        push    si
        pushf
;
        push    ds
        pop     es              ; Copy original DS to ES.
        cld                     ; Set direction plusward for lodsb.
        lea     bx,__revbits    ; Get the byte-reversion table address to BX.
        mov     cx,_bywidth     ; Get the count to CX.
; Modification from near pointers (16 bit) to far pointers (32 bit, seg+off)
; 29-JUL-93. The following three lines were replaced...
;       mov     di,_btables     ; Set destination to zeroth, i.e. top wrap row
;       mov     si,di           ; And set
;       add     si,_orgsilimit  ;  source to be
; by these three lines:
        mov     si,_orgsilimit  ; 
        lds     di,_btables     ; Set destination to zeroth, i.e. top wrap row
        add     si,di           ; And set source to be
;
        sub     si,cx           ;   the last actual row.
        add     di,cx        ; Set DI point to last (+1) byte of top wrap row.
        push    cx              ; Save count for later use.
loop11: ; First copy the last bottom row to top wrap row:
        dec     di
        lodsb                   ; Get byte from DS:[SI++] to AL
        ES_prefix
        xlatb                   ; Reverse it.
        mov     [di],al         ; And store to destination wrap row.
        loop    loop11
;
        pop     cx              ; Get the old count from stack.
        xchg    si,di           ; Exchange src & dst pointers.
        add     si,cx           ; And make some
        add     di,cx           ;  corrections to them.
loop22: ; Then vice versa, i.e. the first actual row to bottom wrap row.
        dec     di
        lodsb                   ; Get byte from DS:[SI++] to AL
        ES_prefix
        xlatb                   ; Reverse it.
        mov     [di],al         ; And store to destination wrap row.
        loop    loop22
;
        popf
        pop     si
        pop     es
        pop     ds
        pop     di
        pop     cx
        pop     bx
        pop     ax
        ret
;
_pwrap2tb endp
;
;
;
        public  _cgaplotcell ; (y,x,cell)
; y    = [bp+4]
; x    = [bp+6]
; cell = [bp+8]
_cgaplotcell proc  near
        push    bp
        mov     bp,sp
        push    ax
        push    bx
        push    cx
        push    es
        push    si
;
        mov     ax,[bp+4]       ; Get y.
        cmp     ax,_vidlines    ; If y >= _vidlines
        jae     ulos            ; Then don't plot.
        mov     si,[bp+6]       ; Get x.
        cmp     si,_vidcols     ; If its above or equivalent to _vidcols
        jae     ulos            ;  then don't output.
        xor     bx,bx           ; Clear BX for the next operation.
        shr     ax,1            ; Shift y one right, and get bit-0 to carry.
        mov     cl,14           ; Set BX to contain 0 or 8192, depending
        rcl     bx,cl           ;  whether y was even or odd.
        mul     _vidwidth       ; Multiply AX with _vidwidth
        add     bx,ax           ; And add it to BX.
        mov     ax,_vidseg      ; Put video
        mov     es,ax           ;  segment to ES.
        mov     ax,si           ; Get copy of X.
        mov     cl,3
        shr     si,cl           ; Divide X by eight to get the byte index.
        and     al,7            ; Get the lowest three bits of X (0-7)
        mov     cl,al           ; Get it to shift count register CL.
        inc     cl              ; And icrement.
        xor     al,al           ; Zero al first.
        stc                     ; Set carry flag.
        rcr     al,cl           ; And rotate carry to bit-position (8-CL)
        cmp     word ptr [bp+8],0 ; Test bit to be output.
        jz      its_zero
        or      es:[bx+si],al   ; If its one, then or 1-bit to dest byte.
        jmp     ulos
its_zero:                       ; But if it's zero, then clear that bit.
        not     al
        and     es:[bx+si],al
;
ulos:
        pop     si
        pop     es
        pop     cx
        pop     bx
        pop     ax
        pop     bp
        ret
;
_cgaplotcell endp
;
;codeseg        ends
        end
