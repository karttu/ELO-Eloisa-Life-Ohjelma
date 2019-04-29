;
; These were for Aztec-C assembler:
;codeseg segment para public 'code'
;dataseg segment para public 'data'
;
        DOSSEG
        .MODEL  SMALL
        .DATA
        extrn _alive:word,_born:word,_died:word,_maxcol:word,_maxrow:word
        extrn _cell_chars:byte ; This is string.
        extrn _print_all:byte ; This is one byte flag.
        extrn _tables:word,_vidlineptr:word ; Actually these are pointers.
        extrn _vidcols:word,_vidlim:word,_vidlines:word,_vidseg:word
        extrn _vidstart:word,_vid2start:word,_vidwidth:word
        extrn _anychanges:word
SAVEJUMP DW 0
; New cell is got from this table by indexing this with
; ((sum of neighbours) or old_state_of_cell):
;               0  1  2  3  4  5  6  7  8  9
QUICKTAB DB     0, 0, 0, 1, 0, 0, 0, 0, 0, 0
NOPNOP  EQU     9090h  ; Two nop (90 hex in Intel *86 processors) instructions
;
; Again, code for Aztec:
;dataseg ends
;
;       assume  cs:codeseg,ds:dataseg,es:dataseg,ss:dataseg
;
        .CODE
;
; Update function coded in 8086 assembler, outputs stuff directly
;  to the video memory.
;
; Requires following global variables:
;
; vidcols      - The width of the video screen in characters (is probably 80).
;                 If SI goes above this, then no output is shown. Set this
;                 to zero if you don't want output at all.
; vidlim       - If BP goes equal or above this, then no output is shown.
;                 This should be (vidstart + (vidwidth_ * rows in screen)),
;                 e.g. 4000 with 80*25 screen.
; vidseg     - Segment to videomemory, e.g. B800 (or A000 with PC98's)
; vid2start  - Starting offset to that. Usually zero or vidwidth if statline
;              specified.
; vidwidth   - Width (in bytes) of one row in the video memory. Should be 160.
; cell_chars - String containing two characters, first which is used
;               for dead cells, and second which is for alive cells. E.g: " *"
; alive      - Integer counter for alive cells.
; born       - Integer counter for born  cells.
; died       - Integer counter for died  cells.
; changes    - Integer counter for changed (= born+died) cells.
;
; tables     - tables+0 contains pointer to the vector containing
;               pointers to the rows of past table.
;              tables+2 contains pointer to the vector containing
;               pointers to the rows of present table.
;
; maxcol     - Maximum value for the column index (si).
;
; Register usage:
;
; SI - Column index, from 1 to maxcol (inclusive).
; DI - Pointer to vector of present rows.
; BP - Pointer to the video memory.
; BX - Pointer to the current past row, or work register in some other
;       places, keeping miscellaneous pointer values.
;
; Registers  CH, CL, DH and DL keep the values of cells around the
;  cell to be computed. CL is central cell itself, CH sum of cells one
;  left and right of it, DH sum of two cells diagonally below it, and DL
;  is the cell just below it, in the next row.
;
; Sum of upper line (old CL+CH) and (new) CH+DH+DL is collected to AL.
;
;  -1 SI +1
;
; +--+--+--+
; !        !
; +--+--+--+
; !CH!CL!CH!  DI points to the row in present table corresponding to this row.
; +--+--+--+
; !DH!DL!DH!  BX points to the beginning of this row.
; +--+--+--+
;
; Note how some "inherent parallelism" of the *86 processors is utilized.
; Namely the fact that registers AX, BX, CX and DX can be divided to
; two halves, AL, AH, BL, BH, etc. (lower & upper byte) allows me to use
; some hacks which handle two byte registers with one instruction.
;
;
;
GEN_SUPDATE MACRO FUNNAME,CHECKVIDLIM,FREDKIN
        local   col_loop,nakkisose,row_loop,hyppy1,cont1,hotcode
        local   changed,output_cell,exceed_vidlim,back_to_loop,the_end
        public  FUNNAME
FUNNAME proc  near
;
;
; Push almost all registers in alphabetical order (probably not all needed.)
        push    ax
        push    bp
        push    bx
        push    cx
        push    di
        push    dx
        push    es
        push    si
; These are zeroed now in the calling function:
;       xor     ax,ax
;       mov     alive_,ax
;       mov     born_,ax
;       mov     died_,ax
;
        mov     ax,_vidseg
        mov     es,ax
if      CHECKVIDLIM
        mov     ax,cs:hotcode   ; Get the copy of jmp instruction in hotcode
        mov     SAVEJUMP,ax     ;  to SAVEJUMP
endif
        mov     si,1            ; Start from column 1.
col_loop:
if      CHECKVIDLIM
        cmp     si,_vidcols     ; Check whether column fits to vidememory?
        ja      nakkisose       ; If not then let the hotcode contain that jump
; But if it fits, then replace the jump with NOP and set the BP to point
;  the videomemory:
        mov     cs:hotcode,NOPNOP
endif
        mov     bp,si           ; Set BP to be vid2start+((columns-1)*2)
        dec     bp
        shl     bp,1
        add     bp,_vid2start
;
nakkisose:
        mov     di,_tables      ; Get pointer to past rows.
        mov     bx,[di]         ; Get pointer to first (wrap) row.
        mov     ax,[bx+si]      ; Set AX to contain
        add     ah,[bx+si-1]    ;  the sum of three cells on top row.
        mov     bx,[di+2]       ; Get pointer to second row.
        mov     cx,[bx+si]      ; And set CL and CH to be central cell and
        add     ch,[bx+si-1]    ;  and neighbouring ones on that row.
        add     di,4            ; Set pointer to past rows to point to
        mov     bx,[di]         ;  second row (rows being 0, 1, 2, etc.)
        push    di              ;   and push it to topmost on stack.
        mov     di,_tables+2    ; Set pointer to present rows to point to
; zeroth row. It's immediately incremented to first row first time in loop.
        jmp     hyppy1          ; Jump to inner loop, but skip the cell
;                                  moving part.
;
row_loop: 
; In this inner loop the column is kept same,
;  but rows 1 - maxrow are computed:
;
        mov     ax,cx           ; nw_n_ne = w_e + centre cell.
        mov     cx,dx           ; cell (CL) = s (DL) and w_e (CH) = sw_se (DH)
hyppy1:
        mov     dx,[bx+si]      ; Get the cells (DL = [bx+si] & DH = [bx+si+1])
        add     dh,[bx+si-1]    ;  of the lowest row.
if      FREDKIN
        add     al,ch           ; With Fredkin's self-replicating algorithm
        add     al,dl           ; we just sum the four orthogonal neighbours &
        and     ax,1            ; check parity. (odd -> alive, even -> dead)
else
; The life algorithm:
        add     ax,dx           ; Sum neighbours into AX in "parallel" way.
        add     al,ah           ; Add the sum in high byte to low byte.
        add     al,ch           ; And also CH.
        or      al,cl           ; Or it with centre cell.
        lea     bx,QUICKTAB     ; Load address of quicktab, and translate al
        xlatb                   ; Index 3 should contain 1, all others 0.
        xor     ah,ah           ; Clear the upper byte of AX.
; (CBW would also work)
endif
        add     _alive,ax       ; Add it to alive count.
        add     di,2            ; Add pointer to present row ptrs to next row.
        mov     bx,[di]         ; Get pointer to present row.
        mov     ah,al           ; Get copy of al to ah.
        xor     ah,[bx+si]      ; Xor it with same cell two generations before.
        or      byte ptr _anychanges,ah ; Or that to low byte of anychanges.
        mov     [bx+si],al      ; Set new cell.
        cmp     al,cl           ; Different from previous generation?
        jne     changed         ; If it is, then go to output it.
        cmp     byte ptr _print_all,0 ; If print_all_ is nonzero then print
        jnz     output_cell     ; all cells anyway, whether changed or not.
back_to_loop:
        add     bp,_vidwidth    ; Set bp to point to next row, same column.
if      CHECKVIDLIM
        cmp     bp,_vidlim      ; Check whether it has gone out of screen?
        jae     exceed_vidlim   ; (vidlim_ contains exclusive limit)
endif
cont1:
        pop     bx              ; Get pointer to past table.
        add     bx,2            ; And increment it to next row.
        push    bx              ; Push it back to stack.
        mov     bx,[bx]         ; And get pointer to past row.
        test    bx,bx           ; And check whether NULL encountered?
        jnz     row_loop        ; If not then continue this inner loop.
;
        pop     bx              ; Pop that pointer from stack.
        inc     si              ; Increment column counter.
        cmp     si,_maxcol      ; If all the columns went through?
        ja      the_end
        jmp     col_loop        ; Not yet, continue the outer loop.
the_end:
;
; Pop registers.
        pop     si
        pop     es
        pop     dx
        pop     di
        pop     cx
        pop     bx
        pop     bp
        pop     ax
        ret
;
changed:
output_cell:
if      CHECKVIDLIM
; WARNING! Some self-modifying code here:
hotcode label   word            ; This form allows accessing this location
        jmp     short cont1     ; Must be only two bytes.
endif
; If cell was changed, then output it to video memory:
        lea     bx,_cell_chars  ; Get the pointer to dead & alive characters.
        xlatb                   ; And do  AL <= [BX+AL]
        mov     es:[bp],al      ; And put it to vidmem.
        jmp     back_to_loop
;
exceed_vidlim:
if      CHECKVIDLIM
        mov     ax,SAVEJUMP     ; If it is, then put the jump instruction
        mov     cs:hotcode,ax   ;  back to hotcode location.
        jmp     cont1
endif
;
FUNNAME endp
        ENDM
;
        GEN_SUPDATE     _svlife,1,0 ; Life algorithm with vidlim checking
        GEN_SUPDATE     _svfred,1,1 ; Fredkin algorithm  ------ "" ------
        GEN_SUPDATE     _slife,0,0  ; Life algorithm without vidlim checking
;
;
;
        public  _cputvid ; (y,x,char)
; y    = [bp+4]
; x    = [bp+6]
; char = [bp+8]
_cputvid proc  near
        push    bp
        mov     bp,sp
        push    es
        push    si
;
        mov     ax,[bp+4]       ; Get y.
        cmp     ax,_vidlines    ; If it's above or
        jae     ulos            ;  equivalent to vidlines, then don't output.
        mul     _vidwidth       ; Multiply it with vidwidth_
        mov     si,[bp+6]       ; Get x.
        cmp     si,_vidcols     ; If its above or equivalent to vidcols_
        jae     ulos            ;  then don't output.
        shl     si,1            ; Multiply by 2, because of those attribytes.
        add     si,ax           ; Add y, so now SI contains the correct address
        mov     ax,_vidseg      ; Put video
        mov     es,ax           ;  segment to ES.
        mov     ax,[bp+8]       ; Get character to be output.
        mov     es:[si],al      ; Put it to video memory.
;
ulos:
        pop     si
        pop     es
        pop     bp
        ret
;
_cputvid endp
;
;
;
        public  _fillmem ; (start_far_pointer,fillword,count)
_fillmem proc  near
        push    bp
        mov     bp,sp
        push    cx
        push    es
        push    di
        pushf
;
        cld                     ; Direction for stosw is plusward.
        les     di,[bp+4]       ; Get starting address.
        mov     ax,[bp+8]       ; Get the fillword (e.g. init char & attrbyte)
        mov     cx,[bp+10]      ; Get the count (in bytes)
        shr     cx,1            ; Divide CX by 2 to get the word count.
        cli                     ; Disable interrupts for rep stosw
        rep     stosw           ; Write AX to ES:[DI++] CX times.
;       sti                     ; Enable interrupts again.
;
        popf                    ; Actually, this will do the job of sti.
        pop     di
        pop     es
        pop     cx
        pop     bp
        ret
;
_fillmem endp
;
;
        public _wrap1edges  ; Normal one.
_wrap1edges proc near
;
        push    bx
        push    di
        push    dx
        push    si
;
        mov     si,_maxcol
        mov     di,_tables      ; Get pointer to past rows.
        mov     dx,_maxrow      ; And let the DX to point the last row of them.
        shl     dx,1            ; (so it works as the end limit,
        add     dx,di           ;  which DI can be compared against).
        add     di,2   ; Initialize DI ptr to second row. (= first actual row).
loop11:
        mov     bx,[di]         ; Get the pointer to row, and wrap the edges:
        mov     al,[bx+si]      ; past(y,0)        = past(y,maxcol);
        mov     [bx],al
        mov     al,[bx+1]       ; past(y,maxcol+1) = past(y,1);
        mov     [bx+si+1],al
        add     di,2            ; Increment to next row.
        cmp     di,dx           ; Continue so long as di <= (*tables)+maxrow
        jbe     loop11
;
out11:
        pop     si
        pop     dx
        pop     di
        pop     bx
        ret
;
_wrap1edges endp
;
;
;
        public _wrap2edges ;  /* Twisted. */
_wrap2edges proc near
;
        push    bp
        push    bx
        push    di
        push    si
;
        mov     si,_maxcol
        mov     di,_tables      ; Get pointer to past rows.
        mov     bp,_maxrow      ; And let the BP
        shl     bp,1            ;  to point the last row of them.
        add     bp,di
loop22:
        add     di,2            ; Increment DI to next destination row.
        mov     bx,ds:[bp]      ; Get pointer to row...
        mov     al,[bx+si]      ; past((maxrow+1)-y,maxcol); get the edge cells
        mov     ah,[bx+1]       ; past((maxrow+1)-y,1);
        mov     bx,[di]         ; Get pointer to destination row.
        mov     [bx],al         ; And put the edge cells to wrap columns.
        mov     [bx+si+1],ah
        sub     bp,2            ; Decrement BP to previous row.
        cmp     bp,_tables      ; If not yet came to zeroth row, (which
        jnz     loop22          ; shouldn't be wrapped) then continue.
;
out22:
        pop     si
        pop     di
        pop     bx
        pop     bp
        ret
;
_wrap2edges endp
;

        public  _aux_copylinetovid
_aux_copylinetovid proc near
;
        pushf
        push    ax
        push    di
        push    es
        push    si
;
        cld                     ; Direction is plusward for lodsb and stosb.
        mov     ax,_vidseg
        mov     es,ax           ; Get videosegment to ES.
        mov     di,_vidstart
        mov     si,_vidlineptr
loop1:
        lodsb                   ; Get the character from line. (al = ds:[si++])
        test    al,al           ; End zero encountered?
        jz      out1
        stosb                   ; And put it to videomemory. (es:[di++] = al)
        inc     di              ; Advance past attribute byte.
        jmp     loop1
out1:
;
        pop     si
        pop     es
        pop     di
        pop     ax
        popf
        ret                     ; And return back
_aux_copylinetovid endp
;
; This was for Aztec:
;codeseg        ends
        END
