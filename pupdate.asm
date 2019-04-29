;
        DOSSEG
        .MODEL  SMALL
; Include the stuff itself:
        include pupdate.h
;
;
;	assume cs:codeseg,ds:dataseg,es:dataseg,ss:dataseg
;codeseg segment para public 'code'
;
        .CODE
; DS -> CS
C_vid2start  DW 0
C_cga1kludge DW 0
C_cga2kludge DW 0
C_bywidth    DW 0
C_silimit    DW 0
C_vidlim     DW 0
C_collimit   DW 0
C_vcollimit  DW 0
C_p_column   DW 0
; CS -> DS
C_anychanges DW 0
             DW 0
C_alive      DW 0
             DW 0
C_born       DW 0
             DW 0
C_died       DW 0
             DW 0
C_minextcol  DW 0
C_maxextcol  DW 0
C_minextrow  DW 0
C_maxextrow  DW 0
; CS internal:
C_kludgeadr  DW 0
C_di_save    DW 0
C_colchanges DW 0
             DW 0
C_c46        DW 0
             DW 0
C_s46        DW 0
             DW 0
;
; Table for counting the number of 1-bits in byte:
C_cntbits    DB  0
        DB  1
        DB  1
        DB  2
        DB  1
        DB  2
        DB  2
        DB  3
        DB  1
        DB  2
        DB  2
        DB  3
        DB  2
        DB  3
        DB  3
        DB  4
        DB  1
        DB  2
        DB  2
        DB  3
        DB  2
        DB  3
        DB  3
        DB  4
        DB  2
        DB  3
        DB  3
        DB  4
        DB  3
        DB  4
        DB  4
        DB  5
        DB  1
        DB  2
        DB  2
        DB  3
        DB  2
        DB  3
        DB  3
        DB  4
        DB  2
        DB  3
        DB  3
        DB  4
        DB  3
        DB  4
        DB  4
        DB  5
        DB  2
        DB  3
        DB  3
        DB  4
        DB  3
        DB  4
        DB  4
        DB  5
        DB  3
        DB  4
        DB  4
        DB  5
        DB  4
        DB  5
        DB  5
        DB  6
        DB  1
        DB  2
        DB  2
        DB  3
        DB  2
        DB  3
        DB  3
        DB  4
        DB  2
        DB  3
        DB  3
        DB  4
        DB  3
        DB  4
        DB  4
        DB  5
        DB  2
        DB  3
        DB  3
        DB  4
        DB  3
        DB  4
        DB  4
        DB  5
        DB  3
        DB  4
        DB  4
        DB  5
        DB  4
        DB  5
        DB  5
        DB  6
        DB  2
        DB  3
        DB  3
        DB  4
        DB  3
        DB  4
        DB  4
        DB  5
        DB  3
        DB  4
        DB  4
        DB  5
        DB  4
        DB  5
        DB  5
        DB  6
        DB  3
        DB  4
        DB  4
        DB  5
        DB  4
        DB  5
        DB  5
        DB  6
        DB  4
        DB  5
        DB  5
        DB  6
        DB  5
        DB  6
        DB  6
        DB  7
        DB  1
        DB  2
        DB  2
        DB  3
        DB  2
        DB  3
        DB  3
        DB  4
        DB  2
        DB  3
        DB  3
        DB  4
        DB  3
        DB  4
        DB  4
        DB  5
        DB  2
        DB  3
        DB  3
        DB  4
        DB  3
        DB  4
        DB  4
        DB  5
        DB  3
        DB  4
        DB  4
        DB  5
        DB  4
        DB  5
        DB  5
        DB  6
        DB  2
        DB  3
        DB  3
        DB  4
        DB  3
        DB  4
        DB  4
        DB  5
        DB  3
        DB  4
        DB  4
        DB  5
        DB  4
        DB  5
        DB  5
        DB  6
        DB  3
        DB  4
        DB  4
        DB  5
        DB  4
        DB  5
        DB  5
        DB  6
        DB  4
        DB  5
        DB  5
        DB  6
        DB  5
        DB  6
        DB  6
        DB  7
        DB  2
        DB  3
        DB  3
        DB  4
        DB  3
        DB  4
        DB  4
        DB  5
        DB  3
        DB  4
        DB  4
        DB  5
        DB  4
        DB  5
        DB  5
        DB  6
        DB  3
        DB  4
        DB  4
        DB  5
        DB  4
        DB  5
        DB  5
        DB  6
        DB  4
        DB  5
        DB  5
        DB  6
        DB  5
        DB  6
        DB  6
        DB  7
        DB  3
        DB  4
        DB  4
        DB  5
        DB  4
        DB  5
        DB  5
        DB  6
        DB  4
        DB  5
        DB  5
        DB  6
        DB  5
        DB  6
        DB  6
        DB  7
        DB  4
        DB  5
        DB  5
        DB  6
        DB  5
        DB  6
        DB  6
        DB  7
        DB  5
        DB  6
        DB  6
        DB  7
        DB  6
        DB  7
        DB  7
        DB  8
;
        public _initCSvars
_initCSvars proc near
; Transfer values of DS-variables to corresponding variables in CS-segment:
        mov     ax,_vid2start
        mov     CS:C_vid2start,ax
        mov     ax,_cga1kludge
        mov     CS:C_cga1kludge,ax
        mov     ax,_cga2kludge
        mov     CS:C_cga2kludge,ax
        mov     ax,_bywidth
        mov     CS:C_bywidth,ax
        mov     ax,_silimit
        mov     CS:C_silimit,ax
        mov     ax,_vidlim
        mov     CS:C_vidlim,ax
        mov     ax,_collimit
        mov     CS:C_collimit,ax
        mov     ax,_vcollimit
        mov     CS:C_vcollimit,ax
        mov     ax,_p_column
        mov     CS:C_p_column,ax
;
        mov     word ptr CS:C_anychanges,0
        mov     word ptr CS:C_anychanges+2,0
        mov     word ptr CS:C_alive,0
        mov     word ptr CS:C_alive+2,0
        mov     word ptr CS:C_born,0
        mov     word ptr CS:C_born+2,0
        mov     word ptr CS:C_died,0
        mov     word ptr CS:C_died+2,0
        mov     CS:C_minextrow,65535
        mov     CS:C_maxextrow,0
        mov     CS:C_minextcol,0
        mov     CS:C_maxextcol,0
        ret
_initCSvars endp
;
        public _exportCSvars
_exportCSvars proc near
        mov     ax,word ptr CS:C_anychanges
        mov     word ptr _anychanges,ax
        mov     ax,word ptr CS:C_anychanges+2
        mov     word ptr _anychanges+2,ax
        cmp     byte ptr _extent_checking,0  ; If extent checking in use,
        jnz     hyppy1                       ; then don't change the old
        mov     ax,word ptr CS:C_alive       ; value of alive...
        mov     word ptr _alive,ax
        mov     ax,word ptr CS:C_alive+2
        mov     word ptr _alive+2,ax
; Otherwise, if extent checking not in use, we don't need born, died, nor
; minimumn & maximum extent variables:
        ret             ; So let's return immediately.
hyppy1:
        mov     ax,word ptr CS:C_born
        mov     word ptr _born,ax
        mov     ax,word ptr CS:C_born+2
        mov     word ptr _born+2,ax
        mov     ax,word ptr CS:C_died
        mov     word ptr _died,ax
        mov     ax,word ptr CS:C_died+2
        mov     word ptr _died+2,ax
        mov     ax,CS:C_minextcol
        mov     _minextcol,ax
        mov     ax,CS:C_maxextcol
        mov     _maxextcol,ax
        mov     ax,CS:C_minextrow
        mov     _minextrow,ax
        mov     ax,CS:C_maxextrow
        mov     _maxextrow,ax
;
        ret
_exportCSvars endp
;
;
;dataseg segment para public 'data'
;        extrn updatefuns_:word, initfuns_:word, ipointers_:word
;dataseg ends
;
        GEN_PUPDATE     pl0000000,0,0,0,0,0
	GEN_PUPDATE     pf0000000,1,0,0,0,0
        GEN_PUPDATE     pl1000000,0,1,0,0,0
	GEN_PUPDATE     pf1000000,1,1,0,0,0
        GEN_PUPDATE     pl0100000,0,0,1,0,0
	GEN_PUPDATE     pf0100000,1,0,1,0,0
        GEN_PUPDATE     pl1100000,0,1,1,0,0
	GEN_PUPDATE     pf1100000,1,1,1,0,0
        GEN_PUPDATE     pl0010000,0,0,0,1,0
	GEN_PUPDATE     pf0010000,1,0,0,1,0
        GEN_PUPDATE     pl1010000,0,1,0,1,0
	GEN_PUPDATE     pf1010000,1,1,0,1,0
        GEN_PUPDATE     pl0110000,0,0,1,1,0
	GEN_PUPDATE     pf0110000,1,0,1,1,0
        GEN_PUPDATE     pl1110000,0,1,1,1,0
	GEN_PUPDATE     pf1110000,1,1,1,1,0
;
;
;codeseg	ends
	end
