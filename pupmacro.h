;
; PUPMACRO.H
; Various definitions for the module pupdate.h.
; Some macros for the Aztec-C 3.40's assembler, which doesn't know
; any 386 instructions nor long registers (EAX, EBX, etc.)
;
; This file is Public Domain, coded by A. Karttunen.
;
; At least R0 must be AX and R1 must be BX.
;
R0      EQU     AX
R1      EQU     BX
R2      EQU     CX
R3      EQU     DX
R4      EQU     BP
;
; Explicit doubleword (32-bit) versions:
R0D     EQU     EAX
R1D     EQU     EBX
R2D     EQU     ECX
R3D     EQU     EDX
R4D     EQU     EBP
;
; Explicit word versions:
R0W     EQU     AX
R1W     EQU     BX
R2W     EQU     CX
R3W     EQU     DX
R4W     EQU     BP
;
; Byte versions:
R0L     EQU     AL
R1L     EQU     BL
R2L     EQU     CL
R3L     EQU     DL
R0H     EQU     AH
R1H     EQU     BH
R2H     EQU     CH
R3H     EQU     DH
;
ifdef   AZTEC
; Define 386-forms of RCL, RCR, ROL and ROR where shift-count is specified
; as immediate byte: (For the Aztec-C 3.40's assembler)
;
codemacro RCL   dst:Ew,cnt:Db(2,255)
segfix  dst
        db      0C1h
	modrm   2,dst
	db      cnt
	endm
;
codemacro RCR   dst:Ew,cnt:Db(2,255)
segfix  dst
        db      0C1h
	modrm   3,dst
	db      cnt
	endm
;
codemacro ROL   dst:Ew,cnt:Db(2,255)
segfix  dst
        db      0C1h
	modrm   0,dst
	db      cnt
	endm
;
codemacro ROR   dst:Ew,cnt:Db(2,255)
segfix  dst
        db      0C1h
	modrm   1,dst
	db      cnt
	endm
;
endif
;
longprefix EQU  66h
;
; When instructions are prefixed with this byte in 386, the extended
;  doubleword registers EAX, EBX, etc. are used instead of word registers
;   AX,BX, etc.
long    MACRO
        DB      longprefix
	ENDM
;
longp   MACRO
if      I386CODE
        long
endif
        ENDM
;
CS_prefix MACRO
        DB      2Eh
	ENDM
;
ES_prefix MACRO
        DB      26h
	ENDM
;
FS_prefix MACRO
        DB      64h
	ENDM
;
; Turbo Assembler should know the 386 instructions.
ifdef   AZTEC
;
; Convert Word to Dword, 386 instruction:
cwde    MACRO
        long
	cbw
	ENDM
; Push all extended general registers: (386 instruction).
pushad  MACRO
        long
	DB      60h
	ENDM
; Pop all extended general registers: (386 instruction).
popad   MACRO
        long
	DB      61h
	ENDM
;
endif
;
;
_add    MACRO   DST,SRC
        longp
	add     DST,SRC
	ENDM
;
_and    MACRO   DST,SRC
        longp
	and     DST,SRC
	ENDM
;
_cmp    MACRO   DST,SRC
        longp
	cmp     DST,SRC
	ENDM
;
_mov    MACRO   DST,SRC
        longp
	mov     DST,SRC
	ENDM
;
_or     MACRO   DST,SRC
        longp
	or      DST,SRC
	ENDM
;
_rcl    MACRO   DST,CNT
        longp
	rcl     DST,CNT
	ENDM
;
_rcr    MACRO   DST,CNT
        longp
	rcr     DST,CNT
	ENDM
;
_rol    MACRO   DST,CNT
        longp
	rol     DST,CNT
	ENDM
;
_ror    MACRO   DST,CNT
        longp
	ror     DST,CNT
	ENDM
;
_test   MACRO   DST,SRC
        longp
	test    DST,SRC
	ENDM
;
_xor    MACRO   DST,SRC
        longp
	xor     DST,SRC
	ENDM
;
; Instructions with one argument:
_not    MACRO   DST
        longp
	not     DST
	ENDM
;
_pop    MACRO   X
        longp
	pop     X
	ENDM
;
_push   MACRO   X
        longp
	push    X
	ENDM
;
addbits MACRO   SUM,CARRIES
        _xor    SUM,CARRIES
 	_not    CARRIES         ; This is
	_or     CARRIES,SUM     ;  same
	_not    CARRIES         ;   as bic CARRIES,SUM
	ENDM
;
; This is like previous, but leaves the carry-bits complemented.
add2bit MACRO   SUM,CARRIES
        _xor    SUM,CARRIES
 	_not    CARRIES         ;
	_or     CARRIES,SUM     ;
;	_not    CARRIES         ; Commented out, leave carries complemented
	ENDM
;
; This is probably faster than addbits, but requires a surplus temporary
; register:
add3bit MACRO   SUM,CARRIES,TMP
        _mov    TMP,SUM
	_xor    SUM,CARRIES
	_and    CARRIES,TMP
	ENDM
;
; (If we had BIC, (Bit Clear), i.e. AND-NOT instruction like in PDP-11 or VAX,
; then two instructions (XOR & BIC) would be enough for summing the bits!)
;
; myrcl & myrcr macros are for rotating the register one left or right
; in a way like in some ???-endian machine, e.g. M68K.
;
myrcl   MACRO   REG,REG2
if      I386CODE
	_rol    REG,10h         ; Swap the words in doubleword register.
endif
        rol     REG2,1          ; Get the most significant bit of REG2 to carry
        rcl     REG&H,1         ; Shift two bytes
	rcl     REG&L,1         ;  right one bit.
if      I386CODE
	_rcl    REG,10h         ; Swap the dword halves again.
	rcl     REG&W,1         ; Make a minor correction...
	rcl     REG&H,1
	rcl     REG&L,1
endif
	ENDM
;
; The first two instructions are probably equal to:
; xchg  REG&L,REG&H
; rcr   REG,1
; xchg  REG&L,REG&H
;
myrcr   MACRO   REG
        rcr     REG&L,1         ; Shift the two leftmost
	rcr     REG&H,1         ;  bytes one right.
if      I386CODE                ; If rotating doubleword?
	_rcl    REG,10h         ; Swap the halves of doubleword.
	rcl     REG&W,1         ; Minor correction...
        rcr     REG&L,1         ; Shift the two rightmost
	rcr     REG&H,1         ;  bytes one right.
	_rol    REG,10h         ; Swap the halves back.
endif
	ENDM
;
