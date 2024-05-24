; This disassembly was created using Emulicious (http://www.emulicious.net)	
.MEMORYMAP	
SLOTSIZE $7FF0	
SLOT 0 $0000	
SLOTSIZE $10	
SLOT 1 $7FF0	
SLOTSIZE $4000	
SLOT 2 $8000	
DEFAULTSLOT 2	
.ENDME	
.ROMBANKMAP	
BANKSTOTAL 8	
BANKSIZE $7FF0	
BANKS 1	
BANKSIZE $10	
BANKS 1	
BANKSIZE $4000	
BANKS 6	
.ENDRO	
	
.enum $C000 export	
_RAM_C000_ db	
_RAM_C001_ db	
_RAM_C002_ db	
.ende	
	
.enum $C005 export	
_RAM_C005_ db	
_RAM_C006_ dw	
_RAM_C008_ db	
.ende	
	
.enum $C026 export	
_RAM_C026_ dw	
_RAM_C028_ db	
.ende	
	
.enum $C046 export	
_RAM_C046_ db	
.ende	
	
.enum $C048 export	
_RAM_C048_ db	
.ende	
	
.enum $C054 export	
_RAM_C054_ db	
.ende	
	
.enum $C066 export	
_RAM_C066_ db	
.ende	
	
.enum $C068 export	
_RAM_C068_ db	
.ende	
	
.enum $C086 export	
_RAM_C086_ dsb $9	
.ende	
	
.enum $C0A6 export	
_RAM_C0A6_ dsb $9	
.ende	
	
.enum $C0C6 export	
_RAM_C0C6_ dsb $9	
.ende	
	
.enum $C0FE export	
_RAM_C0FE_ db	
_RAM_C0FF_ db	
_RAM_C100_ db	
_RAM_C101_ db	
.ende	
	
.enum $C109 export	
_RAM_C109_ db	
.ende	
	
.enum $C10B export	
_RAM_C10B_ db	
_RAM_C10C_ db	
_RAM_C10D_ db	
.ende	
	
.enum $C10F export	
_RAM_C10F_ db	
.ende	
	
.enum $C111 export	
_RAM_C111_ db	
.ende	
	
.enum $C113 export	
_RAM_C113_ db	
_RAM_C114_ db	
_RAM_C115_ db	
_RAM_C116_ db	
_RAM_C117_ db	
.ende	
	
.enum $C11E export	
_RAM_C11E_ db	
_RAM_C11F_ db	
_RAM_C120_ db	
_RAM_C121_ dw	
_RAM_C123_ db	
.ende	
	
.enum $C127 export	
_RAM_C127_ db	
_RAM_C128_ db	
_RAM_C129_ db	
_RAM_C12A_ db	
_RAM_C12B_ db	
_RAM_C12C_ db	
_RAM_C12D_ db	
_RAM_C12E_ db	
_RAM_C12F_ db	
_RAM_C130_ db	
_RAM_C131_ db	
_RAM_C132_ db	
_RAM_C133_ db	
_RAM_C134_ db	
_RAM_C135_ db	
_RAM_C136_ db	
_RAM_C137_ dw	
_RAM_C139_ db	
.ende	
	
.enum $C143 export	
_RAM_C143_ db	
.ende	
	
.enum $C14D export	
_RAM_C14D_ dw	
_RAM_C14F_ db	
.ende	
	
.enum $C159 export	
_RAM_C159_ db	
.ende	
	
.enum $C163 export	
_RAM_C163_ dsb $8	
_RAM_C16B_ db	
_RAM_C16C_ db	
_RAM_C16D_ db	
_RAM_C16E_ db	
.ende	
	
.enum $C170 export	
_RAM_C170_ db	
_RAM_C171_ db	
_RAM_C172_ db	
_RAM_C173_ db	
_RAM_C174_ db	
_RAM_C175_ db	
.ende	
	
.enum $C178 export	
_RAM_C178_ db	
_RAM_C179_ db	
_RAM_C17A_ db	
_RAM_C17B_ db	
_RAM_C17C_ dw	
.ende	
	
.enum $C181 export	
_RAM_C181_ db	
.ende	
	
.enum $C183 export	
_RAM_C183_ db	
.ende	
	
.enum $C18C export	
_RAM_C18C_ db	
_RAM_C18D_ dw	
_RAM_C18F_ dw	
.ende	
	
.enum $C194 export	
_RAM_C194_ db	
_RAM_C195_ db	
_RAM_C196_ db	
_RAM_C197_ db	
_RAM_C198_ db	
_RAM_C199_ db	
_RAM_C19A_ db	
_RAM_C19B_ db	
_RAM_C19C_ db	
_RAM_C19D_ db	
_RAM_C19E_ db	
_RAM_C19F_ db	
_RAM_C1A0_ db	
_RAM_C1A1_ dsb $78	
.ende	
	
.enum $C220 export	
_RAM_C220_ db	
_RAM_C221_ db	
_RAM_C222_ db	
_RAM_C223_ db	
_RAM_C224_ db	
.ende	
	
.enum $C226 export	
_RAM_C226_ db	
_RAM_C227_ db	
.ende	
	
.enum $C22C export	
_RAM_C22C_ db	
.ende	
	
.enum $C230 export	
_RAM_C230_ db	
_RAM_C231_ db	
_RAM_C232_ db	
_RAM_C233_ db	
_RAM_C234_ db	
.ende	
	
.enum $C38C export	
_RAM_C38C_ db	
.ende	
	
.enum $C3A0 export	
_RAM_C3A0_ dw	
_RAM_C3A2_ db	
.ende	
	
.enum $C3A4 export	
_RAM_C3A4_ db	
.ende	
	
.enum $C3A7 export	
_RAM_C3A7_ db	
.ende	
	
.enum $C422 export	
_RAM_C422_ db	
.ende	
	
.enum $C424 export	
_RAM_C424_ db	
.ende	
	
.enum $C427 export	
_RAM_C427_ db	
.ende	
	
.enum $C4A0 export	
_RAM_C4A0_ dw	
_RAM_C4A2_ db	
_RAM_C4A3_ db	
_RAM_C4A4_ db	
.ende	
	
.enum $C4A6 export	
_RAM_C4A6_ db	
_RAM_C4A7_ db	
.ende	
	
.enum $C4AC export	
_RAM_C4AC_ db	
.ende	
	
.enum $C4B0 export	
_RAM_C4B0_ dw	
.ende	
	
.enum $C520 export	
_RAM_C520_ dw	
_RAM_C522_ db	
_RAM_C523_ db	
_RAM_C524_ db	
_RAM_C525_ db	
_RAM_C526_ db	
_RAM_C527_ db	
.ende	
	
.enum $C5B5 export	
_RAM_C5B5_ db	
.ende	
	
.enum $C640 export	
_RAM_C640_ db	
.ende	
	
.enum $C644 export	
_RAM_C644_ db	
.ende	
	
.enum $C647 export	
_RAM_C647_ db	
.ende	
	
.enum $C64F export	
_RAM_C64F_ db	
.ende	
	
.enum $CA02 export	
_RAM_CA02_ db	
.ende	
	
.enum $CA21 export	
_RAM_CA21_ dsb $10	
_RAM_CA31_ dsb $9	
.ende	
	
.enum $CA61 export	
_RAM_CA61_ dsb $80	
.ende	
	
.enum $CB01 export	
_RAM_CB01_ dsb $18	
_RAM_CB19_ db	
.ende	
	
.enum $CB39 export	
_RAM_CB39_ db	
_RAM_CB3A_ db	
_RAM_CB3B_ dw	
_RAM_CB3D_ dw	
_RAM_CB3F_ db	
.ende	
	
.enum $CB7E export	
_RAM_CB7E_ db	
_RAM_CB7F_ db	
.ende	
	
.enum $CBFD export	
_RAM_CBFD_ db	
.ende	
	
.enum $CBFF export	
_RAM_CBFF_ db	
_RAM_CC00_ db	
_RAM_CC01_ db	
_RAM_CC02_ db	
_RAM_CC03_ db	
.ende	
	
.enum $CC05 export	
_RAM_CC05_ db	
_RAM_CC06_ db	
_RAM_CC07_ db	
_RAM_CC08_ db	
_RAM_CC09_ db	
.ende	
	
.enum $CC0B export	
_RAM_CC0B_ db	
.ende	
	
.enum $CC0E export	
_RAM_CC0E_ dw	
_RAM_CC10_ dw	
_RAM_CC12_ dw	
_RAM_CC14_ db	
.ende	
	
.enum $CC17 export	
_RAM_CC17_ dw	
_RAM_CC19_ db	
.ende	
	
.enum $CC1C export	
_RAM_CC1C_ dw	
_RAM_CC1E_ db	
.ende	
	
.enum $CC21 export	
_RAM_CC21_ db	
.ende	
	
.enum $CC23 export	
_RAM_CC23_ dw	
_RAM_CC25_ dw	
_RAM_CC27_ dw	
_RAM_CC29_ dw	
_RAM_CC2B_ dw	
.ende	
	
.enum $CC30 export	
_RAM_CC30_ db	
_RAM_CC31_ db	
.ende	
	
.enum $CC33 export	
_RAM_CC33_ db	
_RAM_CC34_ db	
_RAM_CC35_ db	
_RAM_CC36_ db	
_RAM_CC37_ db	
_RAM_CC38_ db	
_RAM_CC39_ db	
_RAM_CC3A_ db	
_RAM_CC3B_ db	
_RAM_CC3C_ db	
_RAM_CC3D_ db	
_RAM_CC3E_ db	
_RAM_CC3F_ db	
_RAM_CC40_ db	
_RAM_CC41_ dsb $8	
.ende	
	
.enum $CC4A export	
_RAM_CC4A_ dsb $6	
.ende	
	
.enum $CC52 export	
_RAM_CC52_ dsb $7	
.ende	
	
.enum $CC5A export	
_RAM_CC5A_ dw	
.ende	
	
.enum $CC5F export	
_RAM_CC5F_ db	
.ende	
	
.enum $CC80 export	
_RAM_CC80_ db	
.ende	
	
.enum $CC84 export	
_RAM_CC84_ db	
_RAM_CC85_ db	
_RAM_CC86_ db	
_RAM_CC87_ db	
_RAM_CC88_ db	
_RAM_CC89_ db	
.ende	
	
.enum $CCA7 export	
_RAM_CCA7_ db	
_RAM_CCA8_ db	
_RAM_CCA9_ db	
.ende	
	
.enum $CCE9 export	
_RAM_CCE9_ db	
.ende	
	
.enum $CD69 export	
_RAM_CD69_ db	
.ende	
	
.enum $CE29 export	
_RAM_CE29_ db	
.ende	
	
.enum $CEE9 export	
_RAM_CEE9_ db	
.ende	
	
.enum $CFA9 export	
_RAM_CFA9_ db	
.ende	
	
.enum $D069 export	
_RAM_D069_ db	
.ende	
	
.enum $D1E9 export	
_RAM_D1E9_ db	
.ende	
	
.enum $D369 export	
_RAM_D369_ db	
.ende	
	
.enum $D373 export	
_RAM_D373_ dsb $10	
_RAM_D383_ db	
_RAM_D384_ dsb $8	
_RAM_D38C_ db	
.ende	
	
.enum $D394 export	
_RAM_D394_ db	
_RAM_D395_ db	
_RAM_D396_ db	
_RAM_D397_ db	
_RAM_D398_ db	
_RAM_D399_ db	
_RAM_D39A_ db	
_RAM_D39B_ db	
.ende	
	
.enum $DFFF export	
_RAM_DFFF_ db	
.ende	
	
.enum $E205 export	
_RAM_E205_ db	
_RAM_E206_ db	
.ende	
	
.enum $F000 export	
_RAM_F000_ db	
.ende	
	
.enum $FCFF export	
_RAM_FCFF_ db	
.ende	
	
.enum $FFFF export	
_RAM_FFFF_ db	
.ende	
	
; Ports	
.define Port_IOPortControl $3F	
.define Port_PSG $7F	
.define Port_VDPData $BE	
.define Port_VDPAddress $BF	
.define _PORT_DE_ $DE	
	
; Input Ports	
.define Port_VDPStatus $BF	
.define Port_IOPort1 $DC	
.define Port_IOPort2 $DD	
	
.BANK 0 SLOT 0	
.ORG $0000	
	
_LABEL_0_:	
		im 1
		ld sp, $DFFF
		jp _LABEL_69_
	
_LABEL_8_:	
		ld bc, $0006
		add hl, bc
		ld c, (hl)
		inc l
		ld b, (hl)
		ex de, hl
		add hl, bc
		ex de, hl
		ld (hl), d
		ld a, d
		dec l
		ld (hl), e
		ret
	
	; Data from 17 to 17 (1 bytes)
	.db $FF
	
_LABEL_18_:	
		ld bc, $0003
		add hl, bc
		ld c, (hl)
		inc l
		ld b, (hl)
		ex de, hl
		add hl, bc
		ex de, hl
		ld (hl), d
		ld a, d
		dec l
		ld (hl), e
		ret
	
	; Data from 27 to 27 (1 bytes)
	.db $FF
	
_LABEL_28_:	
		jp _LABEL_50E_
	
	; Data from 2B to 2F (5 bytes)
	.db $00 $00 $00 $00 $00
	
_LABEL_30_:	
		jp _LABEL_C14_
	
	; Data from 33 to 37 (5 bytes)
	.db $00 $00 $00 $00 $00
	
_LABEL_38_:	
		jp _LABEL_AF_
	
_LABEL_3B_:	
		push hl
		push bc
		ld a, b
		out (Port_VDPAddress), a
		ld a, c
		or $80
		out (Port_VDPAddress), a
		ld a, b
		ld b, $00
		ld hl, _RAM_C100_
		add hl, bc
		ld (hl), a
		pop bc
		pop hl
		ret
	
	; Data from 50 to 50 (1 bytes)
	.db $93
	
; Data from 51 to 65 (21 bytes)	
_DATA_51_:	
	.db $2E $D5 $CD $9C $0C $CD $BC $2D $D1 $E1 $C3 $97 $2B $D5 $C5 $ED
	.db $4B $D8 $5D $EB $B7
	
_LABEL_66_:	
		jp _LABEL_154_
	
_LABEL_69_:	
		di
		call _LABEL_4B9_
		ld hl, _RAM_C000_
		ld de, _RAM_C000_ + 1
		ld bc, $1FFE
		ld (hl), a
		ldir
		call _LABEL_26D_
		call _LABEL_27A_
		call _LABEL_28E_
		call _LABEL_AB1_
		ld hl, _RAM_C111_
		ld (hl), $00
		inc hl
		ld (hl), $30
		inc hl
		ld (hl), $00
		call _LABEL_224_
		call _LABEL_24E_
		ei
-:	
		call _LABEL_4C8_
		call _LABEL_59D_
		ld a, $01
		call _LABEL_A4_
		jr -
	
_LABEL_A4_:	
		ei
		ld (_RAM_C10C_), a
-:	
		ld a, (_RAM_C10C_)
		or a
		jr nz, -
		ret
	
_LABEL_AF_:	
		di
		push af
		push bc
		in a, (Port_VDPStatus)
		push de
		push hl
		ex af, af'
		exx
		push af
		push bc
		push de
		push hl
		push ix
		push iy
		ld hl, _RAM_C10C_
		ld a, (hl)
		or a
		jr z, _LABEL_130_
		dec (hl)
		call _LABEL_614_
		call _LABEL_1E03_
		call _LABEL_F5D_
		ld hl, _RAM_C10D_
		bit 7, (hl)
		res 7, (hl)
		call nz, _LABEL_22A_
		ld hl, _RAM_C10F_
		in a, (Port_IOPort2)
		and $10
		jr nz, +
		bit 0, (hl)
		res 0, (hl)
		ld a, $00
		call nz, _LABEL_4E3_
		ld hl, _RAM_C10D_
		res 0, (hl)
		ld hl, _RAM_C114_
		res 2, (hl)
		jr ++
	
+:	
		set 0, (hl)
++:	
		ld hl, _RAM_C10B_
		inc (hl)
		ld hl, _RAM_C10D_
		bit 0, (hl)
		jr z, +
		inc hl
		inc (hl)
		ld a, (hl)
		and $0F
		jr nz, +
		dec hl
		res 0, (hl)
+:	
		ld hl, _RAM_D395_
		ld e, $01
		call _LABEL_171_
		inc hl
		ld e, $05
		call _LABEL_171_
		inc hl
		ld e, $07
		call _LABEL_171_
		inc hl
		ld e, $0B
		call _LABEL_171_
		inc hl
		ld e, $0E
		call _LABEL_171_
_LABEL_130_:	
		ld a, (_RAM_C114_)
		and $04
		jr z, +
		ld hl, _RAM_C10C_
		inc (hl)
		call _LABEL_7168_
		jr ++
	
+:	
		call _LABEL_6CA8_
++:	
		pop iy
		pop ix
		pop hl
		pop de
		pop bc
		pop af
		ex af, af'
		exx
		pop hl
		pop de
		pop bc
		pop af
		ei
		reti
	
_LABEL_154_:	
		push af
		push hl
		ld hl, _RAM_C10D_
		bit 0, (hl)
		jr nz, +
		ld a, (_RAM_C115_)
		cp $03
		jr nz, +
		set 0, (hl)
		ld hl, _RAM_C114_
		ld a, $04
		xor (hl)
		ld (hl), a
+:	
		pop hl
		pop af
		retn
	
_LABEL_171_:	
		ld a, (hl)
		or a
		ret z
		push hl
		ld d, $00
		ld hl, $C000
		add hl, de
		dec a
		call _LABEL_406_
		pop hl
		ret
	
_LABEL_181_:	
		ld a, $07
		out (_PORT_DE_), a
		in a, (Port_IOPort1)
		cpl
		ld e, a
		and $3F
		ld d, a
		ld a, e
		and $C0
		ld l, a
		in a, (Port_IOPort2)
		cpl
		and $0F
		ld h, a
		add hl, hl
		add hl, hl
		ld l, d
		jp +
	
+:	
		ld a, (_RAM_C115_)
		cp $03
		ld a, h
		jr nz, +
		ld a, (_RAM_C114_)
		bit 6, a
		ld a, h
		jr nz, ++
+:	
		rla
		rla
		and $C0
		or l
++:	
		ld (_RAM_C11F_), a
		ld hl, _RAM_C0FE_
		cp (hl)
		jr nz, +
		inc hl
		ld (hl), $00
		ret
	
+:	
		ld c, a
		xor (hl)
		ld (hl), c
		inc hl
		and c
		ld (hl), a
		ld a, c
		ret
	
_LABEL_1C6_:	
		push af
		ld a, l
		out (Port_VDPAddress), a
		ld a, h
		and $3F
		or $40
		out (Port_VDPAddress), a
		pop af
		ret
	
-:	
		ld a, l
		out (Port_VDPAddress), a
		ld a, h
		and $3F
		out (Port_VDPAddress), a
		ret
	
_LABEL_1DC_:	
		call -
		ex (sp), hl
		ex (sp), hl
		in a, (Port_VDPData)
		ret
	
_LABEL_1E4_:	
		ex de, hl
		call _LABEL_1C6_
		ex (sp), hl
		ex (sp), hl
-:	
		ld a, (de)
		out (Port_VDPData), a
		xor a
		push hl
		pop hl
		out (Port_VDPData), a
		push hl
		pop hl
		out (Port_VDPData), a
		push hl
		pop hl
		out (Port_VDPData), a
		inc de
		dec bc
		ld a, b
		or c
		jp nz, -
		ret
	
_LABEL_202_:	
		push de
		ld d, a
		call _LABEL_1C6_
		ex (sp), hl
		ex (sp), hl
; 242nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_209_:	
		ld a, d
		out (Port_VDPData), a
		dec bc
		ld a, b
		or c
		jr nz, _LABEL_209_
		pop de
		ret
	
_LABEL_213_:	
		push af
		ld a, (_RAM_C101_)
		and $BF
		ld (_RAM_C101_), a
		out (Port_VDPAddress), a
		ld a, $81
		out (Port_VDPAddress), a
		pop af
		ret
	
_LABEL_224_:	
		ld hl, _RAM_C10D_
		set 7, (hl)
		ret
	
_LABEL_22A_:	
		push af
		ld a, (_RAM_C101_)
		or $40
		ld (_RAM_C101_), a
		out (Port_VDPAddress), a
		ld a, $81
		out (Port_VDPAddress), a
		pop af
		ret
	
_LABEL_23B_:	
		push af
		ld a, (_RAM_C101_)
		and $DF
		or $20
		ld (_RAM_C101_), a
		out (Port_VDPAddress), a
		ld a, $81
		out (Port_VDPAddress), a
		pop af
		ret
	
_LABEL_24E_:	
		push af
		ld a, (_RAM_C101_)
		or $20
		ld (_RAM_C101_), a
		out (Port_VDPAddress), a
		ld a, $81
		out (Port_VDPAddress), a
		pop af
		ret
	
--:	
		ld c, $00
		ld e, $0B
-:	
		ld b, (hl)
		call _LABEL_3B_
		inc hl
		inc c
		dec e
		jr nz, -
		ret
	
_LABEL_26D_:	
		ld hl, _DATA_3DB_
		jr --
	
_LABEL_272_:	
		ld hl, $C000
		ld bc, $0010
		jr +
	
_LABEL_27A_:	
		ld de, _DATA_3E6_
		ld hl, $C000
		ld bc, $0020
+:	
		ld a, l
		out (Port_VDPAddress), a
		ld a, h
		out (Port_VDPAddress), a
		ex (sp), hl
		ex (sp), hl
		jp _LABEL_472_
	
_LABEL_28E_:	
		ld hl, $0000
		ld bc, $0020
		xor a
		call _LABEL_202_
		ld hl, $3800
		ld bc, $0700
		xor a
		call _LABEL_202_
		ld hl, $0000
		ld bc, $0020
		xor a
		call _LABEL_202_
		ret
	
_LABEL_2AD_:	
		ex de, hl
		call _LABEL_1C6_
		ex de, hl
		ld d, b
		ld e, c
		ld c, Port_VDPData
		inc d
		dec d
		jp z, +
-:	
		call ++
		call ++
		dec d
		jp nz, -
+:	
		ld b, e
		ld a, e
		cp $80
		call nc, ++
		cp $40
		call nc, _LABEL_359_
		cp $20
		call nc, _LABEL_399_
		or a
		ret z
		ret
	
++:	
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
_LABEL_359_:	
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
_LABEL_399_:	
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		outi
		ld a, b
		ret
	
; Data from 3DB to 3E1 (7 bytes)	
_DATA_3DB_:	
	.db $26 $80 $FF $FF $FF $FF $FF
	
; 1st entry of Pointer Table from 7CDB (indexed by unknown)	
; Data from 3E2 to 3E5 (4 bytes)	
_DATA_3E2_:	
	.db $00 $00 $00 $00
	
; Data from 3E6 to 3FC (23 bytes)	
_DATA_3E6_:	
	.db $00 $3F $2B $0F $0B $03 $1B $06 $30 $04 $0C $2A $16 $01 $13 $15
	.db $00 $3F $2B $0F $0B $03 $20
	
; 2nd entry of Pointer Table from 73A1 (indexed by unknown)	
; Data from 3FD to 3FD (1 bytes)	
_DATA_3FD_:	
	.db $3D
	
; 2nd entry of Pointer Table from 777A (indexed by unknown)	
; Data from 3FE to 3FE (1 bytes)	
_DATA_3FE_:	
	.db $30
	
; 2nd entry of Pointer Table from 7D11 (indexed by unknown)	
; Data from 3FF to 405 (7 bytes)	
_DATA_3FF_:	
	.db $08 $0C $2A $16 $02 $13 $00
	
_LABEL_406_:	
		push af
		ld a, l
		out (Port_VDPAddress), a
		ld a, h
		out (Port_VDPAddress), a
		ex (sp), hl
		ex (sp), hl
		pop af
		out (Port_VDPData), a
		ret
	
_LABEL_413_:	
		push af
		ld a, l
		out (Port_VDPAddress), a
		ld a, h
		and $3F
		or $40
		out (Port_VDPAddress), a
		pop af
		out (Port_VDPData), a
		ret
	
_LABEL_422_:	
		ex de, hl
		ld c, $04
_LABEL_425_:	
		push hl
--:	
		ld a, (de)
		or a
		jp z, ++
		inc a
		jp z, +
		dec a
		call _LABEL_413_
		inc hl
		inc hl
		inc hl
		inc hl
		inc de
		jp --
	
+:	
		inc de
		ld a, (de)
		call _LABEL_413_
		inc hl
		inc hl
		inc hl
		inc hl
		call _LABEL_413_
		inc hl
		inc hl
		inc hl
		inc hl
		inc de
		jp --
	
++:	
		inc de
		ld a, (de)
		inc de
		or a
		jr z, +
		ld b, a
		ld a, (de)
-:	
		call _LABEL_413_
		inc hl
		inc hl
		inc hl
		inc hl
		djnz -
		inc de
		jp --
	
+:	
		pop hl
		inc hl
		dec c
		jp nz, _LABEL_425_
		ex de, hl
		ret
	
_LABEL_46C_:	
		ex de, hl
		call _LABEL_1C6_
		ex (sp), hl
		ex (sp), hl
_LABEL_472_:	
		ld a, (de)
		out (Port_VDPData), a
		inc de
		dec bc
		ld a, b
		or c
		jr nz, _LABEL_472_
		ret
	
_LABEL_47C_:	
		push af
		call _LABEL_1C6_
		ex (sp), hl
		ex (sp), hl
		pop af
		out (Port_VDPData), a
		ret
	
_LABEL_486_:	
		ex de, hl
		call _LABEL_1C6_
		ex (sp), hl
		ex (sp), hl
-:	
		ld a, (de)
		out (Port_VDPData), a
		ex (sp), hl
		ex (sp), hl
		xor a
		out (Port_VDPData), a
		inc de
		dec bc
		ld a, b
		or c
		jr nz, -
		ret
	
_LABEL_49B_:	
		ld a, $F5
		out (Port_IOPortControl), a
		in a, (Port_IOPort2)
		and $C0
		cp $C0
		jr nz, +
		ld a, $55
		out (Port_IOPortControl), a
		in a, (Port_IOPort2)
		and $C0
		or a
		jr nz, +
		ld a, $FF
		out (Port_IOPortControl), a
		ret
	
+:	
		xor a
		ret
	
_LABEL_4B9_:	
		ld b, $0A
--:	
		push bc
		ld bc, $3333
-:	
		dec bc
		ld a, b
		or c
		jr nz, -
		pop bc
		djnz --
		ret
	
_LABEL_4C8_:	
		call _LABEL_181_
		ld a, (_RAM_C115_)
		add a, a
		ld l, a
		ld h, $00
		ld de, _DATA_4DB_
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ex de, hl
		jp (hl)
	
; Jump Table from 4DB to 4E2 (4 entries, indexed by _RAM_C115_)	
_DATA_4DB_:	
	.dw _LABEL_120F_ _LABEL_12AF_ _LABEL_13CB_ _LABEL_1422_
	
_LABEL_4E3_:	
		ld (_RAM_C115_), a
		ld hl, _RAM_C116_
		ld de, _RAM_C116_ + 1
		ld bc, $0007
		ld (hl), $00
		ldir
		call _LABEL_AB1_
		ret
	
_LABEL_4F7_:	
		ld a, (_RAM_C0FF_)
		and $F0
		ret
	
--:	
		exx
		ld a, $F6
-:	
		ld (de), a
		djnz -
		ret
	
_LABEL_504_:	
		exx
		inc l
		inc l
		ld a, $F6
		ld (de), a
		inc e
		jp _LABEL_56D_
	
_LABEL_50E_:	
		ld a, (ix+13)
		and $3F
		ld e, a
		ld d, $00
		ld hl, $CA21
		add hl, de
		ex de, hl
		add hl, de
		ld a, $40
		add a, l
		ld l, a
		exx
		push ix
		pop hl
		ld c, l
		inc l
		inc l
		inc l
		ld a, (hl)
		inc l
		rla
		ld a, (hl)
		rla
		jr c, --
		neg
		add a, $DC
		ld b, a
		inc l
		inc l
		ld e, (hl)
		inc l
		ld d, (hl)
		rl e
		ld a, d
		adc a, a
		ld e, a
		ld l, c
		exx
-:	
		exx
		ld a, (hl)
		add a, b
		cp $D0
		jr z, _LABEL_504_
		exx
		ld (de), a
		exx
		inc l
		ld a, (hl)
		ld a, (hl)
		sra a
		add a, d
		cp $42
		jr c, _LABEL_504_
		cp $C2
		jr nc, _LABEL_504_
		ld a, (hl)
		add a, e
		add a, $7C
		exx
		ld (hl), a
		exx
		ld a, $0B
		add a, l
		ld l, a
		ld a, (hl)
		inc a
		jr z, _LABEL_504_
		dec a
		exx
		inc l
		ld (hl), a
		inc l
		inc e
_LABEL_56D_:	
		exx
		ld a, $20
		add a, c
		ld l, a
		ld c, l
		ld a, $00
		adc a, h
		ld h, a
		exx
		djnz -
		ret
	
_LABEL_57B_:	
		ld ix, (_RAM_C196_)
_LABEL_57F_:	
		ld a, (ix+13)
		and $3F
		ld e, a
		ld d, $00
		ld hl, $CA21
		add hl, de
		ex de, hl
		add hl, de
		ld a, $40
		add a, l
		ld l, a
-:	
		xor a
		ld (hl), a
		inc l
		ld (hl), a
		inc l
		ld a, $F6
		ld (de), a
		inc e
		djnz -
		ret
	
_LABEL_59D_:	
		ld hl, _RAM_CC3F_
		inc (hl)
		ld a, (hl)
		and $3F
		ld c, a
		ld e, a
		ld d, $00
		add a, a
		exx
		ld e, a
		ld d, $00
		bit 1, a
		jr nz, ++
		ld hl, _RAM_CA61_
		add hl, de
		ld de, _RAM_CB7F_
		exx
		ld hl, _RAM_CA21_
		add hl, de
		ld de, _RAM_CB3F_
		ld a, $40
		sub c
		ld b, a
		ld a, c
		ld c, b
		call +
		or a
		ret z
		ld hl, _RAM_CA61_
		exx
		ld hl, _RAM_CA21_
		ld c, a
+:	
		ld b, $00
		push bc
		ldir
		exx
		pop bc
		rlc c
		ldir
		ret
	
++:	
		ld hl, _RAM_CA61_
		add hl, de
		ld de, _RAM_CBFD_
		exx
		ld hl, _RAM_CA21_
		add hl, de
		ld de, _RAM_CB7E_
		ld a, $40
		sub c
		ld b, a
		ld a, c
		call +
		or a
		ret z
		exx
		ld hl, _RAM_CA61_
		exx
		ld hl, _RAM_CA21_
		ld b, a
+:	
		ld c, $FF
-:	
		ldi
		dec e
		dec e
		exx
		ldi
		ldi
		dec e
		dec e
		dec e
		dec e
		exx
		djnz -
		ret
	
_LABEL_614_:	
		ld hl, _RAM_CB3F_
		ld de, $3F00
		ld bc, $0040
		call _LABEL_2AD_
		ld hl, _RAM_CB7F_
		ld de, $3F80
		ld bc, $0080
		call _LABEL_2AD_
		ret
	
_LABEL_62D_:	
		ld hl, $2000
		xor a
		ld bc, $0800
		call _LABEL_202_
		ld hl, _RAM_CA21_
		ld de, _RAM_CA21_ + 1
		ld bc, $003F
		ld (hl), $F6
		ldir
		ld hl, _RAM_CA61_
		ld de, _RAM_CA61_ + 1
		ld bc, $007F
		ld (hl), $00
		ldir
		call _LABEL_59D_
		ld hl, _RAM_CA21_
		ld de, $3F00
		ld bc, $0040
		call _LABEL_46C_
		ld hl, _RAM_CA61_
		ld de, $3F80
		ld bc, $0080
		call _LABEL_46C_
		ret
	
_LABEL_66D_:	
		xor a
		ld (_RAM_C194_), a
_LABEL_671_:	
		ld a, (_RAM_C194_)
		add a, a
		add a, a
		ld c, a
		add a, a
		add a, c
		ld c, a
		ld b, $00
		ld hl, _RAM_C1A1_
		add hl, bc
		ld a, (hl)
		or a
		jr z, ++
		add a, a
		push af
		add a, a
		ld c, a
		rl b
		pop af
		jr c, +
		set 7, (hl)
		inc c
		inc c
+:	
		push bc
		ld de, _RAM_C195_
		call _LABEL_708_
		pop bc
		ld hl, _DATA_721_
		add hl, bc
		ld a, (hl)
		inc hl
		ld h, (hl)
		ld l, a
		ld de, +	; Overriding return address
		push de
		ld ix, (_RAM_C196_)
		jp (hl)
	
+:	
		ld a, (_RAM_C194_)
		add a, a
		add a, a
		ld c, a
		add a, a
		add a, c
		ld c, a
		ld b, $00
		ld hl, $C1A1
		add hl, bc
		ex de, hl
		ld hl, _RAM_C195_
		call _LABEL_708_
++:	
		ld hl, _RAM_C194_
		inc (hl)
		ld a, (hl)
		cp $0A
		jr c, _LABEL_671_
		ld (hl), $00
		ret
	
; 1st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6CC_:	
		ld hl, _RAM_C195_
		ld de, _RAM_C195_ + 1
		ld bc, $000B
		ld (hl), $00
		ldir
		ret
	
; 221st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6DA_:	
		push af
		ld a, $80
		ld (_RAM_C195_), a
		pop af
		ret
	
_LABEL_6E2_:	
		push af
		push bc
		push de
		push hl
		ld hl, _RAM_C1A1_
		ld de, $000C
		ld b, $0A
		ld c, a
		xor a
-:	
		cp (hl)
		jr z, +
		add hl, de
		djnz -
		pop hl
		pop de
		pop bc
		pop af
		scf
		ret
	
+:	
		ld (hl), c
		pop de
		inc hl
		ld (hl), e
		inc hl
		ld (hl), d
		pop hl
		ex de, hl
		pop bc
		pop af
		or a
		ret
	
_LABEL_708_:	
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ret
	
; Jump Table from 721 to 904 (242 entries, indexed by _RAM_C1A1_)	
_DATA_721_:	
	.dw _LABEL_6CC_ _LABEL_6CC_ _LABEL_21A4_ _LABEL_217F_ _LABEL_2401_ _LABEL_2381_ _LABEL_2401_ _LABEL_2397_
	.dw _LABEL_24DC_ _LABEL_24C8_ _LABEL_2538_ _LABEL_2513_ _LABEL_25D2_ _LABEL_25B3_ _LABEL_28BB_ _LABEL_289B_
	.dw _LABEL_279E_ _LABEL_2766_ _LABEL_279E_ _LABEL_2782_ _LABEL_279E_ _LABEL_2790_ _LABEL_279E_ _LABEL_2774_
	.dw _LABEL_2997_ _LABEL_2977_ _LABEL_2937_ _LABEL_2915_ _LABEL_29C6_ _LABEL_29A6_ _LABEL_2968_ _LABEL_2946_
	.dw _LABEL_2A63_ _LABEL_2A39_ _LABEL_2A63_ _LABEL_29EC_ _LABEL_2A63_ _LABEL_29FC_ _LABEL_2A63_ _LABEL_2A0B_
	.dw _LABEL_2A63_ _LABEL_2A1A_ _LABEL_2C06_ _LABEL_2BDF_ _LABEL_2C4D_ _LABEL_2C2F_ _LABEL_2C9C_ _LABEL_2C7E_
	.dw _LABEL_2CE0_ _LABEL_2CC0_ _LABEL_307B_ _LABEL_2FED_ _LABEL_301D_ _LABEL_2FC2_ _LABEL_3252_ _LABEL_31FF_
	.dw _LABEL_3252_ _LABEL_3218_ _LABEL_2F78_ _LABEL_2F4B_ _LABEL_2F78_ _LABEL_2F4F_ _LABEL_2F78_ _LABEL_2F53_
	.dw _LABEL_2F78_ _LABEL_2F57_ _LABEL_2FB1_ _LABEL_2F8F_ _LABEL_53A1_ _LABEL_5379_ _LABEL_2D21_ _LABEL_2D04_
	.dw _LABEL_54A3_ _LABEL_545E_ _LABEL_64E3_ _LABEL_64C4_ _LABEL_5592_ _LABEL_5576_ _LABEL_563B_ _LABEL_561B_
	.dw _LABEL_4C5B_ _LABEL_4B98_ _LABEL_2E30_ _LABEL_2E0D_ _LABEL_2E4A_ _LABEL_2E0D_ _LABEL_2E64_ _LABEL_2E0D_
	.dw _LABEL_2E7E_ _LABEL_2E0D_ _LABEL_2E98_ _LABEL_2E0D_ _LABEL_2EB2_ _LABEL_2E0D_ _LABEL_4DCC_ _LABEL_4DA9_
	.dw _LABEL_334F_ _LABEL_3328_ _LABEL_3419_ _LABEL_3403_ _LABEL_5712_ _LABEL_56C3_ _LABEL_68CD_ _LABEL_67FA_
	.dw _LABEL_3252_ _LABEL_31C7_ _LABEL_23CF_ _LABEL_2360_ _LABEL_6B95_ _LABEL_6B6E_ _LABEL_3252_ _LABEL_31E3_
	.dw _LABEL_549E_ _LABEL_545E_ _LABEL_58F4_ _LABEL_58BE_ _LABEL_5BF4_ _LABEL_5BD0_ _LABEL_5A22_ _LABEL_59C4_
	.dw _LABEL_5B31_ _LABEL_5B0F_ _LABEL_3482_ _LABEL_3436_ _LABEL_26FC_ _LABEL_26DD_ _LABEL_27C2_ _LABEL_27A2_
	.dw _LABEL_379A_ _LABEL_3725_ _LABEL_3506_ _LABEL_34E3_ _LABEL_5CAA_ _LABEL_5C84_ _LABEL_6590_ _LABEL_6571_
	.dw _LABEL_664E_ _LABEL_6611_ _LABEL_5CFB_ _LABEL_5CD5_ _LABEL_58E2_ _LABEL_58BE_ _LABEL_664E_ _LABEL_6620_
	.dw _LABEL_35F4_ _LABEL_35A9_ _LABEL_35BD_ _LABEL_356E_ _LABEL_35BD_ _LABEL_357E_ _LABEL_35BD_ _LABEL_3591_
	.dw _LABEL_35BD_ _LABEL_3594_ _LABEL_49A5_ _LABEL_4967_ _LABEL_5D55_ _LABEL_5D2D_ _LABEL_664E_ _LABEL_662F_
	.dw _LABEL_64E3_ _LABEL_64BF_ _LABEL_2D61_ _LABEL_2D42_ _LABEL_2D61_ _LABEL_2D42_ _LABEL_5A22_ _LABEL_59B4_
	.dw _LABEL_6C24_ _LABEL_6C07_ _LABEL_6701_ _LABEL_66E2_ _LABEL_6C7B_ _LABEL_6C7B_ _LABEL_4E9F_ _LABEL_4E79_
	.dw _LABEL_5DEE_ _LABEL_5D8C_ _LABEL_4E9F_ _LABEL_4E79_ _LABEL_4F16_ _LABEL_4EF6_ _LABEL_4FA3_ _LABEL_4F65_
	.dw _LABEL_279E_ _LABEL_272A_ _LABEL_279E_ _LABEL_2748_ _LABEL_36C2_ _LABEL_3689_ _LABEL_36A9_ _LABEL_3672_
	.dw _LABEL_2ECC_ _LABEL_2E0D_ _LABEL_5D74_ _LABEL_5D2D_ _LABEL_6C96_ _LABEL_6C96_ _LABEL_28CA_ _LABEL_289B_
	.dw _LABEL_5492_ _LABEL_545E_ _LABEL_5499_ _LABEL_545E_ _LABEL_570D_ _LABEL_56C3_ _LABEL_5CA5_ _LABEL_5C84_
	.dw _LABEL_677E_ _LABEL_675C_ _LABEL_37FB_ _LABEL_37DD_ _LABEL_38A5_ _LABEL_38A0_ _LABEL_2EEA_ _LABEL_2E0D_
	.dw _LABEL_3904_ _LABEL_38FC_ _LABEL_2A63_ _LABEL_29D5_ _LABEL_6DA_ _LABEL_4E6D_ _LABEL_3904_ _LABEL_3901_
	.dw _LABEL_53A1_ _LABEL_5374_ _LABEL_2F04_ _LABEL_2E0D_ _LABEL_6DA_ _LABEL_3907_ _LABEL_6DA_ _LABEL_3916_
	.dw _LABEL_6DA_ _LABEL_391E_ _LABEL_6DA_ _LABEL_3922_ _LABEL_6DA_ _LABEL_3926_ _LABEL_6DA_ _LABEL_392A_
	.dw $8D01 _LABEL_209_
	
	; Data from 905 to 92A (38 bytes)
	.db $8F $09 $03 $93 $09 $04 $99 $09 $0C $19 $0A $06 $A1 $09 $09 $AD
	.db $09 $0C $D1 $09 $08 $E9 $09 $10 $F9 $09 $07 $31 $0A $06 $31 $0A
	.db $02 $71 $0A $08 $75 $0A
	
_LABEL_92B_:	
		ld e, a
		add a, a
		add a, e
		ld e, a
		ld d, $00
		ld hl, $0901
		add hl, de
		ld b, (hl)
		inc hl
		ld a, (hl)
		inc hl
		ld h, (hl)
		ld l, a
		ld a, b
		exx
		call +
		ret c
		exx
		call +++
		exx
		or a
		ret
	
+:	
		ld b, $24
		ld hl, _RAM_CA02_
		ld de, $FFE0
		ld c, a
-:	
		bit 7, (hl)
		jr nz, +
		dec a
		jr nz, ++
		dec hl
		dec hl
		ld de, (_RAM_C196_)
		ld (_RAM_C196_), hl
		ld a, $1B
		add a, b
		ret
	
+:	
		ld a, c
++:	
		add hl, de
		djnz -
		scf
		ret
	
+++:	
		ld de, (_RAM_C196_)
		or $80
_LABEL_971_:	
		ex af, af'
		ld a, $80
		ex af, af'
-:	
		push bc
		ldi
		ldi
		ex de, hl
		ex af, af'
		ld (hl), a
		set 6, a
		ex af, af'
		ld bc, $000B
		add hl, bc
		ld (hl), a
		ld c, $13
		add hl, bc
		ex de, hl
		pop bc
		djnz -
		ret
	
	; Data from 98D to 9A0 (20 bytes)
	.db $00 $00 $00 $FC $00 $04 $00 $F8 $00 $00 $00 $08 $FC $FC $FC $04
	.db $04 $FC $04 $04
	
; Data from 9A1 to 9AC (12 bytes)	
_DATA_9A1_:	
	.db $F8 $FC $F8 $04 $00 $FC $00 $04 $08 $FC $08 $04
	
; Data from 9AD to 9D0 (36 bytes)	
_DATA_9AD_:	
	.db $F8 $F8 $F8 $00 $F8 $08 $00 $F8 $00 $00 $00 $08 $08 $F8 $08 $00
	.db $08 $08 $F4 $F4 $F4 $00 $F4 $0C $00 $F4 $00 $00 $00 $0C $0C $F4
	.db $0C $00 $0C $0C
	
; Data from 9D1 to 9E8 (24 bytes)	
_DATA_9D1_:	
	.db $F4 $F8 $F4 $00 $F4 $08 $FC $F8 $FC $00 $FC $08 $04 $F8 $04 $00
	.db $04 $08 $0C $F8 $0C $00 $0C $08
	
; Data from 9E9 to A3E (86 bytes)	
_DATA_9E9_:	
	.db $00 $E4 $00 $EC $00 $F4 $00 $FC $00 $04 $00 $0C $00 $14 $00 $1C
	.db $FC $E4 $FC $EC $FC $F4 $FC $FC $FC $04 $FC $0C $FC $14 $FC $1C
	.db $04 $E4 $04 $EC $04 $F4 $04 $FC $04 $04 $04 $0C $04 $14 $04 $1C
	.db $FC $F4 $FC $F8 $FC $FC $FC $04 $FC $08 $FC $0C $04 $F4 $04 $F8
	.db $04 $FC $04 $04 $04 $08 $04 $0C $FC $F8 $FC $00 $FC $08 $04 $F8
	.db $04 $00 $04 $08 $0C $00
	
; Data from A3F to A74 (54 bytes)	
_DATA_A3F_:	
	.db $F0 $F0 $F0 $F8 $F0 $00 $F0 $08 $F0 $10 $F8 $F0 $F8 $F8 $F8 $00
	.db $F8 $08 $F8 $10 $00 $F0 $00 $F8 $00 $00 $00 $08 $00 $10 $08 $F0
	.db $08 $F8 $08 $00 $08 $08 $08 $10 $10 $F0 $10 $F8 $10 $00 $10 $08
	.db $10 $10 $FC $00 $04 $00
	
; Data from A75 to A84 (16 bytes)	
_DATA_A75_:	
	.db $F4 $FC $F4 $04 $FC $FC $FC $04 $04 $FC $04 $04 $0C $FC $0C $04
	
; Data from A85 to A98 (20 bytes)	
_DATA_A85_:	
	.db $FA $FA $FA $06 $06 $E4 $06 $EC $06 $F4 $06 $FC $06 $04 $06 $0C
	.db $06 $14 $06 $1C
	
_LABEL_A99_:	
		push hl
		push bc
		ld hl, _RAM_C002_
		ld b, $03
-:	
		cp (hl)
		jr z, ++
		ld c, (hl)
		inc c
		dec c
		jr nz, +
		ld (hl), a
		jr ++
	
+:	
		inc hl
		djnz -
++:	
		pop bc
		pop hl
		ret
	
_LABEL_AB1_:	
		push af
		push ix
		push hl
		push de
		push bc
		ld hl, _RAM_C002_
		xor a
		ld (hl), a
		inc hl
		ld (hl), a
		inc hl
		ld (hl), a
		call _LABEL_7159_
		pop bc
		pop de
		pop hl
		pop ix
		pop af
		ret
	
_LABEL_ACA_:	
		ld bc, $0008
		call _LABEL_3B_
		ld bc, $0009
		call _LABEL_3B_
		ret
	
_LABEL_AD7_:	
		ld bc, $0084
		ld hl, _RAM_C194_
		ld de, _RAM_C194_ + 1
		ld (hl), $00
		ldir
		ret
	
_LABEL_AE5_:	
		ld bc, $1238
		ld hl, _RAM_C163_
		ld de, _RAM_C163_ + 1
		ld (hl), $00
		ldir
		ret
	
_LABEL_AF3_:	
		ld hl, _RAM_C220_
		ld de, _RAM_C220_ + 1
		ld bc, $07FF
		ld (hl), $00
		ldir
		ret
	
_LABEL_B01_:	
		ld a, $03
		call _LABEL_B13_
		ld hl, _DATA_DB6A_
		ld de, $0020
		ld bc, $05E0
		call _LABEL_422_
		ret
	
_LABEL_B13_:	
		ld (_RAM_FFFF_), a
		ret
	
_LABEL_B17_:	
		ld a, (_RAM_DFFF_)
		ret
	
_LABEL_B1B_:	
		ld hl, _RAM_C11E_
		ld de, _RAM_C11E_ + 1
		ld bc, $127D
		ld (hl), $00
		ldir
		ld a, $03
		ld (_RAM_C12D_), a
		ld (_RAM_C143_), a
		ld (_RAM_C159_), a
		ret
	
_LABEL_B34_:	
		ld hl, _RAM_C520_
		ld de, _RAM_C520_ + 1
		ld bc, $04FF
		jr +
	
_LABEL_B3F_:	
		ld hl, _RAM_C4A0_
		ld de, _RAM_C4A0_ + 1
		ld bc, $057F
+:	
		ld (hl), $00
		ldir
		ld hl, _RAM_C1A1_
		ld de, _RAM_C1A1_ + 1
		ld bc, $0077
		ld (hl), $00
		ldir
		ld ix, _RAM_C4A0_
		ld (ix+13), $14
		ld b, $2C
		call _LABEL_57F_
		ret
	
_LABEL_B67_:	
		push bc
		ld a, $04
		call _LABEL_B13_
		ld a, c
		add a, a
		add a, a
		ld l, a
		ld h, $00
		ld de, _DATA_F9B_
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		inc hl
		ld a, (hl)
		inc hl
		ld h, (hl)
		ld l, a
		ld c, (hl)
		inc hl
		ld b, $00
		di
		call _LABEL_486_
		ei
		pop bc
		ret
	
_LABEL_B8A_:	
		ld hl, _DATA_BF1_
		ld de, _RAM_D373_
		ld bc, $0011
		ldir
		ld hl, _DATA_C02_
		ld de, _RAM_D384_
		ld bc, $0011
		ldir
		ld hl, _RAM_C114_
		bit 6, (hl)
		ld a, $02
		jr z, +
		ld a, $03
+:	
		ld (_RAM_D38C_), a
		ld hl, _RAM_D383_
		ld de, _RAM_C111_
		call +
		ld hl, _RAM_D394_
		ld de, _RAM_C121_
		call +
		ld c, $31
		call _LABEL_B67_
		inc c
		call _LABEL_B67_
		ret
	
+:	
		ld (hl), $01
		dec hl
		ld b, $03
-:	
		ld a, (de)
		ld c, a
		and $0F
		add a, $01
		ld (hl), a
		dec hl
		ld a, c
		rrca
		rrca
		rrca
		rrca
		and $0F
		add a, $01
		ld (hl), a
		dec hl
		inc de
		djnz -
		ld a, $01
		ld bc, $0600
-:	
		inc hl
		cp (hl)
		ret nz
		ld (hl), c
		djnz -
		ret
	
; Data from BF1 to C01 (17 bytes)	
_DATA_BF1_:	
	.db $10 $2C $0D $0E $0F $0D $2C
	.dsb 10, $00
	
; Data from C02 to C08 (7 bytes)	
_DATA_C02_:	
	.db $10 $0C $1A $15 $17 $13 $20
	
; 3rd entry of Pointer Table from 7D1A (indexed by unknown)	
; Data from C09 to C12 (10 bytes)	
_DATA_C09_:	
	.dsb 10, $00
	
_LABEL_C13_:	
		ret
	
_LABEL_C14_:	
		push af
		push bc
		push de
		push hl
		ld c, a
		dec c
		call _LABEL_B17_
		push af
		ld b, $00
		ld l, c
		ld h, b
		add hl, hl
		add hl, hl
		add hl, bc
		ld bc, _DATA_C6A_
		add hl, bc
		ld a, (hl)
		call _LABEL_B13_
		inc hl
		ld c, (hl)
		inc hl
		ld b, (hl)
		inc hl
		push hl
		ld l, c
		ld h, $00
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		ld c, l
		ld l, b
		ld b, h
		ld h, $00
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		ld de, $2000
		add hl, de
		ex de, hl
		pop hl
		ld a, (hl)
		inc hl
		ld h, (hl)
		ld l, a
		ld a, (_RAM_CC84_)
		or a
		jr z, +
		di
+:	
		call _LABEL_46C_
		ld a, (_RAM_CC84_)
		or a
		jr z, +
		ei
+:	
		pop af
		call _LABEL_B13_
		pop hl
		pop de
		pop bc
		pop af
		inc a
		ret
	
; Data from C6A to D09 (160 bytes)	
_DATA_C6A_:	
	.db $04 $09 $18 $00 $80 $04 $09 $18 $20 $81 $04 $09 $18 $40 $82 $04
	.db $09 $18 $60 $83 $04 $09 $18 $80 $84 $04 $09 $18 $A0 $85 $04 $09
	.db $18 $C0 $86 $04 $09 $18 $E0 $87 $04 $09 $18 $00 $89 $04 $09 $18
	.db $20 $8A $04 $0F $21 $40 $8B $04 $0A $30 $20 $8D $04 $0F $21 $60
	.db $8E $04 $0A $30 $40 $90 $04 $0F $21 $00 $94 $04 $0A $30 $E0 $95
	.db $04 $0F $21 $20 $97 $04 $0A $30 $00 $99 $03 $06 $AB $9B $AD $03
	.db $06 $B1 $5B $AE $04 $0C $AB $8D $BA $04 $0C $7F $0D $B9 $03 $0C
	.db $8B $1B $AC $03 $04 $30 $BB $A7 $03 $04 $30 $BB $A8 $04 $04 $2C
	.db $AD $AE $04 $04 $1C $2D $AF $04 $04 $28 $AD $B0 $04 $04 $20 $2D
	.db $B0 $04 $04 $24 $AD $AF $04 $04 $48 $BD $A6 $04 $08 $40 $0D $BC
	
; 3rd entry of Pointer Table from 777A (indexed by unknown)	
; Data from D0A to F5C (595 bytes)	
_DATA_D0A_:	
	.db $04 $02 $34 $CD $AC $04 $02 $36 $0D $AD $04 $02 $38 $4D $AD $04
	.db $02 $3A $8D $AD $04 $03 $20 $4D $AE $04 $08 $54 $0D $BD $04 $08
	.db $5C $0D $BE $07 $08 $54 $98 $B0 $07 $08 $5C $98 $B1 $07 $08 $54
	.db $98 $B2 $07 $08 $5C $98 $B3 $07 $08 $54 $98 $B4 $07 $08 $5C $98
	.db $B5 $07 $08 $54 $98 $B6 $07 $08 $5C $98 $B7 $07 $08 $54 $98 $B8
	.db $07 $08 $5C $98 $B9 $03 $08 $64 $00 $80 $03 $07 $6C $00 $81 $03
	.db $06 $64 $1B $AF $03 $06 $6A $DB $AF $03 $06 $70 $9B $B0 $03 $06
	.db $76 $5B $B1 $03 $06 $7C $1B $B2 $03 $06 $82 $DB $B2 $03 $0C $81
	.db $DB $BB $03 $06 $A9 $DB $B2 $03 $06 $AF $9B $B3 $03 $06 $B5 $5B
	.db $B4 $03 $0C $8B $9B $BD $03 $12 $97 $9B $B9 $03 $12 $A9 $5B $B7
	.db $03 $09 $97 $1B $B5 $03 $09 $A0 $3B $B6 $03 $02 $74 $5B $BD $03
	.db $04 $72 $1B $BF $04 $08 $4C $3D $A5 $04 $02 $24 $80 $91 $04 $02
	.db $29 $C0 $91 $04 $02 $2E $00 $92 $04 $02 $24 $40 $92 $04 $02 $29
	.db $80 $92 $04 $02 $2E $C0 $92 $04 $02 $24 $40 $9A $04 $02 $29 $80
	.db $9A $04 $02 $2E $C0 $9A $04 $02 $24 $00 $9B $04 $02 $29 $40 $9B
	.db $04 $02 $2E $80 $9B $04 $0C $3A $00 $93 $04 $08 $80 $3D $A8 $04
	.db $08 $AB $3D $A7 $04 $0C $8B $3D $A9 $03 $09 $76 $E0 $81 $02 $0C
	.db $64 $C0 $98 $07 $04 $27 $F8 $AB $07 $03 $2B $78 $AC $07 $04 $27
	.db $D8 $AC $07 $03 $2B $58 $AD $07 $04 $27 $B8 $AD $07 $03 $2B $38
	.db $AE $07 $04 $27 $98 $AE $07 $03 $2B $18 $AF $07 $09 $27 $78 $AF
	.db $02 $0C $97 $C0 $9B $05 $08 $A3 $F8 $BD $04 $06 $8B $2D $B1 $04
	.db $06 $91 $ED $B1 $04 $01 $80 $ED $B3 $03 $04 $BC $9B $AB $03 $04
	.db $30 $1B $AB $04 $06 $3A $C0 $9B $04 $06 $3A $80 $9C $04 $06 $20
	.db $2D $B3 $04 $04 $20 $AD $B2 $04 $10 $64 $CD $AA $03 $04 $1C $BB
	.db $9E $03 $04 $20 $3B $9F $03 $04 $24 $BB $9F $03 $04 $28 $3B $A0
	.db $03 $04 $2C $BB $A0 $03 $04 $30 $3B $A1 $03 $04 $34 $BB $A1 $03
	.db $04 $38 $3B $A2 $03 $04 $3C $BB $A2 $03 $04 $40 $3B $A3 $03 $04
	.db $44 $BB $A3 $03 $04 $48 $3B $A4 $04 $02 $3E $CD $AD $04 $02 $3C
	.db $0D $AE $03 $04 $30 $3B $A5 $03 $04 $30 $BB $A5 $03 $04 $30 $3B
	.db $A6 $03 $04 $30 $BB $A6 $03 $04 $30 $3B $A7 $03 $09 $A9 $8A $97
	.db $03 $0A $B2 $AA $98 $04 $04 $1C $0D $B4 $04 $04 $1C $8D $B4 $04
	.db $04 $1C $0D $B5 $04 $04 $1C $8D $B5 $04 $04 $1C $0D $B6 $04 $04
	.db $1C $8D $B6 $04 $04 $1C $0D $B7 $04 $04 $1C $8D $B7 $04 $04 $1C
	.db $0D $B8 $04 $04 $1C $8D $B8 $03 $0C $7F $EA $99 $04 $10 $7F $BD
	.db $A1 $03 $03 $30 $BB $A9 $03 $04 $30 $9B $AA $07 $06 $00 $98 $BA
	.db $07 $04 $06 $58 $BB $03 $04 $40 $BB $A4 $04 $0C $8B $BD $A3 $03
	.db $04 $30 $1B $AA $03 $04 $30 $3B $A8 $03 $04 $30 $3B $A9 $02 $0C
	.db $64 $40 $9A
	
_LABEL_F5D_:	
		ld hl, _RAM_CC5F_
		bit 6, (hl)
		res 6, (hl)
		ret nz
		ld de, $0000
		ld hl, (_RAM_CC10_)
		ld a, h
		or l
		jr z, +
		ld (_RAM_CC10_), de
		ld de, $2000
		jr ++
	
+:	
		ld hl, (_RAM_CC12_)
		ld a, h
		or l
		jp z, _LABEL_C13_
		ld (_RAM_CC12_), de
		ld de, $20C0
++:	
		call _LABEL_B17_
		push af
		ld a, $02
		call _LABEL_B13_
		ld bc, $00C0
		call _LABEL_2AD_
		pop af
		call _LABEL_B13_
		ret
	
; Data from F9B to F9C (2 bytes)	
_DATA_F9B_:	
	.db $22 $38
	
	; Pointer Table from F9D to F9E (1 entries, indexed by _RAM_C109_)
	.dw _DATA_11D40_
	
	; Data from F9F to FE0 (66 bytes)
	.db $60 $38 $46 $9D $9E $38 $4D $9D $DE $38 $55 $9D $14 $39 $5E $9D
	.db $54 $39 $6D $9D $94 $39 $7B $9D $C4 $39 $88 $9D $06 $3A $A3 $9D
	.db $46 $3A $BF $9D $84 $3A $DB $9D $C4 $3A $F8 $9D $02 $3B $16 $9E
	.db $42 $3B $35 $9E $08 $3C $54 $9E $48 $3C $6D $9E $88 $3C $86 $9E
	.db $C8 $3C
	
; 1st entry of Pointer Table from 7E49 (indexed by unknown)	
; Data from FE1 to 10C9 (233 bytes)	
_DATA_FE1_:	
	.db $9F $9E $14 $3D $B8 $9E $54 $3D $C5 $9E $94 $3D $D2 $9E $D4 $3D
	.db $DE $9E $48 $3E $EA $9E $80 $3E $81 $9F $DC $3E $03 $9F $00 $38
	.db $81 $9F $48 $38 $06 $9F $80 $38 $81 $9F $C0 $38 $81 $9F $08 $39
	.db $1F $9F $54 $39 $34 $9F $80 $39 $81 $9F $C4 $39 $49 $9F $04 $3A
	.db $64 $9F $40 $3A $81 $9F $80 $3A $81 $9F $D8 $38 $41 $CC $58 $3A
	.db $4A $CC $18 $3B $52 $CC $16 $3C $5A $CC $96 $38 $A2 $9F $0E $3B
	.db $AC $9F $24 $3B $B5 $9F $00 $38 $6B $10 $12 $38 $6F $10 $90 $38
	.db $87 $10 $10 $39 $9A $10 $90 $39 $AE $10 $10 $39 $A2 $10 $10 $39
	.db $73 $D3 $90 $39 $84 $D3 $18 $3B $C2 $10 $03 $B1 $9F $B4 $17 $B6
	.db $A1 $A9 $B0 $AD $00 $AB $B1 $9F $00 $A8 $B1 $B4 $00 $B4 $A2 $B5
	.db $AD $A2 $B0 $A3 $00 $AB $12 $AE $A2 $A8 $A7 $00 $B6 $B1 $00 $B4
	.db $A7 $B5 $A5 $9F $A7 $00 $AF $A7 $BD $07 $B1 $A1 $C2 $A4 $B1 $AB
	.db $C2 $0B $B1 $A1 $C2 $B6 $B1 $AF $C1 $B6 $B1 $AF $C2 $13 $A2 $00
	.db $AE $B1 $B8 $A7 $00 $AB $B1 $9F $00 $B5 $B1 $00 $AF $9F $A5 $A1
	.db $BF $07 $22 $0D $13 $00 $13 $1C $12
	
_LABEL_10CA_:	
		ld a, (_RAM_C129_)
		cp $04
		ret nc
		ld hl, $CC80
		rla
		ret c
		ld hl, _RAM_C18C_
		bit 0, (hl)
		jr nz, _LABEL_10E1_
		inc (hl)
		call _LABEL_119D_
		ret
	
_LABEL_10E1_:	
		ld a, $02
		call _LABEL_B13_
		xor a
		ld hl, (_RAM_C18D_)
		ld c, (hl)
		rl c
		ld a, c
		cp $FE
		ret z
		ld a, (_RAM_C12A_)
		cp c
		ret c
		ld a, (hl)
		rla
		jr c, _LABEL_1156_
		inc hl
		ld d, (hl)
		bit 7, d
		jr z, ++
		ld a, (_RAM_C128_)
		cp $20
		jr nc, +
		ld a, (_RAM_C114_)
		and $01
		jp nz, _LABEL_1198_
+:	
		res 7, d
++:	
		inc hl
		ld a, (_RAM_C128_)
		cp $28
		jr nz, ++
		ld a, (hl)
		and $03
		ld b, a
		ld a, (hl)
		and $FC
		and a
		jr z, +
		ld c, a
		ld a, (_RAM_C136_)
		cp c
		jr c, _LABEL_1190_
+:	
		ld a, b
		jp _LABEL_113E_
	
++:	
		ld a, (hl)
		and $E0
		and a
		jr z, _LABEL_113B_
		ld c, a
		ld a, (_RAM_C136_)
		cp c
		jr c, _LABEL_1190_
_LABEL_113B_:	
		ld a, (hl)
		and $1F
_LABEL_113E_:	
		exx
		ld e, a
		ld d, $00
		ld hl, (_RAM_C18F_)
		add hl, de
		ld a, (hl)
		exx
		inc hl
		ld (_RAM_C18D_), hl
		ld e, $D0
		ex de, hl
		call _LABEL_6E2_
		jp _LABEL_10E1_
	
	; Data from 1155 to 1155 (1 bytes)
	.db $C9
	
_LABEL_1156_:	
		inc hl
		ld d, (hl)
		bit 7, d
		jr z, ++
		ld a, (_RAM_C128_)
		cp $20
		jr nc, +
		ld a, (_RAM_C114_)
		and $01
		jr nz, _LABEL_1198_
+:	
		res 7, d
++:	
		inc hl
		ld a, (_RAM_C128_)
		cp $28
		jr nz, +
		ld a, (hl)
		and $03
		ld b, a
		ld a, (hl)
		and $FC
		ld c, a
		ld a, (_RAM_C136_)
		cp c
		jr nz, _LABEL_1190_
		ld a, b
		jp _LABEL_113E_
	
+:	
		ld a, (hl)
		and $E0
		ld c, a
		ld a, (_RAM_C136_)
		cp c
		jr z, _LABEL_113B_
_LABEL_1190_:	
		inc hl
		ld (_RAM_C18D_), hl
		jp _LABEL_10E1_
	
	; Data from 1197 to 1197 (1 bytes)
	.db $C9
	
_LABEL_1198_:	
		inc hl
		jp _LABEL_1190_
	
	; Data from 119C to 119C (1 bytes)
	.db $C9
	
_LABEL_119D_:	
		ld a, $02
		call _LABEL_B13_
		ld a, (_RAM_C128_)
		add a, a
		add a, a
		ld l, a
		ld h, $00
		ld a, (_RAM_C129_)
		ld e, a
		ld d, h
		add hl, de
		add hl, hl
		push hl
		ld de, _DATA_9EB6_
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ld (_RAM_C18D_), de
		pop hl
		ld de, _DATA_9D6E_
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ld (_RAM_C18F_), de
		ld a, (_RAM_C128_)
		cp $28
		jr nz, +
		ld a, (_RAM_C132_)
		and $FC
		jp ++
	
+:	
		ld hl, _DATA_11E7_
		ld a, (_RAM_C128_)
		ld e, a
		ld d, $00
		add hl, de
		ld a, (hl)
++:	
		ld (_RAM_C136_), a
		ret
	
; Data from 11E7 to 120E (40 bytes)	
_DATA_11E7_:	
	.db $00 $00 $00 $00 $00 $00 $00 $20 $00 $00 $40 $40 $00 $40 $00 $00
	.db $40 $00 $00 $60 $40 $20 $40 $80 $80 $40 $20 $A0 $00 $80 $40 $40
	.db $80 $80 $40 $C0 $40 $E0 $80 $E0
	
; 1st entry of Jump Table from 4DB (indexed by _RAM_C115_)	
_LABEL_120F_:	
		ld hl, _RAM_C116_
		bit 0, (hl)
		jr nz, _LABEL_126E_
		inc (hl)
		ld hl, _RAM_C120_
		ld (hl), $00
		call _LABEL_AB1_
		ld a, $02
		call _LABEL_A4_
		call _LABEL_B1B_
		call _LABEL_49B_
		or a
		jr z, +
		ld a, $01
		jp _LABEL_4E3_
	
+:	
		di
		call _LABEL_23B_
		call _LABEL_213_
		call _LABEL_26D_
		call _LABEL_27A_
		call _LABEL_62D_
		call _LABEL_28E_
		ld a, $35
		ld hl, $C000
		call _LABEL_406_
		ld a, $35
		ld hl, $C010
		call _LABEL_406_
		ld a, $05
		call _LABEL_B13_
		ld hl, _DATA_17D18_
		ld de, $0020
		ld bc, $00E0
		call _LABEL_1E4_
		call _LABEL_224_
		call _LABEL_24E_
		ei
		ret
	
_LABEL_126E_:	
		bit 1, (hl)
		jr nz, +
		set 1, (hl)
		ld a, $05
		call _LABEL_B13_
		ld hl, _DATA_17CF2_
		ld de, $3A8E
		ld bc, $0013
		di
		call _LABEL_486_
		ei
		ld hl, _DATA_17D05_
		ld de, $3ACE
		ld bc, $0013
		di
		call _LABEL_486_
		ei
		ret
	
+:	
		call _LABEL_4F7_
		jr z, +
		ld a, $01
		call _LABEL_4E3_
		ret
	
+:	
		ld hl, _RAM_C117_
		inc (hl)
		ld a, (hl)
		cp $90
		ret c
		ld a, $01
		call _LABEL_4E3_
		ret
	
; 2nd entry of Jump Table from 4DB (indexed by _RAM_C115_)	
_LABEL_12AF_:	
		ld hl, _RAM_C116_
		bit 0, (hl)
		jp nz, _LABEL_132D_
		call _LABEL_B1B_
		ld hl, _RAM_C116_
		set 0, (hl)
		ld hl, _RAM_C120_
		ld (hl), $00
		di
		call _LABEL_23B_
		call _LABEL_213_
		call _LABEL_26D_
		call _LABEL_27A_
		call _LABEL_28E_
		call _LABEL_62D_
		call _LABEL_ACA_
		ld a, $03
		call _LABEL_B13_
		ld hl, _DATA_C300_
		ld de, $0000
		ld bc, $1FE0
		call _LABEL_422_
		call _LABEL_49B_
		or a
		jr nz, +
		ld a, $07
		call _LABEL_B13_
		ld hl, _DATA_1FBD8_
		ld de, $2000
		call _LABEL_422_
		ld hl, _DATA_13AB_
		ld de, $3B98
		ld bc, $0010
		call _LABEL_46C_
		ld hl, _DATA_13BB_
		ld de, $3BD8
		ld bc, $0010
		call _LABEL_46C_
+:	
		ld bc, $1600
-:	
		call _LABEL_B67_
		inc c
		djnz -
		call _LABEL_224_
		call _LABEL_24E_
		ei
		ld a, $83
		call _LABEL_A99_
		ret
	
_LABEL_132D_:	
		call _LABEL_4F7_
		jr z, ++
		and $C0
		jr z, +
		ld a, $80
+:	
		push af
		call _LABEL_49B_
		and $01
		pop bc
		or b
		ld (_RAM_C114_), a
		call _LABEL_B1B_
		ld a, $03
		call _LABEL_4E3_
		ret
	
++:	
		ld hl, _RAM_C117_
		bit 0, (hl)
		jr nz, +
		inc hl
		inc (hl)
		ld a, (hl)
		cp $60
		ret nz
		inc hl
		inc (hl)
		ld a, (hl)
		dec a
		ret z
		ld (hl), $00
		dec hl
		ld (hl), $00
		dec hl
		inc (hl)
		ret
	
+:	
		bit 1, (hl)
		jr nz, ++
		ld hl, _RAM_C109_
		inc (hl)
		ld b, (hl)
		ld c, $09
		push bc
		di
		call _LABEL_3B_
		ei
		pop bc
		ld a, b
		and $07
		ret nz
		ld a, b
		rra
		rra
		rra
		and $1F
		add a, $15
		cp $23
		jr c, +
		ld hl, _RAM_C117_
		set 1, (hl)
+:	
		ld c, a
		call _LABEL_B67_
		ret
	
++:	
		inc hl
		inc (hl)
		ld a, (hl)
		cp $FF
		push af
		bit 1, a
		ld a, $04
		jr nz, +
		ld a, $10
+:	
		ld (_RAM_D396_), a
		pop af
		ret c
		ld a, $02
		call _LABEL_4E3_
		ret
	
; Data from 13AB to 13BA (16 bytes)	
_DATA_13AB_:	
	.db $00 $09 $01 $09 $04 $09 $05 $09 $08 $09 $09 $09 $0C $09 $0D $09
	
; Data from 13BB to 13CA (16 bytes)	
_DATA_13BB_:	
	.db $02 $09 $03 $09 $06 $09 $07 $09 $0A $09 $0B $09 $0E $09 $0F $09
	
; 3rd entry of Jump Table from 4DB (indexed by _RAM_C115_)	
_LABEL_13CB_:	
		ld hl, _RAM_C116_
		bit 0, (hl)
		jr nz, +
		inc (hl)
		call _LABEL_213_
		di
		call _LABEL_26D_
		call _LABEL_24E_
		call _LABEL_28E_
		call _LABEL_62D_
		ei
		call _LABEL_224_
		call _LABEL_AE5_
		ret
	
+:	
		call _LABEL_4F7_
		jr nz, +
		ld a, (_RAM_C120_)
		or a
		jr z, ++
		ld a, $00
		call _LABEL_4E3_
		ret
	
+:	
		ld a, $01
		call _LABEL_4E3_
		ret
	
++:	
		ld a, (_RAM_C10B_)
		rrca
		rrca
		rrca
		and $38
		bit 4, a
		ld c, $20
		jr nz, +
		ld c, $00
+:	
		bit 5, a
		jr z, +
		set 4, c
+:	
		and $08
		or c
		ld (_RAM_C11F_), a
		call _LABEL_14E3_
		ret
	
; 4th entry of Jump Table from 4DB (indexed by _RAM_C115_)	
_LABEL_1422_:	
		ld hl, _RAM_C116_
		bit 0, (hl)
		jr nz, +
		set 0, (hl)
		call _LABEL_B1B_
		ret
	
+:	
		ld a, (_RAM_C11E_)
		add a, a
		ld l, a
		ld h, $00
		ld de, _DATA_143F_
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ex de, hl
		jp (hl)
	
; Jump Table from 143F to 144A (6 entries, indexed by _RAM_C11E_)	
_DATA_143F_:	
	.dw _LABEL_145C_ _LABEL_14FF_ _LABEL_150E_ _LABEL_15C4_ _LABEL_1617_ _LABEL_161D_
	
_LABEL_144B_:	
		ld (_RAM_C11E_), a
		ld hl, _RAM_C163_
		ld de, _RAM_C163_ + 1
		ld bc, $0007
		ld (hl), $00
		ldir
		ret
	
; 1st entry of Jump Table from 143F (indexed by _RAM_C11E_)	
_LABEL_145C_:	
		ld hl, _RAM_C163_
		bit 0, (hl)
		jr nz, _LABEL_14A2_
		inc (hl)
		xor a
		ld (_RAM_C120_), a
		ld hl, _RAM_C114_
		bit 6, (hl)
		ld hl, _RAM_C137_
		jr z, +
		ld hl, _RAM_C14D_
+:	
		ld de, _RAM_C121_
		ld bc, $0016
		ldir
		call _LABEL_AF3_
		ld hl, _RAM_C12D_
		dec (hl)
		di
		call _LABEL_23B_
		call _LABEL_213_
		call _LABEL_62D_
		call _LABEL_28E_
		call _LABEL_27A_
		call _LABEL_B01_
		call _LABEL_ACA_
		call _LABEL_224_
		call _LABEL_24E_
		ei
		ret
	
_LABEL_14A2_:	
		ld a, (_RAM_C120_)
		or a
		jr z, _LABEL_14E3_
		ld c, $01
		bit 5, a
		jr nz, _LABEL_14C2_
		ld c, $03
		bit 4, a
		jr nz, _LABEL_14C2_
		ld c, $02
		bit 3, a
		jr nz, +
		ld a, (_RAM_C12D_)
		or a
		jr nz, _LABEL_14C2_
		ld c, $05
_LABEL_14C2_:	
		ld a, c
		call _LABEL_144B_
		ret
	
+:	
		ex af, af'
		ld a, (_RAM_D39A_)
		ld (_RAM_C129_), a
		ld a, (_RAM_D39B_)
		ld (_RAM_C128_), a
		ex af, af'
		bit 6, a
		jr z, _LABEL_14C2_
		ld a, (_RAM_C12D_)
		or a
		jr nz, _LABEL_14C2_
		ld c, $05
		jr _LABEL_14C2_
	
_LABEL_14E3_:	
		ld ix, $C220
		call _LABEL_3C9F_
		ld a, (_RAM_C16B_)
		rra
		ret nc
		call _LABEL_47A9_
		call _LABEL_10CA_
		call _LABEL_66D_
		call _LABEL_5E5E_
		call _LABEL_6375_
		ret
	
; 2nd entry of Jump Table from 143F (indexed by _RAM_C11E_)	
_LABEL_14FF_:	
		call _LABEL_5E8B_
		ld a, (_RAM_C163_)
		bit 7, a
		ret z
		ld a, $02
		call _LABEL_144B_
		ret
	
; 3rd entry of Jump Table from 143F (indexed by _RAM_C11E_)	
_LABEL_150E_:	
		call _LABEL_AB1_
		call _LABEL_AE5_
		ld a, (_RAM_C120_)
		bit 6, a
		jr nz, ++
		bit 3, a
		jr nz, +
		xor a
		ld (_RAM_C129_), a
		ld (_RAM_C12F_), a
		ld (_RAM_C131_), a
		ld hl, _RAM_C128_
		inc (hl)
+:	
		ld hl, _RAM_C12D_
		inc (hl)
		ld hl, _RAM_C127_
		res 1, (hl)
		res 3, (hl)
		jr +++
	
++:	
		xor a
		ld (_RAM_C127_), a
		ld a, (_RAM_C12A_)
		cp $16
		jr nc, +++
		ld a, (_RAM_C129_)
		or a
		jr z, +++
		dec a
		ld (_RAM_C129_), a
+++:	
		xor a
		ld (_RAM_C12A_), a
		ld hl, $0000
		ld (_RAM_C12B_), hl
		ld a, (_RAM_C114_)
		bit 6, a
		ld de, _RAM_C137_
		ld hl, _RAM_C121_
		jr z, +
		ld de, _RAM_C14D_
+:	
		ld bc, $0016
		ldir
		ld hl, _RAM_C114_
		ld a, (_RAM_C120_)
		bit 6, a
		jr z, ++++
		ld a, (_RAM_C12D_)
		or a
		jr nz, ++
		bit 7, (hl)
		jr z, +++++
		bit 6, (hl)
		jr nz, +
		set 4, (hl)
		bit 5, (hl)
		jr nz, +++++
		jr +++
	
+:	
		set 5, (hl)
		bit 4, (hl)
		jr nz, +++++
		jr +++
	
++:	
		bit 7, (hl)
		jr z, ++++
		bit 6, (hl)
		jr nz, +
		bit 5, (hl)
		jr nz, ++++
		jr +++
	
+:	
		bit 4, (hl)
		jr nz, ++++
+++:	
		ld a, $40
		xor (hl)
		ld (hl), a
		jr ++++
	
++++:	
		ld hl, _RAM_C120_
		ld (hl), $00
		ld a, $00
		call _LABEL_144B_
		ret
	
+++++:	
		ld hl, _RAM_C120_
		ld (hl), $00
		ld a, $04
		call _LABEL_144B_
		ret
	
; 4th entry of Jump Table from 143F (indexed by _RAM_C11E_)	
_LABEL_15C4_:	
		ld hl, _RAM_C163_
		bit 0, (hl)
		jr nz, +
		inc (hl)
		xor a
		ld (_RAM_C128_), a
		ld (_RAM_C12D_), a
		call _LABEL_AB1_
		di
		call _LABEL_23B_
		call _LABEL_213_
		call _LABEL_62D_
		call _LABEL_28E_
		call _LABEL_27A_
		call _LABEL_B01_
		call _LABEL_ACA_
		call _LABEL_224_
		call _LABEL_24E_
		ei
		call _LABEL_B8A_
		ld c, $33
		call _LABEL_B67_
		ret
	
+:	
		ld a, (_RAM_C11F_)
		and $30
		jr nz, +
		inc hl
		inc (hl)
		ret nz
		inc hl
		inc (hl)
		ld a, (hl)
		cp $02
		ret c
+:	
		ld hl, _RAM_C120_
		ld (hl), $40
		ld a, $02
		call _LABEL_144B_
		ret
	
; 5th entry of Jump Table from 143F (indexed by _RAM_C11E_)	
_LABEL_1617_:	
		ld a, $00
		call _LABEL_4E3_
		ret
	
; 6th entry of Jump Table from 143F (indexed by _RAM_C11E_)	
_LABEL_161D_:	
		ld hl, _RAM_C163_
		bit 0, (hl)
		jr nz, _LABEL_167B_
		inc (hl)
		inc hl
		ld a, (_RAM_C11F_)
		and $3C
		ld (hl), a
		call _LABEL_AD7_
		call _LABEL_AF3_
		call _LABEL_50D0_
		ld hl, _RAM_C3A4_
		ld (hl), $48
		inc hl
		inc hl
		inc hl
		ld (hl), $6C
		ld a, $33
		call _LABEL_6E2_
		ld hl, _RAM_D395_
		ld bc, $0005
		ld de, _RAM_D395_ + 1
		ld (hl), $00
		ldir
		di
		call _LABEL_23B_
		call _LABEL_213_
		call _LABEL_62D_
		call _LABEL_28E_
		call _LABEL_27A_
		call _LABEL_B01_
		call _LABEL_ACA_
		call _LABEL_224_
		call _LABEL_24E_
		ei
		call _LABEL_B8A_
		ld bc, $0328
-:	
		call _LABEL_B67_
		inc c
		djnz -
		ret
	
_LABEL_167B_:	
		inc hl
		ld a, (_RAM_C11F_)
		and $3C
		cp (hl)
		jr z, ++
		ld c, a
		and $30
		jr z, +
		dec hl
		bit 7, (hl)
		jr z, +++
		ld hl, _RAM_C121_
		ld de, _RAM_C121_ + 1
		ld bc, $0015
		ld (hl), $00
		ldir
		jr +++
	
+:	
		ld a, c
		xor (hl)
		xor (hl)
		and $0C
		ld (hl), c
		jr z, ++
		dec hl
		ld a, $80
		xor (hl)
		ld (hl), a
		rla
		ld a, $6C
		jr nc, +
		ld a, $94
+:	
		ld (_RAM_C3A7_), a
++:	
		call _LABEL_66D_
		ld hl, _RAM_C170_
		inc (hl)
		call _LABEL_4FDC_
		ld a, (_RAM_C647_)
		cp $38
; 1st entry of Jump Table from 67BF (indexed by _RAM_C199_)	
_LABEL_16C3_:	
		ret nc
		ld a, $02
		call _LABEL_144B_
		ret
	
+++:	
		xor a
		ld (_RAM_C129_), a
		ld (_RAM_C127_), a
		ld (_RAM_C12E_), a
		ld hl, _RAM_C121_
		ld de, _RAM_C121_ + 1
		ld bc, $0005
		ld (hl), a
		ldir
		ld a, $03
		ld (_RAM_C12D_), a
		ld a, $02
		call _LABEL_144B_
		call _LABEL_AE5_
		call _LABEL_AF3_
		di
		call _LABEL_62D_
		ei
		ret
	
	; Data from 16F6 to 16F6 (1 bytes)
	.db $FF
	
_LABEL_16F7_:	
		call _LABEL_1F31_
		call _LABEL_ACA_
		ld hl, $3800
		ld (_RAM_CC1C_), hl
		ld (_RAM_CC29_), hl
		ld a, $00
		ld (_RAM_CC1E_), a
		ld (_RAM_CC88_), a
		ld (_RAM_CCA7_), a
		ld (_RAM_CC87_), a
		ld (_RAM_CB39_), a
		ld (_RAM_CC86_), a
		ld hl, $0000
		ld (_RAM_CB3B_), hl
		ld (_RAM_CB3D_), hl
		ld a, (_RAM_C129_)
		add a, a
		add a, a
		add a, a
		ld l, $00
		ld h, a
		ld (_RAM_C12B_), hl
		ld a, $07
		call _LABEL_B13_
		ld hl, _DATA_1C000_
		ld a, (_RAM_C128_)
		ld e, a
		ld d, $00
		add hl, de
		ld e, (hl)
		ld a, e
		cp $01
		jp nz, +
		jp ++
	
+:	
		cp $02
		jp nz, +++
++:	
		ld hl, _RAM_CCA7_
		set 0, (hl)
+++:	
		rlc e
		rlc e
		rlc e
		ld hl, _DATA_1C2D5_
		add hl, de
		ld a, (_RAM_C129_)
		ld e, a
		rlc e
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ld (_RAM_CC17_), de
		ld a, $40
		ld (_RAM_CC21_), a
		ld a, $04
		jp _LABEL_1796_
	
_LABEL_1774_:	
		call _LABEL_10CA_
		call _LABEL_66D_
		call _LABEL_3F09_
		ld a, (_RAM_CC21_)
		dec a
		ld (_RAM_CC21_), a
		jp z, +
		ld hl, $0000
		ld (_RAM_CB3B_), hl
		ld (_RAM_CB3D_), hl
		ld a, $04
		jp _LABEL_1796_
	
+:	
		ret
	
_LABEL_1796_:	
		ld c, a
		ld a, (_RAM_CC21_)
		and a
		ld a, c
		jp nz, +
		and a
		ret z
		ld hl, _RAM_CB39_
		bit 0, (hl)
		ret nz
		bit 1, (hl)
		ret nz
		bit 2, (hl)
		ret nz
		bit 3, (hl)
		ret nz
		bit 6, (hl)
		ret nz
+:	
		ld c, a
		ld a, (_RAM_CC1E_)
		ld l, a
		ld h, $00
		add hl, hl
		add hl, hl
		add hl, hl
		ld de, $3800
		add hl, de
		ld (_RAM_CC1C_), hl
		ld a, c
		ld l, $00
		rra
		rr l
		ld h, a
		ld (_RAM_CB3B_), hl
		push bc
		ld a, (_RAM_C12B_)
		add a, c
		and $F8
		ld b, a
		ld a, (_RAM_C12B_)
		and $F8
		cp b
		jp nz, _LABEL_1841_
		ld a, (_RAM_C12B_)
		add a, c
		and $07
		cp $04
		jr c, _LABEL_1841_
		ld a, (_RAM_CC86_)
		or a
		jr z, _LABEL_1841_
		ld hl, _RAM_CB39_
		bit 5, (hl)
		jr nz, +
		bit 7, (hl)
		jp z, _LABEL_1838_
+:	
		ld a, (_RAM_C109_)
		ld c, a
		ld a, (_RAM_CC85_)
		ld h, a
		add a, c
		cp $F0
		jp nc, +
		cp $E0
		jp z, ++
		jp nc, +++
		jp ++++
	
+:	
		sub $20
		jp ++++
	
++:	
		ld a, $00
		jp ++++
	
+++:	
		add a, $20
		jp ++++
	
++++:	
		ld b, a
		ld c, $09
		ld (_RAM_CC27_), bc
		ld l, $00
		sra h
		rr l
		ld (_RAM_CB3D_), hl
		ld hl, _RAM_CB39_
		set 3, (hl)
_LABEL_1838_:	
		ld hl, _RAM_CB39_
		res 5, (hl)
		xor a
		ld (_RAM_CC86_), a
_LABEL_1841_:	
		pop bc
		ld a, (_RAM_C12B_)
		and $07
		add a, c
		ld b, $00
		sub $08
		jp z, +
		jp nc, +
		ld hl, (_RAM_C12B_)
		add hl, bc
		ld (_RAM_C12B_), hl
		ld a, l
		neg
		ld b, a
		ld c, $08
		ld (_RAM_CC23_), bc
		ld hl, _RAM_CB39_
		set 2, (hl)
		ld a, (_RAM_CC21_)
		or a
		jp nz, _LABEL_1E03_
		ret
	
+:	
		push af
		ld a, $07
		call _LABEL_B13_
		pop af
		ld a, c
		ld (_RAM_CB3A_), a
		ld hl, _DATA_1C000_
		ld a, (_RAM_C128_)
		ld e, a
		ld d, $00
		add hl, de
		ld e, (hl)
		rlc e
		rlc e
		ld hl, _DATA_1C029_
		add hl, de
		ld a, (_RAM_C129_)
		ld e, a
		add hl, de
		ld a, (hl)
		ld l, a
		ld h, d
		add hl, hl
		ld de, _DATA_1C065_
		add hl, de
		ld a, (hl)
		inc hl
		ld h, (hl)
		ld l, a
		ld a, (_RAM_C12A_)
		ld b, a
		rra
		rra
		and $3F
		ld e, (hl)
-:	
		bit 7, (hl)
		jr nz, +
		ld e, (hl)
		inc hl
		dec a
		jp p, -
		jr ++
	
+:	
		set 7, a
		sub (hl)
		inc hl
		jp nc, -
++:	
		ld a, b
		and $03
		add a, a
		add a, a
		add a, a
		ld b, a
		add a, a
		add a, b
		ld c, a
		ld d, $00
		ld b, d
		ex de, hl
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		ld e, l
		ld d, h
		add hl, hl
		add hl, de
		add hl, hl
		add hl, bc
		ld de, _DATA_1DED8_
		add hl, de
		ld a, $07
		call _LABEL_B13_
		ld bc, $0018
		ld de, _RAM_CB01_
		ldir
		ld hl, _RAM_CC5F_
		set 7, (hl)
		ld hl, _RAM_CC88_
		bit 0, (hl)
		jp nz, +
		bit 7, (hl)
		jp nz, ++
		jp ++++
	
+:	
		ld hl, _RAM_CC87_
		ld (hl), $01
		jp +++
	
++:	
		ld hl, _RAM_CC87_
		ld (hl), $FF
+++:	
		ld hl, _RAM_CC88_
		ld (hl), $00
++++:	
		ld b, $03
		ld c, $0A
		ld hl, _RAM_CC89_
		ld de, (_RAM_CC17_)
_LABEL_1916_:	
		push bc
		ld a, (de)
		cp $FF
		jp z, _LABEL_19BA_
		ld b, a
		ld a, (_RAM_C12A_)
		cp b
		jp nz, _LABEL_19BA_
		bit 7, (hl)
		jp nz, _LABEL_19AF_
		set 7, (hl)
		push de
		push hl
		ld a, (de)
		inc hl
		ld (hl), a
		dec hl
		inc de
		ld a, (de)
		cp $80
		jp c, +
		set 0, (hl)
		exx
		ld hl, _RAM_CB39_
		set 7, (hl)
		exx
		sub $80
		jp ++
	
+:	
		cp $40
		jp c, +
		set 3, (hl)
		sub $40
		jp ++
	
+:	
		cp $20
		jp c, +
		set 2, (hl)
		sub $20
		jp ++
	
+:	
		set 1, (hl)
++:	
		inc hl
		inc hl
		ld (hl), a
		inc hl
		ld (hl), a
		inc hl
		inc de
		ld a, (de)
		ld (hl), a
		inc de
		inc hl
		ld a, (de)
		push hl
		ld e, a
		ld d, $00
		xor a
		rl e
		rl d
		ld hl, _DATA_1C34D_
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ld a, (de)
		pop hl
		ld (hl), a
		ld b, a
		dec hl
		dec hl
		dec hl
		dec hl
		dec hl
		ld a, (hl)
		and $0C
		jp z, +
		inc hl
		inc hl
		inc hl
		ld (hl), b
		inc hl
		inc hl
		jp ++
	
+:	
		inc hl
		inc hl
		inc hl
		inc hl
		inc hl
++:	
		inc de
		inc hl
		ld a, (de)
		ld (hl), a
		inc de
		inc hl
		ld (hl), e
		inc hl
		ld (hl), d
		inc hl
		ld (hl), a
		pop hl
		pop de
		inc de
		inc de
		inc de
		inc de
		ld (_RAM_CC17_), de
_LABEL_19AF_:	
		ld b, $00
		add hl, bc
		pop bc
		dec b
		jp nz, _LABEL_1916_
		jp +
	
_LABEL_19BA_:	
		pop bc
+:	
		ld b, $03
		ld c, $0A
		ld ix, _RAM_CC89_
_LABEL_19C3_:	
		push bc
		bit 7, (ix+0)
		jp z, _LABEL_1B7C_
		bit 0, (ix+0)
		jp z, _LABEL_1B7C_
		ld a, (ix+5)
		ld l, $00
		sra a
		rr l
		sra a
		rr l
		ld h, a
		ld (_RAM_CB3D_), hl
		ld a, (_RAM_CC1E_)
		and $07
		jp nz, _LABEL_1A78_
		ld a, (ix+5)
		cp $F0
		sbc a, a
		cpl
		jr z, +
		inc a
+:	
		ld (_RAM_CC85_), a
		jp nc, +
		ld a, (_RAM_CC1E_)
		and $F8
		sub $20
		jp nc, _LABEL_1A07_
		sub $20
_LABEL_1A07_:	
		ld l, a
		ld h, $00
		add hl, hl
		add hl, hl
		add hl, hl
		ld de, $3800
		add hl, de
		ld (_RAM_CC2B_), hl
		jp ++
	
+:	
		ld a, (_RAM_CC1E_)
		and $F8
		sub $08
		jp nc, _LABEL_1A07_
		sub $20
		jp _LABEL_1A07_
	
++:	
		ld bc, $0020
		ld l, (ix+7)
		ld h, (ix+8)
		ld de, _RAM_CB19_
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		ldi
		push hl
		ld hl, _RAM_CB39_
		set 6, (hl)
_LABEL_1A78_:	
		ld hl, _RAM_CC1E_
		ld a, (ix+5)
		add a, (hl)
		cp $F0
		jp nc, +
		cp $E0
		jp nc, ++
		jp +++
	
+:	
		sub $20
		jp +++
	
++:	
		add a, $20
+++:	
		ld (_RAM_CC1E_), a
		ld a, (_RAM_C109_)
		ld b, a
		ld a, (ix+5)
		cp $F0
		sbc a, a
		cpl
		jp z, +
		inc a
+:	
		add a, b
		cp $F0
		jp nc, +
		cp $E0
		jp nc, ++
		jp +++
	
+:	
		sub $20
		jp +++
	
++:	
		add a, $20
+++:	
		ld b, a
		ld c, $09
		ld (_RAM_CC27_), bc
		ld hl, _RAM_CB39_
		set 1, (hl)
		ld a, $01
		ld (_RAM_CC86_), a
		ld a, (ix+5)
		cp $F0
		jp c, +
		ld a, b
		and $F8
		add a, $08
		cp $E0
		jp nz, ++
		add a, $20
		jp ++
	
+:	
		ld a, b
		and $07
		jp z, +
		ld a, b
		and $F8
		jp ++
	
+:	
		ld a, b
		and $F8
		sub $08
		cp $F8
		jp nz, ++
		sub $20
++:	
		ld l, a
		ld h, $00
		add hl, hl
		add hl, hl
		add hl, hl
		ld de, $3800
		add hl, de
		ld (_RAM_CC1C_), hl
		ld hl, _RAM_CB39_
		bit 6, (hl)
		jp z, ++
		pop hl
		ld a, (ix+3)
		dec a
		ld (ix+3), a
		jp nz, +
		ld a, (ix+2)
		ld (ix+3), a
		add a, a
		add a, a
		add a, a
		add a, a
		add a, a
		ld e, a
		ld d, $00
		xor a
		sbc hl, de
		jp +
	
+:	
		ld (ix+7), l
		ld (ix+8), h
++:	
		ld a, (ix+6)
		dec a
		ld (ix+6), a
		ld b, a
		and $03
		jp nz, ++
		ld a, (ix+5)
		ld hl, _RAM_CC88_
		cp $F0
		jp c, +
		set 0, (hl)
		jp ++
	
+:	
		set 7, (hl)
++:	
		ld a, b
		and a
		jp z, +
		jp ++
	
+:	
		ld a, (ix+4)
		dec a
		jp z, +
		ld (ix+4), a
		ld a, (ix+9)
		ld (ix+6), a
		jp ++
	
+:	
		ld (ix+0), $00
		ld hl, _RAM_CB39_
		res 7, (hl)
		set 5, (hl)
		jp ++
	
_LABEL_1B7C_:	
		pop bc
		push bc
		ld b, $00
		add ix, bc
		pop bc
		dec b
		jp nz, _LABEL_19C3_
		jp +++
	
++:	
		pop bc
+++:	
		ld b, $03
		ld c, $0A
		ld ix, _RAM_CC89_
_LABEL_1B93_:	
		push bc
		bit 7, (ix+0)
		jp z, _LABEL_1D46_
		bit 0, (ix+0)
		jp nz, _LABEL_1D46_
		bit 1, (ix+0)
		jp nz, +
		bit 2, (ix+0)
		jp nz, ++
		bit 3, (ix+0)
		jp nz, _LABEL_1C43_
+:	
		ld a, (ix+5)
		ld c, a
		ld b, $00
		ld d, b
		ld a, (_RAM_CC87_)
		ld e, a
		ld a, (ix+2)
		add a, e
		ld (ix+2), a
		ld e, a
		ld hl, $CB01
		add hl, de
		ex de, hl
		ld l, (ix+7)
		ld h, (ix+8)
		ldir
		jp _LABEL_1CA2_
	
++:	
		ld a, (ix+3)
		ld (ix+5), a
		ld a, (_RAM_CC87_)
		ld e, a
		ld a, (ix+2)
		add a, e
		ld (ix+2), a
		ld e, a
		ld a, $18
		sub e
		ld b, $00
		ld c, a
		ld d, $00
		ld hl, $CB01
		add hl, de
		ex de, hl
		ld l, (ix+7)
		ld h, (ix+8)
-:	
		ldi
		push de
		ld a, (ix+5)
		and $F0
		ld a, (ix+5)
		jp z, +
		sub $10
		ld (ix+5), a
		cp $10
		jp nc, ++
		ld e, a
		ld d, $00
		and a
		sbc hl, de
		jp ++
	
+:	
		dec a
		ld (ix+5), a
		jp nz, ++
		ld a, (ix+3)
		and $0F
		ld (ix+5), a
		ld e, a
		ld d, $00
		and a
		sbc hl, de
++:	
		pop de
		ld a, b
		and a
		jp nz, -
		ld a, c
		and a
		jp nz, -
		jp _LABEL_1CEF_
	
_LABEL_1C43_:	
		ld a, (ix+3)
		ld (ix+5), a
		ld a, (_RAM_CC87_)
		ld e, a
		ld a, (ix+2)
		add a, e
		ld (ix+2), a
		ld e, a
		inc a
		ld b, a
		ld d, $00
		ld hl, $CB01
		add hl, de
		ex de, hl
		ld l, (ix+7)
		ld h, (ix+8)
-:	
		ld a, (hl)
		ld (de), a
		inc hl
		dec de
		push de
		ld a, (ix+5)
		and $F0
		ld a, (ix+5)
		jp z, +
		sub $10
		ld (ix+5), a
		cp $10
		jp nc, ++
		ld e, a
		ld d, $00
		and a
		sbc hl, de
		jp ++
	
+:	
		dec a
		ld (ix+5), a
		jp nz, ++
		ld a, (ix+3)
		and $0F
		ld (ix+5), a
		ld e, a
		ld d, $00
		and a
		sbc hl, de
++:	
		pop de
		djnz -
		jp _LABEL_1CEF_
	
_LABEL_1CA2_:	
		ld (ix+7), l
		ld (ix+8), h
		ld a, (ix+6)
		dec a
		ld (ix+6), a
		jp z, +
		jp _LABEL_1D46_
	
+:	
		ld a, (ix+4)
		dec a
		jp z, _LABEL_1CE8_
		ld (ix+4), a
		ld a, (ix+9)
		ld (ix+6), a
		ld b, a
		ld l, (ix+5)
		ld h, $00
		ld e, l
		ld d, h
		dec b
		jp z, +
-:	
		add hl, de
		djnz -
+:	
		ld e, l
		ld d, h
		ld l, (ix+7)
		ld h, (ix+8)
		and a
		sbc hl, de
		ld (ix+7), l
		ld (ix+8), h
		jp _LABEL_1D46_
	
_LABEL_1CE8_:	
		ld (ix+0), $00
		jp _LABEL_1D46_
	
_LABEL_1CEF_:	
		ld a, (ix+3)
		and $F0
		and a
		rra
		rra
		rra
		rra
		ld e, a
		ld d, $00
		ld l, (ix+7)
		ld h, (ix+8)
		add hl, de
		ld (ix+7), l
		ld (ix+8), h
		ld a, (ix+6)
		dec a
		ld (ix+6), a
		jp z, +
		jp _LABEL_1D46_
	
+:	
		ld a, (ix+4)
		dec a
		jp z, _LABEL_1CE8_
		ld (ix+4), a
		ld a, (ix+9)
		ld (ix+6), a
		ld d, $00
		ld l, e
		ld h, d
		ld b, a
		dec b
		jp z, +
-:	
		add hl, de
		djnz -
+:	
		ld e, l
		ld d, h
		ld l, (ix+7)
		ld h, (ix+8)
		and a
		sbc hl, de
		ld (ix+7), l
		ld (ix+8), h
		jp _LABEL_1D46_
	
_LABEL_1D46_:	
		pop bc
		push bc
		ld b, $00
		add ix, bc
		pop bc
		dec b
		jp nz, _LABEL_1B93_
		ld hl, _RAM_CC87_
		ld (hl), $00
		ld hl, (_RAM_C12B_)
		ld a, (_RAM_CB3A_)
		ld e, a
		ld d, $00
		add hl, de
		ld (_RAM_C12B_), hl
		ld a, l
		neg
		ld b, a
		ld c, $08
		ld (_RAM_CC25_), bc
		ld a, l
		and $F8
		rra
		rra
		ld e, a
		ld hl, (_RAM_CC1C_)
		add hl, de
		ld (_RAM_CC29_), hl
		ld hl, _RAM_CB39_
		set 0, (hl)
		ld a, (_RAM_C12C_)
		cp $20
		jp c, +
		ld a, (_RAM_C12B_)
		ld hl, _RAM_C16B_
		set 7, (hl)
		ld hl, _RAM_CC30_
		res 6, (hl)
		ld hl, _RAM_CCA7_
		bit 0, (hl)
		ret z
		ld hl, (_RAM_CC29_)
		ld de, $003D
		add hl, de
		ld de, $0300
		add hl, de
		ld b, $07
		ld de, $003C
		ld a, $10
-:	
		di
		call _LABEL_47C_
		inc hl
		inc hl
		call _LABEL_47C_
		inc hl
		inc hl
		call _LABEL_47C_
		ei
		add hl, de
		djnz -
		ret
	
+:	
		ld hl, _RAM_C12A_
		inc (hl)
		jp nz, +
		ld hl, _RAM_C129_
		inc (hl)
		call _LABEL_119D_
		ld a, $07
		call _LABEL_B13_
		ld hl, _DATA_1C000_
		ld a, (_RAM_C128_)
		ld e, a
		ld d, $00
		add hl, de
		ld e, (hl)
		rlc e
		rlc e
		rlc e
		ld hl, _DATA_1C2D5_
		add hl, de
		ld a, (_RAM_C129_)
		ld e, a
		rlc e
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ld (_RAM_CC17_), de
		ld hl, _RAM_CC88_
		ld (hl), $00
+:	
		ld a, (_RAM_CC21_)
		or a
		jp nz, _LABEL_1E03_
		xor a
		ret
	
_LABEL_1E03_:	
		call _LABEL_B17_
		ld (_RAM_CC19_), a
		ld hl, _RAM_CB39_
		bit 2, (hl)
		jp z, +
		res 2, (hl)
		ld bc, (_RAM_CC23_)
		call _LABEL_3B_
		ld hl, _RAM_CB39_
		bit 3, (hl)
		jp z, _LABEL_1F1E_
		res 3, (hl)
		ld bc, (_RAM_CC27_)
		call _LABEL_3B_
		jp _LABEL_1F1E_
	
+:	
		bit 0, (hl)
		jp z, _LABEL_1F1E_
		ld bc, (_RAM_CC25_)
		call _LABEL_3B_
		ld a, (_RAM_CC14_)
		call _LABEL_B13_
		ld hl, (_RAM_CC29_)
		ld de, _RAM_CB01_
		ld bc, $1840
-:	
		push bc
		ld a, (de)
		push af
		ld a, l
		out (Port_VDPAddress), a
		ld a, h
		and $3F
		or $40
		out (Port_VDPAddress), a
		pop af
		out (Port_VDPData), a
		ld b, $00
		add hl, bc
		ld a, h
		cp $3F
		jr nz, +
		sub $07
		ld h, a
+:	
		inc de
		pop bc
		djnz -
		ld hl, _RAM_CC5F_
		set 6, (hl)
		ld hl, _RAM_CB39_
		bit 1, (hl)
		jp z, _LABEL_1F1E_
		ld bc, (_RAM_CC27_)
		call _LABEL_3B_
		res 1, (hl)
		bit 6, (hl)
		jp z, _LABEL_1F1E_
		ld hl, (_RAM_CC2B_)
		ld de, _RAM_CB19_
		ld b, $20
		ld a, l
		out (Port_VDPAddress), a
		ld a, h
		and $3F
		or $40
		out (Port_VDPAddress), a
		ex de, hl
		ld c, Port_VDPData
		xor a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		outi
		out (c), a
		ld hl, _RAM_CB39_
		res 6, (hl)
_LABEL_1F1E_:	
		ld hl, _RAM_CB39_
		res 0, (hl)
		ld a, (_RAM_CC21_)
		or a
		jp nz, _LABEL_1774_
		ld a, (_RAM_CC19_)
		call _LABEL_B13_
		ret
	
_LABEL_1F31_:	
		ld a, $07
		call _LABEL_B13_
		ld hl, _RAM_C128_
		ld e, (hl)
		push de
		ld d, $00
		ld hl, _DATA_1C000_
		add hl, de
		ld e, (hl)
		ld hl, _DATA_2043_
		add hl, de
		ld a, (hl)
		ld (_RAM_CC14_), a
		push de
		ld hl, _DATA_1FE9_
		ld a, e
		add a, a
		add a, a
		add a, e
		add a, e
		ld e, a
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		inc hl
		push hl
		ld l, (hl)
		ld h, $00
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		ld c, l
		ld b, h
		ld l, e
		ld h, d
		add hl, bc
		push hl
		pop hl
		ld hl, $0000
		ex de, hl
		ld a, (_RAM_CC14_)
		call _LABEL_B13_
		call _LABEL_422_
		ex de, hl
		pop hl
		inc hl
		push hl
		ld l, (hl)
		ld h, $00
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		ld c, l
		ld b, h
		ld l, e
		ld h, d
		add hl, bc
		push hl
		pop hl
		ld hl, $1800
		ex de, hl
		call _LABEL_422_
		ex de, hl
		pop hl
		inc hl
		push hl
		ld l, (hl)
		ld h, $00
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		ld c, l
		ld b, h
		ld l, e
		ld h, d
		add hl, bc
		push hl
		pop hl
		ld hl, $1C00
		ex de, hl
		call _LABEL_422_
		ex de, hl
		pop hl
		inc hl
		ld l, (hl)
		ld h, $00
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		ld c, l
		ld b, h
		ld l, e
		ld h, d
		add hl, bc
		ld hl, $1E00
		ex de, hl
		call _LABEL_422_
		pop hl
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		ld a, $05
		call _LABEL_B13_
		ld de, $B9E6
		add hl, de
		ex de, hl
		call _LABEL_272_
		pop de
		ld a, e
		cp $1D
		ret nz
		ld a, $10
		ld hl, $C005
		call _LABEL_406_
		ld a, $19
		ld hl, $C009
		call _LABEL_406_
		ret
	
; Data from 1FE9 to 2042 (90 bytes)	
_DATA_1FE9_:	
	.db $00 $80 $00 $00 $00 $00 $C3 $92 $00 $00 $00 $00 $C3 $92 $00 $00
	.db $00 $00 $00 $80 $00 $00 $00 $00 $00 $80 $00 $00 $00 $00 $CA $9B
	.db $00 $00 $00 $00 $CA $9B $00 $00 $00 $00 $CA $9B $00 $00 $00 $00
	.db $CA $9B $00 $00 $00 $00 $4E $AB $00 $00 $00 $00 $CA $9B $00 $00
	.db $00 $00 $C3 $92 $00 $00 $00 $00 $00 $80 $00 $00 $00 $00 $75 $B2
	.db $00 $00 $00 $00 $4E $AB $00 $00 $00 $00
	
; Data from 2043 to 2051 (15 bytes)	
_DATA_2043_:	
	.db $06 $05 $05 $06 $06 $06 $06 $06 $06 $05 $06 $05 $05 $06 $05
	
_LABEL_2052_:	
		ld hl, _RAM_CC5F_
		bit 7, (hl)
		res 7, (hl)
		ret nz
		ld hl, _RAM_CC40_
		inc (hl)
		ld a, (hl)
		cp $03
		jp c, +
		xor a
		ld (hl), a
		call ++
		ret
	
+:	
		dec a
		jp nz, +
		call _LABEL_210F_
		ret
	
+:	
		call _LABEL_20FD_
		ret
	
++:	
		ld a, (_RAM_C222_)
		bit 6, a
		ret nz
		ld ix, _RAM_C4A0_
		ld b, $2C
		ld a, (_RAM_C224_)
		ld l, a
		ld a, (_RAM_C227_)
		ld h, a
		ld de, $0020
-:	
		ld a, (ix+2)
		rlca
		jr nc, ++
		and $C0
		jr nz, ++
		ld a, h
		sub (ix+7)
		jr nc, +
		neg
+:	
		ld c, a
		ld a, $02
		add a, (ix+8)
		cp c
		jr c, ++
		ld a, l
		sub (ix+4)
		jr nc, +
		neg
+:	
		ld c, a
		ld a, $06
		add a, (ix+5)
		cp c
		jr nc, +++
++:	
		add ix, de
		djnz -
		ret
	
+++:	
		ld a, (_RAM_C127_)
		and $06
		jp z, ++
		and $02
		jp nz, +
		ld a, (_RAM_C222_)
		or $08
		ld (_RAM_C222_), a
+:	
		set 2, (ix+2)
		jp +++
	
++:	
		ld a, (_RAM_C222_)
		ld c, a
		ld a, (ix+10)
		and $3F
		or c
		ld (_RAM_C222_), a
		ld a, (ix+2)
		and $18
		ret z
+++:	
		ld a, (ix+2)
		set 5, (ix+2)
		and $18
		ret z
		ld hl, _RAM_C222_
		res 3, (hl)
		ret
	
_LABEL_20FD_:	
		ld a, (_RAM_C422_)
		rla
		ret nc
		rla
		ret c
		ld a, (_RAM_C424_)
		ld l, a
		ld a, (_RAM_C427_)
		ld h, a
		jp +
	
_LABEL_210F_:	
		ld a, (_RAM_C3A2_)
		rla
		ret nc
		rla
		ret c
		ld a, (_RAM_C3A4_)
		ld l, a
		ld a, (_RAM_C3A7_)
		ld h, a
+:	
		ld ix, _RAM_C4A0_
		ld b, $2C
		ld de, $0020
-:	
		ld a, (ix+2)
		rlca
		jr nc, ++
		and $D0
		jr nz, ++
		ld a, h
		sub (ix+7)
		jr nc, +
		neg
+:	
		ld c, a
		ld a, $03
		add a, (ix+8)
		cp c
		jr c, ++
		ld a, l
		sub (ix+4)
		jr nc, +
		neg
+:	
		ld c, a
		ld a, $03
		add a, (ix+5)
		cp c
		jr nc, +++
++:	
		add ix, de
		djnz -
		ret
	
+++:	
		ld c, (ix+10)
		ld a, (_RAM_CC40_)
		dec a
		ld de, _RAM_C3A2_
		jr z, +
		ld de, _RAM_C422_
+:	
		ld a, (de)
		or c
		ld (de), a
		ld a, (ix+2)
		and $10
		jp nz, +
		ld a, c
		and $18
		ret nz
+:	
		set 5, (ix+2)
		set 0, (ix+2)
		ret
	
; 4th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_217F_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_36E9_
		call _LABEL_225F_
		ld a, $80
		call _LABEL_2257_
		ld a, $15
		rst $30	; _LABEL_30_
	; Data from 2197 to 21A3 (13 bytes)
	.db $3E $AB $06 $04 $CD $F1 $22 $3E $1D $32 $9F $C1 $C9
	
; 3rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_21A4_:	
		call _LABEL_233B_
		cp $40
		jp c, _LABEL_222D_
		bit 2, (ix+2)
		jp nz, _LABEL_2246_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_222D_
		bit 0, a
		jr nz, +
		bit 1, a
		jr nz, ++
		ld b, $28
		call _LABEL_22D9_
		jr nc, _LABEL_2210_
		ld hl, _RAM_C198_
		set 0, (hl)
		jp _LABEL_2210_
	
+:	
		ld a, (_RAM_C19F_)
		ld hl, _RAM_C19D_
		call _LABEL_22C7_
		ld a, d
		and a
		jr nz, +
		ld de, $FFEC
		call _LABEL_227A_
		jr +++
	
+:	
		ld hl, _RAM_C198_
		set 1, (hl)
		res 0, (hl)
++:	
		ld hl, _RAM_C19A_
		ld a, $08
		call _LABEL_22C7_
		ld a, d
		and a
		jr nz, +
		ld de, $0080
		jr +++
	
+:	
		ld de, $FF80
+++:	
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		ld de, $FFC0
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
_LABEL_2210_:	
		ld a, $08
		call _LABEL_22C4_
		ld a, d
		and a
		jr nz, +
		ld a, $AF
		ld b, $04
		call _LABEL_22F1_
		jp ++
	
+:	
		ld a, $AB
		ld b, $04
		call _LABEL_22F1_
		jp ++
	
_LABEL_222D_:	
		call _LABEL_22A6_
		cp $18
		jp c, +++
		ld a, $B3
		ld b, $04
		call _LABEL_22F1_
++:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+++:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
_LABEL_2246_:	
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $71
		call _LABEL_6E2_
		ld b, $04
		call _LABEL_24A9_
		ret
	
_LABEL_2257_:	
		ld hl, (_RAM_C196_)
		inc l
		inc l
		or (hl)
		ld (hl), a
		ret
	
_LABEL_225F_:	
		ld hl, (_RAM_C196_)
		inc l
		inc l
		inc l
		inc l
		ld (hl), d
		inc hl
		ld a, (bc)
		ld (hl), a
		inc l
		inc l
		ld (hl), e
		inc l
		inc bc
		ld a, (bc)
		ld (hl), a
		inc l
		inc bc
		ld a, (bc)
		ld (hl), a
		inc l
		inc bc
		ld a, (bc)
		ld (hl), a
		ret
	
_LABEL_227A_:	
		ld l, (ix+16)
		ld h, (ix+17)
		add hl, de
		ld (ix+16), l
		ld (ix+17), h
		ex de, hl
		ret
	
_LABEL_2289_:	
		ld l, (ix+18)
		ld h, (ix+19)
		add hl, de
		ld (ix+18), l
		ld (ix+19), h
		ex de, hl
		ret
	
_LABEL_2298_:	
		ld (ix+16), e
		ld (ix+17), d
		ret
	
_LABEL_229F_:	
		ld (ix+18), e
		ld (ix+19), d
		ret
	
_LABEL_22A6_:	
		bit 1, (ix+2)
		jr z, +
		ld de, $0040
		jp ++
	
+:	
		ld de, $FFC0
++:	
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
_LABEL_22B9_:	
		ld de, $FFE4
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		ret
	
_LABEL_22C4_:	
		ld hl, _RAM_C199_
_LABEL_22C7_:	
		inc (hl)
		ld c, a
		cp (hl)
		ld d, $00
		jr c, +
		ret
	
+:	
		add a, c
		dec a
		cp (hl)
		inc d
		jr c, +
		ret
	
+:	
		ld (hl), $00
		ret
	
_LABEL_22D9_:	
		ld de, $0007
		ld a, (_RAM_C227_)
		jp +
	
_LABEL_22E2_:	
		ld de, $0004
		ld a, (_RAM_C224_)
+:	
		ld hl, (_RAM_C196_)
		ld c, a
		add hl, de
		ld a, (hl)
		sub b
		cp c
		ret
	
_LABEL_22F1_:	
		ld hl, (_RAM_C196_)
_LABEL_22F4_:	
		ld de, $000C
		add hl, de
-:	
		ld (hl), a
		inc a
		ld de, $0020
		add hl, de
		djnz -
		ret
	
_LABEL_2301_:	
		ld hl, (_RAM_C196_)
		inc l
		inc l
		bit 5, (hl)
		ret z
		ld a, (_RAM_C198_)
		bit 7, a
		ret nz
		bit 0, (hl)
		jr nz, +
		set 6, (hl)
+:	
		ld de, $0005
		add hl, de
		ld a, (_RAM_C227_)
		cp (hl)
		jr nc, +
		xor a
		sbc hl, de
		set 1, (hl)
+:	
		ld hl, _RAM_C198_
		set 7, (hl)
		ld de, $0260
		call _LABEL_2298_
		ld a, (ix+9)
		call _LABEL_615E_
		ld a, $91
		call _LABEL_A99_
		ret
	
_LABEL_233B_:	
		ld hl, (_RAM_CB3D_)
		ld e, (ix+3)
		ld d, (ix+4)
		add hl, de
		ld (ix+3), l
		ld (ix+4), h
_LABEL_234B_:	
		ld de, (_RAM_CB3B_)
		ld l, (ix+6)
		ld h, (ix+7)
		xor a
		sbc hl, de
		ld (ix+6), l
		ld (ix+7), h
		ld a, h
		ret
	
; 108th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2360_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld a, $08
		add a, d
		ld (_RAM_C19B_), a
		ld a, d
		sub $08
		ld (_RAM_C19C_), a
		ld e, $D0
		ld bc, _DATA_36F1_
		call _LABEL_225F_
		ld a, $8C
		rst $30	; _LABEL_30_
	; Data from 237F to 2380 (2 bytes)
	.db $18 $35
	
; 6th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2381_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld a, $0C
		add a, d
		ld (_RAM_C19B_), a
		ld a, d
		sub $0C
		ld (_RAM_C19C_), a
		jr +
	
; 8th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2397_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld a, $06
		add a, d
		ld (_RAM_C19B_), a
		ld a, d
		sub $06
		ld (_RAM_C19C_), a
+:	
		ld e, $D0
		ld bc, _DATA_36F1_
		call _LABEL_225F_
		ld a, $16
		rst $30	; _LABEL_30_
	; Data from 23B6 to 23CE (25 bytes)
	.db $21 $98 $C1 $CB $C6 $11 $00 $00 $DD $2A $96 $C1 $CD $7A $22 $3E
	.db $7F $06 $04 $CD $F1 $22 $C3 $99 $24
	
; 107th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_23CF_:	
		call _LABEL_233B_
		cp $38
		jp c, _LABEL_249D_
		bit 2, (ix+2)
		jp nz, _LABEL_24A3_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		and $80
		jp nz, _LABEL_248A_
		ld a, (_RAM_C19D_)
		bit 0, a
		jr nz, +
		ld b, $04
		call _LABEL_28_
		ld b, $30
		call _LABEL_22D9_
		ret nc
		ld hl, _RAM_C19D_
		set 0, (hl)
		ret
	
; 5th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2401_:	
		call _LABEL_233B_
		cp $38
		jp c, _LABEL_249D_
		bit 2, (ix+2)
		jp nz, _LABEL_24A3_
		call _LABEL_2301_
+:	
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_248A_
		bit 0, a
		jr nz, +
		ld de, $000C
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		ld hl, $C19B
		cp (hl)
		call nc, +++
		jr ++
	
+:	
		ld de, $FFF4
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		ld hl, $C19C
		cp (hl)
		call c, ++++
++:	
		ld de, $FF80
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld a, $08
		call _LABEL_22C4_
		ld a, d
		and a
		jr nz, +
		ld a, $83
		ld b, $04
		call _LABEL_22F1_
		jp +++++
	
+:	
		ld a, $7F
		ld b, $04
		call _LABEL_22F1_
		jp +++++
	
+++:	
		ld hl, _RAM_C198_
		set 0, (hl)
		ld a, $00
		ld (ix+16), a
		ld (ix+17), a
		ret
	
++++:	
		ld hl, _RAM_C198_
		res 0, (hl)
		ld a, (_RAM_C19C_)
		ld (ix+4), a
		ld de, $0000
		ld (ix+3), d
		call _LABEL_227A_
		ret
	
_LABEL_248A_:	
		call _LABEL_22A6_
		cp $10
		jp c, _LABEL_249D_
		ld a, $87
		ld b, $04
		call _LABEL_22F1_
+++++:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_249D_:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
_LABEL_24A3_:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
_LABEL_24A9_:	
		push bc
		call _LABEL_57B_
		pop bc
		xor a
		ld h, a
		ld l, b
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		ld b, h
		ld c, l
		dec bc
		ld hl, (_RAM_C196_)
		inc hl
		ex de, hl
		ld hl, (_RAM_C196_)
		ld (hl), a
		ldir
		call _LABEL_6DA_
		ret
	
; 10th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_24C8_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_36F9_
		call _LABEL_225F_
		ld a, $20
		rst $30	; _LABEL_30_
	; Data from 24DB to 24DB (1 bytes)
	.db $C9
	
; 9th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_24DC_:	
		bit 2, (ix+2)
		jr nz, ++++
		call _LABEL_233B_
		cp $40
		jr c, +++
		ld a, $08
		call _LABEL_22C4_
		ld a, d
		and a
		jr nz, +
		ld a, $40
		ld b, $04
		call _LABEL_22F1_
		jp ++
	
+:	
		ld a, $44
		ld b, $04
		call _LABEL_22F1_
++:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+++:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
++++:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
; 12th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2513_:	
		ld a, $04
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_370D_
		call _LABEL_225F_
		ld a, $57
		rst $30	; _LABEL_30_
	; Data from 2526 to 2537 (18 bytes)
	.db $11 $60 $02 $DD $2A $96 $C1 $CD $7A $22 $3E $64 $06 $0C $CD $F1
	.db $22 $C9
	
; 11th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2538_:	
		call _LABEL_233B_
		cp $3A
		jr c, _LABEL_25A7_
		ld a, (_RAM_C198_)
		bit 7, a
		jr nz, _LABEL_259B_
		bit 2, (ix+2)
		jr nz, _LABEL_25AD_
		bit 5, (ix+2)
		jr z, +
		ld hl, _RAM_C198_
		set 7, (hl)
		ld a, $97
		rst $30	; _LABEL_30_
	; Data from 255A to 2560 (7 bytes)
	.db $3E $91 $CD $99 $0A $18 $3A
	
+:	
		ld de, $FF80
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld de, $FFF5
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		cp $0C
		jr c, +
		ld b, $0C
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld a, $02
		ld hl, _RAM_C199_
		inc (hl)
		cp (hl)
		ret nc
		xor a
		ld (hl), a
		ld de, $0260
		ld hl, (_RAM_C196_)
		call _LABEL_2298_
		or a
		ld hl, (_RAM_C196_)
		ld de, $0003
		add hl, de
		ld (hl), $00
		inc l
		ld (hl), $20
		ret
	
_LABEL_259B_:	
		call _LABEL_22B9_
		cp $08
		jp c, _LABEL_25A7_
		ld b, $0C
		rst $28	; _LABEL_28_
		ret
	
_LABEL_25A7_:	
		ld b, $0C
		call _LABEL_24A9_
		ret
	
_LABEL_25AD_:	
		ld b, $0C
		call _LABEL_2DF2_
		ret
	
; 14th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_25B3_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_36FD_
		call _LABEL_225F_
		ld a, $61
		ld b, $02
		rst $30	; _LABEL_30_
	; Data from 25C8 to 25D1 (10 bytes)
	.db $10 $FD $3E $97 $06 $04 $CD $F1 $22 $C9
	
; 13th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_25D2_:	
		call _LABEL_233B_
		cp $40
		jp c, _LABEL_2695_
		call _LABEL_269B_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_2683_
		ld a, (_RAM_C198_)
		bit 5, a
		jr nz, ++
		ld b, $26
		call _LABEL_22D9_
		jr nc, _LABEL_2603_
		ld de, $0260
		call _LABEL_2298_
		ld hl, _RAM_C198_
		set 5, (hl)
		ld a, $9F
		call _LABEL_A99_
_LABEL_2603_:	
		ld a, (_RAM_C198_)
		bit 6, a
		jr nz, +
		ld a, $97
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld a, $A3
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
++:	
		ld de, $FFE7
		call _LABEL_227A_
		bit 7, d
		jr z, +
		ld hl, _RAM_C19D_
		inc (hl)
		ld a, (hl)
		rra
		jr c, +
		ld hl, $0504
		xor a
		call _LABEL_392F_
+:	
		ld a, (ix+20)
		bit 1, a
		jr nz, +++
		ld hl, (_RAM_C196_)
		ld e, (ix+16)
		ld d, (ix+17)
		rst $18	; _LABEL_18_
		cp $18
		jr c, _LABEL_2695_
		ld a, (_RAM_C198_)
		bit 6, a
		jr z, +
		ld a, $A7
		jr ++
	
+:	
		ld a, $9B
++:	
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		rst $28	; _LABEL_28_
		ld de, $FF40
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ret
	
+++:	
		ld de, $0000
		call _LABEL_2298_
		ld a, $08
		ld hl, _RAM_C199_
		inc (hl)
		cp a
		jp nc, _LABEL_2603_
		xor a
		ld (hl), a
		ld hl, _RAM_C198_
		res 5, (hl)
		ret
	
_LABEL_2683_:	
		call _LABEL_22B9_
		cp $10
		jr c, _LABEL_2695_
		ld a, $9F
		ld b, $04
		call _LABEL_22F1_
_LABEL_2691_:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_2695_:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
_LABEL_269B_:	
		ld hl, (_RAM_C196_)
		inc l
		inc l
		bit 5, (hl)
		ret z
		bit 2, (hl)
		jr nz, ++
		res 5, (hl)
		ld a, (_RAM_C198_)
		bit 6, a
		jr nz, +
		ld hl, _RAM_C198_
		set 6, (hl)
		ld a, $02
		call _LABEL_615E_
		ret
	
+:	
		set 5, (hl)
		ld hl, _RAM_C198_
		bit 7, (hl)
		ret nz
		set 7, (hl)
		ld de, $0260
		call _LABEL_2298_
		ld a, (ix+9)
		call _LABEL_615E_
		ld a, $91
		call _LABEL_A99_
		ret
	
++:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
; 126th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_26DD_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_3721_
		call _LABEL_225F_
		ld a, $61
		ld b, $02
		rst $30	; _LABEL_30_
	; Data from 26F2 to 26FB (10 bytes)
	.db $10 $FD $3E $97 $06 $04 $CD $F1 $22 $C9
	
; 125th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_26FC_:	
		call _LABEL_233B_
		cp $40
		jp c, _LABEL_2695_
		bit 2, (ix+2)
		jr nz, +
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_2683_
		ld a, $A3
		ld b, $04
		call _LABEL_22F1_
		jp _LABEL_2691_
	
+:	
		ld a, $05
		ld (ix+9), a
		ld b, $04
		call _LABEL_2DF2_
		ret
	
; 186th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_272A_:	
		ld a, $01
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_287F_
		call _LABEL_225F_
		ld a, $79
		rst $30	; _LABEL_30_
	; Data from 273B to 2747 (13 bytes)
	.db $3E $C0 $CD $57 $22 $3E $3E $06 $02 $CD $F1 $22 $C9
	
; 188th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2748_:	
		ld a, $01
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_287F_
		call _LABEL_225F_
		ld a, $7A
		rst $30	; _LABEL_30_
	; Data from 2759 to 2765 (13 bytes)
	.db $3E $C0 $CD $57 $22 $3E $3C $06 $02 $CD $F1 $22 $C9
	
; 18th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2766_:	
		call _LABEL_2867_
		ld a, $21
		rst $30	; _LABEL_30_
	; Data from 276C to 2773 (8 bytes)
	.db $3E $34 $06 $02 $CD $F1 $22 $C9
	
; 24th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2774_:	
		call _LABEL_2867_
		ld a, $22
		rst $30	; _LABEL_30_
	; Data from 277A to 2781 (8 bytes)
	.db $3E $36 $06 $02 $CD $F1 $22 $C9
	
; 20th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2782_:	
		call _LABEL_2867_
		ld a, $23
		rst $30	; _LABEL_30_
	; Data from 2788 to 278F (8 bytes)
	.db $3E $38 $06 $02 $CD $F1 $22 $C9
	
; 22nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2790_:	
		call _LABEL_2867_
		ld a, $24
		rst $30	; _LABEL_30_
	; Data from 2796 to 279D (8 bytes)
	.db $3E $3A $06 $02 $CD $F1 $22 $C9
	
; 17th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_279E_:	
		call _LABEL_2883_
		ret
	
; 128th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_27A2_:	
		ld a, $02
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_287F_
		call _LABEL_225F_
		ld a, $C0
		call _LABEL_2257_
		ld a, $20
		ld b, $03
		call _LABEL_22F1_
		ld a, $07
		call _LABEL_615E_
		ret
	
; 127th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_27C2_:	
		call _LABEL_233B_
		cp $3C
		jr c, ++
		ld hl, _RAM_C19B_
		bit 0, (hl)
		jr nz, +
		set 0, (hl)
		ld a, $25
		rst $30	; _LABEL_30_
+:	
		ld hl, _RAM_C199_
		inc (hl)
		ld a, $04
		cp (hl)
		ret nc
		ld b, $03
		rst $28	; _LABEL_28_
		ld a, $10
		ld hl, $C199
		cp (hl)
		ret nc
++:	
		ld b, $03
		call _LABEL_24A9_
		ret
	
_LABEL_27ED_:	
		ld hl, _RAM_C19C_
		ld a, $04
		cp (hl)
		jr z, +
		inc (hl)
		call _LABEL_233B_
		ret
	
+:	
		call _LABEL_233B_
		cp $40
		jr c, +
		bit 5, (ix+2)
		jr nz, ++
		ld b, $04
		rst $28	; _LABEL_28_
		ld a, $E0
		ld hl, _RAM_C199_
		inc (hl)
		cp (hl)
		ret nz
+:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
++:	
		ld a, $8D
		call _LABEL_A99_
		ld a, (_RAM_C127_)
		bit 3, a
		jr nz, +
		ld a, (ix+9)
		call _LABEL_615E_
+:	
		ld d, (ix+4)
		ld e, (ix+7)
		ld a, (ix+9)
		ld hl, _RAM_C127_
		ld c, (hl)
		ex de, hl
		bit 3, c
		jr nz, +
		ld b, $08
		add a, b
		sub $03
		jp ++
	
+:	
		push hl
		ld a, $06
		call _LABEL_615E_
		ld a, $0B
		pop hl
++:	
		call _LABEL_6E2_
		scf
		ret
	
	; Data from 2852 to 2866 (21 bytes)
	.db $DD $7E $09 $F5 $CD $5E $61 $F1 $D6 $02 $CD $8D $64 $C9 $DD $7E
	.db $09 $CD $5E $61 $C9
	
_LABEL_2867_:	
		ld a, $01
		call _LABEL_92B_
		jr c, +
		ld bc, _DATA_287F_
		call _LABEL_225F_
		ld a, $C0
		call _LABEL_2257_
		ret
	
+:	
		pop hl
		call _LABEL_6DA_
		ret
	
; Data from 287F to 2882 (4 bytes)	
_DATA_287F_:	
	.db $00 $00 $00 $00
	
_LABEL_2883_:	
		call _LABEL_233B_
		cp $3C
		jr c, +
		ld b, $02
		rst $28	; _LABEL_28_
		ld a, $20
		ld hl, _RAM_C199_
		inc (hl)
		cp (hl)
		ret nz
+:	
		ld b, $02
		call _LABEL_24A9_
		ret
	
; 16th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_289B_:	
		ld a, $1A
		rst $30	; _LABEL_30_
	; Data from 289E to 28BA (29 bytes)
	.db $3E $03 $CD $2B $09 $DA $DA $06 $1E $98 $01 $ED $36 $CD $5F $22
	.db $3E $88 $CD $57 $22 $3E $2C $06 $04 $CD $F1 $22 $C9
	
; 15th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_28BB_:	
		call _LABEL_27ED_
		ret nc
		ld a, $01
		call _LABEL_648D_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 199th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_28CA_:	
		call _LABEL_233B_
		cp $38
		jr c, _LABEL_28E2_
		bit 5, (ix+2)
		jr nz, +
		ld b, $04
		rst $28	; _LABEL_28_
		ld a, $FE
		ld hl, _RAM_C199_
		inc (hl)
		cp (hl)
		ret nz
_LABEL_28E2_:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
+:	
		ld a, $8D
		call _LABEL_A99_
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, (_RAM_C131_)
		bit 7, a
		jr z, +++
		ld a, (_RAM_C127_)
		bit 3, a
		jr nz, +
		ld a, $08
		jr ++
	
+:	
		ld a, $0B
++:	
		call _LABEL_6E2_
		jp ++++
	
+++:	
		ld a, $40
		call _LABEL_6E2_
++++:	
		jr _LABEL_28E2_
	
	; Data from 2914 to 2914 (1 bytes)
	.db $C9
	
; 28th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2915_:	
		ld a, $1D
		rst $30	; _LABEL_30_
	; Data from 2918 to 2936 (31 bytes)
	.db $3E $03 $CD $2B $09 $DA $DA $06 $1E $98 $01 $15 $37 $CD $5F $22
	.db $06 $04 $3E $88 $CD $57 $22 $3E $20 $06 $04 $CD $F1 $22 $C9
	
; 27th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2937_:	
		call _LABEL_27ED_
		ret nc
		ld a, $02
		call _LABEL_648D_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 32nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2946_:	
		ld a, $1C
		rst $30	; _LABEL_30_
	; Data from 2949 to 2967 (31 bytes)
	.db $3E $03 $CD $2B $09 $DA $DA $06 $1E $98 $01 $ED $36 $CD $5F $22
	.db $06 $04 $3E $88 $CD $57 $22 $3E $28 $06 $04 $CD $F1 $22 $C9
	
; 31st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2968_:	
		call _LABEL_27ED_
		ret nc
		ld a, $01
		call _LABEL_648D_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 26th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2977_:	
		ld a, $1B
		rst $30	; _LABEL_30_
	; Data from 297A to 2996 (29 bytes)
	.db $3E $03 $CD $2B $09 $DA $DA $06 $1E $98 $01 $11 $37 $CD $5F $22
	.db $3E $88 $CD $57 $22 $3E $1C $06 $04 $CD $F1 $22 $C9
	
; 25th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2997_:	
		call _LABEL_27ED_
		ret nc
		ld a, $02
		call _LABEL_648D_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 30th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_29A6_:	
		ld a, $1E
		rst $30	; _LABEL_30_
	; Data from 29A9 to 29C5 (29 bytes)
	.db $3E $03 $CD $2B $09 $DA $DA $06 $1E $98 $01 $15 $37 $CD $5F $22
	.db $3E $88 $CD $57 $22 $3E $24 $06 $04 $CD $F1 $22 $C9
	
; 29th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_29C6_:	
		call _LABEL_27ED_
		ret nc
		ld a, $01
		call _LABEL_648D_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 220th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_29D5_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld a, (_RAM_C127_)
		bit 2, a
		jr nz, +
		ld hl, _RAM_C199_
		ld (hl), $16
		jp _LABEL_2A41_
	
; 36th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_29EC_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
+:	
		ld hl, _RAM_C199_
		ld (hl), $17
		jp _LABEL_2A41_
	
; 38th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_29FC_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, _RAM_C199_
		ld (hl), $18
		jr _LABEL_2A41_
	
; 40th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2A0B_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, _RAM_C199_
		ld (hl), $99
		jr _LABEL_2A41_
	
; 42nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2A1A_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, _RAM_C199_
		ld (hl), $9A
		ld e, $D0
		ld bc, _DATA_36F5_
		call _LABEL_225F_
		ld hl, _RAM_C198_
		set 3, (hl)
		ld a, $19
		jp +
	
; 34th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2A39_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
_LABEL_2A41_:	
		ld e, $D0
		ld bc, _DATA_36F5_
		call _LABEL_225F_
		ld a, $18
+:	
		rst $30	; _LABEL_30_
	; Data from 2A4C to 2A62 (23 bytes)
	.db $3E $90 $CD $57 $22 $3E $30 $06 $04 $CD $F1 $22 $DD $2A $96 $C1
	.db $11 $01 $00 $CD $9F $22 $C9
	
; 33rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2A63_:	
		ld a, (_RAM_C198_)
		and $20
		jr z, +
		ld a, (_RAM_C199_)
		and $80
		jr z, +
		ld hl, _RAM_C16D_
		set 6, (hl)
+:	
		call _LABEL_233B_
		cp $30
		jp c, _LABEL_2BC5_
		call _LABEL_2B74_
		ld hl, _RAM_C198_
		bit 7, (hl)
		jp nz, +
		ld a, (_RAM_C198_)
		bit 6, a
		ld a, $90
		jp nz, _LABEL_2B5F_
		ld a, $90
		call _LABEL_2257_
		ld a, $30
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld hl, (_RAM_C19B_)
		ex de, hl
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld de, $FFE8
		call _LABEL_227A_
		ld hl, _RAM_C19E_
		ld a, $04
		cp (hl)
		jr z, +
		inc (hl)
		jr +++
	
+:	
		xor a
		ld hl, $C19B
		cp (hl)
		jr nz, +
		inc hl
		cp (hl)
		jr nz, +
		xor a
		jr ++
	
+:	
		ld a, $7F
++:	
		ld hl, $0804
		call _LABEL_392F_
		ld a, (ix+20)
		and $0C
		jr z, +++
		ld hl, $0000
		ld (_RAM_C19B_), hl
		jp _LABEL_2B4B_
	
+++:	
		ld a, (ix+20)
		bit 1, a
		jr z, _LABEL_2B4B_
		res 1, a
		ld (ix+20), a
_LABEL_2AEE_:	
		ld h, (ix+4)
		ld l, (ix+7)
		ld b, $04
		push hl
		call _LABEL_24A9_
		pop hl
		ld a, (_RAM_C199_)
		bit 7, a
		jr nz, _LABEL_2B45_
		ld a, (_RAM_D369_)
		bit 0, a
		jr nz, +
		ld a, (_RAM_C127_)
		ld c, a
		and $05
		ld a, $15
		jr z, _LABEL_2B36_
+:	
		ld a, (_RAM_C199_)
		and a
		jr z, +
		cp $17
		jr nz, _LABEL_2B36_
		ld c, a
		ld a, (_RAM_C127_)
		rra
		ld a, c
		jr c, _LABEL_2B36_
+:	
		ld a, (_RAM_C127_)
		ld c, a
		bit 0, c
		ld a, $15
		jr z, _LABEL_2B36_
		bit 2, c
		ld a, $16
		jr z, _LABEL_2B36_
		inc a
_LABEL_2B36_:	
		cp $19
		jr nz, +
		ld a, $67
		call _LABEL_30_
		ld a, $19
+:	
		call _LABEL_6E2_
		ret
	
_LABEL_2B45_:	
		res 7, a
		jp _LABEL_2B36_
	
	; Data from 2B4A to 2B4A (1 bytes)
	.db $C9
	
_LABEL_2B4B_:	
		ld hl, (_RAM_C196_)
		ld de, $0010
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		cp $0F
		jr c, _LABEL_2BC5_
		ld a, $C0
_LABEL_2B5F_:	
		ld hl, _RAM_C198_
		bit 5, (hl)
		jr nz, _LABEL_2AEE_
		call _LABEL_2257_
		ld a, $30
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_2B74_:	
		ld hl, (_RAM_C196_)
		inc l
		inc l
		bit 5, (hl)
		ret z
		res 5, (hl)
		bit 0, (hl)
		jr z, ++
		res 0, (hl)
		res 1, (hl)
		ld a, (_RAM_C198_)
		bit 3, a
		jr nz, +
		ld a, $95
		rst $30	; _LABEL_30_
	; Data from 2B90 to 2B91 (2 bytes)
	.db $18 $03
	
+:	
		ld a, $96
		rst $30	; _LABEL_30_
	; Data from 2B95 to 2BA1 (13 bytes)
	.db $21 $98 $C1 $CB $76 $20 $03 $CB $F6 $C9 $CB $EE $C9
	
++:	
		ld hl, _RAM_C198_
		bit 7, (hl)
		ret nz
		set 7, (hl)
		ld hl, (_RAM_C232_)
		ld (_RAM_C19B_), hl
		ld de, $0260
		call _LABEL_2298_
		ld a, (_RAM_C198_)
		bit 3, a
		jr nz, +
		ld a, $95
		rst $30	; _LABEL_30_
	; Data from 2BC0 to 2BC0 (1 bytes)
	.db $C9
	
+:	
		ld a, $96
		rst $30	; _LABEL_30_
	; Data from 2BC4 to 2BC4 (1 bytes)
	.db $C9
	
_LABEL_2BC5_:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
	; Data from 2BCB to 2BDE (20 bytes)
	.db $3A $27 $C1 $1F $DA $DA $06 $3E $03 $CD $2B $09 $DA $DA $06 $1E
	.db $D0 $C3 $EE $2B
	
; 44th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2BDF_:	
		ld a, (_RAM_C127_)
		rra
		jp c, _LABEL_6DA_
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_3719_
		call _LABEL_225F_
		ld a, $88
		call _LABEL_2257_
		ld a, $0C
		ld b, $04
		call _LABEL_22F1_
		ld hl, _RAM_D369_
		set 0, (hl)
		ret
	
; 43rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2C06_:	
		call _LABEL_233B_
		cp $30
		jr c, +
		ld b, $04
		rst $28	; _LABEL_28_
		bit 5, (ix+2)
		ret z
		ld hl, _RAM_C127_
		set 0, (hl)
		ld hl, _RAM_D369_
		res 0, (hl)
		ld a, $8F
		call _LABEL_A99_
		ld a, $04
		call _LABEL_615E_
+:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 46th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2C2F_:	
		ld a, $02
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_3719_
		call _LABEL_225F_
		ld a, $8E
		rst $30	; _LABEL_30_
	; Data from 2C40 to 2C4C (13 bytes)
	.db $3E $88 $CD $57 $22 $3E $30 $06 $03 $CD $F1 $22 $C9
	
; 45th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2C4D_:	
		ld hl, _RAM_C19C_
		ld a, $08
		cp (hl)
		jr z, +
		inc (hl)
		call _LABEL_233B_
		ret
	
+:	
		call _LABEL_233B_
		cp $30
		jr c, +
		ld b, $03
		rst $28	; _LABEL_28_
		bit 5, (ix+2)
		ret z
		ld hl, _RAM_C127_
		set 2, (hl)
		ld a, $8F
		call _LABEL_A99_
		ld a, $04
		call _LABEL_615E_
+:	
		ld b, $03
		call _LABEL_24A9_
		ret
	
; 48th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2C7E_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_3719_
		call _LABEL_225F_
		ld a, $94
		rst $30	; _LABEL_30_
	; Data from 2C8F to 2C9B (13 bytes)
	.db $3E $88 $CD $57 $22 $3E $30 $06 $04 $CD $F1 $22 $C9
	
; 47th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2C9C_:	
		call _LABEL_233B_
		cp $08
		jr c, +
		ld b, $04
		rst $28	; _LABEL_28_
		bit 5, (ix+2)
		ret z
		ld hl, _RAM_C127_
		set 3, (hl)
		ld a, $8F
		call _LABEL_A99_
		ld a, $03
		call _LABEL_615E_
+:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 50th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2CC0_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_3719_
		call _LABEL_225F_
		ld a, $8F
		rst $30	; _LABEL_30_
	; Data from 2CD1 to 2CDF (15 bytes)
	.db $06 $04 $3E $88 $CD $57 $22 $3E $30 $06 $04 $CD $F1 $22 $C9
	
; 49th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2CE0_:	
		call _LABEL_233B_
		cp $30
		jr c, +
		ld b, $04
		rst $28	; _LABEL_28_
		bit 5, (ix+2)
		ret z
		ld a, $10
		call _LABEL_648D_
		ld a, $9D
		call _LABEL_A99_
		ld a, $05
		call _LABEL_615E_
+:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 72nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2D04_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_3705_
		call _LABEL_225F_
		ld a, $1F
		ld b, $01
		rst $30	; _LABEL_30_
	; Data from 2D19 to 2D20 (8 bytes)
	.db $3E $48 $06 $04 $CD $F1 $22 $C9
	
; 71st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2D21_:	
		ld ix, (_RAM_C196_)
		bit 2, (ix+2)
		jr nz, ++
		call _LABEL_233B_
		cp $40
		jr c, +
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
++:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
; 164th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2D42_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, $0080
		ld (_RAM_C19B_), hl
		ld bc, _DATA_36E9_
		call _LABEL_225F_
		ld a, $8D
		rst $30	; _LABEL_30_
	; Data from 2D59 to 2D60 (8 bytes)
	.db $3E $7F $06 $04 $CD $F1 $22 $C9
	
; 163rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2D61_:	
		call _LABEL_233B_
		cp $38
		jp c, _LABEL_2DE6_
		bit 2, (ix+2)
		jp nz, _LABEL_2DEC_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jr nz, _LABEL_2DD4_
		bit 1, a
		jr nz, +
		ld b, $38
		call _LABEL_22D9_
		jr nc, _LABEL_2D8A_
		ld hl, _RAM_C198_
		set 1, (hl)
_LABEL_2D8A_:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld de, (_RAM_C19B_)
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		cp $30
		jr nc, +
		ld hl, $0080
		ld (_RAM_C19B_), hl
		jp ++
	
+:	
		ld hl, $FF80
		ld (_RAM_C19B_), hl
++:	
		ld de, $FF20
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld hl, _RAM_C199_
		ld a, $08
		inc (hl)
		cp (hl)
		jr c, +
		ld a, $83
		ld b, $04
		call _LABEL_22F1_
		jp _LABEL_2D8A_
	
+:	
		add a, a
		cp (hl)
		jr nc, +
		xor a
		ld (hl), a
+:	
		ld a, $87
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_2DD4_:	
		call _LABEL_22A6_
		cp $10
		jr c, _LABEL_2DE6_
		ld a, $8B
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_2DE6_:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
_LABEL_2DEC_:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
_LABEL_2DF2_:	
		push bc
		ld h, (ix+4)
		ld l, (ix+7)
		ld c, (ix+9)
		dec c
		ld a, c
		add a, $28
		call _LABEL_6E2_
		xor a
		ld a, c
		call _LABEL_615E_
		pop bc
		call _LABEL_24A9_
		ret
	
; 84th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2E0D_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_287F_
		call _LABEL_225F_
		ld a, $45
		rst $30	; _LABEL_30_
	; Data from 2E1E to 2E2F (18 bytes)
	.db $3E $C0 $CD $57 $22 $3E $4C $06 $04 $CD $F1 $22 $3E $95 $CD $99
	.db $0A $C9
	
; 83rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2E30_:	
		call _LABEL_2F13_
		ld a, (_RAM_C19A_)
		and $10
		ret z
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $5D
		call _LABEL_6E2_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 85th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2E4A_:	
		call _LABEL_2F13_
		ld a, (_RAM_C19A_)
		and $10
		ret z
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $5C
		call _LABEL_6E2_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 87th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2E64_:	
		call _LABEL_2F13_
		ld a, (_RAM_C19A_)
		and $10
		ret z
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $08
		call _LABEL_6E2_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 89th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2E7E_:	
		call _LABEL_2F13_
		ld a, (_RAM_C19A_)
		and $10
		ret z
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $09
		call _LABEL_6E2_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 91st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2E98_:	
		call _LABEL_2F13_
		ld a, (_RAM_C19A_)
		and $10
		ret z
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $0A
		call _LABEL_6E2_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 93rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2EB2_:	
		call _LABEL_2F13_
		ld a, (_RAM_C19A_)
		and $10
		ret z
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $0B
		call _LABEL_6E2_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 193rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2ECC_:	
		call _LABEL_2F13_
		ld a, (_RAM_C19A_)
		and $10
		ret z
		ld h, (ix+4)
		ld l, (ix+7)
		ld de, $1800
		add hl, de
		ld a, $58
		call _LABEL_6E2_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 215th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2EEA_:	
		call _LABEL_2F13_
		ld a, (_RAM_C19A_)
		and $10
		ret z
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $3F
		call _LABEL_6E2_
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 227th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2F04_:	
		call _LABEL_2F13_
		ld a, (_RAM_C19A_)
		and $10
		ret z
		ld b, $04
		call _LABEL_24A9_
		ret
	
_LABEL_2F13_:	
		call _LABEL_233B_
		cp $38
		jr c, ++++
		ld a, $04
		ld hl, _RAM_C199_
		inc (hl)
		ld c, a
		cp (hl)
		jr c, +
		ld a, $4C
		ld b, $04
		call _LABEL_22F1_
		jp ++
	
+:	
		add a, c
		dec a
		cp (hl)
		inc d
		jr c, +++
		ld a, $50
		ld b, $04
		call _LABEL_22F1_
++:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+++:	
		ld hl, _RAM_C19A_
		ld (hl), $10
		ret
	
++++:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 60th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2F4B_:	
		ld a, $58
		jr +
	
; 62nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2F4F_:	
		ld a, $5A
		jr +
	
; 64th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2F53_:	
		ld a, $5C
		jr +
	
; 66th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2F57_:	
		ld a, $5E
+:	
		rst $30	; _LABEL_30_
	; Data from 2F5A to 2F77 (30 bytes)
	.db $F7 $3E $0A $CD $2B $09 $DA $DA $06 $1E $C8 $01 $7F $28 $CD $5F
	.db $22 $3E $C0 $CD $57 $22 $3E $27 $06 $07 $CD $F1 $22 $C9
	
; 59th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2F78_:	
		call _LABEL_233B_
		cp $3C
		jr c, +
		ld b, $07
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld b, $07
		call _LABEL_24A9_
		ld a, $1C
		rst $30	; _LABEL_30_
	; Data from 2F8B to 2F8E (4 bytes)
	.db $3E $1A $F7 $C9
	
; 68th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2F8F_:	
		ld a, $60
		rst $30	; _LABEL_30_
	; Data from 2F92 to 2FB0 (31 bytes)
	.db $3E $06 $CD $2B $09 $DA $DA $06 $1E $D0 $01 $7F $28 $CD $5F $22
	.db $06 $09 $3E $C0 $CD $57 $22 $3E $27 $06 $09 $CD $F1 $22 $C9
	
; 67th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2FB1_:	
		call _LABEL_233B_
		cp $3C
		jr c, +
		ld b, $09
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld b, $09
		call _LABEL_24A9_
		ret
	
; 54th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2FC2_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_287F_
		call _LABEL_225F_
		ld a, $66
		call _LABEL_30_
		ld a, $C0
		call _LABEL_2257_
		ld b, $04
		ld a, $BC
		call _LABEL_22F1_
		ld a, (_RAM_C129_)
		ld (_RAM_C19C_), a
		ld a, $08
		ld (_RAM_C000_), a
		ret
	
; 52nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_2FED_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, _RAM_C127_
		set 1, (hl)
		ld bc, _DATA_287F_
		call _LABEL_225F_
		ld a, $86
		call _LABEL_A99_
		ld a, $67
		call _LABEL_30_
		ld a, $C0
		call _LABEL_2257_
		ld b, $04
		ld a, $30
		call _LABEL_22F1_
		ld a, (_RAM_C129_)
		ld (_RAM_C19C_), a
		ret
	
; 53rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_301D_:	
		ld a, (_RAM_C16B_)
		bit 7, a
		jr z, +
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $6B
		call _LABEL_6E2_
		jp _LABEL_31A5_
	
+:	
		ld hl, _RAM_C19B_
		ld (hl), $00
		ld a, $48
		ld hl, _RAM_C19A_
		inc (hl)
		cp (hl)
		jr nc, +
		xor a
		ld (hl), a
		inc hl
		inc hl
		dec a
		ld (hl), a
		ld a, $01
		call _LABEL_64A1_
		ld ix, (_RAM_C196_)
+:	
		ld hl, $C198
		ld a, (_RAM_C198_)
		bit 3, a
		jr nz, _LABEL_307B_
		ld a, (_RAM_CC36_)
		ld hl, _RAM_C198_
		cp $04
		jr nc, +
		set 0, (hl)
		set 3, (hl)
		ld hl, _RAM_C19A_
		ld (hl), $48
		jp _LABEL_307B_
	
+:	
		ld a, (_RAM_C16B_)
		and $20
		jr nz, _LABEL_307B_
		ld hl, _RAM_C198_
		res 0, (hl)
; 51st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_307B_:	
		ld a, $05
		ld hl, _RAM_C199_
		cp (hl)
		jr z, +
		inc (hl)
		jp ++
	
+:	
		ld b, $04
		rst $28	; _LABEL_28_
++:	
		ld a, (_RAM_C198_)
		bit 0, a
		jp nz, _LABEL_3190_
		ld a, (_RAM_C129_)
		ld c, a
		ld a, (_RAM_C19C_)
		cp c
		jr z, +
		ld a, (_RAM_C128_)
		or a
		jr z, +
		ld hl, _RAM_C198_
		set 0, (hl)
+:	
		ld a, (_RAM_C16B_)
		and $20
		jr z, +
		ld hl, _RAM_C198_
		set 0, (hl)
+:	
		ld hl, _RAM_C19B_
		dec (hl)
		jr nz, ++
		ld a, (_RAM_C198_)
		bit 1, a
		jr nz, +
		set 1, a
		ld (_RAM_C198_), a
		jr ++
	
+:	
		bit 2, a
		jr nz, +
		set 2, a
		ld (_RAM_C198_), a
		jr ++
	
+:	
		and $01
		set 0, a
		ld (_RAM_C198_), a
++:	
		ld hl, _RAM_C19D_
		inc (hl)
		ld a, (hl)
		rra
		rra
		jr c, +
		ld de, $0A00
		jp ++
	
+:	
		ld de, $0B00
++:	
		ld hl, _RAM_C198_
		bit 7, (hl)
		jr nz, ++
		ld a, (_RAM_C224_)
		add a, d
		sub (ix+4)
		push af
		jr nc, +
		neg
+:	
		cp $03
		jr nc, +
		set 7, (hl)
+:	
		pop af
		bit 7, (ix+17)
		jr c, +++
		ld de, $0006
		jr z, ++++
		ld de, $0014
		jr ++++
	
++:	
		ld hl, (_RAM_C223_)
		add hl, de
		ld (ix+3), l
		ld (ix+4), h
		jr +++++
	
+++:	
		ld de, $FFFA
		jr nz, ++++
		ld de, $FFEC
++++:	
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
+++++:	
		ld de, $F600
		ld hl, _RAM_C198_
		bit 6, (hl)
		jr nz, ++
		ld a, (_RAM_C227_)
		add a, d
		sub (ix+7)
		push af
		jr nc, +
		neg
+:	
		cp $03
		jr nc, +
		set 6, (hl)
+:	
		pop af
		bit 7, (ix+19)
		jr c, +++
		ld de, $0006
		jr z, ++++
		ld de, $0014
		jr ++++
	
++:	
		ld hl, (_RAM_C226_)
		add hl, de
		ld (ix+6), l
		ld (ix+7), h
		ret
	
+++:	
		ld de, $FFFA
		jr nz, ++++
		ld de, $FFEC
++++:	
		call _LABEL_2289_
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ret
	
	; Data from 3178 to 318F (24 bytes)
	.db $3A $24 $C2 $C6 $08 $2A $96 $C1 $11 $04 $00 $19 $77 $3A $27 $C2
	.db $D6 $08 $11 $03 $00 $19 $77 $C9
	
_LABEL_3190_:	
		ld hl, (_RAM_C196_)
		ld de, $0068
		rst $18	; _LABEL_18_
		cp $88
		jr nc, _LABEL_31A5_
		ld de, $0078
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		cp $B0
		ret c
_LABEL_31A5_:	
		ld a, (_RAM_C195_)
		and $7F
		cp $1A
		jr z, +
		call _LABEL_51E6_
		ld hl, _RAM_C127_
		res 1, (hl)
		ld b, $04
		call _LABEL_24A9_
		ret
	
+:	
		ld a, $04
		ld (_RAM_C000_), a
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 106th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_31C7_:	
		ld a, $06
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, $0080
		ld (_RAM_C19B_), hl
		ld (_RAM_C19F_), hl
		ld hl, $FF80
		ld (_RAM_C19D_), hl
		ld e, $48
		jp _LABEL_3231_
	
; 112th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_31E3_:	
		ld a, $06
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, $00A0
		ld (_RAM_C19B_), hl
		ld (_RAM_C19F_), hl
		ld hl, $FF60
		ld (_RAM_C19D_), hl
		ld e, $48
		jp _LABEL_3231_
	
; 56th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_31FF_:	
		ld a, $06
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, $FFA0
		ld (_RAM_C19B_), hl
		ld (_RAM_C19F_), hl
		ld hl, $0060
		ld (_RAM_C19D_), hl
		jr +
	
; 58th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3218_:	
		ld a, $06
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, $FF80
		ld (_RAM_C19B_), hl
		ld (_RAM_C19F_), hl
		ld hl, $0080
		ld (_RAM_C19D_), hl
+:	
		ld e, $C0
_LABEL_3231_:	
		ld bc, _DATA_371D_
		call _LABEL_225F_
		ld a, $56
		rst $30	; _LABEL_30_
	; Data from 323A to 3251 (24 bytes)
	.db $ED $5B $9B $C1 $DD $2A $96 $C1 $CD $9F $22 $3E $80 $CD $57 $22
	.db $06 $09 $3E $76 $CD $F1 $22 $C9
	
; 55th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3252_:	
		call _LABEL_233B_
		cp $38
		jp c, _LABEL_32FE_
		bit 2, (ix+2)
		jp nz, _LABEL_3304_
		ld a, (ix+7)
		cp $BF
		ld de, $FFF8
		call c, _LABEL_227A_
		ld hl, (_RAM_C19F_)
		rl h
		sbc a, a
		ld c, a
		ld a, $4A
		jr c, +
		ld a, $B8
+:	
		ld b, a
		cp (ix+7)
		sbc a, a
		xor c
		jr z, +
		ld c, (ix+7)
		ld (ix+7), b
		push bc
		ld hl, $0606
		ld a, $7F
		call _LABEL_392F_
		pop bc
		ld (ix+7), c
		jr ++
	
+:	
		ld a, $7F
		ld hl, $0606
		call _LABEL_392F_
++:	
		ld a, (ix+20)
		rra
		rra
		jr c, +
		rra
		jr c, ++
		rra
		jr c, +++
		jr +++++
	
+:	
		ld hl, (_RAM_C196_)
		ld de, $0010
		add hl, de
		ld (hl), $30
		inc l
		ld (hl), $00
		jr +++++
	
++:	
		ld a, (_RAM_C1A0_)
		rla
		jr nc, +++++
		jr ++++
	
+++:	
		ld a, (_RAM_C1A0_)
		rla
		jr c, +++++
++++:	
		ld de, (_RAM_C19F_)
		ld hl, $0000
		or a
		sbc hl, de
		ld (_RAM_C19F_), hl
		ex de, hl
		call _LABEL_229F_
		jr +++++
	
+++++:	
		ld e, (ix+16)
		ld d, (ix+17)
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		cp $0C
		jr c, _LABEL_32FE_
		ld de, (_RAM_C19F_)
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		cp $3C
		jr c, _LABEL_32FE_
		cp $C8
		jr nc, _LABEL_32FE_
		ld b, $09
		rst $28	; _LABEL_28_
		ret
	
_LABEL_32FE_:	
		ld b, $09
		call _LABEL_24A9_
		ret
	
_LABEL_3304_:	
		ld a, $04
		ld (ix+9), a
		ld b, $09
		call _LABEL_2DF2_
		ret
	
	; Data from 330F to 3327 (25 bytes)
	.db $2A $96 $C1 $01 $10 $00 $09 $4E $23 $46 $E5 $60 $69 $EB $ED $52
	.db $EB $E1 $7A $E6 $FC $77 $2B $73 $C9
	
; 98th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3328_:	
		ld a, $0A
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_36E9_
		call _LABEL_225F_
		ld b, $02
		ld a, $80
		rst $30	; _LABEL_30_
	; Data from 333B to 334E (20 bytes)
	.db $10 $FD $06 $07 $3E $B5 $CD $F1 $22 $11 $C0 $04 $DD $2A $96 $C1
	.db $CD $98 $22 $C9
	
; 97th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_334F_:	
		call _LABEL_233B_
		cp $3C
		jp c, _LABEL_33DE_
		bit 2, (ix+2)
		jp nz, _LABEL_33FD_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jr nz, _LABEL_33C3_
		bit 0, a
		jr nz, +
		ld b, $0A
		call _LABEL_22D9_
		jr nc, +
		call _LABEL_33E4_
+:	
		ld de, $FFA0
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld a, $08
		call _LABEL_22C4_
		ld a, d
		and a
		jr nz, ++
		ld a, (_RAM_C198_)
		bit 0, a
		jp nz, +
		ld b, $07
		ld a, $B5
		call _LABEL_22F1_
		ld b, $07
		jr _LABEL_33A2_
	
+:	
		ld b, $06
		ld a, $B5
		call _LABEL_22F1_
		ld b, $06
_LABEL_33A2_:	
		rst $28	; _LABEL_28_
		ret
	
++:	
		ld a, (_RAM_C198_)
		bit 0, a
		jr nz, +
		ld b, $06
		ld a, $AF
		call _LABEL_22F1_
		ld b, $07
		jp _LABEL_33A2_
	
+:	
		ld b, $06
		ld a, $AF
		call _LABEL_22F1_
		ld b, $06
		jp _LABEL_33A2_
	
_LABEL_33C3_:	
		ld hl, _RAM_C198_
		bit 0, (hl)
		call z, _LABEL_33E4_
		call _LABEL_22A6_
		cp $10
		jp c, _LABEL_33DE_
		ld a, $A9
		ld b, $06
		call _LABEL_22F1_
		ld b, $06
		rst $28	; _LABEL_28_
		ret
	
_LABEL_33DE_:	
		ld b, $07
		call _LABEL_24A9_
		ret
	
_LABEL_33E4_:	
		ld hl, _RAM_C198_
		set 0, (hl)
		ld b, $07
		call _LABEL_57B_
		ld b, $06
		rst $28	; _LABEL_28_
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $31
		call _LABEL_6E2_
		ret
	
_LABEL_33FD_:	
		ld b, $07
		call _LABEL_2DF2_
		ret
	
; 100th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3403_:	
		ld a, $00
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_3701_
		call _LABEL_225F_
		ld b, $01
		ld a, $BB
		call _LABEL_22F1_
		ret
	
; 99th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3419_:	
		call _LABEL_233B_
		cp $3C
		jr c, +
		ld b, $01
		rst $28	; _LABEL_28_
		ld de, $FFF0
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		cp $10
		ret nc
+:	
		ld b, $01
		call _LABEL_24A9_
		ret
	
; 124th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3436_:	
		ld a, (_RAM_C131_)
		bit 6, a
		jp nz, _LABEL_6DA_
		ld a, (_RAM_C135_)
		and a
		jr nz, +
		ld a, $7B
		jp ++
	
+:	
		dec a
		jr nz, +
		ld a, $7C
		jp ++
	
+:	
		dec a
		jr nz, +
		ld a, $7D
		jp ++
	
+:	
		ld a, $7E
++:	
		rst $30	; _LABEL_30_
	; Data from 345C to 3481 (38 bytes)
	.db $3E $03 $CD $2B $09 $DA $DA $06 $1E $D0 $ED $53 $9A $C1 $21 $00
	.db $20 $19 $EB $01 $09 $37 $CD $5F $22 $3E $90 $CD $57 $22 $06 $04
	.db $3E $30 $CD $F1 $22 $C9
	
; 123rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3482_:	
		call _LABEL_233B_
		cp $3C
		jr c, _LABEL_34DD_
		ld a, (_RAM_C198_)
		bit 0, a
		jr nz, +
		ld hl, (_RAM_C196_)
		inc l
		inc l
		bit 5, (hl)
		ret z
		res 5, (hl)
		ld de, (_RAM_C19A_)
		inc l
		inc l
		ld (hl), d
		ld hl, _RAM_C198_
		set 0, (hl)
		ld a, $A0
		call _LABEL_A99_
		ld a, $88
		call _LABEL_2257_
+:	
		bit 5, (ix+2)
		jr nz, +
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld hl, _RAM_C135_
		inc (hl)
		ld a, (hl)
		cp $04
		jr nz, +
		xor a
		ld (hl), a
		call _LABEL_616F_
+:	
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $3F
		call _LABEL_6E2_
		ld a, $9B
		call _LABEL_A99_
		ld hl, _RAM_C131_
		set 6, (hl)
_LABEL_34DD_:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 132nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_34E3_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, $0C00
		add hl, de
		ex de, hl
		ld bc, _DATA_3709_
		call _LABEL_225F_
		ld a, $7F
		rst $30	; _LABEL_30_
	; Data from 34F9 to 3505 (13 bytes)
	.db $3E $90 $CD $57 $22 $06 $04 $3E $30 $CD $F1 $22 $C9
	
; 131st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3506_:	
		call _LABEL_233B_
		cp $3C
		jr c, +++
		ld a, (_RAM_C198_)
		bit 7, a
		jr nz, +
		bit 5, (ix+2)
		ret z
		ld a, (ix+4)
		sub $0C
		ld (ix+4), a
		ld a, $88
		ld (ix+2), a
		ld hl, _RAM_C198_
		set 7, (hl)
		ret
	
+:	
		ld a, $20
		ld hl, _RAM_C19D_
		cp (hl)
		jr z, +
		inc (hl)
		jr ++
	
+:	
		bit 5, (ix+2)
		jr nz, ++++
++:	
		ld b, $04
		rst $28	; _LABEL_28_
		ld a, $FE
		ld hl, _RAM_C199_
		inc (hl)
		cp (hl)
		ret nz
+++:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
++++:	
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $0B
		call _LABEL_6E2_
		ld b, $04
		call _LABEL_24A9_
		ld a, $06
		call _LABEL_615E_
		ld a, $02
		call _LABEL_648D_
		ld a, $8D
		call _LABEL_A99_
		ret
	
; 148th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_356E_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, _RAM_C19B_
		ld (hl), $50
		jp +
	
; 150th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_357E_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld a, $50
		ld (_RAM_C19B_), a
		ld (_RAM_C19A_), a
		jp +
	
; 152nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3591_:	
		jp _LABEL_6DA_
	
; 154th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3594_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld a, $50
		ld (_RAM_C19B_), a
		ld (_RAM_C19A_), a
		ld e, $CC
		jp ++
	
; 146th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_35A9_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
+:	
		ld e, $D0
++:	
		ld bc, _DATA_36E9_
		call _LABEL_225F_
		ld a, $17
		rst $30	; _LABEL_30_
	; Data from 35BC to 35BC (1 bytes)
	.db $C9
	
; 147th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_35BD_:	
		ld a, (_RAM_C19B_)
		ld hl, _RAM_C19A_
		inc (hl)
		cp (hl)
		jr c, +
		ld de, $0040
		ld l, (ix+3)
		ld h, (ix+4)
		add hl, de
		ld (ix+3), l
		ld (ix+4), h
		jp _LABEL_35F4_
	
+:	
		ld a, (_RAM_C19B_)
		add a, a
		dec a
		cp (hl)
		jr nc, +
		xor a
		ld (hl), a
+:	
		ld de, $FFC0
		ld l, (ix+3)
		ld h, (ix+4)
		add hl, de
		ld (ix+3), l
		ld (ix+4), h
; 145th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_35F4_:	
		ld hl, (_RAM_CB3D_)
		ld e, (ix+3)
		ld d, (ix+4)
		add hl, de
		ld (ix+3), l
		ld (ix+4), h
		ld de, (_RAM_CB3B_)
		ld l, (ix+6)
		ld h, (ix+7)
		xor a
		sbc hl, de
		ld (ix+6), l
		ld (ix+7), h
		ld a, h
		cp $3C
		jr c, _LABEL_3666_
		bit 2, (ix+2)
		jr nz, _LABEL_366C_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jr nz, ++
		ld b, $04
		rst $28	; _LABEL_28_
		ld hl, _RAM_C199_
		ld a, $08
		inc (hl)
		cp (hl)
		jr c, +
		ld a, $8F
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld a, $10
		cp (hl)
		jr nc, +
		xor a
		ld (hl), a
+:	
		ld a, $8B
		ld b, $04
		call _LABEL_22F1_
-:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
++:	
		call _LABEL_22A6_
		cp $10
		jr c, _LABEL_3666_
		ld a, $93
		ld b, $04
		call _LABEL_22F1_
		jp -
	
_LABEL_3666_:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
_LABEL_366C_:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
; 192nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3672_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_36ED_
		call _LABEL_225F_
		ld a, $20
		rst $30	; _LABEL_30_
	; Data from 3683 to 3688 (6 bytes)
	.db $3E $88 $CD $57 $22 $C9
	
; 190th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3689_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_36ED_
		call _LABEL_225F_
		ld a, $1F
		ld b, $01
		rst $30	; _LABEL_30_
	; Data from 369C to 36A8 (13 bytes)
	.db $3E $48 $06 $04 $CD $F1 $22 $3E $88 $CD $57 $22 $C9
	
; 191st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_36A9_:	
		ld a, $08
		call _LABEL_22C4_
		ld a, d
		and a
		jr nz, +
		ld a, $40
		ld b, $04
		call _LABEL_22F1_
		jr _LABEL_36C2_
	
+:	
		ld a, $44
		ld b, $04
		call _LABEL_22F1_
; 189th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_36C2_:	
		call _LABEL_233B_
		cp $40
		jr c, ++
		bit 5, (ix+2)
		jr nz, +
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld hl, _RAM_C222_
		set 3, (hl)
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $60
		call _LABEL_6E2_
++:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
; Data from 36E9 to 36EC (4 bytes)	
_DATA_36E9_:	
	.db $02 $02 $03 $20
	
; Data from 36ED to 36F0 (4 bytes)	
_DATA_36ED_:	
	.db $05 $05 $03 $80
	
; Data from 36F1 to 36F4 (4 bytes)	
_DATA_36F1_:	
	.db $03 $03 $04 $20
	
; Data from 36F5 to 36F8 (4 bytes)	
_DATA_36F5_:	
	.db $05 $05 $04 $08
	
; Data from 36F9 to 36FC (4 bytes)	
_DATA_36F9_:	
	.db $03 $03 $04 $10
	
; Data from 36FD to 3700 (4 bytes)	
_DATA_36FD_:	
	.db $03 $03 $05 $20
	
; Data from 3701 to 3704 (4 bytes)	
_DATA_3701_:	
	.db $02 $02 $03 $20
	
; Data from 3705 to 3708 (4 bytes)	
_DATA_3705_:	
	.db $03 $03 $05 $08
	
; Data from 3709 to 370C (4 bytes)	
_DATA_3709_:	
	.db $02 $02 $05 $80
	
; Data from 370D to 3718 (12 bytes)	
_DATA_370D_:	
	.db $03 $05 $04 $20 $05 $05 $05 $80 $05 $05 $04 $80
	
; Data from 3719 to 371C (4 bytes)	
_DATA_3719_:	
	.db $05 $05 $04 $80
	
; Data from 371D to 3720 (4 bytes)	
_DATA_371D_:	
	.db $04 $04 $04 $28
	
; Data from 3721 to 3724 (4 bytes)	
_DATA_3721_:	
	.db $03 $03 $02 $20
	
; 130th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3725_:	
		ld a, (_RAM_C128_)
		cp $28
		jr nz, +
		ld a, (_RAM_C132_)
+:	
		and $FC
		and a
		jr nz, +
		ld a, $82
		jr _LABEL_377A_
	
+:	
		cp $04
		jr nz, +
		ld a, $83
		jr _LABEL_377A_
	
+:	
		cp $08
		jr nz, +
		ld a, $84
		jr _LABEL_377A_
	
+:	
		cp $0C
		jr nz, +
		ld a, $85
		jr _LABEL_377A_
	
+:	
		cp $10
		jr nz, +
		ld a, $86
		jr _LABEL_377A_
	
+:	
		cp $14
		jr nz, +
		ld a, $87
		jr _LABEL_377A_
	
+:	
		cp $18
		jr nz, +
		ld a, $88
		jr _LABEL_377A_
	
+:	
		cp $1C
		jr nz, +
		ld a, $89
		jr _LABEL_377A_
	
+:	
		cp $20
		jr nz, +
		ld a, $8A
		jr _LABEL_377A_
	
+:	
		ld a, $8B
_LABEL_377A_:	
		rst $30	; _LABEL_30_
	; Data from 377B to 3799 (31 bytes)
	.db $3E $03 $CD $2B $09 $DA $DA $06 $01 $19 $37 $CD $5F $22 $3E $C8
	.db $CD $57 $22 $3E $1C $06 $04 $CD $F1 $22 $21 $A8 $CC $34 $C9
	
; 129th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_379A_:	
		call _LABEL_233B_
		cp $38
		jr c, ++
		ld b, $04
		rst $28	; _LABEL_28_
		ld a, (_RAM_C198_)
		bit 7, a
		jr nz, +
		ld a, $20
		ld hl, _RAM_C199_
		inc (hl)
		cp (hl)
		ret nc
		ld hl, _RAM_C198_
		set 7, (hl)
		res 6, (ix+2)
+:	
		bit 5, (ix+2)
		ret z
		ld hl, _RAM_C16D_
		set 0, (hl)
		ld a, (_RAM_C128_)
		cp $40
		jr z, ++
		ld hl, _RAM_C131_
		set 7, (hl)
		ld a, $A5
		call _LABEL_A99_
++:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 212th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_37DD_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_36E9_
		call _LABEL_225F_
		ld a, $80
		call _LABEL_2257_
		ld a, $93
		rst $30	; _LABEL_30_
	; Data from 37F5 to 37FA (6 bytes)
	.db $3E $1D $32 $9F $C1 $C9
	
; 211th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_37FB_:	
		call _LABEL_233B_
		cp $38
		jp c, _LABEL_3881_
		bit 2, (ix+2)
		jp nz, _LABEL_389A_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_3881_
		ld de, $FF80
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld a, (_RAM_C198_)
		bit 0, a
		jr nz, +
		bit 1, a
		jr nz, ++
		ld b, $00
		call _LABEL_22E2_
		jr nc, _LABEL_386F_
		ld hl, _RAM_C198_
		set 0, (hl)
		jp _LABEL_386F_
	
+:	
		ld a, (_RAM_C19F_)
		ld hl, _RAM_C19D_
		call _LABEL_22C7_
		ld a, d
		and a
		jr nz, +
		ld de, $FFF8
		call _LABEL_227A_
		jr ++
	
+:	
		ld hl, _RAM_C198_
		set 1, (hl)
		res 0, (hl)
++:	
		ld hl, _RAM_C19A_
		ld a, $30
		call _LABEL_22C7_
		ld a, d
		and a
		jr nz, _LABEL_386F_
		ld de, $0020
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		ld a, $8F
		ld b, $04
		call _LABEL_22F1_
		jp +
	
_LABEL_386F_:	
		ld de, $FFE0
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		ld a, $8B
		ld b, $04
		call _LABEL_22F1_
+:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_3881_:	
		call _LABEL_22A6_
		cp $10
		jp c, +
		ld a, $93
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
_LABEL_389A_:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
; 214th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_38A0_:	
		ld a, $11
		ld (_RAM_D397_), a
; 213th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_38A5_:	
		ld hl, _RAM_C198_
		inc (hl)
		ld a, (hl)
		ld a, (_RAM_CC30_)
		bit 0, a
		jp nz, +
		ld a, (hl)
		rra
		rra
		rra
		and $07
		ld e, a
		ld d, $00
		ld hl, _DATA_38EC_
		add hl, de
		ld a, (hl)
		ld (_RAM_D395_), a
		ld hl, _DATA_38F4_
		add hl, de
		ld a, (hl)
		ld (_RAM_D398_), a
		ld hl, _DATA_38F4_
		add hl, de
		ld a, (hl)
		ld (_RAM_D399_), a
		ret
	
+:	
		ld a, $40
		ld (_RAM_D395_), a
		ld a, $3A
		ld (_RAM_D397_), a
		ld a, $3F
		ld (_RAM_D398_), a
		ld a, $3E
		ld (_RAM_D399_), a
		jp _LABEL_6DA_
	
	; Data from 38EB to 38EB (1 bytes)
	.db $C9
	
; Data from 38EC to 38F3 (8 bytes)	
_DATA_38EC_:	
	.db $10 $0C $08 $04 $08 $0C $10 $20
	
; Data from 38F4 to 38FB (8 bytes)	
_DATA_38F4_:	
	.db $04 $03 $02 $02 $02 $03 $04 $08
	
; 218th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_38FC_:	
		ld a, $87
		call _LABEL_A99_
; 224th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3901_:	
		call _LABEL_4392_
; 217th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3904_:	
		jp _LABEL_6DA_
	
; 230th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3907_:	
		ld a, $3B
		ld b, $03
		rst $30	; _LABEL_30_
	; Data from 390C to 3915 (10 bytes)
	.db $10 $FD $3E $63 $06 $02 $F7 $10 $FD $C9
	
; 232nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3916_:	
		ld b, $02
		ld a, $2A
		rst $30	; _LABEL_30_
	; Data from 391B to 391D (3 bytes)
	.db $10 $FD $C9
	
; 234th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_391E_:	
		ld a, $56
		rst $30	; _LABEL_30_
	; Data from 3921 to 3921 (1 bytes)
	.db $C9
	
; 236th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3922_:	
		ld a, $93
		rst $30	; _LABEL_30_
	; Data from 3925 to 3925 (1 bytes)
	.db $C9
	
; 238th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_3926_:	
		ld a, $1B
		rst $30	; _LABEL_30_
	; Data from 3929 to 3929 (1 bytes)
	.db $C9
	
; 240th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_392A_:	
		ld a, $80
		rst $30	; _LABEL_30_
	; Data from 392D to 392E (2 bytes)
	.db $F7 $C9
	
_LABEL_392F_:	
		ld (_RAM_CC01_), hl
		ld hl, _RAM_CBFF_
		ld (ix+20), $00
		ld (hl), $00
		or a
		jr nz, +
		set 1, (hl)
+:	
		bit 7, a
		jr z, +
		set 4, (hl)
+:	
		ld e, (ix+6)
		ld d, (ix+7)
		ld a, (_RAM_CBFF_)
		bit 1, a
		jr z, +
		ld a, (ix+7)
		ld de, $4400
		cp $44
		jr c, +
		ld e, (ix+6)
		ld d, (ix+7)
		cp $BC
		jr c, +
		ld de, $BC00
+:	
		call _LABEL_3AF4_
		ret c
		xor a
		cp (ix+18)
		jr nz, +
		cp (ix+19)
		jp z, _LABEL_3A1D_
+:	
		ld a, (_RAM_CBFF_)
		and $02
		jp nz, _LABEL_3A1D_
		ld a, $80
		cp (ix+19)
		jr c, _LABEL_39D2_
		ld a, (_RAM_CC05_)
		cp $C0
		jp c, _LABEL_3ADC_
		cp $E0
		jp nc, _LABEL_3ADC_
		ld c, (ix+18)
		ld b, (ix+19)
		ld l, (ix+6)
		ld h, (ix+7)
		ld (_RAM_CC0E_), bc
		add hl, bc
		ex de, hl
		ld a, (_RAM_CC03_)
		add a, a
		add a, $40
		ex af, af'
		ld a, (_RAM_CC02_)
		ld c, a
		ex af, af'
		sub c
		ld l, $00
		ld h, a
		ld a, (_RAM_C12B_)
		and $07
		ld c, l
		srl a
		rr c
		ld b, a
		or a
		sbc hl, bc
		or a
		sbc hl, de
		add hl, de
		jr nc, _LABEL_39D2_
		set 3, (ix+20)
		jr _LABEL_3A17_
	
_LABEL_39D2_:	
		ld a, (_RAM_CC05_)
		cp $C0
		jp c, _LABEL_3ADC_
		cp $E0
		jp nc, _LABEL_3ADC_
		ld c, (ix+18)
		ld b, (ix+19)
		ld l, (ix+6)
		ld h, (ix+7)
		ld (_RAM_CC0E_), bc
		add hl, bc
		ex de, hl
		ld a, (_RAM_CC03_)
		add a, a
		add a, $43
		ld c, a
		ld a, (_RAM_CC02_)
		add a, c
		ld l, $FF
		ld h, a
		ld a, (_RAM_C12B_)
		and $07
		ld c, l
		srl a
		rr c
		ld b, a
		or a
		sbc hl, bc
		or a
		sbc hl, de
		add hl, de
		jr c, _LABEL_3A1D_
		set 2, (ix+20)
_LABEL_3A17_:	
		ld (ix+6), l
		ld (ix+7), h
_LABEL_3A1D_:	
		ld a, (_RAM_CC08_)
		cp $C0
		jp c, _LABEL_3AE2_
		cp $E0
		jp c, +
		cp $F0
		jr nc, +
		ld a, (_RAM_CBFF_)
		bit 4, a
		jp z, _LABEL_3AC4_
+:	
		ld l, (ix+3)
		ld h, (ix+4)
		ld bc, (_RAM_CB3D_)
		or a
		sbc hl, bc
		ld c, (ix+16)
		ld b, (ix+17)
		ld (_RAM_CC0E_), bc
		add hl, bc
		ld a, b
		cp $80
		jr c, _LABEL_3AC4_
		ex de, hl
		ld a, (_RAM_CC07_)
		add a, a
		add a, a
		add a, $14
		ld c, a
		ld a, (_RAM_CC01_)
		add a, c
		ld l, $00
		ld h, a
		ld a, (_RAM_CC00_)
		and $07
		ld c, $00
		srl a
		rr c
		ld b, a
		add hl, bc
		ld bc, $0000
		ld a, (_RAM_CC08_)
		cp $F0
		jr c, +
		cp $F8
		jr nc, +
		exx
		sub $F0
		add a, a
		ld e, a
		ld d, $00
		ld hl, _DATA_3AE4_
		add hl, de
		ld c, (hl)
		inc hl
		ld b, (hl)
		set 7, (ix+22)
		ld (ix+23), c
		ld (ix+24), b
		push bc
		exx
		pop bc
		or a
		sbc hl, bc
		ld bc, $0000
		ld a, (_RAM_CBFF_)
		and $80
		jr z, +
		ld bc, $0400
+:	
		add hl, bc
		or a
		sbc hl, de
		add hl, de
		jr c, _LABEL_3AC4_
		set 1, (ix+20)
		or a
		sbc hl, bc
		ld bc, (_RAM_CB3D_)
		add hl, bc
		ld (ix+3), l
		ld (ix+4), h
		jr _LABEL_3AC4_
	
_LABEL_3AC4_:	
		ld a, (_RAM_CBFF_)
		bit 4, a
		ret z
		ld hl, _RAM_C16C_
		bit 2, (hl)
		ret z
		ld a, (_RAM_CC08_)
		cp $CC
		ret c
		cp $E8
		ret nc
		set 3, (hl)
		ret
	
_LABEL_3ADC_:	
		ld hl, $CBFF
		jp _LABEL_3A1D_
	
_LABEL_3AE2_:	
		jr _LABEL_3AC4_
	
; Data from 3AE4 to 3AF3 (16 bytes)	
_DATA_3AE4_:	
	.db $80 $03 $80 $02 $80 $01 $80 $00 $80 $00 $80 $01 $80 $02 $80 $03
	
_LABEL_3AF4_:	
		ld l, (ix+3)
		ld h, (ix+4)
		ld bc, (_RAM_CB3D_)
		or a
		sbc hl, bc
		bit 7, (ix+22)
		jr z, +
		exx
		ld hl, _RAM_CBFF_
		set 7, (hl)
		exx
		res 7, (ix+22)
		ld c, (ix+23)
		ld b, (ix+24)
		add hl, bc
+:	
		ld a, (_RAM_CC00_)
		ld c, $00
		and $07
		srl a
		rr c
		ld b, a
		or a
		sbc hl, bc
		ld bc, $7400
		or a
		sbc hl, bc
		jp nc, _LABEL_3BF5_
		ld b, $60
		add hl, bc
		jp nc, _LABEL_3BF5_
		ex de, hl
		ld a, (_RAM_C12B_)
		and $07
		srl a
		rr c
		ld b, a
		add hl, bc
		ld bc, $C000
		or a
		sbc hl, bc
		call nc, _LABEL_3BE2_
		ld b, $80
		add hl, bc
		call nc, _LABEL_3BEA_
		push de
		push hl
		ld a, (_RAM_CBFF_)
		and $03
		jp nz, _LABEL_3BDC_
		ld a, (_RAM_CC01_)
		sub $04
		jr nc, +
		xor a
+:	
		ld b, a
		ld c, $00
		ex de, hl
		or a
		sbc hl, bc
		ex de, hl
		ld a, (_RAM_CC02_)
		ld b, a
		ld a, $80
		cp (ix+19)
		jr c, +
		add hl, bc
		exx
		ld hl, _RAM_CC03_
		exx
		call _LABEL_3C08_
		jr _LABEL_3B8D_
	
+:	
		or a
		sbc hl, bc
		exx
		ld hl, _RAM_CC03_
		exx
		call _LABEL_3C08_
_LABEL_3B8D_:	
		pop hl
		pop de
		ld a, (_RAM_CC01_)
		ld c, $00
		ld b, a
		ex de, hl
		sbc hl, bc
		ex de, hl
		jr c, _LABEL_3BFE_
		push de
		push hl
		ld a, (_RAM_CC02_)
		sub $04
		jr nc, +
		xor a
+:	
		ld b, a
		add hl, bc
		exx
		ld hl, _RAM_CC06_
		exx
		call _LABEL_3C08_
		pop hl
		pop de
		ld c, $00
		or a
		sbc hl, bc
		exx
		ld hl, _RAM_CC09_
		exx
		call _LABEL_3C08_
		ld a, (_RAM_CC08_)
		cp $F8
		jr nc, +
		cp $F0
		ret nc
+:	
		call _LABEL_3C30_
		ld a, (_RAM_CC08_)
		cp $C0
		ret nc
		ld a, (_RAM_CC0B_)
		cp $F0
		ret nc
		ld (_RAM_CC08_), a
		or a
		ret
	
_LABEL_3BDC_:	
		xor a
		ld (_RAM_CC05_), a
		jr _LABEL_3B8D_
	
_LABEL_3BE2_:	
		ld l, c
		ld h, b
		ld a, $F8
		add a, h
		ld h, a
		jr +
	
_LABEL_3BEA_:	
		ld hl, $0800
+:	
		exx
		ld hl, _RAM_CBFF_
		set 0, (hl)
		exx
		ret
	
_LABEL_3BF5_:	
		xor a
		ld (_RAM_CC05_), a
		ld (_RAM_CC08_), a
		scf
		ret
	
_LABEL_3BFE_:	
		ld hl, _RAM_CBFF_
		set 3, (hl)
		xor a
		ld (_RAM_CC08_), a
		ret
	
_LABEL_3C08_:	
		ld a, h
		srl a
		and $3E
		exx
		ld (hl), a
		inc hl
		exx
		ld c, a
		ld a, (_RAM_C12B_)
		rra
		rra
		and $3E
		add a, c
		and $3E
		ld l, a
		ld h, $00
		srl d
		srl d
		ld a, d
		exx
		ld (hl), a
		inc hl
		exx
		call _LABEL_3C77_
		exx
		ld (hl), a
		inc hl
		exx
		ret
	
_LABEL_3C30_:	
		ld a, (_RAM_CC06_)
		ld c, a
		ld a, (_RAM_C12B_)
		rra
		rra
		and $3E
		add a, c
		and $3E
		ld l, a
		ld h, $00
		ld a, (_RAM_CC07_)
		inc a
		ld d, a
		call _LABEL_3C77_
		cp $C0
		ret c
		ex af, af'
		call _LABEL_3ECE_
		cp $07
		jr z, +
		ex af, af'
		cp $FC
		ret nc
		ex af, af'
+:	
		ex af, af'
		ld c, a
		and $E0
		cp $C0
		jr nz, +
		ld a, (_RAM_CC08_)
		and $E0
		cp $C0
		jr nz, +
		xor a
		ld (_RAM_CC08_), a
+:	
		ld a, c
		ld (_RAM_CC08_), a
		ld hl, _RAM_CC07_
		inc (hl)
		ret
	
_LABEL_3C77_:	
		ld a, $17
		sub d
		ld c, a
		ld a, (_RAM_CC00_)
		rra
		rra
		rra
		and $1F
		add a, c
-:	
		sub $1C
		jr nc, -
		add a, $1C
		ld e, h
		srl a
		rr e
		srl a
		rr e
		ld d, a
		add hl, de
		ld de, $3800
		add hl, de
		di
		call _LABEL_1DC_
		ei
		ret
	
_LABEL_3C9F_:	
		call _LABEL_3F09_
		ld hl, $0000
		ld (_RAM_CB3D_), hl
		ld (_RAM_CB3B_), hl
		ld a, (_RAM_C16D_)
		bit 6, a
		ret nz
		ld a, (_RAM_CC30_)
		bit 5, a
		jr z, +
		ld hl, _RAM_C179_
		set 0, (hl)
+:	
		ld a, (_RAM_C128_)
		cp $28
		jr nz, +
		ld a, (_RAM_C12A_)
		cp $C0
		jr c, +
		ld hl, _RAM_C179_
		set 0, (hl)
		set 2, (hl)
+:	
		ld a, (_RAM_C16D_)
		bit 1, a
		jp nz, _LABEL_4AAB_
		call _LABEL_3F29_
		ld a, (_RAM_C16C_)
		bit 6, a
		jr nz, +
		ld a, (_RAM_C16B_)
		rra
		call c, _LABEL_3EDF_
+:	
		ld a, (_RAM_C178_)
		bit 1, a
		call nz, _LABEL_4658_
		ld hl, _RAM_C170_
		inc (hl)
		inc l
		ld (hl), $00
		ld hl, _RAM_C16B_
		bit 0, (hl)
		jp z, _LABEL_3DBE_
		ld hl, _RAM_C131_
		bit 1, (hl)
		call nz, _LABEL_3EBE_
_LABEL_3D0A_:	
		ld a, (_RAM_C16C_)
		bit 7, a
		jr nz, +
		ld a, (_RAM_C178_)
		rla
		jp c, +
		ld a, (_RAM_CC34_)
		bit 7, a
		jr z, +
		ld hl, _RAM_C222_
		set 5, (hl)
+:	
		ld hl, _RAM_C16B_
		ld a, (_RAM_C222_)
		bit 5, (hl)
		jp nz, _LABEL_452B_
		bit 6, (hl)
		jp nz, _LABEL_4414_
		ld c, a
		and $38
		jp nz, _LABEL_43A0_
		bit 2, (hl)
		jr nz, +
		ld a, (_RAM_C127_)
		bit 2, a
		call nz, _LABEL_41C7_
+:	
		ld a, (_RAM_C127_)
		bit 0, a
		jr z, +
		ld a, (_RAM_C16B_)
		bit 1, a
		jp nz, _LABEL_3F8F_
		ld a, (_RAM_C174_)
		and $3F
		ld c, a
		ld a, (_RAM_C173_)
		and $30
		or $0F
		and c
		bit 4, a
		jp nz, _LABEL_3F58_
		jr _LABEL_3D6F_
	
+:	
		ld hl, _RAM_C173_
		res 4, (hl)
_LABEL_3D6F_:	
		call _LABEL_4017_
_LABEL_3D72_:	
		call _LABEL_4295_
		ld de, (_RAM_C223_)
		ld hl, (_RAM_CB3D_)
		add hl, de
		ld (_RAM_C223_), hl
		ld a, (_RAM_C224_)
		cp $08
		jp c, _LABEL_44CD_
		call _LABEL_2052_
		ld ix, $C220
		ld hl, _RAM_C16B_
		bit 7, (hl)
		call nz, _LABEL_45B7_
_LABEL_3D97_:	
		ld hl, (_RAM_CC10_)
		ld a, h
		or l
		jr nz, +
		ld a, (_RAM_C171_)
		or a
		jr z, +
		ld c, a
		ld a, (_RAM_C172_)
		cp c
		jr z, +
		ld b, a
		ld a, c
		ld (_RAM_C172_), a
_LABEL_3DB0_:	
		call _LABEL_531F_
+:	
		call _LABEL_477E_
		ld b, $0C
		ld ix, _RAM_C220_
		rst $28	; _LABEL_28_
		ret
	
_LABEL_3DBE_:	
		ld a, (_RAM_C115_)
		cp $03
		jr nz, +
		ld a, (_RAM_C131_)
		bit 0, a
		jr nz, +
		ld hl, _RAM_C175_
		bit 7, (hl)
		jp z, _LABEL_4FA4_
+:	
		ld hl, _RAM_C16B_
		set 0, (hl)
		ld de, _RAM_C220_
		ld a, $80
		ld b, $0C
		ld hl, _DATA_9D1_
		call _LABEL_971_
		ld hl, _RAM_C222_
		ld (hl), $C0
		inc l
		inc l
		ld (hl), $56
		inc l
		inc l
		inc l
		ld (hl), $60
		inc l
		call _LABEL_3EAF_
		ld a, $01
		ld (_RAM_C171_), a
		ld a, $02
		call _LABEL_A4_
		di
		call _LABEL_213_
		call _LABEL_23B_
		call _LABEL_16F7_
		ld hl, $0000
		ld (_RAM_CB3D_), hl
		ld (_RAM_CB3B_), hl
		ld a, $02
		call _LABEL_B13_
		ld hl, _DATA_9740_
		ld de, $2180
		ld bc, $0180
		call _LABEL_2AD_
-:	
		call _LABEL_4295_
		ld a, (_RAM_C234_)
		bit 1, a
		jr z, -
		ld hl, _RAM_C16C_
		set 5, (hl)
		call +++
		call _LABEL_3ECE_
		cp $01
		jr z, +
		cp $02
		jr nz, ++
+:	
		ld hl, _RAM_C16C_
		set 1, (hl)
++:	
		cp $05
		jr z, +
		cp $0A
		jr nz, ++
+:	
		ld hl, _RAM_C16C_
		set 2, (hl)
++:	
		call _LABEL_52EA_
		jp _LABEL_3D0A_
	
+++:	
		ld hl, _RAM_C170_
		ld (hl), $00
		call _LABEL_51E6_
		ld a, $02
		call _LABEL_B13_
		ld b, $04
-:	
		push bc
		ld hl, _RAM_C10C_
		inc (hl)
		ld a, $01
		call _LABEL_A4_
		ld hl, _DATA_8000_
		ld de, $2000
		ld bc, $0180
		call _LABEL_2AD_
		pop bc
		djnz -
		ld a, $80
		ld (_RAM_C222_), a
		call _LABEL_3EA5_
		ld hl, $C16C
		call _LABEL_24E_
		ei
		ld a, $02
		call _LABEL_A4_
		ld hl, _RAM_CC84_
		set 0, (hl)
		ld a, $03
		call _LABEL_A4_
		call _LABEL_224_
		ret
	
_LABEL_3EA5_:	
		ld hl, $C220
		xor a
		ld b, $0C
		call _LABEL_22F4_
		ret
	
_LABEL_3EAF_:	
		ld hl, _RAM_C38C_
		ld de, $FFE0
		ld b, $0C
		ld a, $FF
-:	
		ld (hl), a
		add hl, de
		djnz -
		ret
	
_LABEL_3EBE_:	
		res 1, (hl)
		ld a, (_RAM_C134_)
		sub $0C
		ret z
		jp nc, _LABEL_648D_
		neg
		jp _LABEL_64A1_
	
_LABEL_3ECE_:	
		ld a, (_RAM_C128_)
		ld l, a
		ld h, $00
		ld a, $07
		call _LABEL_B13_
		ld de, _DATA_1C000_
		add hl, de
		ld a, (hl)
		ret
	
_LABEL_3EDF_:	
		ld a, (_RAM_C170_)
		cp $30
		jr c, +
		ld hl, _RAM_C16C_
		set 6, (hl)
		ld hl, _RAM_C131_
		res 0, (hl)
		call _LABEL_3EA5_
		ret
	
+:	
		ex af, af'
		ld a, (_RAM_C131_)
		rra
		ret nc
		ex af, af'
		add a, $02
		ld c, a
		and $03
		ret nz
		bit 3, c
		jp z, _LABEL_3EAF_
		jp _LABEL_3EA5_
	
_LABEL_3F09_:	
		ld hl, (_RAM_CB3D_)
		xor a
		or l
		jr nz, +
		or h
		ret z
+:	
		ld c, $80
		rl l
		rl h
		rr c
		sra c
		ld a, (_RAM_CC00_)
		add a, h
		cp $E0
		jr c, +
		add a, c
+:	
		ld (_RAM_CC00_), a
		ret
	
_LABEL_3F29_:	
		ld a, (_RAM_C11F_)
		and $3F
		ld c, a
		ld hl, _RAM_C174_
		ld a, (_RAM_C16C_)
		rla
		jr c, ++
		rla
		jr nc, +
		ld a, (_RAM_C178_)
		bit 2, a
		jr nz, +
		ld a, (_RAM_CC30_)
		rra
		jr nc, +++
+:	
		ld c, $00
		jr +++
	
++:	
		ld c, $08
+++:	
		ld (hl), c
		dec l
		ld a, c
		xor $FF
		or (hl)
		or $0F
		ld (hl), a
		ret
	
_LABEL_3F58_:	
		ld hl, _RAM_C173_
		res 4, (hl)
		ld hl, _RAM_C3A2_
		bit 7, (hl)
		jr z, +
		ld hl, _RAM_C422_
		bit 7, (hl)
		jp nz, _LABEL_3D6F_
+:	
		ld a, $90
		call _LABEL_A99_
		set 7, (hl)
		ld hl, _RAM_C16B_
		set 1, (hl)
		ld hl, _RAM_C170_
		ld (hl), $00
		inc l
		ld (hl), $14
		ld a, (_RAM_C16B_)
		bit 2, a
		jr nz, _LABEL_3F8F_
		ld (hl), $09
		bit 3, a
		jr z, _LABEL_3F8F_
		ld (hl), $0B
_LABEL_3F8F_:	
		ld a, (_RAM_C170_)
		or a
		jp z, _LABEL_3D6F_
		ld c, a
		and $03
		jp nz, _LABEL_3D6F_
		ld a, (_RAM_C16B_)
		bit 2, a
		jr nz, +
		bit 3, c
		jr nz, +
		ld a, (_RAM_C172_)
		inc a
		ld (_RAM_C171_), a
		jp _LABEL_3D6F_
	
+:	
		ld hl, _RAM_C16B_
		res 1, (hl)
		jp _LABEL_3D6F_
	
_LABEL_3FB9_:	
		ld b, $04
		ld de, (_RAM_C17C_)
		ld a, $A0
		cp e
		ld a, $8C
		jr z, +
		add a, b
+:	
		ld hl, $0999
		call _LABEL_971_
		ld (ix+2), $81
		ld a, (_RAM_C224_)
		add a, $03
		ld (ix+3), $80
		ld (ix+4), a
		ld a, (_RAM_C227_)
		add a, $FE
		ld (ix+7), a
		ld (ix+16), $20
		ld (ix+17), $00
		ld a, (_RAM_C16B_)
		bit 3, a
		jr nz, +
		ld (ix+18), $00
		ld (ix+19), $02
		jr ++
	
+:	
		ld (ix+18), $00
		ld (ix+19), $FE
++:	
		ld (ix+12), $0C
		ld (ix+44), $0D
		ld (ix+76), $0E
		ld (ix+108), $0F
		ret
	
_LABEL_4017_:	
		ld hl, _RAM_C16B_
		ld a, (_RAM_C174_)
		and $3F
		ld c, a
		ld a, (_RAM_C173_)
		and c
		bit 4, (hl)
		jp nz, _LABEL_4128_
		bit 5, a
		jp nz, _LABEL_4105_
		ld a, c
		bit 2, (hl)
		jp nz, _LABEL_41D4_
		bit 2, a
		jp nz, _LABEL_4093_
		bit 3, a
		jp nz, _LABEL_4081_
		call _LABEL_46EC_
		ld hl, (_RAM_C232_)
		ld de, $0000
		or a
		sbc hl, de
		ret z
		ld d, $80
		add hl, de
		add hl, de
		ex de, hl
		ex af, af'
		ld a, (_RAM_C16C_)
		bit 3, a
		ld hl, _DATA_40F9_
		jr z, +
		ld hl, _DATA_4101_
+:	
		ld c, (hl)
		inc hl
		ld b, (hl)
		ex af, af'
		jr c, +
		inc hl
		ld c, (hl)
		inc hl
		ld b, (hl)
+:	
		ex de, hl
		add hl, bc
		ld de, $0007
		add hl, de
		ld bc, $000E
		or a
		sbc hl, bc
		add hl, bc
		jr nc, +
		ld hl, $0007
+:	
		or a
		sbc hl, de
		ld (_RAM_C232_), hl
		ret
	
_LABEL_4081_:	
		res 3, (hl)
		ld hl, (_RAM_C232_)
		ld de, $40F9
		call _LABEL_40B2_
		ld (_RAM_C232_), hl
		call _LABEL_46EC_
		ret
	
_LABEL_4093_:	
		set 3, (hl)
		ld de, (_RAM_C232_)
		ld hl, $0000
		or a
		sbc hl, de
		ld de, $40F9
		call _LABEL_40B2_
		ex de, hl
		ld hl, $0000
		sbc hl, de
		ld (_RAM_C232_), hl
		call _LABEL_46EC_
		ret
	
_LABEL_40B2_:	
		ex af, af'
		ld a, (_RAM_C16C_)
		bit 3, a
		jr z, +
		ld de, _DATA_4101_
+:	
		ex af, af'
		push de
		ld de, $8000
		add hl, de
		ld bc, $8104
		bit 4, a
		jr z, +
		ld bc, $8118
+:	
		or a
		sbc hl, bc
		add hl, bc
		ex (sp), hl
		ld c, (hl)
		inc hl
		ld b, (hl)
		jr nc, +
		inc hl
		ld c, (hl)
		inc hl
		ld b, (hl)
+:	
		pop hl
		add hl, bc
		add hl, de
		ld de, $FF03
		bit 4, a
		jr z, +
		ld de, $FEEF
+:	
		add hl, de
		ld bc, $000E
		or a
		sbc hl, bc
		add hl, bc
		jr nc, +
		ld hl, $0007
+:	
		or a
		sbc hl, de
		ret
	
; Data from 40F9 to 40FC (4 bytes)	
_DATA_40F9_:	
	.db $F2 $FF $0E $00
	
; Data from 40FD to 4100 (4 bytes)	
_DATA_40FD_:	
	.db $FC $FF $04 $00
	
; Data from 4101 to 4104 (4 bytes)	
_DATA_4101_:	
	.db $FC $FF $04 $00
	
_LABEL_4105_:	
		ld a, $8E
		call _LABEL_A99_
		exx
		ld hl, _RAM_C173_
		res 5, (hl)
		exx
		ld a, (_RAM_C174_)
		and $3F
		ld hl, _RAM_C16B_
		set 4, (hl)
		ld hl, $01F0
		and $10
		jr z, +
		ld hl, $02B3
+:	
		ld (_RAM_C230_), hl
_LABEL_4128_:	
		ld a, (_RAM_C174_)
		and $0C
		jr nz, +
		ld hl, (_RAM_C232_)
		ld de, $FFFF
		add hl, de
		ld a, $80
		cp h
		jr c, +
		ld (_RAM_C232_), hl
+:	
		ld a, (_RAM_C16B_)
		bit 2, a
		ld a, (_RAM_C174_)
		jr nz, ++
		and $3F
		ld c, a
		and $0C
		jr z, _LABEL_418F_
		ld a, c
		ld hl, (_RAM_C232_)
		bit 3, a
		jr nz, +
		ex de, hl
		ld hl, $0000
		or a
		sbc hl, de
		ld de, $40FD
		call _LABEL_40B2_
		ex de, hl
		ld hl, $0000
		or a
		sbc hl, de
		jr +++
	
+:	
		ld de, _DATA_40FD_
		call _LABEL_40B2_
		ld (_RAM_C232_), hl
		jr +++
	
++:	
		ld hl, _LABEL_418F_	; Overriding return address
		push hl
		ld de, _DATA_40FD_
		bit 2, a
		jp nz, _LABEL_4262_
		bit 3, a
		jp nz, _LABEL_4221_
		jp _LABEL_41E9_
	
+++:	
		ld (_RAM_C232_), hl
_LABEL_418F_:	
		ret
	
_LABEL_4190_:	
		ld hl, _RAM_C16B_
		ld a, (hl)
		and $42
		ret nz
		ld a, (_RAM_C174_)
		and $0C
		jr z, ++
		ld c, a
		bit 2, (hl)
		ld a, $16
		jr nz, +
		res 3, (hl)
		bit 3, c
		ld a, $04
		jr nz, +
		set 3, (hl)
		ld a, $08
+:	
		ld (_RAM_C171_), a
		ret
	
++:	
		bit 2, (hl)
		ld a, $16
		jr nz, +
		bit 3, (hl)
		ld a, $04
		jr z, +
		ld a, $08
+:	
		ld (_RAM_C171_), a
		ret
	
_LABEL_41C7_:	
		ld hl, _RAM_C16B_
		bit 2, (hl)
		ret nz
		set 2, (hl)
		xor a
		ld (_RAM_C170_), a
		ret
	
_LABEL_41D4_:	
		res 3, (hl)
		call _LABEL_46EC_
		ld de, _DATA_40F9_
		ld a, (_RAM_C174_)
		and $3F
		bit 2, a
		jr nz, _LABEL_4262_
		bit 3, a
		jr nz, _LABEL_4221_
_LABEL_41E9_:	
		push de
		ld hl, (_RAM_C232_)
		ld de, $8000
		add hl, de
		ld bc, $80C8
		or a
		sbc hl, bc
		pop de
		ret z
		add hl, bc
		ex de, hl
		ld c, (hl)
		inc hl
		ld b, (hl)
		jr nc, +
		inc hl
		ld c, (hl)
		inc hl
		ld b, (hl)
+:	
		ex de, hl
		add hl, bc
		ld de, $8000
		add hl, de
		ld de, $FF3F
		add hl, de
		ld bc, $000E
		or a
		sbc hl, bc
		add hl, bc
		jr nc, +
		ld hl, $0007
+:	
		or a
		sbc hl, de
		ld (_RAM_C232_), hl
		ret
	
_LABEL_4221_:	
		push de
		ld hl, (_RAM_C232_)
		ld de, $8000
		add hl, de
		ld bc, $8118
		bit 4, a
		jr z, +
		ld bc, $8168
+:	
		or a
		sbc hl, bc
		add hl, bc
		ex (sp), hl
		ld c, (hl)
		inc hl
		ld b, (hl)
		jr nc, +
		inc hl
		ld c, (hl)
		inc hl
		ld b, (hl)
+:	
		pop hl
		add hl, bc
		add hl, de
		ld de, $FEEF
		bit 4, a
		jr z, +
		ld de, $FE9F
+:	
		add hl, de
		ld bc, $000E
		or a
		sbc hl, bc
		add hl, bc
		jr nc, +
		ld hl, $0007
+:	
		or a
		sbc hl, de
		ld (_RAM_C232_), hl
		ret
	
_LABEL_4262_:	
		push de
		ld hl, (_RAM_C232_)
		ld de, $8000
		add hl, de
		ld bc, $8078
		or a
		sbc hl, bc
		add hl, bc
		ex (sp), hl
		ld c, (hl)
		inc hl
		ld b, (hl)
		jr nc, +
		inc hl
		ld c, (hl)
		inc hl
		ld b, (hl)
+:	
		pop hl
		add hl, bc
		add hl, de
		ld de, $FF8F
		add hl, de
		ld bc, $000E
		or a
		sbc hl, bc
		add hl, bc
		jr nc, +
		ld hl, $0007
+:	
		or a
		sbc hl, de
		ld (_RAM_C232_), hl
		ret
	
_LABEL_4295_:	
		ld hl, (_RAM_C230_)
		ld de, $FFE4
		add hl, de
		ld (_RAM_C230_), hl
		ld hl, _RAM_C16C_
		bit 0, (hl)
		jr z, +
		res 0, (hl)
		ld a, $96
		call _LABEL_A99_
		ld hl, $034C
		ld (_RAM_C230_), hl
+:	
		ld hl, _RAM_C16C_
		res 3, (hl)
		ld ix, _RAM_C220_
		ld hl, $0608
		ld a, $FF
		call _LABEL_392F_
		call _LABEL_48BE_
		ld hl, _RAM_C16B_
		bit 6, (hl)
		call nz, _LABEL_449F_
		ld a, (_RAM_C234_)
		bit 7, a
		jr z, +
		ld hl, _RAM_C222_
		set 3, (hl)
+:	
		and $02
		jr z, +++
		ld a, (_RAM_C16D_)
		rra
		jr nc, ++
		call _LABEL_4392_
		jr nc, ++
		ld hl, _RAM_C16D_
		set 1, (hl)
		call _LABEL_4B66_
		ld a, (_RAM_C128_)
		cp $28
		ld h, $00
		jr nz, +
		ld h, $01
+:	
		ld a, $4D
		call _LABEL_6E2_
		ld hl, _RAM_CC30_
		set 7, (hl)
		ld hl, _RAM_CC80_
		set 7, (hl)
++:	
		xor a
		ld (_RAM_C230_), a
		ld (_RAM_C231_), a
		ld hl, _RAM_C16B_
		res 4, (hl)
		jr ++++
	
+++:	
		ld hl, _RAM_C16B_
		set 4, (hl)
		call _LABEL_4190_
		ld hl, (_RAM_C223_)
		ld de, (_RAM_C230_)
		add hl, de
		ld (_RAM_C223_), hl
++++:	
		ld a, (_RAM_C234_)
		and $0C
		jr z, +
		ld hl, $0000
		ld (_RAM_C232_), hl
+:	
		ld hl, (_RAM_C226_)
		ld de, (_RAM_C232_)
		add hl, de
		ld a, h
		cp $48
		jr nc, +
		ld hl, $0000
		ld (_RAM_C232_), hl
		ld hl, $4800
		ld (_RAM_C226_), hl
+:	
		ex af, af'
		ld a, (_RAM_C179_)
		rra
		jr c, ++
		ex af, af'
		cp $78
		jr c, ++
		ld de, $7800
		or a
		sbc hl, de
		push hl
		add hl, hl
		ld a, h
		cp $04
		jr c, +
		ld a, $03
+:	
		call _LABEL_1796_
		ld ix, $C220
		pop hl
		ld h, $78
		ld a, $7F
		and l
		ld l, a
++:	
		ld (_RAM_C226_), hl
		ld a, (_RAM_C179_)
		bit 2, a
		ret z
		ld a, $B8
		cp h
		ret nc
		ld (_RAM_C227_), a
		ld a, $80
		ld (_RAM_C226_), a
		ret
	
_LABEL_4392_:	
		ld a, (_RAM_C16B_)
		bit 2, a
		scf
		ret z
		ld hl, _RAM_C222_
		set 3, (hl)
		or a
		ret
	
_LABEL_43A0_:	
		set 6, (hl)
		ld a, (_RAM_CC34_)
		bit 7, a
		jr nz, +
		ld hl, _RAM_C127_
		bit 2, (hl)
		jr z, +
		res 2, (hl)
		ld a, (_RAM_C16B_)
		bit 4, a
		jp nz, _LABEL_44B3_
		ld hl, _RAM_C222_
		ld (hl), $88
		ld hl, _RAM_C16B_
		res 2, (hl)
+:	
		ld hl, _RAM_C222_
		set 6, (hl)
		xor a
		ld (_RAM_C170_), a
		ld a, (hl)
		and $30
		jr nz, ++
		ld a, $93
		call _LABEL_A99_
		ld a, $02
		call _LABEL_64A1_
		ld a, $0D
		ld hl, _RAM_C16B_
		bit 3, (hl)
		jr z, +
		ld a, $0E
+:	
		ld (_RAM_C171_), a
		ld hl, $00A0
		ld (_RAM_C230_), hl
		ld hl, $0080
		ld (_RAM_C232_), hl
		jp _LABEL_3D72_
	
++:	
		call _LABEL_AB1_
		bit 5, (hl)
		ld a, $0F
		jr nz, +
		ld a, $11
+:	
		ld (_RAM_C171_), a
		ld hl, $00C0
		ld (_RAM_C230_), hl
		ld l, h
		ld (_RAM_C232_), hl
		jp _LABEL_4491_
	
_LABEL_4414_:	
		and $30
		jr nz, +
		jp _LABEL_3D72_
	
+:	
		ld hl, _RAM_C222_
		ld a, (_RAM_C170_)
		cp $10
		jp c, _LABEL_4491_
		jr z, ++
		cp $20
		jr z, +++
		jr nc, ++++
		and $07
		jp nz, _LABEL_3D97_
		ld a, $0F
		bit 5, (hl)
		jr nz, +
		ld a, $11
+:	
		ld (_RAM_C171_), a
		jp _LABEL_3D97_
	
++:	
		ld a, $10
		bit 5, (hl)
		jr nz, +
		ld a, $12
+:	
		ld (_RAM_C171_), a
		jp _LABEL_3D97_
	
+++:	
		ld a, $10
		bit 5, (hl)
		jr nz, +
		ld a, $12
+:	
		ld (_RAM_C171_), a
		ld hl, $FFC0
		ld (_RAM_C230_), hl
		jp _LABEL_3D97_
	
++++:	
		ex af, af'
		ld a, (_RAM_C224_)
		cp $08
		jr c, +
		exx
		ld hl, (_RAM_C230_)
		ld de, $FFE4
		add hl, de
		ld (_RAM_C230_), hl
		exx
		bit 4, (hl)
		jr nz, _LABEL_4491_
		ex af, af'
		ld c, a
		and $03
		jr nz, _LABEL_4491_
		ld a, c
		rra
		rra
		and $01
		ld c, $0F
		add a, c
		ld (_RAM_C171_), a
		jr _LABEL_4491_
	
+:	
		jp _LABEL_44D5_
	
_LABEL_4491_:	
		ld hl, (_RAM_C223_)
		ld de, (_RAM_C230_)
		add hl, de
		ld (_RAM_C223_), hl
		jp _LABEL_3D97_
	
_LABEL_449F_:	
		ld a, (_RAM_C234_)
		or a
		ret z
		ld a, $BD
		and (hl)
		ld (hl), a
		ld hl, _RAM_C222_
		ld (hl), $80
		ld a, $80
		ld (_RAM_C222_), a
		ret
	
_LABEL_44B3_:	
		ld hl, _RAM_C222_
		ld (hl), $C8
		ld hl, _RAM_C16B_
		res 2, (hl)
		set 6, (hl)
		ld a, $93
		call _LABEL_A99_
		ld a, $0D
		ld (_RAM_C171_), a
		jp _LABEL_3D72_
	
	; Data from 44CC to 44CC (1 bytes)
	.db $C9
	
_LABEL_44CD_:	
		ld a, (_RAM_C128_)
		cp $28
		jp z, _LABEL_4ADB_
_LABEL_44D5_:	
		ld a, (_RAM_C178_)
		rla
		jp c, _LABEL_4583_
		ld b, $0C
		ld ix, _RAM_C220_
		call _LABEL_57F_
		xor a
		ld (_RAM_C170_), a
		ld hl, _RAM_C16B_
		set 5, (hl)
		inc l
		bit 1, (hl)
		jr nz, +
		call _LABEL_AB1_
		ld a, $9A
		jp _LABEL_A99_
	
+:	
		ld a, $80
		ld b, $04
		ld de, _RAM_C220_
		ld hl, $0999
		call _LABEL_971_
		ld a, $08
		ld (_RAM_C224_), a
		ld hl, $02F0
		ld (_RAM_C230_), hl
		ld a, $04
		call _LABEL_B13_
		ld hl, _DATA_1263D_
		ld de, $2000
		ld bc, $0080
		di
		call _LABEL_46C_
		ei
		ld hl, _RAM_C224_
		inc (hl)
		ret
	
_LABEL_452B_:	
		inc l
		bit 1, (hl)
		jr z, +
		ld hl, (_RAM_C230_)
		ld de, $FFE4
		add hl, de
		ld (_RAM_C230_), hl
		ld de, (_RAM_C223_)
		add hl, de
		ld (_RAM_C223_), hl
		ld a, h
		cp $08
		ld b, $04
		ld ix, _RAM_C220_
		jr nc, _LABEL_4590_
		call _LABEL_57F_
		ld hl, _RAM_C16C_
		res 1, (hl)
		xor a
		ld (_RAM_C170_), a
		call _LABEL_AB1_
		ld a, $9A
		call _LABEL_A99_
+:	
		ld a, (_RAM_CC34_)
		rla
		jr nc, +
		ld hl, _RAM_C16C_
		bit 4, (hl)
		call z, ++
+:	
		ld a, (_RAM_C170_)
		cp $C0
		ret c
		ld a, (_RAM_C128_)
		cp $28
		jp z, _LABEL_4ADB_
		ld hl, _RAM_C120_
		set 6, (hl)
		ret
	
_LABEL_4583_:	
		ld hl, _RAM_C16B_
		res 5, (hl)
		res 6, (hl)
		ld hl, _RAM_C222_
		ld (hl), $80
		ret
	
_LABEL_4590_:	
		rst $28	; _LABEL_28_
		ret
	
++:	
		set 4, (hl)
		ld a, $80
		ld b, $0A
		ld de, _RAM_C220_
		ld hl, _DATA_A85_
		call _LABEL_971_
		ld a, $44
		ld (_RAM_C224_), a
		ld a, $80
		ld (_RAM_C227_), a
		ld a, $90
		rst $30	; _LABEL_30_
	; Data from 45AE to 45B6 (9 bytes)
	.db $F7 $06 $0A $DD $21 $20 $C2 $EF $C9
	
_LABEL_45B7_:	
		inc l
		bit 7, (hl)
		call z, _LABEL_4611_
		ld a, (_RAM_C178_)
		bit 1, a
		ret nz
		ld hl, _RAM_CC30_
		bit 6, (hl)
		jr z, ++
		ld hl, (_RAM_C226_)
		ld a, (_RAM_C17A_)
		ld e, $00
		ld d, a
		or a
		sbc hl, de
		jr c, ++
		ld a, l
		exx
		and $7F
		ld l, a
		ld a, (_RAM_C17A_)
		ld h, a
		ld (_RAM_C226_), hl
		exx
		add hl, hl
		ld a, h
		cp $04
		jr c, +
		ld a, $03
+:	
		call _LABEL_1796_
		ld ix, $C220
		ld hl, _RAM_C16D_
		bit 7, (hl)
		jr z, +++
++:	
		ld a, (_RAM_C227_)
		cp $8D
		ret c
		ld hl, _RAM_C16D_
		bit 7, (hl)
		jr z, +++
		cp $C8
		ret c
		ld hl, _RAM_C120_
		set 5, (hl)
		ret
	
_LABEL_4611_:	
		set 7, (hl)
		ld hl, _RAM_C179_
		set 0, (hl)
		ld hl, _RAM_CC30_
		set 7, (hl)
		call _LABEL_4637_
		jp c, ++++
		ld a, (_RAM_C227_)
		cp $78
		jr nc, +
		ld a, $78
+:	
		ld (_RAM_C17A_), a
		ret
	
+++:	
		set 7, (hl)
		ld a, $89
		jp _LABEL_A99_
	
_LABEL_4637_:	
		ld a, (_RAM_C128_)
		cp $27
		jr z, ++
		cp $23
		jr nz, +
		ld c, a
		ld a, (_RAM_C130_)
		inc c
		cp c
		jr c, ++
+:	
		or a
		ret
	
++:	
		scf
		ret
	
++++:	
		ld hl, _RAM_C178_
		set 1, (hl)
		ld a, $8A
		call _LABEL_A99_
_LABEL_4658_:	
		ld hl, _RAM_C178_
		bit 2, (hl)
		jr nz, _LABEL_46A5_
		ld a, (_RAM_C227_)
		cp $74
		ld a, $04
		jr z, ++
		jr nc, +
		ld a, $08
+:	
		ld (_RAM_C174_), a
		ret
	
++:	
		set 2, (hl)
		ld hl, _RAM_C16B_
		res 3, (hl)
		ld a, $5A
		call _LABEL_6E2_
		xor a
		ld (_RAM_C232_), a
		ld (_RAM_C233_), a
		ld hl, $0000
		ld bc, $0020
		xor a
		di
		call _LABEL_202_
		call _LABEL_46DA_
		ei
		ld bc, $052B
-:	
		call _LABEL_B67_
		inc c
		djnz -
		ld a, (_RAM_C114_)
		bit 0, a
		ret z
		call _LABEL_B67_
		ret
	
_LABEL_46A5_:	
		xor a
		ld (_RAM_C174_), a
		ld hl, _RAM_C16E_
		inc (hl)
		ld e, (hl)
		inc hl
		jr nz, +
		inc (hl)
+:	
		ld d, (hl)
		ex de, hl
		ld b, h
		ld c, l
		ld de, $04C0
		or a
		sbc hl, de
		jr nc, +
		ld h, b
		ld l, c
		ld de, $0080
		or a
		sbc hl, de
		ret c
		ld hl, _RAM_C16D_
		bit 5, (hl)
		ret nz
		set 5, (hl)
		ld a, $89
		jp _LABEL_A99_
	
+:	
		ld hl, _RAM_C120_
		set 4, (hl)
		ret
	
_LABEL_46DA_:	
		ld a, $03
		call _LABEL_B13_
		ld hl, _DATA_DB6A_
		ld de, $12A0
		ld bc, $05E0
		call _LABEL_422_
		ret
	
_LABEL_46EC_:	
		ld hl, _RAM_C16B_
		bit 1, (hl)
		ret nz
		ld a, (_RAM_C174_)
		and $3F
		ld c, a
		bit 2, (hl)
		jr nz, _LABEL_474A_
		and $0C
		ld a, (_RAM_C170_)
		ld b, a
		jr z, ++
		bit 4, c
		jr nz, +++
		and $07
		ret nz
		ld a, b
		rra
		rra
		rra
		and $03
		ld c, $01
		bit 3, (hl)
		jr z, +
		ld c, $05
+:	
		add a, c
		ld (_RAM_C171_), a
		ret
	
++:	
		and $07
		ret nz
		ld a, b
		rra
		rra
		rra
		and $01
		ld c, $01
		bit 3, (hl)
		jr z, +
		ld c, $05
+:	
		add a, a
		add a, c
		ld (_RAM_C171_), a
		ret
	
+++:	
		and $03
		ret nz
		ld a, b
		rra
		rra
		and $03
		ld c, $01
		bit 3, (hl)
		jr z, +
		ld c, $05
+:	
		add a, c
		ld (_RAM_C171_), a
		ret
	
_LABEL_474A_:	
		and $08
		ld a, (_RAM_C170_)
		ld b, a
		jr nz, +
		bit 2, c
		jr nz, ++
		and $07
		ret nz
		ld a, b
		rra
		rra
		rra
		and $01
		ld c, $14
		add a, c
		ld (_RAM_C171_), a
		ret
	
+:	
		and $03
		ret nz
		ld a, b
		rra
		rra
		and $01
		ld c, $14
		add a, c
		ld (_RAM_C171_), a
		ret
	
++:	
		and $03
		ret nz
		ld a, $17
		ld (_RAM_C171_), a
		ret
	
_LABEL_477E_:	
		ld hl, _RAM_C221_
		ld de, $0020
		ld a, (_RAM_C16B_)
		bit 3, a
		jr nz, +
		ld bc, $0407
		ld a, $FF
-:	
		ld (hl), $F7
		add hl, de
		ld (hl), a
		add hl, de
		ld (hl), c
		add hl, de
		djnz -
		ret
	
+:	
		ld bc, $0409
		ld a, $01
-:	
		ld (hl), $F9
		add hl, de
		ld (hl), a
		add hl, de
		ld (hl), c
		add hl, de
		djnz -
		ret
	
_LABEL_47A9_:	
		ld a, (_RAM_C16D_)
		bit 1, a
		ret nz
		ld hl, _RAM_C181_
		ld a, (hl)
		or a
		jr z, +
		inc l
		inc l
		cp (hl)
		jr nz, +
		inc (hl)
+:	
		ld hl, _RAM_C3A2_
		bit 7, (hl)
		call nz, +
		ld hl, _RAM_C422_
		bit 7, (hl)
		jr nz, +
		ret
	
+:	
		bit 0, (hl)
		dec hl
		dec hl
		ld (_RAM_C17C_), hl
		ld ix, (_RAM_C17C_)
		jp z, _LABEL_3FB9_
		ld a, (ix+4)
		cp $0D
		jp c, _LABEL_4880_
		ld a, (ix+7)
		cp $E1
		jp nc, _LABEL_4880_
		cp $31
		jp c, _LABEL_4880_
		ld a, (ix+2)
		bit 6, a
		jr nz, _LABEL_47FB_
		and $28
		jp nz, _LABEL_4893_
_LABEL_47FB_:	
		ld l, (ix+16)
		ld h, (ix+17)
		ld de, $FFF8
		add hl, de
		ld (ix+16), l
		ld (ix+17), h
		ld a, (_RAM_C17C_)
		cp $A0
		ld hl, _RAM_C181_
		jr z, +
		ld hl, _RAM_C183_
+:	
		ld a, (hl)
		and $01
		jr z, +
		ld hl, $0202
		xor a
		call _LABEL_392F_
		ld a, (ix+20)
		or a
		jr nz, _LABEL_4880_
+:	
		ld l, (ix+3)
		ld h, (ix+4)
		ld e, (ix+16)
		ld d, (ix+17)
		add hl, de
		ld (ix+3), l
		ld (ix+4), h
		ld l, (ix+6)
		ld h, (ix+7)
		ld e, (ix+18)
		ld d, (ix+19)
		add hl, de
		ld (ix+6), l
		ld (ix+7), h
		call _LABEL_233B_
		ld a, (_RAM_C17C_)
		cp $A0
		ld hl, _RAM_C181_
		jr z, +
		ld hl, _RAM_C183_
+:	
		inc (hl)
		ld a, (hl)
		and $07
		jr nz, ++
		inc l
		inc (hl)
		ld a, (hl)
		cp $03
		jr c, +
		xor a
		ld (hl), a
+:	
		add a, a
		add a, a
		ld c, $0C
		add a, c
		ld hl, (_RAM_C17C_)
		ld b, $04
		call _LABEL_22F4_
++:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_4880_:	
		ld b, $04
		call _LABEL_57F_
		ld hl, (_RAM_C17C_)
		ld e, l
		ld d, h
		inc de
		ld bc, $007F
		ld (hl), $00
		ldir
		ret
	
_LABEL_4893_:	
		ld a, (ix+2)
		ld (ix+2), $C1
		bit 3, a
		jp z, _LABEL_4880_
		ld hl, $FF80
		bit 1, a
		jr nz, +
		ld hl, $0080
+:	
		ld (ix+16), l
		ld (ix+17), h
		xor a
		ld (ix+18), a
		ld (ix+19), a
		ld a, $92
		call _LABEL_A99_
		jp _LABEL_47FB_
	
_LABEL_48BE_:	
		ld hl, _RAM_C5B5_
		ld de, $0020
		ld bc, $2403
		ld a, c
-:	
		and (hl)
		call nz, +
		add hl, de
		ld a, c
		djnz -
		ret
	
+:	
		dec l
		ld (hl), $00
		inc l
		ld a, (_RAM_C231_)
		cp $80
		ret c
		push bc
		push hl
		bit 1, (hl)
		exx
		ld bc, $0014
		jr z, +
		ld bc, $010A
+:	
		exx
		ex de, hl
		ld hl, (_RAM_C226_)
		ld bc, (_RAM_C232_)
		add hl, bc
		ex de, hl
		ld bc, $FFF2
		add hl, bc
		ld a, d
		sub (hl)
		jr nc, +
		neg
+:	
		exx
		cp c
		jr nc, _LABEL_4952_
		ld a, b
		exx
		dec l
		dec l
		dec l
		ex de, hl
		ld hl, (_RAM_C223_)
		ld bc, (_RAM_C230_)
		or a
		jr nz, ++
		add hl, bc
		ld a, h
		sub $0B
		jr c, _LABEL_4952_
		ex de, hl
		sub (hl)
		jr nc, +
		neg
+:	
		cp $03
		jr nc, _LABEL_4952_
		ld bc, $0C00
		jr +++
	
++:	
		ld a, h
		sub $0E
		jr c, _LABEL_4952_
		ex de, hl
		cp (hl)
		jr c, _LABEL_4952_
		ex de, hl
		add hl, bc
		ld a, h
		sub $0F
		ex de, hl
		cp (hl)
		jr nc, _LABEL_4952_
		ld bc, $0E00
+++:	
		ld d, (hl)
		dec l
		ld l, (hl)
		ld h, d
		add hl, bc
		ld (_RAM_C223_), hl
		ld hl, _RAM_C234_
		set 1, (hl)
		pop hl
		pop bc
		dec l
		set 0, (hl)
		inc l
		ld de, $0020
		ret
	
_LABEL_4952_:	
		pop hl
		pop bc
		ld de, $0020
		ret
	
_LABEL_4958_:	
		ld hl, (_RAM_C196_)
		inc l
		inc l
		inc l
		ld e, (hl)
		inc l
		ld d, (hl)
		ex de, hl
		add hl, bc
		ld (_RAM_C223_), hl
		ret
	
; 156th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_4967_:	
		ld a, (_RAM_C197_)
		rra
		ld a, (_RAM_C224_)
		jr nc, +
		add a, $20
+:	
		ld (_RAM_C4A4_), a
		ld a, $38
		ld (_RAM_C4A7_), a
		ld de, _RAM_C4A0_
		ld a, $94
		ld b, $08
		ld hl, _DATA_A75_
		call _LABEL_971_
		ld hl, $C4A0
		ld a, $24
		ld b, $08
		call _LABEL_22F4_
		ld a, $04
		call _LABEL_B13_
		ld hl, _DATA_11FBD_
		ld de, $2380
		ld bc, $0200
		di
		call _LABEL_46C_
		ei
		ret
	
; 155th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_49A5_:	
		ld ix, _RAM_C4A0_
		ld hl, _RAM_C19A_
		inc (hl)
		dec l
		bit 0, (hl)
		jr nz, _LABEL_49FA_
		bit 1, (hl)
		jr nz, +
		ld a, (_RAM_C197_)
		or a
		jp nz, _LABEL_4A53_
		inc l
		ld a, (hl)
		and $07
		call z, _LABEL_4A44_
		ld hl, (_RAM_C4A6_)
		ld bc, $00A0
		add hl, bc
		ld (_RAM_C4A6_), hl
		ld a, (_RAM_C227_)
		add a, $0E
		cp h
		jr nc, _LABEL_4A33_
_LABEL_49D6_:	
		ld hl, _RAM_C19A_
		ld (hl), $00
		dec l
		set 1, (hl)
		exx
		ld hl, _RAM_C16D_
		set 2, (hl)
		exx
+:	
		inc l
		ld a, (hl)
		cp $20
		ret c
		dec l
		set 0, (hl)
		ld (ix+16), $C8
		ld (ix+17), $00
		ld a, $A1
		call _LABEL_A99_
_LABEL_49FA_:	
		ld hl, (_RAM_C4B0_)
		ld a, (_RAM_C19A_)
		and $03
		ld de, $0000
		jr nz, +
		ld de, $FFFF
+:	
		add hl, de
		ld (_RAM_C4B0_), hl
		ex de, hl
		ld hl, (_RAM_C4A3_)
		add hl, de
		ld (_RAM_C4A3_), hl
		ld hl, (_RAM_C223_)
		add hl, de
		ld (_RAM_C223_), hl
		ld de, $00A0
		ld hl, (_RAM_C4A6_)
		add hl, de
		ld (_RAM_C4A6_), hl
		ld hl, (_RAM_C226_)
		add hl, de
		ld (_RAM_C226_), hl
		ld a, $D0
		cp h
		jr c, +
_LABEL_4A33_:	
		ld b, $08
		rst $28	; _LABEL_28_
		ld b, $0C
		ld ix, $C220
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld hl, _RAM_C16D_
		set 4, (hl)
		ret
	
_LABEL_4A44_:	
		ld hl, $C4A0
		ld a, (_RAM_C19A_)
		ld b, $08
		and b
		add a, $1C
		call _LABEL_22F4_
		ret
	
_LABEL_4A53_:	
		ld hl, (_RAM_C226_)
		ld de, $0E00
		add hl, de
		ld de, (_RAM_C4A6_)
		or a
		sbc hl, de
		res 7, h
		xor a
		ld b, a
-:	
		sra h
		rr l
		inc b
		cp h
		jr nz, -
		inc hl
		push hl
		ld hl, (_RAM_C223_)
		ld de, (_RAM_C4A3_)
		or a
		sbc hl, de
-:	
		sra h
		rr l
		djnz -
		ld de, (_RAM_C4A3_)
		add hl, de
		ld (_RAM_C4A3_), hl
		pop de
		ld hl, (_RAM_C4A6_)
		add hl, de
		ld (_RAM_C4A6_), hl
		ld a, (_RAM_C227_)
		add a, $0C
		cp h
		jp nc, _LABEL_4A33_
		ld hl, (_RAM_C223_)
		ld (_RAM_C4A3_), hl
		ld hl, $C4A0
		ld a, $24
		ld b, $08
		call _LABEL_22F4_
		jp _LABEL_49D6_
	
_LABEL_4AAB_:	
		ld hl, _RAM_C16D_
		bit 4, (hl)
		jr nz, +
		bit 3, (hl)
		ret nz
		bit 2, (hl)
		ret z
		set 3, (hl)
		ld a, $03
		jp _LABEL_3DB0_
	
+:	
		ld a, (_RAM_C128_)
		ld c, $28
		cp c
		jr z, +
		ld (_RAM_C132_), a
		ld a, c
		ld (_RAM_D39B_), a
		ld a, (_RAM_C129_)
		ld (_RAM_C133_), a
		ld a, $03
		ld (_RAM_D39A_), a
		jr _LABEL_4B22_
	
_LABEL_4ADB_:	
		ld a, (_RAM_C132_)
		ld (_RAM_D39B_), a
		ld a, (_RAM_C133_)
		ld (_RAM_D39A_), a
		jr _LABEL_4B22_
	
+:	
		ld a, (_RAM_C132_)
		ld (_RAM_D39B_), a
		srl a
		srl a
		ld b, a
		add a, a
		add a, b
		ld c, a
		ld a, (_RAM_CCA8_)
		dec a
		add a, c
		ld e, a
		ld d, $00
		ld hl, _DATA_4B3E_
		add hl, de
		ld a, (hl)
		ld (_RAM_D39A_), a
		ld c, a
		ld a, (_RAM_C12F_)
		rla
		jr c, _LABEL_4B22_
		ld e, b
		ld hl, _DATA_4B5C_
		add hl, de
		ld a, (hl)
		cp c
		jr nc, _LABEL_4B22_
		ld hl, _RAM_C12F_
		set 0, (hl)
		set 7, (hl)
		ld hl, _RAM_C130_
		inc (hl)
_LABEL_4B22_:	
		ld hl, _RAM_C120_
		ld a, (_RAM_CC34_)
		rla
		set 3, (hl)
		jr c, +
		ld hl, _RAM_C131_
		set 0, (hl)
		set 1, (hl)
		ld a, (_RAM_CC36_)
		ld (_RAM_C134_), a
		ret
	
+:	
		set 6, (hl)
		ret
	
; Data from 4B3E to 4B5B (30 bytes)	
_DATA_4B3E_:	
	.db $02 $03 $02 $02 $03 $02 $02 $03 $01 $02 $03 $03 $03 $02 $03 $01
	.db $03 $02 $03 $02 $01 $02 $03 $02 $01 $03 $02 $02 $01 $03
	
; Data from 4B5C to 4B65 (10 bytes)	
_DATA_4B5C_:	
	.db $03 $03 $01 $00 $02 $01 $00 $01 $02 $03
	
_LABEL_4B66_:	
		ld hl, _RAM_C1A1_
		ld de, $000C
		ld bc, $0A28
-:	
		ld a, (hl)
		and $7F
		cp c
		jr nz, +
		add hl, de
		jr ++
	
+:	
		push bc
		ld bc, $000B
		ld e, l
		ld d, h
		inc de
		ld (hl), $00
		ldir
		pop bc
		ld de, $000A
		inc hl
++:	
		djnz -
		ld ix, _RAM_C4A0_
		ld (ix+13), $14
		ld b, $2C
		call _LABEL_57F_
		ret
	
; 82nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_4B98_:	
		ld a, $09
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld l, d
		ld h, $00
		add hl, hl
		ld a, $02
		call _LABEL_B13_
		ld bc, _DATA_BB3E_
		add hl, bc
		ld ix, (_RAM_C196_)
		ld (ix+2), $C0
		ld a, (hl)
		ld (ix+3), $80
		ld (ix+4), a
		ld (ix+7), $D0
		ld (ix+21), $01
		inc hl
		ld a, (hl)
		cp $35
		jr nz, +
		ld (ix+3), $80
		ld (ix+4), $34
		ld (ix+6), $00
		ld (ix+7), e
+:	
		cp $33
		jr z, +
		cp $31
		jr z, +
		cp $2C
		jr nz, ++
+:	
		ld (ix+7), $CC
++:	
		cp $07
		jr z, +
		cp $0C
		jr z, +
		cp $32
		jr z, +
		cp $11
		jr z, +
		cp $1A
		jr z, +
		cp $2A
		jr z, +
		cp $2B
		jr nz, ++
+:	
		ld (ix+7), $D4
++:	
		ld e, a
		add a, a
		ld l, a
		ld d, $00
		ld h, d
		add hl, hl
		add hl, de
		add hl, hl
		ld a, $05
		call _LABEL_B13_
		ld de, _DATA_17AD6_
		add hl, de
		ex de, hl
		ld hl, (_RAM_C196_)
		ld bc, $0031
		add hl, bc
		ex de, hl
		ld c, $09
		ldir
		ld a, (hl)
		rst $30	; _LABEL_30_
	; Data from 4C2B to 4C5A (48 bytes)
	.db $F7 $3E $54 $06 $10 $CD $F1 $22 $DD $5E $33 $DD $56 $34 $DD $73
	.db $10 $DD $72 $11 $DD $5E $35 $DD $56 $36 $DD $73 $12 $DD $72 $13
	.db $0E $28 $AF $BB $20 $05 $BA $20 $02 $0E $38 $79 $32 $98 $C1 $C9
	
; 81st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_4C5B_:	
		call _LABEL_233B_
		ex af, af'
		ld a, (_RAM_C198_)
		ld c, a
		ex af, af'
		cp c
		jp c, _LABEL_4D9D_
		ld a, (ix+49)
		and $C0
		jr z, +
		ld a, $0C
		cp (ix+4)
		jp nc, _LABEL_4D9D_
		ld a, $74
		cp (ix+4)
		jp c, _LABEL_4D9D_
+:	
		bit 7, (ix+57)
		jr z, ++
		ld c, $73
		ld a, $0C
		cp (ix+4)
		jr nc, +
		ld a, $74
		ld c, $0D
		cp (ix+4)
		jr nc, ++
+:	
		ld (ix+4), c
		res 0, (ix+20)
++:	
		ld a, (ix+49)
		and $C0
		jr z, +
		ld a, $0C
		cp (ix+4)
		jp nc, _LABEL_4DA7_
+:	
		bit 0, (ix+49)
		jr nz, +
		ld b, (ix+50)
		call _LABEL_22D9_
		jp nc, _LABEL_4D99_
		bit 0, (ix+20)
		jp z, _LABEL_4D99_
		set 0, (ix+49)
+:	
		inc (ix+58)
		bit 1, (ix+49)
		jr z, _LABEL_4D13_
		ld a, $08
		cp (ix+58)
		jp nc, _LABEL_4D99_
		ld (ix+58), $00
		res 1, (ix+49)
		ld a, $04
		xor (ix+49)
		ld (ix+49), a
		ld e, (ix+51)
		ld d, (ix+52)
		ld c, (ix+53)
		ld b, (ix+54)
		bit 2, a
		jr z, +
		ld hl, $0000
		or a
		sbc hl, de
		ex de, hl
		ld hl, $0000
		or a
		sbc hl, bc
		ld c, l
		ld b, h
+:	
		ld (ix+16), e
		ld (ix+17), d
		ld (ix+18), c
		ld (ix+19), b
_LABEL_4D13_:	
		ld a, (ix+58)
		bit 7, (ix+57)
		jr nz, +
		cp (ix+57)
		jr c, +
		ld (ix+58), $00
		set 1, (ix+49)
		ld hl, (_RAM_C196_)
		ld de, $0010
		add hl, de
		xor a
		ld (hl), a
		inc l
		ld (hl), a
		inc l
		ld (hl), a
		inc l
		ld (hl), a
		jr _LABEL_4D99_
	
+:	
		ld l, (ix+55)
		ld h, (ix+56)
		ld e, (ix+16)
		ld d, (ix+17)
		add hl, de
		ld (ix+16), l
		ld (ix+17), h
		ld e, (ix+3)
		ld d, (ix+4)
		add hl, de
		ld (ix+3), l
		ld (ix+4), h
		ld l, (ix+6)
		ld h, (ix+7)
		ld e, (ix+18)
		ld d, (ix+19)
		add hl, de
		ld (ix+6), l
		ld (ix+7), h
		ld a, (_RAM_C16D_)
		bit 1, a
		jr nz, _LABEL_4D99_
		bit 0, (ix+20)
		jr z, _LABEL_4D99_
		ld a, (_RAM_C16B_)
		bit 6, a
		jr nz, _LABEL_4D99_
		ld c, (ix+18)
		ld b, (ix+19)
		ld hl, (_RAM_C226_)
		add hl, bc
		ld a, $47
		cp h
		jr nc, +
		ld (_RAM_C226_), hl
+:	
		ld bc, $0C00
		call _LABEL_4958_
_LABEL_4D99_:	
		ld b, $10
		rst $28	; _LABEL_28_
		ret
	
_LABEL_4D9D_:	
		ld b, $10
		call _LABEL_57B_
		ld b, $10
		jp _LABEL_24A9_
	
_LABEL_4DA7_:	
		jr _LABEL_4D9D_
	
; 96th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_4DA9_:	
		ld a, $06
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld ix, (_RAM_C196_)
		ld (ix+2), $C0
		ld (ix+4), d
		ld (ix+7), $CC
		ld (ix+21), $02
		ld a, $32
		rst $30	; _LABEL_30_
	; Data from 4DC7 to 4DCB (5 bytes)
	.db $F7 $CD $10 $4E $C9
	
; 95th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_4DCC_:	
		call _LABEL_233B_
		cp $3C
		jp c, _LABEL_4E63_
		ld hl, (_RAM_C196_)
		ld de, $0014
		add hl, de
		ld a, (_RAM_C199_)
		ex af, af'
		ld a, (hl)
		ld (_RAM_C199_), a
		or a
		jr z, _LABEL_4E49_
		ex af, af'
		and (hl)
		jr nz, _LABEL_4E49_
		ld a, (_RAM_C198_)
		xor $01
		ld (_RAM_C198_), a
		ld hl, (_RAM_C196_)
		ld de, $0016
		add hl, de
		ld (hl), a
		jr nz, +
		ld hl, (_RAM_C196_)
		ld de, $0004
		add hl, de
		ld a, (hl)
		sub $04
		ld (hl), a
		add a, $0C
		ld (_RAM_C224_), a
		xor a
		ld (_RAM_C223_), a
		ld hl, (_RAM_C196_)
		ld de, $000C
		add hl, de
		ld e, $20
		ld a, $64
		ld b, $06
-:	
		ld (hl), a
		add hl, de
		inc a
		djnz -
		ld a, $FF
		ld (hl), a
		add hl, de
		ld (hl), a
		add hl, de
		ld (hl), a
		jr _LABEL_4E49_
	
+:	
		ld hl, _RAM_C16C_
		set 0, (hl)
		ld hl, (_RAM_C196_)
		ld de, $0004
		add hl, de
		ld a, (hl)
		add a, $04
		ld (hl), a
		ld e, $08
		add hl, de
		ld e, $20
		ld a, $6A
		ld b, $09
-:	
		ld (hl), a
		add hl, de
		inc a
		djnz -
_LABEL_4E49_:	
		ld a, (_RAM_C16B_)
		bit 6, a
		jr nz, +
		ld hl, (_RAM_C196_)
		ld de, $0014
		add hl, de
		bit 0, (hl)
		ld bc, $0E00
		call nz, _LABEL_4958_
+:	
		ld b, $09
		rst $28	; _LABEL_28_
		ret
	
_LABEL_4E63_:	
		ld b, $09
		call _LABEL_57B_
		ld b, $09
		jp _LABEL_24A9_
	
; 222nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_4E6D_:	
		call _LABEL_6DA_
		call _LABEL_4637_
		ret nc
		ld a, $57
		jp _LABEL_6E2_
	
; 176th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_4E79_:	
		ld a, $07
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, (_RAM_C196_)
		ld de, $0004
		add hl, de
		ld a, $30
		ld (hl), a
		inc l
		inc l
		inc l
		ld (hl), $D0
		ld a, $1C
		ld b, $0C
		call _LABEL_22F1_
		ld a, $6D
		ld b, $06
		rst $30	; _LABEL_30_
	; Data from 4E9C to 4E9E (3 bytes)
	.db $10 $FD $C9
	
; 175th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_4E9F_:	
		call _LABEL_233B_
		ld hl, _RAM_C198_
		bit 0, (hl)
		jr nz, +
		cp $B8
		jr nc, ++
		set 0, (hl)
+:	
		push af
		ld hl, (_RAM_C196_)
		ld de, $0008
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		ld hl, (_RAM_C196_)
		ld de, $0100
		rst $08	; _LABEL_8_
		pop af
		cp $C8
		jr nc, +++
		ld a, (ix+4)
		cp $78
		jr nc, +++
++:	
		ld hl, _RAM_C199_
		inc (hl)
		ld a, (hl)
		and $07
		call z, ++++
		ld b, $0C
		rst $28	; _LABEL_28_
		ret
	
+++:	
		ld b, $0C
		call _LABEL_57B_
		ld b, $0C
		jp _LABEL_24A9_
	
++++:	
		bit 3, (hl)
		ld a, $1C
		jr nz, +
		ld a, $28
+:	
		ld b, $0C
		call _LABEL_22F1_
		ret
	
; 182nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_4EF6_:	
		ld a, $07
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld de, $70A4
		ld bc, _DATA_287F_
		call _LABEL_225F_
		ld a, $28
		ld b, $0C
		call _LABEL_22F1_
		ld a, $70
		ld b, $09
		rst $30	; _LABEL_30_
	; Data from 4F13 to 4F15 (3 bytes)
	.db $10 $FD $C9
	
; 181st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_4F16_:	
		ld a, (_RAM_C198_)
		bit 0, a
		jr nz, +
		ld hl, (_RAM_C196_)
		ld de, $FFF2
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		ld hl, (_RAM_C196_)
		ld de, $FF80
		rst $08	; _LABEL_8_
		ld hl, (_RAM_C196_)
		ld de, $0004
		add hl, de
		ld a, $30
		cp (hl)
		jr c, ++
		ld (hl), a
		dec l
		ld (hl), $00
		ld hl, _RAM_C198_
		set 0, (hl)
		ld a, $5B
		call _LABEL_6E2_
+:	
		ld hl, _RAM_C199_
		inc (hl)
		ld a, (hl)
		and $0F
		jr nz, ++
		bit 4, (hl)
		ld a, $34
		jr nz, +
		ld a, $40
+:	
		ld b, $0C
		call _LABEL_22F1_
++:	
		ld b, $0C
		rst $28	; _LABEL_28_
		ret
	
; 184th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_4F65_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld ix, (_RAM_C196_)
		ld a, $03
		call _LABEL_B13_
		ld hl, _DATA_E4BB_
		ld de, $2300
		ld bc, $0080
		di
		call _LABEL_46C_
		ei
		ld hl, (_RAM_C196_)
		ld de, $0004
		add hl, de
		ld (hl), $3C
		inc l
		inc l
		inc l
		ld (hl), $7E
		ld e, $05
		add hl, de
		ld e, $20
		ld a, $18
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		jp _LABEL_28_
	
; 183rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_4FA3_:	
		ret
	
_LABEL_4FA4_:	
		bit 0, (hl)
		call z, _LABEL_5061_
		call +
		call _LABEL_4FDC_
		call _LABEL_4FF6_
		call _LABEL_5206_
		jp c, _LABEL_52B2_
		ld a, (_RAM_C170_)
		cp $90
		ret c
		ld hl, _RAM_C175_
		set 7, (hl)
		call _LABEL_62D_
		call _LABEL_AF3_
		ret
	
+:	
		ld a, (_RAM_C170_)
		ld c, a
		and $07
		ret nz
		ld a, c
		rra
		rra
		rra
		and $03
		add a, $01
		jp _LABEL_3DB0_
	
_LABEL_4FDC_:	
		ld a, (_RAM_C170_)
		add a, $03
		ld c, a
		and $0F
		ret nz
		ld a, c
		rra
		rra
		rra
		and $02
		call _LABEL_5041_
		ld b, $0C
		ld ix, $C3A0
		rst $28	; _LABEL_28_
		ret
	
_LABEL_4FF6_:	
		ld hl, (_RAM_C523_)
		ld de, $0018
		add hl, de
		ld a, $60
		cp h
		jr c, +
		ld (_RAM_C523_), hl
+:	
		ld a, (_RAM_C170_)
		rra
		and $1E
		ld e, a
		ld d, $00
		ld hl, _DATA_5021_
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ld (_RAM_C526_), de
		ld b, $04
		ld ix, $C520
		rst $28	; _LABEL_28_
		ret
	
; Data from 5021 to 5040 (32 bytes)	
_DATA_5021_:	
	.db $00 $96 $80 $96 $00 $97 $80 $97 $00 $98 $80 $97 $00 $97 $80 $96
	.db $00 $96 $80 $95 $00 $95 $80 $94 $38 $95 $80 $94 $00 $95 $80 $95
	
_LABEL_5041_:	
		ld l, a
		ld h, $00
		ld de, _DATA_505D_
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ex de, hl
		ld de, $2180
		ld bc, $0180
		ld a, $03
		call _LABEL_B13_
		di
		call _LABEL_46C_
		ei
		ret
	
; Pointer Table from 505D to 5060 (2 entries, indexed by _RAM_C170_)	
_DATA_505D_:	
	.dw _DATA_DEBB_ _DATA_E03B_
	
_LABEL_5061_:	
		set 0, (hl)
		ld a, $02
		call _LABEL_A4_
		di
		call _LABEL_213_
		call _LABEL_23B_
		call +
		call _LABEL_24E_
		ei
		ld a, $03
		call _LABEL_A4_
		call _LABEL_50D0_
		call _LABEL_510B_
		call _LABEL_B8A_
		call _LABEL_5147_
		call _LABEL_224_
		ret
	
+:	
		ld de, _RAM_C220_
		ld a, $80
		ld b, $0C
		ld hl, _DATA_9D1_
		call _LABEL_971_
		ld a, $2C
		ld (_RAM_C224_), a
		ld a, $64
		ld (_RAM_C227_), a
		call _LABEL_3EA5_
		ld a, $02
		call _LABEL_B13_
		ld b, $04
-:	
		push bc
		ld hl, _RAM_C10C_
		inc (hl)
		ld a, $01
		call _LABEL_A4_
		ld hl, _DATA_8000_
		ld de, $2000
		ld bc, $0180
		call _LABEL_2AD_
		pop bc
		djnz -
		call _LABEL_477E_
		ld b, $0C
		ld ix, $C220
		rst $28	; _LABEL_28_
		ret
	
_LABEL_50D0_:	
		ld de, _RAM_C3A0_
		ld a, $8C
		ld b, $0C
		ld hl, _DATA_9D1_
		call _LABEL_971_
		ld a, $2C
		ld (_RAM_C3A4_), a
		ld a, $9A
		ld (_RAM_C3A7_), a
		ld hl, $C3A0
		ld a, $0C
		ld b, a
		call _LABEL_22F4_
		ld a, $03
		call _LABEL_B13_
		ld hl, _DATA_DEBB_
		ld de, $2180
		ld bc, $0180
		di
		call _LABEL_46C_
		ei
		ld b, $0C
		ld ix, $C3A0
		rst $28	; _LABEL_28_
		ret
	
_LABEL_510B_:	
		ld de, _RAM_C520_
		ld a, $98
		ld b, $04
		ld hl, $0999
		call _LABEL_971_
		ld a, $40
		ld (_RAM_C524_), a
		ld a, $96
		ld (_RAM_C527_), a
		ld hl, $C520
		ld a, $18
		ld b, $04
		call _LABEL_22F4_
		ld a, $03
		call _LABEL_B13_
		ld hl, _DATA_E4BB_
		ld de, $2300
		ld bc, $0080
		di
		call _LABEL_46C_
		ei
		ld b, $04
		ld ix, $C520
		rst $28	; _LABEL_28_
		ret
	
_LABEL_5147_:	
		call +
		ld bc, $0325
-:	
		call _LABEL_B67_
		inc c
		djnz -
		ret
	
+:	
		ld hl, _DATA_51CF_
		ld de, _RAM_CC41_
		ld bc, $0008
		ldir
		ld a, (_RAM_C114_)
		ld c, a
		xor a
		bit 6, c
		jr z, +
		inc a
+:	
		add a, $02
		ld (de), a
		inc de
		ld hl, _DATA_51D7_
		ld c, $06
		ldir
		ld a, (_RAM_C128_)
		rra
		rra
		and $1F
		cp $09
		jr c, +
		ld c, $02
		ld a, $FF
+:	
		add a, $02
		ld l, a
		ld a, c
		ex de, hl
		ld (hl), a
		inc hl
		ld (hl), e
		inc hl
		ex de, hl
		ld hl, _DATA_51DD_
		ld c, $07
		ldir
		ld a, (_RAM_C128_)
		and $03
		add a, $02
		ld (de), a
		inc de
		ld hl, _DATA_51E4_
		ld c, $02
		ldir
		ex de, hl
		ld a, (_RAM_C12D_)
		inc a
		cp $64
		jp nc, +
		ld bc, $FF0A
-:	
		inc b
		sub c
		jr nc, -
		add a, c
		add a, $01
		ld e, a
		ld a, b
		or a
		jp z, ++
		add a, $01
-:	
		ld (hl), a
		inc hl
		ld (hl), e
		ret
	
+:	
		ld a, $0A
		ld e, a
		jp -
	
++:	
		ld a, $00
		jp -
	
; Data from 51CF to 51D6 (8 bytes)	
_DATA_51CF_:	
	.db $08 $0C $1A $15 $17 $13 $20 $00
	
; Data from 51D7 to 51DC (6 bytes)	
_DATA_51D7_:	
	.db $07 $15 $20 $13 $15 $00
	
; Data from 51DD to 51E3 (7 bytes)	
_DATA_51DD_:	
	.db $07 $20 $1D $0B $1C $12 $00
	
; Data from 51E4 to 51E5 (2 bytes)	
_DATA_51E4_:	
	.db $03 $16
	
_LABEL_51E6_:	
		ld a, (_RAM_C16B_)
		and $E0
		ret nz
		ld a, (_RAM_C128_)
		cp $28
		jr z, ++
		and $03
		xor $03
		ld a, $81
		jr nz, +
		inc a
+:	
		ld (_RAM_C002_), a
		ret
	
++:	
		ld a, $8C
		ld (_RAM_C002_), a
		ret
	
_LABEL_5206_:	
		ld hl, _RAM_C175_
		ld de, _RAM_C17B_
		bit 6, (hl)
		jp nz, _LABEL_52AE_
		ld a, (_RAM_C11F_)
		and $30
		bit 1, (hl)
		jp nz, _LABEL_52A6_
		ex de, hl
		bit 7, (hl)
		jr nz, _LABEL_528C_
		bit 6, (hl)
		jr nz, _LABEL_5281_
		bit 5, (hl)
		jr nz, _LABEL_5278_
		bit 4, (hl)
		jr nz, _LABEL_526D_
		bit 3, (hl)
		jr nz, ++++
		bit 2, (hl)
		jr nz, +++
		bit 1, (hl)
		jr nz, ++
		bit 0, (hl)
		jr nz, +
		or a
		jr z, _LABEL_52AE_
		cp $10
		jr nz, _LABEL_52AB_
		set 0, (hl)
+:	
		or a
		jr z, +
		cp $10
		jr nz, _LABEL_52AB_
		jr _LABEL_52AE_
	
+:	
		set 1, (hl)
++:	
		or a
		jr z, _LABEL_52AE_
		cp $10
		jr nz, _LABEL_52AB_
		set 2, (hl)
+++:	
		or a
		jr z, +
		cp $10
		jr nz, _LABEL_52AB_
		jr _LABEL_52AE_
	
+:	
		set 3, (hl)
++++:	
		or a
		jr z, _LABEL_52AE_
		cp $20
		jr nz, _LABEL_52AB_
		set 4, (hl)
_LABEL_526D_:	
		or a
		jr z, +
		cp $20
		jr nz, _LABEL_52AB_
		jr _LABEL_52AE_
	
+:	
		set 5, (hl)
_LABEL_5278_:	
		or a
		jr z, _LABEL_52AE_
		cp $20
		jr nz, _LABEL_52AB_
		set 6, (hl)
_LABEL_5281_:	
		or a
		jr z, +
		cp $20
		jr nz, _LABEL_52AB_
		jr _LABEL_52AE_
	
+:	
		set 7, (hl)
_LABEL_528C_:	
		bit 0, (hl)
		jr z, +
		or a
		jr z, _LABEL_52AE_
		res 1, (hl)
		cp $30
		jr z, ++
		jr _LABEL_52AE_
	
+:	
		or a
		jr z, _LABEL_52AB_
		cp $30
		jp nz, _LABEL_52AE_
++:	
		ex de, hl
		set 1, (hl)
_LABEL_52A6_:	
		cp $30
		jr z, +
		ex de, hl
_LABEL_52AB_:	
		ex de, hl
		set 6, (hl)
_LABEL_52AE_:	
		or a
		ret
	
+:	
		scf
		ret
	
_LABEL_52B2_:	
		ld a, (_RAM_C10B_)
		and $0F
		ret nz
		ld a, (_RAM_C11F_)
		and $0F
		or a
		ret z
		ld hl, _RAM_C129_
		ld (hl), $00
		ld b, $04
		ld hl, _DATA_52E6_
-:	
		rra
		jr c, +
		inc hl
		djnz -
+:	
		ld a, (_RAM_C128_)
		add a, (hl)
		cp $24
		jr c, ++
		ld c, $DC
		cp $50
		jr c, +
		ld c, $24
+:	
		add a, c
++:	
		ld (_RAM_C128_), a
		jp _LABEL_5147_
	
; Data from 52E6 to 52E9 (4 bytes)	
_DATA_52E6_:	
	.db $04 $FC $FF $01
	
_LABEL_52EA_:	
		ld a, $02
		call _LABEL_B13_
		ld hl, _RAM_CCE9_
		exx
		ld hl, _DATA_8000_
		ld c, $12
		exx
----:	
		ld c, $03
---:	
		exx
		ld b, $20
--:	
		ld a, (hl)
		exx
		ld b, $08
-:	
		rra
		rl e
		djnz -
		ld (hl), e
		inc hl
		exx
		inc hl
		djnz --
		exx
		ld de, $FFC0
		add hl, de
		dec c
		jr nz, ---
		ld de, $00C0
		add hl, de
		exx
		dec c
		exx
		jr nz, ----
		ret
	
_LABEL_531F_:	
		cp $09
		jr nc, +++
		dec a
		dec b
		ld c, a
		ld a, b
		cp $08
		jr nc, +
		xor c
		and $04
		ld hl, $0000
		jr z, ++
+:	
		ld a, c
		and $04
		ld hl, _DATA_8000_
		jr z, ++
		ld hl, _RAM_CCA9_
++:	
		ld (_RAM_CC10_), hl
		ld l, c
		ld h, $00
		add hl, hl
		ld a, $02
		call _LABEL_B13_
		ld de, _DATA_9D40_
		add hl, de
		ld a, (hl)
		inc hl
		ld h, (hl)
		ld l, a
		ld (_RAM_CC12_), hl
		ret
	
+++:	
		sub $09
		ld l, a
		ld h, $00
		add hl, hl
		ld a, $02
		call _LABEL_B13_
		ld de, _DATA_9D50_
		add hl, de
		ld a, (hl)
		inc hl
		ld h, (hl)
		ld l, a
		ld (_RAM_CC10_), hl
		ld de, $00C0
		add hl, de
		ld (_RAM_CC12_), hl
		ret
	
; 226th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5374_:	
		ld a, $10
		ld (_RAM_C198_), a
; 70th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5379_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_539D_
		call _LABEL_225F_
		ld a, $13
		ld b, $02
		rst $30	; _LABEL_30_
	; Data from 538E to 539C (15 bytes)
	.db $10 $FD $3E $AB $06 $04 $CD $F1 $22 $21 $98 $C1 $CB $CE $C9
	
; Data from 539D to 53A0 (4 bytes)	
_DATA_539D_:	
	.db $03 $03 $02 $20
	
; 69th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_53A1_:	
		call _LABEL_233B_
		cp $40
		jp c, _LABEL_544D_
		bit 2, (ix+2)
		jp nz, _LABEL_5458_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_5431_
		bit 4, a
		jp nz, +
		ld a, (ix+7)
		cp $BE
		jr nc, +
		ld de, $FFF0
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		cp $10
		jp c, _LABEL_544D_
		xor a
		ld hl, $0404
		call _LABEL_392F_
		bit 1, (ix+20)
		jr z, +
		xor a
		ld (ix+16), a
		ld (ix+17), a
+:	
		ld de, $FFFE
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld a, $20
		ld hl, _RAM_C199_
		inc (hl)
		ld c, a
		cp (hl)
		ld d, $00
		jr nc, +
		add a, c
		dec a
		cp (hl)
		inc d
		jr nc, +
		ld (hl), $00
+:	
		ld a, d
		and a
		jr nz, +
		ld hl, _RAM_C198_
		bit 1, (hl)
		jr nz, ++
		set 1, (hl)
		res 0, (hl)
		ld a, $AB
		ld b, $04
		call _LABEL_22F1_
		jr ++
	
+:	
		ld hl, _RAM_C198_
		bit 0, (hl)
		jr nz, ++
		set 0, (hl)
		res 1, (hl)
		ld a, $B3
		ld b, $04
		call _LABEL_22F1_
		jr ++
	
_LABEL_5431_:	
		call _LABEL_22A6_
		cp $10
		jp c, _LABEL_544D_
		ld hl, _RAM_C198_
		bit 2, (hl)
		jr nz, ++
		set 2, (hl)
		ld a, $AF
		ld b, $04
		call _LABEL_22F1_
++:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_544D_:	
		ld b, $04
		call _LABEL_24A9_
		ld hl, _RAM_C198_
		res 7, (hl)
		ret
	
_LABEL_5458_:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
; 74th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_545E_:	
		ld a, $05
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_548E_
		call _LABEL_225F_
		ld a, $3D
		rst $30	; _LABEL_30_
	; Data from 5471 to 548D (29 bytes)
	.db $3E $3C $F7 $3E $3B $F7 $3E $A9 $06 $06 $CD $F1 $22 $21 $98 $C1
	.db $CB $CE $DD $2A $96 $C1 $11 $FF $FF $CD $98 $22 $C9
	
; Data from 548E to 5491 (4 bytes)	
_DATA_548E_:	
	.db $07 $03 $04 $20
	
; 201st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5492_:	
		ld hl, _RAM_C198_
		set 6, (hl)
		jr _LABEL_54A3_
	
; 203rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5499_:	
		ld hl, _RAM_C198_
		set 6, (hl)
; 113th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_549E_:	
		ld hl, _RAM_C198_
		set 0, (hl)
; 73rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_54A3_:	
		call _LABEL_233B_
		cp $3C
		jp c, _LABEL_5565_
		bit 2, (ix+2)
		jp nz, _LABEL_5570_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_5549_
		ld a, (_RAM_C198_)
		bit 0, a
		jr nz, +
		ld de, $FFC4
		jr ++
	
+:	
		ld de, $FF94
++:	
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld hl, _RAM_C198_
		bit 6, (hl)
		jr z, ++
		ld a, (ix+7)
		cp $BE
		jr nc, ++
		ld de, $FFE8
		call _LABEL_227A_
		xor a
		ld hl, $0406
		call _LABEL_392F_
		bit 1, (ix+20)
		jr z, +
		xor a
		ld (ix+16), a
		ld (ix+17), a
+:	
		ld e, (ix+16)
		ld d, (ix+17)
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		cp $10
		jp c, _LABEL_5565_
++:	
		ld a, $10
		ld hl, _RAM_C199_
		inc (hl)
		ld c, a
		cp (hl)
		ld d, $00
		jr nc, +
		add a, c
		dec a
		cp (hl)
		inc d
		jr nc, +
		ld (hl), $00
+:	
		ld a, d
		and a
		jr nz, +
		ld hl, _RAM_C198_
		bit 1, (hl)
		jr nz, ++
		set 1, (hl)
		res 2, (hl)
		ld a, $AF
		ld b, $06
		call _LABEL_22F1_
		jp ++
	
+:	
		ld hl, _RAM_C198_
		bit 2, (hl)
		jr nz, ++
		set 2, (hl)
		res 1, (hl)
		ld a, $B5
		ld b, $06
		call _LABEL_22F1_
		jp ++
	
_LABEL_5549_:	
		call _LABEL_22B9_
		cp $10
		jp c, _LABEL_5565_
		ld hl, _RAM_C198_
		bit 3, (hl)
		jr nz, ++
		set 3, (hl)
		ld a, $A9
		ld b, $06
		call _LABEL_22F1_
++:	
		ld b, $06
		rst $28	; _LABEL_28_
		ret
	
_LABEL_5565_:	
		ld b, $06
		call _LABEL_24A9_
		ld hl, _RAM_C198_
		res 7, (hl)
		ret
	
_LABEL_5570_:	
		ld b, $06
		call _LABEL_2DF2_
		ret
	
; 78th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5576_:	
		ld a, $05
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_558E_
		call _LABEL_225F_
		ld b, $02
		ld a, $41
		rst $30	; _LABEL_30_
	; Data from 558B to 558D (3 bytes)
	.db $10 $FD $C9
	
; Data from 558E to 5591 (4 bytes)	
_DATA_558E_:	
	.db $07 $03 $04 $20
	
; 77th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5592_:	
		call _LABEL_233B_
		cp $38
		jp c, _LABEL_560A_
		bit 2, (ix+2)
		jp nz, _LABEL_5615_
		call _LABEL_2301_
		ld hl, _RAM_C198_
		bit 7, (hl)
		jp nz, _LABEL_55EE_
		ld hl, _RAM_C199_
		ld a, $10
		inc (hl)
		ld c, a
		cp (hl)
		ld d, $00
		jr nc, +
		add a, c
		dec a
		cp (hl)
		inc d
		jr nc, +
		ld (hl), $00
+:	
		ld a, d
		and a
		jr nz, +
		ld hl, _RAM_C198_
		bit 0, (hl)
		jr nz, ++
		set 0, (hl)
		res 1, (hl)
		ld a, $9D
		ld b, $06
		call _LABEL_22F1_
		jp ++
	
+:	
		ld hl, _RAM_C198_
		bit 1, (hl)
		jr nz, ++
		set 1, (hl)
		res 0, (hl)
		ld a, $97
		ld b, $06
		call _LABEL_22F1_
		jp ++
	
_LABEL_55EE_:	
		call _LABEL_22B9_
		cp $10
		jp c, _LABEL_560A_
		ld hl, _RAM_C19C_
		bit 0, (hl)
		jr nz, ++
		set 0, (hl)
		ld a, $A3
		ld b, $06
		call _LABEL_22F1_
++:	
		ld b, $06
		rst $28	; _LABEL_28_
		ret
	
_LABEL_560A_:	
		ld b, $06
		call _LABEL_24A9_
		ld hl, _RAM_C198_
		res 7, (hl)
		ret
	
_LABEL_5615_:	
		ld b, $06
		call _LABEL_2DF2_
		ret
	
; 80th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_561B_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_5637_
		call _LABEL_225F_
		ld a, $64
		rst $30	; _LABEL_30_
	; Data from 562E to 5636 (9 bytes)
	.db $3E $63 $F7 $21 $98 $C1 $CB $CE $C9
	
; Data from 5637 to 563A (4 bytes)	
_DATA_5637_:	
	.db $03 $03 $02 $20
	
; 79th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_563B_:	
		call _LABEL_233B_
		cp $40
		jp c, _LABEL_56B2_
		bit 2, (ix+2)
		jp nz, _LABEL_56BD_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_5697_
		ld a, $08
		ld hl, _RAM_C199_
		inc (hl)
		ld c, a
		cp (hl)
		ld d, $00
		jr nc, +
		add a, c
		dec a
		cp (hl)
		inc d
		jr nc, +
		ld (hl), $00
+:	
		ld a, d
		and a
		jr nz, +
		ld hl, _RAM_C198_
		bit 1, (hl)
		jr nz, ++
		set 1, (hl)
		res 0, (hl)
		ld a, $93
		ld b, $04
		call _LABEL_22F1_
		jp ++
	
+:	
		ld hl, _RAM_C198_
		bit 0, (hl)
		jr nz, ++
		set 0, (hl)
		res 1, (hl)
		ld a, $8F
		ld b, $04
		call _LABEL_22F1_
		jp ++
	
_LABEL_5697_:	
		call _LABEL_22A6_
		cp $10
		jr c, _LABEL_56B2_
		ld hl, _RAM_C198_
		bit 2, (hl)
		jr nz, ++
		set 2, (hl)
		ld a, $8B
		ld b, $04
		call _LABEL_22F1_
++:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_56B2_:	
		ld b, $04
		call _LABEL_24A9_
		ld hl, _RAM_C198_
		res 7, (hl)
		ret
	
_LABEL_56BD_:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
; 102nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_56C3_:	
		ld a, $05
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_5709_
		call _LABEL_225F_
		ld a, $34
		ld b, $05
		rst $30	; _LABEL_30_
	; Data from 56D6 to 5708 (51 bytes)
	.db $10 $FD $3E $64 $06 $06 $CD $F1 $22 $21 $98 $C1 $CB $D6 $DD $2A
	.db $96 $C1 $3A $27 $C1 $CB $47 $20 $0D $11 $FF $FF $CD $98 $22 $11
	.db $80 $01 $CD $9F $22 $C9 $11 $C0 $02 $CD $98 $22 $11 $80 $01 $CD
	.db $9F $22 $C9
	
; Data from 5709 to 570C (4 bytes)	
_DATA_5709_:	
	.db $07 $03 $05 $20
	
; 205th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_570D_:	
		ld hl, _RAM_C1A0_
		set 0, (hl)
; 101st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5712_:	
		call _LABEL_233B_
		cp $C0
		jp nc, _LABEL_58AA_
		bit 2, (ix+2)
		jp nz, _LABEL_58B8_
		ld a, (_RAM_C198_)
		bit 6, a
		jr z, +
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_585F_
+:	
		bit 6, a
		jr nz, +
		bit 5, (ix+2)
		jr z, +
		res 5, (ix+2)
		ld hl, _RAM_C198_
		set 6, (hl)
		ld a, $02
		call _LABEL_615E_
+:	
		ld a, (_RAM_C1A0_)
		bit 0, a
		jr z, _LABEL_57BB_
		ld a, (_RAM_C198_)
		bit 0, a
		jr nz, +
		ld b, $D4
		call _LABEL_22D9_
		jp c, _LABEL_57BB_
		ld hl, _RAM_C198_
		set 0, (hl)
		bit 1, (hl)
		jr nz, _LABEL_57BB_
+:	
		xor a
		ld hl, $0606
		call _LABEL_392F_
		ld a, (ix+20)
		bit 1, a
		jr z, +
		ld hl, _RAM_C198_
		set 1, (hl)
		jp _LABEL_57BB_
	
+:	
		ld a, (_RAM_C127_)
		bit 2, a
		jr z, +
		ld de, $0001
		call _LABEL_2289_
		jp ++
	
+:	
		ld de, $0180
++:	
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld de, $FFF0
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		cp $10
		jp c, _LABEL_58AA_
		ld a, (_RAM_C198_)
		bit 6, a
		jr nz, +
		ld a, $64
		jr ++
	
+:	
		ld a, $70
++:	
		ld b, $06
		call _LABEL_22F1_
		jp _LABEL_5891_
	
_LABEL_57BB_:	
		ld a, (_RAM_C127_)
		bit 2, a
		jr z, +
		ld de, $0001
		call _LABEL_2289_
		jp ++
	
+:	
		ld de, $0180
++:	
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld a, (_RAM_C198_)
		bit 6, a
		jr nz, _LABEL_581D_
		ld a, $08
		ld hl, _RAM_C199_
		inc (hl)
		ld c, a
		cp (hl)
		ld d, $00
		jr nc, +
		add a, c
		dec a
		cp (hl)
		inc d
		jr nc, +
		ld (hl), $00
+:	
		ld a, d
		and a
		jr nz, +
		ld hl, _RAM_C198_
		bit 2, (hl)
		jp nz, _LABEL_5891_
		set 2, (hl)
		res 3, (hl)
		ld a, $64
		ld b, $06
		call _LABEL_22F1_
		jp _LABEL_5891_
	
+:	
		ld hl, _RAM_C198_
		bit 3, (hl)
		jp nz, _LABEL_5891_
		set 3, (hl)
		res 2, (hl)
		ld a, $6A
		ld b, $06
		call _LABEL_22F1_
		jp _LABEL_5891_
	
_LABEL_581D_:	
		ld hl, _RAM_C19A_
		ld a, $08
		inc (hl)
		ld c, a
		cp (hl)
		ld d, $00
		jr nc, +
		add a, c
		dec a
		cp (hl)
		inc d
		jr nc, +
		ld (hl), $00
+:	
		ld a, d
		and a
		jr nz, +
		ld hl, _RAM_C198_
		bit 4, (hl)
		jr nz, _LABEL_5891_
		set 4, (hl)
		res 5, (hl)
		ld a, $70
		ld b, $06
		call _LABEL_22F1_
		jp _LABEL_5891_
	
+:	
		ld hl, _RAM_C198_
		bit 5, (hl)
		jr nz, _LABEL_5891_
		set 5, (hl)
		res 4, (hl)
		ld a, $76
		ld b, $06
		call _LABEL_22F1_
		jp _LABEL_5891_
	
_LABEL_585F_:	
		ld a, (_RAM_C19C_)
		bit 6, a
		jr nz, ++
		ld hl, (_RAM_C196_)
		ld de, $0007
		add hl, de
		ld a, (_RAM_C227_)
		cp (hl)
		ld hl, _RAM_C19C_
		jr nc, +
		set 7, (hl)
+:	
		set 6, (hl)
++:	
		call _LABEL_22A6_
		cp $10
		jr c, +
		ld hl, _RAM_C19C_
		bit 1, (hl)
		jr nz, _LABEL_5891_
		set 1, (hl)
		ld a, $7C
		ld b, $06
		call _LABEL_22F1_
_LABEL_5891_:	
		ld b, $06
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld a, (_RAM_C19C_)
		bit 7, a
		jr z, _LABEL_58AA_
		ld a, $40
		ld h, a
		ld a, (_RAM_C227_)
		add a, $20
		ld l, a
		ld a, $45
		call _LABEL_6E2_
_LABEL_58AA_:	
		ld b, $06
		call _LABEL_24A9_
		ld hl, _RAM_C198_
		res 7, (hl)
		ld a, $56
		rst $30	; _LABEL_30_
	; Data from 58B7 to 58B7 (1 bytes)
	.db $C9
	
_LABEL_58B8_:	
		ld b, $06
		call _LABEL_2DF2_
		ret
	
; 116th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_58BE_:	
		ld a, $0B
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_58DE_
		call _LABEL_225F_
		ld a, $40
		rst $30	; _LABEL_30_
	; Data from 58D1 to 58DD (13 bytes)
	.db $06 $06 $3E $A9 $CD $F1 $22 $21 $98 $C1 $CB $C6 $C9
	
; Data from 58DE to 58E1 (4 bytes)	
_DATA_58DE_:	
	.db $03 $05 $03 $20
	
; 141st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_58E2_:	
		ld a, (_RAM_C198_)
		bit 3, a
		jr nz, _LABEL_58F4_
		ld de, $0260
		call _LABEL_2298_
		ld hl, _RAM_C198_
		set 3, (hl)
; 115th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_58F4_:	
		call _LABEL_233B_
		cp $30
		jp c, _LABEL_59A3_
		bit 2, (ix+2)
		jp nz, _LABEL_59AE_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_5987_
		bit 4, a
		jr nz, +
		ld b, $40
		call _LABEL_22D9_
		jp nc, ++
		ld hl, _RAM_C198_
		set 4, (hl)
+:	
		xor a
		ld hl, $0604
		call _LABEL_392F_
		ld a, (ix+20)
		bit 1, a
		jr z, +
		ld hl, _RAM_C198_
		res 3, (hl)
		jr +++
	
+:	
		ld de, $FFEC
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
++:	
		ld de, $FF80
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
+++:	
		ld a, $10
		ld hl, _RAM_C199_
		inc (hl)
		ld c, a
		cp (hl)
		ld d, $00
		jr nc, +
		add a, c
		dec a
		cp (hl)
		inc d
		jr nc, +
		ld (hl), $00
+:	
		ld a, d
		and a
		jr nz, +
		ld hl, _RAM_C198_
		bit 0, (hl)
		jr nz, ++
		set 0, (hl)
		res 1, (hl)
		ld a, $A9
		ld b, $06
		call _LABEL_22F1_
		jp ++
	
+:	
		ld hl, _RAM_C198_
		bit 1, (hl)
		jr nz, ++
		set 1, (hl)
		res 0, (hl)
		ld a, $AF
		ld b, $06
		call _LABEL_22F1_
		jp ++
	
_LABEL_5987_:	
		call _LABEL_22A6_
		cp $10
		jp c, _LABEL_59A3_
		ld hl, _RAM_C198_
		bit 2, (hl)
		jr nz, ++
		set 2, (hl)
		ld a, $B5
		ld b, $06
		call _LABEL_22F1_
++:	
		ld b, $06
		rst $28	; _LABEL_28_
		ret
	
_LABEL_59A3_:	
		ld b, $06
		call _LABEL_24A9_
		ld hl, _RAM_C198_
		set 7, (hl)
		ret
	
_LABEL_59AE_:	
		ld b, $06
		call _LABEL_2DF2_
		ret
	
; 168th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_59B4_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, _RAM_C198_
		set 6, (hl)
		jp +
	
; 120th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_59C4_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
+:	
		ld bc, _DATA_5A1E_
		call _LABEL_225F_
		ld hl, _RAM_C19F_
		ld (hl), $30
		inc l
		ld (hl), $60
		ld a, (_RAM_C198_)
		bit 6, a
		jr nz, +
		ld a, d
		ld (_RAM_C19E_), a
		ld a, e
		ld (_RAM_C19D_), a
		jp ++
	
+:	
		ld hl, (_RAM_C196_)
		ld de, $000A
		add hl, de
		set 3, (hl)
++:	
		ld a, $3A
		rst $30	; _LABEL_30_
	; Data from 59F8 to 5A1D (38 bytes)
	.db $3A $98 $C1 $CB $77 $20 $11 $3A $0B $C1 $32 $A0 $C1 $87 $32 $9F
	.db $C1 $2A $96 $C1 $2C $2C $CB $F6 $16 $02 $3A $A0 $C1 $5F $DD $2A
	.db $96 $C1 $CD $98 $22 $C9
	
; Data from 5A1E to 5A21 (4 bytes)	
_DATA_5A1E_:	
	.db $03 $03 $03 $20
	
; 119th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5A22_:	
		call _LABEL_234B_
		cp $38
		jp c, _LABEL_5AFE_
		bit 2, (ix+2)
		jp nz, _LABEL_5B09_
		call _LABEL_2301_
		ld hl, _RAM_C198_
		bit 7, (hl)
		jp nz, _LABEL_5AE3_
		ld a, (_RAM_C198_)
		bit 6, a
		jr nz, +
		ld de, (_RAM_CB3B_)
		ld hl, (_RAM_C19C_)
		xor a
		sbc hl, de
		ld (_RAM_C19C_), hl
		jp ++
	
+:	
		ld a, (_RAM_C198_)
		bit 0, a
		jr nz, ++
		ld b, $30
		call _LABEL_22D9_
		ret nc
		ld hl, _RAM_C198_
		set 0, (hl)
++:	
		ld a, (_RAM_C198_)
		bit 6, a
		jr z, +
		ld de, $FFC0
		jp ++
	
+:	
		ld a, (_RAM_C19F_)
		ld e, a
		bit 3, a
		jr nz, +
		neg
		ld d, $FF
		jp ++
	
+:	
		ld d, $00
++:	
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld de, $FFE0
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		cp $10
		jp c, _LABEL_5AFE_
		ld hl, _RAM_C199_
		ld a, $20
		inc (hl)
		cp (hl)
		jr nc, +
		ld (hl), $00
		ld a, (_RAM_C198_)
		bit 6, a
		jr nz, ++
		ld a, (_RAM_C19E_)
		ld h, a
		ld a, (_RAM_C19D_)
		ld l, a
		ld a, $3B
		call _LABEL_6E2_
		res 6, (ix+2)
		jp ++
	
+:	
		ld hl, _RAM_C198_
		bit 1, (hl)
		jp nz, +++
		set 1, (hl)
		ld a, $85
		ld b, $04
		call _LABEL_22F1_
		jp +++
	
++:	
		ld hl, _RAM_C198_
		bit 2, (hl)
		jr nz, +++
		set 2, (hl)
		ld a, $81
		ld b, $04
		call _LABEL_22F1_
		jp +++
	
_LABEL_5AE3_:	
		call _LABEL_22A6_
		cp $10
		jr c, _LABEL_5AFE_
		ld hl, _RAM_C198_
		bit 3, (hl)
		jr nz, +++
		set 3, (hl)
		ld a, $89
		ld b, $04
		call _LABEL_22F1_
+++:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_5AFE_:	
		ld b, $04
		call _LABEL_24A9_
		ld hl, _RAM_C198_
		res 7, (hl)
		ret
	
_LABEL_5B09_:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
; 122nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5B0F_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_5B2D_
		call _LABEL_225F_
		ld a, $3E
		rst $30	; _LABEL_30_
	; Data from 5B22 to 5B2C (11 bytes)
	.db $DD $2A $96 $C1 $11 $00 $03 $CD $98 $22 $C9
	
; Data from 5B2D to 5B30 (4 bytes)	
_DATA_5B2D_:	
	.db $03 $03 $03 $20
	
; 121st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5B31_:	
		call _LABEL_233B_
		cp $40
		jp c, _LABEL_5BBF_
		bit 2, (ix+2)
		jp nz, _LABEL_5BCA_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_5BA3_
		bit 0, a
		jp nz, +
		ld b, $28
		call _LABEL_22D9_
		ret nc
		ld hl, _RAM_C198_
		set 0, (hl)
+:	
		call _LABEL_22B9_
		cp $10
		jr c, _LABEL_5BBF_
		ld a, (_RAM_C198_)
		bit 1, a
		jr nz, _LABEL_5BBB_
		ld hl, _RAM_C19A_
		ld a, $18
		inc (hl)
		ld c, a
		cp (hl)
		ld d, $00
		jr nc, +
		add a, c
		dec a
		cp (hl)
		inc d
		jr nc, +
		ld (hl), $00
+:	
		ld a, d
		and a
		jr nz, +
		ld hl, _RAM_C198_
		bit 2, (hl)
		jr nz, _LABEL_5BBB_
		set 2, (hl)
		ld a, $8F
		ld b, $04
		call _LABEL_22F1_
		jp _LABEL_5BBB_
	
+:	
		ld hl, _RAM_C198_
		set 1, (hl)
		ld a, $8B
		ld b, $04
		call _LABEL_22F1_
		jp _LABEL_5BBB_
	
_LABEL_5BA3_:	
		call _LABEL_22A6_
		cp $10
		jp c, _LABEL_5BBF_
		ld hl, _RAM_C198_
		bit 4, (hl)
		jr nz, _LABEL_5BBB_
		set 4, (hl)
		ld a, $93
		ld b, $04
		call _LABEL_22F1_
_LABEL_5BBB_:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_5BBF_:	
		ld b, $04
		call _LABEL_24A9_
		ld hl, _RAM_C198_
		res 7, (hl)
		ret
	
_LABEL_5BCA_:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
; 118th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5BD0_:	
		ld a, $05
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_5BF0_
		call _LABEL_225F_
		ld a, $3F
		rst $30	; _LABEL_30_
	; Data from 5BE3 to 5BEF (13 bytes)
	.db $06 $06 $3E $97 $CD $F1 $22 $21 $98 $C1 $CB $C6 $C9
	
; Data from 5BF0 to 5BF3 (4 bytes)	
_DATA_5BF0_:	
	.db $04 $03 $03 $20
	
; 117th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5BF4_:	
		call _LABEL_233B_
		cp $30
		jp c, _LABEL_5C73_
		bit 2, (ix+2)
		jp nz, _LABEL_5C7E_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_5C57_
		ld de, $FF60
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld a, $08
		ld hl, _RAM_C199_
		inc (hl)
		ld c, a
		cp (hl)
		ld d, $00
		jr nc, +
		add a, c
		dec a
		cp (hl)
		inc d
		jr nc, +
		ld (hl), $00
+:	
		ld a, d
		and a
		jr nz, +
		ld hl, _RAM_C198_
		bit 0, (hl)
		jr nz, ++
		set 0, (hl)
		res 1, (hl)
		ld a, $97
		ld b, $06
		call _LABEL_22F1_
		jp ++
	
+:	
		ld hl, _RAM_C198_
		bit 1, (hl)
		jr nz, ++
		set 1, (hl)
		res 0, (hl)
		ld a, $9D
		ld b, $06
		call _LABEL_22F1_
		jp ++
	
_LABEL_5C57_:	
		call _LABEL_22A6_
		cp $10
		jp c, _LABEL_5C73_
		ld hl, _RAM_C198_
		bit 2, (hl)
		jr nz, ++
		set 2, (hl)
		ld a, $A3
		ld b, $06
		call _LABEL_22F1_
++:	
		ld b, $06
		rst $28	; _LABEL_28_
		ret
	
_LABEL_5C73_:	
		ld b, $06
		call _LABEL_24A9_
		ld hl, _RAM_C198_
		res 7, (hl)
		ret
	
_LABEL_5C7E_:	
		ld b, $06
		call _LABEL_2DF2_
		ret
	
; 134th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5C84_:	
		ld a, $0C
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, (_RAM_C196_)
		inc l
		inc l
		set 6, (hl)
		inc l
		inc l
		ld (hl), d
		inc l
		inc l
		inc l
		ld (hl), e
		ld a, $43
		rst $30	; _LABEL_30_
	; Data from 5C9D to 5CA4 (8 bytes)
	.db $06 $02 $3E $74 $CD $F1 $22 $C9
	
; 207th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5CA5_:	
		ld hl, _RAM_C198_
		set 0, (hl)
; 133rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5CAA_:	
		call _LABEL_233B_
		cp $3C
		jp c, +
		ld b, $02
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld a, (ix+4)
		add a, $02
		ld h, a
		ld l, (ix+7)
		ld a, (_RAM_C198_)
		bit 0, a
		jr nz, +
		ld a, $32
		jr ++
	
+:	
		ld a, $66
++:	
		call _LABEL_6E2_
		ld b, $02
		call _LABEL_24A9_
		ret
	
; 140th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5CD5_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, (_RAM_C196_)
		inc l
		inc l
		set 3, (hl)
		inc l
		inc l
		ld (hl), d
		inc l
		ld (hl), $02
		inc l
		inc l
		ld (hl), e
		inc l
		ld (hl), $03
		ld a, $44
		rst $30	; _LABEL_30_
	; Data from 5CF3 to 5CFA (8 bytes)
	.db $06 $04 $3E $72 $CD $F1 $22 $C9
	
; 139th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5CFB_:	
		call _LABEL_233B_
		cp $40
		jp c, ++
		bit 5, (ix+2)
		jr nz, +
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, $0B
		call _LABEL_6E2_
		ld a, $06
		call _LABEL_615E_
		ld a, $02
		call _LABEL_648D_
		ld a, $8D
		call _LABEL_A99_
++:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 158th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5D2D_:	
		ld a, $00
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, (_RAM_C196_)
		inc l
		inc l
		set 6, (hl)
		inc l
		inc l
		ld (hl), d
		inc l
		inc l
		inc l
		ld (hl), e
		ld a, (_RAM_C199_)
		ld a, e
		ld (_RAM_C199_), a
		ld a, $65
		rst $30	; _LABEL_30_
	; Data from 5D4D to 5D54 (8 bytes)
	.db $06 $01 $3E $80 $CD $F1 $22 $C9
	
; 157th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5D55_:	
		ld hl, _RAM_C19A_
		bit 0, (hl)
		jr nz, _LABEL_5D74_
		set 0, (hl)
		ld b, $03
-:	
		ld de, _RAM_C199_
		ld a, (_RAM_C10B_)
		add a, a
		ld h, a
		ld a, (de)
		add a, $06
		ld (de), a
		ld l, a
		ld a, $61
		call _LABEL_6E2_
		djnz -
; 195th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5D74_:	
		call _LABEL_234B_
		cp $3D
		jr c, +
		ld hl, (_RAM_C196_)
		ld de, $BDEF
		rst $18	; _LABEL_18_
		ld b, $01
		rst $28	; _LABEL_28_
		ret
	
+:	
		ld b, $01
		call _LABEL_24A9_
		ret
	
; 178th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5D8C_:	
		ld a, (_RAM_C12F_)
		bit 7, a
		jr nz, +
		bit 0, a
		jr nz, +
		ld a, $05
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, (_RAM_C196_)
		inc l
		inc l
		set 3, (hl)
		inc l
		inc l
		ld (hl), d
		inc l
		ld (hl), $07
		inc l
		inc l
		ld (hl), e
		inc l
		ld (hl), $03
		inc l
		ld (hl), $80
		ld a, $6A
		rst $30	; _LABEL_30_
	; Data from 5DB8 to 5DC4 (13 bytes)
	.db $06 $06 $3E $20 $CD $F1 $22 $21 $2F $C1 $CB $F6 $C9
	
+:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld hl, (_RAM_C196_)
		inc l
		inc l
		set 3, (hl)
		inc l
		inc l
		ld (hl), d
		inc l
		ld (hl), $03
		inc l
		inc l
		ld (hl), e
		inc l
		ld (hl), $03
		inc l
		ld (hl), $80
		ld a, $6B
		rst $30	; _LABEL_30_
	; Data from 5DE6 to 5DED (8 bytes)
	.db $06 $04 $3E $20 $CD $F1 $22 $C9
	
; 177th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_5DEE_:	
		call _LABEL_233B_
		cp $30
		jp c, _LABEL_5E48_
		ld hl, (_RAM_C196_)
		inc l
		inc l
		bit 5, (hl)
		jr nz, +++
		ld a, (_RAM_C12F_)
		bit 6, a
		jr z, +
		ld b, $06
		jr ++
	
+:	
		ld b, $04
++:	
		rst $28	; _LABEL_28_
		ret
	
+++:	
		ld h, (ix+4)
		ld l, (ix+7)
		ld a, (_RAM_C12F_)
		bit 6, a
		jr z, +
		ld a, $3F
		call _LABEL_6E2_
		ld a, $97
		call _LABEL_A99_
		ld hl, _RAM_C130_
		inc (hl)
		ld hl, _RAM_C12F_
		ld a, (hl)
		add a, $41
		ld (hl), a
		ld b, $06
		jp ++
	
+:	
		ld a, $0B
		call _LABEL_6E2_
		ld a, $97
		call _LABEL_A99_
		ld a, $06
		call _LABEL_615E_
		ld b, $04
		jr ++
	
_LABEL_5E48_:	
		ld a, (_RAM_C12F_)
		bit 6, a
		jr nz, +
		ld b, $04
		jr ++
	
+:	
		ld hl, _RAM_C12F_
		res 6, (hl)
		ld b, $06
++:	
		call _LABEL_24A9_
		ret
	
_LABEL_5E5E_:	
		ld hl, _RAM_CC31_
		ld de, _RAM_C121_
		ld b, $02
		call _LABEL_61A8_
		jr nc, +
		ld a, (de)
		add a, $01
		daa
		cp $10
		jr nc, ++
		ld (de), a
+:	
		call _LABEL_6187_
		call _LABEL_633D_
		call _LABEL_618F_
		ret
	
++:	
		call _LABEL_6187_
		call _LABEL_6179_
		call _LABEL_633D_
		call _LABEL_618F_
		ret
	
_LABEL_5E8B_:	
		ld hl, _RAM_CC3D_
		bit 6, (hl)
		jp nz, _LABEL_5FA9_
		set 6, (hl)
		di
		call _LABEL_213_
		ei
		di
		call _LABEL_ACA_
		ei
		di
		call _LABEL_27A_
		ei
		di
		call _LABEL_62D_
		ei
		di
		call _LABEL_6271_
		ei
		di
		call _LABEL_62AA_
		ei
		ld a, $01
		call _LABEL_A4_
		di
		ld hl, $0000
		ld bc, $0020
		xor a
		call _LABEL_202_
		ei
		ld a, $01
		call _LABEL_A4_
		di
		ld hl, $3800
		ld bc, $0100
		xor a
		call _LABEL_202_
		ei
		ld a, $01
		call _LABEL_A4_
		di
		ld hl, $3900
		ld bc, $0100
		xor a
		call _LABEL_202_
		ei
		ld a, $01
		call _LABEL_A4_
		di
		ld hl, $3A00
		ld bc, $0100
		xor a
		call _LABEL_202_
		ei
		ld a, $01
		call _LABEL_A4_
		di
		ld hl, $3B00
		ld bc, $0100
		xor a
		call _LABEL_202_
		ei
		ld a, $01
		call _LABEL_A4_
		di
		ld hl, $3C00
		ld bc, $0100
		xor a
		call _LABEL_202_
		ei
		ld a, $01
		call _LABEL_A4_
		di
		ld hl, $3D00
		ld bc, $0100
		xor a
		call _LABEL_202_
		ei
		ld a, $01
		call _LABEL_A4_
		di
		ld hl, $3E00
		ld bc, $0100
		xor a
		call _LABEL_202_
		ei
		ld a, $01
		call _LABEL_A4_
		ld a, $03
		call _LABEL_B13_
		ld hl, _DATA_DB6A_
		ld de, $0020
		ex de, hl
		ld c, $04
_LABEL_5F4E_:	
		push hl
_LABEL_5F4F_:	
		ld a, (de)
		or a
		jp z, ++
		inc a
		jp z, +
		dec a
		di
		call _LABEL_413_
		ei
		inc hl
		inc hl
		inc hl
		inc hl
		inc de
		jp _LABEL_5F4F_
	
+:	
		inc de
		ld a, (de)
		di
		call _LABEL_413_
		ei
		inc hl
		inc hl
		inc hl
		inc hl
		di
		call _LABEL_413_
		ei
		inc hl
		inc hl
		inc hl
		inc hl
		inc de
		jp _LABEL_5F4F_
	
++:	
		inc de
		ld a, (de)
		inc de
		or a
		jr z, +
		ld b, a
		ld a, (de)
-:	
		di
		call _LABEL_413_
		ei
		inc hl
		inc hl
		inc hl
		inc hl
		djnz -
		inc de
		jp _LABEL_5F4F_
	
+:	
		pop hl
		inc hl
		dec c
		jp nz, _LABEL_5F4E_
		ex de, hl
		call _LABEL_224_
		ld hl, _RAM_C198_
		ld (hl), $00
		ld hl, _RAM_CC38_
		ld (hl), $C0
_LABEL_5FA9_:	
		call _LABEL_60A0_
		ld hl, _RAM_CC38_
		xor a
		cp (hl)
		jr z, +
		dec (hl)
		ret
	
+:	
		ld a, (_RAM_CC3D_)
		bit 0, a
		jr nz, +
		call ++
		ret
	
+:	
		ld a, (_RAM_CC3D_)
		bit 1, a
		jr nz, +
		ld hl, _RAM_CC3E_
		ld (hl), $04
		call _LABEL_601A_
		ret
	
+:	
		ld a, (_RAM_CC3D_)
		bit 2, a
		jr nz, +
		ld hl, _RAM_CC3E_
		ld (hl), $04
		call _LABEL_605B_
		ret
	
+:	
		ld hl, _RAM_C12F_
		ld (hl), $00
		ld hl, _RAM_C163_
		set 7, (hl)
		ret
	
++:	
		ld hl, _RAM_CC36_
		xor a
		cp (hl)
		jr nz, +
		ld hl, _RAM_CC3D_
		set 0, (hl)
		ld hl, _RAM_CC38_
		ld (hl), $60
		ret
	
+:	
		dec (hl)
		ld hl, _RAM_CC39_
		ld a, (hl)
		add a, $30
		daa
		ld (hl), a
		jr c, +
		ld hl, _RAM_CC38_
		ld (hl), $06
		ret
	
+:	
		inc hl
		xor a
		ld a, (hl)
		inc a
		daa
		ld (hl), a
		ld hl, _RAM_CC38_
		ld (hl), $06
		ret
	
_LABEL_601A_:	
		ld a, (_RAM_CC3D_)
		bit 3, a
		jr nz, ++
		ld a, (_RAM_C12F_)
		bit 0, a
		jr z, +
		ld hl, _RAM_CC39_
		ld de, _RAM_CC3B_
		ld a, (hl)
		ld (de), a
		inc hl
		inc de
		ld a, (hl)
		ld (de), a
+:	
		ld hl, _RAM_CC3D_
		set 3, (hl)
++:	
		ld de, (_RAM_CC39_)
		ld a, e
		or a
		jr nz, +
		ld a, d
		or a
		jr nz, +
		ld hl, _RAM_CC3D_
		set 1, (hl)
		ld hl, _RAM_CC38_
		ld (hl), $60
		ret
	
+:	
		call _LABEL_62D7_
		ld (_RAM_CC39_), de
		call _LABEL_62F0_
		ret
	
_LABEL_605B_:	
		ld a, (_RAM_C12F_)
		bit 0, a
		jr nz, +
		ld hl, _RAM_CC3D_
		set 2, (hl)
		ld hl, _RAM_CC38_
		ld (hl), $FF
		ret
	
+:	
		ld hl, _RAM_CC3D_
		bit 4, (hl)
		jr nz, +
		set 7, (hl)
		set 4, (hl)
		ld hl, _RAM_CC38_
		ld (hl), $60
		ret
	
+:	
		ld de, (_RAM_CC3B_)
		ld a, e
		or a
		jr nz, +
		ld a, d
		or a
		jr nz, +
		ld hl, _RAM_CC3D_
		set 2, (hl)
		ld hl, _RAM_CC38_
		ld (hl), $A0
		ret
	
+:	
		call _LABEL_62D7_
		ld (_RAM_CC3B_), de
		call _LABEL_62F0_
		ret
	
_LABEL_60A0_:	
		call _LABEL_632E_
		ld de, $3910
		call _LABEL_61FF_
		ld hl, $3924
		ld de, _RAM_C113_
		ld b, $03
		call _LABEL_61B2_
		ld de, $3990
		call _LABEL_6211_
		ld a, (_RAM_C114_)
		bit 7, a
		jr nz, +
		ld hl, $39A4
		ld de, _RAM_C123_
		ld b, $03
		call _LABEL_61B2_
		jr ++
	
+:	
		ld de, $3A10
		call _LABEL_6225_
		ld a, (_RAM_C114_)
		bit 6, a
		jr nz, +
		ld hl, $39A4
		ld de, _RAM_C123_
		ld b, $03
		call _LABEL_61B2_
		ld hl, $3A24
		ld de, _RAM_C14F_
		ld b, $03
		call _LABEL_61B2_
		jr ++
	
+:	
		ld hl, $39A4
		ld de, _RAM_C139_
		ld b, $03
		call _LABEL_61B2_
		ld hl, $3A24
		ld de, _RAM_C123_
		ld b, $03
		call _LABEL_61B2_
++:	
		call _LABEL_624A_
		ld de, $3B22
		call _LABEL_6239_
		ld hl, $3B9C
		ld de, _RAM_CC3A_
		ld b, $02
		call _LABEL_61B2_
		call _LABEL_625E_
		ld de, $3C22
		call _LABEL_6239_
		ld a, (_RAM_CC3D_)
		bit 7, a
		jr z, ++
		ld hl, _RAM_CC3D_
		bit 5, (hl)
		jr nz, +
		set 5, (hl)
		ld a, $99
		call _LABEL_A99_
		ld ix, _RAM_C220_
		ld b, $06
		call _LABEL_57F_
+:	
		ld hl, $3C9C
		ld de, _RAM_CC3C_
		ld b, $02
		call _LABEL_61B2_
		ret
	
++:	
		ld a, (_RAM_C12F_)
		bit 0, a
		ret z
		ld ix, $C220
		ld b, $06
		rst $28	; _LABEL_28_
		ret
	
_LABEL_615E_:	
		ld b, $00
		ld hl, _DATA_635F_
		add a, a
		ld c, a
		add hl, bc
		ld de, _RAM_CC31_
		ld b, $02
		call _LABEL_61A8_
		ret
	
_LABEL_616F_:	
		ld hl, _RAM_C12D_
		inc (hl)
		ld a, $9C
		call _LABEL_A99_
		ret
	
_LABEL_6179_:	
		ld hl, _RAM_C123_
		ld (hl), $10
		dec hl
		ld b, $02
-:	
		ld (hl), $00
		dec hl
		djnz -
		ret
	
_LABEL_6187_:	
		ld hl, _RAM_CC31_
		xor a
		ld (hl), a
		inc hl
		ld (hl), a
		ret
	
_LABEL_618F_:	
		ld hl, _RAM_C113_
		ld de, _RAM_C123_
		ld b, $03
-:	
		ld a, (de)
		cp (hl)
		ret c
		jr nz, _LABEL_61A1_
		dec hl
		dec de
		djnz -
		ret
	
_LABEL_61A1_:	
		ld (hl), a
		dec hl
		dec de
		ld a, (de)
		djnz _LABEL_61A1_
		ret
	
_LABEL_61A8_:	
		xor a
-:	
		ld a, (de)
		adc a, (hl)
		daa
		ld (de), a
		inc hl
		inc de
		djnz -
		ret
	
_LABEL_61B2_:	
		exx
		ld c, $00
		ld hl, _RAM_CC33_
		ld (hl), $00
		exx
-:	
		ld a, (de)
		exx
		ld (hl), a
		xor a
		rrd
		ld e, $00
		ld e, a
		ld a, (hl)
		or a
		jr nz, +
		cp c
		jr c, +
		exx
		ld a, $00
		jr ++
	
+:	
		inc c
		exx
		add a, $01
++:	
		di
		call _LABEL_47C_
		ei
		inc hl
		inc hl
		exx
		xor a
		ld a, e
		or a
		jr nz, +
		cp c
		jr c, +
		exx
		ld a, $00
		jr ++
	
+:	
		inc c
		exx
		add a, $01
++:	
		di
		call _LABEL_47C_
		ei
		inc hl
		inc hl
		dec de
		djnz -
		ld a, $01
		di
		call _LABEL_47C_
		ei
		ret
	
_LABEL_61FF_:	
		ld hl, _DATA_620B_
		ld bc, $0006
		di
		call _LABEL_486_
		ei
		ret
	
; Data from 620B to 6210 (6 bytes)	
_DATA_620B_:	
	.db $2C $0D $0E $0F $0D $2C
	
_LABEL_6211_:	
		ld hl, _DATA_621D_
		ld bc, $0008
		di
		call _LABEL_486_
		ei
		ret
	
; Data from 621D to 6224 (8 bytes)	
_DATA_621D_:	
	.db $0C $1A $15 $17 $13 $20 $00 $02
	
_LABEL_6225_:	
		ld hl, _DATA_6231_
		ld bc, $0008
		di
		call _LABEL_486_
		ei
		ret
	
; Data from 6231 to 6238 (8 bytes)	
_DATA_6231_:	
	.db $0C $1A $15 $17 $13 $20 $00 $03
	
_LABEL_6239_:	
		ld hl, _DATA_6245_
		ld bc, $0005
		di
		call _LABEL_486_
		ei
		ret
	
; Data from 6245 to 6249 (5 bytes)	
_DATA_6245_:	
	.db $10 $1D $1C $0B $21
	
_LABEL_624A_:	
		ld de, $3B16
		ld hl, _DATA_6259_
		ld bc, $0005
		di
		call _LABEL_486_
		ei
		ret
	
; Data from 6259 to 625D (5 bytes)	
_DATA_6259_:	
	.db $20 $1D $0B $1C $12
	
_LABEL_625E_:	
		ld de, $3C18
		ld hl, _DATA_626D_
		ld bc, $0004
		di
		call _LABEL_486_
		ei
		ret
	
; Data from 626D to 6270 (4 bytes)	
_DATA_626D_:	
	.db $12 $1D $1A $1A
	
_LABEL_6271_:	
		ld hl, _DATA_9A1_
		ld de, _RAM_C220_
		ld a, $80
		ld b, $06
		call _LABEL_971_
		ld hl, _RAM_C224_
		ld (hl), $24
		ld hl, _RAM_C227_
		ld (hl), $82
		ld a, $04
		call _LABEL_B13_
		ld hl, _DATA_1332D_
		ld de, $2000
		ld bc, $00C0
		di
		call _LABEL_2AD_
		ei
		ld hl, _RAM_C22C_
		ld de, $0020
		ld b, $06
		xor a
-:	
		ld (hl), a
		add hl, de
		inc a
		djnz -
		ret
	
_LABEL_62AA_:	
		ld hl, _DATA_9E9_
		ld de, _RAM_C4A0_
		ld a, $94
		ld b, $08
		call _LABEL_971_
		ld hl, _RAM_C4A4_
		ld (hl), $6A
		ld hl, _RAM_C4A7_
		ld (hl), $82
		push af
		ld a, $04
		call _LABEL_B13_
		pop af
		ld hl, _DATA_13F54_
		ld de, $2300
		ld bc, $0080
		di
		call _LABEL_2AD_
		ei
		ret
	
_LABEL_62D7_:	
		xor a
		ld a, $96
		add a, e
		daa
		ld e, a
		ret c
		xor a
		ld a, d
		or a
		jr z, +
		dec d
		ret
	
+:	
		xor a
		ld a, e
		sub $96
		ld hl, _RAM_CC3E_
		ld (hl), a
		xor a
		ld e, a
		ret
	
_LABEL_62F0_:	
		ld hl, _RAM_C121_
		xor a
		ld a, (_RAM_CC3E_)
		add a, (hl)
		daa
		ld (hl), a
		jr nc, +
		xor a
		inc hl
		ld a, (hl)
		inc a
		daa
		ld (hl), a
		jr nc, +
		xor a
		inc hl
		ld a, (hl)
		inc a
		daa
		cp $10
		jr nc, ++
		ld (hl), a
+:	
		call _LABEL_633D_
		call _LABEL_618F_
		ld a, $06
		ld hl, _RAM_C198_
		inc (hl)
		cp (hl)
		ret nc
		ld (hl), $00
		ld a, $98
		call _LABEL_A99_
		ret
	
++:	
		call _LABEL_6179_
		call _LABEL_633D_
		call _LABEL_618F_
		ret
	
_LABEL_632E_:	
		ld hl, _RAM_CC36_
		ld a, (hl)
		or a
		jr z, +
		call _LABEL_63E5_
		ret
	
+:	
		call _LABEL_63E9_
		ret
	
_LABEL_633D_:	
		ld a, (_RAM_C12E_)
		cp $06
		ret nc
		ld hl, $636F
		ld e, a
		ld d, $00
		add hl, de
		inc hl
		ld de, _RAM_C123_
		ld a, (de)
		cp (hl)
		ret c
		dec de
		dec hl
		ld a, (de)
		cp (hl)
		ret c
		ld hl, _RAM_C12E_
		inc (hl)
		inc (hl)
		call _LABEL_616F_
		ret
	
; Data from 635F to 6374 (22 bytes)	
_DATA_635F_:	
	.db $00 $00 $01 $00 $02 $00 $05 $00 $10 $00 $20 $00 $50 $00 $00 $01
	.db $30 $00 $00 $01 $70 $01
	
_LABEL_6375_:	
		ld hl, _RAM_CC34_
		bit 7, (hl)
		ret nz
		ld hl, _RAM_CC30_
		bit 7, (hl)
		ret nz
		ld a, (_RAM_C16B_)
		and $20
		ret nz
		ld a, (_RAM_CC35_)
		or a
		jr nz, _LABEL_63CB_
		ld hl, _RAM_CC36_
		ld (hl), $0C
		ld hl, _RAM_CC35_
		ld (hl), $FF
		ld hl, _DATA_9E9_
		ld de, _RAM_C4A0_
		ld a, $94
		ld b, $08
		call _LABEL_971_
		push af
		ld a, $04
		call _LABEL_B13_
		pop af
		ld hl, _DATA_13F54_
		ld de, $2300
		ld bc, $0080
		di
		call _LABEL_46C_
		ei
		ld hl, _RAM_C4A2_
		set 6, (hl)
		ld hl, _RAM_C4A4_
		ld (hl), $6A
		ld hl, _RAM_C4A7_
		ld (hl), $82
		jp _LABEL_63E9_
	
_LABEL_63CB_:	
		ld hl, _RAM_CC35_
		dec (hl)
		ld a, (hl)
		or a
		ret nz
		ld (hl), $FF
		ld hl, _RAM_CC36_
		dec (hl)
		ld a, (hl)
		or a
		jp nz, _LABEL_63E5_
		ld a, $01
		ld (_RAM_CC37_), a
		jp _LABEL_63E9_
	
_LABEL_63E5_:	
		xor a
		ld (_RAM_CC37_), a
_LABEL_63E9_:	
		ld bc, $0800
		ld hl, _RAM_C4AC_
-:	
		call +
		ld (hl), a
		ld de, $0020
		add hl, de
		djnz -
		ld ix, $C4A0
		ld b, $08
		call _LABEL_28_
		ld hl, _RAM_CC36_
		ld a, (hl)
		or a
		ret nz
		ld hl, _RAM_CC34_
		set 7, (hl)
		ret
	
+:	
		ld a, (_RAM_CC36_)
		or a
		rra
		ld d, a
		ld a, (_RAM_CC37_)
		bit 0, a
		jr nz, _LABEL_647A_
		bit 1, a
		jr nz, _LABEL_6463_
		ld a, d
		jr nc, ++
		cp $02
		jr c, +
		ld a, $08
		ld (_RAM_CC37_), a
		jp ++
	
+:	
		ld a, $04
		ld (_RAM_CC37_), a
++:	
		ld a, d
		or a
		jr z, _LABEL_647D_
		cp $02
		jr c, +
		ld a, $02
+:	
		inc c
		cp c
		jr c, +
		ld a, $18
		ret
	
+:	
		ld a, (_RAM_CC37_)
		bit 2, a
		jr nz, _LABEL_647D_
		ld a, d
		cp $03
		jr c, +++
		ld a, (_RAM_CC37_)
		bit 3, a
		jr z, +
		ld a, $0A
		jp ++
	
+:	
		ld a, $02
++:	
		ld (_RAM_CC37_), a
		ld c, $00
_LABEL_6463_:	
		ld a, d
		or a
		sub $02
		inc c
		cp c
		jr c, +++
		ld a, $1A
		ret
	
+++:	
		ld a, (_RAM_CC37_)
		bit 3, a
		jr nz, +
		ld a, $01
		ld (_RAM_CC37_), a
_LABEL_647A_:	
		ld a, $FF
		ret
	
_LABEL_647D_:	
		ld a, $01
		ld (_RAM_CC37_), a
		ld a, $19
		ret
	
+:	
		ld a, $01
		ld (_RAM_CC37_), a
		ld a, $1B
		ret
	
_LABEL_648D_:	
		ld hl, _RAM_CC35_
		ld (hl), $FF
		ld hl, _RAM_CC36_
		add a, (hl)
		ld (hl), a
		cp $10
		jp c, _LABEL_63E5_
		ld (hl), $10
		jp _LABEL_63E5_
	
_LABEL_64A1_:	
		ld hl, _RAM_CC35_
		ld (hl), $FF
		ld hl, _RAM_CC36_
		ld c, a
		ld a, (hl)
		sub c
		jr c, +
		or a
		jr z, +
		ld (hl), a
		jp _LABEL_63E5_
	
+:	
		ld (hl), $00
		ld a, $01
		ld (_RAM_CC37_), a
		jp _LABEL_63E9_
	
; 162nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_64BF_:	
		ld a, $40
		ld (_RAM_C198_), a
; 76th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_64C4_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_64DF_
		call _LABEL_225F_
		ld a, $53
		rst $30	; _LABEL_30_
	; Data from 64D7 to 64DE (8 bytes)
	.db $3E $80 $06 $04 $CD $F1 $22 $C9
	
; Data from 64DF to 64E2 (4 bytes)	
_DATA_64DF_:	
	.db $04 $03 $02 $20
	
; 75th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_64E3_:	
		call _LABEL_233B_
		cp $38
		jr c, _LABEL_654D_
		bit 2, (ix+2)
		jp nz, _LABEL_6553_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jr nz, +++
		bit 6, a
		jr nz, ++
		bit 1, a
		jr nz, +
		ld b, $1C
		call _LABEL_22D9_
		jr nc, ++
		ld a, $02
		ld (_RAM_C198_), a
+:	
		ld de, $FFF0
		call _LABEL_655D_
		jr z, ++
		ld a, $80
		ld (_RAM_C198_), a
		jp +++
	
++:	
		ld hl, _RAM_C198_
		bit 2, (hl)
		jr nz, ++++
		set 2, (hl)
		ld a, $80
		ld b, $04
		call _LABEL_22F1_
		jp ++++
	
+++:	
		ld hl, _RAM_C198_
		bit 3, (hl)
		jr nz, +
		set 3, (hl)
		ld a, $84
		ld b, $04
		call _LABEL_22F1_
+:	
		call ++++
		ld hl, _RAM_C19B_
		ld a, $04
		inc (hl)
		cp (hl)
		ret nc
_LABEL_654D_:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
_LABEL_6553_:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
++++:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_655D_:	
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		xor a
		ld hl, $0404
		call _LABEL_392F_
		ld a, (ix+20)
		bit 1, a
		ret
	
; 136th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6571_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_658C_
		call _LABEL_225F_
		ld a, $54
		rst $30	; _LABEL_30_
	; Data from 6584 to 658B (8 bytes)
	.db $3E $AB $06 $04 $CD $F1 $22 $C9
	
; Data from 658C to 658F (4 bytes)	
_DATA_658C_:	
	.db $03 $03 $03 $20
	
; 135th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6590_:	
		call _LABEL_233B_
		cp $38
		jp c, _LABEL_660B_
		bit 2, (ix+2)
		jp nz, _LABEL_6605_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_65EA_
		bit 0, a
		jr nz, _LABEL_65F1_
		bit 1, a
		jr nz, +
		ld b, $10
		call _LABEL_22D9_
		jr nc, ++
		ld a, $02
		ld (_RAM_C198_), a
		jp +
	
+:	
		ld de, $FFE8
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		cp $2E
		jr nc, ++
		ld a, $01
		ld (_RAM_C198_), a
		jp _LABEL_65F1_
	
++:	
		ld hl, _RAM_C198_
		bit 2, (hl)
		jr nz, +
		set 2, (hl)
		ld a, $AB
		ld b, $04
		call _LABEL_22F1_
		jp +
	
_LABEL_65EA_:	
		call _LABEL_22B9_
		cp $10
		jr c, _LABEL_660B_
_LABEL_65F1_:	
		ld hl, _RAM_C198_
		bit 3, (hl)
		jr nz, +
		set 3, (hl)
		ld a, $AF
		ld b, $04
		call _LABEL_22F1_
+:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_6605_:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
_LABEL_660B_:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
; 138th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6611_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld a, $01
		ld (_RAM_C1A0_), a
		jr +
	
; 144th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6620_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld a, $02
		ld (_RAM_C1A0_), a
		jr +
	
; 160th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_662F_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
+:	
		ld e, $CC
		ld bc, _DATA_664A_
		call _LABEL_225F_
		ld a, $55
		rst $30	; _LABEL_30_
	; Data from 6642 to 6649 (8 bytes)
	.db $3E $8B $06 $04 $CD $F1 $22 $C9
	
; Data from 664A to 664D (4 bytes)	
_DATA_664A_:	
	.db $03 $03 $04 $20
	
; 137th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_664E_:	
		call _LABEL_233B_
		cp $40
		jp c, _LABEL_66D6_
		bit 2, (ix+2)
		jp nz, _LABEL_66DC_
		call _LABEL_2301_
		ld a, (_RAM_C198_)
		bit 7, a
		jp nz, _LABEL_66B8_
		ld a, (_RAM_C1A0_)
		bit 0, a
		jr nz, +
		bit 1, a
		jr nz, ++
		ld de, $FFED
		jr +++
	
+:	
		ld de, $FFF6
		jr +++
	
++:	
		ld de, $FFF2
+++:	
		call _LABEL_227A_
		bit 7, d
		jr nz, +
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		jr ++
	
+:	
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		cp $10
		jp c, +++
		ld hl, _RAM_C19A_
		ld a, $06
		call _LABEL_22C7_
		ld a, d
		and a
		jr nz, ++
		ld a, $8F
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
++:	
		ld a, $8B
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
_LABEL_66B8_:	
		call _LABEL_22A6_
		cp $10
		jr c, _LABEL_66D6_
		ld a, $93
		ld b, $04
		call _LABEL_22F1_
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+++:	
		ld de, $0260
		call _LABEL_2298_
		ld a, $A4
		call _LABEL_A99_
		ret
	
_LABEL_66D6_:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
_LABEL_66DC_:	
		ld b, $04
		call _LABEL_2DF2_
		ret
	
; 172nd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_66E2_:	
		ld a, $0D
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $D0
		ld bc, _DATA_66FD_
		call _LABEL_225F_
		ld a, $6C
		rst $30	; _LABEL_30_
	; Data from 66F5 to 66FC (8 bytes)
	.db $3E $64 $06 $08 $CD $F1 $22 $C9
	
; Data from 66FD to 6700 (4 bytes)	
_DATA_66FD_:	
	.db $08 $02 $05 $10
	
; 171st entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6701_:	
		call _LABEL_233B_
		bit 2, (ix+2)
		jr nz, _LABEL_6756_
		ld a, (ix+4)
		cp $30
		jr c, _LABEL_6750_
		ld hl, _RAM_C198_
		bit 0, (hl)
		jr nz, +
		ld b, $23
		call _LABEL_22D9_
		ret nc
		ld hl, _RAM_C198_
		inc (hl)
+:	
		ld de, $FE40
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		ld de, $FFE0
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld a, $03
		ld hl, _RAM_C19F_
		call _LABEL_22C7_
		ld a, d
		and a
		jr nz, +
		ld a, $6C
		ld b, $08
		call _LABEL_22F1_
		jr ++
	
+:	
		ld a, $64
		ld b, $08
		call _LABEL_22F1_
++:	
		ld b, $08
		rst $28	; _LABEL_28_
		ret
	
_LABEL_6750_:	
		ld b, $08
		call _LABEL_24A9_
		ret
	
_LABEL_6756_:	
		ld b, $08
		call _LABEL_2DF2_
		ret
	
; 210th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_675C_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld e, $C8
		ld bc, _DATA_36ED_
		call _LABEL_225F_
		ld a, $92
		rst $30	; _LABEL_30_
	; Data from 676F to 677D (15 bytes)
	.db $06 $04 $3E $88 $CD $57 $22 $3E $40 $06 $04 $CD $F1 $22 $C9
	
; 209th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_677E_:	
		call _LABEL_233B_
		cp $40
		jr c, ++++
		bit 5, (ix+2)
		jr nz, +++
		ld de, $FFEC
		ld hl, (_RAM_C196_)
		rst $08	; _LABEL_8_
		ld a, $40
		ld hl, _RAM_C1A0_
		call _LABEL_22C7_
		ld a, d
		and a
		jr nz, +
		ld de, $000E
		jp ++
	
+:	
		ld de, $FFF2
++:	
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
+++:	
		ld a, $8D
		call _LABEL_A99_
		ld a, $01
		call _LABEL_648D_
++++:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
; Jump Table from 67BF to 67C0 (1 entries, indexed by _RAM_C199_)	
_DATA_67BF_:	
	.dw _LABEL_16C3_
	
	; Data from 67C1 to 67CF (15 bytes)
	.db $69 $C3 $39 $69 $C3 $9C $69 $C3 $01 $6A $C3 $44 $6A $20 $00
	
; Pointer Table from 67D0 to 67D1 (1 entries, indexed by _RAM_C128_)	
_DATA_67D0_:	
	.dw $0800
	
	; Data from 67D2 to 67F9 (40 bytes)
	.db $30 $00 $02 $0A $38 $00 $01 $0C $40 $00 $01 $0E $48 $00 $01 $10
	.db $4C $00 $00 $12 $50 $00 $00 $14 $54 $00 $00 $16 $58 $00 $00 $18
	.db $5C $00 $00 $1A $60 $00 $00 $1C
	
; 104th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_67FA_:	
		ld a, (_RAM_C11E_)
		cp $00
		jr nz, +
		call _LABEL_B3F_
		jr ++
	
+:	
		call _LABEL_B34_
++:	
		ld hl, _RAM_CC30_
		set 7, (hl)
		ld a, $80
		ld (_RAM_C522_), a
		call _LABEL_6B58_
		ld a, $A1
		ld hl, _DATA_A3F_
		ld b, $19
		ld de, _RAM_C640_
		call _LABEL_971_
		ld hl, _RAM_C644_
		ld (hl), $32
		inc l
		ld (hl), $08
		inc l
		inc l
		ld (hl), $D0
		inc l
		ld (hl), $08
		inc l
		ld (hl), $00
		inc l
		ld (hl), $28
		ld hl, _RAM_C525_
		ld (hl), $08
		inc l
		inc l
		inc l
		ld (hl), $05
		inc l
		ld (hl), $04
		inc l
		ld (hl), $20
		ld hl, $C520
		ld b, $09
		ld a, $18
		call _LABEL_22F4_
		ld a, (_RAM_C128_)
		and $3C
		push af
		ld e, a
		ld d, $00
		ld hl, _DATA_67D0_ - 2
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ld (_RAM_C196_), de
		inc hl
		ld a, (hl)
		ld (_RAM_C64F_), a
		inc hl
		ld a, (hl)
		ld (_RAM_C1A0_), a
		pop af
		rrca
		rrca
		cp $03
		jr z, +
		cp $07
		jr nz, ++
+:	
		ld hl, _RAM_C19B_
		set 2, (hl)
++:	
		add a, $01
		rst $30	; _LABEL_30_
	; Data from 6883 to 68CC (74 bytes)
	.db $21 $20 $C5 $3E $18 $06 $09 $CD $F4 $22 $21 $40 $C6 $3E $21 $06
	.db $19 $CD $F4 $22 $3A $1E $C1 $FE $00 $20 $0C $21 $9B $C1 $CB $56
	.db $C8 $3E $88 $CD $99 $0A $C9 $21 $9B $C1 $CB $DE $21 $28 $00 $22
	.db $96 $C1 $3E $24 $32 $44 $C6 $21 $84 $CC $CB $C6 $21 $9B $C1 $CB
	.db $56 $3E $87 $28 $01 $3C $CD $99 $0A $C9
	
; 103rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_68CD_:	
		ld hl, _RAM_CC30_
		bit 5, (hl)
		jr nz, +
		set 5, (hl)
		ld hl, _RAM_C19B_
		bit 2, (hl)
		jr z, +
		bit 3, (hl)
		jr nz, +
		ld a, $6A
		ld hl, $0000
		call _LABEL_6E2_
+:	
		ld hl, (_RAM_C196_)
		add hl, hl
		add hl, hl
		add hl, hl
		ld a, h
		ld hl, _RAM_C198_
		add a, (hl)
		ld (hl), a
		inc l
		ld a, (hl)
		inc l
		inc (hl)
		ld l, a
		add a, l
		add a, l
		ld l, a
		ld h, $00
		ld de, _DATA_67BF_
		add hl, de
		jp (hl)
	
	; Data from 6904 to 6B57 (596 bytes)
	.db $21 $99 $C1 $77 $AF $2C $77 $2C $7E $E6 $07 $77 $AF $2C $77 $2C
	.db $77 $C9 $21 $9B $C1 $CB $5E $20 $0C $3A $47 $C6 $FE $A0 $30 $05
	.db $3E $01 $CD $04 $69 $CD $86 $69 $CD $A7 $6A $CD $E2 $6A $CD $FF
	.db $6A $CD $44 $6B $C9 $2A $96 $C1 $ED $5B $9C $C1 $19 $22 $9C $C1
	.db $29 $3E $30 $BC $30 $11 $3A $4F $C6 $B7 $3E $02 $28 $06 $21 $4F
	.db $C6 $35 $3E $01 $CD $04 $69 $21 $9B $C1 $CB $46 $3A $47 $C6 $20
	.db $0B $FE $68 $30 $02 $CB $C6 $CD $86 $69 $18 $09 $FE $A8 $38 $02
	.db $CB $86 $CD $93 $69 $CD $A7 $6A $CD $E2 $6A $CD $FF $6A $CD $44
	.db $6B $C9 $ED $5B $96 $C1 $21 $00 $00 $B7 $ED $52 $EB $18 $04 $ED
	.db $5B $96 $C1 $21 $40 $C6 $CF $C9 $21 $9B $C1 $CB $7E $20 $2B $CB
	.db $FE $3E $49 $CB $56 $28 $02 $3E $4F $06 $03 $F7 $10 $FD $EB $3A
	.db $44 $C6 $C6 $08 $67 $3A $47 $C6 $C6 $06 $6F $3A $9B $C1 $CB $57
	.db $3E $36 $28 $02 $3E $54 $CD $E2 $06 $EB $CB $76 $20 $1A $2D $7E
	.db $FE $50 $38 $1F $36 $00 $2C $CB $F6 $3E $46 $CB $56 $28 $02 $3E
	.db $4C $06 $03 $F7 $10 $FD $18 $0B $2D $7E $FE $40 $38 $05 $3E $01
	.db $CD $04 $69 $CD $A7 $6A $CD $FF $6A $CD $44 $6B $C9 $21 $9B $C1
	.db $CB $7E $20 $0D $CB $FE $DD $21 $20 $C5 $11 $80 $00 $CD $7A $22
	.db $C9 $DD $21 $20 $C5 $11 $FA $FF $CD $7A $22 $21 $20 $C5 $DF $21
	.db $20 $C5 $11 $D8 $FF $CF $3A $24 $C5 $FE $2E $30 $0F $3E $04 $CD
	.db $04 $69 $DD $21 $20 $C5 $06 $09 $CD $7F $05 $C9 $CD $4B $6B $C9
	.db $21 $9B $C1 $CB $7E $20 $1E $CB $FE $3A $28 $C1 $0F $0F $E6 $0F
	.db $FE $09 $30 $04 $C6 $02 $F7 $C9 $21 $00 $23 $01 $20 $01 $AF $F3
	.db $CD $02 $02 $FB $C9 $CB $76 $20 $0E $2D $7E $FE $30 $D8 $2C $CB
	.db $F6 $3E $9E $CD $99 $0A $C9 $21 $40 $C6 $11 $00 $01 $DF $21 $40
	.db $C6 $11 $60 $00 $CF $3A $44 $C6 $17 $30 $11 $21 $30 $CC $CB $F6
	.db $21 $6B $C1 $CB $FE $CD $3F $0B $CD $DA $06 $C9 $CD $A7 $6A $CD
	.db $44 $6B $C9 $2A $9E $C1 $7D $B4 $28 $07 $11 $D0 $FF $19 $22 $9E
	.db $C1 $ED $5B $23 $C5 $19 $EB $2A $43 $C6 $01 $00 $0F $09 $ED $52
	.db $19 $38 $0C $EB $21 $00 $00 $22 $9E $C1 $D5 $CD $58 $6B $D1 $ED
	.db $53 $23 $C5 $2A $46 $C6 $11 $00 $FF $19 $22 $26 $C5 $C9 $21 $9B
	.db $C1 $0E $0B $CB $56 $28 $02 $0E $0F $3A $98 $C1 $1F $1F $1F $AE
	.db $E6 $02 $C8 $AE $77 $E6 $02 $81 $F7 $F7 $C9 $21 $22 $C5 $CB $6E
	.db $C8 $CB $AE $E5 $3E $04 $CD $5E $61 $E1 $EB $21 $A0 $C1 $35 $20
	.db $1D $3E $A9 $CD $99 $0A $21 $42 $C6 $CB $F6 $21 $22 $C5 $CB $F6
	.db $21 $30 $CC $CB $C6 $3E $03 $CD $04 $69 $CD $58 $6B $C9 $EB $CB
	.db $B6 $21 $00 $01 $22 $9E $C1 $3E $A8 $CD $99 $0A $1A $3D $C0 $C9
	.db $DD $21 $40 $C6 $06 $19 $EF $DD $21 $20 $C5 $06 $09 $EF $C9 $21
	.db $BF $09 $18 $03
	
_LABEL_6B58_:	
		ld hl, _DATA_9AD_
		ld a, (_RAM_C522_)
		push af
		ld a, $98
		ld b, $09
		ld de, _RAM_C520_
		call _LABEL_971_
		pop af
		ld (_RAM_C522_), a
		ret
	
; 110th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6B6E_:	
		ld a, $03
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_6B91_
		call _LABEL_225F_
		ld a, $52
		rst $30	; _LABEL_30_
	; Data from 6B7F to 6B90 (18 bytes)
	.db $3E $3A $06 $04 $CD $F1 $22 $DD $2A $96 $C1 $11 $F0 $00 $CD $7A
	.db $22 $C9
	
; Data from 6B91 to 6B94 (4 bytes)	
_DATA_6B91_:	
	.db $04 $04 $00 $10
	
; 109th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6B95_:	
		ld a, (_RAM_CC30_)
		rra
		jp c, _LABEL_6BEA_
		ld hl, _RAM_C198_
		bit 0, (hl)
		jr nz, ++
		inc l
		inc (hl)
		ld a, (hl)
		cp $4E
		jr c, +
		ld (hl), $00
		dec l
		inc (hl)
		ld a, $A2
		call _LABEL_A99_
+:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
++:	
		ld ix, (_RAM_C196_)
		ld de, $FFF5
		call _LABEL_227A_
		ld hl, (_RAM_C196_)
		rst $18	; _LABEL_18_
		ld hl, (_RAM_C196_)
		ld bc, $0004
		add hl, bc
		ld a, (hl)
		cp $2C
		jr nc, +
		ld de, $00C0
		call _LABEL_2298_
+:	
		ld hl, (_RAM_C196_)
		ld de, $FF10
		rst $08	; _LABEL_8_
		ld hl, (_RAM_C196_)
		ld bc, $0007
		add hl, bc
		ld a, (hl)
		cp $40
		jr nc, +
_LABEL_6BEA_:	
		ld b, $04
		call _LABEL_24A9_
		ret
	
+:	
		ld hl, _RAM_C19A_
		inc (hl)
		ld a, (hl)
		ld c, a
		and $07
		jr nz, +
		ld a, c
		rra
		rra
		rra
		and $01
		add a, $52
		rst $30	; _LABEL_30_
+:	
		ld b, $04
		rst $28	; _LABEL_28_
		ret
	
; 170th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6C07_:	
		ld a, $0B
		call _LABEL_92B_
		jp c, _LABEL_6DA_
		ld bc, _DATA_6C20_
		call _LABEL_225F_
		ld a, $68
		rst $30	; _LABEL_30_
	; Data from 6C18 to 6C1F (8 bytes)
	.db $3E $3A $06 $06 $CD $F1 $22 $C9
	
; Data from 6C20 to 6C23 (4 bytes)	
_DATA_6C20_:	
	.db $02 $05 $00 $10
	
; 169th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6C24_:	
		ld a, (_RAM_CC30_)
		rra
		jp c, +++
		ld hl, _RAM_C198_
		bit 0, (hl)
		jr nz, ++
		inc l
		inc (hl)
		ld a, (hl)
		cp $4E
		jr c, +
		ld (hl), $00
		dec l
		inc (hl)
		ld a, $A2
		call _LABEL_A99_
+:	
		ld b, $06
		rst $28	; _LABEL_28_
		ret
	
++:	
		ld hl, (_RAM_C196_)
		ld de, $FFD0
		rst $18	; _LABEL_18_
		ld hl, (_RAM_C196_)
		ld de, $FE00
		rst $08	; _LABEL_8_
		ld hl, (_RAM_C196_)
		ld bc, $0007
		add hl, bc
		ld a, (hl)
		cp $3C
		jr nc, ++++
+++:	
		ld b, $06
		call _LABEL_24A9_
		ret
	
++++:	
		ld hl, _RAM_C19A_
		inc (hl)
		ld a, (hl)
		and $03
		jr nz, +
		ld a, (hl)
		rra
		rra
		and $01
		add a, $68
		rst $30	; _LABEL_30_
+:	
		ld b, $06
		rst $28	; _LABEL_28_
		ret
	
; 173rd entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6C7B_:	
		ld hl, _RAM_C198_
		inc (hl)
		ld a, (hl)
-:	
		cp $38
		jp nc, _LABEL_6DA_
		rra
		rra
		rra
		and $07
		ld e, a
		ld d, $00
		ld hl, _DATA_6CA1_
		add hl, de
		ld a, (hl)
		ld (_RAM_D396_), a
		ret
	
; 197th entry of Jump Table from 721 (indexed by _RAM_C1A1_)	
_LABEL_6C96_:	
		ld hl, _RAM_C198_
		inc (hl)
		ld a, (hl)
		neg
		add a, $37
		jr -
	
; Data from 6CA1 to 6CA7 (7 bytes)	
_DATA_6CA1_:	
	.db $2A $26 $16 $12 $02 $03 $04
	
_LABEL_6CA8_:	
		call +
_LABEL_6CAB_:	
		call _LABEL_6D04_
		ld ix, _RAM_C006_
		ld b, $07
-:	
		push bc
		bit 7, (ix+0)
		call nz, _LABEL_6E64_
		ld de, $0020
		add ix, de
		pop bc
		djnz -
		ret
	
+:	
		ld de, _RAM_C002_
		call +
		inc de
		call +
		inc de
		call +
		ret
	
+:	
		ld a, (de)
		bit 7, a
		ret z
		and $7F
		ld hl, _DATA_6DCC_
		dec a
		ld b, $00
		ld c, a
		add hl, bc
		bit 7, (hl)
		jr z, +
		ld a, (de)
		ld (_RAM_C001_), a
		xor a
		ld (de), a
		pop hl
		pop hl
		jp _LABEL_6CAB_
	
+:	
		ld a, (_RAM_C005_)
		cp (hl)
		jr z, +
		jr nc, ++
+:	
		ld a, (de)
		ld (_RAM_C001_), a
		ld a, (hl)
		ld (_RAM_C005_), a
++:	
		xor a
		ld (de), a
		ret
	
_LABEL_6D04_:	
		ld a, (_RAM_C001_)
		bit 7, a
		jp z, _LABEL_7159_
		cp $AA
		jp nc, _LABEL_7159_
		sub $81
		ret m
		ld c, a
		ld b, $00
		ld hl, _DATA_6D28_
		add hl, bc
		add hl, bc
		ld c, (hl)
		inc hl
		ld b, (hl)
		ld de, _DATA_51_
		add hl, de
		ld a, (hl)
		inc hl
		ld h, (hl)
		ld l, a
		jp (hl)
	
; Pointer Table from 6D28 to 6D73 (38 entries, indexed by _RAM_C001_)	
_DATA_6D28_:	
	.dw _DATA_722D_ _DATA_7394_ _DATA_74A6_ _DATA_7546_ _DATA_7584_ _DATA_76E5_ _DATA_7776_ _DATA_77EE_
	.dw _DATA_78C6_ _DATA_79FB_ _DATA_7A18_ _DATA_7A18_ _DATA_7BBA_ _DATA_7BD4_ _DATA_7BEE_ _DATA_7C08_
	.dw _DATA_7C1D_ _DATA_7C32_ _DATA_7C49_ _DATA_7C60_ _DATA_7C7D_ _DATA_7C99_ _DATA_7CBD_ _DATA_7CCE_
	.dw $7CDC _DATA_7D0D_ _DATA_7D44_ _DATA_7D6D_ _DATA_7D87_ _DATA_7D9C_ _DATA_7DBC_ _DATA_7DD1_
	.dw _DATA_7DE6_ _DATA_7E0D_ _DATA_7E3C_ _DATA_7E3C_ _DATA_7E4F_ _DATA_7E96_
	
	; Data from 6D74 to 6D79 (6 bytes)
	.db $5E $6E $96 $7E $C1 $7E
	
; Jump Table from 6D7A to 6DCB (41 entries, indexed by _RAM_C001_)	
_DATA_6D7A_:	
	.dw _LABEL_6DF5_ _LABEL_6DF5_ _LABEL_6DF5_ _LABEL_6DF5_ _LABEL_6DF5_ _LABEL_6DF5_ _LABEL_6DF5_ _LABEL_6DF5_
	.dw _LABEL_6DF5_ _LABEL_6DF5_ _LABEL_6DF5_ _LABEL_6DF5_ _LABEL_6E08_ _LABEL_6E08_ _LABEL_6E08_ _LABEL_6E08_
	.dw _LABEL_6E08_ _LABEL_6E08_ _LABEL_6E08_ _LABEL_6E18_ _LABEL_6E03_ _LABEL_6E08_ _LABEL_6E08_ _LABEL_6E08_
	.dw _LABEL_6E1E_ _LABEL_6E1E_ _LABEL_6E1E_ _LABEL_6E18_ _LABEL_6E08_ _LABEL_6E1E_ _LABEL_6E08_ _LABEL_6E08_
	.dw _LABEL_6DFA_ _LABEL_6E13_ _LABEL_6E1E_ _LABEL_6E08_ _LABEL_6E18_ _LABEL_6E13_ _LABEL_6E5E_ _LABEL_6E13_
	.dw _LABEL_6DF5_
	
; Data from 6DCC to 6DF4 (41 bytes)	
_DATA_6DCC_:	
	.dsb 12, $80
	.db $00 $00 $00 $00 $04 $00 $00 $10 $10 $10 $10 $10 $70 $70 $10 $10
	.db $10 $10 $10 $10 $10 $08 $20 $04 $10 $10 $80 $80 $80
	
; 1st entry of Jump Table from 6D7A (indexed by _RAM_C001_)	
_LABEL_6DF5_:	
		ld a, $04
		ld (_RAM_C000_), a
; 33rd entry of Jump Table from 6D7A (indexed by _RAM_C001_)	
_LABEL_6DFA_:	
		call _LABEL_7159_
		ld de, _RAM_C006_
		jp +++
	
; 21st entry of Jump Table from 6D7A (indexed by _RAM_C001_)	
_LABEL_6E03_:	
		ld hl, _RAM_C066_
		set 2, (hl)
; 13th entry of Jump Table from 6D7A (indexed by _RAM_C001_)	
_LABEL_6E08_:	
		ld de, _RAM_C0C6_
		ld hl, _RAM_C046_
		set 2, (hl)
		jp ++
	
; 34th entry of Jump Table from 6D7A (indexed by _RAM_C001_)	
_LABEL_6E13_:	
		ld hl, _RAM_C066_
		set 2, (hl)
; 20th entry of Jump Table from 6D7A (indexed by _RAM_C001_)	
_LABEL_6E18_:	
		ld de, _RAM_C0A6_
		jp +
	
; 25th entry of Jump Table from 6D7A (indexed by _RAM_C001_)	
_LABEL_6E1E_:	
		ld de, _RAM_C086_
		ld hl, _RAM_C006_
		set 2, (hl)
+:	
		ld hl, _RAM_C026_
		set 2, (hl)
		ld hl, _RAM_C046_
		set 2, (hl)
		jp ++
	
++:	
		ld a, $FF
		out (Port_PSG), a
		ld a, $DF
		out (Port_PSG), a
+++:	
		ld h, b
		ld l, c
		ld b, (hl)
		inc hl
-:	
		push bc
		ld bc, $0009
		ldir
		ld a, $20
		ld (de), a
		inc de
		ld a, $01
		ld (de), a
		inc de
		xor a
		ld (de), a
		inc de
		ld (de), a
		inc de
		ld (de), a
		push hl
		ld hl, $0012
		add hl, de
		ex de, hl
		pop hl
		inc de
		pop bc
		djnz -
; 39th entry of Jump Table from 6D7A (indexed by _RAM_C001_)	
_LABEL_6E5E_:	
		ld a, $80
		ld (_RAM_C001_), a
		ret
	
_LABEL_6E64_:	
		ld e, (ix+12)
		ld d, (ix+13)
		inc de
		ld (ix+12), e
		ld (ix+13), d
		ld l, (ix+10)
		ld h, (ix+11)
		or a
		sbc hl, de
		call z, _LABEL_6FCA_
		ld e, (ix+16)
		ld d, (ix+17)
		ld a, e
		or d
		jr nz, +
		ld (ix+22), $0F
		jp _LABEL_6F3C_
	
+:	
		bit 5, (ix+0)
		jr nz, +
		ld a, (ix+6)
		or a
		jr nz, _LABEL_6EAE_
		ld (ix+18), e
		ld (ix+19), d
		jp _LABEL_6EF5_
	
_LABEL_6EA3_:	
		dec a
		ld c, a
		ld b, $00
		add hl, bc
		add hl, bc
		ld a, (hl)
		inc hl
		ld h, (hl)
		ld l, a
		ret
	
_LABEL_6EAE_:	
		ld hl, _DATA_7F59_
		call _LABEL_6EA3_
		call _LABEL_6F94_
		jr _LABEL_6EF5_
	
+:	
		push de
		ld l, (ix+20)
		ld h, (ix+21)
		or a
		sbc hl, de
		push af
		ld a, l
		jp p, +
		neg
+:	
		ld h, a
		ld e, (ix+12)
		call _LABEL_720E_
		ld e, (ix+10)
		call _LABEL_721A_
		ld e, a
		ld d, $00
		pop af
		ld a, e
		jp p, +
		neg
		jr z, +
		dec d
		ld e, a
+:	
		pop hl
		add hl, de
		ex de, hl
		ld (ix+18), e
		ld (ix+19), d
		ld a, (ix+6)
		or a
		jp nz, _LABEL_6EAE_
_LABEL_6EF5_:	
		ld a, (ix+7)
		or a
		jr nz, +
		ld a, (ix+8)
		cpl
		and $0F
		ld (ix+22), a
		jr ++
	
+:	
		res 7, a
		ld hl, _DATA_7EE4_
		call _LABEL_6EA3_
		call _LABEL_6F5A_
++:	
		bit 6, (ix+0)
		jr nz, _LABEL_6F3C_
		ld a, (ix+1)
		and $0F
		ld c, a
		ld b, $00
		ld hl, _DATA_6F4F_
		add hl, bc
		ld c, (hl)
		ld a, (ix+18)
		and $0F
		or c
		call _LABEL_7151_
		ld a, (ix+18)
		and $F0
		or (ix+19)
		rrca
		rrca
		rrca
		rrca
		call _LABEL_7151_
_LABEL_6F3C_:	
		ld a, (ix+1)
		and $0F
		ld c, a
		ld b, $00
		ld hl, _DATA_6F53_
		add hl, bc
		ld a, (hl)
		or (ix+22)
		jp _LABEL_7151_
	
; Data from 6F4F to 6F52 (4 bytes)	
_DATA_6F4F_:	
	.db $80 $A0 $C0 $C0
	
; Data from 6F53 to 6F56 (4 bytes)	
_DATA_6F53_:	
	.db $90 $B0 $D0 $F0
	
-:	
		ld (ix+14), a
_LABEL_6F5A_:	
		push hl
		ld a, (ix+14)
		srl a
		push af
		ld c, a
		ld b, $00
		add hl, bc
		pop af
		ld a, (hl)
		pop hl
		jr c, ++
		rrca
		rrca
		rrca
		rrca
		or a
		jr z, -
		cp $10
		jr nz, +
		dec (ix+14)
		jr _LABEL_6F5A_
	
+:	
		cp $20
		jr z, +++
++:	
		inc (ix+14)
		or $F0
		add a, (ix+8)
		inc a
		jr c, ++++
+++:	
		xor a
++++:	
		cpl
		and $0F
		ld (ix+22), a
		ret
	
-:	
		ld (ix+15), a
_LABEL_6F94_:	
		push hl
		ld a, (ix+15)
		srl a
		push af
		ld c, a
		ld b, $00
		add hl, bc
		pop af
		ld a, (hl)
		pop hl
		jr c, +
		rrca
		rrca
		rrca
		rrca
		or a
		jp z, -
		cp $10
		jr nz, +
		dec (ix+15)
		jr _LABEL_6F94_
	
	; Data from 6FB5 to 6FB7 (3 bytes)
	.db $FE $20 $C8
	
+:	
		inc (ix+15)
		cpl
		and $0F
		ld l, a
		ld h, $00
		ex de, hl
		add hl, de
		ld (ix+18), l
		ld (ix+19), h
		ret
	
_LABEL_6FCA_:	
		ld e, (ix+3)
		ld d, (ix+4)
_LABEL_6FD0_:	
		ld a, (de)
		inc de
		cp $E0
		jp nc, _LABEL_705E_
		bit 3, (ix+0)
		jr nz, _LABEL_703D_
		or a
		jp p, ++
		sub $80
		jr z, +
		add a, (ix+5)
+:	
		ld hl, _DATA_717C_
		ld c, a
		ld b, $00
		add hl, bc
		add hl, bc
		ld a, (hl)
		ld (ix+16), a
		inc hl
		ld a, (hl)
		ld (ix+17), a
		bit 5, (ix+0)
		jr z, _LABEL_7057_
		ld a, (de)
		inc de
		sub $80
		add a, (ix+5)
		ld hl, _DATA_717C_
		ld c, a
		ld b, $00
		add hl, bc
		add hl, bc
		ld a, (hl)
		ld (ix+20), a
		inc hl
		ld a, (hl)
		ld (ix+21), a
--:	
		ld a, (de)
_LABEL_7018_:	
		inc de
++:	
		push de
		ld h, a
		ld e, (ix+2)
		call _LABEL_720E_
		pop de
		ld (ix+10), l
		ld (ix+11), h
-:	
		xor a
		ld (ix+14), a
		ld (ix+15), a
		ld (ix+3), e
		ld (ix+4), d
		xor a
		ld (ix+12), a
		ld (ix+13), a
		ret
	
_LABEL_703D_:	
		ld (ix+17), a
		ld a, (de)
		inc de
		ld (ix+16), a
		bit 5, (ix+0)
		jr z, --
		ld a, (de)
		inc de
		ld (ix+21), a
		ld a, (de)
		inc de
		ld (ix+20), a
		jr --
	
_LABEL_7057_:	
		ld a, (de)
		or a
		jp p, _LABEL_7018_
		jr -
	
_LABEL_705E_:	
		ld hl, +	; Overriding return address
		push hl
		and $1F
		ld hl, _DATA_7075_
		ld c, a
		ld b, $00
		add hl, bc
		add hl, bc
		ld a, (hl)
		inc hl
		ld h, (hl)
		ld l, a
		jp (hl)
	
+:	
		inc de
		jp _LABEL_6FD0_
	
; Jump Table from 7075 to 708C (12 entries, indexed by unknown)	
_DATA_7075_:	
	.dw _LABEL_70A6_ _LABEL_70AB_ _LABEL_70DC_ _LABEL_70B0_ _LABEL_70C7_ _LABEL_70D1_ _LABEL_70FC_ _LABEL_7117_
	.dw _LABEL_712A_ _LABEL_70CC_ _LABEL_70D7_ _LABEL_708D_
	
; 12th entry of Jump Table from 7075 (indexed by unknown)	
_LABEL_708D_:	
		dec de
		ld a, (_RAM_C000_)
		ld (_RAM_C008_), a
		ld (_RAM_C028_), a
		ld (_RAM_C048_), a
		ld (_RAM_C068_), a
		ret
	
	; Data from 709E to 70A5 (8 bytes)
	.db $1A $DD $86 $05 $DD $77 $05 $C9
	
; 1st entry of Jump Table from 7075 (indexed by unknown)	
_LABEL_70A6_:	
		ld a, (de)
		ld (ix+2), a
		ret
	
; 2nd entry of Jump Table from 7075 (indexed by unknown)	
_LABEL_70AB_:	
		ld a, (de)
		ld (ix+8), a
		ret
	
; 4th entry of Jump Table from 7075 (indexed by unknown)	
_LABEL_70B0_:	
		ld a, (de)
		or $E0
		push af
		call _LABEL_7151_
		pop af
		or $FC
		inc a
		jr nz, +
		res 6, (ix+0)
		ret
	
+:	
		set 6, (ix+0)
		ret
	
; 5th entry of Jump Table from 7075 (indexed by unknown)	
_LABEL_70C7_:	
		ld a, (de)
		ld (ix+7), a
		ret
	
; 10th entry of Jump Table from 7075 (indexed by unknown)	
_LABEL_70CC_:	
		ld a, (de)
		ld (ix+6), a
		ret
	
; 6th entry of Jump Table from 7075 (indexed by unknown)	
_LABEL_70D1_:	
		ex de, hl
		ld e, (hl)
		inc hl
		ld d, (hl)
		dec de
		ret
	
; 11th entry of Jump Table from 7075 (indexed by unknown)	
_LABEL_70D7_:	
		ld hl, _RAM_C006_
		res 2, (hl)
; 3rd entry of Jump Table from 7075 (indexed by unknown)	
_LABEL_70DC_:	
		xor a
		ld (ix+0), a
		ld (_RAM_C005_), a
		ld a, $E4
		out (Port_PSG), a
		ld hl, _RAM_C026_
		res 2, (hl)
		ld hl, _RAM_C046_
		res 2, (hl)
		ld hl, _RAM_C066_
		res 2, (hl)
		call _LABEL_7142_
		pop hl
		pop hl
		ret
	
; 7th entry of Jump Table from 7075 (indexed by unknown)	
_LABEL_70FC_:	
		ld a, (de)
		ld c, a
		inc de
		ld a, (de)
		ld b, a
		push bc
		push ix
		pop hl
		dec (ix+9)
		ld c, (ix+9)
		dec (ix+9)
		ld b, $00
		add hl, bc
		ld (hl), d
		dec hl
		ld (hl), e
		pop de
		dec de
		ret
	
; 8th entry of Jump Table from 7075 (indexed by unknown)	
_LABEL_7117_:	
		push ix
		pop hl
		ld c, (ix+9)
		ld b, $00
		add hl, bc
		ld e, (hl)
		inc hl
		ld d, (hl)
		inc (ix+9)
		inc (ix+9)
		ret
	
; 9th entry of Jump Table from 7075 (indexed by unknown)	
_LABEL_712A_:	
		ld a, (de)
		inc de
		add a, $17
		ld c, a
		ld b, $00
		push ix
		pop hl
		add hl, bc
		ld a, (hl)
		or a
		jr nz, +
		ld a, (de)
		ld (hl), a
+:	
		inc de
		dec (hl)
		jp nz, _LABEL_70D1_
		inc de
		ret
	
_LABEL_7142_:	
		ld a, (ix+1)
		and $0F
		ld c, a
		ld b, $00
		ld hl, _DATA_6F53_
		add hl, bc
		ld a, (hl)
		or $0F
_LABEL_7151_:	
		bit 2, (ix+0)
		ret nz
		out (Port_PSG), a
		ret
	
_LABEL_7159_:	
		exx
		ld hl, _RAM_C006_
		ld de, _RAM_C006_ + 1
		ld bc, $00DF
		ld (hl), $00
		ldir
		exx
_LABEL_7168_:	
		exx
		ld hl, _DATA_7178_
		ld c, Port_PSG
		ld b, $04
		otir
		xor a
		ld (_RAM_C005_), a
		exx
		ret
	
; Data from 7178 to 717B (4 bytes)	
_DATA_7178_:	
	.db $9F $BF $DF $FF
	
; Data from 717C to 720D (146 bytes)	
_DATA_717C_:	
	.db $00 $00 $FF $03 $C7 $03 $90 $03 $5D $03 $2D $03 $FF $02 $D4 $02
	.db $AB $02 $85 $02 $61 $02 $3F $02 $1E $02 $00 $02 $E3 $01 $C8 $01
	.db $AF $01 $96 $01 $80 $01 $6A $01 $56 $01 $43 $01 $30 $01 $1F $01
	.db $0F $01 $00 $01 $F2 $00 $E4 $00 $D7 $00 $CB $00 $C0 $00 $B5 $00
	.db $AB $00 $A1 $00 $98 $00 $90 $00 $88 $00 $80 $00 $79 $00 $72 $00
	.db $6C $00 $66 $00 $60 $00 $5B $00 $55 $00 $51 $00 $4C $00 $48 $00
	.db $44 $00 $40 $00 $3C $00 $39 $00 $36 $00 $33 $00 $30 $00 $2D $00
	.db $2B $00 $28 $00 $26 $00 $24 $00 $22 $00 $20 $00 $1E $00 $1C $00
	.db $1B $00 $19 $00 $18 $00 $16 $00 $15 $00 $14 $00 $13 $00 $12 $00
	.db $11 $00
	
_LABEL_720E_:	
		ld d, $00
		ld l, d
		ld b, $08
-:	
		add hl, hl
		jr nc, +
		add hl, de
+:	
		djnz -
		ret
	
_LABEL_721A_:	
		ld b, $08
-:	
		adc hl, hl
		ld a, h
		jr c, +
		cp e
		jr c, ++
+:	
		sub e
		ld h, a
		or a
++:	
		djnz -
		ld a, l
		rla
		cpl
		ret
	
; 1st entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 722D to 7230 (4 bytes)	
_DATA_722D_:	
	.db $04 $80 $20 $04
	
	; Pointer Table from 7231 to 7232 (1 entries, indexed by unknown)
	.dw _DATA_7252_
	
	; Data from 7233 to 7239 (7 bytes)
	.db $FA $01 $09 $0D $80 $21 $04
	
	; Pointer Table from 723A to 723B (1 entries, indexed by unknown)
	.dw _DATA_72CF_
	
	; Data from 723C to 7251 (22 bytes)
	.db $FA $00 $0B $0B $80 $22 $04 $52 $72 $FA $00 $0A $0B $80 $23 $04
	.db $6F $73 $00 $01 $01 $0D
	
; 1st entry of Pointer Table from 7231 (indexed by unknown)	
; Data from 7252 to 72CE (125 bytes)	
_DATA_7252_:	
	.db $E0 $01 $A5 $08 $A2 $10 $9E $08 $9D $10 $9B $E0 $04 $EB $E4 $0A
	.db $AA $04 $A7 $EB $AA $04 $A7 $02 $AA $04 $AA $02 $EB $A7 $04 $AA
	.db $A7 $EB $A9 $04 $A5 $EB $A9 $04 $A5 $02 $A9 $04 $A9 $02 $EB $A5
	.db $04 $A9 $A5 $E8 $00 $04 $5F $72 $EB $9B $04 $98 $EB $9B $04 $98
	.db $02 $9B $04 $9B $02 $EB $98 $04 $9B $98 $EB $9D $04 $99 $EB $9D
	.db $04 $99 $02 $9D $04 $9D $02 $EB $99 $04 $9D $99 $EB $9B $04 $98
	.db $EB $9B $04 $98 $02 $9B $04 $9B $02 $EB $98 $04 $9B $98 $EB $9D
	.db $04 $8D $EB $9D $04 $99 $02 $9D $80 $10 $E5 $5F $72
	
; 1st entry of Pointer Table from 723A (indexed by unknown)	
; Data from 72CF to 7393 (197 bytes)	
_DATA_72CF_:	
	.db $80 $10 $91 $02 $92 $92 $92 $92 $02 $92 $92 $92 $91 $92 $92 $02
	.db $92 $92 $92 $92 $92 $8C $02 $8D $8D $8D $8D $02 $8D $8D $8D $8C
	.db $8D $8D $02 $8D $8D $8D $8F $91 $E8 $00 $03 $D1 $72 $91 $02 $92
	.db $92 $92 $92 $02 $92 $92 $92 $91 $92 $92 $02 $92 $92 $92 $92 $92
	.db $8C $02 $8D $8D $8D $8D $02 $8D $8D $8D $8C $8D $8D $02 $8D $8D
	.db $8D $8C $8A $94 $02 $94 $94 $94 $8C $02 $8C $92 $92 $92 $92 $92
	.db $02 $92 $91 $91 $8F $8F $8D $02 $8D $8D $8D $91 $02 $91 $94 $94
	.db $94 $94 $94 $02 $94 $92 $92 $91 $91 $94 $02 $94 $94 $94 $8C $02
	.db $8C $92 $92 $92 $92 $92 $02 $92 $91 $91 $8F $8F $8D $02 $8D $8D
	.db $8D $8D $02 $8D $8D $8D $80 $04 $04 $04 $8D $02 $8D $E5 $D1 $72
	.db $E3 $04 $80 $10 $E1 $0A $E4 $01 $8D $04 $04 $E1 $0D $E4 $02 $04
	.db $E1 $0A $E4 $01 $04 $04 $02 $02 $E1 $0D $E4 $02 $04 $E1 $0A $E4
	.db $01 $04 $E5 $77 $73
	
; 2nd entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7394 to 7397 (4 bytes)	
_DATA_7394_:	
	.db $04 $80 $20 $04
	
	; Pointer Table from 7398 to 7399 (1 entries, indexed by unknown)
	.dw _DATA_73B9_
	
	; Data from 739A to 73A0 (7 bytes)
	.db $FD $01 $0A $0D $80 $21 $04
	
	; Pointer Table from 73A1 to 73A4 (2 entries, indexed by unknown)
	.dw _DATA_7405_ _DATA_3FD_
	
	; Data from 73A5 to 73B8 (20 bytes)
	.db $09 $0B $80 $22 $04 $B9 $73 $F6 $03 $0A $0A $80 $23 $04 $41 $74
	.db $00 $00 $00 $00
	
; 1st entry of Pointer Table from 7398 (indexed by unknown)	
; Data from 73B9 to 7404 (76 bytes)	
_DATA_73B9_:	
	.db $98 $02 $99 $98 $99 $98 $99 $98 $99 $98 $99 $98 $99 $98 $99 $98
	.db $99 $EB $9E $06 $98 $04 $EB $99 $04 $9B $02 $E8 $00 $04 $CA $73
	.db $EB $96 $08 $80 $96 $EB $80 $08 $96 $EB $80 $08 $96 $80 $E8 $01
	.db $02 $CA $73 $EB $98 $04 $98 $EB $99 $04 $99 $98 $EB $99 $02 $98
	.db $06 $99 $04 $E8 $00 $04 $EC $73 $E5 $CA $73 $E2
	
; 1st entry of Pointer Table from 73A1 (indexed by unknown)	
; Data from 7405 to 74A5 (161 bytes)	
_DATA_7405_:	
	.db $80 $20 $80 $0A $06 $E8 $00 $04 $07 $74 $E6 $30 $74 $98 $0A $80
	.db $06 $E8 $00 $04 $12 $74 $E6 $30 $74 $80 $08 $0C $0C $08 $0C $0C
	.db $9D $02 $9E $E8 $00 $10 $25 $74 $E5 $07 $74 $95 $02 $96 $96 $96
	.db $96 $02 $96 $02 $96 $96 $E8 $00 $04 $30 $74 $E7 $E3 $04 $E6 $6E
	.db $74 $E8 $00 $02 $43 $74 $E6 $80 $74 $E8 $00 $04 $4B $74 $E6 $6E
	.db $74 $E8 $00 $04 $53 $74 $E8 $01 $02 $4B $74 $E6 $6E $74 $E6 $8D
	.db $74 $E8 $00 $04 $60 $74 $E5 $4B $74 $E1 $0A $E4 $01 $8D $04 $04
	.db $E1 $0D $E4 $02 $04 $E1 $0A $E4 $01 $04 $E7 $E1 $0D $E4 $02 $06
	.db $E1 $0A $E4 $01 $04 $04 $02 $E7 $E1 $0A $E4 $01 $02 $E1 $0D $E4
	.db $02 $02 $E1 $0A $E4 $01 $04 $E1 $0D $E4 $02 $8D $04 $E4 $08 $04
	.db $E7
	
; 3rd entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 74A6 to 74A9 (4 bytes)	
_DATA_74A6_:	
	.db $04 $80 $20 $05
	
	; Pointer Table from 74AA to 74AB (1 entries, indexed by unknown)
	.dw _DATA_74CB_
	
	; Data from 74AC to 74B2 (7 bytes)
	.db $F6 $01 $0B $0D $80 $21 $05
	
	; Pointer Table from 74B3 to 74B4 (1 entries, indexed by unknown)
	.dw _DATA_74EE_
	
	; Data from 74B5 to 74CA (22 bytes)
	.db $F6 $01 $0B $0B $80 $22 $05 $0F $75 $F6 $04 $0B $0B $80 $23 $05
	.db $2E $75 $00 $00 $05 $0D
	
; 1st entry of Pointer Table from 74AA (indexed by unknown)	
; Data from 74CB to 74ED (35 bytes)	
_DATA_74CB_:	
	.db $80 $04 $A9 $A8 $A9 $A6 $02 $A2 $04 $80 $02 $AB $A9 $04 $80 $02
	.db $E8 $00 $03 $CB $74 $98 $02 $9F $A8 $A4 $AB $A8 $A4 $9F $E4 $0C
	.db $AD $0C $E2
	
; 1st entry of Pointer Table from 74B3 (indexed by unknown)	
; Data from 74EE to 7545 (88 bytes)	
_DATA_74EE_:	
	.db $9D $02 $02 $02 $02 $02 $02 $02 $02 $96 $02 $02 $02 $02 $02 $02
	.db $02 $02 $E8 $00 $03 $EE $74 $E4 $0A $96 $08 $93 $E4 $0C $9D $0C
	.db $E2 $80 $04 $A4 $A4 $A4 $A9 $02 $A6 $04 $80 $02 $A6 $02 $04 $80
	.db $02 $E8 $00 $03 $0F $75 $E4 $0A $9F $08 $98 $E4 $0C $A4 $0C $E2
	.db $E3 $04 $E1 $0A $E4 $01 $8D $02 $02 $E1 $0D $E4 $02 $02 $E4 $08
	.db $02 $E8 $00 $0E $30 $75 $08 $E2
	
; 4th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7546 to 7549 (4 bytes)	
_DATA_7546_:	
	.db $03 $80 $20 $04
	
	; Pointer Table from 754A to 754B (1 entries, indexed by unknown)
	.dw _DATA_7562_
	
	; Data from 754C to 7552 (7 bytes)
	.db $03 $01 $03 $0E $80 $21 $04
	
	; Pointer Table from 7553 to 7554 (1 entries, indexed by unknown)
	.dw _DATA_756D_
	
	; Data from 7555 to 7561 (13 bytes)
	.db $03 $01 $03 $0E $80 $22 $04 $79 $75 $03 $01 $03 $0D
	
; 1st entry of Pointer Table from 754A (indexed by unknown)	
; Data from 7562 to 756C (11 bytes)	
_DATA_7562_:	
	.db $9D $04 $02 $99 $9B $9D $99 $80 $04 $A5 $E2
	
; 1st entry of Pointer Table from 7553 (indexed by unknown)	
; Data from 756D to 7583 (23 bytes)	
_DATA_756D_:	
	.db $99 $04 $02 $96 $02 $98 $99 $96 $80 $04 $A2 $E2 $8D $04 $02 $8C
	.db $8A $88 $8D $80 $04 $91 $E2
	
; 5th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7584 to 7587 (4 bytes)	
_DATA_7584_:	
	.db $04 $80 $20 $02
	
	; Pointer Table from 7588 to 7589 (1 entries, indexed by unknown)
	.dw _DATA_75A9_
	
	; Data from 758A to 7590 (7 bytes)
	.db $F6 $01 $0A $0D $80 $21 $02
	
	; Pointer Table from 7591 to 7592 (1 entries, indexed by unknown)
	.dw _DATA_7613_
	
	; Data from 7593 to 75A8 (22 bytes)
	.db $F6 $01 $0A $0B $80 $22 $02 $78 $76 $F6 $00 $0A $0C $80 $23 $02
	.db $B9 $76 $00 $00 $05 $0D
	
; 1st entry of Pointer Table from 7588 (indexed by unknown)	
; Data from 75A9 to 7612 (106 bytes)	
_DATA_75A9_:	
	.db $A7 $12 $AE $03 $AB $A7 $18 $AC $03 $80 $AB $80 $A9 $A2 $80 $A2
	.db $18 $A9 $03 $AE $30 $B1 $0C $B0 $06 $AC $1E $AB $03 $80 $A7 $80
	.db $AC $80 $A7 $AE $80 $A7 $AC $A7 $AB $80 $A7 $80 $AB $0C $A7 $06
	.db $A4 $0C $A2 $12 $AB $03 $80 $A7 $80 $AC $80 $A7 $AE $80 $A7 $AC
	.db $A7 $AB $80 $A7 $80 $A9 $0C $AB $06 $AC $0C $B3 $06 $B2 $0C $E8
	.db $00 $02 $A9 $75 $B5 $30 $AC $03 $AB $AC $AB $AC $AB $A9 $A7 $A9
	.db $A2 $80 $A6 $80 $A7 $80 $A9 $E5 $A9 $75
	
; 1st entry of Pointer Table from 7591 (indexed by unknown)	
; Data from 7613 to 76E4 (210 bytes)	
_DATA_7613_:	
	.db $9B $08 $A2 $04 $0C $9B $08 $A2 $04 $0C $96 $08 $9D $04 $0C $96
	.db $08 $9D $04 $0C $E8 $01 $02 $13 $76 $94 $08 $9B $04 $0C $94 $08
	.db $9B $04 $08 $94 $04 $9B $08 $A2 $04 $0C $9B $08 $A2 $04 $08 $9B
	.db $04 $99 $08 $A0 $04 $0C $99 $08 $A0 $04 $08 $99 $04 $96 $08 $9D
	.db $04 $0C $96 $08 $A2 $04 $08 $96 $04 $E8 $00 $02 $13 $76 $96 $08
	.db $9D $04 $0C $96 $08 $9D $04 $0C $94 $08 $9B $04 $0C $96 $08 $9D
	.db $04 $0C $E5 $13 $76 $AB $30 $A6 $AB $AE $0C $AC $06 $A9 $1E $AB
	.db $03 $80 $A7 $80 $AC $80 $A7 $AE $80 $A7 $AC $A7 $AB $80 $A7 $80
	.db $A7 $30 $AE $03 $80 $AB $80 $B0 $80 $AC $B1 $80 $AE $B0 $AC $AE
	.db $80 $AB $80 $AE $1E $B0 $06 $AE $0C $E8 $00 $02 $78 $76 $B2 $30
	.db $A7 $18 $A9 $E5 $78 $76 $E3 $04 $E1 $0A $E4 $01 $8D $03 $03 $E1
	.db $0D $E4 $08 $06 $E4 $02 $06 $E1 $0A $E4 $01 $03 $03 $E8 $00 $23
	.db $BB $76 $80 $03 $E1 $0D $E4 $08 $8D $03 $E8 $00 $04 $D5 $76 $E5
	.db $BB $76
	
; 6th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 76E5 to 76E8 (4 bytes)	
_DATA_76E5_:	
	.db $04 $80 $20 $03
	
	; Pointer Table from 76E9 to 76EA (1 entries, indexed by unknown)
	.dw _DATA_770A_
	
	; Data from 76EB to 76F1 (7 bytes)
	.db $FE $01 $0A $0D $80 $21 $03
	
	; Pointer Table from 76F2 to 76F3 (1 entries, indexed by unknown)
	.dw _DATA_772F_
	
	; Data from 76F4 to 7709 (22 bytes)
	.db $FE $01 $0A $0B $80 $22 $03 $5E $77 $FE $01 $0A $09 $80 $23 $03
	.db $63 $77 $03 $01 $09 $0B
	
; 1st entry of Pointer Table from 76E9 (indexed by unknown)	
; Data from 770A to 772E (37 bytes)	
_DATA_770A_:	
	.db $80 $08 $A0 $06 $A0 $02 $80 $A0 $80 $04 $A0 $A0 $E8 $00 $02 $0A
	.db $77 $80 $08 $A2 $06 $A2 $02 $80 $A2 $80 $04 $A2 $A2 $E8 $00 $02
	.db $1B $77 $E5 $0A $77
	
; 1st entry of Pointer Table from 76F2 (indexed by unknown)	
; Data from 772F to 7775 (71 bytes)	
_DATA_772F_:	
	.db $8D $04 $8D $9D $06 $9D $02 $8D $02 $9D $02 $8D $04 $9D $04 $9D
	.db $04 $E8 $00 $02 $2F $77 $8D $04 $8D $9E $06 $9E $02 $8D $02 $9E
	.db $02 $8D $04 $9E $04 $9E $04 $E8 $00 $02 $45 $77 $E5 $2F $77 $80
	.db $01 $E5 $0A $77 $E3 $04 $E1 $0D $E4 $01 $8D $04 $04 $06 $02 $02
	.db $02 $04 $04 $04 $E5 $63 $77
	
; 7th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7776 to 7779 (4 bytes)	
_DATA_7776_:	
	.db $04 $80 $20 $04
	
	; Pointer Table from 777A to 777F (3 entries, indexed by unknown)
	.dw _DATA_779B_ _DATA_3FE_ _DATA_D0A_
	
	; Data from 7780 to 7782 (3 bytes)
	.db $80 $21 $04
	
	; Pointer Table from 7783 to 7784 (1 entries, indexed by unknown)
	.dw _DATA_77A3_
	
	; Data from 7785 to 779A (22 bytes)
	.db $FE $04 $0A $0B $80 $22 $04 $BE $77 $FE $04 $0A $0B $80 $23 $04
	.db $D3 $77 $00 $00 $00 $00
	
; 1st entry of Pointer Table from 777A (indexed by unknown)	
; Data from 779B to 77A2 (8 bytes)	
_DATA_779B_:	
	.db $9D $04 $9B $98 $99 $E5 $9B $77
	
; 1st entry of Pointer Table from 7783 (indexed by unknown)	
; Data from 77A3 to 77ED (75 bytes)	
_DATA_77A3_:	
	.db $92 $10 $91 $92 $0E $91 $01 $8F $91 $0C $8D $04 $8F $0E $91 $01
	.db $8F $91 $0C $8F $04 $92 $10 $91 $E5 $B8 $77 $86 $10 $85 $86 $10
	.db $85 $0C $81 $04 $83 $10 $85 $0C $83 $04 $86 $10 $85 $E5 $CD $77
	.db $E3 $04 $E1 $0A $E4 $01 $8D $08 $E1 $0D $E4 $02 $08 $E1 $0A $E4
	.db $01 $06 $02 $E1 $0D $E4 $02 $08 $E5 $D5 $77
	
; 8th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 77EE to 77F1 (4 bytes)	
_DATA_77EE_:	
	.db $04 $80 $20 $06
	
	; Pointer Table from 77F2 to 77F3 (1 entries, indexed by unknown)
	.dw _DATA_7813_
	
	; Data from 77F4 to 77FA (7 bytes)
	.db $F6 $04 $0A $0D $80 $21 $06
	
	; Pointer Table from 77FB to 77FC (1 entries, indexed by unknown)
	.dw _DATA_7844_
	
	; Data from 77FD to 7812 (22 bytes)
	.db $F6 $01 $0D $0D $80 $22 $06 $67 $78 $F6 $04 $0A $08 $80 $23 $06
	.db $9A $78 $00 $00 $05 $0D
	
; 1st entry of Pointer Table from 77F2 (indexed by unknown)	
; Data from 7813 to 7843 (49 bytes)	
_DATA_7813_:	
	.db $80 $0A $A9 $02 $AE $B1 $B0 $08 $AE $02 $AC $04 $A9 $02 $AE $04
	.db $A5 $0E $9F $02 $A5 $A9 $1A $AA $02 $A9 $A7 $AA $A9 $A7 $AA $A9
	.db $A7 $AA $A9 $A7 $AC $AA $A9 $A7 $A9 $A7 $A9 $AA $A9 $0A $E5 $15
	.db $78
	
; 1st entry of Pointer Table from 77FB (indexed by unknown)	
; Data from 7844 to 78C5 (130 bytes)	
_DATA_7844_:	
	.db $96 $02 $96 $A2 $9D $A0 $A2 $9B $9D $99 $9B $E8 $00 $04 $44 $78
	.db $92 $92 $9E $99 $9C $9E $04 $99 $02 $9C $9E $E8 $00 $02 $54 $78
	.db $E5 $44 $78 $80 $0A $80 $02 $A9 $02 $AE $B1 $B0 $08 $AE $02 $AC
	.db $04 $A9 $02 $AE $04 $A5 $0E $9F $02 $A3 $A6 $1A $AA $02 $A9 $A7
	.db $AA $A9 $A7 $AA $A9 $A7 $AA $A9 $A7 $AC $AA $A9 $A7 $A9 $A7 $A9
	.db $AA $A9 $0A $E5 $69 $78 $E3 $04 $E1 $0A $E4 $01 $8D $02 $02 $E1
	.db $0D $E4 $02 $02 $E1 $0A $E4 $01 $02 $E1 $0D $E4 $02 $02 $E1 $0A
	.db $E4 $01 $02 $02 $02 $E1 $0D $E4 $02 $02 $E1 $0A $E4 $01 $02 $E5
	.db $9C $78
	
; 9th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 78C6 to 78C9 (4 bytes)	
_DATA_78C6_:	
	.db $04 $80 $20 $03
	
	; Pointer Table from 78CA to 78CB (1 entries, indexed by unknown)
	.dw _DATA_78EB_
	
	; Data from 78CC to 78D2 (7 bytes)
	.db $F8 $02 $0A $0D $80 $21 $03
	
	; Pointer Table from 78D3 to 78D4 (1 entries, indexed by unknown)
	.dw _DATA_7925_
	
	; Data from 78D5 to 78EA (22 bytes)
	.db $F8 $02 $0A $0C $80 $22 $03 $5F $79 $F8 $01 $0B $0C $80 $23 $03
	.db $9D $79 $03 $01 $01 $0E
	
; 1st entry of Pointer Table from 78CA (indexed by unknown)	
; Data from 78EB to 7924 (58 bytes)	
_DATA_78EB_:	
	.db $AA $06 $AA $02 $A9 $08 $80 $10 $AA $06 $AA $02 $E8 $00 $02 $EF
	.db $78 $A9 $08 $AA $06 $AA $02 $A9 $08 $AA $06 $AA $02 $A7 $08 $80
	.db $10 $A9 $06 $A9 $02 $E8 $00 $03 $08 $79 $A7 $08 $A7 $04 $A7 $AA
	.db $04 $A9 $08 $A7 $04 $A5 $08 $80 $18 $E2
	
; 1st entry of Pointer Table from 78D3 (indexed by unknown)	
; Data from 7925 to 79FA (214 bytes)	
_DATA_7925_:	
	.db $A7 $06 $A7 $02 $A5 $08 $80 $10 $A7 $06 $A7 $02 $E8 $00 $02 $29
	.db $79 $A5 $08 $A7 $06 $A7 $02 $A5 $08 $A7 $06 $A7 $02 $A4 $08 $80
	.db $10 $A5 $06 $A5 $02 $E8 $00 $03 $42 $79 $A4 $08 $A4 $04 $04 $A7
	.db $04 $A5 $08 $A4 $04 $A0 $08 $80 $18 $E2 $80 $10 $94 $08 $99 $08
	.db $E8 $00 $02 $5F $79 $80 $08 $99 $08 $80 $08 $99 $08 $80 $08 $98
	.db $08 $94 $08 $9B $08 $80 $08 $80 $08 $94 $08 $9B $08 $80 $08 $80
	.db $08 $94 $08 $9B $08 $80 $08 $9B $08 $80 $08 $9B $08 $80 $08 $80
	.db $08 $94 $08 $99 $08 $80 $08 $E2 $E3 $04 $E1 $0A $E4 $01 $8D $06
	.db $02 $E1 $0B $E4 $02 $08 $08 $08 $E1 $0A $E4 $01 $06 $02 $E1 $0B
	.db $E4 $02 $08 $08 $08 $E1 $0A $E4 $01 $06 $02 $08 $06 $02 $08 $06
	.db $02 $08 $E1 $0B $E4 $02 $08 $08 $E1 $0A $E4 $01 $06 $02 $E1 $0B
	.db $E4 $02 $08 $08 $08 $E1 $0A $E4 $01 $06 $02 $E1 $0B $E4 $02 $08
	.db $08 $08 $E1 $0A $E4 $01 $06 $02 $08 $04 $04 $04 $08 $04 $08 $E1
	.db $0B $E4 $02 $08 $18 $E2
	
; 10th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 79FB to 79FE (4 bytes)	
_DATA_79FB_:	
	.db $02 $80 $21 $03
	
	; Pointer Table from 79FF to 7A00 (1 entries, indexed by unknown)
	.dw _DATA_7A0E_
	
	; Data from 7A01 to 7A07 (7 bytes)
	.db $FB $01 $05 $0E $80 $22 $03
	
	; Pointer Table from 7A08 to 7A09 (1 entries, indexed by unknown)
	.dw _DATA_7A0E_
	
	; Data from 7A0A to 7A0D (4 bytes)
	.db $FB $00 $09 $0E
	
; 1st entry of Pointer Table from 79FF (indexed by unknown)	
; Data from 7A0E to 7A17 (10 bytes)	
_DATA_7A0E_:	
	.db $AE $04 $B2 $B5 $B0 $B3 $B7 $BA $10 $E2
	
; 11th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7A18 to 7A1B (4 bytes)	
_DATA_7A18_:	
	.db $04 $80 $20 $02
	
	; Pointer Table from 7A1C to 7A1D (1 entries, indexed by unknown)
	.dw _DATA_7A3D_
	
	; Data from 7A1E to 7A24 (7 bytes)
	.db $F6 $01 $0A $0D $80 $21 $02
	
	; Pointer Table from 7A25 to 7A26 (1 entries, indexed by unknown)
	.dw _DATA_7B52_
	
	; Data from 7A27 to 7A3C (22 bytes)
	.db $F6 $01 $0A $0B $80 $22 $02 $CF $7A $F6 $01 $0B $0C $80 $23 $02
	.db $88 $7B $03 $01 $01 $0D
	
; 1st entry of Pointer Table from 7A1C (indexed by unknown)	
; Data from 7A3D to 7B51 (277 bytes)	
_DATA_7A3D_:	
	.db $A2 $04 $A7 $AB $AC $AB $A7 $AB $08 $AC $AE $E4 $0B $A4 $04 $A4
	.db $A9 $B0 $B0 $A9 $E4 $0A $A4 $18 $E4 $0B $A2 $04 $A2 $A7 $AE $AE
	.db $A7 $AD $AE $AD $E4 $0A $A9 $0C $E8 $00 $02 $48 $7A $E4 $0B $A7
	.db $04 $A9 $AB $A5 $A7 $A9 $A4 $A5 $A7 $A2 $A4 $A5 $E8 $00 $03 $6A
	.db $7A $A7 $A9 $AB $A5 $A7 $A9 $E4 $0A $AE $18 $A4 $12 $03 $A9 $B0
	.db $18 $AE $08 $AD $04 $80 $06 $AE $03 $AD $A9 $18 $A7 $04 $80 $A9
	.db $80 $AB $08 $AC $04 $80 $AB $80 $A9 $08 $AE $04 $80 $AC $80 $AB
	.db $08 $A5 $18 $A4 $12 $03 $A9 $B0 $18 $B2 $08 $B0 $04 $80 $06 $AE
	.db $A9 $18 $AC $08 $AB $AC $AE $AC $AB $04 $AC $B0 $24 $80 $0C $E5
	.db $48 $7A $80 $01 $A2 $04 $A7 $AB $AC $AB $A7 $AB $08 $AC $AE $07
	.db $9D $08 $04 $A4 $0C $9D $08 $04 $9D $0C $9B $08 $04 $A2 $0C $9B
	.db $08 $04 $A2 $0C $E8 $00 $02 $DD $7A $99 $08 $04 $A0 $0C $A0 $08
	.db $04 $A0 $9F $9D $E8 $00 $03 $F6 $7A $99 $08 $04 $A0 $0C $9B $08
	.db $04 $A2 $0C $E6 $30 $7B $9B $08 $04 $A2 $0C $9B $08 $04 $A2 $9B
	.db $9B $E6 $30 $7B $98 $0C $04 $99 $9B $E4 $0A $A2 $0C $80 $E4 $0B
	.db $E5 $DD $7A $9D $08 $04 $A4 $0C $9D $08 $04 $A4 $9D $9D $9B $08
	.db $04 $A2 $0C $9B $08 $04 $A2 $9B $9B $99 $08 $04 $A0 $0C $99 $08
	.db $04 $A0 $99 $99 $E7
	
; 1st entry of Pointer Table from 7A25 (indexed by unknown)	
; Data from 7B52 to 7BB9 (104 bytes)	
_DATA_7B52_:	
	.db $96 $04 $9B $9F $A0 $9F $9B $9F $08 $A0 $A2 $9D $30 $9B $9D $9B
	.db $99 $99 $AB $04 $AC $AE $A9 $AB $AC $A7 $A9 $AB $A5 $A7 $A9 $AB
	.db $AC $AE $A9 $AB $AC $A7 $18 $9D $30 $9B $99 $9B $9D $9B $99 $A4
	.db $24 $80 $0C $E5 $5D $7B $E3 $04 $E1 $0A $E4 $01 $8D $0C $0C $0C
	.db $08 $04 $E1 $0A $E4 $01 $8D $04 $04 $04 $E1 $0D $E4 $02 $04 $E1
	.db $0A $E4 $01 $04 $04 $E8 $00 $1E $94 $7B $E1 $0A $E4 $08 $0C $E1
	.db $0A $E4 $01 $0C $18 $E5 $94 $7B
	
; 13th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7BBA to 7BBD (4 bytes)	
_DATA_7BBA_:	
	.db $01 $88 $22 $01
	
	; Pointer Table from 7BBE to 7BBF (1 entries, indexed by unknown)
	.dw _DATA_7BC4_
	
	; Data from 7BC0 to 7BC3 (4 bytes)
	.db $00 $00 $01 $0F
	
; 1st entry of Pointer Table from 7BBE (indexed by unknown)	
; Data from 7BC4 to 7BD3 (16 bytes)	
_DATA_7BC4_:	
	.db $00 $F0 $02 $02 $50 $02 $00 $00 $01 $00 $80 $02 $01 $00 $02 $E2
	
; 14th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7BD4 to 7BD7 (4 bytes)	
_DATA_7BD4_:	
	.db $01 $A8 $22 $01
	
	; Pointer Table from 7BD8 to 7BD9 (1 entries, indexed by unknown)
	.dw _DATA_7BDE_
	
	; Data from 7BDA to 7BDD (4 bytes)
	.db $00 $00 $06 $0F
	
; 1st entry of Pointer Table from 7BD8 (indexed by unknown)	
; Data from 7BDE to 7BED (16 bytes)	
_DATA_7BDE_:	
	.db $01 $00 $00 $80 $03 $00 $00 $00 $00 $01 $00 $F0 $00 $20 $0F $E2
	
; 15th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7BEE to 7BF1 (4 bytes)	
_DATA_7BEE_:	
	.db $01 $A8 $22 $01
	
	; Pointer Table from 7BF2 to 7BF3 (1 entries, indexed by unknown)
	.dw _DATA_7BF8_
	
	; Data from 7BF4 to 7BF7 (4 bytes)
	.db $00 $00 $00 $0F
	
; 1st entry of Pointer Table from 7BF2 (indexed by unknown)	
; Data from 7BF8 to 7C07 (16 bytes)	
_DATA_7BF8_:	
	.db $00 $F0 $01 $50 $04 $00 $00 $00 $00 $01 $00 $80 $00 $90 $04 $E2
	
; 16th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7C08 to 7C0B (4 bytes)	
_DATA_7C08_:	
	.db $01 $A8 $22 $01
	
	; Pointer Table from 7C0C to 7C0D (1 entries, indexed by unknown)
	.dw _DATA_7C12_
	
	; Data from 7C0E to 7C11 (4 bytes)
	.db $00 $00 $00 $0F
	
; 1st entry of Pointer Table from 7C0C (indexed by unknown)	
; Data from 7C12 to 7C1C (11 bytes)	
_DATA_7C12_:	
	.db $00 $80 $02 $00 $02 $01 $00 $02 $50 $06 $E2
	
; 17th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7C1D to 7C20 (4 bytes)	
_DATA_7C1D_:	
	.db $01 $A8 $22 $01
	
	; Pointer Table from 7C21 to 7C22 (1 entries, indexed by unknown)
	.dw _DATA_7C27_
	
	; Data from 7C23 to 7C26 (4 bytes)
	.db $00 $00 $00 $0E
	
; 1st entry of Pointer Table from 7C21 (indexed by unknown)	
; Data from 7C27 to 7C29 (3 bytes)	
_DATA_7C27_:	
	.db $01 $30 $00
	
	; Pointer Table from 7C2A to 7C2B (1 entries, indexed by unknown)
	.dw $0740
	
	; Data from 7C2C to 7C31 (6 bytes)
	.db $01 $40 $02 $30 $06 $E2
	
; 18th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7C32 to 7C35 (4 bytes)	
_DATA_7C32_:	
	.db $01 $88 $22 $01
	
	; Pointer Table from 7C36 to 7C37 (1 entries, indexed by unknown)
	.dw _DATA_7C3C_
	
	; Data from 7C38 to 7C3B (4 bytes)
	.db $03 $01 $01 $0E
	
; 1st entry of Pointer Table from 7C36 (indexed by unknown)	
; Data from 7C3C to 7C3E (3 bytes)	
_DATA_7C3C_:	
	.db $00 $80 $01
	
	; Pointer Table from 7C3F to 7C40 (1 entries, indexed by unknown)
	.dw _RAM_F000_
	
	; Data from 7C41 to 7C48 (8 bytes)
	.db $02 $00 $50 $01 $02 $00 $03 $E2
	
; 19th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7C49 to 7C4C (4 bytes)	
_DATA_7C49_:	
	.db $01 $88 $22 $01
	
	; Pointer Table from 7C4D to 7C4E (1 entries, indexed by unknown)
	.dw _DATA_7C53_
	
	; Data from 7C4F to 7C52 (4 bytes)
	.db $00 $00 $02 $0F
	
; 1st entry of Pointer Table from 7C4D (indexed by unknown)	
; Data from 7C53 to 7C5F (13 bytes)	
_DATA_7C53_:	
	.db $02 $20 $02 $03 $00 $03 $00 $80 $01 $02 $60 $04 $E2
	
; 20th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7C60 to 7C63 (4 bytes)	
_DATA_7C60_:	
	.db $02 $80 $21 $03
	
	; Pointer Table from 7C64 to 7C65 (1 entries, indexed by unknown)
	.dw _DATA_7C73_
	
	; Data from 7C66 to 7C6C (7 bytes)
	.db $03 $01 $04 $0E $80 $22 $03
	
	; Pointer Table from 7C6D to 7C6E (1 entries, indexed by unknown)
	.dw _DATA_7C73_
	
	; Data from 7C6F to 7C72 (4 bytes)
	.db $00 $01 $04 $0D
	
; 1st entry of Pointer Table from 7C64 (indexed by unknown)	
; Data from 7C73 to 7C7C (10 bytes)	
_DATA_7C73_:	
	.db $99 $02 $9D $99 $A0 $9F $A0 $A5 $0A $E2
	
; 21st entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7C7D to 7C80 (4 bytes)	
_DATA_7C7D_:	
	.db $01 $A8 $23 $01
	
	; Pointer Table from 7C81 to 7C82 (1 entries, indexed by unknown)
	.dw _DATA_7C87_
	
	; Data from 7C83 to 7C86 (4 bytes)
	.db $00 $00 $07 $0F
	
; 1st entry of Pointer Table from 7C81 (indexed by unknown)	
; Data from 7C87 to 7C98 (18 bytes)	
_DATA_7C87_:	
	.db $E3 $07 $00 $0A $00 $0E $02 $00 $00 $00 $00 $02 $00 $0A $00 $40
	.db $07 $E2
	
; 22nd entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7C99 to 7C9C (4 bytes)	
_DATA_7C99_:	
	.db $01 $A8 $22 $01
	
	; Pointer Table from 7C9D to 7C9E (1 entries, indexed by unknown)
	.dw _DATA_7CA3_
	
	; Data from 7C9F to 7CA2 (4 bytes)
	.db $00 $00 $00 $0D
	
; 1st entry of Pointer Table from 7C9D (indexed by unknown)	
; Data from 7CA3 to 7CBC (26 bytes)	
_DATA_7CA3_:	
	.db $02 $30 $02 $60 $02 $00 $00 $00 $00 $01 $01 $A0 $00 $20 $08 $E8
	.db $00 $03 $AD $7C $01 $10 $00 $80 $0B $E2
	
; 23rd entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7CBD to 7CC0 (4 bytes)	
_DATA_7CBD_:	
	.db $01 $80 $22 $03
	
	; Pointer Table from 7CC1 to 7CC2 (1 entries, indexed by unknown)
	.dw _DATA_7CC7_
	
	; Data from 7CC3 to 7CC6 (4 bytes)
	.db $03 $00 $03 $0F
	
; 1st entry of Pointer Table from 7CC1 (indexed by unknown)	
; Data from 7CC7 to 7CC9 (3 bytes)	
_DATA_7CC7_:	
	.db $99 $02 $9D
	
	; Pointer Table from 7CCA to 7CCD (2 entries, indexed by unknown)
	.dw _DATA_A5A0_ _RAM_E206_
	
; 24th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7CCE to 7CD1 (4 bytes)	
_DATA_7CCE_:	
	.db $01 $88 $22 $01
	
	; Pointer Table from 7CD2 to 7CD3 (1 entries, indexed by unknown)
	.dw _DATA_7CD8_
	
	; Data from 7CD4 to 7CD7 (4 bytes)
	.db $03 $01 $00 $0D
	
; 1st entry of Pointer Table from 7CD2 (indexed by unknown)	
; Data from 7CD8 to 7CDA (3 bytes)	
_DATA_7CD8_:	
	.db $00 $80 $03
	
	; Pointer Table from 7CDB to 7CDC (1 entries, indexed by unknown)
	.dw _DATA_3E2_
	
	; Data from 7CDD to 7CDF (3 bytes)
	.db $88 $20 $01
	
	; Pointer Table from 7CE0 to 7CE1 (1 entries, indexed by unknown)
	.dw _DATA_7CF8_
	
	; Data from 7CE2 to 7CE8 (7 bytes)
	.db $00 $00 $04 $0D $88 $21 $01
	
	; Pointer Table from 7CE9 to 7CEA (1 entries, indexed by unknown)
	.dw _DATA_7CFF_
	
	; Data from 7CEB to 7CF7 (13 bytes)
	.db $00 $01 $04 $0D $88 $22 $01 $06 $7D $00 $00 $04 $0D
	
; 1st entry of Pointer Table from 7CE0 (indexed by unknown)	
; Data from 7CF8 to 7CFE (7 bytes)	
_DATA_7CF8_:	
	.db $00 $20 $06 $00 $18 $08 $EA
	
; 1st entry of Pointer Table from 7CE9 (indexed by unknown)	
; Data from 7CFF to 7D0C (14 bytes)	
_DATA_7CFF_:	
	.db $00 $30 $06 $00 $14 $08 $EA $00 $40 $06 $00 $30 $08 $EA
	
; 26th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7D0D to 7D10 (4 bytes)	
_DATA_7D0D_:	
	.db $03 $80 $20 $04
	
	; Pointer Table from 7D11 to 7D16 (3 entries, indexed by unknown)
	.dw _DATA_7D29_ _DATA_3FF_ _DATA_D0A_
	
	; Data from 7D17 to 7D19 (3 bytes)
	.db $80 $21 $04
	
	; Pointer Table from 7D1A to 7D1F (3 entries, indexed by unknown)
	.dw _DATA_7D37_ _DATA_3FF_ _DATA_C09_
	
	; Data from 7D20 to 7D28 (9 bytes)
	.db $80 $22 $04 $3E $7D $FF $01 $09 $0B
	
; 1st entry of Pointer Table from 7D11 (indexed by unknown)	
; Data from 7D29 to 7D36 (14 bytes)	
_DATA_7D29_:	
	.db $AA $04 $A7 $A0 $A4 $02 $A5 $80 $08 $B0 $01 $B1 $03 $E2
	
; 1st entry of Pointer Table from 7D1A (indexed by unknown)	
; Data from 7D37 to 7D43 (13 bytes)	
_DATA_7D37_:	
	.db $92 $10 $8D $08 $8D $04 $E2 $80 $01 $E5 $29 $7D $E2
	
; 27th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7D44 to 7D47 (4 bytes)	
_DATA_7D44_:	
	.db $03 $80 $20 $04
	
	; Pointer Table from 7D48 to 7D49 (1 entries, indexed by unknown)
	.dw _DATA_7D60_
	
	; Data from 7D4A to 7D50 (7 bytes)
	.db $03 $01 $04 $0D $80 $21 $04
	
	; Pointer Table from 7D51 to 7D52 (1 entries, indexed by unknown)
	.dw _DATA_7D65_
	
	; Data from 7D53 to 7D5F (13 bytes)
	.db $03 $01 $04 $0D $80 $22 $04 $69 $7D $03 $01 $04 $0D
	
; 1st entry of Pointer Table from 7D48 (indexed by unknown)	
; Data from 7D60 to 7D64 (5 bytes)	
_DATA_7D60_:	
	.db $A0 $08 $94 $10 $EA
	
; 1st entry of Pointer Table from 7D51 (indexed by unknown)	
; Data from 7D65 to 7D6C (8 bytes)	
_DATA_7D65_:	
	.db $9D $08 $10 $E2 $99 $08 $10 $E2
	
; 28th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7D6D to 7D70 (4 bytes)	
_DATA_7D6D_:	
	.db $02 $80 $21 $05
	
	; Pointer Table from 7D71 to 7D72 (1 entries, indexed by unknown)
	.dw _DATA_7D80_
	
	; Data from 7D73 to 7D79 (7 bytes)
	.db $F9 $01 $05 $0F $80 $22 $05
	
	; Pointer Table from 7D7A to 7D7B (1 entries, indexed by unknown)
	.dw _DATA_7D80_
	
	; Data from 7D7C to 7D7F (4 bytes)
	.db $FC $01 $05 $0F
	
; 1st entry of Pointer Table from 7D71 (indexed by unknown)	
; Data from 7D80 to 7D86 (7 bytes)	
_DATA_7D80_:	
	.db $A7 $01 $AC $AB $A7 $AE $E2
	
; 29th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7D87 to 7D8A (4 bytes)	
_DATA_7D87_:	
	.db $01 $A8 $22 $01
	
	; Pointer Table from 7D8B to 7D8C (1 entries, indexed by unknown)
	.dw _DATA_7D91_
	
	; Data from 7D8D to 7D90 (4 bytes)
	.db $00 $01 $0E $0D
	
; 1st entry of Pointer Table from 7D8B (indexed by unknown)	
; Data from 7D91 to 7D9B (11 bytes)	
_DATA_7D91_:	
	.db $00 $E1 $00 $CF $06 $00 $90 $01 $04 $08 $E2
	
; 30th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7D9C to 7D9F (4 bytes)	
_DATA_7D9C_:	
	.db $03 $A0 $20 $04
	
	; Pointer Table from 7DA0 to 7DA1 (1 entries, indexed by unknown)
	.dw _DATA_7DB8_
	
	; Data from 7DA2 to 7DA8 (7 bytes)
	.db $FF $00 $04 $0B $A0 $21 $04
	
	; Pointer Table from 7DA9 to 7DAA (1 entries, indexed by unknown)
	.dw _DATA_7DB8_
	
	; Data from 7DAB to 7DB7 (13 bytes)
	.db $FE $01 $04 $0E $A0 $22 $04 $B8 $7D $FD $00 $04 $0D
	
; 1st entry of Pointer Table from 7DA0 (indexed by unknown)	
; Data from 7DB8 to 7DBB (4 bytes)	
_DATA_7DB8_:	
	.db $99 $B1 $08 $E2
	
; 31st entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7DBC to 7DBF (4 bytes)	
_DATA_7DBC_:	
	.db $01 $A8 $22 $01
	
	; Pointer Table from 7DC0 to 7DC1 (1 entries, indexed by unknown)
	.dw _DATA_7DC6_
	
	; Data from 7DC2 to 7DC5 (4 bytes)
	.db $03 $01 $00 $0D
	
; 1st entry of Pointer Table from 7DC0 (indexed by unknown)	
; Data from 7DC6 to 7DD0 (11 bytes)	
_DATA_7DC6_:	
	.db $00 $60 $00 $30 $01 $00 $40 $00 $80 $04 $E2
	
; 32nd entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7DD1 to 7DD4 (4 bytes)	
_DATA_7DD1_:	
	.db $01 $A8 $22 $01
	
	; Pointer Table from 7DD5 to 7DD6 (1 entries, indexed by unknown)
	.dw _DATA_7DDB_
	
	; Data from 7DD7 to 7DDA (4 bytes)
	.db $03 $01 $00 $0D
	
; 1st entry of Pointer Table from 7DD5 (indexed by unknown)	
; Data from 7DDB to 7DE5 (11 bytes)	
_DATA_7DDB_:	
	.db $02 $80 $02 $00 $06 $01 $70 $01 $00 $0B $E2
	
; 33rd entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7DE6 to 7DE9 (4 bytes)	
_DATA_7DE6_:	
	.db $02 $A0 $21 $02
	
	; Pointer Table from 7DEA to 7DEB (1 entries, indexed by unknown)
	.dw _DATA_7DF9_
	
	; Data from 7DEC to 7DF2 (7 bytes)
	.db $08 $00 $05 $0D $A0 $22 $02
	
	; Pointer Table from 7DF3 to 7DF4 (1 entries, indexed by unknown)
	.dw _DATA_7DF9_
	
	; Data from 7DF5 to 7DF8 (4 bytes)
	.db $05 $01 $05 $0D
	
; 1st entry of Pointer Table from 7DEA (indexed by unknown)	
; Data from 7DF9 to 7E0C (20 bytes)	
_DATA_7DF9_:	
	.db $94 $BA $28 $E2 $00 $60 $00 $20 $2B $00 $60 $00 $40 $06 $00 $40
	.db $00 $10 $18 $E2
	
; 34th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7E0D to 7E10 (4 bytes)	
_DATA_7E0D_:	
	.db $02 $A8 $21 $01
	
	; Pointer Table from 7E11 to 7E12 (1 entries, indexed by unknown)
	.dw _DATA_7E20_
	
	; Data from 7E13 to 7E19 (7 bytes)
	.db $03 $00 $03 $0D $A0 $23 $01
	
	; Pointer Table from 7E1A to 7E1B (1 entries, indexed by unknown)
	.dw _DATA_7E30_
	
	; Data from 7E1C to 7E1F (4 bytes)
	.db $03 $00 $03 $0F
	
; 1st entry of Pointer Table from 7E11 (indexed by unknown)	
; Data from 7E20 to 7E2F (16 bytes)	
_DATA_7E20_:	
	.db $01 $10 $02 $DC $02 $00 $00 $00 $00 $02 $01 $30 $03 $00 $08 $E2
	
; 1st entry of Pointer Table from 7E1A (indexed by unknown)	
; Data from 7E30 to 7E3B (12 bytes)	
_DATA_7E30_:	
	.db $E3 $07 $AA $81 $06 $80 $80 $02 $BD $99 $08 $E2
	
; 35th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7E3C to 7E3F (4 bytes)	
_DATA_7E3C_:	
	.db $01 $A0 $22 $02
	
	; Pointer Table from 7E40 to 7E41 (1 entries, indexed by unknown)
	.dw _DATA_7E46_
	
	; Data from 7E42 to 7E45 (4 bytes)
	.db $03 $00 $03 $0F
	
; 1st entry of Pointer Table from 7E40 (indexed by unknown)	
; Data from 7E46 to 7E48 (3 bytes)	
_DATA_7E46_:	
	.db $8D $99 $07
	
	; Pointer Table from 7E49 to 7E4E (3 entries, indexed by unknown)
	.dw _DATA_FE1_ _DATA_B1A5_ _RAM_E205_
	
; 37th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7E4F to 7E52 (4 bytes)	
_DATA_7E4F_:	
	.db $02 $A8 $21 $01
	
	; Pointer Table from 7E53 to 7E54 (1 entries, indexed by unknown)
	.dw _DATA_7E62_
	
	; Data from 7E55 to 7E5B (7 bytes)
	.db $03 $00 $00 $0D $A8 $22 $01
	
	; Pointer Table from 7E5C to 7E5D (1 entries, indexed by unknown)
	.dw _DATA_7E7C_
	
	; Data from 7E5E to 7E61 (4 bytes)
	.db $00 $00 $00 $0D
	
; 1st entry of Pointer Table from 7E53 (indexed by unknown)	
; Data from 7E62 to 7E7B (26 bytes)	
_DATA_7E62_:	
	.db $00 $C0 $00 $A0 $05 $00 $70 $00 $60 $05 $00 $B0 $00 $90 $05 $00
	.db $B0 $00 $20 $05 $00 $70 $00 $10 $0A $E2
	
; 1st entry of Pointer Table from 7E5C (indexed by unknown)	
; Data from 7E7C to 7E95 (26 bytes)	
_DATA_7E7C_:	
	.db $00 $80 $00 $60 $05 $00 $40 $00 $30 $05 $00 $80 $00 $60 $05 $00
	.db $40 $00 $30 $05 $00 $80 $00 $20 $0A $E2
	
; 38th entry of Pointer Table from 6D28 (indexed by _RAM_C001_)	
; Data from 7E96 to 7E99 (4 bytes)	
_DATA_7E96_:	
	.db $02 $A8 $21 $01
	
	; Pointer Table from 7E9A to 7E9B (1 entries, indexed by unknown)
	.dw _DATA_7EA9_
	
	; Data from 7E9C to 7EA2 (7 bytes)
	.db $00 $00 $02 $0F $A8 $23 $01
	
	; Pointer Table from 7EA3 to 7EA4 (1 entries, indexed by unknown)
	.dw _DATA_7EB4_
	
	; Data from 7EA5 to 7EA8 (4 bytes)
	.db $00 $00 $01 $0D
	
; 1st entry of Pointer Table from 7E9A (indexed by unknown)	
; Data from 7EA9 to 7EB3 (11 bytes)	
_DATA_7EA9_:	
	.db $03 $80 $03 $BF $04 $03 $F2 $03 $FF $07 $E2
	
; 1st entry of Pointer Table from 7EA3 (indexed by unknown)	
; Data from 7EB4 to 7EE3 (48 bytes)	
_DATA_7EB4_:	
	.db $E3 $07 $00 $20 $00 $30 $04 $00 $18 $00 $1A $07 $E2 $03 $80 $20
	.db $04 $DD $7E $F7 $01 $07 $0D $80 $21 $04 $DD $7E $EB $01 $07 $0B
	.db $80 $22 $04 $DD $7E $FE $01 $07 $0A $A0 $04 $A9 $02 $A5 $06 $E2
	
; Pointer Table from 7EE4 to 7EFF (14 entries, indexed by _RAM_C00D_)	
_DATA_7EE4_:	
	.dw _DATA_7F00_ _DATA_7F02_ _DATA_7F07_ _DATA_7F0C_ _DATA_7F12_ _DATA_7F18_ _DATA_7F20_ _DATA_7F27_
	.dw _DATA_7F2E_ _DATA_7F31_ _DATA_7F39_ _DATA_7F3E_ _DATA_7F4A_ _DATA_7F56_
	
; 1st entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F00 to 7F01 (2 bytes)	
_DATA_7F00_:	
	.db $FF $02
	
; 2nd entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F02 to 7F06 (5 bytes)	
_DATA_7F02_:	
	.db $FF $ED $DC $BA $02
	
; 3rd entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F07 to 7F0B (5 bytes)	
_DATA_7F07_:	
	.db $EF $FF $FF $EE $02
	
; 4th entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F0C to 7F11 (6 bytes)	
_DATA_7F0C_:	
	.db $DE $FE $DD $CC $BB $01
	
; 5th entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F12 to 7F17 (6 bytes)	
_DATA_7F12_:	
	.db $EF $FF $EE $DD $CD $01
	
; 6th entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F18 to 7F1F (8 bytes)	
_DATA_7F18_:	
	.db $FF $EE $DC $BA $A9 $87 $76 $01
	
; 7th entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F20 to 7F26 (7 bytes)	
_DATA_7F20_:	
	.db $FF $EE $DD $CC $BB $AA $01
	
; 8th entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F27 to 7F2D (7 bytes)	
_DATA_7F27_:	
	.db $ED $BB $A9 $8A $A9 $85 $02
	
; 9th entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F2E to 7F30 (3 bytes)	
_DATA_7F2E_:	
	.db $DE $FE $02
	
; 10th entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F31 to 7F38 (8 bytes)	
_DATA_7F31_:	
	.db $EE $FF $EE $DD $CC $BB $A9 $01
	
; 11th entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F39 to 7F3D (5 bytes)	
_DATA_7F39_:	
	.db $EF $FF $ED $DC $02
	
; 12th entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F3E to 7F49 (12 bytes)	
_DATA_7F3E_:	
	.db $DE $FF $FE $EE $DD $DC $CC $BB $BB $BA $A9 $01
	
; 13th entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F4A to 7F55 (12 bytes)	
_DATA_7F4A_:	
	.db $DE $EF $E9 $79 $DD $CC $8B $B5 $98 $B7 $A9 $01
	
; 14th entry of Pointer Table from 7EE4 (indexed by _RAM_C00D_)	
; Data from 7F56 to 7F58 (3 bytes)	
_DATA_7F56_:	
	.db $FF $FD $02
	
; Pointer Table from 7F59 to 7F62 (5 entries, indexed by _RAM_C00C_)	
_DATA_7F59_:	
	.dw _DATA_7F63_ _DATA_7F6E_ _DATA_7F7C_ _DATA_7F8B_ _DATA_7F90_
	
; 1st entry of Pointer Table from 7F59 (indexed by _RAM_C00C_)	
; Data from 7F63 to 7F6D (11 bytes)	
_DATA_7F63_:	
	.db $EE $EE $EE $EE $EE $FF $EE $DD $EF $FE $DD
	
; 2nd entry of Pointer Table from 7F59 (indexed by _RAM_C00C_)	
; Data from 7F6E to 7F7B (14 bytes)	
_DATA_7F6E_:	
	.db $EF $FE $DC $CD $EF $FE $DC $CD $DE $FF $FE $DC $BC $DE
	
; 3rd entry of Pointer Table from 7F59 (indexed by _RAM_C00C_)	
; Data from 7F7C to 7F8A (15 bytes)	
_DATA_7F7C_:	
	.db $FE $DB $AB $CD $EF $ED $CB $AB $CD $EF $EE $01 $FF $EE $00
	
; 4th entry of Pointer Table from 7F59 (indexed by _RAM_C00C_)	
; Data from 7F8B to 7F8F (5 bytes)	
_DATA_7F8B_:	
	.db $EE $EE $FF $FF $00
	
; 5th entry of Pointer Table from 7F59 (indexed by _RAM_C00C_)	
; Data from 7F90 to 7FEF (96 bytes)	
_DATA_7F90_:	
	.db $EE $FF $00
	.dsb 93, $FF
	
.BANK 1 SLOT 1	
.ORG $0000	
	
	; Data from 7FF0 to 7FFF (16 bytes)
	.db $54 $4D $52 $20 $53 $45 $47 $41 $FF $FF $6F $34 $68 $50 $01 $4F
	
.BANK 2	
.ORG $0000	
	
; Data from 8000 to 80BF (192 bytes)	
_DATA_8000_:	
	.db $11 $11 $00 $00 $67 $67 $18 $08 $05 $05 $3A $10 $6A $6A $15 $00
	.db $10 $10 $1F $10 $10 $10 $1F $10 $21 $21 $3E $20 $23 $23 $3C $20
	.db $F0 $F0 $00 $00 $FC $CC $00 $00 $FE $F2 $00 $00 $7E $7A $80 $00
	.db $FF $FF $00 $00 $FF $DD $00 $00 $ED $DF $00 $00 $69 $E9 $16 $16
	.dsb 24, $00
	.db $80 $80 $00 $00 $80 $80 $00 $00 $27 $27 $38 $20 $27 $27 $38 $20
	.db $21 $21 $3E $22 $21 $21 $3E $22 $12 $12 $1D $11 $13 $13 $1C $10
	.db $13 $13 $1C $10 $09 $09 $0E $08 $40 $4E $B1 $B1 $1A $25 $C0 $C0
	.db $3B $4E $8A $8A $3B $4E $8A $8A $00 $3F $C0 $C0 $04 $BB $40 $40
	.db $04 $7B $84 $80 $81 $BF $40 $40 $80 $80 $00 $00 $80 $80 $00 $00
	.db $80 $80 $00 $00 $80 $80 $00 $00 $80 $80
	.dsb 14, $00
	
; 1st entry of Pointer Table from 9D40 (indexed by _RAM_C171_)	
; Data from 80C0 to 817F (192 bytes)	
_DATA_80C0_:	
	.db $04 $07 $04 $04 $08 $0F $08 $08 $10 $1F $10 $10 $10 $1F $10 $10
	.db $10 $1F $10 $10 $08 $0F $08 $08 $1E $1F $1E $1E $3F $20 $20 $3F
	.db $02 $C2 $3C $3C $00 $FC $00 $00 $00 $FE $00 $00 $00 $3E $C0 $C0
	.db $00 $DE $21 $21 $00 $EE $11 $11 $00 $EE $11 $11 $00 $C3 $3C $3F
	.dsb 17, $00
	.db $80 $00 $00 $00 $80 $00 $00 $00 $80 $00 $00 $00 $00 $00 $00 $1F
	.db $10 $10 $1F $0D $08 $0A $0F $08 $09 $0E $0E $08 $09 $0E $0E $04
	.db $05 $06 $06 $03 $03 $03 $03
	.dsb 9, $00
	.db $FF $00 $FF $E0 $17 $08 $F9 $E0 $17 $08 $F8 $00 $D7 $28 $38 $50
	.db $D7 $58 $58 $90 $93 $9C $9C $08 $08 $0F $0F $07 $07 $07 $07
	.dsb 21, $00
	.db $80 $00 $00 $00 $80 $00 $00 $00 $00 $00 $00
	
; 2nd entry of Pointer Table from 9D40 (indexed by _RAM_C171_)	
; Data from 8180 to 823F (192 bytes)	
_DATA_8180_:	
	.db $04 $07 $04 $04 $08 $0F $08 $08 $10 $1F $10 $10 $10 $1F $10 $10
	.db $10 $1F $10 $10 $08 $0F $08 $08 $06 $07 $06 $06 $1F $1C $1C $1F
	.db $02 $C2 $3C $3C $00 $FC $00 $00 $00 $7E $80 $80 $00 $3F $C0 $C0
	.db $00 $DE $21 $21 $00 $E2 $1D $1D $00 $BE $41 $43 $80 $7F $00 $81
	.dsb 13, $00
	.db $80 $00 $00 $00 $80 $00 $00 $00 $80 $00 $00 $00 $80 $00 $00 $00
	.db $00 $00 $00 $21 $20 $3E $3F $20 $20 $3F $3F $20 $2F $30 $30 $23
	.db $2F $33 $33 $24 $2C $34 $34 $18 $18 $18 $18 $00 $00 $00 $00 $00
	.db $00 $00 $00 $C0 $0F $30 $F0 $F0 $23 $2C $FC $F8 $B9 $BE $FE $44
	.db $44 $47 $47 $02 $02 $03 $03 $01 $01 $01 $01
	.dsb 9, $00
	.db $60 $00 $00 $00 $F0 $00 $00 $00 $F0 $00 $00 $00 $60 $80 $80 $20
	.db $20 $E0 $E0 $C0 $C0 $C0 $C0 $00 $00 $00 $00 $00 $00 $00 $00
	
; 3rd entry of Pointer Table from 9D40 (indexed by _RAM_C171_)	
; Data from 8240 to 82FF (192 bytes)	
_DATA_8240_:	
	.db $04 $07 $04 $04 $08 $0F $08 $08 $10 $1F $10 $10 $10 $1F $10 $10
	.db $10 $1F $10 $10 $08 $0F $08 $08 $06 $07 $06 $06 $07 $04 $04 $07
	.db $02 $C2 $3C $3C $00 $FE $00 $00 $00 $7F $80 $80 $00 $3E $C1 $C1
	.db $00 $DE $21 $21 $00 $E2 $1D $1D $18 $87 $60 $7F $9C $63 $00 $9F
	.dsb 13, $00
	.db $80 $00 $00 $00 $80 $00 $00 $00 $80
	.dsb 10, $00
	.db $0E $08 $09 $0F $1E $10 $11 $1F $0E $0E $0F $0F
	.dsb 12, $01
	.db $00 $00 $00 $00 $00 $00 $00 $00 $0C $F3 $00 $0F $0A $79 $8C $8F
	.db $00 $72 $8C $8C $00 $74 $88 $88 $00 $30 $C0 $C0 $00 $38 $C0 $C0
	.db $80 $98 $E0 $E0 $70 $70 $70 $70
	.dsb 32, $00
	
; 4th entry of Pointer Table from 9D40 (indexed by _RAM_C171_)	
; Data from 8300 to 83BF (192 bytes)	
_DATA_8300_:	
	.db $06 $07 $06 $06 $04 $07 $04 $04 $08 $0F $08 $08 $08 $0F $08 $08
	.db $08 $0F $08 $08 $30 $3F $30 $3C $7C $43 $40 $7F $3F $20 $20 $3F
	.db $02 $C2 $3C $3C $00 $FC $00 $00 $00 $BE $40 $40 $00 $9E $60 $60
	.db $00 $EE $10 $10 $00 $F6 $08 $08 $00 $F7 $08 $0E $00 $FF $00 $98
	.dsb 25, $00
	.db $80 $00 $00 $00 $D8 $00 $00 $7F $50 $50 $7F $2F $20 $30 $3F $4C
	.db $43 $70 $7C $40 $4E $70 $70 $40 $5C $60 $60 $20 $3C $20 $20 $18
	.db $18 $18 $18 $00 $00 $00 $00 $80 $7B $04 $FC $F0 $00 $0F $FF $FF
	.db $BF $BF $FF $40 $40 $40 $40
	.dsb 17, $00
	.db $FC $00 $00 $00 $7C $80 $80 $04 $3C $C4 $C4 $88 $88 $F8 $F8 $70
	.db $70 $70 $70
	.dsb 12, $00
	
; 1st entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 83C0 to 853F (384 bytes)	
_DATA_83C0_:	
	.db $00 $00 $00 $00 $00 $00 $00 $00 $02 $02 $00 $00 $01 $01 $02 $00
	.db $05 $05 $02 $00 $02 $02 $01 $00 $02 $02 $03 $02 $04 $04 $07 $04
	.db $00 $00 $00 $00 $00 $00 $00 $00 $9C $9C $20 $00 $FF $FF $00 $00
	.db $FF $FF $00 $00 $CF $CF $30 $00 $9B $9B $64 $00 $B9 $B9 $46
	.dsb 17, $00
	.db $80 $80 $00 $00 $C0 $C0 $00 $00 $C0 $C0 $00 $00 $E0 $E0 $00 $00
	.db $05 $05 $06 $04 $05 $05 $06 $04 $04 $04 $07 $04 $04 $04 $07 $04
	.db $08 $0A $0D $0C $08 $0B $0C $0C $08 $0B $0C $0C $08 $0B $0C $0C
	.db $79 $79 $86 $02 $50 $50 $AF $0F $46 $41 $B8 $38 $4E $43 $B2 $B2
	.db $4E $43 $B2 $32 $80 $8F $70 $30 $21 $26 $D8 $18 $21 $A6 $59 $18
	.db $60 $60 $80 $80 $20 $A0 $40 $40 $80 $40 $00 $00 $C0 $80 $80 $80
	.db $C0 $80 $80 $80 $00 $C0 $00 $00 $00 $C0 $00 $00 $00 $C0 $00 $00
	.db $04 $05 $06 $06 $04 $05 $06 $06 $02 $02 $03 $03 $01 $01 $01 $01
	.dsb 17, $00
	.db $F3 $0C $0C $00 $FC $03 $03 $00 $DF $20 $20 $00 $0F $F0 $F0 $C0
	.db $C7 $F8 $F8 $20 $21 $3E $3E $7F $40 $40 $7F $FF $80 $80 $FF $00
	.db $80 $00 $00 $00 $40 $80 $80 $00 $B0 $40 $40 $00 $DC $20 $20 $00
	.db $D8 $20 $20 $10 $F0 $10 $70 $80 $7C $00 $E0 $80 $6E $10 $F0
	.dsb 32, $00
	.db $F3 $8C $80 $F3 $E1 $AE $B0 $F1 $61 $6F $71 $71 $40 $5E $60 $60
	.db $80 $9C $E0 $E0 $80 $9E $E0 $E0 $40 $46 $78 $78 $3C $3C $3C $3C
	.db $00 $CE $30 $F0 $20 $E7 $38 $F8 $90 $57 $18 $D8 $8E $8E $8E $8E
	.dsb 16, $00
	
; 2nd entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 8540 to 86BF (384 bytes)	
_DATA_8540_:	
	.dsb 16, $00
	.db $03 $03 $00 $00 $01 $01 $00 $00 $03 $03 $00 $00 $01 $01 $01 $01
	.dsb 12, $00
	.db $40 $40 $00 $00 $4E $4E $00 $00 $A7 $A7 $58 $00 $21 $21 $DE $00
	.db $41 $41 $BE
	.dsb 21, $00
	.db $C0 $C0 $00 $00 $E0 $E0 $00 $00 $F0 $F0 $00 $00 $02 $02 $03 $02
	.db $02 $02 $03 $02 $02 $02 $03 $02 $02 $02 $03 $02 $02 $02 $03 $02
	.db $02 $02 $03 $02 $01 $01 $01 $01 $01 $01 $01 $01 $00 $00 $FF $00
	.db $00 $00 $FF $00 $00 $00 $FF $00 $00 $00 $FF $00 $00 $00 $FF $00
	.db $00 $00 $FF $00 $00 $00 $FF $00 $00 $00 $FF $08 $F0 $F0 $00 $00
	.db $F8 $F8 $00 $00 $B8 $B8 $40 $00 $B0 $B2 $48 $08 $00 $0E $F0 $30
	.db $00 $3E $C0 $40 $00 $3F $C0 $00 $02 $FE $02 $02 $01 $01 $01 $01
	.db $00 $00 $00 $00 $00 $01 $00 $00 $00 $02 $01 $01 $01 $00 $02 $03
	.db $07 $04 $04 $07 $19 $18 $1E $1F $21 $20 $3E $3F $00 $07 $F8 $98
	.db $00 $03 $FC $FC $00 $01 $FE $FE $C0 $01 $3E $FE $F8 $01 $06 $FE
	.db $F8 $07 $00 $FF $FC $03 $00 $FF $7C $83 $00 $7F $0C $FC $0C $0C
	.db $30 $F0 $30 $30 $40 $C0 $40 $40 $00 $80 $00 $00 $00 $80 $00 $00
	.db $00 $C0 $00 $C0 $00 $E0 $00 $E0 $00 $60 $80 $E0 $40 $43 $7C $7C
	.db $40 $4F $70 $70 $43 $4F $73 $73 $24 $3C $24 $24 $18 $18 $18 $18
	.dsb 12, $00
	.db $3E $C0 $01 $3F $7C $F0 $73 $7F $88 $88 $8F $8F $04 $04 $07 $07
	.db $04 $04 $07 $07 $02 $02 $03 $03 $01 $01 $01 $01 $00 $00 $00 $00
	.db $00 $E0 $00 $20 $00 $D0 $00 $10 $00 $C0 $00 $00 $00 $60 $80 $80
	.db $00 $60 $80 $80 $00 $20 $C0 $C0 $00 $00 $E0 $E0 $E0 $E0 $E0 $E0
	
; 5th entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 86C0 to 883F (384 bytes)	
_DATA_86C0_:	
	.db $00 $00 $00 $00 $02 $02 $01 $00 $05 $05 $02 $00 $02 $02 $01 $01
	.db $04 $04 $07 $04 $0B $0B $0C $08 $0B $0B $0C $08 $12 $12 $1D $10
	.db $00 $00 $00 $00 $80 $80 $00 $00 $1C $1C $E0 $00 $7F $7F $80 $00
	.db $BF $B1 $40 $00 $0F $08 $F0 $00 $1F $1F $E0 $00 $3B $3B $C4
	.dsb 17, $00
	.db $80 $80 $00 $00 $C0 $C0 $00 $00 $C0 $C0 $00 $00 $60 $60 $80 $00
	.db $10 $10 $1F $10 $10 $10 $1F $10 $10 $10 $1F $10 $18 $18 $17 $11
	.db $18 $18 $17 $11 $1C $1C $13 $10 $01 $7D $02 $00 $00 $FE $01 $00
	.db $37 $35 $C8 $00 $76 $73 $88 $88 $C0 $C3 $3C $1C $86 $99 $60 $20
	.db $8E $B1 $40 $40 $8E $B3 $42 $42 $80 $BF $40 $40 $8F $B0 $4C $40
	.db $60 $20 $80 $00 $E0 $E0 $00 $00 $00 $60 $80 $80 $80 $40 $00 $00
	.db $C0 $10 $20 $20 $C0 $90 $A0 $A0 $00 $D0 $20 $20 $80 $20 $C0 $40
	.db $00 $EF $10 $10 $80 $C3 $BC $BC $7C $7D $7E $7E $0C $01 $32 $3E
	.db $04 $01 $7A $7E $88 $B3 $C4 $CC $84 $B9 $C2 $CE $86 $F8 $81 $9F
	.db $07 $D8 $27 $20 $00 $8F $70 $70 $00 $C0 $3E $3E $00 $C0 $3E $3E
	.db $00 $E0 $1E $1E $00 $F0 $0C $0C $00 $F0 $0C $0C $00 $00 $F8 $F8
	.db $00 $80
	.dsb 30, $00
	.db $77 $78 $70 $7F $15 $18 $12 $1F $09 $08 $0E $0F $10 $10 $1F $1F
	.db $10 $13 $1C $1C $10 $17 $18 $18 $08 $0E $08 $08 $06 $06 $06 $06
	.db $F8 $00 $00 $F8 $F8 $00 $00 $F8 $F8 $00 $00 $F8 $70 $10 $90 $F0
	.db $E0 $E0 $E0 $E0
	.dsb 44, $00
	
; 6th entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 8840 to 89BF (384 bytes)	
_DATA_8840_:	
	.dsb 40, $00
	.db $07 $07 $00 $00 $1F $1F $00 $00 $3F $3F $00 $00 $7F $7F $00 $00
	.db $7F $7F $00 $00 $DB $DB $24 $24 $00 $00 $00 $00 $08 $08 $00 $00
	.db $CC $CC $00 $00 $F7 $F7 $08 $00 $38 $38 $C4 $00 $9E $9E $62 $02
	.db $EE $EE $12 $02 $6D $ED $13 $01
	.dsb 32, $00
	.db $C0 $C9 $36 $36 $96 $A9 $40 $40 $37 $48 $00 $00 $37 $5C $14 $14
	.db $00 $7F $00 $00 $18 $66 $11 $01 $1C $62 $1D $01 $00 $3C $03 $03
	.db $75 $F5 $0B $01 $31 $B1 $4F $41 $21 $21 $DF $C1 $02 $02 $FE $E2
	.db $22 $22 $DE $C2 $62 $62 $9E $82 $44 $C4 $3C $04 $C4 $C4 $3C $04
	.dsb 13, $00
	.db $30 $01 $01 $00 $7B $00 $03 $00 $7F $00 $01 $00 $0F $30 $30 $20
	.db $23 $3C $3C $00 $03 $3C $3C $00 $7F $00 $00 $00 $FF $00 $00 $00
	.db $FE $01 $01 $00 $3C $C3 $C3 $40 $80 $3F $FF $39 $C1 $07 $FF $3F
	.db $C0 $00 $FF $88 $E8 $18 $08 $10 $F0 $10 $10 $08 $F8 $08 $08 $08
	.db $78 $88 $88 $04 $3C $C4 $C4 $84 $BC $C4 $C4 $84 $3C $44 $C4 $C8
	.db $98 $A8 $E8 $13 $13 $1F $1F $0C $0C $0C $0C
	.dsb 24, $00
	.db $7F $80 $00 $FF $6F $00 $10 $7F $07 $11 $09 $0F $02 $3A $06 $06
	.db $02 $3A $06 $06 $02 $72 $0E $0E $04 $64 $1C $1C $38 $38 $38 $38
	.db $F0 $B0 $B0 $F0 $C0 $40 $40 $C0 $C0 $40 $40 $C0 $80 $80 $80 $80
	.dsb 16, $00
	
; 7th entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 89C0 to 8B3F (384 bytes)	
_DATA_89C0_:	
	.dsb 16, $00
	.db $01 $01 $00 $00 $02 $02 $01 $00 $02 $02 $01 $00 $01 $01 $06 $00
	.db $02 $02 $00 $00 $00 $00 $01 $00 $3E $3E $01 $01 $C7 $C7 $38 $00
	.db $1F $1F $E0 $00 $3F $3F $C0 $00 $FF $FF $00 $00 $6E $6E $91 $90
	.db $80 $80 $00 $00 $E0 $E0 $00 $00 $40 $40 $80 $80 $80 $80 $00 $00
	.db $C0 $C0 $00 $00 $E0 $E0 $00 $00 $E0 $E0 $00 $00 $E0 $E0 $10 $00
	.db $01 $01 $06 $00 $03 $03 $04 $00 $00 $00 $07 $03 $02 $02 $05 $01
	.db $02 $02 $05 $01 $02 $02 $05 $01 $02 $02 $01 $01 $01 $01 $02 $00
	.db $2C $2C $D3 $D3 $08 $5B $A4 $A4 $77 $88 $00 $00 $FF $14 $14 $14
	.db $FF $00 $00 $00 $77 $88 $00 $00 $08 $F7 $00 $00 $08 $77 $88 $80
	.db $E0 $E0 $10 $00 $40 $40 $B0 $80 $00 $80 $70 $40 $80 $40 $30 $00
	.db $E0 $20 $10 $00 $20 $A0 $50 $00 $20 $A0 $40 $00 $60 $E0 $00 $00
	.db $00 $7F $00 $00 $00 $F7 $08 $08 $00 $F0 $0F $0F $07 $E7 $1F $1F
	.db $78 $78 $78 $78 $03 $0C $00 $03 $01 $1E $00 $01 $00 $1F $00 $00
	.db $80 $9F $60 $60 $81 $C1 $3E $3E $00 $7F $80 $80 $00 $7F $80 $80
	.db $00 $3F $C0 $C0 $00 $1F $E0 $E0 $C0 $06 $39 $F9 $F0 $0F $00 $FF
	.db $80 $F8 $00 $00 $00 $FE $00 $00 $00 $9E $60 $60 $40 $4C $F0 $F0
	.db $38 $38 $B8 $B8 $00 $00 $80 $80 $00 $80 $00 $80 $00 $E0 $00 $80
	.db $10 $1F $10 $10 $08 $0F $08 $08 $04 $07 $04 $04 $03 $03 $03 $03
	.dsb 16, $00
	.db $7C $83 $00 $7F $7E $80 $01 $7F $FF $C7 $C7 $FF $38 $28 $28 $38
	.db $10 $10 $10 $10
	.dsb 13, $00
	.db $76 $80 $80 $00 $26 $D9 $D9 $00 $0F $F0 $F0 $C1 $DF $E1 $E1 $42
	.db $5E $62 $62 $24 $3C $24 $24 $18 $18 $18 $18 $00 $00 $00 $00
	
; 8th entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 8B40 to 8CBF (384 bytes)	
_DATA_8B40_:	
	.dsb 12, $00
	.db $01 $01 $00 $00 $03 $03 $00 $00 $06 $06 $01 $00 $04 $04 $03 $00
	.db $0E $0E $01 $00 $05 $05 $00 $00 $01 $01 $02 $00 $78 $78 $07 $03
	.db $FE $FE $01 $00 $3F $3F $C0 $00 $7F $7F $80 $00 $FF $FF $00 $00
	.db $DD $DD $22 $22 $00 $00 $00 $00 $80 $80
	.dsb 12, $00
	.db $80 $00 $80 $80 $00 $00 $C0 $C0 $00 $00 $C0 $C0 $00 $00 $1A $1B
	.db $04 $00 $04 $05 $0A $02 $0C $0D $02 $02 $19 $18 $06 $02 $03 $38
	.db $04 $00 $00 $7D $02 $02 $00 $7D $02 $02 $40 $44 $7B $7B $58 $5B
	.db $A4 $A4 $10 $B7 $48 $48 $EE $11 $00 $00 $FF $44 $44 $44 $FF $00
	.db $00 $00 $EE $11 $00 $00 $10 $EF $00 $00 $10 $EF $10 $00 $C0 $C0
	.db $00 $00 $E0 $E0 $00 $00 $E0 $60 $00 $00 $D0 $F0 $00 $00 $80 $F0
	.db $00 $00 $80 $F8 $00 $00 $80 $F0 $08 $08 $08 $E8 $18 $18 $30 $36
	.db $39 $39 $08 $0B $0C $0C $04 $04 $07 $07 $02 $02 $03 $03 $00 $00
	.db $01 $01 $00 $00 $01 $01 $03 $00 $00 $03 $07 $00 $00 $07 $00 $7D
	.db $82 $82 $02 $83 $7C $7C $00 $FE $01 $01 $00 $FE $01 $01 $00 $7E
	.db $81 $81 $00 $3E $C1 $C1 $80 $0D $72 $F3 $F0 $0F $00 $FE $10 $D0
	.db $30 $30 $20 $20 $E0 $E0 $40 $40 $C0 $C0 $80 $86 $80 $80 $00 $0F
	.db $00 $00 $00 $7E $01 $01 $01 $FD $03 $03 $01 $C1 $3F $3F $0C $02
	.db $01 $0D $08 $0E $09 $09 $00 $07 $00 $00 $00 $03 $00 $00 $00 $07
	.db $00 $00 $00 $07 $00 $00 $00 $06 $01 $01 $03 $03 $03 $03 $F8 $06
	.db $01 $FF $70 $0C $83 $FF $77 $0F $87 $FF $38 $08 $C8 $F8 $30 $B0
	.db $70 $70 $40 $40 $C0 $C0 $80 $80 $80 $80 $00 $00 $00 $00 $32 $32
	.db $FE $FE $CC $CC $CC $CC
	.dsb 24, $00
	
; 9th entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 8CC0 to 8E3F (384 bytes)	
_DATA_8CC0_:	
	.db $03 $00 $03 $00 $01 $00 $01 $00 $9F $00 $9F $00 $0F $00 $0F $00
	.db $6F $01 $6F $01 $3F $05 $3F $05 $1F $06 $1F $06 $1F $0F $1F $0F
	.db $90 $00 $90 $00 $FC $00 $FC $00 $FE $28 $FE $28 $FF $2C $FF $2C
	.db $FF $FC $FF $FC
	.dsb 12, $FF
	.db $00 $00 $00 $00 $90 $00 $90 $00 $C0 $00 $C0 $00 $E0 $00 $E0 $00
	.db $F1 $80 $F1 $80 $F2 $C0 $F2 $C0 $F8 $D0 $F8 $D0 $F8 $F8 $F8 $F8
	.db $1F $0F $1F $0F $1F $07 $1F $07 $1F $07 $1F $07 $0F $07 $0F $07
	.db $0F $03 $0F $03 $CF $03 $CF $03 $3F $01 $3F $01 $7F $1D $7F $1D
	.dsb 13, $FF
	.db $88 $88 $88 $FF $22 $22 $22 $FF $00 $00 $00 $FF $88 $88 $88 $FF
	.db $FF $FF $FF $F8 $F0 $F8 $F0 $F8 $F0 $F8 $F0 $FA $F0 $FA $F0 $F8
	.db $E0 $F8 $E0 $F8 $00 $18 $00 $F8 $60 $78 $60 $F0 $C0 $F0 $C0 $F4
	.db $C0 $F4 $C0 $7F $3F $7F $3F $7F $3F $7F $3F $7F $1F $7F $1F $3F
	.db $01 $3F $01 $0F $00 $0F $00 $07 $00 $07 $00 $07 $00 $06 $01 $3F
	.db $00 $3C $03
	.dsb 29, $FF
	.db $3F $3F $FF $E0 $80 $E0 $80 $F0 $80 $F0 $80 $FC $E0 $FC $E0 $F8
	.db $F0 $F8 $F0 $FC $F8 $FC $F8 $FE $F8 $FE $F8 $FE $BC $FE $BC $FE
	.db $3C $7E $BC $0F $00 $0C $03 $0F $00 $0E $01 $07 $01 $07 $01 $07
	.db $01 $07 $01 $07 $01 $07 $01 $0F $01 $0F $01 $03 $00 $03 $00 $00
	.db $00 $00 $00 $FF $00 $00 $FF $FF $01 $01 $FF $FF $C3 $C3 $FF $FF
	.db $EF $EF $FF $FF $EF $FF $EF $FF $F7 $FF $F7 $FF $70 $FF $70 $F8
	.db $30 $F8 $30 $FE $18 $7E $98 $FC $00 $7C $80 $E8 $80 $E8 $80 $C0
	.db $80 $C0 $80 $C0 $80 $C0 $80 $C0 $00 $C0 $00 $80 $00 $80 $00 $00
	.db $00 $00 $00
	
; 10th entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 8E40 to 8FBF (384 bytes)	
_DATA_8E40_:	
	.db $02 $02 $02 $02 $03 $02 $03 $02 $0B $03 $0B $03 $0F $0B $0F $0B
	.db $1F $0F $1F $0F $1F $0F $1F $0F $1F $1E $1F $1F $1F $1E $1F $1F
	.db $79 $79 $79 $79 $FB $F9 $FB $F9 $FF $FD $FF $FD
	.dsb 9, $FF
	.db $DF $FF $FF $FF $DF $FF $FF $FF $59 $FF $FF $00 $00 $00 $00 $00
	.db $00 $00 $00 $A0 $80 $A0 $80 $E0 $E0 $E0 $E0 $E0 $E0 $E0 $E0 $F0
	.db $E0 $F0 $E0 $F0 $F0 $F0 $F0 $E0 $E0 $E0 $E0 $0F $0E $0F $0F $1F
	.db $1C $1F $1F $0F $0C $0F $0F $23 $00 $23 $23 $33 $00 $33 $33 $39
	.db $00 $39 $39 $7F $02 $7F $7F $1F $11 $1F $1F $FF $10 $FF $FF $FF
	.db $28 $BB $BB $FF $28 $39 $39 $FF $00 $11 $11 $FF $00 $FF $FF $FF
	.db $00 $FF $FF $FF $00 $FF $FF $FF $83 $FF $FF $E0 $E0 $E0 $E0 $F0
	.db $70 $F0 $F0 $C0 $40 $C0 $C0 $88 $00 $88 $88 $98 $00 $98 $98 $F8
	.db $40 $F8 $F8 $FC $80 $FC $FC $F0 $10 $F0 $F0 $0F $08 $0F $0F $07
	.db $04 $07 $07 $03 $03 $03 $03 $05 $01 $01 $05 $3F $01 $01 $3F $07
	.db $00 $00 $07 $0F $00 $00 $0F $01 $00 $00 $01 $FF $7C $FF $FF $FF
	.db $00 $FF $FF $FF $01 $FF $FF $FF $01 $FF $FF $FF $83 $FF $FF $FF
	.db $7C $7C $FF $FF $00 $00 $FF $FF $00 $00 $FF $E0 $20 $E0 $E0 $C0
	.db $40 $C0 $C0 $80 $80 $80 $80 $40 $00 $00 $40 $C0 $00 $00 $C0 $F0
	.db $00 $00 $F0 $C0 $00 $00 $C0 $40 $00 $00 $40 $03 $00 $00 $03
	.dsb 28, $00
	.db $FF $84 $84 $FF $FE $40 $C6 $FE $FE $20 $EE $FE $FE $00 $FE $FE
	.db $7C $00 $7C $7C $7C $00 $7C $7C $38 $10 $38 $38
	.dsb 36, $00
	
; 11th entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 8FC0 to 913F (384 bytes)	
_DATA_8FC0_:	
	.db $00 $00 $00 $00 $02 $00 $02 $00 $03 $03 $03 $03 $0B $0B $0B $0B
	.db $0F $07 $0F $07
	.dsb 9, $1F
	.db $16 $1F $17 $50 $50 $50 $50 $79 $58 $79 $58 $FB $FB $FB $FB $FF
	.db $FB $FF $FB
	.dsb 9, $FF
	.db $DF $FF $FF $FF $DF $FF $FF
	.dsb 12, $00
	.db $A0 $A0 $A0 $A0 $E0 $A0 $E0 $A0 $E0 $E0 $E0 $E0 $F0 $F0 $F0 $F0
	.db $F0 $E0 $F0 $E0 $1F $1E $1F $1F $0F $0E $0F $0F $1F $1C $1F $1F
	.db $0F $0C $0F $0F $03 $00 $03 $03 $03 $00 $03 $03 $01 $00 $01 $01
	.db $1F $02 $1F $1F $FF $59 $FF $FF $FF $10 $FF $FF $FF $00 $93 $93
	.db $FF $28 $39 $39 $FF $28 $39 $39 $FF $00 $FF $FF $FF $00 $FF $FF
	.db $FF $00 $FF $FF $E0 $E0 $E0 $E0 $E0 $E0 $E0 $E0 $F0 $70 $F0 $F0
	.db $C0 $40 $C0 $C0 $80 $00 $80 $80 $80 $00 $80 $80 $00 $00 $00 $00
	.db $F0 $80 $F0 $F0 $3F $01 $3F $3F $3F $00 $3F $3F $1F $0E $1F $1F
	.db $11 $01 $11 $11 $01 $01 $01 $01 $3F $01 $01 $3F $07 $00 $00 $07
	.db $0F $00 $00 $0F $FF $83 $FF $FF $FF $7C $FF $FF $FF $00 $FF $FF
	.db $FF $01 $FF $FF $FF $01 $FF $FF $FF $83 $FF $FF $FF $40 $7C $FF
	.db $FF $00 $00 $FF $F8 $00 $F8 $F8 $F8 $00 $F8 $F8 $F0 $E0 $F0 $F0
	.db $10 $00 $10 $10 $00 $00 $00 $00 $F0 $00 $00 $F0 $C0 $00 $00 $C0
	.db $C0 $00 $00 $C0 $01 $00 $00 $01 $03 $00 $00 $03 $01 $01 $01 $01
	.db $01 $00 $01 $01 $01 $00 $01 $01
	.dsb 12, $00
	.db $FF $00 $00 $FF $FF $84 $84 $FF $FF $C7 $C7 $FF $FF $28 $EF $FF
	.db $EF $00 $EF $EF $EE $00 $EE $EE $EE $00 $EE $EE $C6 $00 $C6 $C6
	.db $40 $00 $00 $40
	.dsb 28, $00
	
; 12th entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 9140 to 92BF (384 bytes)	
_DATA_9140_:	
	.dsb 35, $00
	.db $0F $01 $00 $00 $3E $00 $00 $00 $7F $00 $00 $00 $7F $00 $00 $00
	.db $FF $00 $00 $00 $FF $00 $00 $00 $FF $03 $03 $00 $FC $00 $00 $00
	.db $80 $00 $00 $00 $E0 $40 $00 $00 $B0 $20 $00 $00 $D0 $00 $00 $00
	.db $F8 $00 $00 $00 $F8 $00 $00 $00 $F8 $0C $3C $C0 $C0
	.dsb 12, $00
	.db $08 $0E $08 $08 $10 $1F $10 $10 $10 $1E $11 $11 $08 $0C $0B $0B
	.db $06 $06 $07 $07 $3C $25 $02 $C2 $C9 $A2 $14 $14 $C9 $A2 $14 $14
	.db $C8 $A3 $14 $14 $B8 $CB $84 $84 $18 $F1 $06 $06 $04 $70 $8B $8B
	.db $03 $39 $C4 $C5 $D0 $28 $00 $00 $D8 $72 $50 $50 $D8 $72 $54 $54
	.db $00 $FB $04 $04 $E0 $1A $84 $04 $60 $98 $64 $04 $78 $88 $00 $08
	.db $F0 $00 $00 $00 $01 $01 $01 $01 $00 $00 $00 $00 $03 $03 $03 $03
	.db $02 $03 $02 $03 $03 $02 $02 $02 $07 $04 $04 $04 $07 $04 $04 $04
	.db $48 $09 $4E $0E $80 $B8 $C7 $C7 $80 $BC $C3 $C3 $00 $DE $21 $E1
	.db $40 $AF $10 $F0 $E0 $9F $00 $FF $E7 $58 $00 $7F $FF $60 $20 $7F
	.db $9F $13 $13 $1F $00 $00 $C0 $C0 $00 $00 $C0 $C0 $00 $00 $80 $80
	.db $30 $80 $00 $00 $F8 $00 $00 $80 $F8 $00 $00 $80 $C0 $38 $00 $80
	.db $80 $B8 $C0 $C0 $F0 $17 $F8 $18 $F0 $17 $78 $18 $FF $8F $BF $8F
	.db $7F $40 $4F $40 $3F $30 $30 $30 $0C $0C $08 $0F $0C $0C $08 $0F
	.db $07 $04 $04 $04 $0C $8C $0C $0C $7F $80 $7F $1F $FF $00 $FF $03
	.db $FF $00 $FF $00 $FF $00 $00 $00 $FF $FF $7F $FF $80 $00 $00 $00
	.db $00 $00 $00 $00 $80 $BC $C0 $C0 $82 $8C $F2 $F0 $FF $78 $FE $F8
	.db $FE $00 $FC $00 $FC $00 $00 $00 $C8 $C8 $80 $F8 $C8 $C0 $80 $F0
	.db $70 $40 $40 $40
	
; 13th entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 92C0 to 943F (384 bytes)	
_DATA_92C0_:	
	.dsb 35, $00
	.db $0F $01 $00 $00 $3E $00 $00 $00 $7F $00 $00 $00 $7F $00 $00 $00
	.db $FF $00 $00 $00 $FF $00 $00 $00 $FF $03 $03 $00 $FC $00 $00 $00
	.db $80 $00 $00 $00 $E0 $40 $00 $00 $B0 $20 $00 $00 $D0 $00 $00 $00
	.db $F8 $00 $00 $00 $F8 $00 $00 $00 $F8 $0C $3C $C0 $C0
	.dsb 21, $00
	.db $01 $00 $00 $10 $1F $10 $10 $20 $3E $21 $21 $3C $25 $02 $C2 $C9
	.db $A2 $14 $14 $C9 $A2 $14 $14 $C8 $A3 $14 $14 $B8 $CB $84 $84 $18
	.db $F1 $06 $06 $04 $70 $8B $8B $03 $39 $C4 $C5 $D0 $28 $00 $00 $D8
	.db $72 $50 $50 $D8 $72 $54 $54 $00 $FB $04 $04 $E0 $1A $84 $04 $60
	.db $98 $64 $04 $78 $88 $00 $08 $F0 $00 $00 $00 $20 $3C $23 $23 $13
	.db $1F $13 $13 $0F $0F $0F $0F $02 $03 $02 $03 $03 $02 $02 $02 $07
	.db $04 $04 $04 $07 $04 $04 $04 $48 $09 $4E $0E $80 $B8 $C7 $C7 $80
	.db $BC $C3 $C3 $00 $DE $21 $E1 $40 $AF $10 $F0 $E0 $9F $00 $FF $E7
	.db $58 $00 $7F $FF $60 $20 $7F $9F $13 $13 $1F $00 $00 $C0 $C0 $00
	.db $00 $C0 $C0 $00 $00 $80 $80 $30 $80 $00 $00 $F8 $00 $00 $80 $F8
	.db $00 $00 $80 $C0 $38 $00 $80 $80 $B8 $C0 $C0 $F0 $17 $F8 $18 $F0
	.db $17 $78 $18 $FF $8F $BF $8F $7F $40 $4F $40 $3F $30 $30 $30 $0C
	.db $0C $08 $0F $0C $0C $08 $0F $07 $04 $04 $04 $0C $8C $0C $0C $7F
	.db $80 $7F $1F $FF $00 $FF $03 $FF $00 $FF $00 $FF $00 $00 $00 $FF
	.db $FF $7F $FF $80 $00 $00 $00 $00 $00 $00 $00 $80 $BC $C0 $C0 $82
	.db $8C $F2 $F0 $FF $78 $FE $F8 $FE $00 $FC $00 $FC $00 $00 $00 $C8
	.db $C8 $80 $F8 $C8 $C0 $80 $F0 $70 $40 $40 $40
	
; 14th entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 9440 to 95BF (384 bytes)	
_DATA_9440_:	
	.db $00 $00 $00 $00 $00 $00 $00 $03 $00 $00 $00 $07 $00 $00 $00 $07
	.db $00 $00 $00 $0F $00 $00 $00 $0F $00 $00 $00 $0F $01 $01 $00 $0E
	.db $00 $00 $00 $F8 $10 $00 $00 $EE $04 $00 $00 $FB $02 $00 $00 $FD
	.db $00 $00 $00 $FF $00 $00 $00 $FF $00 $00 $00 $FF $80 $9F $60 $60
	.dsb 19, $00
	.db $80 $00 $00 $00 $80 $00 $00 $00 $80 $80 $80 $00 $00 $07 $04 $00
	.db $08 $09 $0C $02 $02 $09 $0C $02 $02 $09 $0C $02 $02 $06 $04 $01
	.db $01 $00 $07 $00 $00 $00 $0F $00 $00 $00 $1E $01 $01 $9A $A5 $40
	.db $40 $3B $4E $8A $8A $3B $4E $8A $8A $00 $7F $80 $80 $04 $7B $80
	.db $80 $84 $3B $44 $40 $41 $8E $30 $30 $3E $D0 $01 $11
	.dsb 13, $00
	.db $40 $80 $80 $00 $60 $80 $80 $00 $7C $80 $80 $04 $FC $04 $04 $B8
	.db $F8 $B8 $B8 $00 $1C $03 $03 $02 $3A $07 $07 $04 $FC $05 $05 $73
	.db $78 $70 $73 $0F $08 $08 $0F $07 $04 $04 $04 $0F $08 $08 $08 $09
	.db $08 $0E $0E $0E $F0 $01 $01 $00 $FE $01 $01 $00 $7F $80 $81 $80
	.db $1F $60 $E3 $E0 $1F $00 $FF $FC $03 $00 $FF $FF $09 $09 $7F $FF
	.db $5B $5B $7F $40 $40 $40 $40 $00 $80 $00 $80 $60 $80 $00 $80 $F0
	.db $00 $00 $00 $F0 $80 $00 $80 $06 $30 $C6 $C0 $07 $38 $C6 $C0 $03
	.db $3C $C2 $C0 $30 $11 $3E $1E $70 $13 $7C $1C $7F $0F $3F $0F $7F
	.db $40 $4F $40 $3F $30 $30 $30 $0F $0F $0E $0F $03 $03 $02 $03 $01
	.db $01 $01 $01 $A5 $24 $25 $25 $7F $80 $7F $0F $7F $80 $7F $03 $FF
	.db $00 $FC $00 $FF $03 $03 $03 $BC $BC $1C $FC $A0 $80 $00 $C0 $C0
	.db $80 $00 $80 $87 $81 $FD $F9 $FE $72 $F2 $F2 $FA $0A $C0 $CE $FA
	.db $38 $20 $3C $DC $D8 $D0 $D8
	.dsb 12, $00
	
; 15th entry of Pointer Table from 9D50 (indexed by unknown)	
; Data from 95C0 to 973F (384 bytes)	
_DATA_95C0_:	
	.db $00 $00 $00 $00 $00 $00 $00 $03 $00 $00 $00 $07 $00 $00 $00 $07
	.db $00 $00 $00 $0F $00 $00 $00 $0F $00 $00 $00 $0F $01 $01 $00 $0E
	.db $00 $00 $00 $F8 $10 $00 $00 $EE $04 $00 $00 $FB $02 $00 $00 $FD
	.db $00 $00 $00 $FF $00 $00 $00 $FF $00 $00 $00 $FF $80 $9F $60 $60
	.dsb 19, $00
	.db $80 $00 $00 $00 $80 $00 $00 $00 $80 $80 $80 $00 $00 $07 $04 $00
	.db $08 $09 $0C $02 $02 $09 $0C $02 $02 $09 $0C $02 $02 $06 $04 $01
	.db $01 $00 $07 $00 $00 $00 $0F $00 $00 $00 $1E $01 $01 $9A $A5 $40
	.db $40 $3B $4E $8A $8A $3B $4E $8A $8A $00 $7F $80 $80 $04 $7B $80
	.db $80 $84 $3B $44 $40 $41 $8E $30 $30 $3E $D0 $01 $11
	.dsb 13, $00
	.db $40 $80 $80 $00 $78 $80 $80 $08 $78 $88 $88 $B0 $F0 $B0 $B0 $40
	.db $40 $40 $40 $00 $7C $03 $03 $76 $7E $77 $77 $08 $08 $09 $09 $03
	.db $00 $00 $03 $07 $00 $00 $07 $07 $04 $04 $04 $0F $08 $08 $08 $09
	.db $08 $0E $0E $0E $F0 $01 $01 $00 $FE $01 $01 $00 $7F $80 $81 $80
	.db $1F $60 $E3 $E0 $1F $00 $FF $FC $03 $00 $FF $FF $09 $09 $7F $FF
	.db $5B $5B $7F $00 $00 $00 $00 $00 $80 $00 $80 $60 $80 $00 $80 $F0
	.db $00 $00 $00 $F0 $80 $00 $80 $06 $30 $C6 $C0 $07 $38 $C6 $C0 $03
	.db $3C $C2 $C0 $30 $11 $3E $1E $70 $13 $7C $1C $7F $0F $3F $0F $7F
	.db $40 $4F $40 $3F $30 $30 $30 $0F $0F $0E $0F $03 $03 $02 $03 $01
	.db $01 $01 $01 $A5 $24 $25 $25 $7F $80 $7F $0F $7F $80 $7F $03 $FF
	.db $00 $FC $00 $FF $03 $03 $03 $BC $BC $1C $FC $A0 $80 $00 $C0 $C0
	.db $80 $00 $80 $87 $81 $FD $F9 $FE $72 $F2 $F2 $FA $0A $C0 $CE $FA
	.db $38 $20 $3C $DC $D8 $D0 $D8
	.dsb 12, $00
	
; Data from 9740 to 9D3F (1536 bytes)	
_DATA_9740_:	
	.incbin "FilesA_9740-9D3F.dat"
	
; Pointer Table from 9D40 to 9D4F (8 entries, indexed by _RAM_C171_)	
_DATA_9D40_:	
	.dw _DATA_80C0_ _DATA_8180_ _DATA_8240_ _DATA_8300_ _RAM_CD69_ _RAM_CE29_ _RAM_CEE9_ _RAM_CFA9_
	
; Pointer Table from 9D50 to 9D6D (15 entries, indexed by unknown)	
_DATA_9D50_:	
	.dw _DATA_83C0_ _DATA_8540_ _RAM_D069_ _RAM_D1E9_ _DATA_86C0_ _DATA_8840_ _DATA_89C0_ _DATA_8B40_
	.dw _DATA_8CC0_ _DATA_8E40_ _DATA_8FC0_ _DATA_9140_ _DATA_92C0_ _DATA_9440_ _DATA_95C0_
	
; Data from 9D6E to 9EB5 (328 bytes)	
_DATA_9D6E_:	
	.db $2A $BC $2A $BC $49 $BC $49 $BC $65 $BC $65 $BC $7F $BC $65 $BC
	.db $2B $BD $2B $BD $2B $BD $2B $BD $D3 $BC $F1 $BC $D3 $BC $10 $BD
	.db $94 $BC $94 $BC $94 $BC $94 $BC $49 $BD $49 $BD $49 $BD $49 $BD
	.db $67 $BD $84 $BD $67 $BD $84 $BD $D3 $BC $F1 $BC $D3 $BC $10 $BD
	.db $A2 $BD $A2 $BD $A2 $BD $A2 $BD $B4 $BC $B4 $BC $B4 $BC $B4 $BC
	.db $65 $BC $65 $BC $7F $BC $65 $BC $D3 $BC $F1 $BC $D3 $BC $10 $BD
	.db $58 $BE $58 $BE $58 $BE $58 $BE $49 $BD $49 $BD $49 $BD $49 $BD
	.db $8C $BE $8C $BE $8C $BE $8C $BE $3A $BE $3A $BE $3A $BE $3A $BE
	.db $2A $BC $2A $BC $49 $BC $49 $BC $C2 $BD $C2 $BD $C2 $BD $C2 $BD
	.db $1C $BE $1C $BE $FF $BD $FF $BD $D3 $BC $F1 $BC $D3 $BC $10 $BD
	.db $94 $BC $94 $BC $94 $BC $94 $BC $67 $BD $84 $BD $67 $BD $84 $BD
	.db $A2 $BD $A2 $BD $A2 $BD $A2 $BD $D3 $BC $F1 $BC $D3 $BC $10 $BD
	.db $65 $BC $65 $BC $7F $BC $65 $BC $2B $BD $2B $BD $2B $BD $2B $BD
	.db $E1 $BD $C2 $BD $C2 $BD $C2 $BD $D3 $BC $F1 $BC $D3 $BC $10 $BD
	.db $76 $BE $76 $BE $76 $BE $76 $BE $2A $BC $2A $BC $49 $BC $49 $BC
	.db $B4 $BC $B4 $BC $B4 $BC $B4 $BC $3A $BE $3A $BE $3A $BE $3A $BE
	.db $1C $BE $1C $BE $FF $BD $FF $BD $94 $BC $94 $BC $94 $BC $94 $BC
	.db $67 $BD $84 $BD $67 $BD $84 $BD $D3 $BC $F1 $BC $D3 $BC $10 $BD
	.db $C2 $BD $C2 $BD $C2 $BD $C2 $BD $65 $BC $65 $BC $7F $BC $65 $BC
	.db $A2 $BD $A2 $BD $A2 $BD $A2 $BD $D3 $BC $F1 $BC $D3 $BC $10 $BD
	.db $A8 $BE $A8 $BE $A8 $BE $A8 $BE
	
; Data from 9EB6 to A59F (1770 bytes)	
_DATA_9EB6_:	
	.incbin "FilesB_9EB6-A59F.dat"
	
; 1st entry of Pointer Table from 7CCA (indexed by unknown)	
; Data from A5A0 to B1A4 (3077 bytes)	
_DATA_A5A0_:	
	.incbin "FilesC_A5A0-B1A4.dat"
	
; 2nd entry of Pointer Table from 7E49 (indexed by unknown)	
; Data from B1A5 to BB3D (2457 bytes)	
_DATA_B1A5_:	
	.incbin "FilesD_B1A5-BB3D.dat"
	
; Data from BB3E to BFFF (1218 bytes)	
_DATA_BB3E_:	
	.incbin "FilesE_BB3E-BFFF.dat"
	
.BANK 3	
.ORG $0000	
	
	; Data from C000 to C2FF (768 bytes)
	.db $00 $00 $00 $00 $00 $00 $00 $00 $FF $00 $FF $00 $7F $36 $7F $36
	.db $FF $00 $FF $00 $08 $08 $08 $0F $07 $07 $07 $07 $08 $08 $08 $0F
	.db $00 $00 $00 $00 $00 $00 $00 $00 $FF $00 $FF $00 $FF $DB $FF $DB
	.db $FF $00 $FF $00 $07 $07 $07 $FF $FB $FB $FB $FC $07 $07 $07 $FF
	.db $00 $00 $00 $00 $00 $00 $00 $00 $FF $00 $FF $00 $FE $6C $FE $6C
	.db $FF $00 $FF $00 $E0 $E0 $E0 $E0 $C0 $40 $40 $30 $E0 $E0 $E0 $E0
	.db $07 $07 $07 $07 $08 $08 $08 $0F $07 $07 $07 $07 $08 $08 $08 $0F
	.db $00 $00 $00 $00 $FF $00 $FF $00 $7F $36 $7F $36 $FF $00 $FF $00
	.db $FB $FB $FB $FC $07 $07 $07 $FF $FB $FB $FB $FC $07 $07 $07 $FF
	.db $FB $FB $FB $FC $FF $00 $FF $00 $FF $DB $FF $DB $FF $00 $FF $00
	.db $C0 $40 $40 $30 $E0 $E0 $E0 $E0 $C0 $40 $40 $30 $E0 $E0 $E0 $E0
	.db $C0 $40 $40 $30 $FF $00 $FF $00 $FE $6C $FE $6C $FF $00 $FF $00
	.db $FF $00 $FF $00 $7F $36 $7F $36 $FF $00 $FF $00 $00 $00 $00 $00
	.db $08 $08 $08 $0F $07 $07 $07 $07 $00 $00 $00 $00 $08 $08 $08 $0F
	.db $FF $00 $FF $00 $FF $DB $FF $DB $FF $00 $FF $00 $FF $FF $FF $FF
	.db $FF $F9 $E0 $F9 $0F $09 $00 $F9 $F3 $F3 $F3 $FC $FF $FF $EF $FF
	.db $FF $00 $FF $00 $FE $6C $FE $6C $FF $00 $FF
	.dsb 17, $00
	.db $C0 $40 $40 $30 $07 $07 $07 $07 $00 $00 $00 $00 $08 $08 $08 $0F
	.db $07 $07 $07 $07 $00 $00 $00 $00 $08 $08 $08 $0F $07 $07 $07 $07
	.db $00 $00 $00 $00 $0F $09 $00 $F9 $F3 $F3 $F3 $FC $FF $FF $EF $FF
	.db $0F $09 $00 $F9 $F3 $F3 $F3 $FC $FF $FF $EF $FF $0F $09 $00 $F9
	.db $F3 $F3 $F3 $FC $E0 $E0 $E0 $E0 $00 $00 $00 $00 $C0 $40 $40 $30
	.db $E0 $E0 $E0 $E0 $00 $00 $00 $00 $C0 $40 $40 $30 $E0 $E0 $E0 $E0
	.db $00 $00 $00 $00 $00 $00 $00 $0F $07 $07 $07 $07
	.dsb 12, $00
	.db $FF $00 $FF $00 $7F $36 $7F $36 $FF $00 $FF $00 $FF $FF $EF $FF
	.db $0F $09 $00 $F9 $F3 $F3 $F3 $FC $FF $FF $EF $FF $FF $F9 $E0 $F9
	.db $FF $00 $FF $00 $FF $DB $FF $DB $FF $00 $FF $00 $C0 $40 $40 $30
	.db $E0 $E0 $E0 $E0 $00 $00 $00 $00 $C0 $40 $40 $30 $E0 $E0 $E0 $E0
	.db $FF $00 $FF $00 $FE $6C $FE $6C $FF $00 $FF $00 $00 $00 $00 $00
	.db $01 $00 $01 $01 $04 $00 $07 $07 $18 $00 $1F $1F $30 $01 $3E $3E
	.db $24 $00 $3F $3F $70 $00 $7F $7F $C0 $80 $FF $FF $C0 $00 $FF $FF
	.db $08 $02 $FD $FD $11 $02 $FC $B0 $00 $03 $FC $38 $10 $01 $FE $FC
	.db $04 $00 $FF $FF $03 $E0 $1F $1F $02 $F4 $0B $0B $00 $00 $00 $00
	.db $00 $00 $E0 $E0 $00 $80 $78 $78 $80 $40 $38 $28 $00 $40 $BC $2C
	.db $00 $C0 $3E $3E $10 $8C $72 $72 $A0 $06 $F9 $F1 $F0 $C0 $FF $FF
	.db $F2 $C0 $FF $FF $69 $40 $7F $7F $60 $44 $7B $79 $C0 $80 $FF $FC
	.db $E8 $80 $FF $FF $E2 $80 $FF $FF $FC $C0 $FF $FF $08 $60 $9F $1F
	.db $08 $03 $FC $9C $14 $03 $FC $FC $60 $01 $FE $FC $04 $00 $FF $FF
	.db $23 $18 $E7 $67 $20 $08 $F7 $75 $38 $01 $FE $FE $30 $00 $FF $7F
	.db $1A $80 $7F $7F $2C $40 $BF $3F $01 $C0 $3F $1F $15 $80 $7F $7F
	.db $01 $30 $CF $CF $42 $10 $EE $EE $2B $81 $7F $7F $73 $40 $7F $7F
	.db $6A $40 $7F $7F $7D $60 $7F $7F $3E $30 $3F $3F $1F $18 $1F $1F
	.db $0F $0F $0F $0F $01 $01 $01 $01 $00 $00 $00 $00 $22 $00 $FF $FF
	.db $C9 $00 $FF $FF $37 $00 $FF $FF $46 $00 $FF $FF $AB $00 $FF $FF
	.db $FD $00 $FF $FF $FF $E0 $FF $FF $FF $FF $FF $FF $27 $01 $FF $FF
	.db $86 $02 $FE $FE $1E $02 $FE $FE $7C $04 $FC $FC $B8 $08 $F8 $F8
	.db $F0 $10 $F0 $F0 $E0 $E0 $E0 $E0 $00 $00 $00 $00
	
; Data from C300 to DB69 (6250 bytes)	
_DATA_C300_:	
	.incbin "FilesF_C300-DB69.dat"
	
; Data from DB6A to DEBA (849 bytes)	
_DATA_DB6A_:	
	.db $7C $00 $05 $CE $7C $00 $01 $00 $78 $00 $05 $38 $7C $00 $01 $00
	.db $7C $FF $CE $1C $38 $70 $FE $00 $01 $00 $7C $FF $CE $1C $FF $CE
	.db $7C $00 $01 $00 $0C $1C $3C $6C $CC $FE $0C $00 $01 $00 $FC $E0
	.db $FC $FF $0E $CE $7C $00 $01 $00 $7C $C0 $FC $00 $03 $CE $7C $00
	.db $01 $00 $FE $C6 $CE $1C $00 $03 $18 $00 $01 $00 $7C $FF $CE $7C
	.db $FF $CE $7C $00 $01 $00 $7C $00 $03 $CE $7E $0E $7C $00 $01 $00
	.db $00 $06 $E6 $7C $00 $01 $00 $FC $00 $03 $E6 $FC $FF $E0 $00 $01
	.db $00 $00 $03 $E6 $FE $00 $03 $E6 $00 $01 $00 $00 $07 $38 $00 $01
	.db $00 $7C $FF $E6 $E0 $EE $E6 $7C $00 $01 $00 $FC $FF $E6 $FC $FF
	.db $E6 $FC $00 $01 $00 $7C $FF $EE $E0 $FF $EE $7C $00 $01 $00 $FC
	.db $00 $05 $E6 $FC $00 $01 $00 $FE $FF $E0 $FC $FF $E0 $FE $00 $01
	.db $00 $FE $FF $E0 $FC $00 $03 $E0 $00 $01 $00 $38 $64 $FF $E6 $FE
	.db $FF $E6 $00 $01 $00 $C6 $EE $7C $38 $7C $EE $C6 $00 $01 $00 $FF
	.db $C6 $FE $7C $00 $03 $38 $00 $01 $00 $00 $04 $0E $FF $CE $7C $00
	.db $01 $00 $E0 $E6 $EC $FF $F8 $EC $E6 $00 $01 $00 $00 $06 $E0 $FE
	.db $00 $01 $00 $6C $FE $00 $05 $D6 $00 $01 $00 $C6 $E6 $F6 $FE $EE
	.db $E6 $E2 $00 $01 $00 $7C $00 $05 $E6 $7C $00 $01 $00 $FF $FE $1C
	.db $38 $70 $FF $FE $00 $01 $00 $7C $00 $03 $C6 $F6 $CE $7C $00 $01
	.db $00 $FC $00 $03 $E6 $FC $FF $E6 $00 $01 $00 $7C $E6 $E0 $7C $06
	.db $E6 $7C $00 $01 $00 $FF $FE $00 $05 $38 $00 $01 $00 $00 $05 $D6
	.db $FE $6C $00 $01 $00 $00 $05 $C6 $6C $38 $00 $01 $00 $38 $44 $BA
	.db $A2 $BA $44 $38 $FF $00 $FF $18 $00 $01 $00 $FF $18 $FF $00 $03
	.db $06 $0C $18 $30 $60 $C0 $00 $01 $00 $7C $FF $C6 $FC $FF $C0 $7E
	.db $00 $05 $00 $FF $18 $FF $00 $7C $FF $06 $7E $FF $C6 $7E $00 $01
	.db $00 $00 $03 $3C $18 $00 $01 $00 $FF $18 $FF $00 $6C $FF $7C $38
	.db $10 $00 $05 $00 $FF $7C $00 $07 $00 $1C $0C $18 $20 $00 $00 $00
	.db $FF $00 $00 $5A $00 $6C $FF $7C $38 $10 $00 $12 $00 $00 $00 $00
	.db $FF $00 $00 $71 $00 $00 $00 $02 $31 $00 $04 $21 $02 $00 $01 $00
	.db $00 $06 $04 $02 $00 $01 $00 $02 $31 $21 $C2 $04 $08 $FF $00 $02
	.db $31 $21 $C2 $11 $01 $02 $00 $01 $00 $00 $03 $02 $12 $22 $01 $02
	.db $00 $01 $00 $02 $1C $02 $F1 $FF $01 $02 $00 $01 $00 $02 $3C $02
	.db $31 $FF $21 $02 $00 $01 $00 $01 $39 $21 $02 $00 $03 $04 $00 $01
	.db $00 $02 $31 $21 $02 $31 $21 $02 $00 $01 $00 $02 $31 $FF $21 $01
	.db $71 $02 $00 $01 $00 $00 $06 $11 $02 $00 $01 $00 $02 $19 $FF $11
	.db $02 $FF $10 $00 $01 $00 $00 $03 $11 $01 $19 $FF $11 $00 $01 $00
	.db $00 $07 $04 $00 $01 $00 $02 $19 $11 $17 $FF $11 $02 $00 $01 $00
	.db $02 $19 $11 $02 $19 $11 $02 $00 $01 $00 $02 $FF $11 $1F $FF $11
	.db $02 $00 $01 $00 $02 $19 $00 $04 $11 $02 $00 $01 $00 $01 $1E $10
	.db $02 $1C $10 $01 $00 $01 $00 $01 $1E $10 $02 $1C $FF $10 $00 $01
	.db $00 $04 $1A $FF $11 $01 $19 $11 $00 $01 $00 $21 $11 $02 $04 $02
	.db $11 $21 $00 $01 $00 $FF $21 $01 $02 $00 $03 $04 $00 $01 $00 $00
	.db $04 $01 $FF $21 $02 $00 $01 $00 $10 $11 $12 $FF $04 $12 $11 $00
	.db $01 $00 $00 $06 $10 $01 $00 $01 $00 $12 $01 $00 $05 $29 $00 $01
	.db $00 $21 $11 $09 $01 $00 $03 $11 $00 $01 $00 $02 $19 $00 $04 $11
	.db $02 $00 $01 $00 $FF $01 $02 $04 $08 $FF $01 $00 $01 $00 $02 $39
	.db $FF $21 $01 $31 $02 $00 $01 $00 $02 $19 $FF $11 $02 $19 $11 $00
	.db $01 $00 $02 $19 $16 $02 $79 $01 $02 $00 $01 $00 $FF $01 $06 $00
	.db $04 $04 $00 $01 $00 $00 $05 $29 $01 $12 $00 $01 $00 $00 $05 $21
	.db $12 $04 $FF $00 $38 $40 $5D $41 $1A $04 $18 $FF $00 $04 $1C $00
	.db $01 $00 $04 $1C $FF $00 $01 $02 $04 $08 $10 $20 $FF $00 $38 $21
	.db $02 $3C $20 $01 $00 $05 $00 $FF $04 $0C $FF $00 $39 $FF $01 $39
	.db $21 $01 $00 $01 $00 $00 $03 $02 $04 $18 $FF $04 $0C $FF $00 $FF
	.db $02 $04 $08 $10 $00 $05 $00 $02 $3E $00 $07 $00 $02 $06 $18 $00
	.db $00
	
; 1st entry of Pointer Table from 505D (indexed by _RAM_C170_)	
; Data from DEBB to E03A (384 bytes)	
_DATA_DEBB_:	
	.db $00 $03 $00 $03 $00 $0F $00 $0F $00 $1F $00 $1F $01 $3E $00 $3F
	.db $02 $3C $01 $3F $04 $78 $03 $7F $08 $70 $07 $7F $2C $50 $03 $7F
	.db $18 $F8 $18 $F8 $04 $FC $04 $FC $34 $CC $34 $CC $6A $86 $7A $86
	.db $FA $06 $7A $86 $7A $06 $B2 $CE $72 $0E $82 $FE $32 $0E $C2 $FE
	.dsb 32, $00
	.db $2B $40 $14 $74 $21 $54 $00 $6A $01 $3E $0A $20 $00 $1F $00 $00
	.db $04 $1B $04 $00 $04 $0B $14 $10 $00 $00 $0F $0F $00 $07 $00 $00
	.db $3D $03 $C1 $FF $99 $07 $61 $7F $99 $47 $21 $3F $19 $C7 $21 $3F
	.db $1D $C3 $21 $3F $1D $83 $61 $7F $0D $73 $81 $8F $0C $F3 $08 $07
	.dsb 28, $00
	.db $80 $80 $80 $80 $00 $0B $0D $09 $00 $1D $1F $1D $00 $00 $0F $00
	.db $07 $00 $07 $07 $02 $1C $1F $1E $0D $12 $0F $03 $06 $31 $0F $09
	.db $00 $31 $0F $0F $0A $E5 $98 $8B $12 $8D $70 $13 $60 $13 $EC $61
	.db $8D $03 $FD $8D $79 $07 $F9 $79 $86 $7A $FE $FA $07 $F9 $FF $F9
	.db $0F $10 $FF $F0 $80 $80 $80 $80 $80 $80 $80 $80 $40 $C0 $40 $C0
	.db $80 $80 $80 $80
	.dsb 12, $00
	.db $80 $80 $80 $80 $03 $32 $0F $0E $03 $63 $1F $1F $04 $64 $1C $1C
	.db $38 $38 $38 $38 $00 $00 $00 $00 $00 $01 $00 $00 $00 $01 $00 $00
	.db $00 $00 $00 $00 $1F $C3 $3F $23 $0F $C9 $3F $39 $0E $CE $3E $3E
	.db $08 $C8 $38 $38 $08 $C8 $38 $38 $08 $C8 $38 $38 $10 $90 $70 $70
	.db $E0 $E0 $E0 $E0
	.dsb 32, $00
	
; 2nd entry of Pointer Table from 505D (indexed by _RAM_C170_)	
; Data from E03B to E4BA (1152 bytes)	
_DATA_E03B_:	
	.db $00 $03 $00 $03 $00 $0F $00 $0F $00 $1F $00 $1F $01 $3E $00 $3F
	.db $02 $3C $01 $3F $04 $78 $03 $7F $08 $70 $07 $7F $2C $50 $03 $7F
	.db $18 $F8 $18 $F8 $04 $FC $04 $FC $34 $CC $34 $CC $6E $82 $7A $86
	.db $FE $02 $7A $86 $7A $06 $B2 $CE $72 $0E $82 $FE $32 $0E $C2 $FE
	.dsb 32, $00
	.db $2B $40 $14 $74 $21 $54 $00 $6A $01 $3E $0A $20 $00 $1F $00 $00
	.db $04 $1B $04 $00 $04 $0B $14 $10 $00 $00 $0F $0F $00 $07 $00 $00
	.db $3D $03 $C1 $FF $99 $07 $61 $7F $99 $47 $21 $3F $19 $C7 $21 $3F
	.db $1D $C3 $21 $3F $3D $83 $41 $7F $0D $73 $81 $8F $04 $FB $00 $07
	.dsb 28, $00
	.db $80 $80 $80 $80 $00 $0B $0D $0D $00 $1F $1D $1D $00 $00 $0F $00
	.db $07 $00 $07 $07 $00 $01 $06 $00 $00 $07 $06 $06 $02 $0D $0E $0C
	.db $02 $1D $1E $1C $06 $F1 $8C $87 $0A $85 $78 $0B $72 $01 $FE $73
	.db $8D $43 $BD $8D $01 $87 $79 $79 $0E $92 $7E $72 $07 $99 $7F $79
	.db $12 $9E $7E $7E $80 $80 $80 $80 $80 $80 $80 $80 $40 $C0 $40 $C0
	.db $80 $80 $80 $80
	.dsb 17, $00
	.db $0B $0C $0C $00 $0B $04 $04 $00 $0C $03 $03 $00 $0C $03 $03 $00
	.db $0C $03 $03 $00 $1C $03 $03 $01 $19 $07 $07 $0E $0E $0E $0E $1C
	.db $9C $7C $7C $20 $20 $E0 $E0 $C0 $C0 $C0 $C0
	.dsb 12, $80
	.dsb 45, $00
	.db $03 $00 $03 $00 $07 $00 $07 $00 $0F $00 $0F $00 $0F $00 $0F $01
	.db $1E $00 $1F $02 $1C $01 $1F $0B $14 $00 $1F $06 $FE $06 $FE $01
	.db $FF $01 $FF $0D $F3 $0D $F3 $5A $A1 $1E $E1 $BE $01 $5E $E1 $1C
	.db $03 $EC $F3 $18 $47 $A0 $BF $0C $E3 $10 $1F
	.dsb 12, $00
	.dsb 20, $80
	.db $08 $16 $00 $19 $0A $15 $01 $18 $03 $14 $08 $08 $00 $77 $08 $08
	.db $01 $36 $09 $08 $00 $1B $04 $04 $00 $0C $03 $03 $00 $07 $00 $00
	.db $2F $80 $10 $5F $36 $C1 $48 $0F $77 $80 $08 $0F $06 $F1 $08 $0E
	.db $C4 $33 $C8 $0C $80 $67 $98 $18 $00 $1E $E1 $E1 $01 $FC $02 $03
	.db $40 $C0 $40 $C0 $40 $C0 $40 $40 $00 $F0 $00 $00 $10 $F0 $10 $10
	.db $20 $A0 $60 $60 $40 $40 $C0 $C0 $40 $C0 $40 $C0 $40 $C0 $40 $C0
	.db $00 $07 $06 $06 $01 $0E $0F $0E $01 $0E $0F $0E $00 $07 $00 $00
	.db $00 $07 $00 $00 $00 $07 $07 $07 $00 $0F $0F $0F $00 $09 $0F $0F
	.db $03 $70 $EC $EF $07 $F8 $D8 $DF $06 $F1 $F8 $FF $06 $F1 $08 $0F
	.db $04 $F3 $08 $0F $1F $E7 $FF $E7 $0E $F2 $FE $F2 $07 $89 $FF $F9
	.db $20 $E0 $20 $E0 $20 $E0 $20 $E0 $10 $F0 $10 $F0 $20 $E0 $20 $E0
	.db $C0 $C0 $C0 $C0
	.dsb 13, $00
	.db $0E $09 $09 $00 $07 $00 $00 $00 $07 $00 $00 $00 $07 $08 $00 $0C
	.db $03 $1C $0C $10 $03 $30 $10 $08 $00 $3F $08 $2F $00 $EF $2F $06
	.db $72 $8E $8A $04 $74 $8C $8C $04 $74 $8F $8C $07 $74 $8F $8F $08
	.db $68 $98 $98 $08 $68 $9F $98 $07 $00 $FF $07 $F8 $00 $F8 $F8
	.dsb 14, $00
	.db $E0 $00 $60 $00 $F0 $60 $90 $00 $90 $90 $00 $00 $E0 $00 $E0 $00
	.db $E0 $E0 $00 $00 $00 $00 $00 $03 $00 $03 $00 $07 $00 $07 $00 $0F
	.db $00 $0F $00 $0F $00 $0F $01 $1E $00 $1F $02 $1C $01 $1F $0B $14
	.db $00 $1F $06 $FE $06 $FE $01 $FF $01 $FF $0D $F3 $0D $F3 $5A $A1
	.db $1E $E1 $BE $01 $5E $E1 $1C $03 $EC $F3 $18 $47 $A0 $BF $0C $E3
	.db $10 $1F
	.dsb 12, $00
	.dsb 20, $80
	.db $08 $16 $00 $19 $0A $15 $01 $18 $03 $04 $08 $08 $00 $07 $08 $08
	.db $00 $07 $08 $08 $00 $03 $04 $04 $00 $04 $03 $03 $00 $0F $00 $00
	.db $2F $80 $10 $5F $36 $C1 $48 $0F $77 $80 $08 $0F $07 $F0 $08 $0F
	.db $87 $70 $88 $0F $07 $E0 $18 $1F $03 $1C $E0 $E3 $01 $FE $00 $01
	.db $40 $C0 $40 $C0 $40 $C0 $40 $C0 $40 $C0 $40 $C0 $40 $C0 $40 $C0
	.db $40 $C0 $40 $C0 $40 $C0 $40 $C0 $40 $C0 $40 $C0 $40 $C0 $40 $C0
	.db $00 $1F $06 $06 $01 $1E $0F $0E $01 $1E $0F $0E $00 $37 $08 $08
	.db $00 $37 $08 $08 $00 $27 $17 $17 $00 $09 $2F $2F $00 $0E $09 $09
	.db $01 $76 $E8 $E9 $00 $FB $DC $DC $00 $F3 $FC $FC $04 $F1 $0A $0E
	.db $04 $F1 $0A $0E $1C $E5 $FE $E6 $0E $F2 $FE $F2 $07 $F9 $FF $F9
	.db $20 $E0 $20 $E0 $20 $E0 $20 $E0 $10 $F0 $10 $F0 $20 $E0 $20 $60
	.db $00 $C0
	.dsb 15, $00
	.db $0F $08 $08 $00 $07 $00 $00 $00 $07 $00 $00 $00 $03 $0C $00 $0C
	.db $03 $1C $0C $10 $00 $30 $10 $08 $00 $3F $08 $2F $00 $EF $2F $06
	.db $8A $FE $FA $04 $74 $8C $8C $04 $74 $8F $8C $07 $74 $8F $8F $08
	.db $68 $98 $98 $08 $68 $9F $98 $0F $68 $9F $9F $F8 $00 $F8 $F8
	.dsb 14, $00
	.db $E0 $00 $60 $00 $F0 $60 $90 $00 $90 $90 $00 $00 $E0 $00 $E0 $00
	.db $E0 $E0
	
; Data from E4BB to FFFF (6981 bytes)	
_DATA_E4BB_:	
	.incbin "FilesG_E4BB-FFFF.dat"
	
.BANK 4	
.ORG $0000	
	
	; Data from 10000 to 11D3F (7488 bytes)
	.incbin "FilesH_10000-11D3F.dat"
	
; 1st entry of Pointer Table from F9D (indexed by _RAM_C109_)	
; Data from 11D40 to 11FBC (637 bytes)	
_DATA_11D40_:	
	.db $05 $9D $B8 $B9 $BC $BD $06 $9A $9B $BA $BB $BE $BF $07 $79 $7C
	.db $55 $55 $C0 $C1 $C4 $08 $7B $7E $55 $55 $55 $C3 $C6 $7F $0E $60
	.db $61 $64 $65 $68 $69 $6C $6D $70 $71 $74 $75 $78 $9F $0D $62 $63
	.db $66 $67 $6A $6B $6E $6F $72 $73 $76 $77 $7A $0C $A0 $A1 $A4 $A5
	.db $A8 $A9 $AC $AD $B0 $B1 $B4 $B5 $1A $FF $00 $00 $00 $00 $00 $00
	.db $00 $A2 $A3 $A6 $A7 $AA $AB $AE $AF $B2 $B3 $B6 $B7 $00 $00 $00
	.db $00 $00 $FF $1B $F8 $F9 $00 $00 $00 $00 $00 $E0 $E1 $E4 $E5 $E8
	.db $E9 $EC $ED $F0 $F1 $F4 $F5 $00 $00 $00 $00 $00 $F8 $F9 $FF $1B
	.db $FA $FB $FD $00 $00 $00 $00 $E2 $E3 $E6 $E7 $EA $EB $EE $EF $36
	.db $F3 $F6 $F7 $C2 $00 $00 $00 $00 $FA $FB $FD $1C $FC $DC $DE $FD
	.db $00 $00 $00 $00 $00 $C5 $C8 $C9 $CC $CD $D0 $36 $36 $36 $D1 $36
	.db $DD $00 $00 $00 $FC $DC $DE $FD $1D $F8 $F9 $00 $FF $00 $00 $00
	.db $00 $00 $C7 $CA $CB $CE $CF $F8 $F9 $36 $D2 $D3 $F2 $DF $FE $00
	.db $00 $FF $FE $F8 $F9 $FF $1E $FF $FA $FB $D4 $D5 $D8 $D9 $D4 $D5
	.db $D8 $D9 $D4 $D5 $D8 $D9 $FA $FB $D8 $D9 $D4 $D5 $D8 $D9 $D4 $D5
	.db $D8 $D9 $FA $FB $FD $1E $FC $DC $DE $D6 $D7 $DA $DB $D6 $D7 $DA
	.db $DB $D6 $D7 $DA $DB $DC $DE $DA $DB $D6 $D7 $DA $DB $D6 $D7 $DA
	.db $DB $DC $DE $FD $18 $40 $41 $44 $45 $48 $49 $4C $4D $50 $51 $54
	.db $45 $50 $2D $30 $31 $50 $2D $28 $29 $50 $2D $20 $21 $18 $42 $43
	.db $46 $47 $4A $4B $4E $4F $90 $53 $56 $57 $90 $2F $32 $33 $90 $2C
	.db $2A $2B $90 $2C $22 $23 $18 $80 $81 $84 $85 $88 $89 $8C $8D $90
	.db $91 $94 $95 $90 $35 $38 $39 $90 $2E $24 $25 $90 $2E $1C $1D $18
	.db $82 $83 $86 $87 $8A $8B $8E $8F $92 $93 $96 $97 $92 $37 $3A $3B
	.db $92 $37 $26 $27 $92 $34 $1E $1F $0C $50 $2D $20 $21 $48 $49 $4C
	.db $4D $58 $59 $5C $5D $0C $90 $2C $22 $23 $4A $4B $4E $4F $5A $5B
	.db $5E $5F $0B $90 $2E $3C $3D $88 $89 $8C $8D $98 $99 $9C $0B $92
	.db $37 $3E $3F $8A $8B $8E $8F $00 $92 $9E $18 $0B $0A $05 $0C $00
	.db $01 $00 $0B $15 $14 $16 $00 $05 $07 $14 $13 $07 $00 $0F $0A $07
	.db $07 $18 $17 $02 $18 $13 $18 $0B $0A $05 $0C $00 $02 $00 $0B $15
	.db $14 $16 $00 $05 $07 $14 $13 $07 $00 $0F $0A $07 $07 $18 $17 $14
	.db $18 $13 $0D $0E $0D $17 $14 $15 $00 $0E $14 $1A $12 $03 $00 $00
	.db $00 $00 $00 $00 $14 $00 $00 $04 $00 $05 $12 $0E $14 $7D $1B $05
	.db $10 $19 $0B $1B $00 $01 $09 $08 $52 $1A $00 $00 $13 $12 $0B $13
	.db $18 $0E $13 $14 $1A $1A $12 $11 $00 $0E $14 $1A $12 $03 $00 $00
	.db $00 $00 $00 $00 $1C
	.dsb 10, $00
	.db $04 $00 $05 $12 $0E $14 $00 $01 $09 $08 $06 $00 $00 $00 $00 $00
	.db $00 $00 $20
	.dsb 32, $00
	.db $09 $0F $15 $1B $13 $00 $1D $24 $13 $20 $08 $11 $1D $1C $22 $0E
	.db $1C $0B $13 $07 $20 $13 $21 $22 $15 $20 $22
	
; Data from 11FBD to 1263C (1664 bytes)	
_DATA_11FBD_:	
	.incbin "FilesI_11FBD-1263C.dat"
	
; Data from 1263D to 1332C (3312 bytes)	
_DATA_1263D_:	
	.incbin "FilesJ_1263D-1332C.dat"
	
; Data from 1332D to 13F53 (3111 bytes)	
_DATA_1332D_:	
	.incbin "FilesK_1332D-13F53.dat"
	
; Data from 13F54 to 13FFF (172 bytes)	
_DATA_13F54_:	
	.db $EE $00 $EE $00 $EE $00 $EE $00 $EE $00 $EE $00 $EE $00 $EE $00
	.db $EE $00 $EE $00 $EE $00 $EE $00 $EE $00 $EE $00 $00 $00 $00 $00
	.db $E0 $00 $E0 $00 $E0 $00 $E0 $00 $E0 $00 $E0 $00 $E0 $00 $E0 $00
	.db $E0 $00 $E0 $00 $E0 $00 $E0 $00 $E0 $00 $E0 $00 $00 $00 $00 $00
	.db $EE $EE $00 $00 $EE $EE $00 $00 $EE $EE $00 $00 $EE $EE $00 $00
	.db $EE $EE $00 $00 $EE $EE $00 $00 $EE $EE $00 $00 $00 $00 $00 $00
	.db $E0 $E0 $00 $00 $E0 $E0 $00 $00 $E0 $E0 $00 $00 $E0 $E0 $00 $00
	.db $E0 $E0 $00 $00 $E0 $E0 $00 $00 $E0 $E0 $00 $00 $00 $00 $00 $00
	.dsb 44, $FF
	
.BANK 5	
.ORG $0000	
	
	; Data from 14000 to 17AD5 (15062 bytes)
	.incbin "FilesL_14000-17AD5.dat"
	
; Data from 17AD6 to 17CF1 (540 bytes)	
_DATA_17AD6_:	
	.db $01 $00 $80 $00 $00 $00 $00 $00 $58 $26 $00 $12 $80 $00 $00 $00
	.db $00 $00 $58 $26 $01 $00 $00 $00 $80 $00 $00 $00 $48 $26 $01 $00
	.db $78 $00 $00 $00 $00 $00 $80 $26 $01 $00 $88 $FF $00 $00 $00 $00
	.db $80 $26 $80 $12 $00 $00 $00 $00 $F8 $FF $80 $26 $01 $00 $00 $00
	.db $00 $00 $00 $00 $80 $26 $01 $00 $80 $00 $00 $00 $00 $00 $58 $28
	.db $00 $12 $80 $00 $00 $00 $00 $00 $58 $28 $01 $00 $00 $00 $80 $00
	.db $00 $00 $48 $28 $01 $00 $78 $00 $00 $00 $00 $00 $80 $28 $01 $00
	.db $88 $FF $00 $00 $00 $00 $80 $28 $80 $12 $00 $00 $00 $00 $F8 $FF
	.db $80 $28 $01 $00 $00 $00 $00 $00 $00 $00 $80 $28 $01 $00 $80 $00
	.db $00 $00 $00 $00 $58 $2A $00 $12 $80 $00 $00 $00 $00 $00 $58 $2A
	.db $01 $00 $00 $00 $80 $00 $00 $00 $4C $2A $01 $00 $78 $00 $00 $00
	.db $00 $00 $80 $2A $01 $00 $88 $FF $00 $00 $00 $00 $80 $2A $80 $12
	.db $00 $00 $00 $00 $F8 $FF $80 $2A $01 $00 $00 $00 $00 $00 $00 $00
	.db $80 $2A $01 $00 $80 $00 $00 $00 $00 $00 $58 $2C $00 $12 $80 $00
	.db $00 $00 $00 $00 $58 $2C $01 $00 $00 $00 $80 $00 $00 $00 $48 $2C
	.db $01 $00 $78 $00 $00 $00 $00 $00 $80 $2C $01 $00 $88 $FF $00 $00
	.db $00 $00 $80 $2C $80 $12 $00 $00 $00 $00 $F8 $FF $80 $2C $01 $00
	.db $00 $00 $00 $00 $00 $00 $80 $2C $21 $00 $80 $00 $00 $00 $00 $00
	.db $58 $2E $20 $12 $80 $00 $00 $00 $00 $00 $58 $2E $21 $00 $00 $00
	.db $80 $00 $00 $00 $48 $2E $21 $00 $78 $00 $00 $00 $00 $00 $80 $2E
	.db $21 $00 $88 $FF $00 $00 $00 $00 $80 $2E $A0 $12 $00 $00 $00 $00
	.db $F8 $FF $80 $2E $21 $00 $00 $00 $00 $00 $00 $00 $80 $2E $01 $00
	.db $80 $FF $00 $00 $00 $00 $58 $30 $00 $12 $80 $FF $00 $00 $00 $00
	.db $58 $30 $01 $00 $00 $00 $80 $00 $00 $00 $48 $30 $01 $00 $78 $00
	.db $00 $00 $00 $00 $80 $30 $01 $00 $88 $FF $00 $00 $00 $00 $80 $30
	.db $80 $12 $00 $00 $00 $00 $F8 $FF $80 $30 $01 $00 $00 $00 $00 $00
	.db $00 $00 $80 $30 $40 $12 $80 $00 $00 $00 $00 $00 $80 $2A $40 $12
	.db $80 $00 $00 $00 $00 $00 $80 $26 $00 $12 $00 $00 $80 $00 $00 $00
	.db $20 $28 $00 $12 $00 $00 $80 $00 $00 $00 $40 $28 $00 $12 $00 $00
	.db $80 $00 $00 $00 $20 $2A $01 $00 $00 $00 $80 $FF $00 $00 $49 $2A
	.db $01 $00 $80 $FF $00 $00 $00 $00 $58 $26 $00 $18 $00 $00 $80 $00
	.db $00 $00 $40 $28 $01 $12 $80 $00 $00 $00 $00 $00 $58 $2A $00 $18
	.db $00 $00 $80 $00 $00 $00 $40 $28 $01 $00 $00 $00 $80 $00 $00 $00
	.db $3E $26 $01 $00 $00 $00 $00 $00 $00 $00 $80 $26
	
; Data from 17CF2 to 17D04 (19 bytes)	
_DATA_17CF2_:	
	.db $01 $02 $01 $02 $01 $02 $03 $04 $00 $05 $06 $03 $04 $07 $08 $09
	.db $0A $0B $0C
	
; Data from 17D05 to 17D17 (19 bytes)	
_DATA_17D05_:	
	.db $0D $0E $0F $10 $11 $12 $13 $14 $00 $15 $16 $13 $14 $17 $18 $19
	.db $1A $1B $1C
	
; Data from 17D18 to 17FFF (744 bytes)	
_DATA_17D18_:	
	.db $00 $00 $00 $00 $00 $00 $1F $3F $00 $00 $00 $00 $00 $00 $FF $FE
	.db $00 $00 $00 $00 $00 $00 $00 $01 $00 $00 $00 $00 $00 $00 $E0 $E0
	.db $00 $00 $00 $00 $00 $00 $38 $38 $00 $00 $00 $00 $00 $00 $07 $0F
	.db $00 $00 $00 $00 $00 $00 $3F $3F $00 $00 $00 $00 $00 $00 $FE $FF
	.db $00 $00 $00 $00 $00 $00 $38 $38 $00 $00 $00 $00 $00 $00 $07 $1C
	.db $00 $00 $3F $0C $0C $0C $0C $1D $00 $00 $FF $CC $CC $CC $CC $DC
	.db $38 $70 $7F $3F $00 $00 $7F $FF $00 $00 $FC $FE $0E $1C $FC $F8
	.db $38 $70 $7F $7F $F0 $E0 $FF $7F $00 $00 $FE $FC $00 $00 $FC $F8
	.db $38 $70 $70 $70 $E0 $E0 $FF $7F $00 $00 $7E $FC $1C $38 $F8 $F0
	.db $03 $07 $0E $1D $3B $70 $E0 $C0 $F0 $30 $38 $F8 $FC $1C $0E $0E
	.db $7C $7C $7C $76 $F7 $E7 $E3 $E3 $1E $3E $7E $EE $DE $DC $9C $1C
	.db $70 $70 $77 $73 $F1 $E0 $E0 $E0 $07 $0E $FE $FC $C0 $E0 $70 $38
	.db $78 $71 $77 $73 $F1 $E0 $E0 $E0 $70 $C0 $00 $80 $C0 $E0 $78 $1C
	.db $19 $19 $19 $3B $33 $33 $33 $FF $98 $98 $98 $B8 $30 $30 $30 $FC
	.db $0E $00 $02 $02 $0D $04 $07 $07 $18 $10 $17 $17 $20 $20 $3F $3F
	.db $40 $40 $7F $7F $3F $3F $3F $3F $7F $60 $40 $60 $3F $30 $20 $30
	.db $E0 $00 $20 $20 $F0 $40 $50 $50 $C0 $00 $30 $20 $30 $00 $F0 $F0
	.db $F8 $60 $F8 $F8 $F0 $80 $88 $80 $F8 $00 $18 $18 $28 $08 $F8 $F8
	.db $1F $1F $10 $1F $5F $58 $60 $78 $5F $50 $60 $70 $7F $68 $50 $58
	.db $5F $4F $70 $7F $2F $23 $3F $3F $54 $54 $6F $6F $3B $3B $3B $3B
	.db $A8 $A0 $5C $D8 $C4 $40 $3C $7C $D8 $D8 $76 $F4 $D2 $D0 $7E $FE
	.db $B6 $B4 $DA $DA $96 $10 $FE $FE $66 $62 $BE $BE $FC $FC $FC $FC
	.db $3B $22 $2A $2A $37 $00 $0C $0C $63 $40 $5C $5C $80 $80 $FF $FF
	.db $03 $01 $FF $FF $FF $FE $FE $FE $FF $80 $00 $80 $FC $E0 $83 $E3
	.db $80 $00 $80 $80 $C0 $00 $40 $40 $28 $28 $F8 $B8 $E4 $24 $FC $FC
	.db $EC $84 $FC $FC $FC $14 $2C $2C $E8 $08 $78 $78 $90 $00 $F0 $F0
	.db $7F $7C $70 $7C $3F $28 $30 $38 $3F $28 $30 $38 $1F $16 $18 $1E
	.db $0B $09 $0E $0B $07 $06 $07 $07 $01 $01 $01 $01 $00 $00 $00 $00
	.db $E8 $20 $18 $18 $E4 $00 $1C $1C $E8 $08 $14 $14 $D2 $02 $3E $3E
	.db $EA $82 $7E $EE $FD $31 $FF $FF $6B $28 $EF $EF $E7 $E7 $E7 $E7
	.db $0E $00 $02 $02 $0F $04 $07 $07 $18 $10 $17 $17 $20 $20 $3F $3F
	.db $40 $40 $7F $7F $3F $3F $3F $3F $7F $60 $40 $60 $3F $30 $20 $30
	.db $E0 $00 $20 $20 $F0 $40 $50 $50 $F0 $00 $20 $30 $70 $00 $F0 $F0
	.db $F8 $60 $F8 $F8 $F8 $80 $80 $88 $F8 $00 $18 $18 $E8 $08 $F8 $F8
	.db $1F $1F $10 $1F $7F $58 $60 $78 $7F $50 $60 $70 $5F $68 $50 $78
	.db $7F $4F $70 $7F $2F $23 $3F $3F $64 $54 $6F $7F $3B $3B $3B $3B
	.db $CC $A0 $58 $FC $E4 $40 $3C $7C $F2 $D8 $74 $FE $F2 $D0 $7E $FE
	.db $D2 $B4 $DA $FE $D6 $10 $FE $FE $26 $62 $BE $FE $FC $FC $FC $FC
	.dsb 136, $FF
	
.BANK 6	
.ORG $0000	
	
	; Data from 18000 to 1BFFF (16384 bytes)
	.incbin "FilesM_18000-1BFFF.dat"
	
.BANK 7	
.ORG $0000	
	
; Data from 1C000 to 1C028 (41 bytes)	
_DATA_1C000_:	
	.db $00 $01 $05 $04 $02 $06 $07 $04 $08 $03 $01 $04 $0C $06 $0E $0B
	.db $00 $09 $0A $04 $02 $07 $08 $04 $01 $05 $09 $04 $0D $00 $03 $0B
	.db $0A $02 $07 $04 $09 $01 $08 $04 $0B
	
; Data from 1C029 to 1C064 (60 bytes)	
_DATA_1C029_:	
	.db $00 $00 $01 $02 $03 $04 $12 $13 $0C $15 $16 $17 $1D $1E $1F $20
	.db $00 $00 $14 $05 $06 $18 $07 $19 $06 $08 $10 $11 $06 $1A $1B $1C
	.db $10 $10 $10 $10 $09 $09 $09 $09 $07 $07 $07 $07 $0A $0A $0A $0A
	.db $0D $0E $0D $0E $0F $0F $0F $0F $0B $0B $0B $0B
	
; Pointer Table from 1C065 to 1C0A6 (33 entries, indexed by unknown)	
_DATA_1C065_:	
	.dw _DATA_1C0A7_ _DATA_1C0E7_ _DATA_1C0F1_ _DATA_1C103_ _DATA_1C10F_ _DATA_1C115_ _DATA_1C125_ _DATA_1C127_
	.dw _DATA_1C129_ _DATA_1C12F_ _DATA_1C131_ _DATA_1C133_ _DATA_1C173_ _DATA_1C179_ _DATA_1C1B9_ _DATA_1C1F9_
	.dw _DATA_1C1FB_ _DATA_1C1FD_ _DATA_1C203_ _DATA_1C20D_ _DATA_1C213_ _DATA_1C23D_ _DATA_1C243_ _DATA_1C25B_
	.dw _DATA_1C261_ _DATA_1C267_ _DATA_1C26D_ _DATA_1C278_ _DATA_1C28B_ _DATA_1C2A5_ _DATA_1C2B9_ _DATA_1C2C1_
	.dw _DATA_1C2CD_
	
; 1st entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C0A7 to 1C0E6 (64 bytes)	
_DATA_1C0A7_:	
	.db $02 $03 $00 $01 $02 $03 $00 $01 $02 $03 $00 $01 $02 $01 $02 $03
	.db $00 $03 $00 $03 $00 $03 $00 $03 $00 $01 $02 $03 $00 $03 $00 $01
	.db $02 $03 $00 $03 $00 $03 $00 $01 $02 $01 $02 $03 $00 $01 $02 $03
	.db $00 $03 $00 $01 $02 $03 $00 $03 $00 $01 $02 $03 $00 $03 $00 $01
	
; 2nd entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C0E7 to 1C0F0 (10 bytes)	
_DATA_1C0E7_:	
	.db $06 $86 $04 $8D $06 $85 $05 $97 $08 $8C
	
; 3rd entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C0F1 to 1C102 (18 bytes)	
_DATA_1C0F1_:	
	.db $08 $88 $07 $81 $08 $88 $07 $82 $08 $88 $07 $83 $08 $87 $07 $8B
	.db $08 $87
	
; 4th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C103 to 1C10E (12 bytes)	
_DATA_1C103_:	
	.db $0D $87 $0E $83 $0D $97 $0E $87 $0D $91 $0E $81
	
; 5th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C10F to 1C114 (6 bytes)	
_DATA_1C10F_:	
	.db $0E $83 $0D $B0 $0E $8A
	
; 6th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C115 to 1C124 (16 bytes)	
_DATA_1C115_:	
	.db $06 $8B $05 $83 $07 $87 $06 $95 $07 $81 $0F $89 $11 $81 $10 $83
	
; 7th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C125 to 1C126 (2 bytes)	
_DATA_1C125_:	
	.db $0A $BF
	
; 8th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C127 to 1C128 (2 bytes)	
_DATA_1C127_:	
	.db $1E $BF
	
; 9th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C129 to 1C12E (6 bytes)	
_DATA_1C129_:	
	.db $0A $8B $0B $9B $0C $97
	
; 10th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C12F to 1C130 (2 bytes)	
_DATA_1C12F_:	
	.db $1D $BF
	
; 11th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C131 to 1C132 (2 bytes)	
_DATA_1C131_:	
	.db $1A $BF
	
; 12th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C133 to 1C172 (64 bytes)	
_DATA_1C133_:	
	.db $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C
	.db $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C
	.db $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C
	.db $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C $1B $1C
	
; 13th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C173 to 1C178 (6 bytes)	
_DATA_1C173_:	
	.db $0E $9F $0D $95 $0E $89
	
; 14th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C179 to 1C1B8 (64 bytes)	
_DATA_1C179_:	
	.db $12 $13 $14 $15 $12 $13 $14 $15 $12 $13 $14 $15 $12 $13 $14 $15
	.db $12 $13 $14 $15 $12 $13 $14 $15 $12 $13 $14 $15 $12 $13 $14 $15
	.db $12 $13 $14 $15 $12 $13 $14 $15 $12 $13 $14 $15 $12 $13 $14 $15
	.db $12 $13 $14 $15 $12 $13 $14 $15 $12 $13 $14 $15 $12 $13 $14 $15
	
; 15th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C1B9 to 1C1F8 (64 bytes)	
_DATA_1C1B9_:	
	.db $12 $16 $17 $18 $16 $17 $18 $16 $17 $18 $16 $17 $18 $16 $17 $18
	.db $16 $17 $18 $16 $17 $18 $16 $17 $18 $16 $17 $18 $16 $17 $18 $16
	.db $17 $18 $16 $17 $18 $16 $17 $18 $16 $17 $18 $16 $17 $18 $16 $17
	.db $18 $16 $17 $18 $16 $17 $18 $16 $17 $18 $16 $17 $18 $16 $17 $18
	
; 16th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C1F9 to 1C1FA (2 bytes)	
_DATA_1C1F9_:	
	.db $19 $BF
	
; 17th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C1FB to 1C1FC (2 bytes)	
_DATA_1C1FB_:	
	.db $0C $BF
	
; 18th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C1FD to 1C202 (6 bytes)	
_DATA_1C1FD_:	
	.db $0C $93 $0B $97 $0A $93
	
; 19th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C203 to 1C20C (10 bytes)	
_DATA_1C203_:	
	.db $0E $87 $0D $83 $0E $9B $0D $93 $0E $83
	
; 20th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C20D to 1C212 (6 bytes)	
_DATA_1C20D_:	
	.db $0E $87 $0D $AF $0E $87
	
; 21st entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C213 to 1C23C (42 bytes)	
_DATA_1C213_:	
	.db $00 $01 $02 $03 $00 $01 $02 $03 $00 $03 $00 $01 $02 $03 $00 $01
	.db $01 $03 $00 $03 $00 $03 $00 $03 $00 $01 $02 $03 $00 $03 $00 $01
	.db $02 $03 $00 $03 $00 $03 $00 $01 $06 $97
	
; 22nd entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C23D to 1C242 (6 bytes)	
_DATA_1C23D_:	
	.db $0E $87 $0D $8F $0E $A7
	
; 23rd entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C243 to 1C25A (24 bytes)	
_DATA_1C243_:	
	.db $0E $8B $0D $83 $0E $83 $0D $83 $0E $83 $0D $85 $0E $85 $0D $87
	.db $0E $83 $0D $83 $0D $83 $0E $83
	
; 24th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C25B to 1C260 (6 bytes)	
_DATA_1C25B_:	
	.db $0E $83 $0D $B1 $0E $89
	
; 25th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C261 to 1C266 (6 bytes)	
_DATA_1C261_:	
	.db $0A $A3 $0B $97 $1E $83
	
; 26th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C267 to 1C26C (6 bytes)	
_DATA_1C267_:	
	.db $1E $93 $0B $8B $0A $9F
	
; 27th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C26D to 1C277 (11 bytes)	
_DATA_1C26D_:	
	.db $0A $8B $0B $83 $1F $9C $22 $21 $20 $1F $8F
	
; 28th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C278 to 1C28A (19 bytes)	
_DATA_1C278_:	
	.db $1F $22 $21 $20 $1F $22 $21 $20 $1F $AC $22 $21 $20 $1F $22 $21
	.db $20 $1F $83
	
; 29th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C28B to 1C2A4 (26 bytes)	
_DATA_1C28B_:	
	.db $1F $94 $22 $21 $20 $1F $22 $21 $20 $1F $22 $21 $20 $1F $90 $22
	.db $21 $20 $1F $22 $21 $20 $1F $83 $0B $83
	
; 30th entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C2A5 to 1C2B8 (20 bytes)	
_DATA_1C2A5_:	
	.db $07 $8B $05 $83 $07 $87 $05 $83 $07 $83 $05 $8B $07 $83 $09 $83
	.db $07 $83 $05 $87
	
; 31st entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C2B9 to 1C2C0 (8 bytes)	
_DATA_1C2B9_:	
	.db $05 $97 $09 $8F $05 $93 $07 $83
	
; 32nd entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C2C1 to 1C2CC (12 bytes)	
_DATA_1C2C1_:	
	.db $07 $87 $09 $87 $05 $87 $09 $87 $07 $87 $09 $97
	
; 33rd entry of Pointer Table from 1C065 (indexed by unknown)	
; Data from 1C2CD to 1C2D4 (8 bytes)	
_DATA_1C2CD_:	
	.db $09 $8F $05 $87 $09 $A3 $07 $83
	
; Data from 1C2D5 to 1C34C (120 bytes)	
_DATA_1C2D5_:	
	.db $E3 $83 $E4 $83 $E8 $83 $4C $84 $95 $84 $D9 $84 $25 $85 $55 $85
	.db $AA $85 $DA $85 $FA $85 $42 $86 $9F $86 $FB $86 $47 $87 $93 $87
	.db $D4 $87 $D5 $87 $D6 $87 $EA $87 $53 $88 $58 $88 $78 $88 $A0 $88
	.db $CD $88 $D2 $88 $02 $89 $42 $89 $67 $89 $83 $89 $AB $89 $F7 $89
	.db $38 $8A $54 $8A $80 $8A $B4 $8A $F5 $8A $49 $8B $B9 $8B $3D $8C
	.db $C6 $8C $12 $8D $72 $8D $D2 $8D $2F $8E $53 $8E $97 $8E $CF $8E
	.db $18 $8F $19 $8F $39 $8F $3E $8F $5F $8F $7B $8F $97 $8F $BB $8F
	.db $DC $8F $48 $90 $C0 $90 $58 $91
	
; Pointer Table from 1C34D to 1C3E4 (76 entries, indexed by unknown)	
_DATA_1C34D_:	
	.dw _DATA_1D20D_ _DATA_1D253_ _DATA_1D257_ _DATA_1D25B_ _DATA_1D273_ _DATA_1D283_ _DATA_1D291_ _DATA_1D2B3_
	.dw _DATA_1D2C5_ _DATA_1D333_ _DATA_1D337_ _DATA_1D33B_ _DATA_1D341_ _DATA_1D347_ _DATA_1D365_ _DATA_1D383_
	.dw _DATA_1D391_ _DATA_1D483_ _DATA_1D4DD_ _DATA_1D4E5_ _DATA_1D527_ _DATA_1D569_ _DATA_1D5BF_ _DATA_1D5D1_
	.dw _DATA_1D60F_ _DATA_1D639_ _DATA_1D663_ _DATA_1D6D1_ _DATA_1D6D9_ _DATA_1D6F7_ _DATA_1D708_ _DATA_1D7CA_
	.dw _DATA_1D7DC_ _DATA_1D8C2_ _DATA_1D8CB_ _DATA_1D8EB_ _DATA_1D8FD_ _DATA_1D907_ _DATA_1D91D_ _DATA_1D92F_
	.dw _DATA_1D941_ _DATA_1D95B_ _DATA_1D975_ _DATA_1DA67_ _DATA_1DA81_ _DATA_1DB63_ _DATA_1DB85_ _DATA_1DBA7_
	.dw _DATA_1DBB9_ _DATA_1DBDB_ _DATA_1DBFD_ _DATA_1DC1B_ _DATA_1DC2C_ _DATA_1DC46_ _DATA_1DC90_ _DATA_1DCA2_
	.dw _DATA_1DCE4_ _DATA_1DD26_ _DATA_1DD48_ _DATA_1DD5E_ _DATA_1DD6C_ _DATA_1DD72_ _DATA_1DD84_ _DATA_1DDF6_
	.dw _DATA_1DE38_ _DATA_1DEBA_ _DATA_1DECC_ _DATA_1DED2_ _DATA_1D7CA_ _DATA_1D7CA_ _DATA_1D7CA_ _DATA_1D7CA_
	.dw _DATA_1D7CA_ _DATA_1D7CA_ _DATA_1D7CA_ _RAM_FCFF_
	
	; Data from 1C3E5 to 1D20C (3624 bytes)
	.incbin "FilesN_1C3E5-1D20C.dat"
	
; Data from 1D20D to 1D252 (70 bytes)	
_DATA_1D20D_:	
	.db $11 $04 $7A $7A $92 $7A $7A $7A $3C $29 $29 $29 $29 $29 $29 $29
	.db $29 $1C $2B $7A $7A $93 $7A $7A $90 $84
	.dsb 9, $29
	.db $6D $14 $7D $7F $8C $8E $8D $8F
	.dsb 9, $29
	.db $6C
	.dsb 16, $29
	.db $6D
	
; Data from 1D253 to 1D256 (4 bytes)	
_DATA_1D253_:	
	.db $01 $02 $A8 $A9
	
; Data from 1D257 to 1D25A (4 bytes)	
_DATA_1D257_:	
	.db $01 $02 $AA $AB
	
; Data from 1D25B to 1D272 (24 bytes)	
_DATA_1D25B_:	
	.db $0B $02 $AC $B4 $B6 $B0 $B2 $94 $96 $98 $9A $10 $12 $AD $B5 $B7
	.db $B1 $B3 $95 $97 $99 $9B $11 $13
	
; Data from 1D273 to 1D282 (16 bytes)	
_DATA_1D273_:	
	.db $07 $02 $AC $B4 $B6 $94 $96 $10 $12 $AD $B5 $B7 $95 $97 $11 $13
	
; Data from 1D283 to 1D290 (14 bytes)	
_DATA_1D283_:	
	.db $02 $06 $9C $9E $9D $9F $A0 $A2 $A1 $A3 $A4 $A6 $A5 $A7
	
; Data from 1D291 to 1D2B2 (34 bytes)	
_DATA_1D291_:	
	.db $FE $08
	.dsb 32, $29
	
; Data from 1D2B3 to 1D2C4 (18 bytes)	
_DATA_1D2B3_:	
	.db $02 $08 $E0 $E2 $E1 $E3 $E4 $E6 $E5 $E7 $E8 $EA $E9 $EB $EC $EE
	.db $ED $EF
	
; Data from 1D2C5 to 1D332 (110 bytes)	
_DATA_1D2C5_:	
	.db $09 $0C $29 $29 $F0 $F8 $0E $03 $D4 $D6 $D8 $29 $29 $F1 $F9 $21
	.db $23 $D5 $D7 $D9 $29 $29 $F2 $FA $20 $22 $D8 $DA $D6 $29 $29 $F3
	.db $FB $21 $00 $D9 $DB $D7 $29 $F0 $F8 $0E $03 $D4 $D6 $D8 $DA $29
	.db $F1 $F9 $21 $23 $D5 $D7 $D9 $DB $29 $F2 $FA $20 $22 $D8 $DA $D6
	.db $D4 $29 $F3 $FB $21 $00 $D9 $DB $D7 $D5 $F0 $F8 $0E $03 $D4 $D6
	.db $D8 $DA $D4 $F1 $F9 $21 $23 $D5 $D7 $D9 $DB $D5 $F2 $FA $20 $22
	.db $D8 $DA $D6 $D4 $D8 $F3 $FB $21 $00 $D9 $DB $D7 $D5 $D9
	
; Data from 1D333 to 1D336 (4 bytes)	
_DATA_1D333_:	
	.db $02 $01 $1C $1E
	
; Data from 1D337 to 1D33A (4 bytes)	
_DATA_1D337_:	
	.db $02 $01 $1D $1F
	
; Data from 1D33B to 1D340 (6 bytes)	
_DATA_1D33B_:	
	.db $04 $01 $D0 $D2 $D1 $D3
	
; Data from 1D341 to 1D346 (6 bytes)	
_DATA_1D341_:	
	.db $04 $01 $DD $DF $DC $DE
	
; Data from 1D347 to 1D364 (30 bytes)	
_DATA_1D347_:	
	.db $07 $04 $7A $7A $92 $7A $7A $7A $3C $7A $7A $93 $7A $7A $90 $84
	.db $14 $7D $7F $8C $8E $8D $8F $29 $29 $29 $29 $29 $29 $29
	
; Data from 1D365 to 1D382 (30 bytes)	
_DATA_1D365_:	
	.db $07 $04 $29 $29 $29 $29 $29 $29 $29 $A9 $AB $A8 $AA $A5 $A7 $A6
	.db $7A $7A $7A $7A $7A $AC $A3 $7A $7A $7A $7A $7A $7A $A4
	
; Data from 1D383 to 1D390 (14 bytes)	
_DATA_1D383_:	
	.db $02 $06 $40 $42 $41 $43 $44 $46 $45 $47 $48 $4A $49 $4B
	
; Data from 1D391 to 1D482 (242 bytes)	
_DATA_1D391_:	
	.db $18 $0A $11 $13 $58 $5A $59 $58 $0C $0E $0D $0F $10 $12 $11 $13
	.db $58 $5A $5C $5E $C5 $5F $0D $0F $10 $12
	.dsb 16, $16
	.db $60 $62
	.dsb 18, $16
	.db $20 $22 $16 $16 $61 $63 $16 $16 $16 $16 $16 $16 $16 $24 $26
	.dsb 9, $16
	.db $21 $23 $16 $16 $64 $17 $16 $16 $16 $16 $16 $16 $16 $25 $27
	.dsb 13, $16
	.db $15 $14
	.dsb 15, $16
	.db $24 $26 $16 $16 $16 $16 $16 $60 $62
	.dsb 11, $16
	.db $20 $22 $16 $16 $25 $27 $16 $16 $16 $16 $16 $61 $63
	.dsb 11, $16
	.db $21 $23
	.dsb 9, $16
	.db $64 $17
	.dsb 22, $16
	.db $15 $14 $16 $16 $16 $16 $16 $16 $05 $07 $08 $0A $09 $0B $00 $02
	.db $01 $03 $04 $06 $05 $07 $08 $0A $30 $32 $C0 $5D $01 $03 $04 $06
	
; Data from 1D483 to 1D4DC (90 bytes)	
_DATA_1D483_:	
	.db $0B $08 $16 $1A $28 $2A $16 $16 $16 $16 $16 $16 $D4 $19 $1B $29
	.db $2B $16 $16 $16 $16 $16 $16 $D5 $1C $1E $2C $2E $58 $58 $58 $58
	.db $5A $5C $D6 $16 $1F $2D $2F $59 $59 $59 $59 $5B $5D $D7 $20 $22
	.db $30 $32 $16 $16 $16 $16 $16 $16 $D4 $21 $23 $31 $33 $16 $16 $16
	.db $16 $16 $16 $D5 $16 $26 $34 $36 $16 $16 $16 $16 $16 $16 $D4 $16
	.db $16 $35 $37 $16 $16 $16 $16 $16 $16 $D5
	
; Data from 1D4DD to 1D4E4 (8 bytes)	
_DATA_1D4DD_:	
	.db $03 $02 $D8 $C0 $C2 $D4 $42 $48
	
; Data from 1D4E5 to 1D526 (66 bytes)	
_DATA_1D4E5_:	
	.db $FE $08 $9D $9D $9E $9C $9D $9D $9E $9C $9D $9D $9E $9C $9D $9D
	.db $9E $9C $9D $9D $9E $9C $9D $9D $9E $9C $9D $9D $9E $9C $9D $9D
	.db $9E $9C $9E $9C $9D $9D $9E $9C $9D $9D $9E $9C $9D $9D $9E $9C
	.db $9D $9D $9E $9C $9D $9D $9E $9C $9D $9D $9E $9C $9D $9D $9E $9C
	.db $9D $9D
	
; Data from 1D527 to 1D568 (66 bytes)	
_DATA_1D527_:	
	.db $02 $08 $CA $CA $C9 $C8 $CA $CA $C9 $C8 $CA $CA $C9 $C8 $CA $CA
	.db $C9 $C8 $CA $CA $C9 $C8 $CA $CA $C9 $C8 $CA $CA $C9 $C8 $CA $CA
	.db $C9 $C8 $C9 $C8 $CA $CA $C9 $C8 $CA $CA $C9 $C8 $CA $CA $C9 $C8
	.db $CA $CA $C9 $C8 $CA $CA $C9 $C8 $CA $CA $C9 $C8 $CA $CA $C9 $C8
	.db $CA $CA
	
; Data from 1D569 to 1D5BE (86 bytes)	
_DATA_1D569_:	
	.db $0E $06 $17 $58 $59 $58 $59 $58 $59 $58 $59 $58 $59 $58 $59 $58
	.db $50 $52 $48 $4A $70 $72 $5A $72 $5A $72 $5A $72 $5A $6A $51 $53
	.db $49 $4B $71 $73 $73 $73 $73 $73 $73 $73 $73 $6B $54 $56 $4C $4E
	.db $74 $76 $76 $76 $76 $76 $76 $76 $76 $6E $55 $57 $4D $4F $75 $77
	.db $5B $77 $5B $77 $5B $77 $5B $6F $17 $59 $58 $59 $58 $59 $58 $59
	.db $58 $59 $58 $59 $58 $59
	
; Data from 1D5BF to 1D5D0 (18 bytes)	
_DATA_1D5BF_:	
	.db $04 $04 $40 $42 $42 $5E $41 $43 $5D $5F $44 $46 $60 $62 $45 $47
	.db $47 $63
	
; Data from 1D5D1 to 1D60E (62 bytes)	
_DATA_1D5D1_:	
	.db $0A $06 $17 $58 $59 $58 $59 $58 $59 $58 $59 $58 $50 $52 $48 $4A
	.db $70 $72 $5A $72 $5A $6A $51 $53 $49 $4B $71 $73 $73 $73 $73 $6B
	.db $54 $56 $4C $4E $74 $76 $76 $76 $76 $6E $55 $57 $4D $4F $75 $77
	.db $5B $77 $5B $6F $17 $59 $58 $59 $58 $59 $58 $59 $58 $59
	
; Data from 1D60F to 1D638 (42 bytes)	
_DATA_1D60F_:	
	.db $0A $04 $F0 $F8 $0E $03 $D4 $D6 $D8 $DA $D4 $D6 $F1 $F9 $21 $23
	.db $D5 $D7 $D9 $DB $D5 $D7 $F2 $FA $20 $22 $D8 $DA $D4 $D6 $D8 $DA
	.db $F3 $FB $21 $00 $D9 $DB $D5 $D7 $D9 $DB
	
; Data from 1D639 to 1D662 (42 bytes)	
_DATA_1D639_:	
	.db $0A $04 $F4 $FC $20 $0B $D4 $D6 $D8 $DA $D4 $D6 $F5 $FD $21 $23
	.db $D5 $D7 $D9 $DB $D5 $D7 $F6 $FE $20 $22 $D8 $DA $D4 $D6 $D8 $DA
	.db $F7 $FF $0F $0A $D9 $DB $D5 $D7 $D9 $DB
	
; Data from 1D663 to 1D6D0 (110 bytes)	
_DATA_1D663_:	
	.db $09 $0C $F4 $FC $20 $0B $D4 $D6 $D8 $DA $D4 $F5 $FD $21 $23 $D5
	.db $D7 $D9 $DB $D5 $F6 $FE $20 $22 $D8 $DA $D4 $D6 $D8 $F7 $FF $0F
	.db $0A $D9 $DB $D5 $D7 $D9 $29 $F4 $FC $20 $0B $D4 $D6 $D8 $DA $29
	.db $F5 $FD $21 $23 $D5 $D7 $D9 $DB $29 $F6 $FE $20 $22 $D8 $DA $D4
	.db $D6 $29 $F7 $FF $0F $0A $D9 $DB $D5 $D7 $29 $29 $F4 $FC $20 $0B
	.db $D4 $D6 $D8 $29 $29 $F5 $FD $21 $23 $D5 $D7 $D9 $29 $29 $F6 $FE
	.db $20 $22 $D8 $DA $D4 $29 $29 $F7 $FF $0F $0A $D9 $DB $D5
	
; Data from 1D6D1 to 1D6D8 (8 bytes)	
_DATA_1D6D1_:	
	.db $03 $02 $D5 $43 $4D $D9 $C1 $C3
	
; Data from 1D6D9 to 1D6F6 (30 bytes)	
_DATA_1D6D9_:	
	.db $04 $07 $01 $03 $00 $09 $04 $18 $18 $0C $05 $18 $18 $0D $08 $18
	.db $18 $10 $16 $0B $18 $11 $16 $0E $02 $14 $16 $0F $06 $16
	
; Data from 1D6F7 to 1D707 (17 bytes)	
_DATA_1D6F7_:	
	.db $03 $05 $16 $15 $65 $07 $18 $66 $0A $18 $67 $12 $63 $68 $13 $64
	.db $16
	
; Data from 1D708 to 1D7C9 (194 bytes)	
_DATA_1D708_:	
	.db $0C $10 $DF $A4 $A6 $AC $AE $A4 $A6 $AC $AE $50 $5F $17 $DF $A5
	.db $A7 $AD $AF $A5 $A7 $AD $AE $51 $62 $17 $DF $A8 $AA $B0 $B2 $A8
	.db $AA $B0 $B2 $52 $17 $62 $DF $A9 $AB $B1 $B3 $A9 $AB $B1 $B3 $53
	.db $5E $17 $DF $A4 $A6 $AC $AE $A4 $A6 $AC $AE $50 $5F $17 $DF $A5
	.db $A7 $AD $AF $A5 $A7 $AD $AE $51 $62 $17 $DF $A8 $AA $B0 $B2 $A8
	.db $AA $B0 $B2 $52 $17 $62 $DF $A9 $AB $B1 $B3 $A9 $AB $B1 $B3 $53
	.db $5E $17 $DF $A4 $A6 $AC $AE $A4 $A6 $AC $AE $50 $5F $17 $DF $A5
	.db $A7 $AD $AF $A5 $A7 $AD $AE $51 $62 $17 $DF $A8 $AA $B0 $B2 $A8
	.db $AA $B0 $B2 $52 $17 $62 $DF $A9 $AB $B1 $B3 $45 $47 $44 $46 $53
	.db $5E $17 $DF $A4 $A6 $3C $40 $16 $16 $16 $16 $54 $5F $17 $DF $A5
	.db $24 $3D $41 $16 $16 $16 $16 $54 $5F $17 $DF $4A $4E $16 $16 $16
	.db $16 $16 $16 $54 $5F $17 $DF $4B $4F $16 $16 $16 $16 $16 $16 $54
	.db $5F $17
	
; Data from 1D7CA to 1D7DB (18 bytes)	
_DATA_1D7CA_:	
	.db $02 $08 $E0 $E2 $E1 $E3 $E4 $E6 $E5 $E7 $E8 $EA $E9 $EB $EC $EE
	.db $ED $EF
	
; Data from 1D7DC to 1D8C1 (230 bytes)	
_DATA_1D7DC_:	
	.db $13 $0C $6C $6D $25 $27 $6C $6D $25 $27 $6C $6D $25 $27 $6C $6D
	.db $25 $27 $6C $6D $D6 $AD $AF $A5 $A7 $AD $AF $A5 $A7 $AD $AF $A5
	.db $A7 $AD $AF $A5 $A7 $AD $AF $D7 $B0 $B2 $A8 $AA $B0 $B2 $A8 $AA
	.db $B0 $B2 $A8 $AA $B0 $B2 $A8 $AA $B0 $B2 $D6 $B1 $B3 $A9 $AB $B1
	.db $B3 $A9 $AB $70 $74 $A9 $AB $B1 $B3 $A9 $AB $B1 $B3 $D7 $AC $AE
	.db $A4 $A6 $AC $AE $A4 $A6 $71 $75 $A4 $38 $3A $3B $3A $3B $3A $3B
	.db $D6 $AD $AF $A5 $A7 $AD $AF $A5 $A7 $B4 $B6 $A5 $39 $1D $1D $1D
	.db $1D $1D $1D $D7 $B0 $B2 $A8 $AA $B0 $B2 $A8 $AA $B0 $B2 $A8 $38
	.db $1D $1D $1D $1D $1D $1D $D6 $B1 $B3 $A9 $AB $B1 $B3 $A9 $AB $B1
	.db $B3 $A9 $39 $1D $1D $1D $1D $1D $1D $D7 $AC $AE $A4 $A6 $AC $AE
	.db $A4 $A6 $B5 $B7 $A4 $38 $1D $1D $1D $1D $1D $1D $D6 $AD $AF $A5
	.db $A7 $AD $AF $A5 $A7 $B8 $B9 $A5 $39 $3A $3B $3A $3B $3A $3B $D7
	.db $B0 $B2 $A8 $AA $B0 $B2 $A8 $AA $78 $A3 $A8 $AA $B0 $B2 $A8 $AA
	.db $B0 $B2 $D6 $B1 $B3 $A9 $AB $B1 $B3 $A9 $AB $B1 $B3 $A9 $AB $B1
	.db $B3 $A9 $AB $B1 $B3 $D7
	
; Data from 1D8C2 to 1D8CA (9 bytes)	
_DATA_1D8C2_:	
	.db $07 $01 $4E $6A $6E $73 $17 $17 $4C
	
; Data from 1D8CB to 1D8EA (32 bytes)	
_DATA_1D8CB_:	
	.db $06 $05 $00 $02 $08 $0A $45 $38 $01 $03 $09 $0B $4C $39 $04 $06
	.db $0C $0E $4D $3A $05 $07 $0D $0F $4F $3B $40 $40 $2C $2D $2E $40
	
; Data from 1D8EB to 1D8FC (18 bytes)	
_DATA_1D8EB_:	
	.db $02 $08 $18 $1A $19 $1B $1C $1E $1D $1F $20 $22 $21 $23 $24 $26
	.db $25 $27
	
; Data from 1D8FD to 1D906 (10 bytes)	
_DATA_1D8FD_:	
	.db $04 $02 $40 $42 $50 $52 $41 $43 $51 $53
	
; Data from 1D907 to 1D91C (22 bytes)	
_DATA_1D907_:	
	.db $0A $02 $44 $46 $54 $56 $10 $10 $10 $10 $12 $16 $40 $47 $55 $57
	.db $11 $11 $11 $11 $13 $17
	
; Data from 1D91D to 1D92E (18 bytes)	
_DATA_1D91D_:	
	.db $04 $04 $48 $4A $58 $5A $49 $4B $59 $5B $40 $4E $5C $2F $40 $40
	.db $14 $15
	
; Data from 1D92F to 1D940 (18 bytes)	
_DATA_1D92F_:	
	.db $02 $08 $E0 $E2 $E1 $E3 $E4 $E6 $E5 $E7 $E8 $EA $E9 $EB $EC $EE
	.db $ED $EF
	
; Data from 1D941 to 1D95A (26 bytes)	
_DATA_1D941_:	
	.db $06 $04 $D1 $C6 $C8 $C9 $CC $CE $DE $6F $81 $83 $80 $82 $DA $72
	.db $84 $86 $69 $7C $DB $73 $85 $87 $6A $7D
	
; Data from 1D95B to 1D974 (26 bytes)	
_DATA_1D95B_:	
	.db $06 $04 $D2 $76 $88 $8A $6B $7E $D3 $77 $89 $8B $6E $7F $DA $92
	.db $98 $9A $A0 $A2 $DB $93 $99 $9B $A1 $79
	
; Data from 1D975 to 1DA66 (242 bytes)	
_DATA_1D975_:	
	.db $18 $0A $11 $13 $58 $5A $59 $58 $0C $0E $0D $0F $10 $12 $11 $13
	.db $58 $5A $5C $5E $C5 $5F $0D $0F $10 $12
	.dsb 16, $16
	.db $60 $62 $C0 $C2
	.dsb 16, $16
	.db $20 $22 $16 $16 $61 $63 $C1 $C3 $16 $16 $16 $16 $16 $24 $26
	.dsb 9, $16
	.db $21 $23 $16 $16 $64 $17 $C4 $C6 $16 $16 $16 $16 $16 $25 $27
	.dsb 13, $16
	.db $15 $14 $C5 $C7
	.dsb 13, $16
	.db $24 $26 $16 $16 $16 $16 $16 $60 $62 $C0 $C2
	.dsb 9, $16
	.db $20 $22 $16 $16 $25 $27 $16 $16 $16 $16 $16 $61 $63 $C1 $C3
	.dsb 9, $16
	.db $21 $23
	.dsb 9, $16
	.db $64 $17 $C4 $C6
	.dsb 20, $16
	.db $15 $14 $C5 $C7 $16 $16 $16 $16 $05 $07 $08 $0A $09 $0B $00 $02
	.db $01 $03 $04 $06 $05 $07 $08 $0A $30 $32 $C0 $5D $01 $03 $04 $06
	
; Data from 1DA67 to 1DA80 (26 bytes)	
_DATA_1DA67_:	
	.db $06 $04 $D2 $96 $9C $9E $7B $3E $D3 $97 $9D $9F $3F $8F $DC $7A
	.db $8C $8E $7B $8D $DD $D0 $CA $CB $CD $CF
	
; Data from 1DA81 to 1DB62 (226 bytes)	
_DATA_1DA81_:	
	.db $0E $10 $D1 $C6 $C8 $C9 $C8 $C9 $C8 $C9 $C8 $C9 $C8 $C9 $CC $CE
	.db $DE $6F $81 $83 $81 $83 $81 $83 $81 $83 $81 $83 $80 $82 $DA $72
	.db $84 $86 $98 $9A $88 $8A $9C $9E $84 $86 $69 $7C $DB $73 $85 $87
	.db $99 $9B $89 $8B $9D $9F $85 $87 $6A $7D $D2 $76 $88 $8A $9C $9E
	.db $84 $86 $98 $9A $88 $8A $6B $7E $D3 $77 $89 $8B $9D $9F $85 $87
	.db $99 $9B $89 $8B $6E $7F $DA $92 $98 $9A $88 $8A $9C $9E $84 $86
	.db $98 $9A $A0 $A2 $DB $93 $99 $9B $89 $8B $9D $9F $85 $87 $99 $9B
	.db $A1 $79 $D2 $96 $9C $9E $84 $86 $98 $9A $88 $8A $9C $9E $7B $3E
	.db $D3 $97 $9D $9F $85 $87 $99 $9B $89 $8B $9D $9F $3F $8F $DA $72
	.db $84 $86 $98 $9A $88 $8A $9C $9E $84 $86 $69 $7C $DB $73 $85 $87
	.db $99 $9B $89 $8B $9D $9F $85 $87 $6A $7D $D2 $76 $88 $8A $9C $9E
	.db $84 $86 $98 $9A $88 $8A $6B $7E $D3 $77 $89 $8B $9D $9F $85 $87
	.db $99 $9B $89 $8B $6E $7F $DA $92 $98 $9A $88 $8A $9C $9E $84 $86
	.db $98 $9A $A0 $A2 $DB $93 $99 $9B $89 $8B $9D $9F $85 $87 $99 $9B
	.db $A1 $79
	
; Data from 1DB63 to 1DB84 (34 bytes)	
_DATA_1DB63_:	
	.db $FE $08
	.dsb 32, $40
	
; Data from 1DB85 to 1DBA6 (34 bytes)	
_DATA_1DB85_:	
	.db $02 $08 $C5 $C1 $C9 $C4 $C3 $C7 $C3 $C2 $C5 $C1 $C9 $C4 $C3 $C7
	.db $C3 $C2 $C5 $C1 $C9 $C4 $C3 $C7 $C3 $C2 $C5 $C1 $C9 $C4 $C3 $C7
	.db $C3 $C2
	
; Data from 1DBA7 to 1DBB8 (18 bytes)	
_DATA_1DBA7_:	
	.db $02 $08 $E8 $8C $E9 $8D $EC $90 $ED $91 $EA $8E $EB $8F $EE $92
	.db $EF $93
	
; Data from 1DBB9 to 1DBDA (34 bytes)	
_DATA_1DBB9_:	
	.db $FE $08
	.dsb 32, $40
	
; Data from 1DBDB to 1DBFC (34 bytes)	
_DATA_1DBDB_:	
	.db $02 $08 $CF $D1 $5D $5D $CF $D1 $5D $5D $CF $D1 $5D $5D $CF $D1
	.db $5D $5D $CF $D1 $5D $5D $CF $D1 $5D $5D $CF $D1 $5D $5D $CF $D1
	.db $5D $5D
	
; Data from 1DBFD to 1DC1A (30 bytes)	
_DATA_1DBFD_:	
	.db $04 $07 $65 $67 $64 $66 $68 $75 $75 $6E $69 $75 $75 $6D $6C $75
	.db $75 $70 $40 $6F $75 $71 $40 $72 $6A $80 $40 $73 $6B $40
	
; Data from 1DC1B to 1DC2B (17 bytes)	
_DATA_1DC1B_:	
	.db $03 $05 $40 $74 $76 $81 $75 $77 $82 $75 $7A $83 $79 $7B $84 $85
	.db $40
	
; Data from 1DC2C to 1DC45 (26 bytes)	
_DATA_1DC2C_:	
	.db $06 $04 $60 $D0 $D1 $D0 $D1 $D0 $61 $5E $5D $5D $5D $5D $62 $5D
	.db $5D $5D $5D $5D $63 $CF $CF $CF $CF $CF
	
; Data from 1DC46 to 1DC8F (74 bytes)	
_DATA_1DC46_:	
	.db $0C $06 $03 $1A $24 $26 $03 $32 $04 $16 $03 $16 $03 $06 $19 $1B
	.db $25 $27 $31 $33 $05 $21 $21 $21 $21 $07 $1C $1E $28 $2A $34 $36
	.db $08 $18 $18 $18 $18 $0A $1D $1F $29 $2B $35 $37 $09 $18 $18 $18
	.db $18 $0B $20 $22 $2C $2E $38 $3A $0C $18 $18 $18 $18 $0E $16 $23
	.db $2D $2F $16 $3B $0D $03 $16 $03 $16 $0F
	
; Data from 1DC90 to 1DCA1 (18 bytes)	
_DATA_1DC90_:	
	.db $02 $08 $E0 $E2 $E1 $E3 $E4 $E6 $E5 $E7 $E0 $E2 $E1 $E3 $E4 $E6
	.db $E5 $E7
	
; Data from 1DCA2 to 1DCE3 (66 bytes)	
_DATA_1DCA2_:	
	.db $02 $08 $CD $CC $CD $CC $CD $CC $CD $CC $CD $CC $CD $CC $CD $CC
	.db $CD $CC $CD $CC $CD $CC $CD $CC $CD $CC $CD $CC $CD $CC $CD $CC
	.db $CD $CC $CF $CE $CF $CE $CF $CE $CF $CE $CF $CE $CF $CE $CF $CE
	.db $CF $CE $CF $CE $CF $CE $CF $CE $CF $CE $CF $CE $CF $CE $CF $CE
	.db $CF $CE
	
; Data from 1DCE4 to 1DD25 (66 bytes)	
_DATA_1DCE4_:	
	.db $FE $08 $16 $03 $16 $03 $16 $03 $16 $03 $16 $03 $16 $03 $16 $03
	.db $16 $03 $16 $03 $16 $03 $16 $03 $16 $03 $16 $03 $16 $03 $16 $03
	.db $16 $03 $03 $16 $03 $16 $03 $16 $03 $16 $03 $16 $03 $16 $03 $16
	.db $03 $16 $03 $16 $03 $16 $03 $16 $03 $16 $03 $16 $03 $16 $03 $16
	.db $03 $16
	
; Data from 1DD26 to 1DD47 (34 bytes)	
_DATA_1DD26_:	
	.db $84 $04 $C0 $04 $20 $22 $D4 $D6 $D8 $DA $C1 $05 $21 $23 $D5 $D7
	.db $D9 $DB $C4 $06 $20 $22 $D8 $DA $D4 $D6 $C5 $07 $21 $23 $D9 $DB
	.db $D5 $D7
	
; Data from 1DD48 to 1DD5D (22 bytes)	
_DATA_1DD48_:	
	.db $54 $04 $C0 $7C $7E $84 $86 $C1 $7D $7F $85 $87 $C2 $80 $82 $88
	.db $8A $C3 $81 $83 $89 $8B
	
; Data from 1DD5E to 1DD6B (14 bytes)	
_DATA_1DD5E_:	
	.db $32 $04 $C5 $C8 $CA $C5 $CA $C9 $C5 $CA $C8 $C5 $C9 $CA
	
; Data from 1DD6C to 1DD71 (6 bytes)	
_DATA_1DD6C_:	
	.db $22 $02 $CC $CE $CD $CF
	
; Data from 1DD72 to 1DD83 (18 bytes)	
_DATA_1DD72_:	
	.db $02 $08 $F8 $FA $F9 $FB $FC $FE $F9 $FB $FC $FE $F9 $FB $FC $FE
	.db $FD $FF
	
; Data from 1DD84 to 1DDF5 (114 bytes)	
_DATA_1DD84_:	
	.db $0E $08 $A1 $A3 $94 $96 $A1 $A3 $94 $96 $A1 $A3 $94 $96 $A1 $A3
	.db $A4 $A5 $95 $97 $A4 $A5 $95 $97 $A4 $A5 $95 $97 $A4 $A5 $94 $96
	.db $98 $9A $94 $96 $98 $9A $94 $96 $98 $9A $94 $96 $95 $97 $99 $9B
	.db $95 $97 $99 $9B $95 $97 $99 $9B $95 $97 $98 $9A $94 $96 $98 $9A
	.db $94 $96 $98 $9A $94 $96 $98 $9A $99 $9B $95 $97 $99 $9B $95 $97
	.db $99 $9B $95 $97 $99 $9B $A0 $A2 $98 $9A $A0 $A2 $98 $9A $A0 $A2
	.db $98 $9A $A0 $A2 $A1 $A3 $99 $9B $A1 $A3 $99 $9B $A1 $A3 $99 $9B
	.db $A1 $A3
	
; Data from 1DDF6 to 1DE37 (66 bytes)	
_DATA_1DDF6_:	
	.db $02 $08 $DB $D6 $D7 $DA $DB $D6 $D7 $DA $DB $D6 $D7 $DA $DB $D6
	.db $D7 $DA $DB $D6 $D7 $DA $DB $D6 $D7 $DA $DB $D6 $D7 $DA $DB $D6
	.db $D7 $DA $D9 $D4 $D5 $D8 $D9 $D4 $D5 $D8 $D9 $D4 $D5 $D8 $D9 $D4
	.db $D5 $D8 $D9 $D4 $D5 $D8 $D9 $D4 $D5 $D8 $D9 $D4 $D5 $D8 $D9 $D4
	.db $D5 $D8
	
; Data from 1DE38 to 1DEB9 (130 bytes)	
_DATA_1DE38_:	
	.db $02 $08 $A1 $A1 $A4 $A0 $A1 $A1 $A4 $A0 $A1 $A1 $A4 $A0 $A1 $A1
	.db $A4 $A0 $A1 $A1 $A4 $A0 $A1 $A1 $A4 $A0 $A1 $A1 $A4 $A0 $A1 $A1
	.db $A4 $A0 $A3 $A3 $A5 $A2 $A3 $A3 $A5 $A2 $A3 $A3 $A5 $A2 $A3 $A3
	.db $A5 $A2 $A3 $A3 $A5 $A2 $A3 $A3 $A5 $A2 $A3 $A3 $A5 $A2 $A3 $A3
	.db $A5 $A2 $A4 $A0 $A1 $A1 $A4 $A0 $A1 $A1 $A4 $A0 $A1 $A1 $A4 $A0
	.db $A1 $A1 $A4 $A0 $A1 $A1 $A4 $A0 $A1 $A1 $A4 $A0 $A1 $A1 $A4 $A0
	.db $A1 $A1 $A5 $A2 $A3 $A3 $A5 $A2 $A3 $A3 $A5 $A2 $A3 $A3 $A5 $A2
	.db $A3 $A3 $A5 $A2 $A3 $A3 $A5 $A2 $A3 $A3 $A5 $A2 $A3 $A3 $A5 $A2
	.db $A3 $A3
	
; Data from 1DEBA to 1DECB (18 bytes)	
_DATA_1DEBA_:	
	.db $42 $04 $CA $D0 $D1 $D0 $CB $5E $5D $5D $CC $5D $5D $5D $CE $CF
	.db $CF $CF
	
; Data from 1DECC to 1DED1 (6 bytes)	
_DATA_1DECC_:	
	.db $11 $04 $C8 $C6 $C7 $C9
	
; Data from 1DED2 to 1DED7 (6 bytes)	
_DATA_1DED2_:	
	.db $22 $02 $CE $CC $CF $CD
	
; Data from 1DED8 to 1FBD7 (7424 bytes)	
_DATA_1DED8_:	
	.incbin "FilesO_1DED8-1FBD7.dat"
	
; Data from 1FBD8 to 1FFFF (1064 bytes)	
_DATA_1FBD8_:	
	.db $3F $7F $00 $01 $FF $F1 $F0 $F8 $FF $FF $CF $EF $00 $01 $FF $FF
	.db $F7 $07 $C7 $E7 $7F $01 $FF $F0 $F9 $00 $01 $FF $7F $3F $00 $06
	.db $F7 $E3 $C1 $00 $03 $DF $00 $05 $8F $00 $03 $BF $00 $04 $1E $1F
	.db $00 $04 $8F $DF $00 $01 $FF $FE $FC $FF $1F $00 $03 $1E $00 $03
	.db $3F $F3 $FB $00 $01 $FF $7D $FF $3D $7D $FD $00 $03 $FF $E3 $E0
	.db $E7 $FF $FF $F9 $F1 $00 $03 $01 $00 $03 $03 $00 $01 $FF $E7 $E0
	.db $FF $E3 $00 $03 $FF $00 $03 $DF $CF $0F $00 $03 $8F $F8 $FC $FE
	.db $3E $FF $1E $3E $FE $FF $8F $0F $FF $CF $00 $03 $DF $FF $FC $3C
	.db $3E $1E $9F $FF $8F $00 $00 $3F $7F $F9 $FF $F0 $F8 $FF $00 $CF
	.db $EF $FF $F7 $FF $07 $FF $00 $01 $00 $01 $00 $FF $F0 $F9 $7F $3F
	.db $00 $01 $00 $00 $05 $F7 $E3 $C1 $00 $01 $00 $FF $DF $00 $04 $8F
	.db $FF $00 $FF $BF $00 $04 $1E $FF $00 $00 $04 $8F $DF $FE $FC $00
	.db $01 $00 $1F $00 $04 $1E $FF $3F $00 $01 $00 $F3 $FB $7D $00 $03
	.db $3D $FF $00 $FF $FF $E3 $FF $E0 $E7 $FF $00 $F1 $00 $04 $01 $FF
	.db $03 $00 $01 $00 $E7 $FF $E0 $FF $E3 $FF $FF $00 $01 $00 $FF $DF
	.db $CF $FF $0F $8F $FF $00 $F8 $FC $3E $00 $03 $1E $FF $00 $8F $FF
	.db $0F $FF $CF $FF $DF $00 $01 $00 $F8 $FF $3C $FF $1E $FF $8F $00
	.db $01 $00 $00 $00 $FF $00 $06 $01 $00 $03 $00 $80 $FF $00 $08 $00
	.db $01 $00 $F0 $00 $03 $00 $7E $01 $00 $03 $00 $80 $40 $3F $00 $05
	.db $00 $14 $22 $C1 $FF $00 $50 $00 $07 $00 $A1 $00 $0A $00 $01 $02
	.db $FC $00 $01 $00 $01 $00 $05 $00 $3F $FF $00 $82 $40 $00 $03 $00
	.db $04 $FF $00 $1C $03 $00 $04 $00 $08 $F0 $00 $05 $00 $03 $18 $07
	.db $00 $05 $00 $00 $01 $FF $FF $00 $10 $C0 $00 $06 $00 $C0 $20 $00
	.db $03 $00 $02 $00 $01 $00 $80 $00 $05 $00 $DF $04 $C0 $00 $01 $00
	.db $20 $00 $01 $00 $10 $00 $01 $00 $8F $00 $00 $00 $80 $00 $00 $00
	.db $04 $7F $00 $03 $FE $00 $01 $FF $BF $C7 $EF $77 $7F $00 $07 $FF
	.db $C3 $FB $F9 $F0 $00 $04 $E0 $FF $FF $00 $04 $FE $7E $7F $E0 $FF
	.db $F0 $F8 $FC $F8 $C1 $03 $3F $00 $03 $1F $1E $4F $9F $3F $77 $3B
	.db $3F $1E $0F $07 $03 $00 $01 $00 $FF $FF $EF $1F $3F $00 $03 $FF
	.db $E0 $F8 $FF $FE $3F $0F $E7 $F3 $7F $FF $3F $7F $00 $01 $FF $BF
	.db $9F $C0 $0C $18 $78 $08 $85 $E3 $F3 $F1 $3F $7F $00 $06 $FF $FF
	.db $F9 $00 $05 $F8 $FD $7F $3F $9F $DE $F9 $FF $F8 $FC $F9 $00 $07
	.db $F8 $FF $E0 $F0 $00 $03 $7F $3F $7F $F0 $C0 $80 $00 $01 $00 $83
	.db $5C $3F $1F $FF $5F $CF $EF $27 $17 $93 $D3 $FF $E3 $F7 $00 $03
	.db $7F $3F $7F $DF $FF $4F $00 $04 $FF $73 $FF $7F $FF $FF $3F $1F
	.db $9F $DF $71 $F9 $FF $FD $7D $B1 $DB $FF $FF $87 $C3 $F3 $00 $01
	.db $FF $C8 $0F $F3 $CB $A3 $83 $D8 $E6 $F0 $00 $04 $FF $EF $D1 $1F
	.db $E7 $87 $0F $9D $3F $8B $81 $45 $FF $FF $7F $D7 $A3 $93 $83 $8B
	.db $8A $83 $8B $00 $04 $FF $EF $16 $F0 $00 $01 $FF $00 $03 $8B $8A
	.db $FF $8B $C7 $00 $03 $FF $CF $1E $F0 $00 $08 $FF $BF $5A $C3 $00
	.db $00 $80 $FF $7F $00 $01 $FF $FD $F0 $C0 $C7 $40 $FF $BF $FF $FF
	.db $FE $FF $38 $88 $A2 $83 $D8 $C6 $F0 $7F $DF $12 $40 $54 $B8 $39
	.db $FE $EF $83 $80 $FF $7F $00 $01 $FF $BF $1F $07 $E3 $80 $FF $7F
	.db $00 $01 $FF $EF $C7 $39 $45 $12 $CA $80 $17 $67 $1F $FD $F0 $90
	.db $2A $8A $81 $45 $FF $FF $7D $00 $00 $00 $04 $FF $F2 $C0 $83 $A0
	.db $00 $04 $FF $FE $38 $10 $D6 $43 $01 $00 $01 $00 $80 $A0 $C0 $70
	.db $1F $60 $FF $A1 $10 $14 $38 $EE $83 $00 $04 $FF $5F $07 $62 $1A
	.db $00 $04 $FF $D7 $01 $00 $01 $00 $1A $64 $14 $00 $01 $00 $82 $02
	.db $27 $1D $F0 $0D $15 $01 $FF $00 $01 $C7 $7C $00 $00 $FC $00 $03
	.db $FF $FD $00 $01 $FF $FC $DF $F9 $00 $06 $FF $39 $BC $FE $FF $FF
	.db $DF $00 $03 $FF $9F $FF $5E $00 $01 $FF $FB $00 $03 $FF $F9 $00
	.db $03 $FF $BF $00 $01 $FF $9F $E7 $F9 $00 $03 $FF $EF $FF $FF $E5
	.db $9B $EB $00 $01 $FF $7F $00 $01 $FF $DF $FF $FF $F2 $EA $FE $00
	.db $05 $FF $00 $00 $80 $FF $00 $00 $01 $FF $F0 $C0 $8C $98 $40 $FF
	.db $81 $00 $01 $FF $FE $38 $FF $11 $24 $58 $5C $A7 $98 $CF $70 $1F
	.db $0D $FF $0A $55 $D2 $39 $EE $83 $80 $FF $01 $00 $01 $FF $1F $07
	.db $82 $02 $80 $FF $01 $00 $01 $FF $C7 $01 $00 $01 $00 $A0 $89 $21
	.db $35 $6A $1A $C7 $1D $F0 $60 $40 $54 $6C $BA $01 $C7 $7C
	.dsb 18, $00
	.db $E0 $00 $E0 $00 $E0 $00 $E0 $00 $00 $00 $00 $00 $EE $EE $00 $00
	.db $EE $EE $00 $00 $EE $EE $00 $00 $EE $EE $00 $00 $EE $EE $00 $00
	.db $EE $EE $00 $00 $EE $EE $00 $00 $00 $00 $00 $00 $E0 $E0 $00 $00
	.db $E0 $E0 $00 $00 $E0 $E0 $00 $00 $E0 $E0 $00 $00 $E0 $E0 $00 $00
	.db $E0 $E0 $00 $00 $E0 $E0 $00 $00 $00 $00 $00 $00
	.dsb 44, $FF
	