;vim: ts=8,sw=8

;PPU - Picture Processing Unit
.define	PPUCTRL1		$2000		;PPU control register 1 (W)
.define	PPUCTRL1_NMI		%10000000	;Execute NMI on blank
.define	PPUCTRL1_MASTER		%01000000	;PPU master/slave slection
.define	PPUCTRL1_SPRSIZE	%00100000	;Sprite size, 0=8x8, 1=8x16
.define	PPUCTRL1_BTBLADDR	%00010000	;Background pattern table address, 0=$0000, 1=$1000
.define	PPUCTRL1_STBLADDR	%00001000	;Sprite pattern table address, 0=$0000, 1=$1000
.define	PPUCTRL1_PPUINCR	%00000100	;PPU address increment, 0=1, 1=32
.define	PPUCTRL1_NTBLADDR_MASK	%00000011	;Name table address in VRAM
.define	PPUCTRL1_NTBLADDR_2000	%00000000	;$2000
.define	PPUCTRL1_NTBLADDR_2400	%00000001	;$4000
.define	PPUCTRL1_NTBLADDR_2800	%00000010	;$8000
.define	PPUCTRL1_NTBLADDR_2C00	%00000011	;$C000

.define	PPUCTRL2		$2001		;PPU control register 2 (W)
.define PPUCTRL2_BACKCOL_MASK	%11100000	;Full background color (when PPUCTRL2_DISP=1)
.define PPUCTRL2_BACKCOL_NONE	%00000000	;None	+--------------+
.define PPUCTRL2_BACKCOL_GREEN	%00100000	;Green	| Use only one |
.define PPUCTRL2_BACKCOL_BLUE	%01000000	;Blue	|  at a time   |
.define PPUCTRL2_BACKCOL_RED	%10000000	;Red	+--------------+
.define PPUCTRL2_INTENCOL_MASK	%11100000	;Color intensity (when PPUCTRL2_DISP=0)
.define PPUCTRL2_INTENCOL_NONE	%00000000	;None	+--------------+
.define PPUCTRL2_INTENCOL_RED	%00100000	;Red	+ Use only one +
.define PPUCTRL2_INTENCOL_GREEN	%01000000	;Green	+  at a time   +
.define PPUCTRL2_INTENCOL_BLUE	%10000000	;Blue	+--------------+
.define PPUCTRL2_SPRVIS		%00010000	;Sprite visibility, 0=invisible, 1=visible
.define PPUCTRL2_BACKVIS	%00001000	;Background visibility, 0=invisible, 1=visible
.define PPUCTRL2_SPRCLIP	%00000100	;Sprite clipping, 0=invisible, 1=visible
.define PPUCTRL2_BACKCLIP	%00000010	;Background clipping, 0=invisible, 1=visible
.define PPUCTRL2_DISP		%00000001	;Display type, 0=color, 1=mono

.define PPUSTAT			$2002		;PPU status register (R)
.define	PPUSTAT_VBLANK		%10000000	;VBlank, 0=not occuring, 1=in vblank
.define PPUSTAT_SPR0		%01000000	;Sprite 0 occurance, 0=spr0 not found, 1=found
.define PPUSTAT_SPRCOUNT	%00100000	;Scanline sprite count, 0=8 or less, 1=more than 8
.define	PPUSTAT_VRAMWR		%00010000	;VRAM write flag, 0=write ok, 1=write ignored

.define PPUSPRADDR		$2003		;SPR-RAM address register (W)
.define	PPUSPRIO		$2004		;SPR-RAM data write (W)

.define PPUVRAMADDR1		$2005		;VRAM address register #1 (W2)
.define PPUVRAMADDR2		$2006		;VRAM address register #2 (W2)
.define PPUVRAMIO		$2007		;VRAM I/O register (RW)

.define PPUSPRITEDMA		$4014		;Sprite DMA register (W)

.define	PPUVRAM_PALETTE		$3f00		;PPU VRAM palette address


;APU - Audio Processing Unit
.define	APUPULSE1CTRL		$4000		;Pulse 1 control (W)
.define APUPULSE1RAMP		$4001		;Pulse 1 ramp control (W)
.define	APUPULSE1FINE		$4002		;Pulse 1 fine tune (W)
.define	APUPULSE1COARSE		$4003		;Pulse 1 coarse tune (W)

.define	APUPULSE2CTRL		$4004		;Pulse 2 control (W)
.define APUPULSE2RAMP		$4005		;Pulse 2 ramp control (W)
.define	APUPULSE2FINE		$4006		;Pulse 2 fine tune (W)
.define	APUPULSE2COARSE		$4007		;Pulse 2 coarse tune (W)

.define	APUTRICTRL1		$4008		;Triangle control 1 (W)
.define	APUTRICTRL2		$4009		;Triangle control 2 (W)
.define	APUTRIFREQ1		$400A		;Triangle frequency 1 (W)
.define APUTRIFREQ2		$400B		;Triangle frequency 2 (W)

.define APUNOISECTRL		$400C		;Noise control 1 (W)
.define APUUNUSED		$400D		;Unusued?
.define APUNOISEFREQ1		$400E		;Noise frequency 1 (W)
.define APUNOISEFREQ2		$400F		;Noise frequency 2 (W)

.define APUDELTACTRL		$4010		;Delta modulation control (W)
.define APUDELTADA		$4011		;Delta modulation D/A (W)
.define APUDELTAADDR		$4012		;Delta modulation address (W)
.define APUDELTALEN		$4013		;Delta modulation data length (W)

.define APUSIGNAL		$4015		;Sound/Vertical clock signal (R)
.define APUSIGNAL_VERT		%01000000	;Vertical clock signal IRQ availability
.define	APUSIGNAL_DELTA		%00010000	;Delta modulation	+----------
.define APUSIGNAL_NOISE		%00001000	;Noise			| 0=not in use
.define	APUSIGNAL_TRIANGLE	%00000100	;Triangle		| 1=in use
.define	APUSIGNAL_PULSE2	%00000010	;Pulse 2		|
.define APUSIGNAL_PULSE1	%00000001	;Pulse 1		+----------

.define APUCTRL			$4015		;APU channel control (W)
.define	APUCTRL_DELTA		%00010000	;Delta modulation	+----------
.define APUCTRL_NOISE		%00001000	;Noise			| 0=not in use
.define	APUCTRL_TRIANGLE	%00000100	;Triangle		| 1=in use
.define	APUCTRL_PULSE2		%00000010	;Pulse 2		|
.define APUCTRL_PULSE1		%00000001	;Pulse 1		+----------

.define	APUJOY1			$4016		;Joypad 1 (RW)
.define	APUJOY2			$4017		;Joypad 2 (RW)
.define	JOY_A			%00000001
.define	JOY_B			%00000010
.define	JOY_SELECT		%00000100
.define	JOY_START		%00001000
.define	JOY_UP			%00010000
.define	JOY_DOWN		%00100000
.define JOY_LEFT		%01000000
.define	JOY_RIGHT		%10000000


;iNES Header
.define	INESCTRL1_MAPPERLOW	%11110000	;Mapper (low nybble)
.define	INESCTRL1_FOURSCREEN	%00001000	;Four screen mirroring
.define INESCTRL1_TRAINER	%00000100	;512-byte trainer present
.define	INESCTRL1_SRAM		%00000010	;SRAM enabled
.define	INESCTRL1_MIRROR	%00000001	;Mirroring, 0=horizontal, 1=vertical

.define INESCTRL2_MAPPERHIGH	%11110000	;Mapper (high nybble)


;Macros
.macro VSYNC
:		bit PPUSTAT
		bpl :-
.endmacro

.macro HANG
:		jmp :-
.endmacro


;Types
.struct Sprite
ypos	.byte
idx	.byte
flags	.byte
xpos	.byte
.endstruct


.macro SPRITEDATA name, ypos, sprite, flags, xpos
.byte ypos, sprite, flags, xpos
.ident(.concat(.string(name),"_SPRITE")) = * - spritedata - .sizeof(Sprite)
.define name 0
.endmacro


.macro ZPAGE_DEFINITIONS lbl, size_, default
lbl: .res size_
.endmacro


.macro ZPAGE_DEFAULTS lbl, size_, default
.byte default
.endmacro


.macro SETSPRITEFRAME data, sprite, num
.repeat num
	iny		;skip ypos
	lda data,x
	sta sprite,y
	inx
	iny
	lda data,x
	sta sprite,y
	inx
	iny
	iny		;skip xpos
.endrepeat
.endmacro
