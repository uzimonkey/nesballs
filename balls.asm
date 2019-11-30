.localchar '@'

.include "nes.inc"
.define		INESPRGROMS		2
.define		INESCHRROMS		1
.define		INESCTRL1		0
.define		INESCTRL2		0
.include	"ines.inc"

.define		OAM_MIRROR		$0200
.define		LEFT			1
.define		RIGHT			248
.define		TOP			1
.define		BOTTOM			231

.define		NUM_BALLS		63
.define		BALL_SPEED		1
.define		BALL_SPRITE		$01

.struct Velocity
yvel	.byte
xvel	.byte
.endstruct

.segment	"ZEROPAGE"
screen_inst:	.res 1
screen:		.res 2
seed:		.res 2
velocities:	.res .sizeof(Velocity)*NUM_BALLS

.segment	"OAM"

.segment	"CODE"
reset:		sei
		cld

		ldx #$ff		;set up stack
		txa			;save $ff in a for later
		txs

		inx			;disable rendering
		stx PPUCTRL1
		stx PPUCTRL2
		stx APUCTRL

		VSYNC			;wait 2 frames
		VSYNC

		lda #$00		;initialize zero page
		ldx #$00
:		sta 0,x
		inx
		bne :-

		lda #$ff		;initialize OAM mirror
		ldx #$00
:		sta OAM_MIRROR,x
		inx
		bne :-

		VSYNC
		lda #>PPUVRAM_PALETTE	;initialize_palette
		sta PPUVRAMADDR2
		lda #<PPUVRAM_PALETTE
		sta PPUVRAMADDR2

		ldx #0
:		lda palette,x
		sta PPUVRAMIO
		inx
		cpx #palettelen
		bne :-

		lda #$12		;set random seed
		sta seed
		lda #$34
		sta seed+1

		lda #$4c		;initialize screen instruction
		sta screen_inst
		lda #<balls
		sta screen
		lda #>balls
		sta screen+1

		ldx #0			;initialize ball sprites
@sprinit:	jsr random		;ypos
		cmp #TOP
		bcs :+
		clc
		adc #(BOTTOM-TOP)
:		cmp #BOTTOM
		bcc :+
		sec
		sbc #(BOTTOM-TOP)
:		sta OAM_MIRROR,x

		inx			;sprite
		lda #BALL_SPRITE
		sta OAM_MIRROR,x

		inx			;attribute
		jsr random
		and #$03
		sta OAM_MIRROR,x

		jsr random		;xpos
		cmp #LEFT
		bcs :+
		clc
		adc #(RIGHT-LEFT)
:		cmp #RIGHT
		bcc :+
		sec
		sbc #(RIGHT-LEFT)
:		inx
		sta OAM_MIRROR,x

		inx
		cpx #(NUM_BALLS*4)
		bne @sprinit

		ldx #0
@velinit:	jsr random		;initialize velocities
		and #$07
		sec
		sbc #$04
		beq @velinit
		sta velocities,x
		inx
		cpx #(NUM_BALLS*.sizeof(Velocity))
		bne @velinit

		lda #PPUCTRL1_NMI	;enable interrupts and drawing
		sta PPUCTRL1
		lda #(PPUCTRL2_SPRVIS|PPUCTRL2_BACKVIS|PPUCTRL2_SPRCLIP|PPUCTRL2_BACKCLIP)
		sta PPUCTRL2

		;cli
		HANG			;stop here forever

balls:		ldx #0			;update ball positions
		ldy #0

		;ypos
@loop:		lda OAM_MIRROR+Sprite::ypos,y
		clc
		adc velocities+Velocity::yvel,x
		sta OAM_MIRROR+Sprite::ypos,y
		cmp #TOP
		bcs :+
		lda velocities+Velocity::yvel,x
		eor #$ff
		sec
		adc #0
		sta velocities+Velocity::yvel,x
		jmp :++
:		lda OAM_MIRROR+Sprite::ypos,y
		cmp #BOTTOM
		bcc :+
		lda velocities+Velocity::yvel,x
		eor #$ff
		sec
		adc #0
		sta velocities+Velocity::yvel,x

		;xpos
:		lda OAM_MIRROR+Sprite::xpos,y
		clc
		adc velocities+Velocity::xvel,x
		sta OAM_MIRROR+Sprite::xpos,y
		cmp #LEFT
		bcs :+
		lda velocities+Velocity::xvel,x
		eor #$ff
		sec
		adc #0
		sta velocities+Velocity::xvel,x
		jmp :++
:		lda OAM_MIRROR+Sprite::xpos,y
		cmp #RIGHT
		bcc :+
		lda velocities+Velocity::xvel,x
		eor #$ff
		sec
		adc #0
		sta velocities+Velocity::xvel,x

:		.repeat .sizeof(Velocity)
		inx
		.endrepeat
		.repeat .sizeof(Sprite)
		iny
		.endrepeat

		cpx #(NUM_BALLS*.sizeof(Velocity))
		bne @loop
		
		rts

nmi:		lda #>OAM_MIRROR	;copy OAM_MIRROR to OAM
		sta PPUSPRITEDMA
		nop

		jsr screen_inst
		;jsr balls
		rti

irq:		rti

random:		ldy #8			;generate random number, clobbers A and Y
		lda seed
:		asl
		rol seed+1
		bcc :+
		eor #$39
:		dey
		bne :--
		sta seed
		rts

palette:	.incbin "balls.dat", $0, $20
palettelen=	*-palette

.segment	"VECTORS"
.addr		nmi
.addr		reset
.addr		irq

.segment	"TILES"
.incbin		"balls.chr"
