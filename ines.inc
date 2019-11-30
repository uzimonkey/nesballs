;vim: ts=8,sw=8
.segment	"HEADER"
.byte		"NES",$1a	;Magic
.byte		INESPRGROMS	;16k PRG-ROM count
.byte		INESCHRROMS	;8k CHR-ROM count
.byte		INESCTRL1	;ROM control byte 1
.byte		INESCTRL2	;ROM control byte 2
.byte		$0,$0,$0,$0,$0,$0,$0,$0
