; timer routines
;
; the timer should be a 16-bit counter that's incremented by about
; 1000 units per second. it doesn't have to be particularly accurate,
; if you're working with e.g. a 60 Hz VBLANK IRQ, adding 17 to the
; counter every frame would be just fine.


	.include "../inc/common.i"


	.export timer_init
	.export timer_read

	.code

;initialize timers
;inputs: none
;outputs: none
timer_init:
	lda #$80		; stop timers
	sta $dd0e
	sta $dd0f

	ldax #999		; timer A to 1000 cycles
	stax $dd04

	ldax #$ffff		; timer B to max cycles
	stax $dd06

	lda #$81		; timer A in continuous mode
	sta $dd0e

	lda #$c1		; timer B to count timer A underflows
	sta $dd0f

	rts


;initialize timers
;inputs: none
;outputs: AX = count in milliseconds sent last call to timer_init
timer_read:
	lda $dd07		; cia counts backwards, return inverted value
	eor #$ff
	tax
	lda $dd06
	eor #$ff
	rts

