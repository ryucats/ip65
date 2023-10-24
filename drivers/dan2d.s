;
; Copyright (c) 2013, Daniel Marks
; All rights reserved. 
;
; Redistribution and use in source and binary forms, with or without 
; modification, are permitted provided that the following conditions 
; are met: 
; 1. Redistributions of source code must retain the above copyright 
;    notice, this list of conditions and the following disclaimer. 
; 2. Redistributions in binary form must reproduce the above copyright 
;    notice, this list of conditions and the following disclaimer in the 
;    documentation and/or other materials provided with the distribution. 
; 3. Neither the name of the Institute nor the names of its contributors 
;    may be used to endorse or promote products derived from this software 
;    without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE INSTITUTE AND CONTRIBUTORS ``AS IS'' AND 
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
; ARE DISCLAIMED.  IN NO EVENT SHALL THE INSTITUTE OR CONTRIBUTORS BE LIABLE 
; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS 
; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY 
; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
; SUCH DAMAGE. 
;
; This file is part of the Contiki operating system.
; 
; Author: Daniel Marks <profdc9@gmail.com>
;
;---------------------------------------------------------------------

	.macpack	module
	module_header	_dan2d

	; Driver signature
	.byte	$65, $74, $68	; "eth"
	.byte	$01		; Ethernet driver API version number

	; Ethernet address
mac:	.byte	$00, $08, $DC	; OUI of WIZnet
	.byte	$A2, $A2, $A2

	; Buffer attributes
bufaddr:.res	2		; Address
bufsize:.res	2		; Size

	; Jump table.
	jmp init
	jmp poll
	jmp send
	jmp exit

;---------------------------------------------------------------------

	.if DYN_DRV

	.zeropage
sp:	.res	2		; Stack pointer (Do not trash !)
reg:	.res	2		; Pointer Register content
ptr:	.res	2		; Indirect addressing pointer
len:	.res	2		; Data length

	.else

	.include "zeropage.inc"
reg	:=	ptr1		; Pointer Register content
ptr	:=	ptr2		; Indirect addressing pointer
len	:=	ptr3		; Data length

	.endif

;=====================================================================

	.rodata

fixup:	.byte	fixup02-fixup01, fixup03-fixup02, fixup04-fixup03
	.byte	fixup05-fixup04, fixup06-fixup05, fixup07-fixup06
	.byte	fixup08-fixup07, fixup09-fixup08

fixups	= * - fixup

;---------------------------------------------------------------------

; The addresses are fixed up at runtime
porta		:= $C080
portb		:= $C081
portc		:= $C082
mode8255		:= $C083

;---------------------------------------------------------------------

	.data

init:
	; Convert slot number to slot I/O offset
	asl
	asl
	asl
	asl
	sta reg

	; Start with first fixup location
	lda #<(fixup01+1)
	ldx #>(fixup01+1)
	sta ptr
	stx ptr+1
	ldx #$FF
	ldy #$00

	; Fixup address at location
:	lda (ptr),y
	and #%10001111		; Allow for re-init
	ora reg
	sta (ptr),y
	
	; Advance to next fixup location
	inx
	cpx #fixups
	bcs :+
	lda ptr
	clc
	adc fixup,x
	sta ptr
	bcc :-
	inc ptr+1
	bcs :-			; Always
:

;=====================================================================

sendinit:
	jsr set8255
	lda #$ac
	jsr wrbt
	lda #$10
	jsr wrbt
	lda #<mac
	sta ptr
	lda #>mac
	sta ptr+1
	ldx #6
	jsr wrtpg
	jsr rdbt
	cmp #0
	bne initer
	clc
	rts
initer: sec
	rts

;---------------------------------------------------------------------

poll:
	lda #$ac	; send magic byte
	jsr wrbt
	lda #$11	; send command $11
	jsr wrbt
	lda bufsize ; send the maximum buffer size
	jsr wrbt
	lda bufsize+1
	jsr wrbt
	
	jsr rdbt	; get the low and high byte of length
	sta	len
	jsr rdbt	; is equal to zero for no packet available
	sta len+1

	lda len+1
	bne ispacket
	lda len
	bne ispacket

nopacket:
	lda #$00	; register no packet
	tax
	sec
	rts

ispacket:
; Is bufsize < length ?
	lda bufsize
	cmp len
	lda bufsize+1
	sbc len+1
	bcc nopacket   ; this should not happen....

recvpacket:
	lda bufaddr
	sta ptr
	lda bufaddr+1
	sta ptr+1
	jsr rdlng

quitpkt:
	lda len
	ldx len+1
	clc
	rts


;---------------------------------------------------------------------

send:
	.ifdef __ATARI__
	; Select parallel device
	lda pdbit
	sta shpdvs
	sta pdvs
	.endif

	; Save data length
	sta len
	stx len+1

	lda #$ac	; send magic byte
	jsr wrbt
	lda #$12	; send command $12
	jsr wrbt
	lda len
	jsr wrbt	; write length
	lda len+1
	jsr wrbt

	lda bufaddr
	sta ptr
	lda bufaddr+1
	sta ptr+1
	jsr wrlng	; send the packet

	jsr rdbt	; read the result
	cmp #0
	beq sendnoerr
	sec
	rts
sendnoerr:clc
	rts

exit:rts

;---------------------------------------------------------------------
; Write data to the 8255 (256 bytes or less)
wrtpg:ldy #0
wrtpg2:lda (ptr),y    ; get a byte
fixup01:sta porta     ; send it to the arduino
fixup02:lda portc     ; wait until its received
	bpl fixup02
	iny	               ; increment to next byte
	dex                ; decrease countdown
	bne	wrtpg2         ; keep copying while x > 0
	rts

;--------------------------------------------------------------------
; Write data to the 8255 (len number of bytes)
wrlng:
	lda ptr+1          ; save ptr+1
	pha
	lda len+1
	pha
	beq wrlng3
wrlng2:ldx #0
	jsr wrtpg
	inc ptr+1          ; increment to next page
	dec len+1          ; decrease count by 256 bytes
	bne wrlng2
wrlng3: ldx len
	beq wrlng4
	jsr wrtpg
wrlng4: pla
	sta len+1
	pla
	sta ptr+1
	rts


;---------------------------------------------------------------------
; Read data from the 8255 (256 bytes or less)
rdpg:ldy #0
fixup03:lda portc       ; see if a byte is available
	and #$20
	beq fixup03
fixup04:lda porta         ; get the byte
	sta (ptr),y
	iny
	dex
	bne fixup03
	rts

;--------------------------------------------------------------------
; Read data from the 8255 (len number of bytes)
rdlng:
	lda ptr+1          ; save ptr+1
	pha
	lda len+1
	pha
	beq rdlng3
rdlng2:	ldx #0
	jsr rdpg
	inc ptr+1          ; increment to next page
	dec len+1          ; decrease count by 256 bytes
	bne rdlng2
rdlng3: ldx len
	beq rdlng4
	jsr rdpg
rdlng4: pla
	sta len+1
	pla
	sta	ptr+1
	rts

;---------------------------------------------------------------------
; Write a byte to the 8255
wrbt:
fixup05:sta porta	  ; send the byte
fixup06:lda portc     ; wait until its received
	bpl fixup06
	rts


;---------------------------------------------------------------------
; Read a byte from 8255
rdbt:
fixup07:lda portc       ; see if a byte is available
	and #$20
	beq fixup07
fixup08:lda porta         ; get the byte
	rts

;---------------------------------------------------------------------
; Set Mode 2 on 8255
set8255:lda #$fa
fixup09:sta mode8255
	rts
