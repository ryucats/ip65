.export check_for_abort_key
.export abort_key
.exportzp abort_key_default = $9b
.exportzp abort_key_disable = $80


.data

abort_key: .byte $9b            ; ESC


.code

; check whether the abort key is being pressed
; inputs: none
; outputs: sec if abort key pressed, clear otherwise
check_for_abort_key:
  lda $c000                     ; current key pressed
  cmp abort_key
  bne :+
  bit $c010                     ; clear the keyboard strobe
  sec
  rts
: clc
  rts



; -- LICENSE FOR a2_input.s --
; The contents of this file are subject to the Mozilla Public License
; Version 1.1 (the "License"); you may not use this file except in
; compliance with the License. You may obtain a copy of the License at
; http://www.mozilla.org/MPL/
;
; Software distributed under the License is distributed on an "AS IS"
; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
; License for the specific language governing rights and limitations
; under the License.
;
; The Original Code is ip65.
;
; The Initial Developer of the Original Code is Jonno Downes,
; jonno@jamtronix.com.
; Portions created by the Initial Developer are Copyright (C) 2009
; Jonno Downes. All Rights Reserved.
; -- LICENSE END --
