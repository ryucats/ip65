.import ip65_init
.import ip65_process

.import cfg_mac
.import cfg_ip
.import cfg_netmask
.import cfg_gateway
.import cfg_dns
.import cfg_tftp_server

.import dhcp_init
.import dhcp_server

.import cfg_get_configuration_ptr

.macro init_ip_via_dhcp
  jsr ip65_init
  php
  print_driver_init
  plp
  bcc :+
  print_failed
  sec
  jmp @end_macro
: print_ok
  print_dhcp_init
  jsr dhcp_init
  bcc :+
  print_failed
  sec
  jmp @end_macro
: print_ok
  clc
@end_macro:
.endmacro



; -- LICENSE FOR net.i --
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
