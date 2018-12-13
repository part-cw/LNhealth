#|
lnHealth - Health related apps for the LambdaNative framework
Copyright (c) 2009-2018, University of British Columbia
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(define (ivueparser:parseRorlsId buf)
  (let ((state (u8data-u8 (subu8data buf 0 1)))
        (count (u8data-u8 (subu8data buf 1 2))))
    (list state count)
  ))

(define (ivueparser:parseROLRSapdu buf)
  (let ((linked_id (ivueparser:parseRorlsId buf))
        (invoke_id (u8data-u16 (subu8data buf 2 4)))
        (command_type (u8data-u16 (subu8data buf 4 6)))
        (len (u8data-u16 (subu8data buf 6 8))))
    (if (fx= len (fx- (u8data-length buf) 8))
      command_type
      -1
    )
  ))

(define (ivueparser:parseRemoteOperationLinkedResult buf)
  (let ((command_type (ivueparser:parseROLRSapdu buf)))
    (cond
      ((fx= command_type CMD_EVENT_REPORT)
        (ivueparser:parseCmdEventReport (u8data-skip buf 8)))
      ((fx= command_type CMD_CONFIRMED_EVENT_REPORT)
        (ivueparser:parseCmdConfirmedEventReport (u8data-skip buf 8)))
      ((fx= command_type CMD_GET)
        (ivueparser:parseCmdGet (u8data-skip buf 8)))
      ((fx= command_type CMD_SET)
        (ivueparser:parseCmdSet (u8data-skip buf 8)))
      ((fx= command_type CMD_CONFIRMED_SET)
        (ivueparser:parseCmdConfirmedSet (u8data-skip buf 8)))
      ((fx= command_type CMD_CONFIRMED_ACTION)
        (ivueparser:parseCmdConfirmedActionResult (u8data-skip buf 8)))
      (else
        (ivueparser:log 1 "ivueparser: unknown command_type: " command_type))
    )
  ))
;; eof
