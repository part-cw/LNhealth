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

(include "ro_header.scm")

(define (ivueparser:parseSessionHeader buf)
  (let* ((type (u8data-u8 (subu8data buf 0 1)))
         (len0 (u8data-u8 (subu8data buf 1 2)))
         (len1 (u8data-u16 (subu8data buf 2 4)))
         (len (if (fx= len0 #xff) len1 len0))
         (data (if (fx= len0 #xff) (u8data-skip buf 4) (u8data-skip buf 2))))
    (cond
      ((fx= type CN_SPDU_SI) ;; Session Connect
        (ivueparser:log 2 "ivueparser: connect from monitor") #f)
      ((fx= type AC_SPDU_SI) ;; Session Accept
        (ivueparser:log 2 "ivueparser: accept from monitor") #f)
      ((fx= type RF_SPDU_SI) ;; Session Refuse
        (ivueparser:log 2 "ivueparser: refuse from monitor") #f)
      ((fx= type FN_SPDU_SI) ;; Session Finish
        (ivueparser:log 2 "ivueparser: finish from monitor") #f)
      ((fx= type DN_SPDU_SI) ;; Session Disconnect
        (ivueparser:log 2 "ivueparser: disconnect from monitor") #f)
      ((fx= type AB_SPDU_SI) ;; Session Abort
        (ivueparser:log 2 "ivueparser: abort from monitor") #f)
      ((fx= type #xE1) ;; Data Export Protocol
        (ivueparser:parsedata buf))
      (else (ivueparser:log 2 "ivueparser: parsing error") #f)
    )
  ))

;; eof
