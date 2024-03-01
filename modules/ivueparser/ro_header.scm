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

(include "parse_helpers.scm")

(include "ro_invoke.scm") ;; RemoteOperationInvoke
(include "ro_result.scm") ;; RemoteOperationResult
(include "ro_linkedresult.scm") ;; RemoteOperationLinkedResult
(include "ro_error.scm") ;; RemoteOperationError

;; Session Header and Remote Operation Header
(define (ivueparser:parseSPdu buf)
  (let ((session_id (u8data-u16 (subu8data buf 0 2)))
        (p_context_id (u8data-u16 (subu8data buf 2 4))))
    session_id
  ))
(define (ivueparser:parseROapdus buf)
  (let ((ro_type (u8data-u16 (subu8data buf 4 6)))
        (len (u8data-u16 (subu8data buf 6 8))))
    (if (fx= len (fx- (u8data-length buf) 8))
      ro_type
      -1
    )
  ))

;; RemoteOperationHeader tree
(define (ivueparser:parseRemoteOperationHeader buf ro_type)
  (cond
    ((fx= ro_type ROIV_APDU)
      (ivueparser:parseRemoteOperationInvoke buf))
    ((fx= ro_type RORS_APDU)
      (ivueparser:parseRemoteOperationResult buf))
    ((fx= ro_type ROER_APDU)
      (ivueparser:parseRemoteOperationError buf))
    ((fx= ro_type ROLRS_APDU)
      (ivueparser:parseRemoteOperationLinkedResult buf))
    (else
      (ivueparser:log 1 "ivueparser: unknown ro_type: " ro_type))
  ))

;; The main parsing function
(define (ivueparser:parsedata buf)
  (let ((session_id (ivueparser:parseSPdu buf))
        (ro_type (ivueparser:parseROapdus buf)))
    (cond
      ((fx= session_id #xE100)
        (ivueparser:parseRemoteOperationHeader (u8data-skip buf 8) ro_type)
      )
      ((fx= session_id #xF701)
        (let ((rel_time_stamp (ivueparser:parseRelativeTime (subu8data buf 4 8)))
              (handle_id (u8data-u16 (subu8data buf 10 12)))
              (count (u8data-u16 (subu8data buf 12 14)))
              (len (u8data-u16 (subu8data buf 14 16))))
          (let loop ((n 0)(p (u8data-skip buf 16)))
            (if (or (fx= n count) (fx= (u8data-length p) 0))
              p
              (loop (fx+ n 1) (ivueparser:parseSaObsValueCmp handle_id p))
            )
          )
        )
      )
      (else
        (ivueparser:log 3 "ivueparser: unknown session_id: " session_id " [" (u8data-length buf) "]")
      )
    )
  ))

;;eof
