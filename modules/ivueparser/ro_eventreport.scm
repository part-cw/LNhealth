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

(include "parse_waveform.scm")

;; Parse Server trend package
(define (ivueparser:parseNetworkTrends buf)
  (let ((id (u8data-u16 (subu8data buf 0 2)))
        (poll_info_list (ivueparser:parsePollInfoList (u8data-skip buf 2))))
    (if (fx= (u8data-length poll_info_list) 0)
      #t
      (ivueparser:log 0 "ivueparser: incomplete parse of NetworkTrend [" (u8data-length poll_info_list) "]")
    )
  ))
;; Parse Server waveform package
(define (ivueparser:parseNetworkWaveforms buf)
  (let ((id (u8data-u16 (subu8data buf 0 2)))
        (poll_wave_list (ivueparser:parsePollWaveformList (u8data-skip buf 2))))
    (if (fx= (u8data-length poll_wave_list) 0)
      #t
      (ivueparser:log 0 "ivueparser: incomplete parse of NetworkWaveform [" (u8data-length poll_wave_list) "]")
    )
  ))

;; Parse MDS Create Information structure
(define (ivueparser:parseMdsCreateInfo buf)
  (let* ((managed_object (ivueparser:parseManagedObjectId buf))
         (context_id (cadr managed_object))
         (attribute_list (ivueparser:parseAttributeList context_id (u8data-skip buf 6))))
    (if (fx= (u8data-length attribute_list) 0)
      #t
      (ivueparser:log 0 "ivueparser: incomplete parse of NetworkConnectionIndication [" (u8data-length attribute_list) "]")
    )
  ))

;; Parse Event Report
(define (ivueparser:parseCmdEventReport buf)
  (let ((managed_object (ivueparser:parseManagedObjectId buf))
        (event_time (ivueparser:parseRelativeTime (subu8data buf 6 10)))
        (event_type (u8data-u16 (subu8data buf 10 12)))
        (len (u8data-u16 (subu8data buf 12 14))))
    (cond
      ((fx= len 0) #f)
      ((fx= event_type #x0d01)
        (ivueparser:parseNetworkTrends (u8data-skip buf 14)))
      ((fx= event_type #x0d03)
        (ivueparser:parseNetworkTrends (u8data-skip buf 14)))
      ((fx= event_type #x0d16)
        (ivueparser:parseNetworkTrends (u8data-skip buf 14)))
      ((fx= event_type #x0d04)
        (ivueparser:parseNetworkWaveforms (u8data-skip buf 14)))
      ((fx= event_type #x0d06)
        (ivueparser:parseMdsCreateInfo (u8data-skip buf 14)))
      ((fx= event_type #x0d05) ;; ModeOp, MDSStatus, MdsGenSystemInfo
        (ivueparser:parseAttributeList (cadr managed_object) (u8data-skip buf 14)))
      ((fx= event_type #x0d08) ;; Composit Single Bed MDS information - this is incomplete!!!
        ;; (ivueparser:parseAttributeList (cadr managed_object) (u8data-skip buf (+ 14 22))
        (ivueparser:log 2 "ivueparser: ignoring known event_type: 0d08 [" len "]"))
      ((fx= event_type #x0d19)
        ;; Structure-wise this works, but the second element is a list of three numbered elements?
        ;; (ivueparser:parseObservationPoll (u8data-skip buf 14))
        (ivueparser:log 2 "ivueparser: ignoring known event_type: 0d19 [" len "]"))
      ((fx= event_type #x0d14)
        ;; ManagedObjectId, len=8, bytes
        (ivueparser:log 2 "ivueparser: ignoring known event_type: 0d14 [" len "]"))
      ((fx= event_type #x0d0b)
        (ivueparser:log 2 "ivueparser: ignoring known event_type: 0d0b [" len "]")
      )
      (else
        (ivueparser:log 2 "ivueparser: ignoring event_type: " (number->string event_type 16) " [" len "]"))
    )
  ))

;; Parse Confirmed Event Report
(define (ivueparser:parseCmdConfirmedEventReport buf)
  (ivueparser:parseCmdEventReport buf)
)
;; eof
