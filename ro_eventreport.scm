;; Philips Intellivue Parser
;; Matthias Görges, 2016
(include "parse_waveform.scm")

;; Parse Server trend package
(define (ivueparser:parseNetworkTrends buf)
  (let ((id (u8data-u16 (subu8data buf 0 2)))
        (poll_info_list (ivueparser:parsePollInfoList (u8data-skip buf 2))))
    (if (fx= (u8data-length poll_info_list) 0)
      #t
      (ivueparser:log 0 "ivueparser: incomplete parse of NetworkTrend" (u8data-length poll_info_list))
    )
  ))
;; Parse Server waveform package
(define (ivueparser:parseNetworkWaveforms buf)
  (let ((id (u8data-u16 (subu8data buf 0 2)))
        (poll_wave_list (ivueparser:parsePollWaveformList (u8data-skip buf 2))))
    (if (fx= (u8data-length poll_wave_list) 0)
      #t
      (ivueparser:log 0 "ivueparser: incomplete parse of NetworkWaveform" (u8data-length poll_wave_list))
    )
  ))

;; Parse MDS Create Information structure
(define (ivueparser:parseMdsCreateInfo buf)
  (let* ((managed_object (ivueparser:parseManagedObjectId buf))
         (context_id (cadr managed_object))
         (attribute_list (ivueparser:parseAttributeList context_id (u8data-skip buf 6))))
    (if (fx= (u8data-length attribute_list) 0)
      #t
      (ivueparser:log 0 "ivueparser: incomplete parse of NetworkConnectionIndication" (u8data-length attribute_list))
    )
  ))

;; Parse Event Report
(define (ivueparser:parseCmdEventReport buf)
  (let ((managed_object (ivueparser:parseManagedObjectId (subu8data buf 0 6)))
        (event_time (ivueparser:parseRelativeTime (subu8data buf 6 10)))
        (event_type (u8data-u16 (subu8data buf 10 12)))
        (len (u8data-u16 (subu8data buf 12 14))))
    (cond
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
      (else
        (ivueparser:log 2 "ivueparser: ignoring event_type:" (number->string event_type 16) "len:" len))
    )
  ))

;; Parse Confirmed Event Report
(define (ivueparser:parseCmdConfirmedEventReport buf)
  (ivueparser:parseCmdEventReport buf)
)
;; eof
