;; philips monitor parser

(define ivueparser:islinked #f)
(define ivueparser:islast #f)
(define ivueparser:error #f)

;; table to associate handles with labels
(define ivueparser:labellut (make-table init: 0))

(define ivueparser:handleid 0)
(define ivueparser:cmd_type 0)
(define ivueparser:priolist '())
(define ivueparser:handlelut (make-table init: 0))


;; parse a raw (unvalidated) frame from the monitor
;; returns true if more messages are pending
(define (ivueparser:parseframe buf)
  (if (u8data:sane? buf)
    (let ((msg (ivueparser:decodemessage buf)))
      (if (u8data:sane? msg) 
        (ivueparser:parsemessage msg)
        (begin 
          (log-error "ivueparser: parseframe: malformed frame")
          #f
        )
      )
    ) 
    #f
))

(define (ivueparser:decodeframe ibuf)
  (let ((buflen (u8data-length ibuf)))
    (if (not (and (fx= (u8data-ref ibuf 0) #xC0)
                  (fx= (u8data-ref ibuf (- buflen 1))) #xC1))
      (begin 
        (log-error "Invalid input frame") 
        #f
      )
      (let loop ((i 1)(obuf (u8vector))(escaped #f))
        (if (fx= i (fx- buflen 1)) (u8vector->u8data obuf)
          (let* ((c (u8data-ref ibuf i))
                 (newescaped (fx= c #x7D)))
            (loop (fx+ i 1) (u8vector-append obuf
              (if newescaped (u8vector) (if escaped (u8vector (bitwise-xor c #x20)) (u8vector c)))) newescaped)
           )
         )
       )
     )
))

(define (ivueparser:decodecrc buf)
  (let* ((buflen (u8data-length buf))
         (bufcrc (bitwise-ior (arithmetic-shift (u8data-ref buf (fx- buflen 1)) 8) 
           (u8data-ref buf (fx- buflen 2)))
         )
         (mycrc (ivueparser:crc (u8data->u8vector buf) 0 (fx- buflen 2))))
      ;; the bitwise and's are needed to eliminate sign!
   (if (not (fx= (bitwise-and bufcrc #xffff)
                 (bitwise-and mycrc #xffff))) 
     (begin
       (log-error "ivueparser: bad input frame crc") 
       #f
     )
     (subu8data buf 0 (fx- buflen 2))
    )
))
 
(define (ivueparser:decodeheader buf)
  (if (or (not (= (u8data-ref buf 0) #x11))
           (not (= (u8data-ref buf 1) #x01)))
    (begin (log-error "ivueparser: bad input frame header") #f)
      (let ((buflen (u8data-length buf))
            (mylen (fx+ (arithmetic-shift (u8data-ref buf 2) 8)
                (u8data-ref buf 3))))
        (if (not (fx= (fx- buflen 4) mylen)) 
          (begin
            (log-error "ivueparser: input frame length mismatch") 
            #f
          )
          (subu8data buf 4 buflen)))))

(define (ivueparser:decodemessage buf)
  (let ((f (ivueparser:decodeframe buf)))
    (if (u8data:sane? f)
      (let ((c (ivueparser:decodecrc f)))
        (if (u8data:sane? c) 
          (ivueparser:decodeheader c) 
          #f
        )
      )
      #f
    )
))

;; parse an (already validated) message from the monitor 
;; returns true if more messages are pending
(define (ivueparser:parsemessage buf)    
  (let ((indicator (u8data-ref buf 0)))
    (cond 
       ((fx= indicator #x00) ;; Connect indication 
          (log-system "ivueparser: connect from monitor") #f)
       ((fx= indicator #x0E) ;; association response
          (log-system "ivueparser: association response") #f)
       ((fx= indicator #x0C) ;; Refuse
          (set! ivueparser:error #t)
          (log-error "ivueparser: refuse from monitor") #f)
       ((fx= indicator #x0A) ;; Release response
          (log-error "ivueparser: release from monitor") #f)
        ((fx= indicator #x19) ;; Association abort
          (set! ivueparser:error #t)
          (log-error "ivueparser: abort from monitor") #f)
        ((fx= indicator #xE1) ;; Data Export Protocol
           (let ((roapdus (u8data-ref buf 5)))
              (cond 
                ((fx= roapdus ROIV_APDU)
                  (log-debug "ivueparser: mds create event" 1) 
                  (ivueparser:parsecentralstation buf) #f)
                ((fx= roapdus RORS_APDU)
                  ;;(log-debug "ivueparser: frame single" 1) 
                  (set! ivueparser:islinked #f)
                  (ivueparser:parsedataresult buf #f) #f)
                ((fx= roapdus ROLRS_APDU)
                  ;;(log-debug "ivueparser: linked data result" 1) 
                  (set! ivueparser:islinked #t)
                  (ivueparser:parsedataresult buf #t) #t)
                ((fx= roapdus ROER_APDU)
                  (set! ivueparser:error #t)
                  (log-error "ivueparser: error message " (u8data->u8vector buf)) #f)
                (else 
                  (set! ivueparser:error #t)
                  (log-error "ivueparser: data export protocol error") #f))))
        (else  
          (set! ivueparser:error #t)
          (log-error "ivueparser: parsing error") #f)
 )))

;; try to guess what type of station frame we have
;; 1 = wave, 2 = trend, 0 = just plain weiirrrrd
(define (ivueparser:stationtype buf)
  (let* ((len (u8data-length buf))
         (val (if (> len 40) (u8data-u16 (subu8data buf 24 26)) 0)))
   (cond ((= val #x0d04) 1)
         ((= val #x0d03) 2)
         ((= val #x0d01) 2)
         ((= val #x0d16) 2)
         (else (log-debug (string-append "ignoring type=" (number->string val 16)) 1) 0))))

;; parse a central station data frame
(define (ivueparser:parsecentralstation buf)
  (log-debug (string-append "ivueparser: central station frame length="
                (number->string (u8data-length buf))) 1)
  (let* ((tpe (ivueparser:stationtype buf))
         (res  (cond ((= tpe 1) (ivueparser:parseServerWaveforms buf))
                     ((= tpe 2) (ivueparser:parseServerTrends buf))
                     (else buf))))
   (if (> (u8data-length res) 0)
     (log-debug (string-append
        "ivueparser: central station: message with "
          (number->string (u8data-length res))
         " bytes of " (number->string (u8data-length buf))
         " trailing??") 1))
  #t))

;; server waveforms ----

(define (ivueparser:parseServerWaveforms buf)
  (if (> (u8data-length buf) 40)
  (let ((payload (ivueparser:skip buf 34)))
     (let loop ((p payload))
       (if (= (u8data-length p) 0) p
         (loop (ivueparser:parseServerWaveformBlock p)))))))

(define (ivueparser:parseServerWaveformBlock buf)
  (let ((payload (ivueparser:skip buf 6))
        (count (u8data-u16 (subu8data buf 2 4)))
;;      (len   (u8data-u16 (subu8data buf 4 6)))
       )
       (let loop ((n 0)(p payload))
          (if (fx= n count) p 
             (loop (fx+ n 1) 
                 (ivueparser:parseServerWaveform p))))))

;; parse a server waveform frame  
;; this is purely empirical from inspecting raw frames
(define (ivueparser:parseServerWaveform buf)
  (log-debug (string-append "parseServerWaveform len=" (number->string (u8data-length buf))) 1)
  (let* ((payload   (ivueparser:skip buf 8))
         (handle_id (u8data-u16 (subu8data buf 0 2))) ;; not sure??
         (physio_id (u8data-u16 (subu8data buf 2 4)))
         (len       (u8data-u16 (subu8data buf 6 8)))
         (count (/ len 2))
         (data (let loop ((n 0)(p payload)(l '()))
            (if (= n count) l (loop (+ n 1) 
              (ivueparser:skip p 2) (append l (list
                 (u8data-u16 (subu8data p 0 2)))))))))
    (log-debug (string-append "parseServerWaveform: wave [" 
        (number->string physio_id) "] n=" (number->string count)) 1)
    (ivueparser:storage-data ivueparser:store handle_id physio_id data)
    (ivueparser:skip payload len)))
  
;; server trends and stuff ----

(define (ivueparser:parseServerTrends buf)
  (if (> (u8data-length buf) 56)
  (let ((payload (ivueparser:skip buf 34)))
     (let loop ((p payload))
       (if (= (u8data-length p) 0) p
         (loop (ivueparser:parseServerTrendBlock p)))))))

(define (ivueparser:parseServerTrendBlock buf)
  (let ((payload (ivueparser:skip buf 6))
        (count (u8data-u16 (subu8data buf 2 4)))
;;      (len   (u8data-u16 (subu8data buf 4 6)))
       )
       (let loop ((n 0)(p payload))
          (if (fx= n count) p 
             (loop (fx+ n 1) 
                 (ivueparser:parseServerTrend p))))))

(define (ivueparser:parseServerTrend buf)
  (let ((payload (ivueparser:skip buf 6))
        (id (u8data-u16 (subu8data buf 0 2)))
        (count (u8data-u16 (subu8data buf 2 4)))
        (len (u8data-u16 (subu8data buf 4 6))))
     (log-debug (string-append "parseServerTrend: count="
       (number->string count) " [ id=" (number->string id) "]") 1)
     (set! ivueparser:handleid id)
     (let loop ((n 0)(p payload))
        (if (fx< n count) (loop (fx+ n 1) (ivueparser:parseAttribute p))))
    (ivueparser:skip payload len)))

;; ----------------------
 
;; parse the data results - this is where the magic happens!
(define (ivueparser:parsedataresult buf islinked)
  (log-debug "ivueparser: parse begin" 1)
  (let* ((isextended (fx= (u8data-ref buf (fx+ 21 (if islinked 2 0))) #x3B))
         (step1 (ivueparser:parseSPdpu buf))
         (step2 (ivueparser:parseROapdus step1))
         (step3 (if islinked (ivueparser:parseROLRSapdu step2)
                  (ivueparser:parseRORSapdu step2))))
    (cond
      ((fx= ivueparser:cmd_type CMD_CONFIRMED_ACTION)
         (ivueparser:doActionResult step3 isextended))
      ((fx= ivueparser:cmd_type CMD_GET)
         (ivueparser:doGetResult step3))
      ((fx= ivueparser:cmd_type CMD_CONFIRMED_SET)
         (ivueparser:doSetResult step3))
      (else (log-error (string-append 
               "ivueparser: unknown message type: "
               (number->string ivueparser:cmd_type)))))))

;; handle an ActionResult
(define (ivueparser:doActionResult buf isextended)
  (log-debug "ActionResult" 1)
  (let* ((step4 (ivueparser:parseActionResult buf))
        (step5 (ivueparser:parsePollMdibDataReply step4 isextended))
        (step6 (if (fx> (u8data-length step5) 2) ;; skip empty lists
                  (ivueparser:parsePollInfoList step5)
                  (u8data)
         )))
   (if (fx= (u8data-length step5) 4)
     (set! ivueparser:islast #t) (set! ivueparser:islast #f))
   (if (> (u8data-length step6) 0)
     (log-error (string-append
        "ivueparser: ActionResult: message with "
          (number->string (u8data-length step6))
         " bytes of " (number->string (u8data-length buf))
         " trailing??")))
  #t))

;; handle a GetResult
;; this would be a priority list request response
(define (ivueparser:doGetResult buf)
  (log-debug "GetResult" 1)
  (let* ((step4 (ivueparser:parseGetResult buf))
         (step5 (ivueparser:parseAttributeList step4)))
   (if (> (u8data-length step5) 0)
     (log-error (string-append
        "ivueparser: GetResult: message with "
          (number->string (u8data-length step5))
         " bytes of " (number->string (u8data-length buf))
         " trailing??")))
  #t))

;; handle a SetResult
(define (ivueparser:doSetResult buf)
  (log-debug "SetResult" 1)
  (let* ((step4 (ivueparser:parseSetResult buf))
        (step5 (ivueparser:parseAttributeList step4)))
   (if (> (u8data-length step5) 0)
     (log-error (string-append
        "ivueparser: SetResult: message with "
          (number->string (u8data-length step5))
         " bytes of " (number->string (u8data-length buf))
         " trailing??")))
  #t))

;; ---------------------

;; skip ahead to next object
(define (ivueparser:skip buf n)
  (subu8data buf n (u8data-length buf)))

;; there is no information to be gained here
;; we simply skip to the important stuff
(define (ivueparser:parseSPdpu buf) (ivueparser:skip buf 4))
(define (ivueparser:parseROapdus buf) (ivueparser:skip buf 4))

;; (define (ivueparser:parseRORSapdu buf) (ivueparser:skip buf 6))
(define (ivueparser:parseRORSapdu buf) 
  (let ((payload (ivueparser:skip buf 6))
        (cmd_type (u8data-u16 (subu8data buf 2 4))))
  (set! ivueparser:cmd_type cmd_type) 
  payload))

;;(define (ivueparser:parseROLRSapdu buf) (ivueparser:skip buf 8))
(define (ivueparser:parseROLRSapdu buf) 
  (let ((payload (ivueparser:skip buf 8))
        (cmd_type (u8data-u16 (subu8data buf 4 6))))
  (set! ivueparser:cmd_type cmd_type) 
  payload))

(define (ivueparser:parseActionResult buf) (ivueparser:skip buf 10))
(define (ivueparser:parseGetResult buf) (ivueparser:skip buf 6))
(define (ivueparser:parseSetResult buf) (ivueparser:skip buf 6))
(define (ivueparser:parsePollMdibDataReply buf isext)
  (ivueparser:skip buf (if isext 22 20)))

(define (ivueparser:parsePollInfoList buf)
  (let ((payload (ivueparser:skip buf 4))
        (count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(p payload))
       (if (fx< n count)
         (loop (fx+ n 1) (ivueparser:parseSingleContextPoll p))))
    (ivueparser:skip payload len)))

(define (ivueparser:parseSingleContextPoll buf)
  (let ((payload (ivueparser:skip buf 6))
;;      (context_id (u8data-u16 (subu8data buf 0 2)))
        (count (u8data-u16 (subu8data buf 2 4)))
        (len (u8data-u16 (subu8data buf 4 6))))
    (let loop ((n 0)(p payload))
       (if (fx< n count)
         (loop (fx+ n 1) (ivueparser:parseObservationPoll p))))
     (ivueparser:skip payload len)))

(define (ivueparser:parseObservationPoll buf)
  (let ((payload (ivueparser:skip buf 6))
        (handle_id (u8data-u16 (subu8data buf 0 2)))
        (count (u8data-u16 (subu8data buf 2 4)))
        (len (u8data-u16 (subu8data buf 4 6))))
    (set! ivueparser:handleid handle_id)
;;    (ivueparser:storage-add ivueparser:store handle_id)
    (let loop ((n 0)(p payload))
       (if (fx< n count)
         (loop (fx+ n 1) (ivueparser:parseAttribute p))))
    (ivueparser:skip payload len)))
 
(define (ivueparser:parseAttributeList buf)
  (let ((payload (ivueparser:skip buf 4))
        (count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
 ;;   (log-debug (string-append "parseAttributeList count="
 ;;     (number->string count) " [" (number->string len) "]") 1)
    (let loop ((n 0)(p payload))
      (if (fx< n count)
        (loop (fx+ n 1) (ivueparser:parseAttribute p))))
    (ivueparser:skip payload len)))
 
(define (ivueparser:parseAttribute buf)    
 ;; (log-debug "parseAttribute" 1)
  (let ((payload (ivueparser:skip buf 4))
        (id (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
  (cond 
;;   ((fx= id NOM_ATTR_TIME_PD_SAMP)       ;; AttrSamplePeriod
;;       (ivueparser:parseAttrSamplePeriod payload))
;;    ((fx= id NOM_ATTR_SA_SPECN)       ;; AttrSampleArraySpec
;;       (ivueparser:parseAttrSampleArraySpec payload))
;;    ((fx= id NOM_ATTR_METRIC_INFO_LABEL) ;; AttrMetricInfoLabel
;;       (ivueparser:parseAttrMetricInfoLabel payload))
    ((fx= id  NOM_ATTR_NU_VAL_OBS)       ;; NuObsValue
       (ivueparser:parseNuObsValue payload))
    ((fx= id  NOM_ATTR_NU_CMPD_VAL_OBS)  ;; NuObsValueCmp
       (ivueparser:parseNuObsValueCmp payload))
    ((fx= id  NOM_ATTR_SA_VAL_OBS)       ;; SaObsValue
       (ivueparser:parseSaObsValue payload))
    ((fx= id  NOM_ATTR_SA_CMPD_VAL_OBS)  ;; SaObsValueCmp
       (ivueparser:parseSaObsValueCmp payload))
    ((fx= id NOM_ATTR_SCALE_SPECN_I16) ;; AttrScaleSpec
       (ivueparser:parseAttrScaleSpec payload))
    ((fx= id NOM_ATTR_ID_LABEL) ;; AttrIdLabel
       (ivueparser:parseAttrIdLabel payload))
    ((fx= id NOM_ATTR_POLL_RTSA_PRIO_LIST)  ;; PrioList
       (ivueparser:parsePrioList payload))
    ((fx= id NOM_ATTR_ID_BED_LABEL) ;; BedLabel
       (ivueparser:parseBedLabel payload len))
    ((fx= id NOM_ATTR_TIME_STAMP_ABS) ;; Absolute Timestamp (from boot)
       (ivueparser:parseAbsoluteTimeStamp payload))
    ((fx= id NOM_ATTR_TIME_STAMP_REL) ;; Relative Timestamp
       (ivueparser:parseRelativeTimeStamp payload))
    (else (log-debug "ivueparser: unknown attribute " id " [" len "]" 1) )
    )
   (ivueparser:skip payload len)))


(define (ivueparser:parsePrioList buf)
  (let ((payload (ivueparser:skip buf 4))
        (count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
  (set! ivueparser:priolist '())
  (let loop ((n 0)(p payload)) 
    (if (fx< n count) 
      (loop (fx+ n 1) 
      (ivueparser:parseTextId p))
    )
  )
  (ivueparser:skip payload len)
))

(define (ivueparser:parseTextId buf)
  (let ((payload (ivueparser:skip buf 4))
        (text_id (u8data-u32 (subu8data buf 0 4))))
;;    (log-status (string-append "ivueparser: TextId=" (number->string text_id)))
    (set! ivueparser:priolist (append ivueparser:priolist (list text_id)))
    payload
))

(define (ivueparser:parseRelativeTimeStamp buf)
  (let ((ts (fl/ (flo (u8data-u32 (subu8data buf 0 4))) 8000.))
        (abs_time (store-ref ivueparser:store "abs_time_stamp"))
        (rel_time (store-ref ivueparser:store "rel_time_stamp")))
    (store-set! ivueparser:store "rel_time_stamp" ts "ivue")
    (if (and abs_time rel_time (> ts rel_time))
      (store-set! ivueparser:store "timestamp" (+ abs_time ts) "ivue")
    )
  ))

(define (ivueparser:parseAbsoluteTimeStamp buf)
  (let ((century (ivueparser:decodebcd (u8data-u8 (subu8data buf 0 1))))
        (year (ivueparser:decodebcd (u8data-u8 (subu8data buf 1 2))))
        (month (ivueparser:decodebcd (u8data-u8 (subu8data buf 2 3))))
        (day (ivueparser:decodebcd (u8data-u8 (subu8data buf 3 4))))
        (hour (ivueparser:decodebcd (u8data-u8 (subu8data buf 4 5))))
        (minute (ivueparser:decodebcd (u8data-u8 (subu8data buf 5 6))))
        (second (ivueparser:decodebcd (u8data-u8 (subu8data buf 7 8)))))
    (if century (store-set! ivueparser:store "abs_time_stamp"
      (string->seconds (string-append century year month day "-" hour minute second) "%Y%m%d-%H%M%S") "ivue"
    ))
  ))

(define (ivueparser:parseNuObsValueCmp buf)
  (let ((payload (ivueparser:skip buf 4))
        (count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(p payload))
       (if (fx< n count) (loop (fx+ n 1) (ivueparser:parseNuObsValue p))))
     (ivueparser:skip payload len)))
    
(define (ivueparser:parseNuObsValue buf)
  (let ((payload (ivueparser:skip buf 10))
        (physio_id (u8data-u16 (subu8data buf 0 2)))
        (state (u8data-u16 (subu8data buf 2 4)))
;;      (unit (u8data-u16 (subu8data buf 4 6)))
        (value (ivueparser:decodef32 (subu8data buf 6 10))))
;;     (log-debug (format "NuObsValue: data [~D] ~F" physio_id value) 1)
       (if (not (or (= value 8388607.) (> (bitwise-and state #xff00) 0))) ;; ignore invalid data
         (ivueparser:setphys! ivueparser:store physio_id 
           (table-ref ivueparser:labellut ivueparser:handleid) value)
         (let ((name (ivueparser:findphys physio_id (table-ref ivueparser:labellut ivueparser:handleid))))
           (if name (store-clear! ivueparser:store name))
         )
       )

     payload))

(define (ivueparser:parseSaObsValueCmp buf)
  (let ((payload (ivueparser:skip buf 4))
        (count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
     (let loop ((n 0)(p payload))
        (if (fx< n count) (loop (fx+ n 1) (ivueparser:parseSaObsValue p))))
     (ivueparser:skip payload len)))

(define (ivueparser:parseSaObsValue buf)
  (let* ((payload (ivueparser:skip buf 6)) ;; 10??
         (physio_id (u8data-u16 (subu8data buf 0 2)))
;;       (state (u8data-u16 (subu8data buf 2 4)))
         (len (u8data-u16 (subu8data buf 4 6)))
         (count (/ len 2))
         (data (let loop ((n 0)(p payload)(l '()))
            (if (= n count) l (loop (+ n 1) 
              (ivueparser:skip p 2) (append l (list
                 (u8data-u16 (subu8data p 0 2)))))))))
    (log-debug (string-append "SaObsValue: wave [" (number->string physio_id) 
                    "] n=" (number->string count) 
             " handle=" (number->string ivueparser:handleid)) 1)
     (ivueparser:storage-data ivueparser:store ivueparser:handleid physio_id data)
      (ivueparser:skip payload len)))

(define (ivueparser:parseAttrScaleSpec buf)
  (let ((payload (ivueparser:skip buf 12))
        (lower_abs (ivueparser:decodef32  (subu8data buf 0 4)))
        (upper_abs (ivueparser:decodef32  (subu8data buf 4 8)))
        (lower_scale (u8data-u16 (subu8data buf 8 10)))
        (upper_scale (u8data-u16 (subu8data buf 10 12))))
;;  (log-debug (format "AttrScaleSpec: ~D: [~D-~D] -> [~F-~F]" ivueparser:handleid
;;        lower_scale upper_scale lower_abs  upper_abs) 1)
     (ivueparser:storage-scale ivueparser:store ivueparser:handleid 
        lower_scale upper_scale lower_abs  upper_abs)
   payload))

;; this sends the 32bit label which uniquely identifies the metrics
;; we have to link this to the handleids
(define (ivueparser:parseAttrIdLabel buf)
  (let ((payload (ivueparser:skip buf 4))
        (label (u8data-u32 (subu8data buf 0 4))))
    (table-set! ivueparser:labellut ivueparser:handleid label)
    payload))

;; this is the bed label, set on admission/discharge
(define (ivueparser:parseBedLabel buf len)
  (let ((location (u8data->u8vector (subu8data buf 0 len))))
    (store-set! ivueparser:store "location" (ivueparser:u8vector->string location) "ivue")
  ))

;; NOT USED
;;(define (ivueparser:parseAttrMetricInfoLabel buf)
;;  (let ((payload (ivueparser:skip buf 4))
;;        (metricinfo (u8data-u32 (subu8data buf 0 4))))
;;    (set! ivueparser:metricinfolabel metricinfo)
;;    payload))

;;(define (ivueparser:parseAttrSamplePeriod buf)
;;  (let ((payload (ivueparser:skip buf 4))
;;        (period (* 8000. 
;;         (u8data-u32 (subu8data buf 0 4)))))
;;  (log-debug (string-append "AttrSamplePeriod period="
;;      (number->string period)) 1)
;;   (ivueparser:storage-period ivueparser:store ivueparser:handleid period)
;;   payload))
    
;;(define (ivueparser:parseAttrSampleArraySpec buf)
;;  (let ((payload (ivueparser:skip buf 6))
;;        (asize (u8data-u16 (subu8data buf 0 2)))
;;        (ssize (u8data-u8 (subu8data buf 2 3)))
;;        (sbits (u8data-u8 (subu8data buf 3 4)))
;;        (flags (u8data-u16 (subu8data buf 4 6))))
;;  (log-debug (string-append "AttrSampleArraySpec: array size="
;;      (number->string asize)) 1)
;;   (ivueparser:storage-size ivueparser:store ivueparser:handleid asize)
;;   payload))

;; main function
(define (ivueparser store data)
  (set! ivueparser:store store)
  (ivueparser:parsemessage data)
)

;; eof
