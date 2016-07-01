;; philips monitor parser

;; this is a separate module in order to reuse the code in monitor+tap plugins
(define ivueparser:debuglevel 1)
(define (ivueparser:log level . x) (if (fx>= ivueparser:debuglevel level) (apply log-system x)))

;; Internal variables
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
          (ivueparser:log 2 "ivueparser: parseframe: malformed frame")
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
        (ivueparser:log 2 "Invalid input frame")
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
       (ivueparser:log 2 "ivueparser: bad input frame crc")
       #f
     )
     (subu8data buf 0 (fx- buflen 2))
    )
))

(define (ivueparser:decodeheader buf)
  (if (or (not (= (u8data-ref buf 0) #x11))
           (not (= (u8data-ref buf 1) #x01)))
    (begin (ivueparser:log 2 "ivueparser: bad input frame header") #f)
      (let ((buflen (u8data-length buf))
            (mylen (fx+ (arithmetic-shift (u8data-ref buf 2) 8)
                (u8data-ref buf 3))))
        (if (not (fx= (fx- buflen 4) mylen))
          (begin
            (ivueparser:log 2 "ivueparser: input frame length mismatch")
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
          (ivueparser:log 1 "ivueparser: connect from monitor") #f)
       ((fx= indicator #x0E) ;; association response
          (ivueparser:log 1 "ivueparser: association response") #f)
       ((fx= indicator #x0C) ;; Refuse
          (set! ivueparser:error #t)
          (ivueparser:log 2 "ivueparser: refuse from monitor") #f)
       ((fx= indicator #x0A) ;; Release response
          (ivueparser:log 2 "ivueparser: release from monitor") #f)
        ((fx= indicator #x19) ;; Association abort
          (set! ivueparser:error #t)
          (ivueparser:log 2 "ivueparser: abort from monitor") #f)
        ((fx= indicator #xE1) ;; Data Export Protocol
           (let ((roapdus (u8data-ref buf 5)))
              (cond
                ((fx= roapdus ROIV_APDU)
                  (ivueparser:log 2 "ivueparser: mds create event" 1)
                  (ivueparser:parsecentralstation buf) #f)
                ((fx= roapdus RORS_APDU)
                  ;;(ivueparser:log 2 "ivueparser: frame single" 1)
                  (set! ivueparser:islinked #f)
                  (ivueparser:parsedataresult buf #f) #f)
                ((fx= roapdus ROLRS_APDU)
                  ;;(ivueparser:log 2 "ivueparser: linked data result" 1)
                  (set! ivueparser:islinked #t)
                  (ivueparser:parsedataresult buf #t) #t)
                ((fx= roapdus ROER_APDU)
                  (set! ivueparser:error #t)
                  (ivueparser:log 2 "ivueparser: error message " (u8data->u8vector buf)) #f)
                (else
                  (set! ivueparser:error #t)
                  (ivueparser:log 2 "ivueparser: data export protocol error") #f))))
        (else
          (set! ivueparser:error #t)
          (ivueparser:log 2 "ivueparser: parsing error") #f)
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
         ((= val #x0d06) 3)
         ((= val #x0c11) 4)
         (else (ivueparser:log 2 (string-append "ignoring type=" (number->string val 16))) 0))))

;; parse a central station data frame
(define (ivueparser:parsecentralstation buf)
  (ivueparser:log 2 (string-append "ivueparser: central station frame length="
                (number->string (u8data-length buf))))
  (let* ((tpe (ivueparser:stationtype buf))
         (res  (cond ((= tpe 1) (ivueparser:parseServerWaveforms buf))
                     ((= tpe 2) (ivueparser:parseServerTrends buf))
                     ((= tpe 3) (ivueparser:parseServerConnectIndInfo buf))
                     ((= tpe 4) (ivueparser:parseServerPatientDemographics buf))
                     (else buf))))
   (if (and (fx> (u8data-length res) 0) (not (fx= (u8data-length res) 34)))
     (ivueparser:log 2 (string-append
        "ivueparser: central station: message with "
         (number->string (u8data-length res))
         " bytes of " (number->string (u8data-length buf))
         " trailing??")))
  #t))

;; server patient demographics attributes
(define (ivueparser:parseServerPatientDemographics buf)
  (if (> (u8data-length buf) 50)
    (let ((payload (ivueparser:skip buf 44)))
      (let loop ((ct (u8data-u16 (subu8data buf 40 42))) (p payload))
        (if (or (fx= ct 0) (fx= (u8data-length p) 0))
          p
          (loop (fx- ct 1) (ivueparser:parseAttributeList p))
        ))
    )
    buf
  ))

;; server connect indication
(define (ivueparser:parseServerConnectIndInfo buf)
  (if (> (u8data-length buf) 36)
    (let ((payload (ivueparser:skip buf 30)))
      (ivueparser:parseAttributeList payload)
    )
    buf
  ))

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
  (ivueparser:log 2 (string-append "parseServerWaveform len=" (number->string (u8data-length buf))))
  (let* ((payload   (ivueparser:skip buf 8))
         (handle_id (u8data-u16 (subu8data buf 0 2))) ;; not sure??
         (physio_id (u8data-u16 (subu8data buf 2 4)))
         (len       (u8data-u16 (subu8data buf 6 8)))
         (count (/ len 2))
         (data (let loop ((n 0)(p payload)(l '()))
            (if (= n count) l (loop (+ n 1)
              (ivueparser:skip p 2) (append l (list
                 (u8data-u16 (subu8data p 0 2)))))))))
    (ivueparser:log 2 (string-append "parseServerWaveform: wave ["
        (number->string physio_id) "] n=" (number->string count)))
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
     (ivueparser:log 2 (string-append "parseServerTrend: count="
       (number->string count) " [ id=" (number->string id) "]") 1)
     (set! ivueparser:handleid id)
     (let loop ((n 0)(p payload))
        (if (fx< n count) (loop (fx+ n 1) (ivueparser:parseAttribute p))))
    (ivueparser:skip payload len)))

;; ----------------------

;; parse the data results - this is where the magic happens!
(define (ivueparser:parsedataresult buf islinked)
  (ivueparser:log 2 "ivueparser: parse begin" 1)
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
      (else (ivueparser:log 2 (string-append
               "ivueparser: unknown message type: "
               (number->string ivueparser:cmd_type)))))))

;; handle an ActionResult
(define (ivueparser:doActionResult buf isextended)
  (ivueparser:log 2 "ActionResult")
  (let* ((step4 (ivueparser:parseActionResult buf))
         (step5 (ivueparser:parsePollMdibDataReply step4 isextended))
         (step6 (if (fx> (u8data-length step5) 2) ;; skip empty lists
                  (ivueparser:parsePollInfoList step5)
                  (u8data)
         )))
   (if (fx= (u8data-length step5) 4)
     (set! ivueparser:islast #t) (set! ivueparser:islast #f))
   (if (> (u8data-length step6) 0)
     (ivueparser:log 2 (string-append
        "ivueparser: ActionResult: message with "
          (number->string (u8data-length step6))
         " bytes of " (number->string (u8data-length buf))
         " trailing??")))
  #t))

;; handle a GetResult
;; this would be a priority list request response
(define (ivueparser:doGetResult buf)
  (ivueparser:log 2 "GetResult" 1)
  (let* ((step4 (ivueparser:parseGetResult buf))
         (step5 (ivueparser:parseAttributeList step4)))
   (if (> (u8data-length step5) 0)
     (ivueparser:log 2 (string-append
        "ivueparser: GetResult: message with "
          (number->string (u8data-length step5))
         " bytes of " (number->string (u8data-length buf))
         " trailing??")))
  #t))

;; handle a SetResult
(define (ivueparser:doSetResult buf)
  (ivueparser:log 2 "SetResult" 1)
  (let* ((step4 (ivueparser:parseSetResult buf))
        (step5 (ivueparser:parseAttributeList step4)))
   (if (> (u8data-length step5) 0)
     (ivueparser:log 2 (string-append
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

(define (ivueparser:parseActionResult buf)
  (let ((action_type (u8data-u16 (subu8data buf 6 8)))
        (len (u8data-u16 (subu8data buf 8 10))))
    (if (or (fx= action_type NOM_ACT_POLL_MDIB_DATA)
            (fx= action_type NOM_ACT_POLL_MDIB_DATA_EXT))
      (ivueparser:skip buf 10)
      (begin ;;undocumented types
        (ivueparser:log 2 "ivueparser: unknown action_type:" action_type)
        (if (fx= action_type NOM_ACT_DISCHARGE)
          (store-set! ivueparser:store "CaseEndPending" #t "ivue")
        )
        (u8vector->u8data (u8vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      )
    )
  ))
(define (ivueparser:parseGetResult buf) (ivueparser:skip buf 2))
(define (ivueparser:parseSetResult buf) (ivueparser:skip buf 2))
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
  (let* ((payload (ivueparser:skip buf 8))
         ;;(attribute_id (u8data-u16 (subu8data buf 0 2)))
         (len1 (u8data-u16 (subu8data buf 2 4)))
         (count (u8data-u16 (subu8data buf 4 6)))
         (len2 (u8data-u16 (subu8data buf 6 8)))
         (len (if (fx= len2 0) (fx- len1 4) len2)))
    ;;(ivueparser:log 0 (string-append "parseAttributeList count="
    ;;  (number->string count) " [" (number->string len) "]"))
    (let loop ((n 0)(p payload))
      (if (fx< n count)
        (loop (fx+ n 1) (ivueparser:parseAttribute p))))
    (ivueparser:skip payload len)))

(define (ivueparser:parseAttribute buf)
 ;; (ivueparser:log 0 "parseAttribute" 1)
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
       (ivueparser:parseAttrString "location" payload len))
    ((fx= id 61749) ;; BedLabel - connect Indicator
       (ivueparser:parseAttrString "location" payload len))
    ((fx= id NOM_ATTR_ID_MODEL)
       (ivueparser:parseIdModel payload))
    ((fx= id NOM_ATTR_MODE_OP)
       (ivueparser:parseModeOp payload))
    ((fx= id NOM_ATTR_SYS_ID)
       (ivueparser:parseSysId payload))
    ((fx= id NOM_ATTR_VAL_ENUM_OBS)
       (ivueparser:parseEnumObs payload))
    ((fx= id NOM_ATTR_PT_NAME_GIVEN)
       (ivueparser:parseAttrString "patient_given_name" payload len))
    ((fx= id NOM_ATTR_PT_NAME_MIDDLE)
       (ivueparser:parseAttrString "patient_middle_name" payload len))
    ((fx= id NOM_ATTR_PT_NAME_FAMILY)
       (ivueparser:parseAttrString "patient_family_name" payload len))
    ((fx= id NOM_ATTR_PT_ID)
       (ivueparser:parseAttrString "patient_id" payload len))
    ((fx= id NOM_ATTR_PT_ENCOUNTER_ID)
       (ivueparser:parseAttrString "patient_encounter_id" payload len))
    ((fx= id NOM_ATTR_PT_SEX)
       (ivueparser:parseSex payload))
    ((fx= id NOM_ATTR_PT_DOB)
       (ivueparser:parseAbsoluteTimeStamp "patient_dob" payload))
    ((fx= id NOM_ATTR_PT_HEIGHT)
       (ivueparser:parsePatMeasure "patient_height" payload))
    ((fx= id NOM_ATTR_PT_WEIGHT)
       (ivueparser:parsePatMeasure "patient_height" payload))
    ((fx= id NOM_ATTR_PT_AGE)
       (ivueparser:parsePatMeasure "patient_age" payload))
    ((fx= id NOM_ATTR_PT_BSA)
       (ivueparser:parsePatMeasure "patient_bsa" payload))
    ((fx= id NOM_ATTR_PT_NOTES1)
       (ivueparser:parseAttrString "patient_notes1" payload len))
    ((fx= id NOM_ATTR_PT_NOTES2)
       (ivueparser:parseAttrString "patient_notes2" payload len))
    ((fx= id NOM_ATTR_PT_TYPE)
       (ivueparser:parsePatType payload))
    ((fx= id NOM_ATTR_PT_PACED_MODE)
       (ivueparser:parsePacedMode payload))
    ((fx= id NOM_ATTR_PT_ID_INT)
       (ivueparser:parsePatIdInt payload len))
    ((fx= id NOM_ATTR_TIME_STAMP_ABS) ;; Absolute Timestamp (from boot)
       (ivueparser:parseAbsoluteTimeStamp "abs_time_stamp" payload))
    ((fx= id NOM_ATTR_TIME_STAMP_REL) ;; Relative Timestamp
       (ivueparser:parseRelativeTimeStamp payload))
    ((fx= id NOM_SAT_O2_TONE_FREQ) ;; SpO2 freq.
       (ivueparser:parseSatToneFreq payload))
    ((fx= id NOM_ATTR_DEV_AL_COND) ;; Number of alarms
       (ivueparser:parseDeviceAlertCondition payload))
    ((fx= id NOM_ATTR_AL_MON_T_AL_LIST) ;; T-Alarm List
       (ivueparser:parseDevAlarmList payload #f))
    ((fx= id NOM_ATTR_AL_MON_P_AL_LIST) ;; P-Alarm List
       (ivueparser:parseDevAlarmList payload #t))
    (else (ivueparser:log 2 "ivueparser: unknown attribute " id " [" len "]"))
    )
   (ivueparser:skip payload len)))

(define (ivueparser:parseDevAlarmList buf palarm?)
  (let ((payload (ivueparser:skip buf 4))
        (count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let* ((al_prefix (string-append (if palarm? "p" "t") "_alarm"))
           (al_lst (string-append al_prefix "_lst")))
      (store-clear! ivueparser:store (store-ref ivueparser:store al_lst))
      (store-set! ivueparser:store al_lst '())
      (let loop ((n 0) (p payload))
        (if (fx< n count)
          (loop (fx+ n 1) (ivueparser:parseDevAlarmEntry p n al_prefix))
        )
      )
    )
  ))

(define (ivueparser:parseDevAlarmEntry buf al_ct prefix)
  (let ((al_source (u8data-u16 (subu8data buf 0 2)))
        (al_code (u8data-u16 (subu8data buf 2 4)))
        (al_type (u8data-u16 (subu8data buf 4 6)))
        (al_state (u8data-u16 (subu8data buf 6 8)))
        (alert_info_id (u8data-u16 (subu8data buf 14 16)))
        (len (u8data-u16 (subu8data buf 16 18))))
    (let* ((name (table-ref ivueparser:phystable1 al_source "???"))
           (priostr (if (= al_type LOW_PRI_P_AL) "*" (if (= al_type MED_PRI_P_AL) "**"
                      (if (= al_type HI_PRI_P_AL) "***" ""))))
           (msg0 (car (table-ref ivueparser:alarmtable al_code '(""))))
           (msg (string-replace-substring msg0 "XXXXXX" name))
           (al_lst_name (string-append prefix "_lst"))
           (al_lst (store-ref ivueparser:store al_lst_name '()))
           (al_ct_name (string-append prefix (number->string al_ct))))
      (store-set! ivueparser:store al_ct_name msg "ivue")
      (store-set! ivueparser:store al_lst_name (append al_lst (list al_ct_name)))
      ;; New alarms
      (if (and (fx= al_state 8) (fx> (string-length msg) 0))
        (store-event-add ivueparser:store 0 (store-ref ivueparser:store "location" ivueparser:store) msg))
      ;; Parse alarm internals
      (if (= alert_info_id STR_ALMON_INFO)
        (ivueparser:parseStrAlMonInfo (subu8data buf 18 (fx+ len 18)) al_ct_name))
      (if (= alert_info_id GEN_ALMON_INFO)
        (ivueparser:parseAlMonGenInfo (subu8data buf 18 (fx+ len 18)) al_ct_name))
    )
    (ivueparser:skip buf (fx+ len 18))
  ))

(define (ivueparser:parseStrAlMonInfo buf al_no)
  (ivueparser:parseAlMonGenInfo buf al_no)
)

(define (ivueparser:parseAlMonGenInfo buf al_ct_name)
  (let ((al_text (u8data-u32 (subu8data buf 2 6)))
        (priority (u8data-u16 (subu8data buf 6 8)))
        (flags (u8data-u16 (subu8data buf 8 10))))
    (store-set! ivueparser:store (string-append al_ct_name "_prio") priority)
    (ivueparser:skip buf 10)
  ))

(define (ivueparser:parseDeviceAlertCondition buf)
  (let ((alertstate (u8data-u16 (subu8data buf 0 2)))
;;        (al_stat_chg_cnt (u8data-u16 (subu8data buf 2 4)))
        (max_p_alarm (u8data-u16 (subu8data buf 4 6)))
        (max_t_alarm (u8data-u16 (subu8data buf 6 8)))
        (max_aud_alarm (u8data-u16 (subu8data buf 8 10))))
    (store-set! ivueparser:store "max_p_alarm" max_p_alarm "ivue")
    (store-set! ivueparser:store "max_t_alarm" max_t_alarm "ivue")
    (store-set! ivueparser:store "max_aud_alarm" max_aud_alarm "ivue")
  ))

(define (ivueparser:parseSatToneFreq buf)
  (let ((pitch (flo (u8data-u16 (subu8data buf 0 2)))))
    (store-set! ivueparser:store "sat_o2_freq" (fl/ 1000000. pitch) "ivue")
  ))

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
      (store-set! ivueparser:store "ivue_timestamp" (+ abs_time ts) "ivue")
    )
  ))

(define (ivueparser:parseAbsoluteTimeStamp label buf)
  (let ((century (ivueparser:decodebcd (u8data-u8 (subu8data buf 0 1))))
        (year (ivueparser:decodebcd (u8data-u8 (subu8data buf 1 2))))
        (month (ivueparser:decodebcd (u8data-u8 (subu8data buf 2 3))))
        (day (ivueparser:decodebcd (u8data-u8 (subu8data buf 3 4))))
        (hour (ivueparser:decodebcd (u8data-u8 (subu8data buf 4 5))))
        (minute (ivueparser:decodebcd (u8data-u8 (subu8data buf 5 6))))
        (second (ivueparser:decodebcd (u8data-u8 (subu8data buf 7 8)))))
    (if century (store-set! ivueparser:store label
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
        (value (ivueparser:decodef32 (subu8data buf 6 10)))
        (label (table-ref ivueparser:labellut ivueparser:handleid)))
;;  (if (not (or (= value 8388607.) (> (bitwise-and state #xff00) 0))) ;; ignore invalid data
    (if (not (or (= value 8388607.) (> (bitwise-and state #xf800) 0))) ;; ignore invalid data but allow demo
      (ivueparser:setphys! ivueparser:store physio_id label value)
      (let ((name (ivueparser:findphys physio_id label)))
        (if name (store-clear! ivueparser:store name))
      )
    )
    payload
  ))

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
    (ivueparser:log 0 (string-append "SaObsValue: wave [" (number->string physio_id)
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
;;  (ivueparser:log 0 (format "AttrScaleSpec: ~D: [~D-~D] -> [~F-~F]" ivueparser:handleid
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

;; A generic Attribute String parser
(define (ivueparser:parseAttrString label buf len)
  (let ((str (ivueparser:u8vector->string (u8data->u8vector (subu8data buf 0 len)))))
    (store-set! ivueparser:store label str "ivue")
    (ivueparser:log 1 "ivueparser:" label str)
  ))

(define (ivueparser:parseIdModel buf)
  (let* ((len (u8data-u16 (subu8data buf 0 2)))
         (manufacturer (u8vector->string (u8data->u8vector (subu8data buf 2 (+ 2 len)))))
         (len2 (u8data-u16 (subu8data buf (+ 2 len) (+ 4 len))))
         (model_number (u8vector->string (u8data->u8vector (subu8data buf (+ 4 len) (+ 4 len len2))))))
    (store-set! ivueparser:store "manufacturer" manufacturer "ivue")
    (store-set! ivueparser:store "model_number" model_number "ivue")
    (ivueparser:log 1 "ivueparser: manufacturer" manufacturer)
    (ivueparser:log 1 "ivueparser: model_number" model_number)
  ))

(define (ivueparser:parseModeOp buf)
  (let ((mode_op (u8data-u16 (subu8data buf 0 2))))
    (store-set! ivueparser:store "operation_mode" mode_op "ivue")
    (ivueparser:log 1 "ivueparser: operation_mode" mode_op)
  ))

(define (ivueparser:parseSysId buf)
  (let* ((len (u8data-u16 (subu8data buf 0 2)))
         (mac (u8data->u8vector (subu8data buf 2 (+ 2 len)))))
    (store-set! ivueparser:store "mac" mac "ivue")
    (ivueparser:log 1 "ivueparser: mac" mac)
  ))

(define (ivueparser:parseEnumObs buf)
  #f
)

(define (ivueparser:parseSex buf)
  (let* ((sex (u8data-u16 (subu8data buf 0 2)))
         (sexstr (cond
                   ((fx= sex MALE) "Male")
                   ((fx= sex FEMALE) "Female")
                   (else "Unknown"))))
    (store-set! ivueparser:store "patient_sex" sexstr "ivue")
    (ivueparser:log 1 "ivueparser: patient_sex" sexstr)
  ))

(define (ivueparser:parsePatMeasure label buf)
  (let ((value (ivueparser:decodef32 (subu8data buf 0 4)))
        (m_unit (u8data-u16 (subu8data buf 4 6))))
    (store-set! ivueparser:store label value "ivue")
    (ivueparser:log 1 "ivueparser:" label value)
  ))

(define (ivueparser:parsePatType buf)
  (let* ((type (u8data-u16 (subu8data buf 0 2)))
         (typestr (cond
                   ((fx= type ADULT) "Adult")
                   ((fx= type PEDIATRIC) "Child")
                   ((fx= type NEONATAL) "Neonate")
                   (else "Unspecified"))))
    (store-set! ivueparser:store "patient_type" typestr "ivue")
    (ivueparser:log 1 "ivueparser: patient_type" typestr)
  ))

(define (ivueparser:parsePacedMode buf)
  (let ((mode (u8data-u16 (subu8data buf 0 2))))
    (store-set! ivueparser:store "patient_paced_mode" mode "ivue")
    (ivueparser:log 1 "ivueparser:" "patient_paced_mode" mode)
  ))

(define (ivueparser:parsePatIdInt buf len)
  (let ((id (u8data->u8vector (subu8data buf 0 len))))
    (store-set! ivueparser:store "patient_id_int" id "ivue")
    (ivueparser:log 1 "ivueparser:" "patient_id_int" id)
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
;;  (ivueparser:log 0 (string-append "AttrSamplePeriod period="
;;      (number->string period)) 1)
;;   (ivueparser:storage-period ivueparser:store ivueparser:handleid period)
;;   payload))

;;(define (ivueparser:parseAttrSampleArraySpec buf)
;;  (let ((payload (ivueparser:skip buf 6))
;;        (asize (u8data-u16 (subu8data buf 0 2)))
;;        (ssize (u8data-u8 (subu8data buf 2 3)))
;;        (sbits (u8data-u8 (subu8data buf 3 4)))
;;        (flags (u8data-u16 (subu8data buf 4 6))))
;;  (ivueparser:log 0 (string-append "AttrSampleArraySpec: array size="
;;      (number->string asize)) 1)
;;   (ivueparser:storage-size ivueparser:store ivueparser:handleid asize)
;;   payload))

;; main function
(define (ivueparser store data)
  (set! ivueparser:store store)
  (ivueparser:parsemessage data)
)

;; eof
