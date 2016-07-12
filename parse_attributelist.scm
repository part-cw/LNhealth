;; Philips Intellivue Parser
;; Chris Petersen, 2011
;; Matthias Görges, 2016
(include "parse_numericvalue.scm")
(include "parse_alert.scm")

;; Main Attribute List parser
(define (ivueparser:parseAttributeList obj_handle buf)
  (let ((count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(p (u8data-skip buf 4)))
      (if (or (fx= n count) (fx= (u8data-length p) 0))
        p
        (loop (fx+ n 1) (ivueparser:parseAVAType obj_handle p))
      )
    )
  ))

;; Big switch for different Attribute Types
(define (ivueparser:parseAVAType obj_handle buf)
  (let ((attribute_id (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4)))
        (val (u8data-skip buf 4)))
    (cond
      ;; Patient Attibutes
      ((fx= attribute_id NOM_ATTR_PT_NAME_GIVEN)
        (ivueparser:parseAttrString "patient_given_name" val len))
      ((fx= attribute_id NOM_ATTR_PT_NAME_MIDDLE)
        (ivueparser:parseAttrString "patient_middle_name" val len))
      ((fx= attribute_id NOM_ATTR_PT_NAME_FAMILY)
        (ivueparser:parseAttrString "patient_family_name" val len))
      ((fx= attribute_id NOM_ATTR_PT_ID)
        (ivueparser:parseAttrString "patient_id" val len))
      ((fx= attribute_id NOM_ATTR_PT_ENCOUNTER_ID)
        (ivueparser:parseAttrString "patient_encounter_id" val len))
      ((fx= attribute_id NOM_ATTR_PT_SEX)
        (ivueparser:parseSex val))
      ((fx= attribute_id NOM_ATTR_PT_DOB)
        (ivueparser:parseAbsoluteTime "patient_dob" val))
      ((fx= attribute_id NOM_ATTR_PT_HEIGHT)
        (ivueparser:parsePatMeasure "patient_height" val))
      ((fx= attribute_id NOM_ATTR_PT_WEIGHT)
        (ivueparser:parsePatMeasure "patient_weight" val))
      ((fx= attribute_id NOM_ATTR_PT_AGE)
        (ivueparser:parsePatMeasure "patient_age" val))
      ((fx= attribute_id NOM_ATTR_PT_BSA)
        (ivueparser:parsePatMeasure "patient_bsa" val))
      ((fx= attribute_id NOM_ATTR_PT_NOTES1)
        (ivueparser:parseAttrString "patient_notes1" val len))
      ((fx= attribute_id NOM_ATTR_PT_NOTES2)
        (ivueparser:parseAttrString "patient_notes2" val len))
      ((fx= attribute_id NOM_ATTR_PT_TYPE)
        (ivueparser:parsePatType val))
      ((fx= attribute_id NOM_ATTR_PT_PACED_MODE)
        (ivueparser:parsePacedMode val))
      ((fx= attribute_id NOM_ATTR_PT_ID_INT)
        (ivueparser:parsePatIdInt val len))
      ;; System Attibutes
      ((fx= attribute_id 61749) ;; BedLabel - connect Indicator
        (ivueparser:parseAttrString "location" val len))
      ((fx= attribute_id NOM_ATTR_ID_MODEL)
        (ivueparser:parseIdModel val))
      ((fx= attribute_id NOM_ATTR_MODE_OP)
        (ivueparser:parseModeOp val))
      ((fx= attribute_id NOM_ATTR_SYS_ID)
        (ivueparser:parseSysId val))
      ;; Numeric Observed Values
      ((fx= attribute_id NOM_ATTR_NU_VAL_OBS)
        (ivueparser:parseNuObsValue obj_handle val))
      ((fx= attribute_id NOM_ATTR_NU_CMPD_VAL_OBS)
        (ivueparser:parseNuObsValueCmp obj_handle val))
      ;; Timestamps
      ((fx= attribute_id NOM_ATTR_TIME_STAMP_ABS)
        (ivueparser:parseAbsoluteTime "abs_time_stamp" val))
      ((fx= attribute_id NOM_ATTR_TIME_STAMP_REL)
        (ivueparser:parseRelativeTime val))
      ;; Alarms
      ((fx= attribute_id NOM_ATTR_DEV_AL_COND)
        (ivueparser:parseDeviceAlertCondition val))
      ((fx= attribute_id NOM_ATTR_AL_MON_T_AL_LIST)
        (ivueparser:parseDevAlarmList val #f))
      ((fx= attribute_id NOM_ATTR_AL_MON_P_AL_LIST)
        (ivueparser:parseDevAlarmList val #t))
      ;; And everything else
      ((fx= attribute_id NOM_ATTR_ID_BED_LABEL)
        (ivueparser:parseAttrString "location" val len))
      (else
        (ivueparser:log 2 "ivueparser: unknown attribute:" attribute_id "[" len "]")
      )
    )
    (u8data-skip val len)
  ))

;; A generic Attribute String parser
(define (ivueparser:parseAttrString label buf len)
  (let ((str (ivueparser:u8vector->string (u8data->u8vector (subu8data buf 0 len)))))
    (store-set! ivueparser:store label str "ivue")
    (ivueparser:log 1 "ivueparser:" label str)
  ))

;; Timestamps
(define (ivueparser:parseRelativeTime buf)
  (let ((ts (fl/ (flo (u8data-u32 (subu8data buf 0 4))) 8000.))
        (abs_time (store-ref ivueparser:store "abs_time_stamp"))
        (rel_time (store-ref ivueparser:store "rel_time_stamp")))
    (store-set! ivueparser:store "rel_time_stamp" ts "ivue")
    (if (and abs_time rel_time (fl> ts rel_time))
      (store-set! ivueparser:store "ivue_timestamp" (fl+ abs_time ts) "ivue")
    )
    ts
  ))

(define (ivueparser:parseAbsoluteTime label buf)
  (let ((century (ivueparser:parseBCD (u8data-u8 (subu8data buf 0 1))))
        (year (ivueparser:parseBCD (u8data-u8 (subu8data buf 1 2))))
        (month (ivueparser:parseBCD (u8data-u8 (subu8data buf 2 3))))
        (day (ivueparser:parseBCD (u8data-u8 (subu8data buf 3 4))))
        (hour (ivueparser:parseBCD (u8data-u8 (subu8data buf 4 5))))
        (minute (ivueparser:parseBCD (u8data-u8 (subu8data buf 5 6))))
        (second (ivueparser:parseBCD (u8data-u8 (subu8data buf 7 8)))))
    (if century (store-set! ivueparser:store label (flo (string->seconds
      (string-append century year month day "-" hour minute second) "%Y%m%d-%H%M%S"))
      "ivue"
    ))
  ))

;; Patient Attributes
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

;; System Info
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

;; eof
