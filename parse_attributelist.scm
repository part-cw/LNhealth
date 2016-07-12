;; Philips Intellivue Parser
;; Chris Petersen, 2011
;; Matthias Görges, 2016

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
        (ivueparser:parseAbsoluteTimeStamp "patient_dob" val))
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
      ;; And everything else
      ((fx= attribute_id NOM_ATTR_ID_BED_LABEL)
        (ivueparser:parseAttrString "location" val len))
      (else
        (ivueparser:log 2 "ivueparser: unknown attribute " attribute_id " [" len "]")
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
