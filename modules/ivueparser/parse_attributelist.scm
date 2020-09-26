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

(include "parse_numericvalue.scm")
(include "parse_alert.scm")

;; table to associate handles with labels
(define ivueparser:labellut (make-table init: 0))

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
        (ivueparser:parseAttrString "location_connect" val len))
      ((fx= attribute_id NOM_ATTR_ID_MODEL)
        (ivueparser:parseIdModel val))
      ((fx= attribute_id NOM_ATTR_MODE_OP)
        (ivueparser:parseModeOp val))
      ((fx= attribute_id NOM_ATTR_SYS_ID)
        (ivueparser:parseSysId val))
      ((fx= attribute_id NOM_ATTR_ID_LABEL)
        (ivueparser:parseAttrIdLabel obj_handle val))
      ((fx= attribute_id NOM_ATTR_ID_BED_LABEL)
        (ivueparser:parseAttrString "location" val len))
      ((fx= attribute_id NOM_ATTR_MODE_MSMT)
        (ivueparser:parseMeasureMode val))
      ((fx= attribute_id NOM_ATTR_VMS_MDS_STAT)
        (ivueparser:parseMDSStatus val))
      ((fx= attribute_id NOM_ATTR_MDS_GEN_INFO)
        (ivueparser:parseMdsGenSystemInfo val))
      ((fx= attribute_id NOM_ATTR_SYS_SPECN)
        (ivueparser:parseSystemSpec val))
      ((fx= attribute_id NOM_ATTR_SYS_TYPE)
        (ivueparser:parseSystemType val))
      ;; Numeric Observed Values
      ((fx= attribute_id NOM_ATTR_NU_VAL_OBS)
        (ivueparser:parseNuObsValue obj_handle val))
      ((fx= attribute_id NOM_ATTR_NU_CMPD_VAL_OBS)
        (ivueparser:parseNuObsValueCmp obj_handle val))
      ((fx= attribute_id NOM_ATTR_DISP_RES)
        (ivueparser:parseDispResolution obj_handle val))
      ((fx= attribute_id NOM_ATTR_UNIT_CODE)
        (ivueparser:parseUnitCode obj_handle val))
      ;; Waveform Attributes
      ((fx= attribute_id NOM_ATTR_SA_VAL_OBS)
        (ivueparser:parseSaObsValue obj_handle val))
      ((fx= attribute_id NOM_ATTR_SA_CMPD_VAL_OBS)
        (ivueparser:parseSaObsValueCmp obj_handle val))
      ((fx= attribute_id NOM_ATTR_SCALE_SPECN_I16)
        (ivueparser:parseScaleRangeSpec16 obj_handle val))
      ((fx= attribute_id NOM_ATTR_METRIC_STAT)
        (ivueparser:parseMetricState obj_handle val))
      ((fx= attribute_id NOM_ATTR_COLOR)
        (ivueparser:parseSimpleColourAttribute obj_handle val))
      ((fx= attribute_id NOM_ATTR_GRID_VIS_I16)
        (ivueparser:parseSaVisualGrid16 obj_handle val))
      ((fx= attribute_id NOM_ATTR_TIME_PD_SAMP)
        (ivueparser:parseSamplePeriod obj_handle val))
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
      ((fx= attribute_id NOM_ATTR_VAL_ENUM_OBS)
        (ivueparser:parseEnumObs val))
      ((fx= attribute_id NOM_SAT_O2_TONE_FREQ)
        (ivueparser:parseSatToneFreq val))
      ((fx= attribute_id #x0a1d)
        (ivueparser:parse0a1d val))
      ((fx= attribute_id 62007)
        (ivueparser:log 3 "ivueparser: 62007: " (u8data-u32 (subu8data val 0 4))))
      ((fx= attribute_id #xf383)
        (ivueparser:parseAttributeList obj_handle val))
      ((fx= attribute_id #xf385) ;; This is a string with messages shown on the screen
        (ivueparser:parseAttrString "monitor_msg" val len))
      (else
        (ivueparser:log 1 "ivueparser: unknown attribute: " (number->string attribute_id 16) " [" len "]")
      )
    )
    (u8data-skip val len)
  ))

;; Magic loop thingy for parsing the demographics piece
(define (ivueparser:parse0a1d buf)
  (let ((count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(p (u8data-skip buf 4)))
      (if (fx= n count)
        p
        (loop (fx+ n 1) (ivueparser:parseObservationPoll p))
      )
    )
  ))

;; A generic Attribute String parser
(define (ivueparser:parseAttrString label buf len)
  (let ((str (ivueparser:u8vector->string (u8data->u8vector (subu8data buf 0 len)))))
    (if (fx> (string-length str) 0)
      (store-set! ivueparser:store label str "ivue")
      (store-clear! ivueparser:store label)
    )
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
    (if (and century year month day hour minute second
             (not (fx= (u8data-u8 (subu8data buf 0 1)) 0)))
      (store-set! ivueparser:store label (flo (string->seconds
        (string-append century year month day "-" hour minute second) "%Y%m%d-%H%M%S"))
        "ivue"
      )
      (store-clear! ivueparser:store label)
    )
  ))

;; Patient Attributes
(define (ivueparser:parseSex buf)
  (let* ((sex (u8data-u16 (subu8data buf 0 2)))
         (sexstr (cond
                   ((fx= sex MALE) "Male")
                   ((fx= sex FEMALE) "Female")
                   (else "Unknown"))))
    (store-set! ivueparser:store "patient_sex" sexstr "ivue")
  ))

(define (ivueparser:parsePatMeasure label buf)
  (let ((value (ivueparser:parseFLOATType (subu8data buf 0 4)))
        (m_unit (u8data-u16 (subu8data buf 4 6))))
    (store-set! ivueparser:store label value "ivue")
  ))

(define (ivueparser:parsePatType buf)
  (let* ((type (u8data-u16 (subu8data buf 0 2)))
         (typestr (cond
                   ((fx= type ADULT) "Adult")
                   ((fx= type PEDIATRIC) "Child")
                   ((fx= type NEONATAL) "Neonate")
                   (else "Unspecified"))))
    (store-set! ivueparser:store "patient_type" typestr "ivue")
  ))

(define (ivueparser:parsePacedMode buf)
  (let ((mode (u8data-u16 (subu8data buf 0 2))))
    (store-set! ivueparser:store "patient_paced_mode" mode "ivue")
  ))

(define (ivueparser:parsePatIdInt buf len)
  (let ((id (u8data->u8vector (subu8data buf 0 len))))
    (store-set! ivueparser:store "patient_id_int" id "ivue")
  ))

;; System Type
(define (ivueparser:parseSystemType buf)
  (let ((system_type (ivueparser:parseTYPE (subu8data buf 0 4))))
    (store-set! ivueparser:store "system_type" system_type "ivue")
  ))

;; System Info
(define (ivueparser:parseIdModel buf)
  (let* ((len (u8data-u16 (subu8data buf 0 2)))
         (manufacturer (u8vector->string (u8data->u8vector (subu8data buf 2 (+ 2 len)))))
         (len2 (u8data-u16 (subu8data buf (+ 2 len) (+ 4 len))))
         (model_number (u8vector->string (u8data->u8vector (subu8data buf (+ 4 len) (+ 4 len len2))))))
    (store-set! ivueparser:store "manufacturer" manufacturer "ivue")
    (store-set! ivueparser:store "model_number" model_number "ivue")
  ))

(define (ivueparser:parseAttrIdLabel obj_handle buf)
  (let ((label (u8data-u32 (subu8data buf 0 4))))
    (if (fx> label 0) (table-set! ivueparser:labellut obj_handle label))
  ))

;; New idea based on case state - might need to be tested more fully first
#|
(define (ivueparser:parseModeOp buf)
  (let* ((phase_trend_var_ivue (list "HR" "SpO2" "CO2et"))
         (has-val (map (lambda (var) (> (store-timedref ivueparser:store var 0.) 5)) phase_trend_var_ivue))
         (has-no-val (map (lambda (var) (< (store-timedref ivueparser:store var 0.) 5)) phase_trend_var_ivue))
         (cur-phase (store-ref ivueparser:store "phase" 0)))
    (cond
      ;; Maintenance, all trends are there
      ((and (fx= cur-phase 1) (member #t has-val) (not (member #f has-val)))
        (store-set! ivueparser:store "phase" 2))
      ;; Empty, as they are all false
      ((and (not (fx= cur-phase 0))(not (member #f has-no-val)))
         (let ((params (map car (store-listcat ivueparser:store "ivue")))
               (keep (append ivue:demographics '("location" "CaseEndPending" "operation_mode"))))
           (store-set! ivueparser:store "CaseEndPending" #t "ivue")
           (store-clear! ivueparser:store "CaseStartPending")
           ;; Cleanup old values but keep the stuff to write demographics file
           (for-each (lambda (p) (if (not (member p keep)) (store-clearexpired! ivueparser:store 60 p))) params)
           (store-set! ivueparser:store "phase" 0)
      ))
      ;; Induction: Was empty, now some trends
      ((and (fx= cur-phase 0) (member #t has-val))
         (store-clear! ivueparser:store "CaseEndPending")
         (store-set! ivueparser:store "CaseStartPending" #t "ivue")
         (store-set! ivueparser:store "phase" 1))
      ;; Emergence, used to have all trends, now missing one
      ((and (fx= cur-phase 2) (member #t has-no-val))
       (store-set! ivueparser:store "phase" 3)))
  ))
|#

(define (ivueparser:parseModeOp buf)
  (let ((mode_op (u8data-u16 (subu8data buf 0 2)))
        (old_mode_op (store-ref ivueparser:store "operation_mode" OPMODE_STANDBY)))
    (store-set! ivueparser:store "operation_mode" mode_op "ivue")
    ;; entering standby
    (if (and old_mode_op (fx= (bitwise-and old_mode_op OPMODE_STANDBY) 0)
             (fx= (bitwise-and mode_op OPMODE_STANDBY) OPMODE_STANDBY))
      (let ((params (map car (store-listcat ivueparser:store "ivue")))
            (keep (append ivue:demographics '("location" "CaseEndPending" "operation_mode"))))
        (store-set! ivueparser:store "CaseEndPending" #t "ivue")
        (store-clear! ivueparser:store "CaseStartPending")
        ;; Cleanup old values but keep the stuff to write demographics file
        (for-each (lambda (p) (if (not (member p keep)) (store-clearexpired! ivueparser:store 60 p))) params)
      )
    )
    ;; leaving standby
    (if (and old_mode_op (fx= (bitwise-and old_mode_op OPMODE_STANDBY) OPMODE_STANDBY)
             (fx= (bitwise-and mode_op OPMODE_STANDBY) 0))
      (begin
        (store-clear! ivueparser:store "CaseEndPending")
        (store-set! ivueparser:store "CaseStartPending" #t "ivue")
        ;; Cleanup old demographics
        (for-each (lambda (p) (store-clear! ivueparser:store p)) ivue:demographics)
      )
    )
  ))

(define (ivueparser:parseSysId buf)
  (let* ((len (u8data-u16 (subu8data buf 0 2)))
         (mac (u8data->u8vector (subu8data buf 2 (+ 2 len)))))
    (store-set! ivueparser:store "mac" mac "ivue")
  ))

(define (ivueparser:parseMeasureMode buf)
  (let ((MeasureMode (u8data-u16 (subu8data buf 0 2))))
    (store-set! ivueparser:store "measure_mode" MeasureMode "ivue")
  ))

(define (ivueparser:parseMDSStatus buf)
  (let ((MDSStatus (u8data-u16 (subu8data buf 0 2))))
    (store-set! ivueparser:store "mds_status" MDSStatus "ivue")
  ))

(define (ivueparser:parseMdsGenSystemInfo buf)
  (let ((count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(p (u8data-skip buf 4)))
      (if (or (fx= n count) (fx= (u8data-length p) 0))
        p
        (loop (fx+ n 1) (ivueparser:parseMdsGenSystemInfoEntry p))
      )
    )
  ))

(define (ivueparser:parseMdsGenSystemInfoEntry buf)
  (let ((choice (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (cond
      ((fx= choice MDS_GEN_SYSTEM_INFO_SYSTEM_PULSE_CHOSEN)
        (ivueparser:parseSystemPulseInfo (u8data-skip buf 4))
      )
      (else
        (ivueparser:log 1 "ivueparser: unknown MdsGenSystemInfo choice: " choice)
        (u8data-skip buf (fx+ len 4))
      )
    )
  ))

(define (ivueparser:parseSystemPulseInfo buf)
  (let ((system_pulse (ivueparser:parseManagedObjectId buf))
        (alarm_source (ivueparser:parseManagedObjectId (u8data-skip buf 6))))
    (ivueparser:log 3 "ivueparser: SystemPulseInfo: " system_pulse " " alarm_source)
    (u8data-skip buf 12)
  ))

(define (ivueparser:parseSystemSpec buf)
  (let ((count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(p (u8data-skip buf 4)))
      (if (or (fx= n count) (fx= (u8data-length p) 0))
        p
        (loop (fx+ n 1) (ivueparser:parseSystemSpecEntry p))
      )
    )
  ))

(define (ivueparser:parseSystemSpecEntry buf)
  (let ((component_capab_id (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (ivueparser:parseMdibObjectSupport (u8data-skip buf 4))
    (u8data-skip buf (fx+ len 4))
  ))

(define (ivueparser:parseMdibObjectSupport buf)
  (let ((count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(mos '())(p (u8data-skip buf 4)))
      (if (or (fx= n count) (fx= (u8data-length p) 0))
        (begin
          (store-set! ivueparser:store "MdibObjectSupport" mos "ivue")
          p
        )
        (loop (fx+ n 1) (append mos (ivueparser:parseMdibObjectSupportEntry p)) (u8data-skip p 8))
      )
    )
  ))

(define (ivueparser:parseMdibObjectSupportEntry buf)
  (let ((object_type (ivueparser:parseTYPE buf))
        (max_inst (u8data-u32 (subu8data buf 4 8))))
    (list (append object_type (list max_inst)))
  ))

;; Enum-Ovserved-Value
(define (ivueparser:parseEnumObs buf)
  (let ((physio_id (u8data-u16 (subu8data buf 0 2)))
        (state (u8data-u16 (subu8data buf 2 4)))
        (value (ivueparser:parseEnumVal (u8data-skip buf 4))))
    value
  ))

(define (ivueparser:parseEnumVal buf)
  (let ((choice (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (cond
      ((fx= choice ENUM_OBJ_ID_CHOSEN)
        (let ((enum_obj_id (u8data-u16 (subu8data buf 4 6))))
          enum_obj_id
        ))
      ((fx= choice ENUM_OBJ_ID_VAL_CHOSEN)
        (ivueparser:parseEnumObjIdVal (subu8data buf 4 (fx+ len 4))))
      (else
        (ivueparser:log 1 "ivueparser: unknown Enum choice: " choice)
        #f)
    )
  ))

(define (ivueparser:parseEnumObjIdVal buf)
  (let ((obj_id (u8data-u16 (subu8data buf 0 2)))
        (num_val (ivueparser:parseFLOATType (subu8data buf 2 6)))
        (unit_code (u8data-u16 (subu8data buf 6 8))))
    (list obj_id num_val unit_code)
  ))

;; Private Attributes
(define (ivueparser:parseSatToneFreq buf)
  (let ((freq (flo (u8data-u16 (subu8data buf 0 2)))))
    (store-set! ivueparser:store "sat_o2_freq" (fl/ 1000000. freq) "ivue")
  ))

;; eof
