;; philips monitor trend & waveform parser
;; Christian Leth Petersen 2009/2010
(include "constants.scm")
(include "crc.scm")
(include "lookup.scm")
(include "parse_frame.scm")
(include "parse_session.scm")

;; Add the code needed for serial-based ivue communication
(include "ivue.scm")

;; Set the debug level
(define (ivueparser-set-debuglevel! level)
  (set! ivueparser:debuglevel level)
)
(define ivueparser:debuglevel 0)
(define (ivueparser:log level . x) (if (fx>= ivueparser:debuglevel level) (apply log-system x)))

;; Some variable definitions for data export
(define ivue:physdatavalues_basic '(
  "ivue_timestamp"
  "HR" "btbHR" "STi" "STii" "STiii" "PVC" "RR"
  "NBPsys" "NBPdia" "NBPmean" "PRnbp"
  "ABPmean" "ABPsys" "ABPdia" "PRabp"
  "ARTsys" "ARTdia" "ARTmean" "PRart"
  "PAPmean" "PAPsys" "PAPdia" "PRpap"
  "CVPsys" "CVPdia" "CVPmean" "PRcvp"
  "Temp" "Trect" "Tblood" "Tcore" "Tesoph" "Tnaso" "Tamb"
  "SpO2" "PRspo2" "Perf" "sat_o2_freq"
  "SpO2 l" "PRspo2l" "Perf l"
  "CO2et" "CO2insp" "awRR"
  "BIS" "SQI" "EMG"))
(define ivue:waveform_basic '(
  "I" "II" "III" "aVR" "aVL" "AVF" "V" "ABP" "ART" "PAP" "CVP" "Pleth" "PLETHl" "CO2" "EEG L"))

(define ivue:physdatavalues_aisys '(
  "RRaw" "SpRR"
  "InsTi" "ExpTi" "I:Ei" "I:Ee"
  "MnAwP" "PIP" "Pplat" "PEEP" "tPEEP"
  "TVexp" "TVin" "MVexp" "MVin" "SpMV"
  "AGTet" "AGTinsp" "ISOet" "ISOinsp" "SEVet" "SEVinsp" "DESet" "DESinsp" "MAC"
  "AccISO" "AccSEV" "AccDES" "AccN2O" "AccO2" "AccAir"
  "CO2et" "CO2insp" "O2et" "O2insp" "ΔO2" "fgO2" "fgAir"
  "N2Oet" "N2Oinsp" "fgN2O"
  "Cdyn" "Rdyn" "VCO2" "VO2" "VO2kg"
  "sTV" "sRRaw" "sIE" "sPltTi" "sPEEP" "sPmax" "sPin" "sfgO2"
  "sΔPEEP" "sAADel" "sTrgFl" "sTrgLv" "sEndFl" "sInsTi" "sSIMV" "sPSVbd" "sfgFl" "sAGT"))
(define ivue:waveforms_aisys '("AWP" "AWF" "AWV" "AGT" "ISO" "SEV" "DES" "CO2"))

(define ivue:demographics '(
  "patient_given_name" "patient_middle_name" "patient_family_name" "patient_id" "patient_encounter_id"
  "patient_dob" "patient_height" "patient_weight" "patient_age" "patient_bsa"
  "patient_notes1" "patient_notes2" "patient_sex" "patient_type"  "patient_paced_mode" "patient_id_int"))

;; Public parsing function hook
(define (ivueparser store data)
  (set! ivueparser:store store)
  (ivueparser:parseSessionHeader data)
)
;; eof
