;; philips monitor trend & waveform parser
;; Christian Leth Petersen 2009/2010
(include "constants.scm")
(include "crc.scm")
(include "lookup.scm")
(include "ro_header.scm")
(include "parse_frame.scm")

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
  "HR" "STi" "STii" "STiii" "PVC" "RR"
  "ABPmean" "ABPsys" "ABPdia" "PRabp"
  "PAPmean" "PAPsys" "PAPdia" "PRpap"
  "nibp_sys" "nibp_dia" "nibp_mean" "nibp_hr"
  "Temp"
  "SpO2" "PR(SpO2)" "Perf" "sat_o2_freq"
  "SpO2l" "PR(SpO2l)" "Perfl"
  "CO2e" "CO2imin" "awRR"))
(define ivue:waveform_basic '(
  "II" "V1" "aVR" "ABP" "PAP" "Pleth" "PLETHl" "CO2aw"))

(define ivue:physdatavalues_aisys '(
  "RRaw" "SpRR"
  "InsTi" "ExpTi" "I:E"
  "MnAwP" "PIP" "Pplat" "PEEP" "tPEEP"
  "TVexp" "TVin" "MVexp" "MVin" "SpMV"
  "etAGT" "inAGT" "etHAL" "inHAL" "etISO" "inISO" "etENF" "inENF" "etSEV" "inSEV" "etDES" "inDES" "MAC"
  "AccHAL" "AccISO" "AccENF" "AccSEV" "AccDES"  "AccN2O" "AccO2" "AccAIR"
  "etCO2" "imCO2" "etO2" "inO2" "ΔO2" "fgO2" "fgAir"
  "etN2O" "inN2O" "fgN2O"
  "Cdyn" "Rdyn" "VCO2" "VO2" "VO2kg"
  "sTV" "sRRaw" "sIE" "sPltTi" "sPEEP" "sPmax" "sPin" "sfgO2"
  "sΔPEEP" "sAADel" "sTrgFl" "sTrgLv" "sEndFl" "sInsTi" "sSIMV" "sPSVbd" "sfgFl" "sAGT"))
(define ivue:waveforms_aisys '("AWP" "AWF" "AWV" "AGT" "HAL" "ISO" "ENF" "SEV" "DES" "CO2"))

;; eof
