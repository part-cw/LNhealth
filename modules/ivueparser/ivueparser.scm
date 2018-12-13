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

;; philips monitor trend & waveform parser
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
  "HR" "btbHR" "STi" "STii" "STiii" "STv" "PVC" "RR"
  "NBPsys" "NBPdia" "NBPmean" "PRnbp"
  "ABPmean" "ABPsys" "ABPdia" "PRabp"
  "ARTsys" "ARTdia" "ARTmean" "PRart"
  "PAPmean" "PAPsys" "PAPdia" "PRpap"
  "CVPsys" "CVPdia" "CVPmean" "PRcvp"
  "Aosys" "Aodia" "Aomean" "PRao"
  "RAPsys" "RAPdia" "RAPmean" "PRrap"
  "LAPsys" "LAPdia" "LAPmean" "PRlap"
  "Temp" "Trect" "Tblood" "Tcore" "Tesoph" "Tnaso" "Tamb"
  "SpO2" "PRspo2" "Perf" "sat_o2_freq"
  "SpO2 l" "PRspo2l" "Perf l"
  "SpO2 r" "PRspo2r" "Perf r"
  "SpO2pr" "PRspo2pr"  "PerfPr"
  "SpO2po" "PRspo2po"  "PerfPo"
  "CO2et" "CO2insp" "awRR"
  "BIS" "SQI" "EMG" "SEF"))
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
(define ivue:waveforms_aisys '("AWP" "AWF" "AWV" "AGT" "ISO" "SEV" "DES"))

(define ivue:demographics '("location" "location_connect" "mac"
  "patient_given_name" "patient_middle_name" "patient_family_name" "patient_id" "patient_encounter_id"
  "patient_dob" "patient_height" "patient_weight" "patient_age" "patient_bsa"
  "patient_notes1" "patient_notes2" "patient_sex" "patient_type"  "patient_paced_mode" "patient_id_int"))

;; Public parsing function hook
(define (ivueparser store data)
  (set! ivueparser:store store)
  (store-setnew! store "ivue?" #t "ivue")
  (ivueparser:parseSessionHeader data)
)
;; eof
