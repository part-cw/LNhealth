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

;; datex s5 parser
;; Add the code needed for serial-based s5 communication
(include "s5.scm")
(include "s5parser-tests.scm")

;; this is a separate module in order to reuse the code in monitor+tap plugins
(define s5parser:debuglevel 0)
(define (s5parser:log level . x) (if (fx>= s5parser:debuglevel level) (apply log-system x)))
(define s5parser:fromfile #f)

(define (s5parser-setfromfile! v)
  (set! s5parser:fromfile v))

;; ----------------
;; group header

(define s5parser:group_active? #f)
(define s5parser:group_label #f)    ;; Introduced so I can look at Agent type, P1-6 sources etc.
(define (s5parser:group_hdr buf)
  (let ((skip (u8data-skip buf 6))
        (status_bits (u8data-le-u32 (subu8data buf 0 4)))
        (label_info (u8data-le-u16 (subu8data buf 4 6))))
   (set! s5parser:group_active? (fx= (bitwise-and #x3 status_bits) 3))
   (set! s5parser:group_label label_info)
   (set! s5parser:status_bits status_bits)
   skip))
(define (s5parser:validate val scale) (if (fx< val -32000) #f (/ val scale)))

;; 20101007: start using data category "s5"
(define (s5parser:settrend! store name value . scale0)
  (let* ((scale (if (fx= (length scale0) 1) (car scale0) 1.))
         (val (s5parser:validate value scale)))
    (if s5parser:group_active?
      (store-set! store name val "s5")
      (store-clear! store name)
    )
  ))

;; ----------------
;; trends

;; basic ----------------
(define (s5parser:hr_getsource v)
  (cond ((= v 1) "ECG1")
	((and (>= v 2) (<= v 5)) (string-append "BP" (number->string (- v 1))))
	((= v 6) "PLETH")
	((or (= v 7) (= v 8)) (string-append "BP" (number->string (- v 2))))
	((or (= v 10) (= v 11)) (string-append "BP" (number->string (- v 3))))
  ((= v 12) "PLETH")
	(else "UNKNOWN")))

(define (s5parser:ecg_getlabel l)
  (cond ((= l 1) "I")
        ((= l 2) "II")
        ((= l 3) "III")
        ((= l 4) "AVR")
        ((= l 5) "AVL")
        ((= l 6) "AVF")
        ((= l 7) "V")
        (else "NOT SELECTED")
   ))

(define (s5parser:ecg_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (hr (u8data-le-s16 (subu8data step1 0 2)))
         (st1 (u8data-le-s16 (subu8data step1 2 4)))
         (st2 (u8data-le-s16 (subu8data step1 4 6)))
         (st3 (u8data-le-s16 (subu8data step1 6 8)))
         (imp_rr (u8data-le-s16 (subu8data step1 8 10))))
    (s5parser:settrend! s "hr" hr 	1.)
    (s5parser:settrend! s "HR" hr 	1.)
    ;; HR Source is bits 3-6
    (store-set! s "hr_source" (s5parser:hr_getsource (bitwise-and (arithmetic-shift s5parser:status_bits -3) 15)) "s5")
    ;; ECG Labels are bits 0-11
    (store-set! s "ecg3_label" (s5parser:ecg_getlabel (bitwise-and (arithmetic-shift s5parser:group_label 0) 15)) "s5")
    (store-set! s "ecg2_label" (s5parser:ecg_getlabel (bitwise-and (arithmetic-shift s5parser:group_label -4) 15)) "s5")
    (store-set! s "ecg1_label" (s5parser:ecg_getlabel (bitwise-and (arithmetic-shift s5parser:group_label -8) 15)) "s5")
    (s5parser:settrend! s "st1"     st1 	100.)
    (s5parser:settrend! s "st2"     st2 	100.)
    (s5parser:settrend! s "st3"     st3 	100.)
    (s5parser:settrend! s "imp_rr"      imp_rr 	1.)
    (u8data-skip step1 10)))

(define (s5parser:p_getname l)
  (cond ((= l 1) "ART")
	((= l 2) "CVP")
	((= l 3) "PA")
	((= l 4) "RAP")
	((= l 5) "RVP")
	((= l 6) "LAP")
	((= l 7) "ICP")
	((= l 8) "ABP")
	((= l 9) "P1")
	((= l 10) "P2")
	((= l 11) "P3")
	((= l 12) "P4")
	((= l 13) "P5")
	((= l 14) "P6")
	(else "NOT DEFINED")))

(define (s5parser:p_group s buf idx)
  (let* ((step1 (s5parser:group_hdr buf))
         (sys (u8data-le-s16 (subu8data step1 0 2)))
         (dia (u8data-le-s16 (subu8data step1 2 4)))
         (mean (u8data-le-s16 (subu8data step1 4 6)))
         (hr  (u8data-le-s16 (subu8data step1 6 8))))
   (s5parser:settrend! s (string-append "p" idx "_sys") sys 100.)
   (s5parser:settrend! s (string-append "p" idx "_dia") dia 100.)
   (s5parser:settrend! s (string-append "p" idx "_mean") mean 100.)
   (s5parser:settrend! s (string-append "p" idx "_hr") hr)
   (store-set! s (string-append "p" idx "_name") (s5parser:p_getname s5parser:group_label) "s5")
   (u8data-skip step1 8)))

(define (s5parser:nibp_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (sys (u8data-le-s16 (subu8data step1 0 2)))
         (dia (u8data-le-s16 (subu8data step1 2 4)))
         (mean (u8data-le-s16 (subu8data step1 4 6)))
         (hr  (u8data-le-s16 (subu8data step1 6 8))))
    (if (fx= (bitwise-and s5parser:group_label 32) 32) ;; Bit 5 of LABEL field is Measuring
      (store-set! s "nibp_timestamp" (store-ref s "timestamp") "s5")
    )
   (s5parser:settrend! s "nibp_mean" mean 100.)
   (s5parser:settrend! s "nibp_hr" hr)

;;   (s5parser:settrend! s "nibp_sys" sys 100.)
;;   (s5parser:settrend! s "nibp_dia" dia 100.)
   ;; the S5 keeps outputting stale BP values, don't update if same
   ;; this will let us trigger an alarm on stale numbers later (used in iControl)
   (let ((oldsys (store-ref s "nibp_sys" #f))
         (olddia (store-ref s "nibp_dia" #f))
         (newsys (s5parser:validate sys 100.))
         (newdia (s5parser:validate dia 100.)))
     (if (or (not oldsys) (and newsys oldsys (not (= oldsys newsys))))
       (s5parser:settrend! s "nibp_sys" sys 100.))
     (if (or (not olddia) (and newdia olddia (not (= olddia newdia))))
       (s5parser:settrend! s "nibp_dia" dia 100.))
   )

   (u8data-skip step1 8)))

(define (s5parser:t_getname l)
  (cond ((= l 1) "ESO")
	((= l 2) "NASO")
	((= l 3) "TYMP")
	((= l 4) "RECT")
	((= l 5) "BLAD")
	((= l 6) "AXIL")
	((= l 7) "SKIN")
	((= l 8) "AIRW")
	((= l 9) "ROOM")
	((= l 10) "MYO")
	((= l 11) "T1")
	((= l 12) "T2")
	((= l 13) "T3")
	((= l 14) "T4")
	((= l 15) "CORE")
	((= l 16) "SURF")
	(else "NOT USED")))

(define (s5parser:t_group s buf idx)
  (let* ((step1 (s5parser:group_hdr buf))
         (temp  (u8data-le-s16 (subu8data step1 0 2))))
   (s5parser:settrend! s (string-append "temp" idx) temp 100.)
   (store-set! s (string-append "temp" idx "_name") (s5parser:t_getname s5parser:group_label) "s5")
   (u8data-skip step1 2)))

;; ignore ir_amp and svo2
(define (s5parser:spo2_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (spo2  (u8data-le-s16 (subu8data step1 0 2)))
         (pr (u8data-le-s16 (subu8data step1 2 4)))
         (ir_amp (u8data-le-s16 (subu8data step1 4 6)))
         (svo2  (u8data-le-s16 (subu8data step1 6 8))))
   (s5parser:settrend! s "spo2" spo2 100.)
   (s5parser:settrend! s "pr" pr)
   (s5parser:settrend! s "ir_amp" ir_amp)
   (s5parser:settrend! s "s_o2" svo2 100.)
   (u8data-skip step1 8)))

(define (s5parser:co2_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (et (u8data-le-s16 (subu8data step1 0 2)))
         (fi (u8data-le-s16 (subu8data step1 2 4)))
         (rr (u8data-le-s16 (subu8data step1 4 6)))
         (amb_press (u8data-le-s16 (subu8data step1 6 8))))
   (s5parser:settrend! s "co2_et"  et 100.)
   (s5parser:settrend! s "co2_fi"  fi 100.)
   (s5parser:settrend! s "co2_rr" rr)
   (s5parser:settrend! s "co2_amb"  amb_press 10.)
   (u8data-skip step1 8)))

(define (s5parser:o2_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (et (u8data-le-s16 (subu8data step1 0 2)))
         (fi (u8data-le-s16 (subu8data step1 2 4))))
   (s5parser:settrend! s "o2_et"  et 100.)
   (s5parser:settrend! s "o2_fi"  fi 100.)
   (u8data-skip step1 4)))

(define (s5parser:n2o_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (et (u8data-le-s16 (subu8data step1 0 2)))
         (fi (u8data-le-s16 (subu8data step1 2 4))))
   (s5parser:settrend! s "n2o_et" et 100.)
   (s5parser:settrend! s "n2o_fi" fi 100.)
   (u8data-skip step1 4)))

;;Introduced to parse agent type
(define (s5parser:aa_getname l)
  (cond ((= l 1) "NONE")
	((= l 2) "HAL")
	((= l 3) "ENF")
	((= l 4) "ISO")
	((= l 5) "DES")
	((= l 6) "SEV")
	(else "UNKNOWN")))

(define (s5parser:aa_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (et (u8data-le-s16 (subu8data step1 0 2)))
         (fi (u8data-le-s16 (subu8data step1 2 4)))
         (mac_sum (u8data-le-s16 (subu8data step1 4 6))))
   (s5parser:settrend! s "aa_et"  et 100.)
   (s5parser:settrend! s "aa_fi"  fi 100.)
   (s5parser:settrend! s "aa_mac" mac_sum 100.)
   (store-set! s "aa_name" (s5parser:aa_getname s5parser:group_label) "s5")
   (u8data-skip step1 6)))

(define (s5parser:flow_vol_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (rr (u8data-le-s16 (subu8data step1 0 2)))
         (ppeak (u8data-le-s16 (subu8data step1 2 4)))
         (peep (u8data-le-s16 (subu8data step1 4 6)))
         (pplat (u8data-le-s16 (subu8data step1 6 8)))
         (tv_insp (u8data-le-s16 (subu8data step1 8 10)))
         (tv_exp (u8data-le-s16 (subu8data step1 10 12)))
         (compliance (u8data-le-s16 (subu8data step1 12 14)))
         (mv_exp (u8data-le-s16 (subu8data step1 14 16))))
   (s5parser:settrend! s "rr" rr)
   (s5parser:settrend! s "ppeak"  ppeak 100.)
   (s5parser:settrend! s "peep"  peep 100.)
   (s5parser:settrend! s "pplat"  pplat 100.)
   (s5parser:settrend! s "tv_insp"  tv_insp 10.)
   (s5parser:settrend! s "tv_exp"  tv_exp 10.)
   (s5parser:settrend! s "compliance"  compliance 100.)
   (s5parser:settrend! s "mv_exp" mv_exp 100.)
   (u8data-skip step1 16)))

(define (s5parser:co_wedge_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (co (u8data-le-s16 (subu8data step1 0 2)))
         (blood_temp (u8data-le-s16 (subu8data step1 2 4)))
         (ref (u8data-le-s16 (subu8data step1 4 6)))
         (pcwp (u8data-le-s16 (subu8data step1 6 8))))
   (s5parser:settrend! s "co" co 1000.)
   (s5parser:settrend! s "blood_temp"  blood_temp 100.)
   (s5parser:settrend! s "ref" ref)
   (s5parser:settrend! s "pcwp"  pcwp 100.)
   (u8data-skip step1 8)))

(define (s5parser:nmt_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (t1 (u8data-le-s16 (subu8data step1 0 2)))
         (tratio (u8data-le-s16 (subu8data step1 2 4)))
         (ptc (u8data-le-s16 (subu8data step1 4 6))))
   (s5parser:settrend! s "nmt_t1"  t1 10.)
   (s5parser:settrend! s "nmt_tratio"  tratio 10.)
   (s5parser:settrend! s "nmt_ptc" ptc)
   (u8data-skip step1 6)))

(define (s5parser:ecg_extra_group s buf)
  (let ((hr_ecg (u8data-le-s16 (subu8data buf 0 2)))
        (hr_max (u8data-le-s16 (subu8data buf 2 4)))
        (hr_min (u8data-le-s16 (subu8data buf 4 6))))
   ;; no group header, so assume it's active
   (set! s5parser:group_active? #t)
   (s5parser:settrend! s "hr_ecg" hr_ecg)
   (s5parser:settrend! s "hr_max" hr_max)
   (s5parser:settrend! s "hr_min" hr_min)
   (u8data-skip buf 6)))

(define (s5parser:svo2_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (svo2 (u8data-le-s16 (subu8data step1 0 2))))
   (s5parser:settrend! s "svo2" svo2 100.)
   (u8data-skip step1 2)))

(define (s5parser:basic_phdb s buf)
 ;; (display "s5parser:basic_phdb\n")
  (let* ((step1  (s5parser:ecg_group s buf))
         (step2  (s5parser:p_group s step1 "1"))
         (step3  (s5parser:p_group s step2 "2"))
         (step4  (s5parser:p_group s step3 "3"))
         (step5  (s5parser:p_group s step4 "4"))
         (step6  (s5parser:nibp_group s step5))
         (step7  (s5parser:t_group s step6 "1"))
         (step8  (s5parser:t_group s step7 "2"))
         (step9  (s5parser:t_group s step8 "3"))
         (step10 (s5parser:t_group s step9 "4"))
         (step11 (s5parser:spo2_group s step10))
         (step12 (s5parser:co2_group s step11))
         (step13 (s5parser:o2_group s step12))
         (step14 (s5parser:n2o_group s step13))
         (step15 (s5parser:aa_group s step14))
         (step16 (s5parser:flow_vol_group s step15))
         (step17 (s5parser:co_wedge_group s step16))
         (step18 (s5parser:nmt_group s step17))
         (step19 (s5parser:ecg_extra_group s step18))
         (step20 (s5parser:svo2_group s step19))
         (step21 (s5parser:p_group s step20 "5")))
  (s5parser:p_group s step21 "6")
  (u8data-skip buf 270)))

;; Added so we can reference it in trendoutput or other apps
(define s5parser:physdatavalues_basic (list
  "hr" "st1" "st2" "st3" "imp_rr"
  "p1_sys" "p1_dia" "p1_mean" "p1_hr"
  "p2_sys" "p2_dia" "p2_mean" "p2_hr"
  "p3_sys" "p3_dia" "p3_mean" "p3_hr"
  "p4_sys" "p4_dia" "p4_mean" "p4_hr"
  "nibp_sys" "nibp_dia" "nibp_mean" "nibp_hr"
  "temp1" "temp2" "temp3" "temp4"
  "spo2" "pr" "ir_amp" "s_o2"
  "co2_et" "co2_fi" "co2_rr" "co2_amb"
  "o2_et" "o2_fi" "n2o_et" "n2o_fi"
  "aa_et" "aa_fi" "aa_mac"
  "rr" "ppeak" "peep" "pplat" "tv_insp" "tv_exp" "compliance" "mv_exp"
  "co" "blood_temp" "ref" "pcwp"
  "nmt_t1" "nmt_tratio" "nmt_ptc"
  "hr_ecg" "hr_max" "hr_min"
  "svo2"
  "p5_sys" "p5_dia" "p5_mean" "p5_hr"
  "p6_sys" "p6_dia" "p6_mean" "p6_hr"))

;; ext1 -------------
(define (s5parser:ext1_phdb s buf)
;;  (display "s5parser:ext1_phdb\n")
  (let ((step1 (s5parser:arrh_ecg_group s buf)))
    (s5parser:ecg_12_group s step1)
    (u8data-skip buf 270)))

(define (s5parser:arrh_ecg_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (hr (u8data-le-s16 (subu8data step1 0 2)))
         (rr_time (u8data-le-s16 (subu8data step1 2 4)))
         (pvc (u8data-le-s16 (subu8data step1 4 6))))
    (s5parser:settrend! s "arrh_hr" hr)
    (s5parser:settrend! s "arrh_rr_time" rr_time)
    (s5parser:settrend! s "arrh_pvc" pvc)
    (u8data-skip step1 42)))

(define (s5parser:ecg_12_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (stI (u8data-le-s16 (subu8data step1 0 2)))
         (stII (u8data-le-s16 (subu8data step1 2 4)))
         (stIII (u8data-le-s16 (subu8data step1 4 6)))
         (stAVL (u8data-le-s16 (subu8data step1 6 8)))
         (stAVR (u8data-le-s16 (subu8data step1 8 10)))
         (stAVF (u8data-le-s16 (subu8data step1 10 12)))
         (stV1 (u8data-le-s16 (subu8data step1 12 14)))
         (stV2 (u8data-le-s16 (subu8data step1 14 16)))
         (stV3 (u8data-le-s16 (subu8data step1 16 18)))
         (stV4 (u8data-le-s16 (subu8data step1 18 20)))
         (stV5 (u8data-le-s16 (subu8data step1 20 22)))
         (stV6 (u8data-le-s16 (subu8data step1 22 24))))
    (s5parser:settrend! s "stI" stI 100.)
    (s5parser:settrend! s "stII" stII 100.)
    (s5parser:settrend! s "stIII" stIII 100.)
    (s5parser:settrend! s "stAVL" stAVL 100.)
    (s5parser:settrend! s "stAVR" stAVR 100.)
    (s5parser:settrend! s "stAVF" stAVF 100.)
    (s5parser:settrend! s "stV1" stV1 100.)
    (s5parser:settrend! s "stV2" stV2 100.)
    (s5parser:settrend! s "stV3" stV3 100.)
    (s5parser:settrend! s "stV4" stV4 100.)
    (s5parser:settrend! s "stV5" stV5 100.)
    (s5parser:settrend! s "stV6" stV6 100.)
    (u8data-skip step1 24)))

;; Added so we can reference it in trendoutput or other apps
(define s5parser:physdatavalues_ext1 (list
  "arrh_hr" "arrh_rr_time" "arrh_pvc"
  "stI" "stII" "stIII" "stAVL" "stAVR" "stAVF" "stV1" "stV2" "stV3" "stV4" "stV5" "stV6"))

;; ext2 -------------
(define (s5parser:nmt2_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (nmt_t1 (u8data-le-s16 (subu8data step1 2 4)))
         (nmt_t2 (u8data-le-s16 (subu8data step1 4 6)))
         (nmt_t3 (u8data-le-s16 (subu8data step1 6 8)))
         (nmt_t4 (u8data-le-s16 (subu8data step1 8 10))))
    (s5parser:settrend! s "nmt_t1" nmt_t1)
    (s5parser:settrend! s "nmt_t2" nmt_t2)
    (s5parser:settrend! s "nmt_t3" nmt_t3)
    (s5parser:settrend! s "nmt_t4" nmt_t4)
    (u8data-skip step1 18)))

(define (s5parser:eeg_channel s idx buf)
  (let ((ampl           (u8data-le-s16 (subu8data buf 0 2)))
        (sef            (u8data-le-s16 (subu8data buf 2 4)))
        (mf             (u8data-le-s16 (subu8data buf 4 6)))
        (delta_proc     (u8data-le-s16 (subu8data buf 6 8)))
        (theta_proc     (u8data-le-s16 (subu8data buf 8 10)))
        (alpha_proc     (u8data-le-s16 (subu8data buf 10 12)))
        (beta_proc      (u8data-le-s16 (subu8data buf 12 14)))
        (bsr            (u8data-le-s16 (subu8data buf 14 16))))
  (s5parser:settrend! s (string-append "eeg" idx "_ampl")  ampl 10.)
  (s5parser:settrend! s (string-append "eeg" idx "_sef")  sef 10.)
  (s5parser:settrend! s (string-append "eeg" idx "_mf")  mf 10.)
  (s5parser:settrend! s (string-append "eeg" idx "_deltap") delta_proc)
  (s5parser:settrend! s (string-append "eeg" idx "_thetap") theta_proc)
  (s5parser:settrend! s (string-append "eeg" idx "_alphap") alpha_proc)
  (s5parser:settrend! s (string-append "eeg" idx "_betap") beta_proc)
  (s5parser:settrend! s (string-append "eeg" idx "_bsr") bsr)
))

(define (s5parser:eeg_group s buf)
   (let* ((step1 (s5parser:group_hdr buf))
          (femg (u8data-le-s16 (subu8data step1 0 2))))
     (s5parser:settrend! s "femg" femg 10.)
     (s5parser:eeg_channel s "1" (subu8data step1 2 18))
     (s5parser:eeg_channel s "2" (subu8data step1 18 34))
     (s5parser:eeg_channel s "3" (subu8data step1 34 50))
     (s5parser:eeg_channel s "4" (subu8data step1 50 66))
   (u8data-skip step1 66)))

(define (s5parser:eeg_bis_group s buf)
   (let* ((step1 (s5parser:group_hdr buf))
          (bis     (u8data-le-s16 (subu8data step1 0 2)))
          (sqi_val (u8data-le-s16 (subu8data step1 2 4)))
          (emg_val (u8data-le-s16 (subu8data step1 4 6)))
          (sr_val  (u8data-le-s16 (subu8data step1 6 8))))
   (s5parser:settrend! s "bis" bis)
   (s5parser:settrend! s "bis_sqi" sqi_val)
   (s5parser:settrend! s "bis_emg" emg_val)
   (s5parser:settrend! s "bis_sr"  sr_val)
   (u8data-skip step1 10))) ;; 8 + 2

(define (s5parser:entropy_group s buf)
   (let* ((step1 (s5parser:group_hdr buf))
          (eeg_ent (u8data-le-s16 (subu8data step1 0 2)))
          (emg_ent (u8data-le-s16 (subu8data step1 2 4)))
          (bsr_ent (u8data-le-s16 (subu8data step1 4 6))))
    (s5parser:settrend! s "ent_eeg" eeg_ent)
    (s5parser:settrend! s "ent_emg" emg_ent)
    (s5parser:settrend! s "ent_bsr" bsr_ent)
    (u8data-skip step1 22)))  ;; 6 + 2*8

;; electrode labels, we ignore those for now..
(define (s5parser:eeg2_group s buf)
   (let* ((step1 (s5parser:group_hdr buf)) ;;keeping let* in case rest is commented out
;;        (common_reference     (u8data-u8 (subu8data step1 0 1)))
;;        (montage_label_ch_1_m (u8data-u8 (subu8data step1 1 2)))
;;        (montage_label_ch_1_p (u8data-u8 (subu8data step1 2 3)))
;;        (montage_label_ch_2_m (u8data-u8 (subu8data step1 3 4)))
;;        (montage_label_ch_2_p (u8data-u8 (subu8data step1 4 5)))
;;        (montage_label_ch_3_m (u8data-u8 (subu8data step1 5 6)))
;;        (montage_label_ch_3_p (u8data-u8 (subu8data step1 6 7)))
;;        (montage_label_ch_4_m (u8data-u8 (subu8data step1 7 8)))
;;        (montage_label_ch_4_p (u8data-u8 (subu8data step1 8 9)))
;;        one reserved byte here
         )
     (u8data-skip step1 18)))

(define (s5parser:spi_group s buf)
   (let* ((step1 (s5parser:group_hdr buf))
          (spi (u8data-le-s16 (subu8data step1 0 2))))
    (s5parser:settrend! s "spi" spi 100.)
    (u8data-skip step1 10)))

(define (s5parser:ext2_phdb s buf)
 ;; (display "s5parser:ext2_phdb\n")
  (let* ((step1 (s5parser:nmt2_group s buf))
         (step2 (s5parser:eeg_group s step1))
         (step3 (s5parser:eeg_bis_group s step2))
         (step4 (s5parser:entropy_group s step3))
         (step5 (u8data-skip step4 58))
         (step6 (s5parser:eeg2_group s step5)))
    (s5parser:spi_group s step6)
    (u8data-skip buf 270)))

;; Added so we can reference it in trendoutput or other apps
(define s5parser:physdatavalues_ext2 (list
  "nmt_t1" "nmt_t2" "nmt_t3" "nmt_t4"
  "femg"
  "eeg1_ampl" "eeg1_sef" "eeg1_mf" "eeg1_deltap" "eeg1_thetap" "eeg1_alphap" "eeg1_betap" "eeg1_bsr"
  "eeg2_ampl" "eeg2_sef" "eeg2_mf" "eeg2_deltap" "eeg2_thetap" "eeg2_alphap" "eeg2_betap" "eeg2_bsr"
  "eeg3_ampl" "eeg3_sef" "eeg3_mf" "eeg3_deltap" "eeg3_thetap" "eeg3_alphap" "eeg3_betap" "eeg3_bsr"
  "eeg4_ampl" "eeg4_sef" "eeg4_mf" "eeg4_deltap" "eeg4_thetap" "eeg4_alphap" "eeg4_betap" "eeg4_bsr"
  "bis" "bis_sqi" "bis_emg" "bis_sr"
  "ent_eeg" "ent_emg" "ent_bsr"
  "spi"))

;; ext3 ------------------
(define (s5parser:gasex_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (vo2   (u8data-le-s16 (subu8data step1 0 2)))
         (vco2  (u8data-le-s16 (subu8data step1 2 4)))
         (ee    (u8data-le-s16 (subu8data step1 4 6)))
         (rq    (u8data-le-s16 (subu8data step1 6 8))))
    (s5parser:settrend! s "gasex_vo2" vo2 10.)
    (s5parser:settrend! s "gasex_vco2" vco2 10.)
    (s5parser:settrend! s "gasex_ee" ee)
    (s5parser:settrend! s "gasex_rq" rq)
    (u8data-skip step1 8)))

(define (s5parser:flow_vol_group2 s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (ipeep (u8data-le-s16 (subu8data step1 0 2)))
         (pmean (u8data-le-s16 (subu8data step1 2 4)))
         (raw (u8data-le-s16 (subu8data step1 4 6)))
         (mv_insp (u8data-le-s16 (subu8data step1 6 8)))
         (epeep (u8data-le-s16 (subu8data step1 8 10)))
         (mv_spont (u8data-le-s16 (subu8data step1 10 12)))
         (ie_ratio (u8data-le-s16 (subu8data step1 12 14)))
         (insp_time (u8data-le-s16 (subu8data step1 14 16)))
         (exp_time (u8data-le-s16 (subu8data step1 16 18)))
         (static_compliance (u8data-le-s16 (subu8data step1 18 20)))
         (static_pplat (u8data-le-s16 (subu8data step1 20 22)))
         (static_peepe (u8data-le-s16 (subu8data step1 22 24)))
         (static_peepi (u8data-le-s16 (subu8data step1 24 26))))
    (s5parser:settrend! s "ipeep" ipeep 100.)
    (s5parser:settrend! s "pmean" pmean 100.)
    (s5parser:settrend! s "raw" raw 100.)
    (s5parser:settrend! s "mv_insp" mv_insp 100.)
    (s5parser:settrend! s "epeep" epeep 100.)
    (s5parser:settrend! s "mv_spont" mv_spont 100.)
    (s5parser:settrend! s "ie_ratio" ie_ratio)
    (s5parser:settrend! s "insp_time" insp_time 100.)
    (s5parser:settrend! s "exp_time" exp_time 100.)
    (s5parser:settrend! s "static_compliance" static_compliance 100.)
    (s5parser:settrend! s "static_pplat" static_pplat 100.)
    (s5parser:settrend! s "static_peepe" static_peepe 100.)
    (s5parser:settrend! s "static_peepi" static_peepi 100.)
    (u8data-skip step1 40)))  ;; 26+7*2

(define (s5parser:bal_gas_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (et (u8data-le-s16 (subu8data step1 0 2)))
         (fi (u8data-le-s16 (subu8data step1 2 4))))
    (s5parser:settrend! s "bal_gas_et" et 100.)
    (s5parser:settrend! s "bal_gas_fi" fi 100.)
    (u8data-skip step1 4)))

(define (s5parser:tono_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (prco2       (u8data-le-s16 (subu8data step1 0 2)))
         (pr_et       (u8data-le-s16 (subu8data step1 2 4)))
         (pr_pa       (u8data-le-s16 (subu8data step1 4 6)))
         (pa_delay    (u8data-le-s16 (subu8data step1 6 8)))
         (phi         (u8data-le-s16 (subu8data step1 8 10)))
         (phi_delay   (u8data-le-s16 (subu8data step1 10 12)))
         (amb_press   (u8data-le-s16 (subu8data step1 12 14)))
         (cpma        (u8data-le-s16 (subu8data step1 14 16))))
    (s5parser:settrend! s "prco2"  prco2 100.)
    (s5parser:settrend! s "pr_et"  pr_et 100.)
    (s5parser:settrend! s "pr_pa"  pr_pa 100.)
    (s5parser:settrend! s "pa_delay" pa_delay)
    (s5parser:settrend! s "phi" phi 100.)
    (s5parser:settrend! s "phi_delay" phi_delay)
    (s5parser:settrend! s "amb_press"  amb_press 10.)
    (s5parser:settrend! s "cpma" cpma 10.)
    (u8data-skip step1 16)))

(define (s5parser:aa2_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (mac_age_sum (u8data-le-s16 (subu8data step1 0 2))))
    (s5parser:settrend! s "mac_age_sum" mac_age_sum 100.)
    (u8data-skip step1 18)))

(define (s5parser:delp_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (spv (u8data-le-s16 (subu8data step1 0 2)))
         (ppv (u8data-le-s16 (subu8data step1 2 4))))
    (s5parser:settrend! s "spv" spv 100.)
    (s5parser:settrend! s "ppv" ppv 100.)
    (u8data-skip step1 4)))

(define (s5parser:cpp_group s buf)
  (let* ((step1 (s5parser:group_hdr buf))
         (cpp (u8data-le-s16 (subu8data step1 0 2))))
    (s5parser:settrend! s "cpp" cpp 100.)
    (u8data-skip step1 2)))

(define (s5parser:ext3_phdb s buf)
 ;; (display "s5parser:ext3_phdb\n")
  (let* ((step1 (s5parser:gasex_group s buf))
         (step2 (s5parser:flow_vol_group2 s step1))
         (step3 (s5parser:bal_gas_group s step2))
         (step4 (s5parser:tono_group s step3))
         (step5 (s5parser:aa2_group s step4))
         (step6 (s5parser:delp_group s step5))
         (step7 (s5parser:cpp_group s step6)))
    ;;(s5parser:cpp_group2 s step7)
    (u8data-skip buf 270)))

;; Added so we can reference it in trendoutput or other apps
(define s5parser:physdatavalues_ext3 (list
  "gasex_vo2" "gasex_vco2" "gasex_ee" "gasex_rq"
  "ipeep" "pmean" "raw" "mv_insp" "epeep" "mv_spont" "ie_ratio" "insp_time" "exp_time" "static_compliance" "static_pplat" "static_peepe" "static_peepi"
  "bal_gas_et" "bal_gas_fi"
  "prco2" "pr_et" "pr_pa" "pa_delay" "phi" "phi_delay" "amb_press" "cpma"
  "mac_age_sum" "spv" "ppv" "cpp"))

;;--------------------------
;; the xx_phdb groups are unioned in the dri_phdb structure
;; the data structures are 270 bytes long
(define (s5parser:dri_phdb store buf)
  (let ((time (u8data-le-u32 (subu8data buf 0 4)))
        (payload (subu8data buf 4 274))
        (marker (u8data-u8 (subu8data buf 274 275))) ;;needed for iFish-AA application
        (cl_drivl_subt (u8data-le-u16 (subu8data buf 276 278))))
  (let ((flag (bitwise-and (arithmetic-shift cl_drivl_subt -8) 3)))
    (cond
      ((fx= flag 0) (s5parser:basic_phdb store payload))
      ((fx= flag 1) (s5parser:ext1_phdb store payload))
      ((fx= flag 2) (s5parser:ext2_phdb store payload))
      ((fx= flag 3) (s5parser:ext3_phdb store payload))
      (else (log-error "s5parser: dri_phdb: unknown subrecord"))))
    (store-set! store "timestamp" time "s5")
    (store-set! store "marker" marker "s5") ;; Need marker for iFish-AA application
  (u8data-skip buf 278)))

(define (s5parser:parsetrends store buf srlist)
  ;; subrecord type is displ/10s/60s.. we don't care?
  ;; update - there is an undocumented type 4 which doesn't conform..
  (let loop ((sr srlist))
    (if (fx> (length sr) 0)
      (let ((ofs (car (car sr)))
            (type (cadr (car sr))))
       ;;  (for-each display (list "s5parser: dri_phdb subrecord=" type "\n"))
         ;;(if (fx< type 4) (s5parser:dri_phdb store (u8data-skip buf ofs)))
         ;; We do care as the 60 sec trend causes a jump from the current value to the 1min avg. [MG 29May2011]
         (if (fx= type 1) (s5parser:dri_phdb store (u8data-skip buf ofs)))
         (loop (cdr sr))))))

;; ----------------
;; waveforms   [Changed ECG from 1.0 to 1000.0 as the standard is mV not uV]

(define s5parser:waveforms '(
  (1 "ECG1" 1000.)  (2 "ECG2" 1000.) (3 "ECG3" 1000.)
  (4 "INVP1" 100.) (5 "INVP2" 100.) (6 "INVP3" 100.) (7 "INVP4" 100.)
  (8 "PLETH" 100.) (9 "CO2" 100.) (10 "O2" 100.) (11 "N2O" 100.)
  (12 "AA" 100.) (13 "AWP" 100.) (14 "FLOW" 100.) (15 "RESP" 100.)
  (16 "INVP5" 100.) (17 "INVP6" 100.)
  (18 "EEG1" 10.) (19 "EEG2" 10.) (20 "EEG3" 10.) (21 "EEG4" 10.)
  (23 "VOL" 1.) (24 "TONO" 10.) (29 "SPILOOP" 1.) (32 "ENT" 10.)
  (35 "BIS" 1.)
 ))

(define (s5parser:getwaveformtype str)
  (let ((ret 0))
    (for-each (lambda (w) (if (string=? (cadr w) str) (set! ret (car w)))) s5parser:waveforms)
    ret
  ))

(define (s5parser:parsewaveforms s buf srlist)
 ;; (display "parsing waveforms\n")
  (let loop ((srs srlist))
    (if (> (length srs) 0)
      (let* ((ofs (car (car srs)))
             (type (cadr (car srs)))
             (wave (assoc type s5parser:waveforms))
             (wavename (if wave (cadr wave) #f))
             (wavescale (if wave (caddr wave) #f))
             (wavelen (u8data-le-s16 (subu8data buf ofs (+ ofs 2))))
             (wavevalid (if (> wavelen 0) (fx> (u8data-le-s16 (subu8data buf (fx+ ofs 6) (fx+ ofs 8))) -32000) #f)))
     ;; To save CPU time we only take valid waveform packages if running online - invalid are padded
     (if (and wavename (if s5parser:fromfile (> wavelen 0) wavevalid))
       (let ((wavedata (##still-copy (make-f32vector wavelen)))
             (wavescaleinv (/ 1. wavescale)))
         ;; populate the vector
         (let loop2 ((o (fx+ ofs 6))(n 0)(flag 0))
           (if (fx= n wavelen)
             (if (and (> flag 0) (not s5parser:fromfile)) (s5parser:log 0 "s5parser: invalid data in waveform " flag))
             (let* ((val (u8data-le-s16 (subu8data buf o (fx+ o 2))))
                    (newflag (fx< val -32000))
                    (sval (if newflag 0. (fl* (exact->inexact val) wavescaleinv))))
               (f32vector-set! wavedata n sval)
               (loop2 (fx+ o 2) (fx+ n 1) (if newflag (+ flag 1) flag))
            )))
         (store-set! s "gotwaveforms?" #t "s5")
         (store-waveform-append s wavename wavedata)
       )
       (begin
         (if (and wavename (> wavelen 0) (not wavevalid)) (s5parser:log 1 "s5parser: invalid waveform data: [" buf "]" ))
         (if (and wavename (or (fx= type 1) (fx= type 8) (fx= type 9)) (fx> wavelen 0)) (begin
;;         (if (and wavename (> wavelen 0)) (begin
           (store-waveform-append s wavename (make-f32vector wavelen))
         ))
       ))
     (loop (cdr srs))))))

;; ----------------
;; patient data
(define s5parser:demographics '(
  "FirstName" "LastName" "PatientID" "Sex" "Age" "Height" "Weight"))

(define (s5parser:nw_pat_descr s buf)
  (let ((pat_1stname (u8data->u8vector (subu8data buf 0 30)))
        (pat_2ndname (u8data->u8vector (subu8data buf 30 70)))
        (pat_id      (u8data->u8vector (subu8data buf 70 110)))
;;      (middle_name (u8data->u8vector (subu8data buf 110 140)))
        (gender    (u8data-le-s16 (subu8data buf 140 142)))
        (age_years (u8data-le-s16 (subu8data buf 142 144)))
        (age_days (u8data-le-s16 (subu8data buf 144 146)))
        (age_hours (u8data-le-s16 (subu8data buf 146 148)))
        (height (u8data-le-s16 (subu8data buf 148 150)))
        (height_unit (u8data-le-s16 (subu8data buf 150 152)))
        (weight (u8data-le-s16 (subu8data buf 152 154)))
        (weight_unit (u8data-le-s16 (subu8data buf 154 156)))
;;      (year_birth_date (u8data-le-s16 (subu8data buf 156 158)))
;;      (month_birth_date (u8data-le-s16 (subu8data buf 158 160)))
;;      (day_birth_date (u8data-le-s16 (subu8data buf 160 162)))
;;      (hour_birth_date (u8data-le-s16 (subu8data buf 162 164)))
;;      (bsa (u8data-le-s16 (subu8data buf 164 166)))
;;      (location (subu8data buf 166 198))
;;      (issuer (subu8data buf 198 230))
;;      (change_src (u8data-le-s16 (subu8data buf 230 232)))
;;      (reserved (subu8data buf 232 350))
       )
;;   (for-each display (list "s5parser: patient: gender=" gender
;;       " age_years=" age_years
;;       " age_days=" age_days
;;       " age_hours=" age_hours
;;       " weight=" weight
;;       " height=" height  "\n"))
   ;; name [Added 20Jun2011 MG]
   (store-set! s "FirstName" (u8vector->string pat_1stname) "s5")
   (store-set! s "LastName" (u8vector->string pat_2ndname) "s5")
   ;; patient id
   (store-set! s "PatientID" (u8vector->string pat_id) "s5")
   ;; gender
   (if (fx= gender 1) (store-set! s "Sex" "Male" "s5"))
   (if (fx= gender 2) (store-set! s "Sex" "Female" "s5"))
   ;; age
   (if (and (>= age_years 0) (>= age_days 0) (>= age_hours 0))
     (let ((age (+ age_years (/ age_days 360.) (/ age_hours (* 360. 24.)))))
      (if (> age 0) (store-set! s "Age" age "s5"))))
   ;; height
   (if (and (> height 0) (or (fx= height_unit 1) (fx= height_unit 2)))
      (store-set! s "Height" (* height (if (fx= height_unit 1) 1. 2.54)) "s5"))
   ;; weight
   (if (and (> weight 0) (or (fx= weight_unit 1) (fx= weight_unit 2)))
      (store-set! s "Weight" (* weight (if (fx= weight_unit 1) 1. 0.45359237)) "s5"))
 ))


(define (s5parser:parsepatientdata s buf srlist)
;;  (display "parsing patientdata\n")
  (let loop ((srs srlist))
    (if (> (length srs) 0)
      (let ((ofs (car (car srs)))
            (type (cadr (car srs))))
        (cond
          ((fx= type 6) (s5parser:nw_pat_descr s (u8data-skip buf ofs)))
          ((fx= type 10) (begin
            (store-clear! s (map cadr s5parser:physdatavalues_basic))
            (store-clear! s (map cadr s5parser:physdatavalues_ext1))
            (store-clear! s (map cadr s5parser:physdatavalues_ext2))
            (store-clear! s (map cadr s5parser:physdatavalues_ext3))
            (store-clear! s (map cadr s5parser:waveforms))
          ))
;;          (else
;;            (for-each display (list "s5parser: patient subrecord=" type " is not parsed.\n")))
        )
        (loop (cdr srs))))))

;; ---------------
;; anesthesia record keeping event data
(define (s5parser:ar_descr s buf)
  #f
;;  (display (u8vector->string (u8data->u8vector (subu8data buf 0 (length buf)))))(newline)
;;  (display (map (lambda(l) (if (< l 16) (string-append "0" (number->string l 16)) (number->string l 16))) (u8vector->list (u8data->u8vector buf))))(newline)
)

(define (s5parser:anesthesiarecorddata s buf srlist)
  (let loop ((srs srlist))
    (if (> (length srs) 0)
      (let ((ofs (car (car srs)))
            (type (cadr (car srs))))
        (cond
          ((fx= type 0) (s5parser:ar_descr s (u8data-skip buf ofs)))
          ;;(else (for-each display (list "s5parser: anesthetic record subrecord=" type " is not parsed.\n")))
        )
        (loop (cdr srs))))))

;; ---------------
;; alarm data
(define (s5parser:al_disp_al s buf idx)
  (let* ((text (u8data->u8vector (subu8data buf 0 80)))
         (textstr (string-replace-char (u8vector->string text) #\newline #\space)) ;; Strip linebreaks
;;       (text_changed (u8data-le-s16 (subu8data buf 80 82)))
	 ;; DRI_PR0 = 0, //No alarm DRI_PR1 = 1, //White DRI_PR2 = 2, //Yellow DRI_PR3 = 3 //Red
         (color (u8data-le-s16 (subu8data buf 82 84)))
;;       (color_changed (u8data-le-s16 (subu8data buf 84 86)))
;;       (reserved (subu8data buf 86 98))
        )
    ;;(display (string-append s "[" idx "]: " textstr))(newline)
    (store-set! s (string-append "alarm" idx "_text") textstr "s5")
    (store-set! s (string-append "alarm" idx "_color") color "s5")
    ;; Add the alarm event to the respective store
    ;;(if (and (fx> color 0) (fx> text_changed 0)) (store-event-add s color textstr))
    (u8data-skip buf 98)
  ))

(define (s5parser:dri_al_msg s buf)
  (let* (
;;       (reserved1 (subu8data buf 0 2))
;;       (sound_on_off (u8data-le-s16 (subu8data buf 2 4)))
;;       (reserved2 (subu8data buf 4 6))
;;       (reserved3 (subu8data buf 6 8))
;;       (silence_info (u8data-le-s16 (subu8data buf 8 10)))
         (step1 (s5parser:al_disp_al s (u8data-skip buf 10) "1"))
	 (step2 (s5parser:al_disp_al s step1 "2"))
	 (step3 (s5parser:al_disp_al s step2 "3"))
	 (step4 (s5parser:al_disp_al s step3 "4"))
	 (step5 (s5parser:al_disp_al s step4 "5"))
;;       (reserved (subu8data step5 0 10))
        )
    (u8data-skip step5 10)))

(define (s5parser:parsealarms store buf srlist)
  (let loop ((sr srlist))
    (if (fx> (length sr) 0)
      (let ((ofs (car (car sr)))
            (type (cadr (car sr))))
	  ;; subrecord type of alarms is 1
         (if (fx= type 1) (s5parser:dri_al_msg store (u8data-skip buf ofs)))
         (loop (cdr sr))))))

;; ----------------
;; fake marker for new anesthesia machine
(define (s5parser:fakemarker s)
  (let ((oldmarker (store-ref s "fake_marker" 0)))
    (store-set! s "fake_marker" (fx+ oldmarker 1) "s5")
  )
)

;; ----------------
;; top level

;; all frames have the same header format
;M @deffn {procedure} s5parser store buf
;M Takes raw data from S5 patient monitor and saves the patient information in the data store
;M @end deffn
(define (s5parser store buf)
  (let ((subrecords (u8data-skip buf 16))
        (payload (u8data-skip buf 40))
        (r_len (u8data-le-u16 (subu8data buf 0 2)))
        (dri_level (u8data-u8  (subu8data buf 3 4)))
        (plug_id (u8data-le-u16 (subu8data buf 4 6)))
        (r_time (u8data-le-u32 (subu8data buf 6 10)))
        (r_maintype (u8data-le-u16 (subu8data buf 14 16))))
    (store-set! store "dri_level" dri_level "s5")
    (store-set! store "plug_id" plug_id "s5") ;;Added for iFish-AA (we know these from Dave Kobayashi)
    (store-set! store "r_time" r_time "s5")
    (store-setnew! store "s5?" #t "s5")
    (let loop ((n 0)(p subrecords)(srlist '())(done #f))
      (if (or done (fx= n 8)) (begin
;;        (for-each display (list "s5parser: type=" r_maintype " : "
;;           (length srlist) " subrecords..\n"))
        (cond
          ((fx= r_maintype 0)
             (if (> r_len 270)  ;; at least one trend block please
               (s5parser:parsetrends store payload srlist)
               #f ;;(for-each display (list "s5parser: ignoring invalid trend frame from " store "\n"))
           ))
          ((fx= r_maintype 1) (s5parser:parsewaveforms store payload srlist))
	  ((fx= r_maintype 4) (s5parser:parsealarms store payload srlist))
          ((fx= r_maintype 5) (s5parser:parsepatientdata store payload srlist))
	  ((and (fx= r_maintype 8) (> r_len 100)) (s5parser:anesthesiarecorddata store payload srlist))
	  ((fx= r_maintype 18) (s5parser:fakemarker store))
        ))
      (let ((offset (u8data-le-u16 (subu8data p 0 2)))
            (sr_type (u8data-u8 (subu8data p 2 3))))
         (loop (fx+ n 1) (u8data-skip p 3) (append srlist
            (if (not (fx= sr_type #xff))  (list (list
            offset sr_type)) '())) (fx= sr_type #xff)))))))

;; eof
