;; Philips Intellivue Parser
;; Matthias Görges, 2016-2017

(define (ivueparser-set-defaultwaveformscaling! store)
  (for-each (lambda (l) (store-waveform-scale store (car l) (cdr l)))
    '(("ABP" 160 9120 -40 520)
      ("ART" 160 9120 -40 520)
      ("CO2aw" 100 4000 -4 48)
      ("CVP" 160 9120 -40 520)
      ("ECG" 0 16383 -40.96 40.955)
      ("ICP" 160 9120 -40 520)
      ("Resp" 0 4095 -0.6 1.9)
      ("PAP" 160 9120 -40 520)
      ("PLETHl" 0 4095 0. 1.)
      ("Pleth" 0 4095 0. 1.)
      ("Resp" 0 4095 -0.6 1.9))
      ("CO2" 0 4000 -10 50)
      ("AWV" 0 4000 -100 1100)
      ("AWP" 0 4000 -10 50)
      ("EEG" 0 4095 -62.5 62.5)
      ("ST" 32768 32767 -163.84 163.835)
      ("QT" 32768 32767 -81.92 81.9175)
   ))

;; Special network parsing
;; PollWaveformList
(define (ivueparser:parsePollWaveformList buf)
  (let ((count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(p (u8data-skip buf 4)))
      (if (fx= n count)
        p
        (loop (fx+ n 1) (ivueparser:parseSingleWaveform p))
      )
    )
  ))

;; SingleWaveform
(define (ivueparser:parseSingleWaveform buf)
  (let ((context_id (u8data-u16 (subu8data buf 0 2)))
        (count (u8data-u16 (subu8data buf 2 4)))
        (len (u8data-u16 (subu8data buf 4 6))))
    (let loop ((n 0)(p (u8data-skip buf 6)))
      (if (fx= n count)
        p
        (loop (fx+ n 1) (ivueparser:parseWaveform p))
      )
    )
  ))

;; Waveform
(define (ivueparser:parseWaveform buf)
  (let ((handle_id (u8data-u16 (subu8data buf 0 2)))
        (wave (u8data-skip buf 2)))
    (ivueparser:parseSaObsValue handle_id wave)
  ))

;; Regular SaObsValue parsing
(define (ivueparser:parseSaObsValueCmp handle_id buf)
  (let ((count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(p (u8data-skip buf 4)))
      (if (fx= n count)
        p
        (loop (fx+ n 1) (ivueparser:parseSaObsValue handle_id p))
      )
    )
  ))

(define (ivueparser:parseSaObsValue handle_id buf)
  (let* ((physio_id (u8data-u16 (subu8data buf 0 2)))
         (state (u8data-u16 (subu8data buf 2 4)))
         (len (u8data-u16 (subu8data buf 4 6)))
         (vals (u8data->u8vector (subu8data buf 6 (fx+ 6 len))))
         (name (ivueparser:findphys physio_id (fx+ #x20000 physio_id))))
    (if name
      (store-waveform-append ivueparser:store name (u16vector->list (u8vector->u16vector vals)))
      (ivueparser:log 1 "ivueparser: failed to lookup code: " (number->string physio_id 16))
    )
    (u8data-skip buf (fx+ len 6))
  ))

(define (ivueparser:parseScaleRangeSpec16_getnames name)
  (cond
    ((string=? name "ECG")
      ivueparser:physecg)
    ((string=? name "ST")
      ivueparser:physst)
    ((string=? name "EEG")
      (list "EEG L" "EEG R" "EEG1" "EEG2" "EEG3" "EEG4"))
    (else
      (list name))
  ))

(define (ivueparser:parseScaleRangeSpec16 handle_id buf)
  (let ((lower_absolute_value (ivueparser:parseFLOATType (subu8data buf 0 4)))
        (upper_absolute_value (ivueparser:parseFLOATType (subu8data buf 4 8)))
        (lower_scaled_value (u8data-u16 (subu8data buf 8 10)))
        (upper_scaled_value (u8data-u16 (subu8data buf 10 12)))
        (name (ivueparser:getname handle_id)))
    (if name
      (let ((names (ivueparser:parseScaleRangeSpec16_getnames name)))
        (for-each (lambda (n) (store-waveform-scale ivueparser:store n
          (if (not lower_absolute_value)
            (list lower_scaled_value upper_scaled_value 0. 1.)
            (list lower_scaled_value upper_scaled_value lower_absolute_value upper_absolute_value)
          )
        )) names)
      )
      (ivueparser:log 1 "ivueparser: no waveform name for: " handle_id)
    )
    (ivueparser:log 3 "ivueparser: waveform scaling: " name " "
      (table-ref (store:wscalingtable ivueparser:store) name))
    (u8data-skip buf 12)
  ))

;; Metric State
(define (ivueparser:parseMetricState handle_id buf)
  (let ((MetricState (u8data-u16 (subu8data buf 0 2)))
        (name (ivueparser:getname handle_id)))
    (if name (store-set! ivueparser:store
      (string-append name "_state") (not (fx= MetricState #x8000))
      "ivue_display"
    ))
  ))

;; Color
(define (ivueparser:parseSimpleColourAttribute handle_id buf)
  (let ((SimpleColour (ivueparser:parseSimpleColour (subu8data buf 0 2)))
        (name (ivueparser:getname handle_id)))
    (if name
      (store-set! ivueparser:store (string-append name "_color") SimpleColour "ivue_display")
    )
  ))

;; Visual Grid
(define (ivueparser:parseSaVisualGrid16 handle_id buf)
  (let ((count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4)))
        (name (ivueparser:getname handle_id)))
    (if name
      (let loop ((n 0)(grid (list))(p (u8data-skip buf 4)))
        (if (fx= n count)
          (store-set! ivueparser:store (string-append name "_grid") grid "ivue_display")
          (loop (fx+ n 1) (append grid (ivueparser:parseSaGridEntry16 p)) (u8data-skip p 8))
        )
      )
    )
  ))

(define (ivueparser:parseSaGridEntry16 buf)
  (let ((absolute_value (ivueparser:parseFLOATType (subu8data buf 0 4)))
        (scaled_value (u8data-u16 (subu8data buf 4 6)))
        (level (u8data-u16 (subu8data buf 6 8))))
    ;; (list absolute_value) ;; Simpler version only storing the absolute values
    (list (list absolute_value scaled_value level))
  ))

;; Sample Period
(define (ivueparser:parseSamplePeriod handle_id buf)
  (let ((ts (fl/ (flo (u8data-u32 (subu8data buf 0 4))) 8000.))
        (name (ivueparser:getname handle_id)))
    (if name
      (store-set! ivueparser:store (string-append name "_grid") ts "ivue")
    )
  ))

;; eof
