;; Philips Intellivue Parser
;; Matthias Görges, 2016

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
         (localname (ivueparser:findphys physio_id (table-ref ivueparser:labellut handle_id))))
    (if localname
      (store-waveform-append ivueparser:store localname (u16vector->list (u8vector->u16vector vals)))
      (ivueparser:log 1 "ivueparser: no waveform name for" label "," physio_id)
    )
    (u8data-skip buf (fx+ len 6))
  ))

(define (ivueparser:parseScaleRangeSpec16 handle_id buf)
  (let ((lower_absolute_value (ivueparser:parseFLOATType (subu8data buf 0 4)))
        (upper_absolute_value (ivueparser:parseFLOATType (subu8data buf 4 8)))
        (lower_scaled_value (u8data-u16 (subu8data buf 8 10)))
        (upper_scaled_value (u8data-u16 (subu8data buf 10 12))))
    (let* ((physio_id (bitwise-and (table-ref ivueparser:labellut handle_id) #xffff))
           (name (table-ref ivueparser:phystable1 physio_id)))
      (if name
        (store-waveform-scale ivueparser:store name
          (if (not lower_absolute_value)
            (list lower_scaled_value upper_scaled_value 0. 1.)
            (list lower_scaled_value upper_scaled_value lower_absolute_value upper_absolute_value)
          )
        )
        (ivueparser:log 1 "ivueparser: no waveform name for" physio_id)
      )
    )
    (u8data-skip buf 12)
  ))

;; Metric State
(define (ivueparser:parseMetricState handle_id buf)
  (let ((MetricState (u8data-u16 (subu8data buf 0 2))))
    (ivueparser:log 2 "ivueparser: MetricState" handle_id MetricState)
    MetricState
  ))

;; Color
(define (ivueparser:parseSimpleColourAttribute handle_id buf)
  (let* ((SimpleColour (ivueparser:parseSimpleColour (u8data-u16 (subu8data buf 0 2))))
         (physio_id (bitwise-and (table-ref ivueparser:labellut handle_id) #xffff))
         (name (table-ref ivueparser:phystable1 physio_id)))
    (if name
      (store-set! ivueparser:store (string-append name "_color") SimpleColour "ivue")
    )
  ))

;; eof
