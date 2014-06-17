;; temporary storage for waveform data
;; the philips monitor groups waveforms and sends scaling information for the store separately
;; this complicates the usual waveform dispatch slightly 

;; default scaling information
;; this will be overwritten/appended on the fly
;; 20101013: overhauled 
(define ivueparser:scaletable (list->table
  '( (657 	0 16383 -40.96 40.955)
     (932 	0 4095  0. 1.)
     (1035 	0 4095  0. 1.)
     (954 	0 4095  0. 1.)
     (986 	0 4095  0. 1.)
     (745	160 9120 -40. 520)
     (969	160 9120 -40. 520)
     (932	160 9120 -40. 520)
     (761	160 9120 -40. 520)
     (830	160 9120 -40. 520)
     (798	160 9120 -40. 520)
     (869	160 9120 -40. 520)
     (781	160 9120 -40. 520)
     (820	160 9120 -40. 520)
     (682	0 4095 -0.3 0.8)
     (696	0 4095 -0.3 0.8)
     (717	0 4095 -0.3 0.8)
     (1008	100 4000 -5. 60.)
     (667	0 4095 -0.6 1.9)
     (33459	-32767 32767 -163.84 163.835)
     (33494	-32767 32767 -163.84 163.835)
     (33473	-32767 32767 -163.84 163.835) 
     (672	0 16383 -40.96 40.955)
     (686	0 16383 -40.96 40.955)
     (707	0 16383 -40.96 40.955)
     (1091      100 4000 -4. 48.)
     (1042      100 4000 -4. 48.)
   )))

;; register store scaling information
(define (ivueparser:storage-scale store label i_min i_max o_min o_max)
  (table-set! ivueparser:scaletable label (list i_min i_max o_min o_max)))

;; add data to the waveform storage
(define (ivueparser:storage-data store label pid data)
    (let ((localname (ivueparser:findphys pid 
             (table-ref ivueparser:labellut ivueparser:handleid))))
      (if localname (begin
        (store-waveform-append store localname data)
        (let ((scale (table-ref ivueparser:scaletable label #f)))
          (store-waveform-scale store localname (if scale scale (begin 
              (ivueparser:log 2 "ivueparser: no scaling data for " label "," pid)
            '(-32767 32767 -1. 1.))))
        ))
        (ivueparser:log 2 "ivueparser: couldn't find waveform name for " label "," pid)
  )))

;; eof
