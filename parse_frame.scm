;; Philips Intellivue Parser
;; Chris Petersen, 2011
;; Matthias Görges, 2016

(define (ivueparser:parseFrameHdr buf)
  (let ((protocol_id (u8data-u8 (subu8data buf 0 1)))
        (msg_type (u8data-u8 (subu8data buf 1 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (if (and (fx= protocol_id #x11) (fx= msg_type #x01)
             (fx= len (fx- (u8data-length buf) 6))) ;; 4 hdr + 2 crc
      len
      (begin
        (ivueparser:log 1 "ivueparser: bad frame header")
        #f
      )
    )
  ))

(define (ivueparser:verifyFCS buf)
  (let* ((len (u8data-length buf))
         (fcs (u8data-u16 (subu8data buf (fx- len 2) len)))
         (crc (ivueparser:crc (u8data->u8vector buf) 0 (fx- len 2))))
    ;; the bitwise and's are needed to eliminate sign!
    (if (fx= (bitwise-and fcs #xffff) (bitwise-and crc #xffff))
      (bitwise-and fcs #xffff)
      (begin
        (ivueparser:log 1 "ivueparser: frame check sequence failed" fcs crc)
        #f
      )
    )
  ))

(define (ivueparser:decodeframe buf)
  (let ((len (u8data-length buf)))
    (if (and (fx= (u8data-ref buf 0) #xc0)
             (fx= (u8data-ref buf (fx- len 1))) #xc1)
      (let loop ((i 1) (obuf (u8vector)) (escaped #f))
        (if (fx= i (fx- len 1))
          (u8vector->u8data obuf)
          (let* ((c (u8data-ref buf i))
                 (newescaped (fx= c #x7d)))
            (loop (fx+ i 1) (u8vector-append obuf (if newescaped (u8vector)
              (if escaped (u8vector (bitwise-xor c #x20)) (u8vector c)))) newescaped)
           )
         )
       )
       (begin
         (ivueparser:log 2 "ivueparser: invalid BOF or EOF")
         #f
       )
     )
  ))

;; parse a raw (unvalidated) frame from the monitor
(define (ivueparser:parseframe buf)
  (let ((data (ivueparser:decodeframe buf)))
    (if (u8data:sane? data)
      (let ((len (ivueparser:parseFrameHdr (subu8data buf 0 4)))
            (fcs (ivueparser:verifyFCS buf)))
        (if (and fcs len)
          (ivueparser:parseMessage (subu8data buf 4 len))
          #f
        )
      )
      #f
    )
  ))
;; eof
