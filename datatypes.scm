;; philips monitor numerical data types
(define (ivueparser:decodef32 v) 
  (let ((exponent (u8data-ref v 0))
        (mantissa (bitwise-ior
          (arithmetic-shift (u8data-ref v 1) 16)
          (arithmetic-shift (u8data-ref v 2) 8)
          (u8data-ref v 3))))
    (* (u8data:u24->s24 mantissa) 
         (expt 10.0 (u8data:u8->s8 exponent)))))

;; eof
