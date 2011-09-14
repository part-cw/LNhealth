;; philips monitor numerical data types

(define (ivueparser:decodeu8 v) 
  (u8data-ref v 0))

(define (ivueparser:decodeu16 v)
  (bitwise-ior (arithmetic-shift (u8data-ref v 0) 8)
     (u8data-ref v 1)))

(define (ivueparser:decodeu32 v)
  (bitwise-ior (arithmetic-shift (u8data-ref v 0) 24)
              (arithmetic-shift (u8data-ref v 1) 16)
              (arithmetic-shift (u8data-ref v 2) 8)
     (u8data-ref v 3)))

(define (ivueparser:u8->s8 v)
  (if (fx<= v 127) v (fx- v 256)))

(define (ivueparser:u16->s16 v)
  (if (fx<= v 32767) v (fx- v 65536)))

(define (ivueparser:u24->s24 v)
  (if (fx<= v 8388607) v (fx- v 16777216)))

(define (ivueparser:decodef32 v) 
  (let ((exponent (u8data-ref v 0))
        (mantissa (bitwise-ior
          (arithmetic-shift (u8data-ref v 1) 16)
          (arithmetic-shift (u8data-ref v 2) 8)
          (u8data-ref v 3))))
    (* (ivueparser:u24->s24 mantissa) 
         (expt 10.0 (ivueparser:u8->s8 exponent)))))

;; eof
