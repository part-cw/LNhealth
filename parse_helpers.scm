;; Philips Intellivue Parser
;; Matthias Görges, 2016

(define (u8vector->u16vector u8v)
  (let* ((len (u8vector-length u8v))
         (len2 (fix (/ len 2)))
         (u16v (make-u16vector len2)))
    (let loop ((n 0) (n2 0))
      (if (fx= n len)
        u16v
        (begin
          (u16vector-set! u16v n2
            (bitwise-ior (arithmetic-shift (u8vector-ref u8v n) 8) (u8vector-ref u8v (fx+ n 1))))
          (loop (fx+ n 2) (fx+ n2 1))
        )
      )
    )
  ))

;; Parse Helpers
(define (ivueparser:parseGlbHandle buf)
  (let ((context_id (u8data-u16 (subu8data buf 0 2)))
        (handle (u8data-u16 (subu8data buf 2 4))))
    (list context_id handle)
  ))

(define (ivueparser:parseManagedObjectId buf)
  (let ((m_obj_class (u8data-u16 (subu8data buf 0 2)))
        ;;(m_obj_inst (ivueparser:parseGlbHandle buf))
        (context_id (u8data-u16 (subu8data buf 2 4)))
        (handle (u8data-u16 (subu8data buf 4 6))))
    (list m_obj_class context_id handle)
  ))

(define (ivueparser:parseTYPE buf)
  (let ((partition (u8data-u16 (subu8data buf 0 2)))
        (code (u8data-u16 (subu8data buf 2 4))))
    (list partition code)
  ))

(define (ivueparser:parseFLOATType val)
  (let ((exponent (u8data-ref val 0))
        (mantissa (bitwise-ior (arithmetic-shift (u8data-ref val 1) 16)
          (arithmetic-shift (u8data-ref val 2) 8) (u8data-ref val 3))))
    (if (or (fx= mantissa #x800000) (fx= mantissa #x7fffff)
            (fx= mantissa #x7ffffe) (fx= mantissa #x800002))
      #f
      (* (u8data:u24->s24 mantissa) (expt 10.0 (u8data:u8->s8 exponent)))
    )
  ))

(define (ivueparser:parseBCD val)
  (if (and (fx<= (arithmetic-shift val -4) 9)
           (fx<= (modulo val 16) 9))
    (string-append (number->string (arithmetic-shift val -4))
                   (number->string (modulo val 16)))
    #f
  ))

(define (ivueparser:parseString buf)
  (let* ((len (u8data-u16 (subu8data buf 0 2)))
         (value (u8vector->u16vector (u8data->u8vector (subu8data buf 2 (fx+ len 2))))))
    value
  ))

(define (ivueparser:parseSimpleColour buf)
  (let ((SimpleColour (u8data-u16 (subu8data buf 0 2))))
    (cond
      ((fx= SimpleColour COL_BLACK) Black)
      ((fx= SimpleColour COL_RED) Red)
      ((fx= SimpleColour COL_GREEN) Green)
      ((fx= SimpleColour COL_YELLOW) Yellow)
      ((fx= SimpleColour COL_BLUE) Blue)
      ((fx= SimpleColour COL_MAGENTA) Magenta)
      ((fx= SimpleColour COL_CYAN) Cyan)
      ((fx= SimpleColour COL_WHITE) White)
      ((fx= SimpleColour COL_PINK) Pink)
      ((fx= SimpleColour COL_ORANGE) Orange)
      ((fx= SimpleColour COL_LIGHT_GREEN) LightGreen)
      ((fx= SimpleColour COL_LIGHT_RED) (color:shuffle #xff474cff))
      (else Gray)
    )
  ))
;;eof
