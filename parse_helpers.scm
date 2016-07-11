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

;;eof
