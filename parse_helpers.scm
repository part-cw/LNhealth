;; Philips Intellivue Parser
;; Matthias Görges, 2016

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
