;; Philips Intellivue Parser
;; Chris Petersen, 2011
;; Matthias Görges, 2016

;; Helper for quick name retrieval
(define (ivueparser:getname handle_id)
  (let ((physio_id (bitwise-and (table-ref ivueparser:labellut handle_id) #xffff)))
    (table-ref ivueparser:phystable1 physio_id)
  ))

;; Parse Compound Numeric Observed Value
(define (ivueparser:parseNuObsValueCmp handle_id buf)
  (let ((count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(p (u8data-skip buf 4)))
      (if (fx= n count)
        p
        (loop (fx+ n 1) (ivueparser:parseNuObsValue handle_id p))
      )
    )
  ))

;; Parse Numeric Observed Value
(define (ivueparser:parseNuObsValue handle_id buf)
  (let* ((physio_id (u8data-u16 (subu8data buf 0 2)))
         (state (u8data-u16 (subu8data buf 2 4)))
         (unit_code (u8data-u16 (subu8data buf 4 6)))
         (value (ivueparser:parseFLOATType (subu8data buf 6 10)))
         (name (ivueparser:findphys physio_id (table-ref ivueparser:labellut handle_id))))
    (if (and value (fx= (bitwise-and state #xf800) 0)) ;; ignore invalid data but allow demo
      (if name
        (store-set! ivueparser:store name value "ivue")
        (ivueparser:log 1 "ivueparser: failed to lookup code: " (number->string physio_id 16))
      )
      (if name (store-clear! ivueparser:store name))
    )
    (u8data-skip buf 10)
  ))

;; Display Resolution
(define (ivueparser:parseDispResolution handle_id buf)
  (let ((pre_point (u8data-u8 (subu8data buf 0 1)))
        (post_point (u8data-u8 (subu8data buf 1 2)))
        (name (ivueparser:getname handle_id)))
    (if name (store-set! ivueparser:store
      (string-append name "_resolution") (list pre_point post_point)
      "ivue_display"
    ))
  ))

;; Unit Code
(define (ivueparser:parseUnitCode handle_id buf)
  (let* ((unit_code (u8data-u16 (subu8data buf 0 2)))
         (unit (car (table-ref ivueparser:unittable unit_code '(#f))))
         (name (ivueparser:getname handle_id)))
   (if (and name unit) (store-set! ivueparser:store
     (string-append name "_unit") unit
     "ivue"
   ))
    unit_code
  ))

;; eof
