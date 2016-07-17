;; Philips Intellivue Parser
;; Matthias Görges, 2016

;; Parse Get
(define (ivueparser:parse AttributeIdList buf)
  (let ((count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(al (list))(p (u8data-skip buf 4)))
      (if (fx= n count)
        (begin
          (ivueparser:log 2 "ivueparser: get AttributeIdList: " (map (lambda (l) (number->string l 16)) al))
          p
        )
        (loop (fx+ n 1) (append al (list (u8data-u16 (subu8data p 0 2)))) (u8data-skip p 2))
      )
    )
  ))

(define (ivueparser:parseGetArgument buf)
  (let ((managed_object (ivueparser:parseManagedObjectId buf))
        (scope (u8data-u32 (subu8data buf 6 10)))
        (attributeIdList (ivueparser:parse AttributeIdList (u8data-skip buf 10))))
    (if (fx= (u8data-length attributeIdList) 0)
      #t
      (ivueparser:log 1 "ivueparser: incomplete parse of attributeIdList [" (u8data-length attributeIdList) "]")
    )
  ))

(define (ivueparser:parseCmdGet buf)
  (ivueparser:parseGetArgument buf)
)

;;eof
