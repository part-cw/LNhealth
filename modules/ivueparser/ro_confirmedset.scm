;; Philips Intellivue Parser
;; Matthias Görges, 2016

;; Parse Confirmed Set
(define (ivueparser:parseSetResult buf)
  (let* ((managed_object (ivueparser:parseManagedObjectId buf))
         (context_id (cadr managed_object))
         (attributes (ivueparser:parseAttributeList context_id (u8data-skip buf 6))))
    (if (fx= (u8data-length attributes) 0)
      #t
      (ivueparser:log 0 "ivueparser: incomplete parse of SetResult [" (u8data-length attributes) "]")
    )
  ))

(define (ivueparser:parseCmdConfirmedSet buf)
  (ivueparser:parseSetResult buf)
)
;;eof
