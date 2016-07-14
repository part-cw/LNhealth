;; Philips Intellivue Parser
;; Matthias Görges, 2016

;; Parse Confirmed Set
(define (ivueparser:parseSetResult buf)
  (let ((managed_object (ivueparser:parseManagedObjectId buf))
        (attributes (u8data-skip buf 6)))
    (ivueparser:parseAttributeList obj_handle attributes)
  ))

(define (ivueparser:parseCmdConfirmedSet buf)
  (ivueparser:parseSetResult buf)
)
;;eof
