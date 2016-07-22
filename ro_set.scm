;; Philips Intellivue Parser
;; Matthias Görges, 2016

;; Parse Set
(define (ivueparser:parseCmdSet buf)
  (let* ((managed_object (ivueparser:parseManagedObjectId buf))
         (context_id (cadr managed_object))
         (attributeList (ivueparser:parseAttributeList context_id (u8data-skip buf 6))))
    (if (fx= (u8data-length attributeList) 0)
      #t
      (ivueparser:log 1 "ivueparser: incomplete parse of CmdSet [" (u8data-length attributeList) "]")
    )
  ))

;;eof
