;; Philips Intellivue Parser
;; Matthias Görges, 2016

;; Parse Set
(define (ivueparser:parseCmdSet buf)
  (let* ((managed_object (ivueparser:parseManagedObjectId buf))
         (scope (u8data-u32 (subu8data buf 6 10))) ;;Always 0
         (context_id (cadr managed_object))
         (modificationList (ivueparser:parseAttributeList context_id (u8data-skip buf 10))))
    (if (fx= (u8data-length modificationList) 0)
      #t
      (ivueparser:log 1 "ivueparser: incomplete parse of CmdSet [" (u8data-length modificationList) "]")
    )
  ))

;;eof
