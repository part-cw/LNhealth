;; Philips Intellivue Parser
;; Matthias Görges, 2016

(define (ivueparser:parseRorlsId buf)
  (let ((state (u8data-u8 (subu8data buf 0 1)))
        (count (u8data-u8 (subu8data buf 1 2))))
    (list state count)
  ))

(define (ivueparser:parseROLRSapdu buf)
  (let ((linked_id (ivueparser:parseRorlsId buf))
        (invoke_id (u8data-u16 (subu8data buf 2 4)))
        (command_type (u8data-u16 (subu8data buf 4 6)))
        (len (u8data-u16 (subu8data buf 6 8))))
    (if (fx= len (fx- (u8data-length buf) 8))
      command_type
      -1
    )
  ))

(define (ivueparser:parseRemoteOperationLinkedResult buf)
  (let ((command_type (ivueparser:parseROLRSapdu buf)))
    (cond
      ((fx= command_type CMD_EVENT_REPORT)
        (ivueparser:parseCmdEventReport (u8data-skip buf 8)))
      ((fx= command_type CMD_CONFIRMED_EVENT_REPORT)
        (ivueparser:parseCmdConfirmedEventReport (u8data-skip buf 8)))
      ((fx= command_type CMD_GET)
        (ivueparser:parseCmdGet (u8data-skip buf 8)))
      ((fx= command_type CMD_SET)
        (ivueparser:parseCmdSet (u8data-skip buf 8)))
      ((fx= command_type CMD_CONFIRMED_SET)
        (ivueparser:parseCmdConfirmedSet (u8data-skip buf 8)))
      ((fx= command_type CMD_CONFIRMED_ACTION)
        (ivueparser:parseCmdConfirmedActionResult (u8data-skip buf 8)))
      (else
        (ivueparser:log 1 "ivueparser: unknown command_type: " command_type))
    )
  ))
;; eof
