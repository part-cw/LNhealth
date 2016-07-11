;; Philips Intellivue Parser
;; Matthias Görges, 2016

;; Parse Confirmed Action
(include "ro_confirmedaction.scm")
;; Parse Event Report
(include "ro_eventreport.scm")

;; RemoteOperationInvoke tree
(define (ivueparser:parseROIVapdu buf)
  (let ((invoke_id (u8data-u16 (subu8data buf 0 2)))
        (command_type (u8data-u16 (subu8data buf 2 4)))
        (len (u8data-u16 (subu8data buf 4 6))))
    (if (fx= len (fx- (u8data-length buf) 6))
      command_type
      -1
    )
  ))

(define (ivueparser:parseRemoteOperationInvoke buf)
  (let ((command_type (ivueparser:parseROIVapdu buf)))
    (cond
      ((fx= command_type CMD_EVENT_REPORT)
        (ivueparser:parseCmdEventReport (u8data-skip buf 6)))
      ((fx= command_type CMD_CONFIRMED_EVENT_REPORT)
        (ivueparser:parseCmdConfirmedEventReport (u8data-skip buf 6)))
      ((fx= command_type CMD_GET)
        (ivueparser:parseCmdGet (u8data-skip buf 6)))
      ((fx= command_type CMD_SET)
        (ivueparser:parseCmdSet (u8data-skip buf 6)))
      ((fx= command_type CMD_CONFIRMED_SET)
        (ivueparser:parseCmdConfirmedSet (u8data-skip buf 6)))
      ((fx= command_type CMD_CONFIRMED_ACTION)
        (ivueparser:parseCmdConfirmedAction (u8data-skip buf 6)))
      (else
        (set! ivueparser:error #t)
        (ivueparser:log 2 "ivueparser: unknown command_type:" command_type))
    )
  ))
;;eof
