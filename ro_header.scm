;; Philips Intellivue Parser
;; Matthias Görges, 2016
(include "parse_helpers.scm")

;; RemoteOperationInvoke
(include "ro_invoke.scm")

;; Session Header and Remote Operation Header
(define (ivueparser:parseSPdu buf)
  (let ((session_id (u8data-u16 (subu8data buf 0 2)))
        (p_context_id (u8data-u16 (subu8data buf 2 4))))
    session_id
  ))
(define (ivueparser:parseROapdus buf)
  (let ((ro_type (u8data-u16 (subu8data buf 4 6)))
        (len (u8data-u16 (subu8data buf 6 8))))
    (if (fx= len (fx- (u8data-length buf) 8))
      ro_type
      -1
    )
  ))

;; RemoteOperationHeader tree
(define (ivueparser:parseRemoteOperationHeader buf ro_type)
  (cond
    ((fx= ro_type ROIV_APDU)
      (ivueparser:parseRemoteOperationInvoke buf))
    ((fx= ro_type RORS_APDU)
      (set! ivueparser:islinked #f)
      (ivueparser:parseRemoteOperationResult buf))
    ((fx= ro_type ROER_APDU)
      (ivueparser:parseRemoteOperationError buf))
    ((fx= ro_type ROLRS_APDU)
      (set! ivueparser:islinked #t)
      (ivueparser:parseRemoteOperationLinkedResult buf 6))
    (else
      (set! ivueparser:error #t)
      (ivueparser:log 2 "ivueparser: unknown ro_type:" ro_type))
  ))

;; The main parsing function
(define (ivueparser:parsedata buf)
  (let ((session_id (ivueparser:parseSPdu buf))
        (ro_type (ivueparser:parseROapdus buf)))
    (cond
      ((fx= session_id #xE100)
        (ivueparser:parseRemoteOperationHeader (u8data-skip buf 8) ro_type)
      )
      (else
        (set! ivueparser:error #t)
        (ivueparser:log 2 "ivueparser: unknown ro_type:" ro_type)
      )
    )
  ))

;; Public parsing function hook
(define (ivueparser store data)
  (set! ivueparser:store store)
  (ivueparser:parsedata data)
)

;;eof
