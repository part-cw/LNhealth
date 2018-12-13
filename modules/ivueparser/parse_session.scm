;; Philips Intellivue Parser
;; Chris Petersen, 2011
;; Matthias Görges, 2016
(include "ro_header.scm")

(define (ivueparser:parseSessionHeader buf)
  (let* ((type (u8data-u8 (subu8data buf 0 1)))
         (len0 (u8data-u8 (subu8data buf 1 2)))
         (len1 (u8data-u16 (subu8data buf 2 4)))
         (len (if (fx= len0 #xff) len1 len0))
         (data (if (fx= len0 #xff) (u8data-skip buf 4) (u8data-skip buf 2))))
    (cond
      ((fx= type CN_SPDU_SI) ;; Session Connect
        (ivueparser:log 2 "ivueparser: connect from monitor") #f)
      ((fx= type AC_SPDU_SI) ;; Session Accept
        (ivueparser:log 2 "ivueparser: accept from monitor") #f)
      ((fx= type RF_SPDU_SI) ;; Session Refuse
        (ivueparser:log 2 "ivueparser: refuse from monitor") #f)
      ((fx= type FN_SPDU_SI) ;; Session Finish
        (ivueparser:log 2 "ivueparser: fisish from monitor") #f)
      ((fx= type DN_SPDU_SI) ;; Session Disconnect
        (ivueparser:log 2 "ivueparser: disconnect from monitor") #f)
      ((fx= type AB_SPDU_SI) ;; Session Abort
        (ivueparser:log 2 "ivueparser: abort from monitor") #f)
      ((fx= type #xE1) ;; Data Export Protocol
        (ivueparser:parsedata buf))
      (else (ivueparser:log 2 "ivueparser: parsing error") #f)
    )
  ))

;; eof
