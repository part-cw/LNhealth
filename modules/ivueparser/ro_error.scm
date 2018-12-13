;; Philips Intellivue Parser
;; Matthias Görges, 2016

;; RemoteOperationInvoke tree
(define (ivueparser:parseROERapdu buf)
  (let ((invoke_id (u8data-u16 (subu8data buf 0 2)))
        (error_value (u8data-u16 (subu8data buf 2 4)))
        (len (u8data-u16 (subu8data buf 4 6))))
    (if (fx= len (fx- (u8data-length buf) 6))
      error_value
      -1
    )
  ))

(define (ivueparser:parseRemoteOperationError buf)
  (let ((error_value (ivueparser:parseROERapdu buf)))
    (cond
      ((fx= error_value NO_SUCH_OBJECT_CLASS)
        #f)
      ((fx= error_value NO_SUCH_OBJECT_INSTANCE)
        #f)
      ((fx= error_value ACCESS_DENIED)
        #f)
      ((fx= error_value GET_LIST_ERROR)
        #f)
      ((fx= error_value SET_LIST_ERROR)
        #f)
      ((fx= error_value NO_SUCH_ACTION)
        #f)
      ((fx= error_value PROCESSING_FAILURE)
        #f)
      ((fx= error_value INVALID_ARGUMENT_VALUE)
        #f)
      ((fx= error_value INVALID_SCOPE)
        #f)
      ((fx= error_value INVALID_OBJECT_INSTANCE)
        #f)
      (else
        (ivueparser:log 1 "ivueparser: unknown error_value: " error_value))
    )
  ))
;;eof
