;; Philips Intellivue Parser
;; Matthias Görges, 2016
(include "parse_attributelist.scm")


;; Parse Action Argument
(define (ivueparser:parseActionArgument buf)
  (let ((managed_object (ivueparser:parseManagedObjectId buf))
        (scope (u8data-u32 (subu8data buf 6 10))) ;;Always 0
        (action_type (u8data-u16 (subu8data buf 10 12)))
        (len (u8data-u16 (subu8data buf 12 14))))
    (if (fx= len (fx- (u8data-length buf) 14))
      action_type
      -1
    )
  ))

(define (ivueparser:parseCmdConfirmedAction buf)
  (let ((action_type (ivueparser:parseActionArgument buf)))
    (cond
      ((fx= action_type NOM_ACT_POLL_MDIB_DATA)
        (ivueparser:parsePollMdibDataReq (u8data-skip buf 14)))
      ((fx= action_type NOM_ACT_POLL_MDIB_DATA_EXT)
        (ivueparser:parsePollMdibDataReqExt (u8data-skip buf 14)))
      ((fx= action_type #x0c11)
        (ivueparser:log 0 "ivueparser: demographics package:")
        (ivueparser:log 0 (map (lambda (l) (number->string l 16))(u8vector->list (u8data->u8vector (u8data-skip buf 14)))))
      )
      (else
        (ivueparser:log 1 "ivueparser: unknown action_type: " action_type))
    )
  ))

;; PollMdibDataReq
(define (ivueparser:parsePollMdibDataReq buf)
  (let ((poll_number (u8data-u16 (subu8data buf 0 2)))
        (polled_obj_type (ivueparser:parseTYPE (subu8data buf 2 6)))
        (polled_attr_grp (u8data-u16 (subu8data buf 6 8))))
    #t
  ))

;; PollMdibDataReqExt
(define (ivueparser:parsePollMdibDataReqExt buf)
  (let* ((poll_number (u8data-u16 (subu8data buf 0 2)))
         (polled_obj_type (ivueparser:parseTYPE (subu8data buf 2 6)))
         (polled_attr_grp (u8data-u16 (subu8data buf 6 8)))
         (context_id 0) ;; We don't know this
         (poll_ext_attr (ivueparser:parseAttributeList context_id (u8data-skip buf 8))))
    (if (fx= (u8data-length poll_ext_attr) 0)
      #t
      (ivueparser:log 1 "ivueparser: incomplete parse of PollMdibDataReqExt [" (u8data-length poll_ext_attr) "]")
    )
  ))

;; Parse Confirmed Action
(define (ivueparser:parseActionResult buf)
  (let ((managed_object (ivueparser:parseManagedObjectId buf))
        (action_type (u8data-u16 (subu8data buf 6 8)))
        (len (u8data-u16 (subu8data buf 8 10))))
    (if (fx= len (fx- (u8data-length buf) 10))
      action_type
      -1
    )
  ))

(define (ivueparser:parseCmdConfirmedActionResult buf)
  (let ((action_type (ivueparser:parseActionResult buf)))
    (cond
      ((fx= action_type NOM_ACT_POLL_MDIB_DATA)
        (ivueparser:parsePollMdibDataReply (u8data-skip buf 10)))
      ((fx= action_type NOM_ACT_POLL_MDIB_DATA_EXT)
        (ivueparser:parsePollMdibDataReplyExt (u8data-skip buf 10)))
      (else
        (ivueparser:log 1 "ivueparser: unknown action_type: " action_type))
    )
  ))

;; PollMdibDataReply
(define (ivueparser:parsePollMdibDataReply buf)
  (let ((poll_number (u8data-u16 (subu8data buf 0 2)))
        (rel_time_stamp (ivueparser:parseRelativeTime (subu8data buf 2 6)))
        ;;(abs_time_stamp (ivueparser:parseAbsoluteTime "poll_data_result_timestamp" (subu8data buf 6 14)))
        ;;(polled_obj_type (ivueparser:parseTYPE (subu8data buf 14 18)))
        (polled_attr_grp  (u8data-u16 (subu8data buf 18 20)))
        (poll_info_list (ivueparser:parsePollInfoList (u8data-skip buf 20))))
    (if (fx= (u8data-length poll_info_list) 0)
      #t
      (ivueparser:log 1 "ivueparser: incomplete parse of PollMdibDataReply [" (u8data-length poll_info_list) "]")
    )
  ))

;; PollMdibDataReplyExt
(define (ivueparser:parsePollMdibDataReplyExt buf)
  (let ((poll_number (u8data-u16 (subu8data buf 0 2)))
        (sequence_no (u8data-u16 (subu8data buf 2 4)))
        (rel_time_stamp (ivueparser:parseRelativeTime (subu8data buf 4 8)))
        ;;(abs_time_stamp (ivueparser:parseAbsoluteTime "poll_data_result_timestamp" (subu8data buf 8 16)))
        ;;(polled_obj_type (ivueparser:parseTYPE (subu8data buf 16 20)))
        (polled_attr_grp  (u8data-u16 (subu8data buf 20 22)))
        (poll_info_list (ivueparser:parsePollInfoList (u8data-skip buf 22))))
    (if (fx= (u8data-length poll_info_list) 0)
      #t
      (ivueparser:log 1 "ivueparser: incomplete parse of PollMdibDataReplyExt [" (u8data-length poll_info_list) "]")
    )
  ))

;; PollInfoList
(define (ivueparser:parsePollInfoList buf)
  (let ((count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(p (u8data-skip buf 4)))
      (if (fx= n count)
        p
        (loop (fx+ n 1) (ivueparser:parseSingleContextPoll p))
      )
    )
  ))

;; SingleContextPoll
(define (ivueparser:parseSingleContextPoll buf)
  (let ((context_id (u8data-u16 (subu8data buf 0 2)))
        (count (u8data-u16 (subu8data buf 2 4)))
        (len (u8data-u16 (subu8data buf 4 6))))
    (let loop ((n 0)(p (u8data-skip buf 6)))
      (if (fx= n count)
        p
        (loop (fx+ n 1) (ivueparser:parseObservationPoll p))
      )
    )
  ))

;; ObservationPoll
(define (ivueparser:parseObservationPoll buf)
  (let ((obj_handle (u8data-u16 (subu8data buf 0 2)))
        (attributes (u8data-skip buf 2)))
    (ivueparser:parseAttributeList obj_handle attributes)
  ))

;;eof
