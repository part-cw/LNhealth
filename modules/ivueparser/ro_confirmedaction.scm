#|
lnHealth - Health related apps for the LambdaNative framework
Copyright (c) 2009-2018, University of British Columbia
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

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
        (ivueparser:parse0c11 (u8data-skip buf 14)))
      (else
        (ivueparser:log 1 "ivueparser: unknown action_type: " action_type))
    )
  ))

;; Parsing the magic package, which contains the patient admission one
(define (ivueparser:parse0c11 buf)
  (let ((package_type (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (cond
      ((fx= package_type 1)
        (ivueparser:parseparse0c11_01 (u8data-skip buf 4)))
      ((fx= package_type 2)
        (ivueparser:parseparse0c11_02 (u8data-skip buf 4)))
      (else
        (ivueparser:log 1 "ivueparser: unknown 0xc11 package_type: " package_type))
    )
  ))
(define (ivueparser:parseparse0c11_01 buf)
  (ivueparser:parseAttributeList 0 (u8data-skip buf 8))
)
(define (ivueparser:parseparse0c11_02 buf)
  (ivueparser:parseAttributeList 0 (u8data-skip buf 6))
)

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
