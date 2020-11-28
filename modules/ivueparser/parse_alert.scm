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

(define (ivueparser:parseDevAlarmList buf palarm?)
  (let* ((count (u8data-u16 (subu8data buf 0 2)))
         (len (u8data-u16 (subu8data buf 2 4)))
         (al_prefix (string-append (if palarm? "p" "t") "_alarm"))
         (al_lst (string-append al_prefix "_lst")))
    (store-clear! ivueparser:store (store-ref ivueparser:store al_lst))
    (store-set! ivueparser:store al_lst '())
    (let loop ((n 0)(p (u8data-skip buf 4)))
      (if (fx= n count)
        p
        (loop (fx+ n 1) (ivueparser:parseDevAlarmEntry p n al_prefix))
      )
    )
  ))

(define (ivueparser:parseDevAlarmEntry buf al_ct prefix)
  (let ((al_source (u8data-u16 (subu8data buf 0 2)))
        (al_code (u8data-u16 (subu8data buf 2 4)))
        (al_type (u8data-u16 (subu8data buf 4 6)))
        (al_state (u8data-u16 (subu8data buf 6 8)))
        (managed_object (ivueparser:parseManagedObjectId (subu8data buf 8 14)))
        (alert_info_id (u8data-u16 (subu8data buf 14 16)))
        (len (u8data-u16 (subu8data buf 16 18))))
    (let* ((name0 (ivueparser:findphys al_source (fx+ #x20000 al_source)))
           (name (if name0 name0 "???"))
           (priostr (if (= al_type LOW_PRI_P_AL) "*" (if (= al_type MED_PRI_P_AL) "**"
                      (if (= al_type HI_PRI_P_AL) "***" ""))))
           (msg0 (car (table-ref ivueparser:alarmtable al_code '(""))))
           (msg (string-replace-substring msg0 "XXXXXX" name))
           (al_lst_name (string-append prefix "_lst"))
           (al_lst (store-ref ivueparser:store al_lst_name '()))
           (al_ct_name (string-append prefix (number->string al_ct))))
      (store-set! ivueparser:store al_ct_name msg "ivue")
      (store-set! ivueparser:store al_lst_name (append al_lst (list al_ct_name)))
      ;; New alarms
      (if (and (fx= al_state 8) (fx> (string-length msg) 0))
        (store-event-add ivueparser:store 0 (store-ref ivueparser:store "location" ivueparser:store) msg))
      ;; MMS Disconnection
      (if (and (fx= al_state 8) (fx= al_source NOM_OBJ_MMS) (fx= al_code 6257)) ;;NOM_EVT_STAT_DISCONN+1
        (begin
          (store-event-add ivueparser:store 0 (store-ref ivueparser:store "location" ivueparser:store) "MMS Removed")
;;          (store-set! ivueparser:store "CaseEndPending" #t "ivue")
;;          (store-clear! ivueparser:store "CaseStartPending")
        )
      )

      ;; Parse alarm internals
      (cond
        ((fx= alert_info_id STR_ALMON_INFO)
          (ivueparser:parseStrAlMonInfo (subu8data buf 18 (fx+ len 18)) al_ct_name))
        ((fx= alert_info_id GEN_ALMON_INFO)
          (ivueparser:parseAlMonGenInfo (subu8data buf 18 (fx+ len 18)) al_ct_name))
        (else (ivueparser:log 1 "ivueparser: unknown alert_info_id: " alert_info_id))
      )
    )
    (u8data-skip buf (fx+ len 18))
  ))

(define (ivueparser:parseAlMonGenInfo buf al_ct_name)
  (let ((al_inst_no (u8data-u16 (subu8data buf 0 2)))
        (al_text (u8data-u32 (subu8data buf 2 6)))
        (priority (u8data-u16 (subu8data buf 6 8)))
        (flags (u8data-u16 (subu8data buf 8 10))))
    (store-set! ivueparser:store (string-append al_ct_name "_prio") priority)
    (u8data-skip buf 10)
  ))

(define (ivueparser:parseStrAlMonInfo buf al_ct_name)
  (let* ((AlMonGenInfo (ivueparser:parseAlMonGenInfo buf al_ct_name))
         (str (ivueparser:parseString AlMonGenInfo)))
    #f
  ))

(define (ivueparser:parseDeviceAlertCondition buf)
  (let ((device_alert_state (u8data-u16 (subu8data buf 0 2)))
        (al_stat_chg_cnt (u8data-u16 (subu8data buf 2 4)))
        (max_p_alarm (u8data-u16 (subu8data buf 4 6)))
        (max_t_alarm (u8data-u16 (subu8data buf 6 8)))
        (max_aud_alarm (u8data-u16 (subu8data buf 8 10))))
    (store-set! ivueparser:store "max_p_alarm" max_p_alarm "ivue")
    (store-set! ivueparser:store "max_t_alarm" max_t_alarm "ivue")
    (store-set! ivueparser:store "max_aud_alarm" max_aud_alarm "ivue")
  ))

;; eof
