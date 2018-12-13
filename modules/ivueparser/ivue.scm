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

;; Protocol for Philips Intellivue
;; Christian Leth Petersen 03/2009
;; ported and expanded Matthias Görges, 2012
(include "ivue-encoders.scm")
(include "ivue-request.scm")

(define ivue:debuglevel 1)
(define (ivue:log level . x) (if (>= ivue:debuglevel level) (apply log-system (append (list "ivue: ") x))))
(define ivue:data #f)
(define ivue:lastchecked 0.)
(define ivue:attempt 0)

(define (ivue:send dev msg debug)
  (if debug
    (ivue:log 0 "S: " (map (lambda (l) (number->string l 16)) (u8vector->list msg)))
  )
  (let loop ((m (u8vector->list msg))(err #f))
    (if err
      (set! ivue:error #t)
      (if (fx> (length m) 0) (begin
        (serial-writechar dev (car m))
        (loop (cdr m) (or (serial-error) (serial-timeout)))
      ))
    )
  )
)

(define (ivue:recv dev debug)
  (set! ivue:data #f)
  (set! ivue:error #t)
  (let* ((cache (serial-cache-read dev))
         (data (if cache (list->u8vector (map char->integer (string->list cache))) #f))
         (parseddata (if data (ivueparser:parseframe (u8vector->u8data data)) #f)))
    (if (u8data:sane? parseddata) (begin
      (set! ivue:data parseddata)
      (set! ivue:error #f)
      (if debug
        (ivue:log 0 "R: " (map (lambda (l) (number->string l 16)) (u8vector->list (u8data->u8vector ivue:data))))
      )
    ))
  )
)

(define (ivue-init store instance)
  (let ((runlevel (instance-refvar store instance "RunLevel" 0))
        (dev (instance-refvar store instance "Device" #f))
        (debug (instance-refvar store instance "Debug" #f)))
    (cond
      ((fx= runlevel 0)
        (let* ((devname (instance-refvar store instance "Port" "NA"))
               (baudrate (instance-refvar store instance "Baud" 115200))
               (databits (instance-refvar store instance "DataBits" 8))
               (parity (instance-refvar store instance "Parity" 0))
               (stopbits (instance-refvar store instance "StopBits" 1))
               (newdev (serial-try devname baudrate databits parity stopbits #f)))
          ;; If serial-try was successful return new runlevel 1
          (if newdev
            (begin
              (if debug (ivue:log 0 (string-append "connected to " devname)))
              (instance-setvar! store instance "Device" newdev)
              (ivue:send newdev (ivue:arequest) debug)
              (serial-cache-setup newdev #xC0 #xC1) ;; Philips serial is framed between 0xc0 and 0xc1
              (set! ivue:attempt 0)
              1
            )
            0
          )
        )
      )
      ((fx= runlevel 1)
        ;; Test if Association Response was positive or not
        (ivue:recv dev debug)
        (if (and ivue:data (fx= (u8data-ref ivue:data 0) #x0E))
          (begin
            (ivue:log 1 "connect")
            (store-event-add store 0 "Connect" instance)
            (store-set! store "MonitorAlarm" '(0 "Connect" "Monitor"))
            (set! ivue:start ##now)
            2
          )
          (begin
            (ivue:log 1 "refused")
            (store-event-add store 0 "Refused" instance)
            (store-set! store "MonitorAlarm" '(0 "Refused" "Monitor"))
            (set! ivue:attempt (fx+ ivue:attempt 1))
            (if (fx= ivue:attempt 3)
              (begin
               (ivue:send dev (ivue:release) debug)
                0
              )
              1
            )
          )
        )
      )
      ((fx= runlevel 2)
        (ivue:recv dev debug)
        (if (and ivue:data
             (fx= (u8data-ref ivue:data 0) #xE1)
             (fx= (u8data-ref ivue:data 5) ROIV_APDU)
             (fx= (u8data-ref ivue:data 9) CMD_CONFIRMED_EVENT_REPORT))
          (begin
            (ivue:send dev (ivue:mresult ivue:data) debug)
            (instance-setvar! store instance "LastGood" ##now)
            3
          )
          (begin
            (if ivue:data (ivueparser store ivue:data))
            2
          )
        )
      )
      ((fx= runlevel 3)
        (ivue:send dev (ivue:polltrends) debug)
        (if debug (ivue:log 0 "Trends requested"))
        (if (instance-refvar store instance "Waveforms" #f)
          (begin
            (ivue:send dev (ivue:pollwaves) debug)
            (if debug (ivue:log 0 "Waveforms requested"))
            (instance-setvar! store instance "Update" 0.05)
          )
          (instance-setvar! store instance "Update" 0.5)
        )
        16
      )
      (else runlevel)
    )
  )
)

(define (ivue-run store instance)
  (let ((runlevel (instance-refvar store instance "RunLevel" 0))
        (dev (instance-refvar store instance "Device" #f))
        (debug (instance-refvar store instance "Debug" #f)))
    (if (fx= runlevel 16) (begin
      (let loop ()
        (ivue:recv dev debug)
        (if (or ivue:error (not ivue:data))
          ;; we failed
          (let ((lastgood (instance-refvar store instance "LastGood" 0.)))
            (if (> (- ##now lastgood) 15) (begin
              ;; prevent audible alarms outside of case
              (ivue:log 1 "disconnect")
              (store-event-add store (if (store-ref store "CaseID" #f) 4 0) "Disconnect" instance)
              (store-set! store "MonitorAlarm" (list (if (store-ref store "CaseID" #f) 4 0) "Disconnect" "Monitor"))
              ;; Clear all old values
              (store-clear! store (map car (store-listcat store "ivue")))
              (store-clear! store (map car (store-listcat store "ivue_display")))
              ;; See if we can get a new monitor connect?
              (instance-setvar! store instance "RunLevel" 0)
            ))
          )
          ;; we are good
          (begin
            (instance-setvar! store instance "LastGood" ##now)
            (if (fx= (u8data-ref ivue:data 0) #x19)
              (let ((err (string-append "Connection died after " (number->string (fl- ##now ivue:start)) " sec")))
                (ivue:log 1 err)
                (store-event-add store (if (store-ref store "CaseID" #f) 4 0) err instance)
                (store-set! store "MonitorAlarm" (list (if (store-ref store "CaseID" #f) 4 0) err "Monitor"))
                (instance-setvar! store instance "RunLevel" 0)
              )
              (ivueparser store ivue:data)
            )
            ;; This gets new trends every 1 sec - also keeps waveform export alive.
            (if (fl> (fl- ##now ivue:lastchecked) 1.) (begin
              (if debug (ivue:log 1 (store-listcat store "ivue")))
              (ivue:send dev (ivue:polltrends) debug)
              (ivue:send dev (ivue:pollalarms) debug)
              (set! ivue:lastchecked ##now)
            ))
            (loop)
          )
        )
      )
    ))
  )
)

(define (ivue-close store instance)
  (let ((dev (instance-refvar store instance "Device" #f))
        (debug (instance-refvar store instance "Debug" #f)))
    (if dev
      (begin
        ;;(ivue:send dev (ivue:release) debug)
        (serial-close dev)
        (instance-setvar! store instance "Device" #f)
      )
    )
  )
)

;;eof
