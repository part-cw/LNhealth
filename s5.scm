;; Protocol for GE/Datex S5
;; ----------

(define s5:debuglevel 1)
(define (s5:log level . x) (if (>= s5:debuglevel level) (apply log-system (append (list "s5: ") x))))

(define s5:error #f)
(define s5:data #f)

(define (s5:decodemsg ibuf)
  (let ((buflen (u8vector-length ibuf)))
    (if (not (and (fx= (u8vector-ref ibuf 0) #x7e)
                  (fx= (u8vector-ref ibuf (- buflen 1))) #x7e))
      (begin (log-error "s5: invalid input frame") #f)
      (let loop ((i 1)(obuf (u8vector))(escaped #f))
        (if (fx= i (fx- buflen 1)) obuf
          (let* ((c (u8vector-ref ibuf i))
                 (newescaped (if (fx= c #x7D) #t #f)))
             (loop (fx+ i 1) (u8vector-append obuf
               (if newescaped (u8vector)
                 (if escaped (u8vector (if (fx= c #x5e) #x7e #x7d))
                   (u8vector c)))) newescaped)))))))

(define (s5:escapechar msg)
  (let ((mlen (u8vector-length msg)))
    (let loop ((i 0)(res (u8vector)))
      (if (fx= i mlen) res
        (let ((c (u8vector-ref msg i)))
          (loop (fx+ i 1) (u8vector-append res
           (cond ((fx= c #x7e) (u8vector #x7D #x5e))
                 ((fx= c #x7d) (u8vector #x7D #x5d))
                 (else (u8vector c))))))))))

(define (s5:calccsum msg)
  (let ((mlen (u8vector-length msg)))
    (let loop ((i 0)(t 0))
      (if (fx= i mlen) t ;;(u8vector t)
        (loop (fx+ i 1) (bitwise-and 
          (fx+ t (u8vector-ref msg i)) #xff))))))

;; return the senders checksum
;; the decoding has already been done, so csum is last byte
(define (s5:transmittedcsum buf)
  (let* ((buflen (u8vector-length buf))
         (csum (u8vector-ref buf (fx- buflen 1))))
    csum))

;; new version using cache
(define (s5:recv store dev)
  (set! s5:error #t)
  (let* ((cache (serial-cache-read dev))
         (rawdata (if cache (list->u8vector (map char->integer (string->list cache))) #f)))
     (if rawdata (begin
       (set! s5:error #f)
       (set! s5:data #f)
       (let* ((tmpdata (s5:decodemsg rawdata))
              (tmpdatalen (u8vector-length tmpdata))
              (tmpdatanosum (subu8vector tmpdata 0 (fx- tmpdatalen 1)))
              (csum1   (s5:calccsum tmpdatanosum))
              (csum2   (s5:transmittedcsum tmpdata)))
       (if (not (fx= csum1 csum2)) (begin
            (log-error (string-append "s5: error: bad crc check"
                 (number->string csum1) "!=" (number->string csum2) "\n"))
         ) (begin
             (set! s5:data (u8vector->u8data tmpdatanosum))
   )))))))

(define (s5:send dev msg)
  (let* ((csum (u8vector (s5:calccsum msg)))
         (msglist (u8vector->list (u8vector-append
           (u8vector #x7e) (s5:escapechar msg)
           (s5:escapechar csum) (u8vector #x7e)))))
    (let loop ((m msglist)(err #f))
      (if err (set! s5:error #t)
        (if (fx> (length m) 0) (begin
          (serial-writechar dev (car m))
          (loop (cdr m) (if (or (serial-error) 
             (serial-timeout)) #t #f))))))))

;; ----------

(define (s5-init store instance)
  (let ((runlevel (instance-refvar store instance "RunLevel" 0))
        (dev      (instance-refvar store instance "Device" #f)))
   (cond
     ((fx= runlevel 0)
       (let* ((devname (instance-refvar store instance "Port" #f))
          (baudrate 19200) (databits 8) (parity 2) (stopbits 1)
          (newdev (serial-try devname baudrate databits parity stopbits #f)))
          (if newdev (begin 
            (instance-setvar! store instance "Device" newdev) 1) 0)))
      ((fx= runlevel 1) 
         ;; ask for trends
         ;;(instance-setvar! store instance "Update" .01)
         (store-clear! store "gotwaveforms?")
         (s5:send dev (u8vector #x31 
              0 0 0 0 0
              0 0 0 0 0 
              0 0 0 0 0 
              0 0 0 0 0 #xff
              0 0 0 0 0 0
              0 0 0 0 0 0
              0 0 0 0 0 0 1 
              5 0   ;; interval in seconds
              #x0e 0 0 0 0 0)) 
            (s5:log 2 "runlevel 1")
          2)
      ((fx= runlevel 2)
         (if (instance-refvar store instance "Waveforms" #f) 
           (instance-setvar! store instance "Update" 0.05)
           (instance-setvar! store instance "Update" 1.0))
         (serial-cache-setup dev #x7e #x7e)
         (s5:recv store dev)
         (s5:log 2 "runlevel 3 error=" s5:error)
         (if (not (store-timedrefsec store "timestamp" 300)) (begin
           (store-event-add store 0 "Not connected - check cable?" instance)
           (store-set! store "MonitorAlarm" (list 0 "Not connected - check cable?" "Monitor"))
           (store-set! store "timestamp" 0. "s5")
         ))
         (if (and (not s5:error) s5:data) (begin 
           (instance-setvar! store instance "LastGood" ##now)
           (s5:log 1 "connect")
           (store-event-add store 0 "Connect" instance)
           (store-set! store "MonitorAlarm" '(0 "Connect" "Monitor"))
           16) (begin
           ;; try to purge any fragmented data
           (s5:recv store dev) 0)))
      )))

(define (s5-run store instance)
  (let ((runlevel (instance-refvar store instance "RunLevel" 0))
        (dev      (instance-refvar store instance "Device" #f)) )
  (if (fx= runlevel 16) (begin
    (let loop () 
      (s5:recv store dev) 
      (if (or s5:error (not s5:data))
        ;; we failed 
        (let ((lastgood (instance-refvar store instance "LastGood" 0.)))
          (if (> (- ##now lastgood) 15) (begin
              ;; prevent audible alarms outside of case
              (s5:log 1 "disconnect")
              (store-event-add store (if (store-ref store "CaseID" #f) 4 0) "Disconnect" instance)
              (store-set! store "MonitorAlarm" (list (if (store-ref store "CaseID" #f) 4 0) "Disconnect" "Monitor"))
              ;; Clear all old values
              (store-clear! store (map car (store-listcat store "s5")))
              ;; See if we can get a new monitor connect?
              (instance-setvar! store instance "RunLevel" 0)
            )))
       ;; we are good 
       (begin 
         (instance-setvar! store instance "LastGood" ##now)
         (s5parser store s5:data)

         ;; 20130227: repeating waveform request
         (let ((wneeded (instance-refvar store instance "Waveforms" #f))
               (wpresent (store-ref store "gotwaveforms?"))
               (wlast (instance-refvar store instance "LastWaveformRequest" 0.))) 
           (if (and wneeded (not wpresent) (fl> (fl- ##now wlast) 10.)) (begin
             (s5:log 2 "sending waveform request")
             (let ((req (u8vector
                    #x48 0 0 0 0 0 0 0 0 0 0 0 0 0 1
                    0 0 0 0 0 0 #xff 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0
                    ;; Up to 8 waveform types here. terminate less than eight requests with #xff
                    ;; (#xff is not necessary, despite the manual saying so)
                    0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0))
                  (waves (if (list? wneeded) wneeded (list "ECG1" "PLETH" "CO2"))))
               ;; See s5parser.scm for list of waveform types. 1 8 9 works well, more seems to cause data loss.
               (let loop ((i 0))
                 (if (or (fx= i (length waves)) (fx= i 8))
                   #t
                   (begin
                     (u8vector-set! req (fx+ i 44) (s5parser:getwaveformtype (list-ref waves i)))
                     (loop (fx+ i 1))
                   )
                 )
               )
               (s5:send dev req)
             )
             (instance-setvar! store instance "LastWaveformRequest" ##now)
           )))

         ;; deal with missing timeout of bp values
         (let ((new_bpsys (store-ref store "nibp_sys" #f))
               (new_bpdia (store-ref store "nibp_dia" #f))
               (old_bpsys (store-ref store "BPsys" #f))
               (old_bpdia (store-ref store "BPdia" #f)))
           (if (and new_bpsys (not (equal? new_bpsys old_bpsys)))
              (store-set! store "BPsys" new_bpsys))
           (if (and new_bpdia (not (equal? new_bpdia old_bpdia)))
              (store-set! store "BPdia" new_bpdia))
           (store-clearexpired! store 600 "BPsys"
             (lambda (store id) (if (store-ref store "CaseID" #f) (begin
               (store-event-add store 3 (string-append id "Not updating") instance)
               (store-set! store "MonitorAlarm" (list
                 (if (store-ref store "CaseID" #f) 3 0)
                   (string-append id "Not updating") "Monitor"))
             ))))
           (store-clearexpired! store 600 "BPdia"))

         ;; 20120110: zap values in case of a monitor disconnect
         (let loop ((params s5parser:physdatavalues_basic))
           (if (> (length params) 0) (begin
             (if (not (member (car params) '("nibp_sys" "nibp_dia")))
               (store-clearexpired! store 60 (car params)))
             (loop (cdr params)))))

         (store-set! store "SP" (store-ref store "spo2" #f))

         (loop)
       )))
    ))))

(define (s5-close store instance)
  (let ((dev      (instance-refvar store instance "Device" #f)))  
    (s5:log 2 "close")
    (if dev (begin (serial-close dev)
       (instance-setvar!  store instance "Device" #f)))))

;; eof
