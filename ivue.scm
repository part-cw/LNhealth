;; Protocol for Philips Intellivue
;; Christian Leth Petersen 03/2009
;; ported and expanded Matthias Görges, 2012
(include "ivue-encoders.scm")
(include "ivue-request.scm")

(define ivue:data #f)
(define ivue:lastchecked 0.)
(define ivue:attempt 0)

(define (ivue:send dev msg debug)
  (if debug (begin
    (display "ivue S: ")
    (for-each (lambda (cc) (begin (display (number->string cc 16)) (display " ")))
      (u8vector->list msg))
    (newline)
  ))
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
  (let* ((cache (serial-cache-read dev))
         (data (if cache (list->u8vector (map char->integer (string->list cache))) #f))
         (parseddata (if data (ivueparser:parseframe (u8vector->u8data data)) #f)))
    (if (u8data:sane? parseddata) (begin
      (set! ivue:data parseddata)
      (if debug (begin
        (display "ivue R: ")
        (for-each (lambda (cc) (begin (display (number->string cc 16)) (display " ")))
          (u8vector->list (u8data->u8vector ivue:data)))
        (newline)
      ))
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
              (if debug (display (string-append "ivue connected to " devname "\n")))
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
        (if (and (u8data:sane? ivue:data) (fx= (u8data-ref ivue:data 0) #x0E))
          (begin
            (if debug (display (string-append "ivue connection accepted \n")))
            (set! ivue:start ##now)
            2
          )
          (begin
            (if debug (display (string-append "ivue connection refused \n")))
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
        (if (and (u8data:sane? ivue:data)
             (fx= (u8data-ref ivue:data 0) #xE1)
             (fx= (u8data-ref ivue:data 5) ROIV_APDU)
             (fx= (u8data-ref ivue:data 9) CMD_CONFIRMED_EVENT_REPORT))
          (begin
            (if debug (display (string-append "ivue MDS Create Event received \n")))
            (ivue:send dev (ivue:mresult ivue:data) debug)
            3
          )
          (begin
            (if (u8data:sane? ivue:data) (ivueparser store ivue:data))
            2
          )
        )
      )
      ((fx= runlevel 3)
        (ivue:send dev (ivue:polltrends) debug)
        (if debug (display (string-append "ivue Trends requested \n")))
        (if (instance-refvar store instance "Waveforms" #f)
          (begin
            (ivue:send dev (ivue:pollwaves) debug)
            (if debug (display (string-append "ivue Waveforms requested \n")))
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
    (if (fx= runlevel 16)
      (begin
        (ivue:recv dev debug)
        ;; This gets new trends every 1 sec - also keeps waveform export alive.
        (if (fl> (fl- ##now ivue:lastchecked) 1.) (begin
          (if debug (begin
            (display (store-listcat store "ivue"))
            (newline)
            (if (instance-refvar store instance "Waveforms" #f) (begin
              (display (store-ref store "Pleth"))
              (newline)
            ))
          ))
          (ivue:send dev (ivue:polltrends) debug)
          (set! ivue:lastchecked ##now)
        ))
        (if ivue:data
          (begin
            (if (u8data:sane? ivue:data)
              (if (fx= (u8data-ref ivue:data 0) #x19)
                (begin
                  (log-error "ivue died after " (fl- ##now ivue:start) " sec")
                  (display "IVUE DIED after ")(display (fl- ##now ivue:start))(display "sec")(newline)
                  (instance-setvar! store instance "RunLevel" 0)
                )
                (ivueparser store ivue:data)
              )
            )
            (instance-setvar! store instance "BadQueries" 0)

          )
          (let ((missed (instance-refvar store instance "BadQueries" 0)))
            (if (fx> missed (if (instance-refvar store instance "Waveforms" #f) 200 20))
              (begin
                ;; Clear all the stores
                (store-clear! store (map car (store-listcat store "ivue")))
                ;; Go back to search for monitors
                (instance-setvar! store instance "RunLevel" 0)
                (instance-setvar! store instance "BadQueries" 0)
              )
              (instance-setvar! store instance "BadQueries" (fx+ missed 1))
            )
          )
        )
      )
    )
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
