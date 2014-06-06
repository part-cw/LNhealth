;; S5 Generic Waveform and Numerics Logger
;; Matthias Görges 2012-2014

;; Global variables
(define buf "")
(define delta-update 10) ;;sec
(define delta-time-update 1) ;;sec
(define trend-time 1800) ;;sec
(define quit-armed #f)

;; -----------------------------------------------------------------------------
;;  MAIN GUI
;; -----------------------------------------------------------------------------

;; (init-gui-main)
;;
;; The main gui parts: Logging list, Title row and all buttons are defined here.
(define gui:main #f)
(define (init-gui-main)
  (set! gui:main (make-glgui))
  (glgui-pixmap gui:main 675 2 copyright.img)
  (glgui-menubar gui:main 0 (- (glgui-height-get) 30) (glgui-width-get) 30)

  ;; Label in upper left corner
  (glgui-label gui:main 10 (- (glgui-height-get) 24 3) 250 24 (system-appname) ascii_24.fnt White)
  (glgui-label gui:main 160 (- (glgui-height-get) 20 3) 75 16
    (string-append "Ver. " (system-appversion)) ascii_16.fnt White)

  ;; Clock in upper right corner
  (set! clock (glgui-label gui:main (- (glgui-width-get) 100) (- (glgui-height-get) 24) 100 16 "" ascii_24.fnt White))

  ;; Logging List
  (let ((x 525)(y (- (glgui-height-get) 50)) (w 460))
    ;;Header row
    (glgui-label gui:main (+ x 5) y 95 16 "Time" ascii_16.fnt White)
    (glgui-label gui:main (+ x 75) y (- (glgui-width-get) 75 5) 16 "Log Entry" ascii_16.fnt White)
    ;;The actual list itself
    (set! log-list
      (glgui-list gui:main x (- y 5 (* 12 30)) w (* 12 30) 30 (build-log-list) #f)
    )
    ;;Text Entry String
    (glgui-box gui:main x (- y 5 34 (* 12 30)) w 30 (color-shade Blue 0.1))
    (set! text (glgui-label gui:main x (- y 5 34 (* 12 30)) w 30 "" ascii_24.fnt White))
  )
)

;; The build-log list creator, which loops through the log entries and makes appropriate gui elements for it
(define (build-log-list)
  (let ((logs (store-event-listnew store)))
    (if logs
      (let loop ((i 0) (result (list)))
        (if (= i (length logs))
          result
          (loop (+ i 1)(append result (list (log-list-element (list-ref logs i)))))
        )
      )
      (list)
    )
  )
)

;; Draw a log-list element with data from the entry field
(define (log-list-element entry)
  (lambda (g wgt x y w h s)
    (glgui:draw-text-left (+ x 5) (+ y (/ (- h 16) 2)) 70 16 (seconds->string (car entry) "%T") ascii_16.fnt White)  
    (glgui:draw-text-left (+ x 75) (+ y (/ (- h 24) 2)) (- w 90) 24 (cadr entry) ascii_24.fnt White)  
  )
)

;; -----------------------------------------------------------------------------
;;  TREND GUI
;; -----------------------------------------------------------------------------
(define gui:trends #f)
(define (init-gui-trends)
  (set! gui:trends (make-glgui))
  ;; Positions of the trend numbers
  (set! hr_value (glgui-valuelabel gui:trends (+ 5 400 70) (- (glgui-height-get) 115 (* 90 0))
    label_hr.img ascii_40.fnt Green))  
  (set! map_value (glgui-valuelabel gui:trends (+ 5 400 70) (- (glgui-height-get) 115 (* 90 1))
    label_map.img ascii_40.fnt Red))
  (set! mac_value (glgui-valuelabel gui:trends (+ 5 400 75) (- (glgui-height-get) 115 (* 90 2))
    label_mac.img ascii_40.fnt Blue))
  (set! temp_value (glgui-valuelabel gui:trends (+ 5 400 75) (- (glgui-height-get) 115 (* 90 3))
    label_temp.img ascii_40.fnt (color-shade Red 0.5)))

  ;; Define scales for Waveforms
  (set! HR_min 30)(set! HR_max 150)
  (set! MAP_min 30)(set! MAP_max 200)
  (set! MAC_min 0)(set! MAC_max 2)
  (set! TEMP_min 34)(set! TEMP_max 38)

  ;;Define traces to plot waveforms
  (let ((trace-mode GLTRACE_SHIFT)
        (trace-len (fix (/ trend-time delta-update))))
    (set! hr-trace (make-gltrace trace-len 90 trace-mode HR_min HR_max HR_min HR_max))
    (set! art_map-trace (make-gltrace trace-len 90 trace-mode MAP_min MAP_max MAP_min MAP_max))
    (set! nibp_map-trace (make-gltrace trace-len 90 trace-mode MAP_min MAP_max MAP_min MAP_max))
    (set! nibp_sys-trace (make-gltrace trace-len 90 trace-mode MAP_min MAP_max MAP_min MAP_max))
    (set! nibp_dia-trace (make-gltrace trace-len 90 trace-mode MAP_min MAP_max MAP_min MAP_max))
    (set! mac-trace (make-gltrace trace-len 90 trace-mode MAC_min MAC_max MAC_min MAC_max))
    (set! temp-trace (make-gltrace trace-len 90 trace-mode TEMP_min TEMP_max TEMP_min TEMP_max))

  )
  ;; Clear the traces
  (gltrace:clear hr-trace)
  (gltrace:clear art_map-trace)
  (gltrace:clear nibp_map-trace)
  (gltrace:clear nibp_dia-trace)
  (gltrace:clear nibp_sys-trace)
  (gltrace:clear mac-trace)
  (gltrace:clear temp-trace)

  ;; Add a grid
  (glgui-box gui:trends 5 (- (glgui-height-get) 50 (* 120 3)) 2 (* 120 4) DimGray)
  (glgui-box gui:trends (+ 5 133) (- (glgui-height-get) 50 (* 120 3)) 2 (* 120 3) DimGray)
  (glgui-box gui:trends (+ 5 267) (- (glgui-height-get) 50 (* 120 3)) 2 (* 120 3) DimGray)
  (glgui-box gui:trends (+ 5 400) (- (glgui-height-get) 50 (* 120 3)) 2 (* 120 3) DimGray)
  (set! time63 (glgui-label gui:main 0 (- (glgui-height-get) 50 (* 120 3) 20) 60 16 "" ascii_16.fnt DarkGray))
  (set! time43 (glgui-label gui:main (+ 5 133 -17) (- (glgui-height-get) 50 (* 120 3) 20) 60 16 "" ascii_16.fnt DarkGray))
  (set! time23 (glgui-label gui:main (+ 5 266 -17) (- (glgui-height-get) 50 (* 120 3) 20) 60 16 "" ascii_16.fnt DarkGray))
  (set! time03 (glgui-label gui:main (+ 5 400 -17) (- (glgui-height-get) 50 (* 120 3) 20) 60 16 "" ascii_16.fnt DarkGray))

  ;;Place the Trace Widgets
  (set! hr-trend (glgui-trace-slider gui:trends 5 (- (glgui-height-get) 135 (* 90 0)) 400 90
    hr-trace Green ascii_16.fnt))
  (set! art_map-trend (glgui-trace gui:trends 5 (- (glgui-height-get) 135 (* 90 1)) 400 90
    art_map-trace Red))
  (set! nibp_sys-trend (glgui-trace gui:trends 5 (- (glgui-height-get) 135 (* 90 1)) 400 90
    nibp_sys-trace Red))
  (set! nibp_dia-trend (glgui-trace gui:trends 5 (- (glgui-height-get) 135 (* 90 1)) 400 90
    nibp_dia-trace Red))
  (set! nibp_map-trend (glgui-trace-slider gui:trends 5 (- (glgui-height-get) 135 (* 90 1)) 400 90
    nibp_map-trace IndianRed ascii_16.fnt))
  (set! mac-trend (glgui-trace-slider gui:trends 5 (- (glgui-height-get) 135 (* 90 2)) 400 90
    mac-trace Blue ascii_16.fnt))
  (set! temp-trend (glgui-trace-slider gui:trends 5 (- (glgui-height-get) 135 (* 90 3)) 400 90
    temp-trace (color-shade Red 0.5) ascii_16.fnt))
)

;; (update-trends store)
;;
;; Update the trends every delta time using data from STORE
(define last-trend-update 0)
(define (update-trends store)
  (if (> (- ##now last-trend-update) delta-update)
    (begin
      (set! last-trend-update ##now)
      ;; Update the Trend Numerics and Waveform
      (gltrace-add hr-trace (store-timedref store "hr"))
      (gltrace-add art_map-trace (store-timedref store "p1_mean"))
      (gltrace-add nibp_map-trace (store-timedref store "nibp_mean"))
      (gltrace-add nibp_sys-trace (store-timedref store "nibp_sys"))
      (gltrace-add nibp_dia-trace (store-timedref store "nibp_dia"))
      (gltrace-add mac-trace (store-timedref store "mac_age_sum"))
      (gltrace-add temp-trace (store-timedref store "temp1"))

      ;; Update the traces
      (gltrace-update hr-trace)
      (gltrace-update art_map-trace)
      (gltrace-update nibp_map-trace)
      (gltrace-update nibp_dia-trace)
      (gltrace-update nibp_sys-trace)
      (gltrace-update mac-trace)
      (gltrace-update temp-trace)
    )
  )
)

;; (update-values store)
;;
;; Update the values using data from STORE
(define last-value-update 0)
(define (update-values store)
  (if (> (- ##now last-value-update) delta-time-update)
    (begin
      (set! last-value-update ##now)
      ;; Update the log list just in case
      (glgui-widget-set! gui:main log-list 'list (build-log-list))
      ;; Update the Trend Numerics and Waveform
      (let ((hr-val (store-timedref store "hr"))
            (hr-src (store-ref store "hr_source")))
        (glgui-widget-set! gui:trends hr_value 'label (if hr-val (number->string (fix hr-val)) ""))
        (if hr-src
          (glgui-widget-set! gui:trends hr_value 'color (cond
            ((string=? hr-src "ECG1") Green)
            ((string=? hr-src "PLETH") White)
            ((string=? hr-src "BP1") Red)
            (else DarkGray))
          )
        )
      )
      (let ((art-map-val (store-timedref store "p1_mean"))
            (art-hr-val (store-timedref store "p1_hr"))
            (nibp-map-val (store-timedref store "nibp_mean")))
        (glgui-widget-set! gui:trends map_value 'label
          (if (and art-hr-val art-map-val) 
            (number->string (fix art-map-val))
            (if nibp-map-val (number->string (fix nibp-map-val)) "")
          ))
      )
      (let ((mac-val (store-timedref store "mac_age_sum")))
        (glgui-widget-set! gui:trends mac_value 'label (if mac-val (float->string mac-val 2) ""))
      )
      (let ((temp-val (store-timedref store "temp1")))
        (glgui-widget-set! gui:trends temp_value 'label (if temp-val (float->string temp-val 1) ""))
      )

      ;; Update times everywhere
      (store-set! "main" "time_str" (seconds->string ##now "%T"))
      (glgui-widget-set! gui:main clock 'label (seconds->string ##now "%T"))
      ;; Update the other clocks too
      (glgui-widget-set! gui:main time63 'label (seconds->string (- ##now trend-time) "%H:%M"))
      (glgui-widget-set! gui:main time43 'label (seconds->string (- ##now (* (/ trend-time 3) 2)) "%H:%M"))
      (glgui-widget-set! gui:main time23 'label (seconds->string (- ##now (/ trend-time 3)) "%H:%M"))
      (glgui-widget-set! gui:main time03 'label (seconds->string ##now "%H:%M"))
    )
  )
)

(define (detect-mac-usb-serial)
  (let loop ((files (directory-files "/dev")))
    (if (fx= (length files) 0)
      ""
      (let ((file (car files)))
        (if (and (fx> (string-length file) 14)
                 (string=? (substring file 0 14) "tty.usbserial-"))
          (string-append "/dev/" file)
          (loop (cdr files))
        ))
    )
  ))

;; -----------------------------------------------------------------------------
;;  MAIN PROGRAM
;; -----------------------------------------------------------------------------
(main
;; initialization
  (lambda (w h)
    (if (or (string=? (system-platform) "macosx")
            (string=? (system-platform) "linux") 
            (string=? (system-platform) "win32")) (make-window 1000 475))
    (glgui-orientation-set! GUI_LANDSCAPE)
    ;; Initialize the gui and the monitor connection
    (set! store (make-store "main"))
    (init-gui-main)
    (init-gui-trends)    

    ;; Initialize the datex monitor plugin
    (make-instance store "S5monitor" "monitor" `("Port" ,(cond 
      ((string=? (system-platform) "linux") "/dev/ttyUSB0")
      ((string=? (system-platform) "macosx") (detect-mac-usb-serial))
      ((string=? (system-platform) "win32") "COM1")
      (else "/dev/tty.iap"))) '("Waveforms" #t) '("Debug" #f))
    ;; Initialize the CardioQ monitor plugin
    (make-instance store "cardioq" "monitor" `("Port" ,(cond
      ((string=? (system-platform) "linux") "/dev/ttyUSB1")
      ((string=? (system-platform) "win32") "COM2")
      (else "/dev/tty.iap"))) '("Waveforms" #t) '("Debug" #f))
    ;; Initialize all of our output plugins
    (make-instance store "WAVEECG" "waveoutput" '("Source" "ECG1"))
    (make-instance store "WAVEPLETH" "waveoutput" '("Source" "PLETH"))
    (make-instance store "WAVECO2" "waveoutput" '("Source" "CO2"))
    (make-instance store "TREND" "trendoutput" `("Trends"
      ,(append (list "time_str") s5parser:physdatavalues_basic
               s5parser:physdatavalues_ext1 s5parser:physdatavalues_ext2
               s5parser:physdatavalues_ext3 cardioq:parameters
               (list "marker" "alarm1_text" "alarm2_text"))))

    ;;Make sure that scheduler actually runs !!!
    (scheduler-init)
    (scheduler-startcase store (time->timestamp (current-time)))
  )
;; events
  (lambda (t x y) 
    (update-trends store)
    (update-values store)
    (if (= t EVENT_KEYPRESS) (begin
      (cond
        ((= x EVENT_KEYESCAPE) 
          (if quit-armed 
            (terminate)
            (begin
              (set! quit-armed #t)
              (store-event-add store 1 "Press ESC again to quit!")
              (glgui-widget-set! gui:main log-list 'list (build-log-list))
            )
          )
        )
        ((and (>= x 32) (< x 127))
          (set! quit-armed #f)
          (set! buf (string-append buf (string (integer->char x))))
          (glgui-widget-set! gui:main text 'align
            (if (> (glgui:stringwidth buf ascii_24.fnt) (glgui-widget-get gui:main text 'w))
              GUI_ALIGNRIGHT GUI_ALIGNLEFT))
            )
        ((= x 3) ;; This is backspace
          (set! quit-armed #f)
          (if (> (string-length buf) 0) (set! buf (substring buf 0 (- (string-length buf) 1))))
          (glgui-widget-set! gui:main text 'align
            (if (> (glgui:stringwidth buf ascii_24.fnt) (glgui-widget-get gui:main text 'w))
              GUI_ALIGNRIGHT GUI_ALIGNLEFT))
            )
        ((= x 1) (begin ;; This is return
          (set! quit-armed #f)
          (if (> (string-length buf) 0) (begin
            ;; It there is data in the string log it.
            (store-event-add store 1 buf)
            (glgui-widget-set! gui:main log-list 'list (build-log-list))
          ))
          (set! buf ""))
        )
      )
      (glgui-widget-set! gui:main text 'label buf)
    ))
    (glgui-event (list gui:main gui:trends) t x y)

    ;; Garbage collect, sleep and iterate over new plugin data
    (##gc)                     ;; This calls the garbage collector
    (thread-sleep! 0.01)       ;; Sleep for 100 usec
    (scheduler-iterate)
  )
;; termination
  (lambda () 
    (scheduler-cleanup)
    #t
  )
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)
;;eof
