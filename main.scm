;; Philips Generic Waveform and Numerics Logger
;; Matthias Görges 2012-2013

;; Global variables
(define buf "")
(define delta-update 10) ;;sec
(define delta-time-update 1) ;;sec
(define trend-time 3600) ;;sec

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
  (glgui-label gui:main 10 (- (glgui-height-get) 24 3) 350 24 "Philips Data Logger" ascii_24.fnt White)

  ;; Clock in upper right corner
  (set! clock (glgui-label gui:main (- (glgui-width-get) 70) (- (glgui-height-get) 24) 60 16 "" ascii_16.fnt White))

  ;; Logging List
  (let ((x 525)(y (- (glgui-height-get) 50)) (w 460))
    ;;Header row
    (glgui-label gui:main (+ x 5) y 70 16 "Time" ascii_16.fnt White)
    (glgui-label gui:main (+ x 75) y (- (glgui-width-get) 75 5) 16 "Log Entry" ascii_16.fnt White)
    ;;The actual list itself
    (set! log-list
      (glgui-list gui:main x (- y 5 (* 12 30)) w (* 12 30) 30 (build-log-list) #f)
    )
    (glgui-widget-set! gui:main log-list 'hidden #t)
    ;;Text Entry String
    (set! text (glgui-label gui:main (+ x 5) (- y 5 34 (* 12 30)) w 24 "" ascii_24.fnt White))
    ;; Recording start button  
    (set! recording-start-button 
      (glgui-button-string gui:main x (- y (* 6 30)) w 50 "Start Recording" ascii_24.fnt start-recording-callback)
    )
  )
)

;; (build-log-list)
;;
;; The build-log list creator, which loops through the log entries and makes appropriate gui elements for it
(define (build-log-list)
  (let ((logs (store-ref "main" "Log")))
    (if logs
      (let loop ((i 0) (result (list)))
        (if (= i (length logs)) result
	   (loop (+ i 1)(append result (list (log-list-element (list-ref logs i)))))
        )
      )
      (list) 
    )
  )
)

;; (log-list-element entry)
;;
;; Draw a log-list element with data from the entry field
(define (log-list-element entry)
  (lambda (g wgt x y w h s)
    (glgui:draw-text-left (+ x 5) (+ y (/ (- h 16) 2)) 70 16 (seconds->string (car entry) "%T") ascii_16.fnt White)  
    (glgui:draw-text-left (+ x 75) (+ y (/ (- h 24) 2)) (- w 90) 24 (cadr entry) ascii_24.fnt White)  
  )
)

;; Start Recording data
(define (start-recording-callback g w t x y)
  (let ((lst (store-listcat "main" "ivue")))
    ;; Check if we have at least one trend variable before starting to log data
    (if (and (list? lst) (not (null? lst))) (begin
      ;; Log the trend variables recorded
      (let* ((trends (map car (store-listcat "main" "ivue")))
             (buf (string-append "TRENDS: " (string-mapconcat trends ", "))))
        (make-instance "main" "TRENDOUT" "trendoutput" `("Trends" ,(append (list "time_str") trends)))
        (store-set! "main" "Log" (append (list (list (floor ##now) buf)) (store-ref "main" "Log" '())))
        (glgui-widget-set! gui:main log-list 'list (build-log-list))
        (store-event-add "main" 1 buf)
      )
      ;; Log the waveforms recorded
      (let* ((waves (table-ref (store:wdatatable "main") 'IdList '()))
             (buf (string-append "WAVES: " (string-mapconcat waves ", "))))
        (for-each (lambda (l) (make-instance "main" (string-append "WAVEOUT" l) "waveoutput" `("Source" ,l))) waves)
        (store-set! "main" "Log" (append (list (list (floor ##now) buf)) (store-ref "main" "Log" '())))
        (glgui-widget-set! gui:main log-list 'list (build-log-list))
        (store-event-add "main" 1 buf)
      )
      ;; Start the case
      (scheduler-startcase "main" (time->timestamp (current-time)))
      ;; Hide the start button and show the log message list
      (glgui-widget-set! gui:main recording-start-button 'hidden #t)
      (glgui-widget-set! gui:main log-list 'hidden #f)
    ))
    (glgui-widget-set! gui:main recording-start-button 'color Red)
  )
)

;; -----------------------------------------------------------------------------
;;  TREND GUI
;; -----------------------------------------------------------------------------
(define gui:trends #f)
(define (init-gui-trends)
  (set! gui:trends (make-glgui))
  ;; Positions of the trend numbers
  (set! pr_value (glgui-valuelabel gui:trends (+ 5 400 60) (- (glgui-height-get) (* 120 1)) label_pr.img num_40.fnt Green))
  (set! map_value (glgui-valuelabel gui:trends (+ 5 400 60) (- (glgui-height-get) (* 120 2)) label_map.img num_40.fnt Red))
  (set! map_nibp_value (glgui-valuelabel gui:trends (+ 5 400 60) (- (glgui-height-get) (* 120 2.3)) label_map.img num_40.fnt IndianRed))
  (set! spo2_value (glgui-valuelabel gui:trends (+ 5 400 60) (- (glgui-height-get) (* 120 3)) label_spo2.img num_40.fnt Aquamarine))

 ;; Define scales for Waveforms
  (set! PR_min 45)(set! PR_max 175)
  (set! SPO2_min 75)(set! SPO2_max 101)
  (set! MAP_min 35)(set! MAP_max 105)

  ;;Define traces to plot waveforms
  (let ((trace-mode GLTRACE_SHIFT)
        (trace-len (fix (/ trend-time delta-update))))
    (set! pr-trace (make-gltrace trace-len 130 trace-mode PR_min PR_max PR_min PR_max))
    (set! spo2-trace (make-gltrace trace-len 100 trace-mode SPO2_min SPO2_max SPO2_min SPO2_max))
    (set! map-trace (make-gltrace trace-len 140 trace-mode MAP_min MAP_max MAP_min MAP_max))
    (set! map_nibp-trace (make-gltrace trace-len 140 trace-mode MAP_min MAP_max MAP_min MAP_max))
  )
  ;; Clear the traces
  (gltrace:clear pr-trace)
  (gltrace:clear spo2-trace)
  (gltrace:clear map-trace)
  (gltrace:clear map_nibp-trace)

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
  (set! pr-trend (glgui-trace-slider gui:trends 5 (- (glgui-height-get) 50 (* 120 1)) 400 110 pr-trace Green ascii_16.fnt))    
  (set! spo2-trend (glgui-trace-slider gui:trends 5 (- (glgui-height-get) 50 (* 120 3)) 400 110 spo2-trace Aquamarine ascii_16.fnt))
  (set! map-trend (glgui-trace-slider gui:trends 5 (- (glgui-height-get) 50 (* 120 2)) 400 110 map-trace Red ascii_16.fnt))
  (set! map_nibp-trend (glgui-trace-slider gui:trends 5 (- (glgui-height-get) 50 (* 120 2)) 400 110 map_nibp-trace IndianRed ascii_16.fnt))
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
      (gltrace-add pr-trace (store-timedref store "PR(SpO2)"))
      (gltrace-add spo2-trace (store-timedref store "SpO2"))
      (gltrace-add map_nibp-trace (store-timedref store "NIBPmean"))
      (gltrace-add map-trace (store-timedref store "ABPmean"))

      ;; Update the traces
      (gltrace-update pr-trace) 
      (gltrace-update spo2-trace)
      (gltrace-update map-trace)
      (gltrace-update map_nibp-trace)
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
      ;; Update the Trend Numerics and Waveform
      (let ((pr-val (store-timedref store "PR(SpO2)")))
        (glgui-widget-set! gui:trends pr_value 'label (if pr-val (number->string (fix pr-val)) ""))
      )
      (let ((spo2-val (store-timedref store "SpO2")))
        (glgui-widget-set! gui:trends spo2_value 'label (if spo2-val (number->string (fix spo2-val)) ""))
      )
      (let ((map-val (store-timedref store "ABPmean")))
        (glgui-widget-set! gui:trends map_value 'label (if map-val (number->string (fix map-val)) ""))
      )
      (let ((map-val (store-timedref store "NIBPmean")))
        (glgui-widget-set! gui:trends map_nibp_value 'label (if map-val (number->string (fix map-val)) ""))
      )
      ;; Update times everywhere
      (store-set! "main" "time_str" (seconds->string ##now "%H%M%S"))              
      (glgui-widget-set! gui:main clock 'label (seconds->string ##now "%T"))       
      ;; Update the other clocks too
      (glgui-widget-set! gui:main time63 'label (seconds->string (- ##now trend-time) "%H:%M"))
      (glgui-widget-set! gui:main time43 'label (seconds->string (- ##now (* (/ trend-time 3) 2)) "%H:%M"))
      (glgui-widget-set! gui:main time23 'label (seconds->string (- ##now (/ trend-time 3)) "%H:%M"))
      (glgui-widget-set! gui:main time03 'label (seconds->string ##now "%H:%M"))
    )
  )
)


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
    (init-gui-main)
    (init-gui-trends)    
    (make-store "main")

    ;; Initialize the Philips monitor plugin
    (make-instance "main" "PHILIPSmonitor" "monitor" `("Port" ,(cond 
      ((string=? (system-platform) "linux") "/dev/ttyUSB0")
      ((string=? (system-platform) "win32") "COM3")
      (else "/dev/tty.iap"))) '("Waveforms" #t) '("Debug" #f))

    ;;Make sure that scheduler actually runs !!!
    (scheduler-init)
  )
;; events
  (lambda (t x y) 
    (update-trends "main")
    (update-values "main")
    ;; These are button presses
    (if (= t EVENT_KEYPRESS) 
      (begin
	(cond
	  ((= x EVENT_KEYESCAPE) (terminate))
	  ((and (>= x 32) (< x 127))
	    (set! buf (string-append buf (string (integer->char x))))
	  )
	  ((= x 3) ;; This is backspace
	    (if (> (string-length buf) 0) (set! buf (substring buf 0 (- (string-length buf) 1))))
	  )
	  ((= x 1) ;; This is return
	    (begin
	      (if (> (string-length buf) 0)
		;; It there is data in the string log it.
		(begin
		  (store-set! "main" "Log" (append (list (list (floor ##now) buf)) (store-ref "main" "Log" '())))
		  (glgui-widget-set! gui:main log-list 'list (build-log-list))
		  (store-event-add "main" 1 buf)
		)
	      )
	      (set! buf "")
	    )
	  )
	)
	(glgui-widget-set! gui:main text 'label buf)
      )
    ) 
    (glgui-event (list gui:main gui:trends) t x y)
    
    ;; Garbage collect, sleep and iterate over new plugin data
    (##gc)                     ;; This calls the garbage collector 
    (thread-sleep! 0.005)        ;; Sleep for 5 usec
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
