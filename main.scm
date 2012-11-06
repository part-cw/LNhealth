;; Philips Pleth Waveform and Numerics Logger
(include "../s-optimize.inc")

;; Fonts
(include "./textures/ascii24.scm")(include "./textures/ascii24_fnt.scm")
(include "./textures/ascii16.scm")(include "./textures/ascii16_fnt.scm")
(include "./textures/num40.scm")(include "./textures/num40_fnt.scm")
(include "./textures/num18.scm")(include "./textures/num18_fnt.scm")
;; Labels for Waveform screen
(include "./textures/label_pr.scm")(include "./textures/label_prl.scm")
(include "./textures/label_spo2.scm")(include "./textures/label_spo2l.scm")
;; Copyright Line
(include "./textures/copyright.scm")

;; Global variables
(define buf "")
(define delta-update 10) ;;sec
(define delta-time-update 1) ;;sec
(define trend-time 3600) ;;sec
(define config-file (string-append (system-directory) (system-pathseparator) "config" (system-pathseparator) "settings.scm"))

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
  (glgui-label gui:main 10 (- (glgui-height-get) 24 3) 350 24 "Philips Pulseoximeter Datalogger" ascii24.fnt White)

  ;; Clock in upper right corner
  (set! clock (glgui-label gui:main (- (glgui-width-get) 70) (- (glgui-height-get) 24) 60 16 "" ascii16.fnt White))

  ;; Logging List
  (let ((x 525)(y (- (glgui-height-get) 50)) (w 460))
    ;;Header row
    (glgui-label gui:main (+ x 5) y 70 16 "Time" ascii16.fnt White)
    (glgui-label gui:main (+ x 75) y (- (glgui-width-get) 75 5) 16 "Log Entry" ascii16.fnt White)
    ;;The actual list itself
    (set! log-list
      (glgui-list gui:main x (- y 5 (* 12 30)) w (* 12 30) 30 (build-log-list) #f)
    )
    ;;Text Entry String
    (set! text (glgui-label gui:main (+ x 5) (- y 5 34 (* 12 30)) w 24 "" ascii24.fnt White))
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
    (glgui:draw-text-left (+ x 5) (+ y (/ (- h 16) 2)) 70 16 (seconds->string (car entry) "%T") ascii16.fnt White)  
    (glgui:draw-text-left (+ x 75) (+ y (/ (- h 24) 2)) (- w 90) 24 (cadr entry) ascii24.fnt White)  
  )
)

;; -----------------------------------------------------------------------------
;;  TREND GUI
;; -----------------------------------------------------------------------------
(define gui:trends #f)
(define (init-gui-trends)
  (set! gui:trends (make-glgui))
  ;; Positions of the trend numbers
  (set! pr_value (glgui-trend gui:trends (+ 5 400 60) (- (glgui-height-get) 100 (* 80 0)) label_pr.img num40.fnt Green))
  (set! prl_value (glgui-trend gui:trends (+ 5 400 60) (- (glgui-height-get) 100 (* 80 0.5)) label_prl.img num40.fnt DarkGreen))
  (set! spo2_value (glgui-trend gui:trends (+ 5 400 60) (- (glgui-height-get) 100 (* 80 2.25)) label_spo2.img num40.fnt Aquamarine))
  (set! spo2l_value (glgui-trend gui:trends (+ 5 400 60) (- (glgui-height-get) 100 (* 80 2.75)) label_spo2l.img num40.fnt Blue))

 ;; Define scales for Waveforms
  (set! PR_min 45)(set! PR_max 175)
  (set! SPO2_min 75)(set! SPO2_max 101)

  ;;Define traces to plot waveforms
  (let ((trace-mode GLTRACE_SHIFT)
        (trace-len (fix (/ trend-time delta-update))))
    (set! pr-trace (make-gltrace trace-len 150 trace-mode PR_min PR_max PR_min PR_max))
    (set! prl-trace (make-gltrace trace-len 150 trace-mode PR_min PR_max PR_min PR_max))
    (set! spo2-trace (make-gltrace trace-len 200 trace-mode SPO2_min SPO2_max SPO2_min SPO2_max))
    (set! spo2l-trace (make-gltrace trace-len 200 trace-mode SPO2_min SPO2_max SPO2_min SPO2_max))
  )
  ;; Clear the traces
  (gltrace:clear pr-trace) (gltrace:clear prl-trace)
  (gltrace:clear spo2-trace) (gltrace:clear spo2l-trace)

  ;; Add a grid
  (glgui-box gui:trends 5 (- (glgui-height-get) 110 (* 80 4)) 2 (* 80 5) DimGray)
  (glgui-box gui:trends (+ 5 133) (- (glgui-height-get) 110 (* 80 4)) 2 (* 80 5) DimGray)
  (glgui-box gui:trends (+ 5 267) (- (glgui-height-get) 110 (* 80 4)) 2 (* 80 5) DimGray)
  (glgui-box gui:trends (+ 5 400) (- (glgui-height-get) 110 (* 80 4)) 2 (* 80 5) DimGray)
  (set! time63 (glgui-label gui:main 0 (- (glgui-height-get) 110 (* 80 4) 20) 60 16 "" ascii16.fnt DarkGray))
  (set! time43 (glgui-label gui:main (+ 5 133 -17) (- (glgui-height-get) 110 (* 80 4) 20) 60 16 "" ascii16.fnt DarkGray))
  (set! time23 (glgui-label gui:main (+ 5 266 -17) (- (glgui-height-get) 110 (* 80 4) 20) 60 16 "" ascii16.fnt DarkGray))
  (set! time03 (glgui-label gui:main (+ 5 400 -17) (- (glgui-height-get) 110 (* 80 4) 20) 60 16 "" ascii16.fnt DarkGray))

  ;;Place the Trace Widgets
  (set! pr-trend (glgui-trace-slider gui:trends 5 (- (glgui-height-get) 110 (* 80 1)) 400 150 pr-trace Green ascii16.fnt))    
  (set! prl-trend (glgui-trace-slider gui:trends 5 (- (glgui-height-get) 110 (* 80 1)) 400 150 prl-trace DarkGreen ascii16.fnt))
  (set! spo2-trend (glgui-trace-slider gui:trends 5 (- (glgui-height-get) 110 (* 80 4)) 400 200 spo2-trace Aquamarine ascii16.fnt))
  (set! spo2l-trend (glgui-trace-slider gui:trends 5 (- (glgui-height-get) 110 (* 80 4)) 400 200 spo2l-trace Blue ascii16.fnt))
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
      (gltrace-add prl-trace (store-timedref store "PR(SpO2l)"))
      (gltrace-add spo2-trace (store-timedref store "SpO2"))
      (gltrace-add spo2l-trace (store-timedref store "SpO2l"))

      ;; Update the traces
      (gltrace-update pr-trace) 
      (gltrace-update prl-trace) 
      (gltrace-update spo2-trace)
      (gltrace-update spo2l-trace)
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
      (let ((prl-val (store-timedref store "PR(SpO2l)")))
        (glgui-widget-set! gui:trends prl_value 'label (if prl-val (number->string (fix prl-val)) ""))
      )
      (let ((spo2-val (store-timedref store "SpO2")))
        (glgui-widget-set! gui:trends spo2_value 'label (if spo2-val (number->string (fix spo2-val)) ""))
      )
      (let ((spo2l-val (store-timedref store "SpO2l")))
        (glgui-widget-set! gui:trends spo2l_value 'label (if spo2l-val (number->string (fix spo2l-val)) ""))
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

    ;; Load the configuration from files (otherwise use defaults)
    (store-set! "main" "waves" (list "Pleth" "PLETHl" "PLETHr"))
    (store-set! "main" "trends" (list "HR(ECG)" "RR" "STi" "STii" "STiii" "STavr" "STavl" "STavf" "STv"
       "PR(SpO2)" "SpO2" "Perf" "PR(SpO2l)" "SpO2l" "Perfl" "PR(SpO2r)" "SpO2r" "Perfr" 
       "NIBPsys" "NIBPdia" "NIBPmean" "ARTsys" "ARTdia" "ARTmean" "PRabp"
       "CVPmean" "CO2e" "CO2imin" "awRR"))
    (if (file-exists? config-file)
      (let ((data (with-input-from-file (list path: config-file) (lambda () (read)))))
        (for-each (lambda (l) (store-set! "main" (car l) (cadr l))) data)
      )
    )
    (store-set! "main" "trends" (append (list "time_str") (store-ref "main" "trends" '())))

    ;; Initialize the Philips monitor plugin
    (runtime-startcase "main" (time->timestamp (current-time)))
    (make-instance "main" "PHILIPSmonitor" "monitor" `("Port" ,(cond 
      ((string=? (system-platform) "linux") "/dev/ttyUSB0")
      ((string=? (system-platform) "win32") "COM3")
      (else "/dev/tty.iap"))) '("Waveforms" #t) '("Debug" #f))
    (make-instance "main" "TRENDOUT" "trendoutput" `("Trends" ,(store-ref "main" "trends" '())))
    (for-each (lambda (l) (make-instance "main" (string-append "WAVEOUT" l) "waveoutput" `("Source" ,l)))
      (store-ref "main" "waves" '()))

    ;;Make sure that runtime actually runs !!!
    (runtime-init)
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
    (thread-sleep! 0.01)        ;; Sleep for 100 usec
    (runtime-iterate)
  )
;; termination
  (lambda () 
    (runtime-cleanup)
    #t
  )
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)
