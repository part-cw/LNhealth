;; telePORT [formerly known as iFishAA]
;; Matthias Görges 2011-2012
(define voip:enabled #f) ;; Disable until stable

;; Until replaced I need these two to run as they are not in ln_core
(define float->ccstring float->choppedstring)
(define float->autostring float->string)

;; -----------------------------------------------------------------------------
;;  LOADING OF TEXTURES AND FONTS and definition of texture lists
;; -----------------------------------------------------------------------------
;; Load Textures for Overview Screen: Anesthesia Phase Icons, labels and size 24 font
(define phase-labels (list empty.img induction.img maintenance.img emergence.img repeat-empty.img))

;; Textures for Phonebook screen
(define delete-small.img (append (list 24 24) (cddr delete.img)))
(define edit-small.img (append (list 24 24) (cddr edit.img)))

;; Labels for Short Messages
(define message-texts (list "Can you help?" "Thanks"
			    "Are you okay?" "I'm okay" 
			    "Had lunch?" "Food in Lounge" 
			    "I'm leaving" "Go home" 
			    "Meet in Storage" "Meet in Office"
			    "Yes" "No"))

;; Navigation Menu Icons
(define icon-list (list messaging-icon.img phonebook-icon.img home-icon.img rooms-icon.img reminder-icon.img))

;; -----------------------------------------------------------------------------
;;  GLOBAL VARIABLES
;; -----------------------------------------------------------------------------

;; Screen indicies
(define MODE_LOGIN 99)
(define MODE_MESSAGING 0)
(define MODE_PHONEBOOK 1)
(define MODE_OVERVIEW 2)
(define MODE_ROOMS 3)
(define MODE_REMINDER 4)
(define MODE_USERS 5)
(define MODE_CHAT 6)
(define MODE_ALERT 7)
(define MODE_WAVES 8)
(define MODE_REMINDER_SETUP 9)
(define MODE_TRENDS 10)
(define MODE_VOIP 11)
(define MODE_SETUP 98)
(define mode MODE_LOGIN)

; RUPI settings 
(define hostname-file (string-append (system-directory) (system-pathseparator) "server"))
(define rupi:port 8080)
;; DNSdynamic.org account info User:mgorges@cw.bc.ca Pwd: Vitalnod3
(define rupi:hostname "bcch.ece.ubc.ca") ;; This is the default hostname
(define rupi:addr #f)
(define rupi:key (u8vector 77 71 148 114 103 101 115 31))
(define rupi:pin-length 4)
(define rupi:last-wave-request 0.) ;;Timestamp of last waveform request
(define rupi:last-wave-update 0.) ;; Timestamp of last waveform update
(define rupi:last-trend-update 0.) ;; Timestamp of last trend update
(define rupi:wave-request-frequency 1.) ;; Waveforms get dispatched every second so I need to get them then!
(define rupi:wave-update-frequency 0.1) ;; 0.1sec ? - needs to be reasonable for the phone CPU

;; Colors
(define gui:inactive-color DimGray)
(define gui:active-color White)
(define gui:toggle-normal-color gui:inactive-color)
(define gui:toggle-selected-color (color:shuffle #x2a4475ff))

;; Misc gui sizes
(define gui:row-height 40) ;; Height of a standard gui list row
(define gui:detail-height (* 3 40)) ;; This is the amount of space used for the Message Detail Part
(define gui:menu-height 32) ;; Top menu with battery, time, label
(define gui:navigation-height 64) ;; Bottom menu with navigation icons

;; Message expiration settings
(define gui:alert-max-age (* 60 60 6))
(define gui:chat-max-age (* 60 60 24 7))
(define gui:alert-expire-answered-age 300.)
(define gui:alert-expire-unanswered-age 3600.)

;; Misc repeated task timestamps
(define gui:battery-tstamp #f)
(define gui:alertcheck-tstamp #f)

;; Voip setting information
(define voip:ring #f)
(define voip:ring-count 0)
(define voip:volume 0.8)

;; Information needed to relogin on crash
(define login-file (string-append (system-directory) (system-pathseparator) "login"))

;; -----------------------------------------------------------------------------
;;   LOGIN SCREEN
;; -----------------------------------------------------------------------------
(define gui:login #f)
(define (init-gui-login)
  (set! gui:login (make-glgui))

  ;; Logo and Build Information
  (glgui-pixmap gui:login (/ (fx- (glgui-width-get) (car telePORT-logo.img)) 2) (- (glgui-height-get) (cadr telePORT-logo.img) 20) telePORT-logo.img)
  (let ((x (- (glgui-width-get) 90))(y 45))
    (glgui-label gui:login x (+ y 18) 85 16 "Build:" ascii_16.fnt DarkGray)
    (glgui-label gui:login x y 85 16 (system-builddate) ascii_16.fnt DarkGray)
  )    
  
  ;; Message and *s as response
  (set! login-text (glgui-label gui:login 50 (- (glgui-height-get) 150) 250 25 "Please enter your pin:" ascii_24.fnt White))
  (set! login-pin (glgui-label gui:login 50 (- (glgui-height-get) 175) 250 25 "" ascii_24.fnt Green))
  ;; 10 keypad
  (let ((row 3) (col 1))
    (glgui-button-string gui:login (+ 50 (* col 70)) (- (glgui-height-get) 235 (* row 70)) 60 60 "0" num_40.fnt login-callback)
  )
  (let loop ((row 0))
    (if (<= row 2)
      (begin
	(let loop2 ((col 0))
	  (if (<= col 2)
	    (begin 
	      (glgui-button-string gui:login (+ 50 (* col 70)) (- (glgui-height-get) 235 (* row 70)) 60 60 (number->string (+ (* row 3) col 1)) num_40.fnt login-callback)
	      (loop2 (+ col 1))
	    )
	  )
	)
	(loop (+ row 1))
      )
    )
  )
  ;; Copyright Line
  (glgui-pixmap gui:login 10 2 copyright.img)
)

(define (login-callback g wgt t x y)
  ;; Give the user some feedback that they entered sth.
  (let ((oldstr (glgui-widget-get g login-pin 'label)))
    (glgui-widget-set! g login-pin 'label (string-append oldstr "X "))
  )

  ;; Check if a correct pin is entered and start the system
  (let* ((store "main")
         (login (store-ref store "Key" ""))
         (key (if wgt (car (glgui-widget-get g wgt 'image)) "")))
    (store-set! store "Key" (string-append login key))
    ;; Check if we reached pin length [Login isn't queried again so its rupi:pin-length - 1]
    (if (>= (string-length login) (- rupi:pin-length 1))
      (if (not (string=? (store-ref store "Key") "9999"))
        (let* ((rc (rupi-client 0 rupi:key rupi:addr rupi:port))
               (login0 (store-ref store "Key"))
               (login (if (and (string=? rupi:hostname "bcch.ece.ubc.ca") 
                               (not (or (string=? login0 "9999") (string<? login0 "0005"))))
                  (begin (store-set! store "Key" "0000") "0000")
                  login0
               ))
               ;;Include build date so we can send messages to ask users to upgrade
               ;; We will try 2 times and fail otherwise
               (success2 (rupi-cmd rc "LOGIN" login (number->string (system-buildepoch)) (host-name)))
               (success (if (rupi-valid? rc) success2
                 (rupi-cmd rc "LOGIN" login (number->string (system-buildepoch)) (host-name))
               ))) 
          (if success
            (begin ;;If pin is acceptable run the app
              (store-set! store "UserName" (car success))
              (store-set! store "LastUpdateTime" (cadr success)) ;;This way we only request the new messages
              (store-set! store "AlertMessages" (expire-messages (caddr success) gui:alert-max-age)) ;; and we also keep the list of already answered things
              (store-set! store "Reminders" (cadddr success)) ;; and also keep the old reminders
              (if (= (length success) 5) ;; added here as to not break compatibility of old clients
                (store-set! store "ChatMessages" (expire-messages (list-ref success 4) gui:chat-max-age)) 
              )
              (init-gui-messaging)
              (init-gui-alert)
              (init-gui-chat)
              (init-gui-chat-landscape)
              (init-gui-reminder)
              (init-gui-users)
              (init-gui-rooms)
              (if voip:enabled (init-gui-voip))
              (init-gui-phonebook)
              (init-server-communication store) ;;Moved before the overview, reminder-setup so we can initialize values
              (init-gui-overview)
              (init-gui-waves)    
              (init-gui-waves-landscape)
              (init-gui-trends)
              (init-gui-reminder-setup)
              (init-gui-phonebook-editor)
              (set! gui:battery-tstamp 0.) ;; This allows me to record battery timestamps
              (set! gui:alertcheck-tstamp 0.);; Needed to clean up old alert messages
              (set! mode MODE_OVERVIEW) ;; Both of these need to happen
              (glgui-widget-set! gui:menu navigation-bar 'value MODE_OVERVIEW)
              (glgui-widget-set! gui:popup popup-box 'callback hide-popup-click)
              ;; Allow relogin on crash for Android and iOS only [and Linux for testing]
              (if (or (string=? (system-platform) "iphone")
                      (string=? (system-platform) "android")
                      (string=? (system-platform) "linux"))
                (with-output-to-file login-file (lambda () (display login)))
              )
            )
            (if rupi:error
              (begin
                (glgui-widget-set! gui:popup popup-box 'callback #f)
                (store-set! store "popup-text" (list "VITALNODE MISSING" "Can't connect to VitalNode. Please check the Wifi connection is active and retry. Also check that port 8080/tcp is not blocked by your firewall."))
                (show-popup)
                (store-set! store "Key" "") ;; Clear the string and try again.
              )
              (begin
                (glgui-widget-set! gui:popup popup-box 'callback #f)
                (store-set! store "popup-text" (list "BAD PIN" "Please enter a correct pin. (Contact Matthias [mgorges@cw.bc.ca] if you need one.)"))
                (show-popup)
                (store-set! store "Key" "") 
              )
            )
          )
          (glgui-widget-set! gui:login login-pin 'label "")
        )
        (begin
          (glgui-widget-set! gui:login login-pin 'label "") 
          (store-set! store "Key" "") 
          (glgui-widget-set! gui:popup popup-box 'callback #f)
          (set! mode MODE_SETUP)
        )
      )
    )
  )
)

;; -----------------------------------------------------------------------------
;;  OVERVIEW SCREEN RELATED FUNCTIONS
;; -----------------------------------------------------------------------------
;; Initialize the overview gui parts
(define gui:overview #f)	
(define (init-gui-overview)
    (set! gui:overview (make-glgui))

    (let* ((x 0)
	   (number-rows 8)
	   (height (* number-rows gui:row-height))
	   (y (- (glgui-height-get) gui:menu-height 16 3)))
      ;;Write Header Row
      (set! header
	(header-row gui:overview x y)
      )
      ;; Define the Overview List and populate with elements
      ;; position is x=0, y=65 (just above navigation), width=full, height=gui-navigation(65)-titlerow (60?)
      (set! numeric-list
	(glgui-list gui:overview x (- y 3 height) (glgui-width-get) height gui:row-height (build-numeric-list) overview-callback)
      )
    )
)

;;function to write header of the overview screen list 
(define (header-row g x y)
    ;; Place Location and Phase labels
    (glgui-pixmap g (+ x 5) y location.img)
    (glgui-pixmap g (+ x 85) y phase.img)
    ;; Place three vital signs labels and color them appropriately
    (set! HRlabel (glgui-pixmap g (+ x 150) y hr.img))
    (glgui-widget-set! g HRlabel 'color Green)
    (set! SPO2label(glgui-pixmap g (+ x 195) (- y 3) spo2.img))
    (glgui-widget-set! g SPO2label 'color White)
    (set! ETCO2label (glgui-pixmap g (+ x 235) (- y 2) etco2.img))
    (glgui-widget-set! g ETCO2label 'color Orange)
    (set! ALERTlabel (glgui-pixmap g (+ x 280) y alert.img))
    (glgui-widget-set! g ALERTlabel 'color White)
)

;; Function to write one row of data for a given OR
;; row is the row number and n is the OR number for which we show data
(define (numeric-list-element or-name)
  (lambda (g wgt x y w h s)
    ;; Find the approximate center of the height to shift the things up appropriately
    (let* ((y_shift (+ y (/ (- h 24) 2)))
	   ;; Number of message and according color
	   (number-of-messages (page-number-get or-name))
	   (highest-priority-value (highest-priority-value-of-pages-get or-name))
	   (popup-color (priority-color-get highest-priority-value)))

      ;; Place the correct OR label
      (glgui:draw-text-left (+ x 3) y_shift 82 24 or-name ascii_24.fnt 
	(if (= number-of-messages 0) White (if (>= highest-priority-value 0) popup-color White)))

      ;; Put correct phase icon
      (let ((phase-val (store-ref or-name "phase")))
	(if phase-val
	  (glgui:draw-pixmap-left (+ x 85) y_shift 30 24 (list-ref phase-labels phase-val) (if (= phase-val 0) DarkGray White))
	)
      )

      ;; Write the HR,SpO2,etCO2 values, and color them appropriately
      ;; glgui:draw-text-right takes arguments: pos_x pos_y width height String Font Color
      (let ((hr-val (store-ref or-name "hr"))
	    (hr-src (store-ref or-name "hr_source"))
	  )
	(if hr-val (glgui:draw-text-right (+ x 143) y_shift 37 24 (number->string (fix hr-val)) num_24.fnt (if hr-src (cond 
		((string=? hr-src "ECG1") Green)((string=? hr-src "PLETH") White)((string=? hr-src "BP1") Red)(else DarkGray)) Green)))
      )

      (let ((spo2-val (store-ref or-name "spo2")))
	(if spo2-val (glgui:draw-text-right (+ x 188) y_shift 37 24 (number->string (fix spo2-val)) num_24.fnt White))
      )      
      (let ((etco2-val (store-ref or-name "co2_et")))
	(if etco2-val (glgui:draw-text-right (+ x 233) y_shift 37 24 (number->string (fix (* 7.5 etco2-val))) num_24.fnt Orange))
      )

      ;; Place message popup windows to the right
      ;; See if we have messages, as we will color thing if we do
      (if (> number-of-messages 0)
	(begin
	  (glgui:draw-pixmap-left (- w 30) y_shift 30 28 popup.img popup-color)
	  (glgui:draw-text-center (- w 30) (+ y_shift 3) 30 28 (number->string number-of-messages) num_18.fnt popup-color)
	)
      )
    )
  )
)
;; Function to prefill the numerics list elements
;; Tail recursion in loop is the preferred way - so not if i<length do ...
;; returns the list of graphical elements
(define (build-numeric-list)
  ;;Start loop and declare am empty list result to be returned
  (let ((rooms (store-ref "main" "myRooms")))
    (if rooms
      (let loop ((i 0) (result (list)))
        ;;If we reached length, we return the result
        (if (= i (length rooms)) result
	   ;; this is the else case, which appends the list with another element 
	   (loop (+ i 1)(append result (list (numeric-list-element (list-ref rooms i)))))
        )
      )
      (list) ;;empty list returned if no room subscribed to
    )
  )
)

;; This function is called when an element in the Overview list is called
;; If the left part up to the vitals is selected, we go to the waveform screen
;; If the right part, where the popup icon is living, is selected, we go to the message details
(define (overview-callback g wgt t x y)
  (if (< x 270) ;; I want to split the click into GOTO-Waveform and GOTO-AlertDetail
    (begin
      ;;current is a float, which needs to be converted to INTEGER for list referencing
      (store-set! "main" "waveform-room-idx" (fix (glgui-widget-get g wgt 'current)))
      ;;clear the traces
      (for-each (lambda (l) (gltrace:clear l)) (list ecg-trace pleth-trace co2-trace art-trace))
      ;;clear the waveform stores
      (let ((or-name (list-ref (store-ref "main" "myRooms") (store-ref "main" "waveform-room-idx"))))
	(for-each (lambda (l) (store-clear! or-name l)) '("ECG1" "PLETH" "CO2" "INVP1"))
      )
      (set! rupi:last-wave-request 0.)(set! rupi:last-wave-update 0.) ;; Force immediate redraw
      ;;Log which screen is used
      (log-remote "Screen: Waveforms")
      (set! mode MODE_WAVES)
    )
    ;; In this case we hit the popup icon
    (let ((or-name (list-ref (store-ref "main" "myRooms") (fix (glgui-widget-get g wgt 'current)))))
      (if (> (page-number-get or-name) 0)
        (begin
          ;; Go to the message screen
          (glgui-widget-set! gui:menu navigation-bar 'value MODE_MESSAGING)
          (log-remote "Screen: Messaging")
          (set! mode MODE_MESSAGING)
          ;; Select a message screen list element and "click" it
          (glgui-widget-set! gui:messaging alert-list 'current (highest-priority-message-row-get or-name))
          (alert-select gui:messaging alert-list #f #f #f)
        )
      )
    )
  )
)

;; Returns the number of pages from a given LOCATION (e.g. BED5)
(define (page-number-get location)
  (let ((messages (store-ref "main" "AlertMessages")))
    (if (list-notempty? messages)
      (let loop ((i 0) (result 0))
	(if (= i (length messages)) result
	  (loop (+ i 1)(if (string=? (list-ref (list-ref messages i) 1) location) (+ result 1) result))
	)
      )
      0
    )
  )
)

;; -----------------------------------------------------------------------------
;;  MESSAGING SCREEN RELATED FUNCTIONS
;; -----------------------------------------------------------------------------
;; Initialize the messaging gui parts
(define gui:messaging #f)	
(define gui:messaging-quicktext #f)
(define gui:paging-response #f)
(define (init-gui-messaging)
  (set! gui:messaging (make-glgui))
  (let ((x 0)(y (- (glgui-height-get) gui:menu-height 16 3)))
    ;;Header row
    (glgui-pixmap gui:messaging (+ x 5) y source.img)
    (glgui-pixmap gui:messaging (+ x 80) (- y 3) message.img)
    (glgui-label gui:messaging (+ x 265) (- y 3) 50 18 "When" ascii_16.fnt White)
    ;;List of received alerts
    (set! alert-list
      (glgui-list gui:messaging 0 (+ 60 (- y 65 (* 3 40))) (glgui-width-get) (* 3 40) 40 (build-alert-list) alert-select)
    )
    ;;List of received messages from people
    (set! chat-user-list
      (glgui-list gui:messaging 0 (+ 60 (- y 65 (* 7.5 40))) (glgui-width-get) (* 4 40) 40 (build-chat-user-list) chat-user-select)
    )
    ;; Bottom row with secondary navigation buttons
    (set! compose-button
      (glgui-button-string gui:messaging 10 (+ gui:navigation-height 10) (- (glgui-width-get) 20) 30 "Compose New Message" ascii_24.fnt compose-button-callback)	
    )
  )
)

;; Switch to hidden user screen to send message
(define (compose-button-callback g w t x y)
  (set! mode MODE_USERS)
  (log-remote "Screen: Users")
)

;; Builds the list of message rows for the alert-list based on the "main" store's events list
(define (build-alert-list)
  (let ((alerts (store-ref "main" "AlertMessages")))
    (if (list-notempty? alerts)
      (let loop ((i 0) (result (list)))
        (if (= i (length alerts)) 
          result
          (loop (+ i 1)(append result (list (alert-list-row (list-ref alerts i)))))
        )
      )
      '()
    )
  )
)

;; Draw an individual message row: Sender, Message Text
(define (alert-list-row alert)
  (lambda (g wgt x y w h s)
    ;; Find the approximate center of the height to shift the things up appropriately
    (define y_shift (+ y (/ (- h 24) 2)))
    ;;Color the box background in alarm color if such a message was received
    (let ((priority (list-ref alert 3)))
      (if (not (= priority 0)) (glgui:draw-box x y 75 h (priority-color-get priority)))	
    )
    ;;Add the Source name
    (set! source (glgui:draw-text-left (+ x 5) y_shift 70 24 (list-ref alert 1)  ascii_16.fnt White))
    
    ;;Add received time
    (let ((time (- (store-ref "main" "LastUpdateTime") (list-ref alert 0))))
      (if (and time (> time 30))
	(begin
	  (glgui:draw-text-left (+ x 265) (+ y 3 16) 90 16 (string-append 
	    (cond 
	      ((< time 60) (string-append (number->string (fix time)) "sec")) ;;Jonathan didn't like this
	      ((and (> time 60) (< time 3600)) (string-append (number->string (fix (/ time 60))) "min"))
	      ((and (> time 3600) (< time 86400)) (string-append (number->string (fix (/ time 3600))) "hr"))
	      (else (string-append (number->string (fix (/ time 86400))) "days"))
	    )) ascii_16.fnt DarkGray)
	  (glgui:draw-text-left (+ x 265) (+ y 3) 90 16 "ago" ascii_16.fnt DarkGray)
	)
      )
    )
    ;; This is if a message is replied to
    (if (list-ref alert 4)	
      (begin
        (if (> (string-length (list-ref alert 4)) 0) 
          (glgui:draw-pixmap-left (+ x 75 2) (- y_shift 7) 23 14 reply-arrow.img LightGray)
        )
        (set! reply (glgui:draw-text-left (+ x 75 2 25) (- y_shift 5) 150 16 (list-ref alert 4) ascii_16.fnt LightGray))
        (set! y_shift (+ y_shift 6))
      )
    )
    ;; And finally the message itself
    (set! message (glgui:draw-text-left (+ x 75) y_shift 185 24 (list-ref alert 2) ascii_24.fnt White))
  )
)

;; Load the alert detail into Alert gui and display it
(define selected-alert-number #f)
(define (alert-select g wgt t x y)
  ;;Need to store the index of the currently selected message to reply to in the response callback
  (set! selected-alert-number (fix (glgui-widget-get g wgt 'current)))

  (let ((alerts (store-ref "main" "AlertMessages")))
    (if (list-notempty? alerts)
      (let* ((alert (list-ref alerts selected-alert-number))
             (src (list-ref alert 1))
             (prio (list-ref alert 3)))
        ;; Load the right alert row into the list so we can see times, sender and response details.
        (glgui-widget-set! gui:alert alert-detail-list 'list (list (alert-list-row alert)))
        ;; Hide/show the right response buttons [prio < 0 is a repeat view of alert]
        (hide-alert-close-button (fx> prio 0))
        ;; Add the message
        (glgui-widget-set! gui:alert response-string 'label (string-append "Reply to " src ":"))
        ;; Color the priority accordingly
        (glgui-widget-set! gui:alert response-string 'color (priority-color-get (if (fx< prio 0) (+ prio 10) prio)))
        (glgui-widget-set! gui:alert response-detail 'color (priority-color-get (if (fx< prio 0) (+ prio 10) prio)))
        ;; Check if this is a room transfer request/page or a reminder message
        (if (= (length alert) 6)
          (begin ;; Transfer or Page
            (let ((str (list-ref alert 2)))
              (if (and (= (string-length str) 15) (string=? (substring str 0 15) "Transfer rooms?"))
                (glgui-widget-set! gui:alert response-detail 'label "Could you cover my rooms?")
                (glgui-widget-set! gui:alert response-detail 'label 
                  (string-append "Please come to " src (if (or (= prio 2) (= prio -8)) " soon." " now!")))
              )
            )
            (let ((str (string-split (list-ref alert 5) #\;)))
              (glgui-widget-set! gui:alert response-vitals 'label (car str))
              (glgui-widget-set! gui:alert response-vitals2 'label (cadr str))
              (glgui-widget-set! gui:alert response-vitals3 'label (if (fx= (length str) 3) (caddr str) ""))
            )
          )
          (begin ;; Reminder
            (glgui-widget-set! gui:alert response-detail 'label (string-append "Reminder: " (list-ref alert 2)))
            (glgui-widget-set! gui:alert response-vitals 'label "")
            (glgui-widget-set! gui:alert response-vitals2 'label "")
            (glgui-widget-set! gui:alert response-vitals3 'label "")
          )
        )
        ;; And finally go to the ALERT SCREEN
        (log-remote "Screen: Alert")
        (set! mode MODE_ALERT)
      )
    )
  )
)

(define (hide-alert-close-button b)
  (glgui-widget-set! gui:alert accept-alert-screen-button 'hidden (not b))
  (glgui-widget-set! gui:alert ignore-alert-screen-button 'hidden (not b))
  (glgui-widget-set! gui:alert close-alert-screen-button 'hidden b)
  (glgui-widget-set! gui:alert clear-alert-screen-button 'hidden b)
)


;; This function gets called when the user selects a response to an alert or sends a chat message
;; It generates and sends the reply and adds it to the appropriate local message list
(define (reply-callback g w t x y)
  (if (fx= mode MODE_CHAT)
    ;; This is the message sending part for chat type messages
    (let ((reply (car (glgui-widget-get g w 'image)))
          (destination (store-ref "main" "ChatReceiver")))
      ;; Make sure the string is not empty
      (if (> (string-length reply) 0) (begin
        ;; Quickly log which message button was used [No logging of messages locally! - unsecure]
        ;; (log-remote (string-append "Message: " reply))
        ;; Send actual message (and replace all colons so we don't choke on them).
        (rupi-cmd (store-ref "main" "RupiClient" #f) "SENDMESSAGE" (store-ref "main" "Key" #f) destination 
          (string-replace-char reply #\: #\.))
        ;; Add message to chat store
        (store-set! "main" "ChatMessages" (append
          (list (list ##now destination reply 1))
          (store-ref "main" "ChatMessages" '())
        ))
        (glgui-widget-set! gui:chat chat-list 'list (build-chat-list destination))
        (glgui-widget-set! gui:chat-landscape chat-list-landscape 'list (glgui-widget-get gui:chat chat-list 'list))
        ;; Update Messaging screen
        (glgui-widget-set! gui:messaging chat-user-list 'list (build-chat-user-list))
      ))
    )
    ;; This is a message sending part for alert/page/reminder messages
    (let* ((reply (car (glgui-widget-get g w 'image)))
           (alerts (store-ref "main" "AlertMessages"))
           (destination (cadr (list-ref alerts selected-alert-number))))
      ;; Quickly log which message button was used [No logging of messages locally! - unsecure]
      ;; (log-remote (string-append "Message: " reply))
      ;;REPLY: Reset the priority of the replied to message to (VAL-10) to make color darker 
      (alert-markseen selected-alert-number reply)

      ;; Check if this was a room transfer
      (let ((str (caddr (list-ref alerts selected-alert-number))))
        (if (string=? str "Transfer rooms?")
          ;; Was a room transfer so deal with it 
          (let ((rc (store-ref "main" "RupiClient" #f))
                (key (store-ref "main" "Key" #f)))
            (rupi-cmd rc "SENDMESSAGE" key destination 
              (if (char=? (string-ref reply 0) #\I) "Transfer rejected" "Transfer accepted"))
            ;; If I take the rooms
            (if (char=? (string-ref reply 0) #\O)
              (begin
                (rupi-cmd rc "TRANSFERROOMS" key destination)
                (store-set! "main" "myRooms" (rupi-cmd rc "GETMYROOMS" key))
                (update-room-lists)
              )
            )
          )
          ;;Just a message that needs replying to
          (rupi-cmd (store-ref "main" "RupiClient" #f) "SENDMESSAGE" (store-ref "main" "Key" #f) destination 
            (list reply (car (list-ref alerts selected-alert-number)))
          )
        )
      )
      ;;Needed otherwise new text doesn't show immediately
      (glgui-widget-set! gui:messaging alert-list 'list (build-alert-list)) 
      ;; Finally, go back to messaging screen
      (log-remote "Screen: Messaging")
      (set! mode MODE_MESSAGING)
    )
))

;; This function marks a message as seen (we do so by decreasing its value by 10)
;; also store the reply so we can show it with the message
(define (alert-markseen message-num reply)
  (let* ((alerts (store-ref "main" "AlertMessages"))
         (alert (list-ref alerts message-num)))
    (if (> (list-ref alert 3) 0)
      (begin
	(list-set! alert 3 (- (list-ref alert 3) 10))
	(list-set! alert 4 reply)
	(list-set! alerts message-num alert)
	(store-set! "main" "AlertMessages" alerts)
      )
    )
  )
)

;; Returns the row number of the message with the highest priority 
(define (highest-priority-message-row-get location)
  (let ((messages (store-ref "main" "AlertMessages")))
    (if (list-notempty? messages)
      (let loop ((i (- (length messages) 1)) (result (list -10 0)))
	(if (< i 0) (list-ref result 1)
	  (loop (- i 1)(let ((row (list-ref messages i))) 
			(if (string=? (list-ref row 1) location) 
			  (if (> (list-ref row 3) (list-ref result 0)) (list (list-ref row 3) i) result)
			  result
			)
		      )
	  )
	)
      )
      0
    )
  )
)

;; Returns the highest priority of all messages for a given LOCATION
(define (highest-priority-value-of-pages-get location)
  (let ((messages (store-ref "main" "AlertMessages")))
    (if (list-notempty? messages)
      (let loop ((i (- (length messages) 1)) (result -10))
	(if (< i 0) result
	  (loop (- i 1)(if (string=? (list-ref (list-ref messages i) 1) location) 
			(let ((row (list-ref messages i))) 
			  (if (> (list-ref row 3) result) (list-ref row 3) result)
			)
			result
		      )
	  )
	)
      )
      0
    )
  )
)

;; This function updates the message list with the new entries received from the server
(define (store-update-messages data) 
  (store-set! "main" "LastUpdateTime" (car data))
  (store-set! "main" "LastUpdateTimeLocal" ##now) ;; This is needed in case Apple thinks their time is not NTP time
  (glgui-widget-set! gui:menu clock 'label (seconds->string (car data) "%T"))
  (if (list-notempty? (cdr data))
    (begin
      (for-each parse-message (reverse (cdr data))) ;; Reverse is needed as we want older messages first
      (glgui-widget-set! gui:messaging alert-list 'list (build-alert-list))
      (glgui-widget-set! gui:messaging chat-user-list 'list (build-chat-user-list))
      ;; Make sure to show latest message
      (glgui-widget-set! gui:messaging alert-list 'offset 0) 
      (glgui-widget-set! gui:messaging chat-user-list 'offset 0) 
      ;; Also add popup if not already on messaging screen
      (let ((prio (caddr (car (cdr data)))))
        (if (not (or (fx= mode MODE_MESSAGING) (fx= prio 0)))
          (let ((msgs (if (fx= prio 1) (store-ref "main" "ChatMessages")(store-ref "main" "AlertMessages"))))
            (if (not (fx= mode MODE_CHAT))
              ;; All other screens: show the popup!
              (store-set! "main" "popup-text" (list (string-append (cadar msgs) " send:") (caddar msgs)))
              ;; For chat screen check receiver first
              (if (string=? (cadar msgs) (store-ref "main" "ChatReceiver" ""))
                ;; The chat message list needs updating
                (begin 
                  (glgui-widget-set! gui:chat chat-list 'list (build-chat-list (store-ref "main" "ChatReceiver")))
                  (glgui-widget-set! gui:chat-landscape chat-list-landscape 'list (glgui-widget-get gui:chat chat-list 'list))
                )
                ;; Otherwise we still need a popup anyway
                (store-set! "main" "popup-text" (list (string-append (cadar msgs) " send:") (caddar msgs)))
              )
            )
          )
        )
      )
    )
  )
)

;; This function splits the message string into the respective fields as needed.
(define (parse-message msg)
  (let* ((time (car msg))
	 (mid (string-split (cadr msg) #\:)) ;; We need cadr as cdr still has a () around - \: needs to be escaped with #
	 (str (string-split (cadr mid) #\|))
	 (prio (caddr msg))	;; This gives the car of the cdr of the cdr of msg)
	)
    ;; Play alarm sound when receiving message
    (cond
      ((fx= prio 1) (audiofile-forceplay audio:message))
      ((fx= prio 2) (audiofile-forceplay audio:alert))
      ((fx= prio 3) (audiofile-forceplay audio:emergency))
    )
    ;; Handle a recent voice call request
    (if (and (fx= prio 0) (fl< (fl- ##now time) 15.) voip:enabled)
      (handle-voip (car mid) (string->number (car str)) (string->number (cadr str)))
    )
    ;; This is the transfer message acceptance
    (if (string=? (car str) "Transfer accepted")
      (let ((rc (store-ref "main" "RupiClient" #f))
            (key (store-ref "main" "Key")))
        (store-set! "main" "myRooms" (rupi-cmd rc "GETMYROOMS" key))
        (update-room-lists)
        (store-clear! "main" "RoomSendTime")
        (glgui-widget-set! gui:rooms transfer-button 'hidden #f)
      )
    )
    ;; On transfer rejection allow another transfer
    (if (string=? (car str) "Transfer rejected")
      (begin
	(store-clear! "main" "RoomSendTime")
	(glgui-widget-set! gui:rooms transfer-button 'hidden #f)
      )
    )
    ;; Add to ChatMessage log if Prio == 1, else to AlertMessage list
    (if (fx= prio 1)
      ;; The message list order is: TIMESTAMP, SOURCE, MESSAGE_TEXT, SENDER
      ;; Messages from yourself are not shown.
      (if (not (string=? (car mid) (store-ref "main" "UserName" "")))
        (store-set! "main" "ChatMessages" (append
          (list (list time (car mid) (car str) 0))
          (store-ref "main" "ChatMessages" '())
        ))
      )
    )
    (if (or (fx= prio 2) (fx= prio 3))
      ;; The message list order is: TIMESTAMP, SOURCE, MESSAGE_TEXT, PRIORITY
      (store-set! "main" "AlertMessages" (append
        (list (if (= (length str) 1)
          (list time (car mid) (car str) prio #f)      
          (list time (car mid) (car str) prio #f (cadr str))
        ))
        (store-ref "main" "AlertMessages" '())
      ))
    )
  )
)

;; -----------------------------------------------------------------------------
;; ALERT/PAGING SCREEN RELATED FUNCTIONS
;; -----------------------------------------------------------------------------
(define (init-gui-alert)
  (set! gui:alert (make-glgui))
  (let ((x 0)(y (- (glgui-height-get) gui:menu-height 16 3))(w (glgui-width-get)))

    ;;Header row
    (glgui-pixmap gui:alert (+ x 5) y source.img)
    (glgui-pixmap gui:alert (+ x 80) (- y 3) message.img)
    (glgui-label gui:alert (+ x 265) (- y 3) 50 18 "When" ascii_16.fnt White)
    ;;List of received alerts
    (set! alert-detail-list
      (glgui-list gui:alert 0 (+ 60 (- y 65 40)) (glgui-width-get) 40 40 '() #f)
    )
    ;;Text response part
    (set! response-string (glgui-label gui:alert x (- y 175) w 40 "" ascii_24.fnt White))
    (glgui-widget-set! gui:alert response-string 'align GUI_ALIGNCENTER)
  
    (set! response-detail (glgui-label gui:alert x (- y 210) w 40 "" ascii_24.fnt White))
    (glgui-widget-set! gui:alert response-detail 'align GUI_ALIGNCENTER)
    (set! response-vitals (glgui-label gui:alert (+ x 5) (- y 255) (- w 10) 40 "" ascii_20.fnt White))
    (set! response-vitals2 (glgui-label gui:alert (+ x 5) (- y 255 20) (- w 10) 40 "" ascii_20.fnt White))
    (set! response-vitals3 (glgui-label gui:alert (+ x 5) (- y 255 20 20) (- w 10) 40 "" ascii_20.fnt White))
    
    ;; Buttons on the bottom to accept, ignore or return to main screen
    (set! ignore-alert-screen-button
      (glgui-button-string gui:alert 10 (+ gui:navigation-height 10) (- (/ w 2) 20) 30 "Ignore" ascii_24.fnt reply-callback)
    )
    (set! accept-alert-screen-button
      (glgui-button-string gui:alert (+ (/ w 2) 10) (+ gui:navigation-height 10) (- (/ w 2) 20) 30 "Okay" ascii_24.fnt reply-callback)
    )
    (set! close-alert-screen-button
      (glgui-button-string gui:alert 10 (+ gui:navigation-height 10) (- w 20) 30 "Return to Messages" ascii_24.fnt return-message-screen-button-callback)
    )
    (set! clear-alert-screen-button
      (glgui-button-string gui:alert (+ (/ w 2) 10) (+ gui:navigation-height 10 40) (- (/ w 2) 20) 30 "Remove/Clear" ascii_24.fnt clear-message-button-callback)	
    )
  )
)

;; -----------------------------------------------------------------------------
;; LANDSCAPE CHAT SCREEN RELATED FUNCTIONS
;; -----------------------------------------------------------------------------
(define (init-gui-chat-landscape)
  (set! gui:chat-landscape (make-glgui))
  (let ((x 0)
        (y 0)
        (w (glgui-height-get))
        (h (glgui-width-get))
        (g gui:chat-landscape))

    ;;List of chat messages
    (set! chat-list-landscape
      (glgui-chat g (+ x 5) (+ (/ h 2) 35) (- w 65 5 5 5) (- (/ h 2) 35) 16 (list) ascii_16.fnt #f)
    )
    ;; Prompt and message string
    (set! message-string-landscape
      (glgui-label g (+ x 5) (+ (/ h 2) 5) (- w 65 5 5 5) 30 "" ascii_20.fnt White (color-shade White 0.1))
    )
    (glgui-widget-set! g message-string-landscape 'align GUI_ALIGNRIGHT)
    (glgui-button-string g (- w 65 5) (+ (/ h 2) 5) 65 30 "Send" ascii_24.fnt send-message-callback)

    ;; Add landscape keyboard
    (set! keypad-landscape (glgui-ioskeypad g x y))
    (glgui-widget-set! g keypad-landscape 'landscape #t)

    ;; Return button - needed to move to the top right corner.
    (glgui-button-string g (- w 65 5) (* (/ h 4) 3) 65 (/ h 8) "Back" ascii_24.fnt return-messaging-button-callback)
  )
)

;; -----------------------------------------------------------------------------
;; CHAT SCREEN RELATED FUNCTIONS
;; -----------------------------------------------------------------------------
(define gui:messaging-detail-shown #f)

(define (init-gui-chat)
  (set! gui:chat (make-glgui))
  (let ((x 0)(y (+ 30 10)))

    ;;List of chat messages
    (set! chat-list
      (glgui-chat gui:chat x y (glgui-width-get) (* 23 18) 16 (list) ascii_16.fnt #f)
    )

    ;; Pre-defined quick text messages
    (set! gui:messaging-quicktext (make-glgui))
    (init-message-quicktext gui:messaging-quicktext x y)
    ;; Or alternatively freetext entry using a keyboard 
    (set! gui:messaging-keyboard (make-glgui))
    (init-message-keyboard gui:messaging-keyboard x y)
  )

  ;; Bottom row with secondary navigation buttons
  (glgui-button-string gui:chat 10 3
    (- (/ (glgui-width-get) 2) 20) 30 "Back" ascii_24.fnt return-messaging-button-callback)
  (set! chat-input-button (glgui-button-string gui:chat (+ (/ (glgui-width-get) 2) 10) 3 
    (- (/ (glgui-width-get) 2) 20) 30 "Quicktext" ascii_24.fnt quicktext-button-callback))
)

;; Make the on-screen keyboard for text entry
(define (init-message-keyboard g x y)
  (let ((w (glgui-width-get))
        (h (glgui-height-get)))    
    ;; Prompt and message string
    (set! message-string (glgui-label g (+ x 5) (+ y (/ (glgui-width-get) 1.5) 5) (- w 65 5 5 5) 30 "" ascii_20.fnt White (color-shade White 0.1)))
    (glgui-widget-set! g message-string 'align GUI_ALIGNRIGHT)
    
    ;; Add the keyboard and the send button.
    (set! keypad (glgui-ioskeypad g x y))
    (glgui-button-string g (- w 65 5) (+ y (/ (glgui-width-get) 1.5) 5) 65 30 "Send" ascii_24.fnt send-message-callback)
    ;; Save value for later
    (set! gui:messaging-keyboard-height (+ (/ (glgui-width-get) 1.5) 5 30 5))
  )
)

;; This function makes the elements for a message reply or shows more details depending on the source
(define (init-message-quicktext g x y) 
  (let loop ((i 0))
    (if (< i 6)
      (let ((w (/ (- (glgui-width-get) 60) 2)))	
	(glgui-button-string g (+ x 20) (+ y (* 45 i)) w 35 (list-ref message-texts (+ (* i 2) 0)) ascii_16.fnt send-message-callback)
	(glgui-button-string g (+ x w 20 20) (+ y (* 45 i)) w 35 (list-ref message-texts (+ (* i 2) 1)) ascii_16.fnt send-message-callback)
	(loop (+ i 1))
      )
      (set! gui:messaging-quicktext-height (+ (* 45 i) 5))
    )
  )
)

;; Build the List of Users, with whom we have chatted before. It will only show the most current message for each user.
(define (build-chat-user-list)
  (let ((chats (store-ref "main" "ChatMessages")))
    (if (list-notempty? chats)
      (let loop ((i 0) (result (list)) (people (list)))
        (if (= i (length chats)) 
          (begin 
            (store-set! "main" "ChatUsers" people)
            result
          )
          (let* ((msg (list-ref chats i))
                 (person (cadr msg)))
            ;; Test if the list already has an entry of a given person, if not add the first one we occur.
            ;; List is in display order, so append on the bottom!
            (loop (+ i 1)
                  (if (member person people) result (append result (list (chat-user-list-element msg))))
                  (if (member person people) people (append people (list person))))
          )
        )
      )
      '()
    )
  )
)

;; Draw an individual chat message row: Sender, Message Text
(define (chat-user-list-element msg)
  (lambda (g wgt x y w h s)
    ;;Color the box background in chat color if it was received
    (if (fx= (cadddr msg) 0)
      (glgui:draw-box x y 75 h (priority-color-get 1))
    )
    ;; Find the approximate center of the height to shift the things up appropriately    
    (let ((y_shift (+ y (/ (- h 24) 2))))
      ;;Add the Source name
      (if (fx= (cadddr msg) 0)
        (glgui:draw-text-left (+ x 5) y_shift 70 24 (cadr msg) ascii_16.fnt White)
        (begin
          (glgui:draw-text-left (+ x 5) (+ y 14) 70 24 (cadr msg) ascii_16.fnt White)
          (glgui:draw-pixmap-left (+ x 5 23) (+ y 4) 23 14 reply-arrow.img LightGray)
        )
      )
      ;;Add received time
      (let ((time (- (store-ref "main" "LastUpdateTime") (car msg))))
        (if (and time (> time 30))
          (begin
            (glgui:draw-text-left (+ x 265) (+ y 3 16) 90 16 (string-append 
              (cond 
                ((< time 60) (string-append (number->string (fix time)) "sec")) ;;Jonathan didn't like this
                ((and (> time 60) (< time 3600)) (string-append (number->string (fix (/ time 60))) "min"))
                ((and (> time 3600) (< time 86400)) (string-append (number->string (fix (/ time 3600))) "hr"))
                (else (string-append (number->string (fix (/ time 86400))) "days"))
              )) ascii_16.fnt DarkGray)
            (glgui:draw-text-left (+ x 265) (+ y 3) 90 16 "ago" ascii_16.fnt DarkGray)
          )
        )
      )
      ;; And finally the message itself
      (let ((str (string-split-width (caddr msg) 185 ascii_16.fnt)))
        (if (= (length str) 1) 
          (glgui:draw-text-left (+ x 75) y_shift 185 24 (car str) ascii_16.fnt White)
          (begin
            (glgui:draw-text-left (+ x 75) (+ y 16 1) 185 24 (car str) ascii_16.fnt White)
            (glgui:draw-text-left (+ x 75) y 185 24 (cadr str) ascii_16.fnt White)
          )
        )
      )
    )
  )
)

(define (chat-user-select g w t x y)
  (let ((msgs (if (fx= mode MODE_USERS) (store-ref "main" "Users") (store-ref "main" "ChatUsers"))))
    (if (list-notempty? msgs)
      (let* ((user (list-ref msgs (fix (glgui-widget-get g w 'current))))
             (username (if (fx= mode MODE_USERS) (car user) user)) ;; The userlist also has a login status field
             (login (store-ref "main" "UserName"))
             (lbl (string-append "Chat: " username)))
        (if (not (string=?  username login))
          (begin
            (glgui-widget-set! gui:menu title 'label lbl)
            (glgui-widget-set! gui:chat chat-list 'list (build-chat-list username))
            (glgui-widget-set! gui:chat-landscape chat-list-landscape 'list (glgui-widget-get gui:chat chat-list 'list))
            (store-set! "main" "ChatReceiver" username)
            ;; Hide the main navigation bar
            (glgui-widget-set! gui:menu navigation-bar 'hidden #t)
            (glgui-widget-set! gui:menu message-number 'hidden #t)
            (glgui-widget-set! gui:menu reminder-number 'hidden #t)
            ;; If we go there with the compose to user option show a response option!
            (if (fx= mode MODE_USERS) (change-response-button gui:messaging-quicktext))
            ;; Got to chat screen
            (log-remote "Screen: Chat")
            (set! mode MODE_CHAT) 
          )
          (store-set! "main" "popup-text" "You can't send a message to yourself.")
        )
      )
    )
  )
)

(define (build-chat-list user)
  (let ((chats (store-ref "main" "ChatMessages")))
    (if (list-notempty? chats)
      (let loop ((i 0) (result (list)))
        (if (= i (length chats)) 
          result
          ;; Avoid accidential list reverse here, its already sorted so just append at the end.
          (loop (+ i 1) (if (string=? (cadr (list-ref chats i)) user) (append result (list (list-ref chats i))) result))
        )
      )
      '()
    )
  )
)

;; Functions to return to the Messaging Screen
(define (return-messaging-button-callback g w t x y)
  ;; Show the navigation bar
  (glgui-widget-set! gui:menu navigation-bar 'hidden #f)
  (glgui-widget-set! gui:menu message-number 'hidden #f)
  (glgui-widget-set! gui:menu reminder-number 'hidden #f)
  ;; Restore previous height if response option currently shown
  (restore-chat-list-height)
  ;; Hide the detail gui and switch screen to messaging  
  (set! gui:messaging-detail-shown #f)
  (log-remote "Screen: Messaging")
  (set! mode MODE_MESSAGING)
  (if landscape? (glgui-orientation-set! GUI_PORTRAIT))
)

;; Functions to switch between keyboard and quicktext
(define (keyboard-button-callback g w t x y)
  (change-response-button gui:messaging-keyboard)
)
(define (quicktext-button-callback g w t x y)
  (change-response-button gui:messaging-quicktext)
)
(define (change-response-button new-gui)
  (let ((gui gui:chat)
        (lst-wgt chat-list)
        (button-wgt chat-input-button)
        (dh (if (eq? new-gui gui:messaging-keyboard) gui:messaging-keyboard-height gui:messaging-quicktext-height))
        (im (if (eq? new-gui gui:messaging-keyboard) (list "Quicktext") (list "Keyboard")))
        (cb (if (eq? new-gui gui:messaging-keyboard) quicktext-button-callback keyboard-button-callback)))
    ;; Restore previous height if response option currently shown
    (restore-chat-list-height)
    ;; Trim the chat list to fit the response option and toggle the button
    (glgui-widget-set! gui lst-wgt 'h (- (glgui-widget-get gui lst-wgt 'h) dh))
    (glgui-widget-set! gui lst-wgt 'y (+ (glgui-widget-get gui lst-wgt 'y) dh))
    (glgui-widget-set! gui button-wgt 'image im)
    (glgui-widget-set! gui button-wgt 'callback cb)
  )
  ;; And finally activate the new response gui
  (set! gui:messaging-detail-shown new-gui)
)

(define (restore-chat-list-height)
  (let ((gui gui:chat)
        (lst-wgt chat-list))
    ;; Restore previous height if response option currently shown
    (if (eq? gui:messaging-detail-shown gui:messaging-quicktext)
      (begin    
        (glgui-widget-set! gui lst-wgt 'h (+ (glgui-widget-get gui lst-wgt 'h) gui:messaging-quicktext-height))
        (glgui-widget-set! gui lst-wgt 'y (- (glgui-widget-get gui lst-wgt 'y) gui:messaging-quicktext-height))
      )
    ) 
    (if (eq? gui:messaging-detail-shown gui:messaging-keyboard)
      (begin    
        (glgui-widget-set! gui lst-wgt 'h (+ (glgui-widget-get gui lst-wgt 'h) gui:messaging-keyboard-height))
        (glgui-widget-set! gui lst-wgt 'y (- (glgui-widget-get gui lst-wgt 'y) gui:messaging-keyboard-height))
      )
    )
  )
)

;; Function to send a chat message
(define (send-message-callback g w t x y)
  ;; Send the actual message
  (if (eq? gui:messaging-detail-shown gui:messaging-quicktext) 
    (reply-callback g w #f 0 0)
    (begin 
      (glgui-widget-set! g message-string 'image (list (glgui-widget-get gui:messaging-keyboard message-string 'label)))
      (glgui-widget-set! g message-string 'label "")
      (reply-callback g message-string #f 0 0)
    )
  )
  ;; And reset the chat list while hiding response option 
  (let ((gui gui:chat)
        (button-wgt chat-input-button)
        (im (if (eq? gui:messaging-detail-shown gui:messaging-quicktext) 
          (list "Quicktext") (list "Keyboard")))
        (cb (if (eq? gui:messaging-detail-shown gui:messaging-quicktext) 
          quicktext-button-callback keyboard-button-callback)))
    (restore-chat-list-height)
    (set! gui:messaging-detail-shown #f)
    ;; Also reset the button to it's previous state
    (glgui-widget-set! gui button-wgt 'image im)
    (glgui-widget-set! gui button-wgt 'callback cb)
  )
  ;; Hide the return key highlight on the keyboard if pressed instead of send
  (if (eq? g gui:messaging-keyboard)
    (glgui-widget-set! g keypad 'highlight #f)
  )
)

;; -----------------------------------------------------------------------------
;; USERS SCREEN RELATED FUNCTIONS
;; -----------------------------------------------------------------------------
(define gui:users #f)
(define (init-gui-users)
  (set! gui:users (make-glgui))
  (let ((x 0)(y (- (glgui-height-get) gui:menu-height 16 3)))
    ;;Header row
    (glgui-label gui:users (+ x 5) y 160 16 "Compose Message To" ascii_16.fnt White)

    ;;List of users (bright are online, dark are not)
    (set! user-list
       (glgui-list gui:users 0 (- y 5 (* 7 40)) (glgui-width-get) (* 7 40) 40 (build-user-list) chat-user-select)
    )
    ;; Button to return to message screen
    (set! return-message-screen-button
      (glgui-button-string gui:users 10 (+ gui:navigation-height 10) (- (glgui-width-get) 20) 30 "Return to Messages" ascii_24.fnt return-message-screen-button-callback)	
    )
  )
)

(define (return-message-screen-button-callback g w t x y)
  (log-remote "Screen: Messaging")
  (set! mode MODE_MESSAGING)
)

(define (clear-message-button-callback g w t x y)
  (let* ((message-num (glgui-widget-get gui:messaging alert-list 'current))
         (alerts (store-ref "main" "AlertMessages"))
         (alert (list-ref alerts message-num)))
    (store-set! "main" "AlertMessages" (list-delete-item alerts alert))
    (glgui-widget-set! gui:messaging alert-list 'list (build-alert-list))
  )
  (log-remote "Screen: Messaging")
  (set! mode MODE_MESSAGING)
)

(define (build-user-list)
  (let ((users (store-ref "main" "Users")))
    (if (list-notempty? users)
      (let loop ((i 0) (result (list)))
	;;If we reached length, we return the result
	(if (= i (length users)) result
	  ;; this is the else case, which appends the list with another element 
	  (loop (+ i 1)(append result (list (user-list-element (list-ref users i) i))))
	)
      )
      '()
    )
  )
)

(define (user-list-element user idx)
  (lambda (g wgt x y w h s)
    (define y_shift (+ y (/ (- h 24) 2)))    
    ;; Write Users and make Gray if not currently logged in
    (if gui:messaging-detail-shown ;;Highlight selected in DimGray [% Jonathan didn't like the Blue]
      (if (= idx selected-alert-number) (glgui:draw-box x y w h DimGray))
    )
    (glgui:draw-text-left (+ x 5) y_shift 200 24 (car user) ascii_24.fnt (if (string=? (car user) (store-ref "main" "UserName" "")) Blue (if (= (cadr user) 1) White DarkGray)))  
  )
)

;; -----------------------------------------------------------------------------
;;  TREND SCREEN RELATED FUNCTIONS
;; -----------------------------------------------------------------------------
;; Initialize the trend gui parts
(define gui:trends #f)	
(define trend-traces '())
(define (init-gui-trends)
  (set! gui:trends (make-glgui))

  ;; Define scales for Trends
  (set! LARGE_min 0)(set! LARGE_max 200)(set! SMALL_max 100)

  ;; Define trace data source names
  (set! trend-source-names (list "hr_ecg" "pr" "spo2" "co2_et" 
                         "nibp_sys" "nibp_dia" "nibp_mean" 
                         "p1_sys" "p1_dia" "p1_mean" 
                         "co2_rr" "bis" "temp1"
                         "o2_et" "o2_fi" "n2o_et" "n2o_fi" "aa_et" "aa_fi"))
  ;;Define traces to plot trends
  (let ((trace-mode GLTRACE_SHIFT) (w 180) (h 200) (hsmall 100))
    (let loop ((i 0) (ret (list)))
      (if (fx= i (length trend-source-names)) (set! trend-traces ret)
        (loop (fx+ i 1) (append ret (list (make-gltrace w (if (fx> i 12) hsmall h) trace-mode LARGE_min (if (fx> i 12) SMALL_max LARGE_max) LARGE_min (if (fx> i 12) SMALL_max LARGE_max)))))
      )
    )
  )
  (for-each (lambda (l) (gltrace:clear l)) trend-traces)
  ;; Define the trace label texts and colors
  (set! trend-label-texts (list "HR" "PR" "SpO2" "etCO2" 
                         "" "" "NIBPm" 
                         "" "" "ARTm" 
                         "RR" "BIS" "Temp"
                         "O2" "" "N2O" "" "AA" ""))
  (set! trend-trace-colors (list Green LightGreen White Orange Red Red Red Red Red Red Blue Yellow DarkRed
                LightGray DarkGray LightBlue DarkBlue LightYellow Yellow))

  ;;Place the Trace Widgets
  (let ((x 5)(y (- (glgui-height-get) gui:menu-height 200)) (w 270) (h 200) (hsmall 100) (c DimGray))
    ;; Coordinate system
    (glgui-box gui:trends x y 2 h c)
    (glgui-box gui:trends x (- y 110) 2 hsmall c)
    (glgui-label gui:trends (+ x 3) (- (+ y h) 3) 20 12 "200" ascii_12.fnt LightGray)
    (glgui-label gui:trends (+ x 3) (- (+ y (/ h 2)) 3) 20 12 "100" ascii_12.fnt LightGray)
    (glgui-box gui:trends x y w 2 c)
    (glgui-box gui:trends  x (- y 110) w 2 c)
    (glgui-label gui:trends (+ x 3) (+ (- y 110 3) hsmall) 20 12 "100" ascii_12.fnt LightGray)
    ;; OR Case Time Label
    (set! case-time (glgui-label gui:trends (+ x (/ w 4)) (- (+ y h) 12) (/ w 2) 12 "" ascii_16.fnt LightGray))
    (glgui-widget-set! gui:trends case-time 'align GUI_ALIGNCENTER)
    ;; And now the widgets
    (let loop ((i 0))
      (if (fx= i (length trend-traces)) #t
        (begin
          (glgui-trace gui:trends x (if (fx> i 12) (fx- y 110) y) w (if (fx> i 12) hsmall h) (list-ref trend-traces i) (list-ref trend-trace-colors i))
          (loop (fx+ i 1))
        )
      )
    )
  )

  ;;Place labels
  (let ((x (fx+ 5 270))(y (- (glgui-height-get) gui:menu-height 200)))
    (let loop ((i 0) (ret (list)))
      (if (fx= i (length trend-label-texts)) (set! trend-labels ret)
        (loop (fx+ i 1) (append ret (list (glgui-label gui:trends (if (fx= i 1) (fx+ x 25) x) (if (fx> i 12) (fx- y 110) y) 50 12 (list-ref trend-label-texts i) ascii_12.fnt (list-ref trend-trace-colors i)))))
      )
    )
  )
  (for-each (lambda (wgt) (glgui-widget-set! gui:trends wgt 'hidden #t)) trend-labels)

  ;; Add Time Labels
  (let ((y (- (glgui-height-get) gui:menu-height 200 110 20)))
    (set! time30 (glgui-label gui:trends 0 y 60 16 "" ascii_16.fnt DarkGray))
    (set! time00 (glgui-label gui:trends 255 y 60 16 "" ascii_16.fnt DarkGray))
  )

  ;; Bottom row with secondary navigation buttons
  (glgui-button-string gui:trends 10 (+ gui:navigation-height 10) (- (/ (glgui-width-get) 2) 20) 30 "Overview" ascii_24.fnt goto-overview-button-callback)
  (glgui-button-string gui:trends (+ (/ (glgui-width-get) 2) 10) (+ gui:navigation-height 10) (- (/ (glgui-width-get) 2) 20) 30 "Waveforms" ascii_24.fnt goto-waveforms-button-callback)
)

(define (goto-waveforms-button-callback g w t x y)
  (log-remote "Screen: Waveforms")
  (set! mode MODE_WAVES)
)

(define (load-trends or-name)
  (let* ((rupi (store-ref "main" "RupiClient" #f))
         (pin (store-ref "main" "Key"))
         (data (rupi-cmd rupi "GETTRENDS" pin or-name)))
    (if (list-notempty? data) (store-update-list or-name data))
    (for-each (lambda (l) (gltrace:clear l)) trend-traces)
    (let loop ((i 0))
      (if (fx= i (length trend-traces)) #t
        (let ((trace (list-ref trend-traces i))
              (trend-values (store-ref or-name (string-append (list-ref trend-source-names i) "-trend"))))
          (if trend-values (let loop2 ((k 0))
            (if (fx= k (f32vector-length trend-values)) 
              (let* ((y (- (glgui-height-get) gui:menu-height 200))
                     (last (f32vector-ref trend-values (fx- (f32vector-length trend-values) 1)))
                     (label-value (if (fl= last -32000.) #f last)))
                (if (and (number? label-value) (fl> label-value 0.1))
                  (begin
                    (glgui-widget-set! gui:trends (list-ref trend-labels i) 'hidden #f)
                    (glgui-widget-set! gui:trends (list-ref trend-labels i) 'y (fx+ (if (fx> i 12) (fx- y 110) y) (fix label-value)))
                  )
                  (glgui-widget-set! gui:trends (list-ref trend-labels i) 'hidden #t)
                )
              )
              (let ((val (f32vector-ref trend-values k)))
                (gltrace-add trace (if (fl= val -32000.) #f val))
                (loop2 (fx+ k 1))
              )
            )
          ))
          (loop (fx+ i 1))
        )
      )
    )
    (for-each (lambda (l) (gltrace-update l)) trend-traces)
  )
  (let ((monitor-time (store-ref "main" "LastUpdateTime")))
    (if monitor-time (begin
      (glgui-widget-set! gui:trends time30 'label (seconds->string (fl- monitor-time 1800.) "%H:%M"))
      (glgui-widget-set! gui:trends time00 'label (seconds->string monitor-time "%H:%M"))
    ))
  )
  (let ((induction-time (store-ref or-name "induction_timestamp"))
        (wall-time (store-ref "main" "LastUpdateTime")))
    (if (and induction-time wall-time)
      (glgui-widget-set! gui:trends case-time 'label (string-append "Case Time: " (localseconds->string (fl- wall-time induction-time) "%H:%M")))
      (glgui-widget-set! gui:trends case-time 'label "Case Time: N/A")
    )
  )
)

;; -----------------------------------------------------------------------------
;;  WAVEFORM SCREEN RELATED FUNCTIONS
;; -----------------------------------------------------------------------------
;; Initialize the waveform gui parts
(define gui:waves #f)	
(define (init-gui-waves)
  (set! gui:waves (make-glgui))

  ;; Need this to catch the callback of dragging in the screen
  (set! wave-canvas (glgui-box-dragable gui:waves 5 (- (glgui-height-get) gui:navigation-height 295) (- (glgui-width-get) 10) (+ 295 20) Black wave-canvas-callback))

  ;;Define Widgets to present current value numbers
  ;; glgui-trend has parameters: gui, x y label texture, font, color)
  (set! hr_value (glgui-trend gui:waves (- (glgui-width-get) 50) (- (glgui-height-get) gui:navigation-height 20) label_hr.img num_40.fnt Green))
  (set! spo2_value (glgui-trend gui:waves (- (glgui-width-get) 50) (- (glgui-height-get) gui:navigation-height 65) label_spo2.img num_40.fnt White))
  (set! etco2_value (glgui-trend gui:waves (- (glgui-width-get) 50) (- (glgui-height-get) gui:navigation-height 110) label_etco2.img num_40.fnt Orange))
  (set! art_value (glgui-trend gui:waves (- (glgui-width-get) 50) (- (glgui-height-get) gui:navigation-height 155) label_art.img num_40.fnt Red))
  (set! nibp_value (glgui-trend gui:waves (- (glgui-width-get) 50) (- (glgui-height-get) gui:navigation-height 200) label_nibp.img num_40.fnt Red))
  (set! temp_value (glgui-trend gui:waves (- (glgui-width-get) 50) (- (glgui-height-get) gui:navigation-height 245) label_temp.img num_40.fnt White))
  (set! agent_value (glgui-trend gui:waves (- (/ (glgui-width-get) 2) 50) (- (glgui-height-get) gui:navigation-height 245) label_agent.img num_40.fnt White))
  ;;(glgui-label g x y w h label fnt color)
  (set! agent_name (glgui-label gui:waves 5 (- (glgui-height-get) gui:navigation-height 245) 60 24 "" ascii_24.fnt White))  

  ;; Define scales for Waveforms
  (set! ECG_min (- 0.5))(set! ECG_max 2)
  (set! PLETH_min 0)(set! PLETH_max 10) ;;Why is the pleth not normalized to the range 0-1?
  (set! CO2_min 0)(set! CO2_max (/ 60 7.5))
  (set! ART_min 0)(set! ART_max 200)

  ;;Define traces to plot waveforms
  ;; make-gltrace width[number of points] height mode[OVERWRITE] y_min y_max y_min_value[for label] y_max_value[for label])
  (let ((trace-mode GLTRACE_OVERWRITE)) ;;was GLTRACE_RESET starts from scratch
    (set! ecg-trace (make-gltrace 601 30 trace-mode ECG_min ECG_max ECG_min ECG_max))
    (set! pleth-trace (make-gltrace 201 30 trace-mode PLETH_min PLETH_max PLETH_min PLETH_max))
    (set! co2-trace (make-gltrace 201 30 trace-mode CO2_min CO2_max CO2_min CO2_max))
    (set! art-trace (make-gltrace 201 30 trace-mode ART_min ART_max ART_min ART_max))
  )
  ;; Clear the traces
  (for-each (lambda (l) (gltrace:clear l)) (list ecg-trace pleth-trace co2-trace art-trace))

  ;;Place the Trace Widgets
  (set! ecg-wave (glgui-trace gui:waves 5 (- (glgui-height-get) gui:navigation-height 20) 200 40 ecg-trace Green))    
  (set! pleth-wave (glgui-trace gui:waves 5 (- (glgui-height-get) gui:navigation-height 65) 200 40 pleth-trace White))  
  (set! co2-wave (glgui-trace gui:waves 5 (- (glgui-height-get) gui:navigation-height 110) 200 40 co2-trace Orange))  
  (set! art-wave (glgui-trace gui:waves 5 (- (glgui-height-get) gui:navigation-height 155) 200 40 art-trace Red))  
  
  ;; If I wanted labels to the side indicated low and high values the font needs to be set
  ;;(glgui-widget-set! gui:waves art_wave 'limfnt num_18.fnt)

  ;; Screen Indicator 
  (set! screenindicator (glgui-screenindicator gui:waves 0 (+ gui:navigation-height 40) (glgui-width-get) 20 White))

  ;; Bottom row with secondary navigation buttons
  (glgui-button-string gui:waves 10 (+ gui:navigation-height 10) (- (/ (glgui-width-get) 2) 20) 30 "Overview" ascii_24.fnt goto-overview-button-callback)
  (glgui-button-string gui:waves (+ (/ (glgui-width-get) 2) 10) (+ gui:navigation-height 10) (- (/ (glgui-width-get) 2) 20) 30 "Trends" ascii_24.fnt goto-trends-button-callback)
)

;; Give an easy and consistent way of returning to the overview screen.
(define (goto-overview-button-callback g w t x y)
  (log-remote "Screen: Overview")
  (set! mode MODE_OVERVIEW)
  (if landscape? (glgui-orientation-set! GUI_PORTRAIT))
)
;; Going to the trend screen
(define (goto-trends-button-callback g w t x y)
  (log-remote "Screen: Trends")
  (set! rupi:last-trend-update 0.) ;; Force immediate update
  (set! mode MODE_TRENDS)
  (if landscape? (glgui-orientation-set! GUI_PORTRAIT))
)

;; Allow sliding motion to change rooms
(define (wave-canvas-callback g w t x y)
  (let ((ox (glgui-widget-get g w 'offsetx))
	(rooms (store-ref "main" "myRooms"))
	(waveform-room-idx (store-ref "main" "waveform-room-idx")))
    (if (and ox (> ox 20)) ;; go down as dragging is L->R
      (store-set! "main" "waveform-room-idx" (if (<= waveform-room-idx 0) (- (length rooms) 1) (- waveform-room-idx 1)))
    )
    (if (and ox (< ox -20));; go up, as dragging R->L
      (store-set! "main" "waveform-room-idx" (if (>= waveform-room-idx (- (length rooms) 1)) 0 (+ waveform-room-idx 1)))
    )
    (if (and ox (> (abs ox) 20))
      (begin
	(for-each (lambda (l) (gltrace:clear l)) (list ecg-trace pleth-trace co2-trace art-trace))
	(let ((or-name (list-ref rooms (store-ref "main" "waveform-room-idx"))))
	  (for-each (lambda (l) (store-clear! or-name l)) '("ECG1" "PLETH" "CO2" "INVP1"))
	)
	(set! rupi:last-wave-request 0.)(set! rupi:last-wave-update 0.) ;; Force immediate redraw
	(glgui-widget-set! g w 'offsetx #f); //Otherwise consecutive taps go there too
      )
    )
  )
)

;; This function updates the waveform screen numbers and waveforms
(define (update-waves or-name)
  ;; Set Title row to reflect which OR's waveforms we are looking at
  (glgui-widget-set! gui:menu title 'label or-name)
  (glgui-widget-set! gui:waves-landscape location-landscape 'label or-name)

  ;;Request new values
  (if (fl> (fl- ##now rupi:last-wave-update) rupi:wave-update-frequency)
    (begin
      ;;Refill traces with values obtained from Server but split in smaller pieces
      (waveform-add or-name "ECG1" ecg-trace rupi:wave-request-frequency rupi:wave-update-frequency)
      (waveform-add or-name "PLETH" pleth-trace rupi:wave-request-frequency rupi:wave-update-frequency)
      (waveform-add or-name "CO2" co2-trace rupi:wave-request-frequency rupi:wave-update-frequency)
      (waveform-add or-name "INVP1" art-trace rupi:wave-request-frequency rupi:wave-update-frequency)
      (gltrace-update ecg-trace) (gltrace-update pleth-trace) (gltrace-update co2-trace) (gltrace-update art-trace)
      ;; Update the timestamp
      (set! rupi:last-wave-update ##now)
    )
  )
  (if (fl> (fl- ##now rupi:last-wave-request) rupi:wave-request-frequency)
    (begin
      (set! rupi:last-wave-request ##now)	
      ;; Request new data from network
      (let* ((rupi (store-ref "main" "RupiClient" #f))(pin (store-ref "main" "Key"))
	      (data (rupi-cmd rupi "GETWAVES" pin or-name)))
	(if (list-notempty? data) ;; Always a list
	  (begin
	    (waveform-add-rest or-name "ECG1" ecg-trace)
	    (waveform-add-rest or-name "PLETH" pleth-trace)
	    (waveform-add-rest or-name "CO2" co2-trace)
	    (waveform-add-rest or-name "INVP1" art-trace)
	    ;; Flush the local stores first then append the new data
	    (store-update-data data)
	    (for-each (lambda (l) (set-waveform-length or-name l)) '("ECG1" "PLETH" "CO2" "INVP1"))
            (store-set! or-name "temp1" (rupi-cmd rupi "GETVALUE" pin or-name "temp1"))
            (store-set! or-name "aa_et" (rupi-cmd rupi "GETVALUE" pin or-name "aa_et"))
            (store-set! or-name "aa_name" (rupi-cmd rupi "GETVALUE" pin or-name "aa_name"))		
            (store-set! or-name "p1_mean" (rupi-cmd rupi "GETVALUE" pin or-name "p1_mean"))		
            (store-set! or-name "p1_dia" (rupi-cmd rupi "GETVALUE" pin or-name "p1_dia"))
            (store-set! or-name "p1_sys" (rupi-cmd rupi "GETVALUE" pin or-name "p1_sys"))
	  )
	)
      )
      
      ;; Update the Trend Numerics
      (let ((hr-val (store-ref or-name "hr"))
	    (hr-src (store-ref or-name "hr_source")))
	(glgui-widget-set! gui:waves hr_value 'label (if hr-val (number->string (fix hr-val)) ""))
	(if hr-src
	  (glgui-widget-set! gui:waves hr_value 'color (cond 
	    ((string=? hr-src "ECG1") Green)((string=? hr-src "PLETH") White)((string=? hr-src "BP1") Red)(else DarkGray))
	  )
	)
      )
      (let ((spo2-val (store-ref or-name "spo2")))
	(glgui-widget-set! gui:waves spo2_value 'label (if spo2-val (number->string (fix spo2-val)) ""))
      )
      (let ((etco2-val (store-ref or-name "co2_et")))
	(glgui-widget-set! gui:waves etco2_value 'label (if etco2-val (number->string (fix (* 7.5 etco2-val))) ""))
      )
      (let ((nibp-sys (store-ref or-name "nibp_sys")) (nibp-dia (store-ref or-name "nibp_dia"))
	    (nibp-mean (store-ref or-name "nibp_mean")))
	(glgui-widget-set! gui:waves nibp_value 'label 
	  (if (and nibp-mean nibp-sys nibp-dia)
	    (string-append (number->string (fix nibp-sys)) "/" (number->string (fix nibp-dia)) " (" (number->string (fix nibp-mean)) ")")
	    ""
	  )
	)
      )
      (let ((art-mean (store-ref or-name "p1_mean")) (art-dia (store-ref or-name "p1_dia")) (art-sys (store-ref or-name "p1_sys")))
	(glgui-widget-set! gui:waves art_value 'label (if art-mean (if (> art-mean 200) (number->string (fix art-mean)) "") #f))
	;;The < ART_dia<200 removes showing the ARTline bag pressure
	(if (and art-mean art-sys art-dia (< art-dia 200)) 
	  (begin
	    (glgui-widget-set! gui:waves nibp_value 'label (string-append (number->string (fix art-sys)) "/" (number->string (fix art-dia)) " (" (number->string (fix art-mean)) ")"))
	    (glgui-widget-set! gui:waves nibp_value 'image label_art.img)
	  )
	  (begin
	    (glgui-widget-set! gui:waves nibp_value 'image label_nibp.img)
	  )
	)
      )
      (let ((temp-val (store-ref or-name "temp1")))
	(glgui-widget-set! gui:waves temp_value 'label (if temp-val (number->string temp-val) ""))
      )  
      (let ((agent-val (store-ref or-name "aa_et")))
	(glgui-widget-set! gui:waves agent_value 'label (if agent-val (number->string agent-val) ""))
      )
      (let ((agent-name (store-ref or-name "aa_name")))
	(if agent-name
	  (let ((color (cond ((string=? agent-name "ISO") Purple)((string=? agent-name "DES") Blue)
			    ((string=? agent-name "SEV") Yellow)((string=? agent-name "NONE") DarkGray)(else White)) ))
	    (glgui-widget-set! gui:waves agent_name 'label agent-name)
	    (glgui-widget-set! gui:waves agent_value 'color color)
	    (glgui-widget-set! gui:waves agent_name 'color (if (string=? agent-name "NONE") Black color))
	  )(glgui-widget-set! gui:waves agent_name 'label "")
	)
      )
      ;; Update the labels in the landscape gui mode
      (if landscape? (begin
        (glgui-widget-set! gui:waves-landscape agent_name-landscape 'label (glgui-widget-get gui:waves agent_name 'label))
        (glgui-widget-set! gui:waves-landscape agent_name-landscape 'color (glgui-widget-get gui:waves agent_name 'color))
        (glgui-widget-set! gui:waves-landscape hr_value-landscape 'label (glgui-widget-get gui:waves hr_value 'label))
        (glgui-widget-set! gui:waves-landscape hr_value-landscape 'color (glgui-widget-get gui:waves hr_value 'color))
        (glgui-widget-set! gui:waves-landscape spo2_value-landscape 'label (glgui-widget-get gui:waves spo2_value 'label))
        (glgui-widget-set! gui:waves-landscape etco2_value-landscape 'label (glgui-widget-get gui:waves etco2_value 'label))
        (glgui-widget-set! gui:waves-landscape nibp_value-landscape 'label (glgui-widget-get gui:waves nibp_value 'label))
        (glgui-widget-set! gui:waves-landscape nibp_value-landscape 'image (glgui-widget-get gui:waves nibp_value 'image))
        (glgui-widget-set! gui:waves-landscape agent_value-landscape 'label (glgui-widget-get gui:waves agent_value 'label))
        (glgui-widget-set! gui:waves-landscape agent_value-landscape 'color (glgui-widget-get gui:waves agent_value 'color))
        (glgui-widget-set! gui:waves-landscape temp_value-landscape 'label (glgui-widget-get gui:waves temp_value 'label))
      ))
    )
  )
)

;; Save the received waveform lengths
(define (set-waveform-length store wave)
  (let ((val (store-ref store wave '())))
    ;; Check if they actually contain proper values, otherwise zero them
    ;; Mainly, this removes the ART line in patients with only NIBP
    (if (= (sum val) 0)
      (begin
	(store-set! store (string-append wave "-len") 0)
	(store-set! store wave '())
      )
      (store-set! store (string-append wave "-len") (length val))
    )
  )
)

;; Add the remaining parts of the waveform
(define (waveform-add-rest or-name wave-name trace)
  (let ((wave-val (store-ref or-name wave-name)))
    (if (list-notempty? wave-val)
      (let ((num-samples (length wave-val)))
	(let loop ((i 0))
	  (if (< i num-samples)
	    (begin
	      (gltrace-add trace (list-ref wave-val i))
	      (loop (+ i 1))
	    )))))))

;; This function retrieves data for a given waveform from the WAVES list and
;; updates the trace by appending the obtained values
(define (waveform-add or-name wave-name trace overall-time window-time)
  (let ((wave-val (store-ref or-name wave-name)) (wave-len (store-ref or-name (string-append wave-name "-len") 0)))
    (if (list-notempty? wave-val)
      (let ((num-samples (inexact->exact (floor (* (/ wave-len overall-time) window-time)))))
	(let loop ((i 0))
	  (if (and (< i num-samples) (< i (length wave-val)))
	    (begin
	      (gltrace-add trace (list-ref wave-val i))
	      (loop (+ i 1))
	    )
	  )
	)
	(if (< num-samples (length wave-val))
	  (store-set! or-name wave-name (list-tail wave-val num-samples))
	  (store-set! or-name wave-name '())
	)
      )
    )
  )
)

;; -----------------------------------------------------------------------------
;;  SIMPLIFIED HORIZONTAL WAVEFORM SCREEN FUNCTIONS
;; -----------------------------------------------------------------------------
;; Initialize the landscape waveform gui parts
(define gui:waves-landscape #f)	
(define (init-gui-waves-landscape)
  (set! gui:waves-landscape (make-glgui))

  ;; Need this to catch the callback of dragging in the screen
  (set! wave-canvas (glgui-box-dragable gui:waves-landscape 5 5 (- (glgui-height-get) 10) (- (glgui-width-get) 10) Black wave-canvas-callback))

  ;;Define Widgets to present current value numbers
  ;; glgui-trend has parameters: gui, x y label texture, font, color)
  (set! hr_value-landscape (glgui-trend gui:waves-landscape (- (glgui-height-get) 50) (- (glgui-width-get) 50) label_hr.img num_40.fnt Green))
  (set! spo2_value-landscape (glgui-trend gui:waves-landscape (- (glgui-height-get) 50) (- (glgui-width-get) 105) label_spo2.img num_40.fnt White))
  (set! etco2_value-landscape (glgui-trend gui:waves-landscape (- (glgui-height-get) 50) (- (glgui-width-get) 150) label_etco2.img num_40.fnt Orange))
  (set! art_value-landscape (glgui-trend gui:waves-landscape (- (glgui-height-get) 50) (- (glgui-width-get) 195) label_art.img num_40.fnt Red))
  (set! nibp_value-landscape (glgui-trend gui:waves-landscape (- (glgui-height-get) 50) (- (glgui-width-get) 240) label_nibp.img num_40.fnt Red))
  (set! temp_value-landscape (glgui-trend gui:waves-landscape (- (glgui-height-get) 50) (- (glgui-width-get) 280) label_temp.img num_40.fnt White))
  (set! agent_value-landscape (glgui-trend gui:waves-landscape (- (/ (glgui-height-get) 2) 50) (- (glgui-width-get) 280) label_agent.img num_40.fnt White))
  (set! agent_name-landscape (glgui-label gui:waves-landscape 85 (- (glgui-width-get) 280) 60 24 "" ascii_24.fnt White))  

  ;;Place the Trace Widgets
  (set! ecg-wave-landscape (glgui-trace gui:waves-landscape 10 (- (glgui-width-get) 60) 350 40 ecg-trace Green))    
  (set! pleth-wave-landscape (glgui-trace gui:waves-landscape 10 (- (glgui-width-get) 105) 350 40 pleth-trace White))  
  (set! co2-wave-landscape (glgui-trace gui:waves-landscape 10 (- (glgui-width-get) 150) 350 40 co2-trace Orange))  
  (set! art-wave-landscape (glgui-trace gui:waves-landscape 10 (- (glgui-width-get) 195) 350 40 art-trace Red))  
  
  ;; Screen Indicator 
  (set! screenindicator-landscape (glgui-screenindicator gui:waves-landscape 0 5 (glgui-height-get) 20 White))
  (set! location-landscape (glgui-label gui:waves-landscape 10 (- (glgui-width-get) 30) 70 25 "Room" ascii_24.fnt White))

  ;; Bottom row with secondary navigation buttons
  (let ((w (/ (glgui-width-get) 3)))
    (glgui-button-string gui:waves-landscape 5 5 w 30 "Overview" ascii_24.fnt goto-overview-button-callback)
    (glgui-button-string gui:waves-landscape (- (glgui-height-get) w 10) 5 w 30 "Trends" ascii_24.fnt goto-trends-button-callback)
  )
)

;; -----------------------------------------------------------------------------
;;  ROOM SUBSCRIPTION SCREEN RELATED FUNCTIONS
;; -----------------------------------------------------------------------------
;; Initialize the room subscription gui parts
(define gui:rooms #f)	
(define (init-gui-rooms)
  (set! gui:rooms (make-glgui))

  (let* ((x 0)
	 (number-rows 8)
	 (height (* number-rows gui:row-height))
	 (y (- (glgui-height-get) gui:menu-height 16 3)))
    ;;Header row
    (glgui-label gui:rooms (+ x 5) y 70 16 "Room" ascii_16.fnt White)
    (glgui-label gui:rooms (+ x 85) y (- (glgui-width-get) 80 5) 16 "Subscribers" ascii_16.fnt White)
    (glgui-label gui:rooms (+ x 270) y (- (glgui-width-get) 50 5) 16 "Status" ascii_16.fnt White)
    (set! room-list
       (glgui-list gui:rooms 0 (- y 2 height) (glgui-width-get) height gui:row-height (build-room-list) room-callback)
    )
    (set! transfer-user-list
       (glgui-list gui:rooms (/ (glgui-width-get) 6) (- y height) (* (/ (glgui-width-get) 3) 2) (* 3 gui:row-height) gui:row-height '() transfer-user-list-callback)
    )
    (glgui-widget-set! gui:rooms transfer-user-list 'hidden #t)
    (set! transfer-user-label
       (glgui-label gui:rooms 0 (- y (* 5 gui:row-height)) (glgui-width-get) 18 "Transfer rooms to :" ascii_16.fnt White)
    )
    (glgui-widget-set! gui:rooms transfer-user-label 'align GUI_ALIGNCENTER)
    (glgui-widget-set! gui:rooms transfer-user-label 'hidden #t)

    ;; Bottom row with secondary navigation buttons
    (set! pacu-button
      (glgui-button-string gui:rooms (+ (/ (glgui-width-get) 2) 10) (+ gui:navigation-height 10) 
	(- (/ (glgui-width-get) 2) 20) 30 "Show PACU" ascii_24.fnt pacu-button-callback)
    )
    (set! transfer-button
      (glgui-button-string gui:rooms 10 (+ gui:navigation-height 10) (- (/ (glgui-width-get) 2) 20) 30 "Transfer To?" ascii_24.fnt transfer-button-callback)
    )
  )
)

(define (transfer-button-callback g w t x y)
  (if (> (length (store-ref "main" "myRooms" '())) 0) 
    (begin
      (let ((curh (glgui-widget-get g room-list 'h))
	    (cury (glgui-widget-get g room-list 'y)))
	
	(glgui-widget-set! g room-list 'h (if (< curh (/ (glgui-height-get) 2)) (* curh 2) (/ curh 2))) 
	(glgui-widget-set! g room-list 'y (if (< curh (/ (glgui-height-get) 2)) (- cury curh) (+ cury (/ curh 2))))
      )
      (glgui-widget-set! g transfer-button 'color (if (= (glgui-widget-get g transfer-button 'color) White) Blue White))
      (glgui-widget-set! g transfer-user-list 'list (glgui-widget-get gui:users user-list 'list))
      (glgui-widget-set! g transfer-user-list 'hidden (not (glgui-widget-get g transfer-user-list 'hidden)))
      (glgui-widget-set! g transfer-user-label 'hidden (not (glgui-widget-get g transfer-user-label 'hidden))
)
    )
  )
)

(define (transfer-user-list-callback g w t x y)
  (let ((users (store-ref "main" "Users")))
    (if users 
      (let ((receiver (car (list-ref users (fix (glgui-widget-get g w 'current))))))
	(rupi-cmd (store-ref "main" "RupiClient" #f) "OFFERROOMS" (store-ref "main" "Key" #f) receiver)
	;; Also give the user some indication that rooms were send
	(store-set! "main" "popup-text" (list "Room Transfer" (string-append "Rooms offered to " receiver)))
	(store-set! "main" "RoomSendTime" ##now)
        (transfer-button-callback g w t x y)
        (glgui-widget-set! g transfer-button 'hidden #t)
      )
    )
  )
)


(define (pacu-button-callback g w t x y)
  (let ((color (glgui-widget-get g w 'color)))
    (glgui-widget-set! g w 'color (if (= color White) Blue White)) 
    (update-room-lists)
    (glgui-widget-set! g room-list 'offset 0)
  )
)


(define (build-room-list)
  (let ((rooms (store-ref "main" "Rooms")))
    ;;build the room entry list
    (if rooms
      (let loop ((i 0) (result (list)))
        ;;If we reached length, we return the result
        (if (= i (length rooms)) result
	   ;; this is the else case, which appends the list with another element 
	   (loop (+ i 1)(append result (list (room-list-element (list-ref rooms i)))))
        )
      )
      (list) ;;empty list returned if no room subscribed to
    )
  )
)

(define (room-list-element room)
  (lambda (g wgt x y w h s)
    ;; Find the approximate center of the height to shift the things up appropriately
    (define y_shift (+ y (/ (- h 24) 2)))
    ;; name of current or, so we can query the right bed

    ;; Place the correctly colored OR label
    (let* ((myrooms (store-ref "main" "myRooms"))
	   (users (cadr room))
	   (num-users (length users))
	   (color (if (and myrooms (member (car room) myrooms)) White (if (list-notempty? users) LightSlateGray Black)))
	   (user-name (store-ref "main" "UserName"))
	   (rooms-send (store-ref "main" "RoomSendTime")))
      (glgui:draw-text-left (+ x 3) y_shift 82 24 (car room) ascii_24.fnt color)   
      (if (> num-users 0)
	(let loop ((i 0))
	  (if (not (or (= i num-users) (= i 5)))
	    (begin
	      (if (< i 2) (glgui:draw-text-left (+ x 85 (* i 75)) (+ y (/ h 2)) 70 16 (list-ref users i) ascii_16.fnt 
		(if (string=? (list-ref users i) user-name) White LightSlateGray)))
	      (if (and (> i 1)(< i 4)) (glgui:draw-text-left (+ x 85 (* (- i 2) 75)) (+ y 2) 70 16 (list-ref users i) ascii_16.fnt 
		(if (string=? (list-ref users i) user-name) White LightSlateGray)))
	      (if (= i 4) (glgui:draw-text-left (+ x 85 (* (- i 2) 75)) (+ y 2) 70 16 
		(string-append "(+" (number->string (- num-users 4)) "...)") ascii_16.fnt White)
	      )
	      (loop (+ i 1))
	    )
	  )
	)
      )
      (if (and rooms-send (= color White))
	(let ((i rooms-transfer.img))
	  ;; This is copied from glgui:pixmap-draw
	  (glCoreColor gui:active-color)
	  (apply glCoreTextureDraw (append (list (+ x w (- 0 (car i))) y (car i) (cadr i)) (cddr i) (list 0)))
	)
        (if (= color White)
          (let ((i rooms-subscribed.img))
            ;; This is copied from glgui:pixmap-draw
            (glCoreColor gui:active-color)
            (apply glCoreTextureDraw (append (list (+ x w (- 0 (car i))) y (car i) (cadr i)) (cddr i) (list 0)))
          )
        )
      )
    )
  )
)

;; This is the function called when a room is selected or unselected
;; It updates the myORs list which contains the subscriptions
(define (room-callback g w t x y)
  (let ((idx (fix (glgui-widget-get g w 'current)))
        (myrooms (store-ref "main" "myRooms"))
	(rooms (store-ref "main" "Rooms")))
    ;; Needed as myrooms might be #f (store-ref "main" "myRooms" '()) will still return #f !!!
    (if (not myrooms)
      (set! myrooms '())
    )
    ;; If we have this room unsubscribe, otherwise subscribe
    (if (member (car (list-ref rooms idx)) myrooms)
      (begin
	(let ((del-room (car (list-ref rooms idx))))
	  (store-set! "main" "myRooms" (list-delete-item myrooms del-room))
	)
      )
      ;; sort takes two parameters: list and comparison
      (store-set! "main" "myRooms" (sort (append myrooms (list (car (list-ref rooms idx)))) string<?))
    )
  )
  (rupi-cmd (store-ref "main" "RupiClient" #f) "SETMYROOMS" (store-ref "main" "Key" #f) (store-ref "main" "myRooms"))
  ;; We want immediate refresh of who has which
  (let ((ofs (glgui-widget-get gui:rooms room-list 'offset)))
    (update-room-lists)
    (glgui-widget-set! gui:rooms room-list 'offset ofs)
  )
)

(define (update-room-lists)
  (let ((data (rupi-cmd (store-ref "main" "RupiClient" #f) "GETROOMS" (store-ref "main" "Key"))))
    (if (list? data)
      (begin
	;; Room list
	(store-set! "main" "Rooms" (sort-rooms data))
	(glgui-list-reset gui:rooms room-list (build-room-list))
	;; Update the other places these lists are used
	(glgui-list-reset gui:overview numeric-list (build-numeric-list))
	(glgui-list-reset gui:reminder-setup reminder-room-list (build-reminder-room-list))
      )
    )
  )
)


;; -----------------------------------------------------------------------------
;;  REMINDER RELATED FUNCTIONS
;; -----------------------------------------------------------------------------
(define gui:reminder #f)	
(define (init-gui-reminder)
  (set! gui:reminder (make-glgui))

 (let ((x 0)(y (- (glgui-height-get) gui:menu-height 16 3)))
    ;;Header row
    (glgui-label gui:reminder (+ x 5) y 50 16 "Room" ascii_16.fnt White)
    (glgui-label gui:reminder (+ x 85) y 50 16 "Action" ascii_16.fnt White)
    (glgui-label gui:reminder (+ x 230) y 30 16 "Edit" ascii_16.fnt White)
    (glgui-label gui:reminder (+ x 268) y 50 16 "Delete" ascii_16.fnt White)

    (set! reminder-list
       (glgui-list gui:reminder 0 (- y 5 (* 5 60)) (glgui-width-get) (* 5 60) 60 (build-reminder-list) reminder-list-callback)
    )
  )
  ;; Bottom row with secondary navigation buttons
  (glgui-button-string gui:reminder 10 (+ gui:navigation-height 10) (- (glgui-width-get) 20) 30 "Add Reminder" ascii_24.fnt reminder-add-button-callback)
)

(define (reminder-list-callback g w t x y)
  ;; Edit an entry
  (if (and (< x 265) (> x 230)) 
    (let* ((cur (fix (glgui-widget-get g w 'current)))
	   (r (store-ref "main" "Reminders"))
	   (entry (list-ref r cur))
	   (task-names (store-ref "main" "task-names"))
	   (room-names (store-ref "main" "myRooms")))

      (set! gui:reminder-room-list-pos (find-list-pos room-names (car entry)))
      (set! gui:reminder-task-list-pos (find-list-pos task-names (cadr entry)))
      (if (list-notempty? (caddr entry))
	(begin
          (glgui-widget-set! gui:reminder-setup hour 'value (string->number (seconds->string (caaddr entry) "%H")))
	  (glgui-widget-set! gui:reminder-setup minute 'value (fix (/ (string->number (seconds->string (caaddr entry) "%M")) 5)))
	  (glgui-widget-set! gui:reminder-setup repeat 'value (- (length gui:reminder-repeat-minutes-list) (length (member (number->string (car (cdaddr entry))) gui:reminder-repeat-minutes-list))))
          (time-change-button-callback gui:reminder-setup w t x y) 
	)
	(begin
          (phase-button-unselect)
	  (glgui-widget-set! gui:reminder-setup (list-ref phase-selector (caddr entry)) 'color gui:active-color)
	  (time-selection-unselect)
	)
      )
      ;;Log which screen is used
      (log-remote "Screen: ReminderSetup")      
      (set! mode MODE_REMINDER_SETUP)
    )
  )
  ;; Delete an entry
  (if (and (< x 310) (> x 270)) 
    (let ((r (store-ref "main" "Reminders"))
	  (cur (fix (glgui-widget-get g w 'current))))
      (if (and r (>= cur 0))
	(begin
	  (store-set! "main" "Reminders"
	    (let loop ((i 0) (result (list)))
	      (if (= i (length r)) result
		(loop (+ i 1)(if (= i cur) result (append result (list (list-ref r i)))))
	      )
	    )
	  )
	  (rupi-cmd (store-ref "main" "RupiClient" #f) "UPDATEREMINDERS" (store-ref "main" "Key" #f) (store-ref "main" "Reminders"))
	  (glgui-widget-set! g w 'list (build-reminder-list))
	)
      )
    )
  )
)

(define (reminder-add-button-callback g w t x y)
  (let ((hour-pos (fix (string->number (seconds->string ##now "%H")))))
    (if (>= hour-pos 24)
      (begin
        (glgui-widget-set! gui:reminder-setup hour 'value 0)
        (glgui-widget-set! gui:reminder-setup minute 'value 1)
      )
      (glgui-widget-set! gui:reminder-setup hour 'value hour-pos)
    )
  )
  (let ((minute-pos (+ 1 (fix (/ (string->number (seconds->string ##now "%M")) 5)))))
    (if (>= minute-pos 12)
      (begin 
        (glgui-widget-set! gui:reminder-setup minute 'value 0)
        (glgui-widget-set! gui:reminder-setup hour 'value (+ (glgui-widget-get gui:reminder-setup hour 'value) 1))
      )
      (glgui-widget-set! gui:reminder-setup minute 'value minute-pos)
    )
  )
  (glgui-widget-set! gui:reminder-setup repeat 'value 0)
  (set! gui:reminder-room-list-pos 0)
  (set! gui:reminder-task-list-pos 0)

  ;; We use this to differentiate between an APPEND and a REPLACE
  (glgui-widget-set! g reminder-list 'current -1)
  (time-change-button-callback gui:reminder-setup w t x y)

  ;; Don't show add reminder setup screen if we are not subscribed to at least one room
  (if (list-notempty? (store-ref "main" "myRooms")) 
    (begin 
      (log-remote "Screen: ReminderSetup")
      (set! mode MODE_REMINDER_SETUP)
    )
  )
)

(define (build-reminder-list)
  (let ((reminders (store-ref "main" "Reminders")))
    (if reminders
      (let loop ((i 0) (result (list)))
        (if (= i (length reminders)) result
	   (loop (+ i 1)(append result (list (reminder-list-element (list-ref reminders i)))))
        )
      )
      (list) 
    )
  )
)

(define (reminder-list-element entry)
  (lambda (g wgt x y w h s)
    (glgui:draw-text-left (+ x 5) (+ y (/ (- h 24) 2)) 70 24 (car entry) ascii_24.fnt White)  
    (if (list-notempty? (caddr entry))
      (let ((then (caaddr entry)))
	(glgui:draw-text-left (+ x 85) (+ y 5) 140 16 (string-append (seconds->string then "%H:%M") " (in "
								    (number->string (fix (/ (- then ##now) 60))) "min)") ascii_16.fnt White) 
	(if (> (cadr (caddr entry)) 0)
	  (glgui:draw-pixmap-left (+ x 66) (+ y 5) 16 16 repeat-small.img White)
	)
      )
      (glgui:draw-pixmap-left (+ x 85) (+ y 5) 50 21 (list-ref phase-labels (caddr entry)) White)
    )
    (glgui:draw-text-left (+ x 85) (+ y 30) 150 24 (cadr entry) ascii_24.fnt White)    
   
    ;;Delete and Edit buttons
    (glgui:draw-pixmap-left (+ x 225) (+ y 10) 40 40 edit.img White)
    (glgui:draw-pixmap-left (+ x 270) (+ y 10) 40 43 delete.img White)
  )
)


;; Data fields for Reminder setup screens
(define gui:reminder-minutes-list '("00" "05" "10" "15" "20" "25" "30" "35" "40" "45" "50" "55"))
(define gui:reminder-hours-list '( "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23"))
(define gui:reminder-repeat-minutes-list '("0" "15" "30" "60" "90" "120" "300"))
(define gui:reminder-room-list-pos 0)
(define gui:reminder-task-list-pos 0)

;; And now the reminder setup screen
(define gui:reminder-setup #f)	
(define (init-gui-reminder-setup)
  (set! gui:reminder-setup (make-glgui))

  ;; Room list and Task selector
  (let ((x 5)(y (- (glgui-height-get) gui:menu-height 16 3)))
    (glgui-label gui:reminder-setup x y 40 16 "Room" ascii_16.fnt White)
    (set! reminder-room-list
      (glgui-list gui:reminder-setup x (- y (* 3 40)) (- (/ (glgui-width-get) 2) 10) (* 3 40) 40 (build-reminder-room-list) reminder-room-list-callback)
    )

    (store-set! "main" "task-names" '("Check Room" "Draw ABG" "Meeting"))
    (glgui-label gui:reminder-setup (+ x (/ (glgui-width-get) 2)) y 40 16 "Task" ascii_16.fnt White)    
    (set! reminder-task-list
      (glgui-list gui:reminder-setup (+ x (/ (glgui-width-get) 2)) (- y (* 3 40)) (- (/ (glgui-width-get) 2) 10) (* 3 40) 40 (build-reminder-task-list) reminder-task-list-callback)
    )
  )

  (let ((x 5)(y 205))
    (glgui-label gui:reminder-setup (+ x 12) (+ y 40 40 3) 110 16 "Reminder Time" ascii_16.fnt White)
    ;;Hour field
    (set! hour (glgui-vwheel gui:reminder-setup (+ x 12) (- y 58) 55 135 #f #f #f #f gui:active-color gui:inactive-color
        num_40.fnt num_24.fnt #f gui:reminder-hours-list))
    (glgui-widget-set! gui:reminder-setup hour 'callback time-change-button-callback)
    (glgui-widget-set! gui:reminder-setup hour 'cycle #t)
    (set! minute (glgui-vwheel gui:reminder-setup (+ x 72) (- y 58) 45 135 #f #f #f #f gui:active-color gui:inactive-color
        num_40.fnt num_24.fnt #f gui:reminder-minutes-list))
    (glgui-widget-set! gui:reminder-setup minute 'callback time-change-button-callback)
    (glgui-widget-set! gui:reminder-setup minute 'cycle #t)

    ;; The image needs to be shifted by 15px
    (glgui-label gui:reminder-setup (+ x 150) (+ y 40 40 3) 50 16 "Repeat" ascii_16.fnt White)
    (set! repeat (glgui-vwheel gui:reminder-setup (+ x 145) (- y 58) 50 135 #f #f #f #f gui:active-color gui:inactive-color
        num_24.fnt num_24.fnt #f gui:reminder-repeat-minutes-list))
    (glgui-widget-set! gui:reminder-setup repeat 'callback time-change-button-callback)
    (set! repeat-line (glgui-pixmap gui:reminder-setup (+ x 145) (- y 15) repeat.img))
  )
  
  ;; Phase Selector Row
  (let ((x 230)(y 160))
    (glgui-label gui:reminder-setup (+ x 5 10) (+ y 90 35 3) 100 16 "Phase" ascii_16.fnt White)
    (set! phase-selector 
      (let loop ((i 0) (result (list)))
        (if (= i (length phase-labels)) result
	   (loop (+ i 1)(append result (list (glgui-button gui:reminder-setup (+ x 5) (- (+ y 93) (* i 35)) 65 30 (list-ref phase-labels i) phase-button-callback))))
        )
      )
    )
  )
  
  ;; Bottom row with secondary navigation buttons
  (glgui-button-string gui:reminder-setup 10 (+ gui:navigation-height 10) (- (/ (glgui-width-get) 2) 20) 30 "Cancel" ascii_24.fnt reminder-cancel-button-callback)
  (glgui-button-string gui:reminder-setup (+ (/ (glgui-width-get) 2) 10) (+ gui:navigation-height 10) (- (/ (glgui-width-get) 2) 20) 30 "Save" ascii_24.fnt reminder-save-button-callback)
)


;; Reminder Room List functions
(define (reminder-room-list-callback g w t x y)
  (set! gui:reminder-room-list-pos (fix (glgui-widget-get g w 'current)))
)

(define (build-reminder-room-list)
   (let ((rooms (store-ref "main" "myRooms")))
    (if rooms
      (let loop ((i 0) (result (list)))
        (if (= i (length rooms)) result
	   (loop (+ i 1)(append result (list (reminder-room-list-element (list-ref rooms i) i))))
        )
      )
      (list)
    )
  )
)

(define (reminder-room-list-element room idx)
  (lambda (g wgt x y w h s)
    (let ((y_shift (+ y (/ (- h 24) 2))))
      (glgui:draw-text-left (+ x 5) y_shift 100 24 room ascii_24.fnt (if (= idx gui:reminder-room-list-pos) gui:active-color gui:inactive-color))   
    )
  )
)


;; Reminder Task List functions
(define (reminder-task-list-callback g w t x y)
  (set! gui:reminder-task-list-pos (fix (glgui-widget-get g w 'current)))
)

(define (build-reminder-task-list)
   (let ((tasks (store-ref "main" "task-names")))
    (if tasks
      (let loop ((i 0) (result (list)))
        (if (= i (length tasks)) result
	   (loop (+ i 1)(append result (list (reminder-task-list-element (list-ref tasks i) i))))
        )
      )
      (list)
    )
  )
)

(define (reminder-task-list-element room idx)
  (lambda (g wgt x y w h s)
    (let ((y_shift (+ y (/ (- h 24) 2))))
      (glgui:draw-text-left (+ x 5) y_shift 130 24 room ascii_24.fnt (if (= idx gui:reminder-task-list-pos) gui:active-color gui:inactive-color))   
    )
  )
)

;; Time Change Functions
(define (time-change-button-callback g w t x y)
  ;; Update the fields
  (glgui-widget-set! g hour 'colorvalue gui:active-color)  
  (glgui-widget-set! g minute 'colorvalue gui:active-color)  
  (let ((repeat-minute-pos (glgui-widget-get g repeat 'value)))
    (glgui-widget-set! g repeat 'colorvalue (if (= repeat-minute-pos 0) gui:inactive-color gui:active-color))
    (glgui-widget-set! g repeat-line 'color (if (= repeat-minute-pos 0) gui:inactive-color gui:active-color))
  )

  ;;Disable Phase buttons
  (phase-button-unselect)
)

(define (time-selection-unselect)
  (let ((g gui:reminder-setup)
	(c gui:inactive-color))
    (glgui-widget-set! g hour 'colorvalue c)  
    (glgui-widget-set! g minute 'colorvalue c)
    (glgui-widget-set! g repeat 'colorvalue c)
    (glgui-widget-set! g repeat-line 'color c)
  )
)

;; Phase Selection Functions
(define (phase-button-unselect)
  (let loop ((i 0))
    (if (not (= i (length phase-selector)))
      (begin 
	(glgui-widget-set! gui:reminder-setup (list-ref phase-selector i) 'color gui:inactive-color)
	(loop (+ i 1))
      )
    )
  )
)

(define (phase-button-callback g w t x y)
  (if (= (glgui-widget-get g w 'color) gui:inactive-color)
    (begin
      (phase-button-unselect)
      (time-selection-unselect)
      (glgui-widget-set! g w 'color gui:active-color)
      (set! gui:reminder-task-list-pos 0)
    )
  )
)

;; Cancel Button
(define (reminder-cancel-button-callback g w t x y)
  (log-remote "Screen: Reminder")
  (set! mode MODE_REMINDER)
)

(define (reminder-save-helper g)
  (let ((rooms (store-ref "main" "myRooms"))
        (tasks (store-ref "main" "task-names"))
        (hours-pos (fix (glgui-widget-get gui:reminder-setup hour 'value)))
        (minutes-pos (fix (glgui-widget-get gui:reminder-setup minute 'value)))
        (repeat-minutes-pos (fix (glgui-widget-get gui:reminder-setup repeat 'value))))
    (list
      (list-ref rooms gui:reminder-room-list-pos)
      (list-ref tasks gui:reminder-task-list-pos)
      (if (= (glgui-widget-get g hour 'colorvalue) gui:active-color)
	(let ((delta-time (+ (* (- (string->number (list-ref gui:reminder-hours-list hours-pos)) (string->number (seconds->string ##now "%H"))) 3600)
			    (* (- (string->number (list-ref gui:reminder-minutes-list minutes-pos)) (string->number (seconds->string ##now "%M"))) 60)
 			    (- 0 (string->number (seconds->string ##now "%S")))
			  )))
	  (list
	    (+ ##now (if (< delta-time 0) 86400 0) delta-time)
	    (string->number (list-ref gui:reminder-repeat-minutes-list repeat-minutes-pos))
	  )
	)
	(let loop ((i 0) (phase-val 0))
	  (if (= i (length phase-selector)) 
	    phase-val
	    (loop (+ i 1) (if (= (glgui-widget-get g (list-ref phase-selector i) 'color) gui:active-color) i phase-val))
	  )
	)
      )
    )
  )
)

;; Save Button
(define (reminder-save-button-callback g w t x y)
  (let ((reminders (store-ref "main" "Reminders"))
	(cur (glgui-widget-get gui:reminder reminder-list 'current ))
	(new-reminder (reminder-save-helper g)))
    (if (>= cur 0)
      (begin
	(list-set! reminders cur new-reminder)
	(store-set! "main" "Reminders" reminders)
      )
      (store-set! "main" "Reminders" (append (list new-reminder) reminders))
    )
    (glgui-widget-set! gui:reminder reminder-list 'list (build-reminder-list))
    (rupi-cmd (store-ref "main" "RupiClient" #f) "UPDATEREMINDERS" (store-ref "main" "Key" #f) (store-ref "main" "Reminders"))
  )
  (log-remote "Screen: Reminder")
  (set! mode MODE_REMINDER)
)

;; -----------------------------------------------------------------------------
;;  PHONEBOOK RELATED FUNCTIONS
;; -----------------------------------------------------------------------------

;; Initialize the phonebook gui
(define gui:phonebook #f)
(define (init-gui-phonebook)
  (set! gui:phonebook (make-glgui))

  (let ((x 0)(y (- (glgui-height-get) gui:menu-height 16 3)))
    ;;Header row
    (glgui-label gui:phonebook (+ x 5) y 70 16 "Name" ascii_16.fnt White)
    (glgui-label gui:phonebook (+ x 155) y 105 16 "Number/Pager" ascii_16.fnt White)
    ;; The actual phonebook
    (set! phonebook-list
       (glgui-list gui:phonebook 0 (- y 5 (* 10 30)) (glgui-width-get) (* 10 30) 30 (build-phonebook-list) phonebook-select-callback)
    )
    ;; Bottom row with secondary navigation buttons
    (set! phonebook-edit-button
      (glgui-button-string gui:phonebook 10 (+ gui:navigation-height 10) (- (glgui-width-get) 20) 30 "Modify Phonebook" ascii_24.fnt phonebook-modify-button-callback)
    )
    (set! phonebook-add-button
      (glgui-button-string gui:phonebook 10 (+ gui:navigation-height 10) (- (/ (glgui-width-get) 2) 20) 30 "Add Entry" ascii_24.fnt phonebook-add-button-callback)
    )
    (set! phonebook-save-button
      (glgui-button-string gui:phonebook (+ (/ (glgui-width-get) 2) 10) (+ gui:navigation-height 10) (- (/ (glgui-width-get) 2) 20) 30 "Save Update" ascii_24.fnt phonebook-save-button-callback)
    )
    (glgui-widget-set! gui:phonebook phonebook-add-button 'hidden #t)
    (glgui-widget-set! gui:phonebook phonebook-save-button 'hidden #t)
  )
)

(define (phonebook-select-callback g w t x y)
  (if (and (not (glgui-widget-get g phonebook-edit-button 'hidden)) (fx> x 155) (fx< x 310)) 
    (let* ((login (store-ref "main" "UserName"))
           (lst (list-keep (list-keep (store-ref "main" "Users" '()) (lambda (l) (fx= (cadr l) 1))) 
             (lambda (l) (not (string=? (car l) login)))))
           (cur (glgui-widget-get g w 'current)))
      (if (and (fx< cur (length lst)) voip:enabled)
        (call-voip (car (list-ref lst cur)))
      )
    )
  )
  (if (and (glgui-widget-get g phonebook-edit-button 'hidden) (fx> x 285) (fx< x 310))
    (let ((lst (store-ref "main" "Phonebook"))
          (cur (glgui-widget-get g w 'current)))
      (store-set! "main" "Phonebook" (list-delete-item lst (list-ref lst cur)))
      (glgui-widget-set! g w 'list (build-phonebook-list))
    )
  )
  (if (and (glgui-widget-get g phonebook-edit-button 'hidden) (fx> x 255) (fx< x 280))
    (let ((lst (store-ref "main" "Phonebook"))
          (cur (glgui-widget-get g w 'current)))
      (glgui-widget-set! gui:phonebook-editor phonebook-name 'label (car (list-ref lst cur)))
      (glgui-widget-set! gui:phonebook-editor phonebook-number 'focus #f)
      (glgui-widget-set! gui:phonebook-editor phonebook-name 'focus #t)
      (glgui-widget-set! gui:phonebook-editor phonebook-number 'label (cadr (list-ref lst cur)))
      ;; Hide the menubar
      (glgui-widget-set! gui:menu navigation-bar 'hidden #t)
      (glgui-widget-set! gui:menu message-number 'hidden #t)
      (glgui-widget-set! gui:menu reminder-number 'hidden #t)
      ;; Show the editor screen
      (set! gui:phonebook-editor-shown #t)
    )
  )
)

(define (phonebook-add-button-callback g w t x y)
  (glgui-widget-set! gui:phonebook-editor phonebook-name 'label "")
  (glgui-widget-set! gui:phonebook-editor phonebook-number 'focus #f)
  (glgui-widget-set! gui:phonebook-editor phonebook-name 'focus #t)
  (glgui-widget-set! gui:phonebook-editor phonebook-number 'label "")
  (glgui-widget-set! g phonebook-list 'current (length (store-ref "main" "Phonebook")))
  ;; Hide the menubar
  (glgui-widget-set! gui:menu navigation-bar 'hidden #t)
  (glgui-widget-set! gui:menu message-number 'hidden #t)
  (glgui-widget-set! gui:menu reminder-number 'hidden #t)
  ;; Show the editor screen
  (set! gui:phonebook-editor-shown #t)
)

(define (phonebook-modify-button-callback g w t x y)
  ;; Make sure we have the latest version of the phonebook
  (let ((data (rupi-cmd (store-ref "main" "RupiClient" #f) "GETPHONEBOOK" (store-ref "main" "Key"))))
    (if (list-notempty? data) (begin
      (store-set! "main" "Phonebook" data)
      (glgui-widget-set! g phonebook-list 'list (build-phonebook-list))
    ))
  )
  ;; Show the editing options
  (glgui-widget-set! g w 'hidden #t)
  (glgui-widget-set! g phonebook-add-button 'hidden #f)
  (glgui-widget-set! g phonebook-save-button 'hidden #f)
)

(define (phonebook-save-button-callback g w t x y)
  (glgui-widget-set! g w 'hidden #t)
  (glgui-widget-set! g phonebook-add-button 'hidden #t)
  (glgui-widget-set! g phonebook-edit-button 'hidden #f)
  ;; Send the changes to the server
  (let ((book (store-ref "main" "Phonebook")))
    (rupi-cmd (store-ref "main" "RupiClient" #f) "SETPHONEBOOK" (store-ref "main" "Key") book)
  )
)

(define (build-voip-phonebook-list)
  (append
    (let ((users (store-ref "main" "Users"))
          (thisuser (store-ref "main" "UserName" "")))
      (if (list-notempty? users)
        (let loop ((i 0) (result (list)))
          (if (= i (length users)) 
            result
            (loop (+ i 1)
              (let ((entry (list-ref users i)))
                (if (and (fx= (cadr entry) 1) (string? (car entry)) (not (string=? (car entry) thisuser)))
                  (append result (list (phonebook-list-element (list (car entry) "VOIP"))))
                  result
                ))
              )
          )
        )
        (list)
      )
    )
    (build-phonebook-list)
  )
)
(define (build-phonebook-list)
  (let ((phones (store-ref "main" "Phonebook")))
    (if phones
      (let loop ((i 0) (result (list)))
        (if (= i (length phones)) 
         result
         (loop (+ i 1)(append result (list (phonebook-list-element (list-ref phones i)))))
        )
      )
      (list) 
    )
  )
)
  
(define (phonebook-list-element entry)
  (lambda (g wgt x y w h s)
    (glgui:draw-text-left (+ x 5) (+ y (/ (- h 24) 2)) 145 24 (car entry) ascii_24.fnt White)   
    (if (string=? (cadr entry) "VOIP") 
      (glgui:draw-pixmap-left (+ x 155) (+ y 2) 76 24 voip-small.img (if voip:enabled gui:active-color gui:inactive-color)) 
      (glgui:draw-text-left (+ x 155) (+ y (/ (- h 24) 2)) (- (glgui-width-get) 155 10) 24 (cadr entry) ascii_24.fnt White)  
    )
    (if (glgui-widget-get g phonebook-edit-button 'hidden)
      (begin 
        (glgui:draw-pixmap-left (+ x 255) (+ y 2) 24 24 edit-small.img White)
        (glgui:draw-pixmap-left (+ x 285) (+ y 2) 24 24 delete-small.img White)
      )
    )
  )
)

;; Initialize the phonebook editor gui
(define gui:phonebook-editor-shown #f)
(define gui:phonebook-editor #f)
(define (init-gui-phonebook-editor)
  (set! gui:phonebook-editor (make-glgui))

  (let ((x 0)(y (+ 30 10))(h (glgui-height-get))(g gui:phonebook-editor))
    ;;Header row
    (glgui-label g (+ x 150) 330 50 16 "Value:" ascii_16.fnt White)
    ;; Labels
    (glgui-label g (+ x 5) 300 70 24 "Name" ascii_24.fnt White)
    (glgui-label g (+ x 5) 270 145 24 "Number/Pager" ascii_24.fnt White)
    ;;Data values
    (set! phonebook-name (glgui-inputlabel g (+ x 150) 300 150 24 "" ascii_24.fnt White))
    (glgui-widget-set! g phonebook-name 'callback phonebook-editor-name-callback)
    (set! phonebook-number (glgui-inputlabel g (+ x 150) 270 150 24 "" ascii_24.fnt White))
    (glgui-widget-set! g phonebook-number 'callback phonebook-editor-number-callback)
    ;; Keyboard for editing
    (set! phonebook-keyboard (glgui-ioskeypad g x y))
    ;; Bottom row with secondary navigation buttons
    (glgui-button-string g 10 3 (- (/ (glgui-width-get) 2) 20) 30 "Cancel" ascii_24.fnt phonebook-editor-cancel-button-callback)
    (glgui-button-string g (+ (/ (glgui-width-get) 2) 10) 3 (- (/ (glgui-width-get) 2) 20) 30 "Save" ascii_24.fnt phonebook-editor-save-button-callback)
  )
)

(define (phonebook-editor-name-callback g w t x y)
  (glgui-widget-set! g w 'focus #f)
  (glgui-widget-set! g phonebook-number 'focus #t)
  (glgui-widget-set! g phonebook-keyboard 'toggle #t)
)

(define (phonebook-editor-number-callback g w t x y)
  (glgui-widget-set! g phonebook-keyboard 'toggle #f)
  (phonebook-editor-save-button-callback g w t x y)
)

(define (phonebook-editor-cancel-button-callback g w t x y)
  ;; Show the menubar
  (glgui-widget-set! gui:menu navigation-bar 'hidden #f)
  (glgui-widget-set! gui:menu message-number 'hidden #f)
  (glgui-widget-set! gui:menu reminder-number 'hidden #f)
  ;; Hide the editor screen
  (set! gui:phonebook-editor-shown #f)
)

(define (phonebook-editor-save-button-callback g w t x y)
  (let ((lst (store-ref "main" "Phonebook"))
        (cur (glgui-widget-get gui:phonebook phonebook-list 'current)))
    (if (fx= cur (length lst))
      (set! lst (append lst (list (list (glgui-widget-get g phonebook-name 'label) (glgui-widget-get g phonebook-number 'label)))))
      (list-set! lst cur (list (glgui-widget-get g phonebook-name 'label) (glgui-widget-get g phonebook-number 'label)))
    )
    (store-set! "main" "Phonebook" (sort lst (lambda (a b) (string<? (car a) (car b)))))
    (glgui-widget-set! gui:phonebook phonebook-list 'list (build-voip-phonebook-list))
  )
  (glgui-widget-set! gui:phonebook-editor phonebook-number 'focus #f)
  ;; Show the menubar
  (glgui-widget-set! gui:menu navigation-bar 'hidden #f)
  (glgui-widget-set! gui:menu message-number 'hidden #f)
  (glgui-widget-set! gui:menu reminder-number 'hidden #f)
  ;; Hide the editor screen
  (set! gui:phonebook-editor-shown #f)
)

;; -----------------------------------------------------------------------------
;;  VOIP SCREEN RELATED FUNCTIONS
;; -----------------------------------------------------------------------------
(define gui:voip #f)	
(define (init-gui-voip)
  (set! gui:voip (make-glgui))
  (let ((x 10)(y (- (glgui-height-get) gui:menu-height 24 3))(w (glgui-width-get)))
    ;; Some text about call
    (set! voip-caller-label (glgui-label gui:voip x y w 24 "Call with: " ascii_24.fnt White))
    (set! voip-state-label (glgui-label gui:voip x (- y 125) w 24 "" ascii_24.fnt White))
    (glgui-widget-set! gui:voip voip-state-label 'align GUI_ALIGNCENTER)
    ;; IP info
    (set! voip-ip-label (glgui-label gui:voip (+ (/ w 2) x) (+ gui:navigation-height 20 30) (- (/ w 2) 20) 16 "IP:" ascii_16.fnt LightGray))

    ;; Buttons on the bottom to accept or ignore call. 
    (set! accept-voip-button
      (glgui-button-string gui:voip x (+ gui:navigation-height 10) (- (/ w 2) 20) 30 "Accept" ascii_24.fnt accept-voip-callback)
    )
    (set! reject-voip-button
      (glgui-button-string gui:voip (+ (/ w 2) x) (+ gui:navigation-height 10) (- (/ w 2) 20) 30 "Reject" ascii_24.fnt reject-voip-callback)
    )
    (set! terminate-voip-button
      (glgui-button-string gui:voip x (+ gui:navigation-height 10) (- w 20) 30 "Hang up / Disconnect" ascii_24.fnt terminate-voip-callback)
    ) 
    (glgui-widget-set! gui:voip terminate-voip-button 'hidden #t)    
    (set! return-phonebook-button
      (glgui-button-string gui:voip x (+ gui:navigation-height 10) (- w 20) 30 "Back to Phonebook" ascii_24.fnt return-phonebook-callback)
    ) 
    (glgui-widget-set! gui:voip return-phonebook-button 'hidden #t)
  )
)

(define (return-phonebook-callback g wgt t x y)
  (log-remote "Screen: Phonebook")
  (set! mode MODE_PHONEBOOK)
)

(define (hide-voip-init-buttons b)
  (glgui-widget-set! gui:voip accept-voip-button 'hidden b)
  (glgui-widget-set! gui:voip reject-voip-button 'hidden b)
  (glgui-widget-set! gui:voip terminate-voip-button 'hidden (not b))
  (glgui-widget-set! gui:voip return-phonebook-button 'hidden #t)
)


(define (accept-voip-callback g wgt t x y)
  (set! voip:ring #f)
  (hide-voip-init-buttons #t)
  (let ((ip (store-ref "main" "VOIPipaddr")))
    (if ip (begin 
      (glgui-widget-set! g voip-state-label 'label "CALL ACTIVE")  
      ;;Initialize our side of VOIP, parameters are IP volume 
      (rtaudio-start ip voip:volume)
      ;;Tell the other side to open their VOIP too
      ;;Syntax is (CALL pin DestinationName host-ipaddr state)
      (rupi-cmd (store-ref "main" "RupiClient" #f) "CALL" (store-ref "main" "Key" #f) 
        (store-ref "main" "VOIPcaller") (host-ipaddr) 1)
    ))
  )
)

(define (reject-voip-callback g wgt t x y)
  (set! voip:ring #f)
  (glgui-widget-set! g wgt 'hidden #t)
  (glgui-widget-set! g accept-voip-button 'hidden #t)
  (glgui-widget-set! g return-phonebook-button 'hidden #f)
  (glgui-widget-set! g voip-state-label 'label "CALL REJECTED")
  (rupi-cmd (store-ref "main" "RupiClient" #f) "CALL" (store-ref "main" "Key" #f) (store-ref "main" "VOIPcaller") (host-ipaddr) 0)
)

(define (terminate-voip-callback g wgt t x y)
  (set! voip:ring #f)
  (glgui-widget-set! g wgt 'hidden #t)
  (glgui-widget-set! g return-phonebook-button 'hidden #f)
  (glgui-widget-set! g voip-state-label 'label "CONNECTION CLOSED")
  (rtaudio-stop)
  (let ((caller (store-ref "main" "VOIPcaller")))
    (if caller  
      (rupi-cmd (store-ref "main" "RupiClient" #f) "CALL" (store-ref "main" "Key" #f) caller (host-ipaddr) 0)
    )
  )
  (store-set! "main" "VOIPcaller" #f)
  (log-remote "Closed call")
)

(define (call-voip caller)
  (hide-voip-init-buttons #t)
  (store-set! "main" "VOIPcaller" caller)
  (glgui-widget-set! gui:voip voip-caller-label 'label (string-append "Call with: " caller))
  (glgui-widget-set! gui:voip voip-state-label 'label "RINGING")
  (log-remote "Screen: VOIP")
  (set! mode MODE_VOIP)
  ;; Make the call
  (rupi-cmd (store-ref "main" "RupiClient" #f) "CALL" (store-ref "main" "Key" #f) caller (host-ipaddr) 9)
  ;; Init the audio parts  
  (audiofile-forceplay audio:phone)
  (set! voip:ring (fl+ ##now 4.))
  (set! voip:ring-count 3)
)

(define (handle-voip caller ip state)
  (glgui-widget-set! gui:voip voip-ip-label 'label (string-append "IP: " (ipaddr->string (s32->u8vector ip))))
  (if (fx= state 1)
    ;; Add the Call to the Chat log
    (begin
      (store-set! "main" "ChatMessages" (append
        (list (list ##now caller (string-append "Made VOIP call to " caller) 0))
        (store-ref "main" "ChatMessages" '())
      ))
      (glgui-widget-set! gui:voip voip-state-label 'label "CALL ACTIVE")
      (hide-voip-init-buttons #t)
      (set! voip:ring #f)
      (rtaudio-start ip voip:volume)
    )
  )
  (if (fx= state 9)
    (begin 
      ;; Add the Call to the Chat log
      (store-set! "main" "ChatMessages" (append
        (list (list ##now caller (string-append "Received VOIP call by " caller) 1))
        (store-ref "main" "ChatMessages" '())
      ))
      (hide-voip-init-buttons #f)
      (store-set! "main" "VOIPipaddr" ip)
      (store-set! "main" "VOIPcaller" caller)
      (glgui-widget-set! gui:voip voip-state-label 'label "RINGING")
      (glgui-widget-set! gui:voip voip-caller-label 'label (string-append "Call from: " caller))
      (log-remote "Screen: VOIP")
      (set! mode MODE_VOIP)
      ;; Init the audio parts  
      (audiofile-forceplay audio:phone)
      (set! voip:ring (fl+ ##now 4.))
      (set! voip:ring-count 3)
    )
  )
  (if (fx= state 0)
    (begin
      (set! voip:ring #f)
      (rtaudio-stop)
      (glgui-widget-set! gui:voip reject-voip-button 'hidden #t)
      (glgui-widget-set! gui:voip accept-voip-button 'hidden #t)
      (glgui-widget-set! gui:voip terminate-voip-button 'hidden #t)
      (glgui-widget-set! gui:voip voip-state-label 'label "CALL HUNG UP")
      (glgui-widget-set! gui:voip return-phonebook-button 'hidden #f)
    )
  )
)

;; -----------------------------------------------------------------------------
;;  MENU BAR RELATED FUNCTIONS
;; -----------------------------------------------------------------------------

;; Initialize the main gui parts
(define gui:menu #f)	
(define gui:empty #f)	;; Used if I don't want any gui elements to be shown - else case for messaging details not shown
(define (init-gui-menu)
    (set! gui:menu (make-glgui))
    (set! gui:empty (make-glgui))
    
    ;; Top menu bar with title
    (glgui-menubar gui:menu 0 (- (glgui-height-get) gui:menu-height) (glgui-width-get) gui:menu-height)
  
    (set! title (glgui-label gui:menu 5 (- (glgui-height-get) 28) 200 24 "" ascii_24.fnt White))
    ;; Clock in upper right corner
    (set! clock (glgui-label gui:menu (- (glgui-width-get) 60) (- (glgui-height-get) 24) 60 16 "" ascii_16.fnt White))
    ;; Battery Indicator left of Clock
    (set! battery (glgui-battery gui:menu (- (glgui-width-get) 100) (- (glgui-height-get) 25) 40 18 100))

    ;; Bottom menu bar
    (set! navigation-bar (glgui-button gui:menu 1 1 (- (glgui-width-get) 1) gui:navigation-height icon-list navigation-callback))
    (glgui-widget-set! gui:menu navigation-bar 'value mode)
    (glgui-widget-set! gui:menu navigation-bar 'toggle-normal-color gui:toggle-normal-color)
    (glgui-widget-set! gui:menu navigation-bar 'toggle-selected-color gui:toggle-selected-color)
    
    ;; Number of messages
    ;;glgui-label g x y w h label fnt color
    (set! message-number (glgui-label gui:menu (* MODE_MESSAGING (/ (glgui-width-get) (length icon-list))) 22 64 24 "0" ascii_24.fnt White))
    (glgui-widget-set! gui:menu message-number 'align GUI_ALIGNCENTER)

    ;; Number of reminders
    (set! reminder-number (glgui-label gui:menu (- (* MODE_REMINDER (/ (glgui-width-get) (length icon-list))) 4) 19 64 24 "" ascii_24.fnt gui:toggle-normal-color))
    (glgui-widget-set! gui:menu reminder-number 'align GUI_ALIGNCENTER)
)

;; -----------------------------------------------------------------------------
;;  POPUP RELATED FUNCTIONS
;; -----------------------------------------------------------------------------
(define gui:popup #f)	
(define (init-gui-popup)
    (set! gui:popup (make-glgui))
    ;; Message popup bubble
    (let ((x (/ (glgui-width-get) 8))
	  (y (/ (glgui-height-get) 2))
	  (w (* (/ (glgui-width-get) 4) 3))
	  (h (/ (glgui-height-get) 4)))
      (set! popup-box (glgui-box gui:popup x y w h (color-fade Black 0.75)))
      (glgui-widget-set! gui:popup popup-box 'callback hide-popup-click)
      (glgui-widget-set! gui:popup popup-box 'rounded #t)
      (set! popup-label (glgui-label gui:popup x (- (+ y h) 25) w 25 "" ascii_24.fnt White))
      (glgui-widget-set! gui:popup popup-label 'align GUI_ALIGNCENTER)
      (set! popup-text (glgui-label-wrapped gui:popup x (+ y 10) w (- h 25 12) "" ascii_16.fnt White))
      (glgui-widget-set! gui:popup popup-text 'align GUI_ALIGNCENTER)
      (set! popup-close-text (glgui-label gui:popup x y w 12 "Touch to see Message" ascii_12.fnt White))
      (glgui-widget-set! gui:popup popup-close-text 'align GUI_ALIGNCENTER)
      (set! popup-timer (glgui-box gui:popup x y w 12 gui:active-color))
    )
)

(define (hide-popup-click g w t x y)
  (hide-popup)
  (glgui-widget-set! gui:menu navigation-bar 'value MODE_MESSAGING)
  (log-remote "Screen: Messaging")
  (set! mode MODE_MESSAGING)
  (if landscape? (glgui-orientation-set! GUI_PORTRAIT))
)

(define (hide-popup)
  (glgui-widget-set! gui:popup popup-box 'hidden #t)
  (glgui-widget-set! gui:popup popup-label 'hidden #t)
  (glgui-widget-set! gui:popup popup-text 'hidden #t)
  (glgui-widget-set! gui:popup popup-close-text 'hidden #t)
  (glgui-widget-set! gui:popup popup-timer 'hidden #t)
  (store-set! "main" "popup-text" #f)
)

(define (show-popup)
  (glgui-set! gui:popup 'yofs (if landscape? -125 0))
  (glgui-set! gui:popup 'xofs (if landscape? 75 0))
  (glgui-widget-set! gui:popup popup-box 'hidden #f)
  (glgui-widget-set! gui:popup popup-label 'hidden #f)
  (glgui-widget-set! gui:popup popup-text 'hidden #f)
  (glgui-widget-set! gui:popup popup-close-text 'hidden #f)
  (glgui-widget-set! gui:popup popup-timer 'hidden #f)
  (glgui-widget-set! gui:popup popup-timer 'w (glgui-widget-get gui:popup popup-text 'w))
  (let ((popup (store-ref "main" "popup-text" "")))
    (glgui-widget-set! gui:popup popup-label 'label (if (list-notempty? popup) (car popup) ""))
    (glgui-widget-set! gui:popup popup-text 'label (if (list-notempty? popup) (cadr popup) popup))
  )
)

(define (navigation-callback g wgt t x y)
  ;; Get the currently selected screen from the navigation-menu value
  (let ((newmode (glgui-widget-get g wgt 'value)))
    ;; Option to log out off app on iPod
    (if (and (fx= mode MODE_OVERVIEW) (fx= mode newmode))
      (let ((min_count 1)
            (lc (store-ref "main" "LogoutCount" 0)))
	  (store-set! "main" "LogoutCount" (fx+ lc 1))
	  (if (fx> lc min_count)
	    (begin
	      ;; Logout 
              (rupi-logout)
              ;; Terminate the app after showing popup.
              (set! rupi:addr #f)
              (glgui-widget-set! gui:popup popup-close-text 'label "")
              (glgui-widget-set! gui:popup popup-box 'callback #f) ;; needed so can't go to messaging screen
              (store-set! "main" "popup-text" (list "Shutting Down" "Closing the application to conserve battery power."))
	    )
	  )
      )
      (store-clear! "main" "LogoutCount")
    )
    (if (not (fx= mode newmode))
      (begin
	;; Go to the new screen
	(set! mode newmode)
	;; Hide popups if currently shown
	(hide-popup)
	;; Log which screen is used
	(log-remote (string-append "Screen: " 
	  (cond 
	    ((fx= newmode MODE_OVERVIEW) "Overview")
	    ((fx= newmode MODE_MESSAGING) "Messaging")
	    ((fx= newmode MODE_REMINDER) "Reminder")
	    ((fx= newmode MODE_ROOMS) "RoomSubscription")
	    ((fx= newmode MODE_USERS) "Users")
	    ((fx= newmode MODE_PHONEBOOK) "Phonebook")
	    (else "Unknown")
	  )
	))
      )
    )
  )
)

;; Number of yet unanswered alert messages for the menu icon
(define (unanswered-message-number-get) 
(+ ;; Add number of unanswered chat messages and pages
  (let ((chats (store-ref "main" "ChatMessages")))
    (if (list-notempty? chats)
      (let loop ((i 0) (result 0) (people (list)))
        (if (fx= i (length chats)) result
          (let* ((msg (list-ref chats i))
                 (person (cadr msg))
                 (tstamp (car msg))
                 (send (cadddr msg)))
            ;; Test if the list already has an entry of a given person, if not add the first one we occur.
            ;; List is in display order, so append on the bottom!
            (loop (fx+ i 1)
                  (if (member person people) result (if (and (fl< (fl- ##now tstamp) 3600.)(fx= send 0)) (fx+ result 1) result))
                  (if (member person people) people (append people (list person))))
          )
        )
      )
      0
    )
  )

  (let ((messages (store-ref "main" "AlertMessages")))
    (if (list-notempty? messages)
      (let loop ((i 0) (result 0))
	(if (fx= i (length messages)) result
	  (loop (fx+ i 1)(if (fx> (list-ref (list-ref messages i) 3) 0) (fx+ result 1) result))
	)
      )
      0
    )
  )
))

(define (unanswered-message-highest-priority-value-get)
  (let ((messages (store-ref "main" "AlertMessages")))
    (if (list-notempty? messages)
      (let loop ((i 0) (result -1))
	(if (fx= i (length messages)) (if (fx= result -1) 1 result)
	  (loop (fx+ i 1)(let ((prio (list-ref (list-ref messages i) 3)))
			(if (and (fx< prio 10) (fx> prio result)) prio result)
		      )
	  )
	)
      )
      1
    )
  )
)
;; -----------------------------------------------------------------------------
;;  NETWORK COMMUNICATION FUNCTIONALITY
;; -----------------------------------------------------------------------------
(define gui:setup #f)	
(define (init-gui-setup)
  (set! gui:setup (make-glgui))
  (let ((x 0)(y 0)(h (glgui-height-get))(w (glgui-width-get))(g gui:setup))
    (glgui-pixmap g (/ (fx- w (car telePORT-logo.img)) 2) (- h (cadr telePORT-logo.img) 20) telePORT-logo.img)  
    ;;Header row
    (glgui-label g (+ x 10) 350 (- w 20) 24 "Please set your VitalNode" ascii_24.fnt White)
    (glgui-label g (+ x 10) 320 (- w 20) 24 "server name:" ascii_24.fnt White)
    ;; Labels and Data value
    (set! server-name
      (glgui-inputlabel g (+ x 5) (- (/ h 2) 15) (- w 65 5 5 5) 30 rupi:hostname ascii_24.fnt White (color-shade White 0.1))
    )
    (glgui-widget-set! g server-name 'callback server-name-callback)
    (glgui-widget-set! g server-name 'focus #t)
    (glgui-widget-set! g server-name 'align GUI_ALIGNRIGHT)
    (glgui-button-string g (- w 65 5) (- (/ h 2) 15) 65 30 "Save" ascii_24.fnt server-name-callback)
    ;; Keyboard for editing
    (glgui-ioskeypad g x y)
  )
)

(define (server-name-callback g wgt t x y)
  (hide-popup)
  (set! rupi:addr 
    (with-exception-catcher
      (lambda (e) #f)
      (lambda () (car (host-info-addresses (host-info (glgui-widget-get gui:setup server-name 'label)))))
    )
  )
  (if rupi:addr
    (begin
      (set! rupi:hostname (glgui-widget-get gui:setup server-name 'label))
      (with-output-to-file hostname-file (lambda () (display rupi:hostname)))
      (set! mode MODE_LOGIN)
    )
    (store-set! "main" "popup-text" (list "Invalid Hostname" "Please select a valid VitalNode server name."))
  )
)

;; Log some data to the status store
(define (log-remote str)
  (if (string? str) 
    (let ((buf (store-ref "main" "LogBuffer" '())))
      ;; We submit logs in Message thread now
      (store-set! "main" "LogBuffer" (append buf (list str))) 
      (log-status str)
    )
    (log-warning str)
  )
)

;; Parse a rupi-return list
 (define (store-update-data lst)
  (let loop ((i 0))
    (if (< i (length lst))
      (let ((row (list-ref lst i)))
	(if (and (list? row) (fx> (length row) 1) (list-notempty? (cdr row))) (store-update-list (car row) (cdr row)))
	(loop (+ i 1))
      )
    )
  )
)

;; Assign a returned data structure to a store
(define (store-update-list store lst)
  (let loop ((i 0))
    (if (< i (length lst))
      (let ((row (list-ref lst i)))
	;; I want to use CADR not CDR here as otherwise I get an extra list around the thing
	(store-set! store (car row)(cadr row))
	(loop (+ i 1))
      )
    )
  )
)

;; Next iteration using RUPI for connectivity
(define (init-server-communication store)
  (let ((rc (rupi-client 0 rupi:key rupi:addr rupi:port)))
    (store-set! store "RupiClient" rc) ;;stores the handle to a RUPI client
    (let ((data (rupi-cmd rc "GETMYROOMS" (store-ref store "Key")))) 
       (store-set! store "myRooms" data)) ;; Load our subscribed rooms from the last logout
    (let ((data (rupi-cmd rc "GETROOMS" (store-ref store "Key"))))
       (store-set! store "Rooms" (if (list-notempty? data) data '())))
    (for-each (lambda (l) (make-store (car l))) (store-ref store "Rooms")) ;; For the list of rooms we got make stores
  )

  ;; Load the phonebook
  (let ((data (rupi-cmd (store-ref store "RupiClient" #f) "GETPHONEBOOK" (store-ref store "Key"))))
    (if data (store-set! store "Phonebook" data))
  )

  ;; Start the main communication thread
  (store-set! store "Thread" (thread-start! (make-safe-thread (lambda ()
    (let loop ()
      (let ((lastupdatetime (store-ref store "LastUpdateTime" 0.))
            (rupi  (store-ref store "RupiClient" #f))
            (key   (store-ref store "Key"))
            (timeout 2.))
        (let ((data (rupi-cmd rupi "GETMESSAGES" key lastupdatetime)))
          (if (list-notempty? data) ;; Always a list
            (begin
              (store-update-messages data)
              (let ((data2 (rupi-cmd rupi "GETREMINDERS" key)))
                (if data2 ;; An empty list is a valid option here
                  (begin
                    (store-set! store "Reminders" data2)
                    (glgui-widget-set! gui:reminder reminder-list 'list (build-reminder-list))
                  )
                )
              )
            ) 
          )
        )
        (if (not app:suspended) (begin
          (let ((data (rupi-cmd rupi "GETOVERVIEW" key)))
            (if (list-notempty? data) (store-update-data data)) ;; Always a list
          )
          (let ((data (rupi-cmd rupi "GETUSERS" key)))
            (if (list-notempty? data) ;; Always a list
              (begin
                (store-set! store "Users" (append (map (lambda (l) (list l 1)) (car data))
                                                  (map (lambda (l) (list l 0)) (cadr data))))
                (glgui-widget-set! gui:users user-list 'list (build-user-list))
                (if (not (glgui-widget-get gui:phonebook phonebook-edit-button 'hidden))
                  (glgui-widget-set! gui:phonebook phonebook-list 'list (build-voip-phonebook-list))
                )
              ) 
            )
          )
          (let ((data (rupi-cmd rupi "GETROOMS" key)))
            (if (list-notempty? data) 
              (begin
                (store-set! store "Rooms" (sort-rooms data))
                (glgui-widget-set! gui:rooms room-list 'list (build-room-list))
              )
            )
          )
        ))
        (let ((buf (store-ref store "LogBuffer")))
          (if (list-notempty? buf)
            (let loop ((i 0))
              (if (fx= i (length buf)) (store-clear! store "LogBuffer")
                (begin
                  (rupi-cmd rupi "LOG" key (list-ref buf i))
                  (loop (fx+ i 1))
                )
              )
            )
          )
        )
        (thread-receive timeout #f) ;; this way we can instant sync with (thread-send curstore #f)
        (loop)
      )
    )
  ) 'communication-thread))
  )
)

;; -----------------------------------------------------------------------------
;;  OTHER FUNCTIONS
;; -----------------------------------------------------------------------------

;; Check for empty lists - this one is better than using pair? for it!
(define (list-notempty? lst)
  (and (list? lst) (not (null? lst)))
)

;; keep only alerts/pages/requests/message that are younger than age seconds
(define (expire-messages lst age)
  (maps (lambda(l) (if (< (- ##now (car l)) age) l '())) lst)
)

;; logout function, so we transfer our internal state back to the server
(define (rupi-logout)
  (rupi-cmd (rupi-client 0 rupi:key rupi:addr rupi:port) "LOGOUT" 
    (store-ref "main" "Key") (store-ref "main" "LastUpdateTime") 
    (store-ref "main" "AlertMessages") (store-ref "main" "ChatMessages")
  )
  ;; Delete the file which would log us in again automatically
  (if (file-exists? login-file)
    (delete-file login-file)
  )
)

;; I need a function to find the position of a string in a list
(define (find-list-pos lst str)
  (cond
    ((not (list-notempty? lst)) #f)
    ((string=? str (car lst)) 0)
    (else (+ 1 (find-list-pos (cdr lst) str)))
  )
)

(define (sort-rooms rooms)
  ;; No longer sorting by subscription status just PACU or not
  (let ((user-name (store-ref "main" "UserName"))
        (with-pacu (= (glgui-widget-get gui:rooms pacu-button 'color) Blue)))
    (if with-pacu rooms
      (let loop ((i 0) (result (list)))
        (if (= i (length rooms)) result
          (loop (+ i 1)(if (or (not (char=? (string-ref (car (list-ref rooms i)) 0) #\P))
                               (member user-name (cadr (list-ref rooms i))))
            (append result (list (list-ref rooms i))) result))
        ))
    )   
#|
    ;; Some sorting dark Magic: First rooms I have, then rooms that are uncovered, finally rooms covered by others.
    (append 
      (let loop ((i 0) (result (list)))
	(if (= i (length rooms)) result
	    (loop (+ i 1)(if (member user-name (cadr (list-ref rooms i)))(append result (list (list-ref rooms i))) result))
	)
      )
      (let loop ((i 0) (result (list)))
	(if (= i (length rooms)) result
	    (loop (+ i 1)(if (and (not (list-notempty? (cadr (list-ref rooms i))))
				  (if with-pacu #t (not (char=? (string-ref (car (list-ref rooms i)) 0) #\P)))
			     ) (append result (list (list-ref rooms i))) result))
	)
      )
      (let loop ((i 0) (result (list)))
	(if (= i (length rooms)) result
	    (loop (+ i 1)(if (and (list-notempty? (cadr (list-ref rooms i)))
				  (not (member user-name (cadr (list-ref rooms i))))
				  (if with-pacu #t (not (char=? (string-ref (car (list-ref rooms i)) 0) #\P)))
			     ) (append result (list (list-ref rooms i))) result))
	)
      )
    )
|#
  )
)

;; Define the colors to be shown for a given message priority
(define (priority-color-get priority)
  ;; Number < 0 are answered messages/alarms, >=0 are unanswered
  (cond ((= priority 3) Red)
	((= priority 2) Orange)
	((= priority 1) Blue)
	((= priority -7) (color-fade Red 0.2))
	((= priority -8) (color-fade Orange 0.2))
	((= priority -9) (color-fade Blue 0.2))
	(else Black)
  )
)

;; -----------------------------------------------------------------------------
;;  MAIN PROGRAM
;; -----------------------------------------------------------------------------
(main
;;
;; initialization, here the static gui elements get build
;;
  (lambda (w h)
    ;; If we don't run on the phone the window size needs to be limited to iPhone <=3 size
    (if (or (string=? (system-platform) "macosx")
	    (string=? (system-platform) "linux") 
            (string=? (system-platform) "android") 
            (string=? (system-platform) "win32")) 
    	      (make-window 320 480)
    )
    ;; Our gui will be designed for portrait layout
    (glgui-orientation-set! GUI_PORTRAIT)
    ;; Initialize the local data stores
    (make-store "main")
    ;; Initialize the menu and the popup
    (init-gui-menu)
    (init-gui-popup)
    ;; Get the server hostname
    (if (file-exists? hostname-file) 
        (set! rupi:hostname (with-input-from-file hostname-file (lambda () (read-line))))
    )
    (set! rupi:addr 
      (with-exception-catcher
        (lambda (e) #f)
        (lambda () (car (host-info-addresses (host-info rupi:hostname))))
      )
    )
    ;; If we can't get a hostname resolved we don't want to bring up login screen
    (if rupi:addr
      (begin
	;; Initializers for the different gui screens and parts [the rest is AFTER LOGIN]
	(init-gui-login)
	(audiofile-init)
        (set! audio:message (audiofile-load "Message"))
        (set! audio:alert (audiofile-load "Alert"))
        (set! audio:emergency (audiofile-load "Emergency"))
        (set! audio:phone (audiofile-load "Phone"))
        (set! audio:disconnect (audiofile-load "Disconnect"))
        ;; Also need to initialize the setup gui
        (init-gui-setup)
      )
      (begin 
	(set! gui:login (make-glgui))
	(glgui-box gui:login 0 0 (glgui-width-get) (glgui-height-get) (color-shade Red 0.4))
	(store-set! "main" "popup-text" (list "NO CONNECTION" "FAILURE: Can't find the VitalNode. Please check the Wifi connection is active. We will quit now"))
	(show-popup)
      )
    )
    ;; I want logfiles so the logile directory needs to be made here
    (if (not (file-exists? log:path)) (create-directory log:path)) 
    ;; Load the login-file if we crashed and log us back in
    (if (file-exists? login-file) 
      (begin 
        (store-set! "main" "Key" (with-input-from-file login-file (lambda () (read-line))))
        (delete-file login-file)
        (login-callback gui:login #f 0 0 0)
      )
    )
  )
;;
;; events (and their handling) - Here updates are done after guis are already defined
;;
  (lambda (t x y) 
    ;; Check if a screen is in landscape orientation
    (let ((w (glgui-width-get))
          (h (glgui-height-get)))
      (if (and w h)
        (set! landscape? (> w h))
      )
    )
    ;; Rotation change events
    (if (fx= t EVENT_ORIENTATION)
      (if (or (fx= mode MODE_WAVES) (fx= mode MODE_CHAT))
        (glgui-orientation-set! x)
      )
    )
    ;; Keyboard events
    (if (fx= t EVENT_KEYRELEASE) 
      (begin
        ;; These are key presses, in case of ESC exit
        (if (fx= x EVENT_KEYESCAPE)
          (terminate)
        )
        ;; Change orientation with TAB
        (if (fx= x EVENT_KEYTAB)
          (if (or (fx= mode MODE_WAVES) (fx= mode MODE_CHAT))
            (glgui-orientation-set! (if (fx= glgui:rotate 0) 2 1))
          )
        )
	;; Keyboard events:
	(if (fx= mode MODE_CHAT)
	  (let ((oldstr (glgui-widget-get gui:messaging-keyboard message-string 'label))
		(maxlen 160)) ;; Twitter uses 140, SMS are 160
	    ;; Legal ASCII characters
	    (if (and (fx>= x 32) (fx< x 127) (fx< (string-length oldstr) maxlen))
	      (glgui-widget-set! gui:messaging-keyboard message-string 'label (string-append oldstr (string (integer->char x))))
	    )
	    ;; Backspace
	    (if (and (fx= x 3) (fx> (string-length oldstr) 0)) 
              (glgui-widget-set! gui:messaging-keyboard message-string 'label (substring oldstr 0 (- (string-length oldstr) 1)))
	    )
	    ;; Return
	    (if (fx= x 1)
	      (send-message-callback (if landscape? gui:chat-landscape gui:messaging-keyboard) message-string #f 0 0)
	    )
            ;; Update the message string in the landscape version too
            (glgui-widget-set! gui:chat-landscape message-string-landscape 'label 
              (glgui-widget-get gui:messaging-keyboard message-string 'label)
            )
	  )
	)
      )
    ) 
    ;; Add something for the battery level
    (if (fx= t EVENT_BATTERY) 
      (begin  
	(store-set! "main" "battery" x)
	(glgui-widget-set! gui:menu battery 'value x)
      )
    )
    ;;Redraw with some updated numbers
    (if (fx= mode MODE_WAVES)
      (let* ((rooms (store-ref "main" "myRooms"))
	     (wav-room (store-ref "main" "waveform-room-idx"))
	     (or-name (list-ref rooms wav-room)))
	(if or-name
	  (begin
	    (update-waves or-name)
	    ;; also update the screen indicator
	    (glgui-widget-set! gui:waves screenindicator 'idx wav-room)
	    (glgui-widget-set! gui:waves screenindicator 'screens (length rooms))
	    (glgui-widget-set! gui:waves-landscape screenindicator-landscape 'idx wav-room)
	    (glgui-widget-set! gui:waves-landscape screenindicator-landscape 'screens (length rooms))
	  )
	)
      )
    )
    ;; Update trend data
    (if (fx= mode MODE_TRENDS)
      (if (fl> (fl- ##now rupi:last-trend-update) 10.)
        (let* ((rooms (store-ref "main" "myRooms"))
               (wav-room (store-ref "main" "waveform-room-idx"))
               (or-name (list-ref rooms wav-room)))
          (if or-name (load-trends or-name))
          (set! rupi:last-trend-update ##now)
        )
      )
    )
    ;; Ring for VOIP scren
    (if (and (fx= mode MODE_VOIP) voip:ring) (begin
      (if (and (fx= voip:ring-count 0) (fl> ##now voip:ring))
        (begin
          (glgui-widget-set! gui:voip voip-state-label 'label "CALL TIMED OUT")
          (glgui-widget-set! gui:voip terminate-voip-button 'hidden #t)
          (set! voip:ring #f)
        )
      )
      (if (and (fx> voip:ring-count 0) (fl> ##now voip:ring))
        (begin
          (set! voip:ring (fl+ ##now 4.))
          (set! voip:ring-count (fx- voip:ring-count 1))
          (audiofile-forceplay audio:phone) 
        )
      )
    ))
    ;; Update the menu icon with the number of messages
    (let ((num (unanswered-message-number-get)))
      (if (fx= num 0)
	(glgui-widget-set! gui:menu message-number 'label "")
	(begin
	  (glgui-widget-set! gui:menu message-number 'label (number->string num))
	  (glgui-widget-set! gui:menu message-number 'color (priority-color-get (unanswered-message-highest-priority-value-get)))
	)
      )
    )
    ;; Update the reminder icon with the number of reminders
    (let ((num (length (store-ref "main" "Reminders" '()))))
      (glgui-widget-set! gui:menu reminder-number 'label (if (fx= num 0) "" (number->string num)))
    )
    ;; Create the popup if needed and hide when time expired
    (let ((timeout 4.)
	  (text (store-ref "main" "popup-text" #f))
	  (tstamp (store-timestamp "main" "popup-text"))
	  (hidden (glgui-widget-get gui:popup popup-box 'hidden)))
      (if (and text hidden (fl< (fl- ##now tstamp) 1.))
	(show-popup)
      )
      (if (and text (not hidden))
        (glgui-widget-set! gui:popup popup-timer 'w 
          (fix (* (glgui-widget-get gui:popup popup-text 'w ) (fl/ (fl- timeout (fl- ##now tstamp)) timeout))))
      )
      ;; We do a hack here: The ##now-3.0 is because the rupi timout in Login takes 3 sec and we still want the popup.
      (if (and (not hidden) 
               (fl> (fl- (if (and rupi:error (fx= mode MODE_LOGIN)) (fl- ##now 2.) ##now) tstamp) (if (glgui-widget-get gui:popup popup-box 'callback) timeout (fl/ timeout 2.)))
          )
        (begin
          (hide-popup)
          (if (and (not rupi:addr) (not (fx= mode MODE_SETUP)))
            (force-terminate) ;;(terminate) doesn't do it on the iPod/iPhone
          ) 
        )
      )
    )

    ;; Remove old messages from alert list
    (let ((age 30.))
      (if (and gui:alertcheck-tstamp (fl> (fl- ##now gui:alertcheck-tstamp) age))
        (begin
          (set! gui:alertcheck-tstamp ##now)
          (store-set! "main" "AlertMessages"
            (maps (lambda (lst) (if (fl> (fl- ##now (car lst)) (if (fx< (cadddr lst) 0) gui:alert-expire-answered-age gui:alert-expire-unanswered-age)) 
              '() 
              lst)) 
              (store-ref "main" "AlertMessages" '())
            )
          )
          (glgui-widget-set! gui:messaging alert-list 'list (build-alert-list)) 
          (glgui-widget-set! gui:messaging alert-list 'offset 0) 
        )
      )
    )

    ;; Add logging of battery level every 5min
    (let ((age 300.))
      (if (and gui:battery-tstamp (fl> (fl- ##now gui:battery-tstamp) age))
	(begin
	  (set! gui:battery-tstamp ##now)
	  (log-remote (string-append "Battery: " (number->string (store-ref "main" "battery" -1)) "%"))
	)
      )
    )
    ;; VitalNode disconnect logging
    (let ((age 10.)
	  (lstchk (store-ref "main" "ConnChkTime" 0.)))
      (if (and (not (fx= mode MODE_LOGIN)) (fl> (fl- ##now lstchk) age))
	(let ((lstmsg (store-ref "main" "LastUpdateTimeLocal" 0.))) ;; This needs to use local time to avoid clock drift problems.
	  (if (and (fl> lstmsg 0.) (fl> (fl- ##now lstmsg) age))
	    (let* ((delta-time (fix (fl- ##now lstmsg)))
                   (logstr (string-append "No communication with VitalNode for " 
		                         (number->string delta-time) "sec")))
	      (log-status logstr)
	      (glgui-widget-set! gui:menu clock 'label "OFFLINE")
              (store-set! "main" "popup-text" (list "Connection Lost" (string-append logstr ". Trying to reconnect!")))
              (if (and (fx> delta-time 15) (fx<= (modulo delta-time 60) 10))
                (audiofile-forceplay audio:disconnect)
              )
	    )
	  )
	  (store-set! "main" "ConnChkTime" ##now)
	)
      )
    )
    ;; Room Transfer Icon Timeout
    (let ((rst (store-ref "main" "RoomSendTime")))
      (if rst
        (let ((age 60.))
          (if (fl> (fl- ##now rst) age) (begin
            (store-clear! "main" "RoomSendTime")
            (glgui-widget-set! gui:rooms transfer-button 'hidden #f))
          )
        )
      )
    )
    ;; Update the title row according to where we are
    (cond
      ((fx= mode MODE_MESSAGING) (glgui-widget-set! gui:menu title 'label "Messaging"))
      ((fx= mode MODE_OVERVIEW) (glgui-widget-set! gui:menu title 'label "Overview"))
      ((fx= mode MODE_ROOMS) (glgui-widget-set! gui:menu title 'label "Room Subscription"))
      ((fx= mode MODE_USERS) (glgui-widget-set! gui:menu title 'label "Contacts"))
      ((fx= mode MODE_REMINDER) (glgui-widget-set! gui:menu title 'label "Reminders"))
      ((fx= mode MODE_REMINDER_SETUP) (glgui-widget-set! gui:menu title 'label "Reminder Setup"))
      ((fx= mode MODE_PHONEBOOK) (glgui-widget-set! gui:menu title 'label "Phonebook"))
      ((fx= mode MODE_VOIP) (glgui-widget-set! gui:menu title 'label "VOIP Call"))
    )
    ;;
    ;; Calls some event handler, and plot the right gui depending on which screen is active
    ;;
    (glgui-event (if (fx= mode MODE_LOGIN)
      (list gui:login gui:popup)
      (list 
        (cond ((fx= mode MODE_OVERVIEW) gui:overview)
              ((fx= mode MODE_WAVES) (if landscape? gui:waves-landscape gui:waves))
              ((fx= mode MODE_MESSAGING) gui:messaging)
              ((fx= mode MODE_REMINDER) gui:reminder)
              ((fx= mode MODE_REMINDER_SETUP) gui:reminder-setup)
              ((fx= mode MODE_ROOMS) gui:rooms)
              ((fx= mode MODE_USERS) gui:users)
              ((fx= mode MODE_CHAT) (if landscape? gui:chat-landscape gui:chat))
              ((fx= mode MODE_ALERT) gui:alert)
              ((fx= mode MODE_TRENDS) gui:trends)
              ((fx= mode MODE_VOIP) gui:voip)
              ((fx= mode MODE_PHONEBOOK) (if gui:phonebook-editor-shown gui:phonebook-editor gui:phonebook))
              ((fx= mode MODE_SETUP) gui:setup)
              (else gui:login)
        )
        (if (and (fx= mode MODE_CHAT) gui:messaging-detail-shown (not landscape?)) gui:messaging-detail-shown gui:empty)
        (if (or (fx= mode MODE_SETUP) (and (fx= mode MODE_WAVES) landscape?)) gui:empty gui:menu)
        gui:popup
      )
    ) t x y)

    ;; Try this to see if it reduces CPU usage
    (##gc)                      ;; This calls the garbage collector 
    (if (and (fx= t EVENT_REDRAW) (not (fx= mode MODE_CHAT)))
      (thread-sleep! 0.04) ;; If all we do is redraw sleep a bit more 40usec (10 was from normal sleep)
    )
  )
  ;;
  ;; termination
  ;;
  (lambda ()
    (rupi-logout) 
    #t
  )
  ;;
  ;; suspension
  ;;
  (lambda () 
    (if (fx= mode MODE_WAVES) (begin
      (set! mode MODE_OVERVIEW)
      (if landscape? (glgui-orientation-set! GUI_PORTRAIT))
    ))
    (if (fx= mode MODE_TRENDS) (set! mode MODE_OVERVIEW))
    (glgui-suspend)
  )
  (lambda () 
   (glgui-resume)
  )
) ;;eof
