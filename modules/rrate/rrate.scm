#|
lnHealth - Health related apps for the LambdaNative framework
Copyright (c) 2009-2015, University of British Columbia
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

;; Module for measuring and confirming respiratory rate
;; Christian Leth Petersen 2012, Dustin Dunsmuir 2014, Matthias Görges 2015

;; Load the localization support
(local-load "rrate-local.csv")
(local-index-set! 1);; 1 English, 2 Khmer

;; Settings page for configuring number of taps and consistency percent for threshold
(define rrate:settings:cont #f)
(define rrate:settings:tapslist #f)
(define rrate:settings:percentlist #f)
(define rrate:settings:donebutton #f)

;; Flag for whether on settings page or not
(define rrate:settings:viewing #f)

;; Setting options to chose from
(define rrate:settings:tapchoices (list "3" "4" "5" "6"))
(define rrate:settings:consistency #f)
(define rrate:settings:percentchoices (list "10" "11" "12" "13" "14"))
(define rrate:settings:vibrate_trigger #f)
(define rrate:settings:vibrate_box #f)

;; Showing vibrate setting option
(define rrate:settings:show_vibrate #f)

(define (rrate:setting-init x y w h)
  (set! rrate:settings:cont (glgui-container rrate:gui x y w h))
  
  ;; Only show option for vibrating the phone with sound if on Android
  (set! rrate:settings:show_vibrate (string=? (system-platform) "android"))

  (glgui-pixmap rrate:settings:cont 0 43 settings_bg.img)
  
  ;; Setting for how many taps are needed
  (glgui-widget-set! rrate:settings:cont (glgui-box rrate:settings:cont 10 (- h 150) 300 145 (color:shuffle #xd7eaefff)) 'rounded #t)
  (glgui-label-wrapped rrate:settings:cont 20 (- h 155) 215 140 
    (local-get-text "CONSISTENCY_NUM_TAPS") text_20.fnt Black)
  (set! rrate:settings:tapslist (glgui-list rrate:settings:cont 218 (- h 145) 90 140 35 
    (map (lambda (n) (lambda (g wgt bx by bw bh selected?)
      (glgui:draw-pixmap-center (+ bx 5) by 30 29 (if selected? checkedcircle.img uncheckedcircle.img) White)
      (glgui:draw-text-left (+ bx 42) (+ by 2) 40 23 (local-get-text n) text_20.fnt Black)
    )) rrate:settings:tapchoices)
    (lambda (g wgt type mx my)
      ;; Save the new settings
      (let* ((tindex (glgui-widget-get rrate:settings:cont rrate:settings:tapslist 'current))
             (tstr (list-ref rrate:settings:tapchoices (max tindex 0)))
             (tvalue (string->number tstr)))
        (settings-set! "Taps" tvalue)
      )
    )
  ))
  (glgui-widget-set! rrate:settings:cont rrate:settings:tapslist 'autohidebar #t)
  (glgui-widget-set! rrate:settings:cont rrate:settings:tapslist 'bgcol1 (color-fade White 0))
  (glgui-widget-set! rrate:settings:cont rrate:settings:tapslist 'bgcol2 (color-fade White 0))
  (glgui-widget-set! rrate:settings:cont rrate:settings:tapslist 'current 2)
  
  ;; Setting for consistency percent threshold
  (set! rrate:settings:consistency (glgui-container rrate:settings:cont 9 (if rrate:settings:show_vibrate 85 50) 302 187))
  (glgui-widget-set! rrate:settings:consistency (glgui-box rrate:settings:consistency 1 1 300 185 (color:shuffle #xd7eaefff)) 'rounded #t)
  (glgui-label-wrapped rrate:settings:consistency 11 80 215 100 
    (local-get-text "CONSISTENCY_THRESH") text_20.fnt Black)
  (glgui-pixmap rrate:settings:consistency 16 4 diagram.img)
  (set! rrate:settings:percentlist (glgui-list rrate:settings:consistency 209 10 100 175 35 
    (map (lambda (p) (lambda (g wgt bx by bw bh selected?)
      (glgui:draw-pixmap-center (+ bx 5) by 30 29 (if selected? checkedcircle.img uncheckedcircle.img) White)
      (glgui:draw-text-left (+ bx 42) (+ by 2) 50 23 (string-append (local-get-text p) "%") text_20.fnt Black)
    )) rrate:settings:percentchoices)
    ;; Save the setting
    (lambda (g wgt type mx my)
      (let* ((pindex (glgui-widget-get rrate:settings:consistency rrate:settings:percentlist 'current))
             (pstr (list-ref rrate:settings:percentchoices (max pindex 0)))
             (pvalue (string->number pstr)))
        (settings-set! "Consistency" pvalue)
      )
    )
  ))
  (glgui-widget-set! rrate:settings:consistency rrate:settings:percentlist 'autohidebar #t)
  (glgui-widget-set! rrate:settings:consistency rrate:settings:percentlist 'bgcol1 (color-fade White 0))
  (glgui-widget-set! rrate:settings:consistency rrate:settings:percentlist 'bgcol2 (color-fade White 0))
  (glgui-widget-set! rrate:settings:consistency rrate:settings:percentlist 'current 4)
  
  (if rrate:settings:show_vibrate
    ;; Show checkbox for turning on and off vibration with sound (only Android)
    (begin
      (glgui-widget-set! rrate:settings:cont (glgui-box rrate:settings:cont 10 48 300 30 (color:shuffle #xd7eaefff)) 'rounded #t)
      (glgui-label rrate:settings:cont 42 50 205 25 (local-get-text "VIBRATE_SOUND") text_20.fnt Black)
      (set! rrate:settings:vibrate_box (glgui-pixmap rrate:settings:cont 258 48 checkedbox.img))
      (set! rrate:settings:vibrate_trigger (glgui-box rrate:settings:cont 42 48 268 30 (color-fade White 0)))
      (glgui-widget-set! rrate:settings:cont rrate:settings:vibrate_trigger 'callback
          (lambda (g . x)
             (if (settings-ref "VibrateSound")
               (begin
                 (settings-set! "VibrateSound" #f)
                 (glgui-widget-set! rrate:settings:cont rrate:settings:vibrate_box 'image uncheckedbox.img))
               (begin
                 (settings-set! "VibrateSound" #t)
                 (glgui-widget-set! rrate:settings:cont rrate:settings:vibrate_box 'image checkedbox.img)))))))
  
  ;; Back to the rest of the app
  (set! rrate:settings:donebutton (glgui-button-string rrate:settings:cont 218 6 95 32 (local-get-text "DONE") text_20.fnt
    (lambda (g . x)
      ;; Leave the settings page
      (set! rrate:settings:viewing #f)
    )
  ))
  (glgui-widget-set! rrate:settings:cont rrate:settings:donebutton 'button-normal-color Green)
  (glgui-widget-set! rrate:settings:cont rrate:settings:donebutton 'button-selected-color DarkGreen)
)
   
(define rrate:gui #f)
(define rrate:cont #f)
(define rrate:tapbg #f)
(define rrate:cancelbutton #f)
(define rrate:settingsbutton #f)
(define rrate:nobutton #f)
(define rrate:restartbutton #f)
(define rrate:confirm #f)
(define rrate:yesbutton #f)
(define rrate:exitbutton #f)
(define rrate:value #f)
(define rrate:toplayer #f)
(define rrate:toplayer_rrate #f)
(define rrate:toplayer_unit #f)
(define rrate:extratop #f)
(define rrate:tapicons #f)
(define rrate:qualitybg #f)
(define rrate:qualitybg_high #f)
(define rrate:qualitybg_consistent #f)
(define rrate:qualitybg_low #f)
(define rrate:rarm #f)
(define rrate:body #f)
(define rrate:dbody #f)
(define rrate:larm #f)
(define rrate:mouth #f)
(define rrate:trigger #f)
(define rrate:tapmessage #f)

;; The popup with error messages
(define rrate:popup:cont #f)
(define rrate:popup:inconsistent #f)
(define rrate:popup:bg #f)
(define rrate:popup:inconsistent #f)
(define rrate:popup:notenough #f)
(define rrate:popup:toofast #f)
(define rrate:popup:retrybutton #f)
(define rrate:popup:ignorebutton #f)

;; The procedures for what to do when done with the module
(define rrate:cancelproc #f)
(define rrate:doneproc #f)

;; The store to save the values to
(define rrate:store #f)

;; Defines position of baby body parts
(define rrate:position:larmx 70)
(define rrate:position:larmy 77)
(define rrate:position:rarmx 70)
(define rrate:position:rarmy 74)
(define rrate:position:mouthx 149)
(define rrate:position:mouthy 264)
(define rrate:position:bodyx 70)
(define rrate:position:bodyy 74)

(define rrate:times '())
(define rrate:rate 0.)
(define rrate:animateoffset 0.0)

;; Start of 1 minute timer
(define rrate:starttime #f)

;; Calculations
(define rrate:calc:medinterval #f)
(define rrate:calc:yscale #f)

;; Flag for whether the audio has been started, it is turned off if an alert is needed and the volume is 0
(define rrate:sound_on #t)

;; Keep track of when the mouth is small to begin breathing sound
(define rrate:readysound #f)

;; The time at which we start skipping the breathing for one second
(define rrate:skipbreath #f)

;; Sound to play on tapping
(define rrate:sound:breath #f)

;; Light blue colour of the side around the baby and colour of the text that displays the RR
(define rrate:sidecolor (color:shuffle #xd8eaefff))
(define rrate:textcolor (color:shuffle #x293790ff))

;; Quality marks showing timing of taps
(define rrate:qualitydots #f)

(define (rate->afreq r)
  (let ((pi (fl* 4. (flatan 1.))))
    (fl/ (fl* r 2. pi) 60.)))

;; Sets the offset of the animation so that it starts on inhalation right now
(define (rrate:set-animate-offset)
  (let* ((freq (rate->afreq rrate:rate))
         (now (time->seconds (current-time)))
         (tx (* freq now))
         ;; Estimate 3pi/2
         (pi32 (fl* 6. (flatan 1.)))
         ;; Determine offset to force tx to be 3pi/2
         (txoffset (- tx pi32)))
    (set! rrate:animateoffset (/ txoffset freq)))) 

;; Animate the right arm, move up and down
(define (rrate:animate-right-arm gui wgt)
  (let* ((now (time->seconds (current-time)))
         (tmp (flsin (* (rate->afreq rrate:rate) (- now rrate:animateoffset))))
         (tmp2 (/ (fl+ tmp 1.) 2.))
         (dy (fl* 7. tmp2)))
    (glgui-widget-set! gui wgt 'y (+ rrate:position:rarmy dy))
  ))

;; Animate the body, move left and right
(define (rrate:animate-body gui wgt)
  (let* ((now (time->seconds (current-time)))
         (tmp (flsin (* (rate->afreq rrate:rate) (- now rrate:animateoffset))))
         (tmp2 (/ (fl+ tmp 1.) 2.))
         (dx (fl* 7. tmp2)))
    (glgui-widget-set! gui wgt 'x (- rrate:position:bodyx dx))
  ))

;; Animate the left arm, move up and down
(define (rrate:animate-left-arm gui wgt)
  (let* ((now (time->seconds (current-time)))
         (tmp (flsin (* (rate->afreq rrate:rate) (- now rrate:animateoffset))))
         (tmp2 (/ (fl+ tmp 1.) 2.))
         (dx (fl* 2. tmp2))
         (dy (fl* 7. tmp2)))
    (glgui-widget-set! gui wgt 'x (- rrate:position:larmx dx))
    (glgui-widget-set! gui wgt 'y (+ rrate:position:larmy dy))
  ))

;; Animate the mouth, increase its height, while maintaining the same center coordinates
(define (rrate:animate-mouth gui wgt)
  (let* ((now (time->seconds (current-time)))
         (tmp (flsin (* (rate->afreq rrate:rate) (- now rrate:animateoffset))))
         (tmp2 (/ (fl+ tmp 1.) 2.))
         (dx (fl* 2. tmp2))
         (dy (fl* 1. tmp2))
         (neww (+ 25. (* dx 2.))))
    (glgui-widget-set! gui wgt 'w neww)
    (glgui-widget-set! gui wgt 'x (- rrate:position:mouthx dx))
    (glgui-widget-set! gui wgt 'h (+ 16. (* dy 2.)))
    (glgui-widget-set! gui wgt 'y (- rrate:position:mouthy dy))
    
    (if (< neww 25.5)
      ;; If mouth small, get ready for breathing sound
      (set! rrate:readysound #t)
      ;; If mouth now no longer small, perform breathing sound once if not skipping it
      (if rrate:readysound
        (begin
          (set! rrate:readysound #f)
          (if rrate:skipbreath
            ;; If no skip breath time has passed, turn to false and play breath or vibration
            (if (> (- ##now rrate:skipbreath) 1.)
              (begin
                (set! rrate:skipbreath #f)
                (rrate:breath-feedback)))
            ;; If no skipping of breath, play it or vibrate
            (rrate:breath-feedback)))))
  ))

(define (rrate:tapcb g wgt . x)
  (let* ((now (time->seconds (current-time)))
         (count (length rrate:times))
         (taplimit (settings-ref "Taps" 4))
         (playbreath #t))
    (set! rrate:times (append rrate:times (list now)))

    ;; If this is the first tap, set start time
    (if (fx= count 0)
      (set! rrate:starttime (car rrate:times)))
    
    ;; Change the colour of the icon for the most recent tap
    (glgui-widget-set! rrate:cont (list-ref rrate:tapicons count) 'image dot_dark.img)
    (set! count (+ count 1))
    
    ;; After first tap show cancel button
    (glgui-widget-set! rrate:cont rrate:cancelbutton 'hidden #f)
    
    (if (fx>= count taplimit)
      ;; Get rate using taps from the <taplimit> most recent intervals and then make sure each is close enough to their median
      (let* ((tapintervals
               ;; Get a list of the intervals between the last <taplimit> taps
               (let tloop ((ts (reverse rrate:times)) (intervals (list)))
                  ;; If still look back at most recent <taplimit> taps
                  (if (fx> (length ts) (+ (- count taplimit) 1))
                    (tloop (cdr ts) (append intervals (list (- (car ts) (cadr ts)))))
                    intervals)))
             ;; Determine median interval
             (medinterval (median tapintervals))
             ;; Get current consistency threshold percentage and determine percent of median
             (consistency (/ (settings-ref "Consistency" 12) 100))
             (percthreshold (* medinterval consistency))
             (yscale (/ 9.5 percthreshold)))
         
         ;; Make sure that the difference between each interval and the median interval is not greater than the consistency threshold
         (if (not (member #f (map (lambda (interval) (<= (abs (- interval medinterval)) percthreshold)) tapintervals)))
           (let* ((medrate (/ 60. medinterval))
                  (valstr (if (> medinterval 0.) (number->string (fix (round medrate))) #f)))
              
              ;; Don't play a breathing sound from this tap, and delay the breathing sounds during animation
              (set! playbreath #f)
              (set! rrate:skipbreath ##now)
              
              ;; No longer tapping
              (set! rrate:starttime #f)
             
              ;; Remember properties of respiratory rate calculated
              (set! rrate:calc:medinterval medinterval)
              (set! rrate:calc:yscale yscale)
             
              ;; Show the quality feedback as a artificial horizon
              (rrate:show-quality)
             
              ;; Set rate to be this median rate
              (glgui-widget-set! rrate:cont rrate:value 'label (local-get-text valstr))
              (glgui-widget-set! rrate:cont rrate:value 'x (if (fx= (string-length valstr) 3) 0 9))
              (set! rrate:rate medrate)

              ;; Set animate offset
              (rrate:set-animate-offset)
             
              ;; Display message depending on if the rate is too fast
              (if (>= medrate 140)
                (begin
                  (rrate:fail-feedback)
                  (glgui-widget-set! rrate:cont rrate:value 'color Grey)
                  (rrate:show-popup rrate:popup:toofast (< medrate 200)))
                (begin
                  (rrate:success-feedback)
                  (set! successtap #t)
                  (rrate:go-to-stage 2)
                  (glgui-widget-set! rrate:cont rrate:confirm 'hidden #f)
                  (glgui-widget-set! rrate:cont rrate:nobutton 'hidden #f)
                  (glgui-widget-set! rrate:cont rrate:yesbutton 'hidden #f))))
             ;; Otherwise still not consistent, if this is the 12th tap go to confirm screen anyway
             (if (fx> count 11)
              (begin

               ;; Just use median of all intervals to calculate rate
               (let loop ((ts rrate:times) (diffs '()))
                 (if (fx> (length ts) 1)
                   (loop (cdr ts) (append diffs (list (- (cadr ts) (car ts)))))
                   ;; At the end of the loop, compute median interval and determine scale from this,
                   ;; display the rate and animate
                   (begin
                     (set! rrate:calc:medinterval (median diffs))
                     (set! rrate:calc:yscale (/ 9.5 (* rrate:calc:medinterval consistency)))
                     (set! rrate:rate (/ 60. rrate:calc:medinterval))
                     (let ((valstr (number->string (fix (round rrate:rate)))))
                       (glgui-widget-set! rrate:cont rrate:value 'label (local-get-text valstr))
                       (glgui-widget-set! rrate:cont rrate:value 'x (if (fx= (string-length valstr) 3) 0 9)))
                     
                     ;; Show message about error with taps and change number to red
                     (glgui-widget-set! rrate:cont rrate:value 'color Grey)
                     (rrate:show-popup (if (>= rrate:rate 140) rrate:popup:toofast rrate:popup:inconsistent) (< rrate:rate 200)))))

               ;; Don't play a breathing sound from this tap, and delay the breathing sounds during animation
               (set! playbreath #f)
               (set! rrate:skipbreath ##now)
               (rrate:fail-feedback)

               ;; Set animate offset
               (rrate:set-animate-offset)
               (set! rrate:starttime #f)
               
               (rrate:show-quality))))))
    
    ;; Only play breath sound (or vibration) from tap if not going to animation (which itself will play breath sound)
    (if playbreath
      ;; Play breath sound or vibration
      (rrate:breath-feedback)))
)
  
;; Shows the quality feedback display which is an artificial horizon
;; with one dot per tap spaced equally horizontally but placed vertically
;; based on speed with higher dots being too fast and low dots being taps
;; that were too slow.
(define (rrate:show-quality)
 
  ;; Show quality lines
  (for-each (lambda (w) (glgui-widget-set! rrate:cont w 'hidden #f)) 
    (list rrate:qualitybg rrate:qualitybg_high rrate:qualitybg_consistent rrate:qualitybg_low))
  
  ;; Put first dot on center line
  (glgui-widget-set! rrate:cont (car rrate:qualitydots) 'y 73)
  
  ;; Scale y so that 20 percent to be the height of the green bar (19 pixels)
  (let dotloop ((qd (cdr rrate:qualitydots)) (ts rrate:times))
    (if (fx> (length ts) 1)
      (let* ((interval (- (cadr ts) (car ts))))
        (glgui-widget-set! rrate:cont (car qd) 'y (max (min (+ (* -1 (- interval rrate:calc:medinterval) rrate:calc:yscale) 73) 100) 46))
        (dotloop (cdr qd) (cdr ts)))))
)
  
;; Goes to the second stage which displays the RR and animation
;; or goes back to the first stage which displays the tap icons.
;; stage = 1 is first stage and stage = 2 is second stage
(define (rrate:go-to-stage stage)

  (let ((stage2 (fx= stage 2)))
    (glgui-widget-set! rrate:cont rrate:cancelbutton 'hidden (or stage2 (not rrate:cancelproc)))
    (glgui-widget-set! rrate:cont rrate:settingsbutton 'hidden stage2)
    (glgui-widget-set! rrate:cont rrate:nobutton 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:confirm 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:yesbutton 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:rarm 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:body 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:dbody 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:larm 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:mouth 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:trigger 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:tapbutton 'hidden stage2)
    (glgui-widget-set! rrate:cont rrate:tapmessage 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:qualitybg 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:qualitybg_high 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:qualitybg_consistent 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:qualitybg_low 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:value 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:toplayer 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:toplayer_rrate 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:toplayer_unit 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:extratop 'hidden (not stage2))
    (glgui-widget-set! rrate:cont rrate:tapbg 'image (if stage2 stage2_bg.img stage1_bg.img))

    ;; Reset skipping breath sound if going back to stage 1
    (if (not stage2)
      (set! rrate:skipbreath #f))
    
    ;; Hide or show the tap icons
    (let loop ((ws rrate:tapicons))
       (if (> (length ws) 0) 
         (begin
            (glgui-widget-set! rrate:cont (car ws) 'hidden stage2)
            (loop (cdr ws)))))
    
    ;; If going to the first stage, reset the taps
    (if (not stage2)
      (rrate:reset-taps)))
)
    
;; Displays the popup with the given message, which is rrate:popup:inconsistent, rrate:popup:toofast or rrate:popup:notenough
;; If ignore? is true, the ignore button is shown as an option
(define (rrate:show-popup message ignore?)
 
  ;; Enter modal mode, which show the popup background and buttons 
  (glgui-modal-set! #t)
  
  ;; Show the message
  (glgui-widget-set! rrate:popup:cont message 'hidden #f)
  
  ;; Remember what the last error was
  (set! lasterror (cond
                     ((eq? message rrate:popup:toofast)
                        "Too fast")
                     ((eq? message rrate:popup:inconsistent)
                        "Inconsistent")
                     (else
                        "Not enough taps")))
        
   ;; If the ignore option is used, show this button, otherwise hide it and center the retry button
   (if ignore?
     (begin
        (glgui-widget-set! rrate:popup:cont rrate:popup:ignorebutton 'hidden #f)
        (glgui-widget-set! rrate:popup:cont rrate:popup:retrybutton 'x 43))
     (begin
        (glgui-widget-set! rrate:popup:cont rrate:popup:ignorebutton 'hidden #t)
        (glgui-widget-set! rrate:popup:cont rrate:popup:retrybutton 'x 106)))
  
  ;; Hide the cancel button
  (glgui-widget-set! rrate:cont rrate:cancelbutton 'hidden #t)
)

;; Hides the popup
(define (rrate:hide-popup)
 
  ;; Exits modal mode, which hides the popup background
  (glgui-modal-set! #f)
  
  ;; Hide all the messages
  (glgui-widget-set! rrate:popup:cont rrate:popup:inconsistent 'hidden #t)
  (glgui-widget-set! rrate:popup:cont rrate:popup:toofast 'hidden #t)
  (glgui-widget-set! rrate:popup:cont rrate:popup:notenough 'hidden #t)
)

;; Give feedback that a breath has been tapped or during the animation
;; give feedback of the rate by regularly playing a sound or vibrating
(define (rrate:breath-feedback)
  (if (= (audiofile-getvolume) 0.)
    (begin
      (if rrate:sound_on
        (begin
          (set! rrate:sound_on #f)
          (audiofile-stop)))
      (vibrate))
    (begin
      (if (not rrate:sound_on)
        (begin
           (set! rrate:sound_on #t)
           (audiofile-start)))
      (audiofile-play rrate:sound:breath)
      ;; Also vibrate if this option is checked
      (if (settings-ref "VibrateSound")
        (vibrate))))
)

;; Give feedback that the RRate has been successfully measured by playing a sound or lack of vibrating
(define (rrate:success-feedback)
  (if (> (audiofile-getvolume) 0.)
    (begin
      (if (not rrate:sound_on)
        (begin
           (set! rrate:sound_on #t)
           (audiofile-start)))
      (audiofile-play chimes)))
)

;; Give feedback that the RRate has not been successfully measured by playing a sound or lack of vibrating
(define (rrate:fail-feedback)
  (if (> (audiofile-getvolume) 0.)
    (begin
      (if (not rrate:sound_on)
        (begin
           (set! rrate:sound_on #t)
           (audiofile-start)))
      (audiofile-play beep)))
)

;; Resets the taps by clearing the tap icons and the data quality
;; and hiding the data quality and cancel button
(define (rrate:reset-taps)
  
  ;; Clear tap data
  (set! rrate:times '())
  
  (let loop ((taps rrate:tapicons) (qd rrate:qualitydots))
     (if (fx> (length taps) 0)
       (begin
         (glgui-widget-set! rrate:cont (car taps) 'image dot_light.img)
         (if (fx> (length qd) 0)
           (glgui-widget-set! rrate:cont  (car qd) 'y -20))
         (loop (cdr taps) (if (fx> (length qd) 0) (cdr qd) qd)))))
)

;; Resets the module, going back to the tap page and clearing all the taps
(define (rrate-reset)
  
  ;; Reset trend text colour
  (glgui-widget-set! rrate:cont rrate:value 'color rrate:textcolor)

  ;; Clear interval and scale values
  (set! rrate:calc:medinterval #f)
  (set! rrate:calc:yscale #f)

  ;; Hide these buttons
  (glgui-widget-set! rrate:cont rrate:restartbutton 'hidden #t)
  (glgui-widget-set! rrate:cont rrate:exitbutton 'hidden #t)

  ;; Go back to stage 1 to try again
  (rrate:go-to-stage 1)
) 

;M @deffn {procedure} rrate-init x y w h store cancelproc doneproc
;M Initializes the Respiratory Rate module graphics.
;M Display the rrate:cont to see the module graphics.
;M Store is the store where the RR will be saved.
;M Cancel procedure is a procedure that gets called when the user cancels
;M when they haven't done any tapping yet. Done procedure is a procedure
;M that is called once the user confirms the rate. These procedures are
;M all optional and are called with no arguments. Currently only the height 
;M is adapted to (best at 433, keep >= 402), keep x, y, or w as 0, 0, and 320.
;M @end deffn
(define (rrate-init x y w h store cancelproc doneproc)
   (set! rrate:store store)
   (set! rrate:cancelproc cancelproc)
   (set! rrate:doneproc doneproc)
  
   (set! rrate:gui (make-glgui))
   (set! rrate:cont (glgui-container rrate:gui x y w h))

   ;; Initialize the settings
   (settings-init (list (cons "Taps" 5)
                        (cons "Consistency" 13)
                        (cons "VibrateSound" #f)))
     
   ;; Initialize the settings page and set the settings
   (rrate:setting-init 0 0 w h)
   (let* ((taps (settings-ref "Taps" 5))
          (tindex (list-pos rrate:settings:tapchoices (number->string taps)))
          (consistency (settings-ref "Consistency" 13))
          (pindex (list-pos rrate:settings:percentchoices (number->string consistency))))
     (glgui-widget-set! rrate:settings:cont rrate:settings:tapslist 'current (if tindex tindex 0))
     (glgui-widget-set! rrate:settings:consistency rrate:settings:percentlist 'current (if pindex pindex 0))
     (if rrate:settings:show_vibrate
       (glgui-widget-set! rrate:settings:cont rrate:settings:vibrate_box 'image (if (settings-ref "VibrateSound") checkedbox.img uncheckedbox.img))))
    
   ;; initialize audio - must come before audiofile-load! 
   (audiofile-init)

   ;; Load sound effect
   (set! rrate:sound:breath (audiofile-load "breath"))
   (set! beep (audiofile-load "beep"))
   (set! chimes (audiofile-load "chimes"))
    
   (set! rrate:tapbg (glgui-pixmap rrate:cont 0 43 stage1_bg.img))
 
   (set! rrate:cancelbutton (glgui-button-string rrate:cont 12 6 119 32 (local-get-text "CANCEL") text_20.fnt
     (lambda (g . x)
       ;; Determine if there are any taps, if none then cancel out of module, otherwise just reset
       (if (fx= (length rrate:times) 0)
         (if rrate:cancelproc (rrate:cancelproc))
         (begin
           ;; Clear interval and scale values
           (set! rrate:calc:medinterval #f)
           (set! rrate:calc:yscale #f)
           (rrate:go-to-stage 1)
         )
       )
     )
   ))
   (glgui-widget-set! rrate:cont rrate:cancelbutton 'button-normal-color Gold)
   (glgui-widget-set! rrate:cont rrate:cancelbutton 'button-selected-color Goldenrod)
   (glgui-widget-set! rrate:cont rrate:cancelbutton 'hidden (not rrate:cancelproc))
     
   ;; Go to the settings page after cancelling the current tapping
   (set! rrate:settingsbutton (glgui-button-string rrate:cont 157 6 156 32 (local-get-text "SETTINGS") text_20.fnt
     (lambda (g . x)
       ;; Clear interval and scale values
       (set! rrate:calc:medinterval #f)
       (set! rrate:calc:yscale #f)
       (rrate:go-to-stage 1)
       (set! rrate:settings:viewing #t)
     )
   ))
   (glgui-widget-set! rrate:cont rrate:settingsbutton 'button-normal-color LightGray)
   (glgui-widget-set! rrate:cont rrate:settingsbutton 'button-selected-color Gray)
     
   ;; Remove the confirm question and show the other buttons instead
   (set! rrate:nobutton (glgui-button-string rrate:cont 6 6 55 32 (local-get-text "NO") text_20.fnt
     (lambda (g . x)
       ;; Reset trend text colour
       (glgui-widget-set! rrate:cont rrate:value 'color rrate:textcolor)
       ;; Clear interval and scale values
       (set! rrate:calc:medinterval #f)
       (set! rrate:calc:yscale #f)
       ;; Hide these buttons
       (glgui-widget-set! rrate:cont rrate:restartbutton 'hidden #t)
       (glgui-widget-set! rrate:cont rrate:exitbutton 'hidden #t)
       ;; Go back to stage 1 to try again
       (rrate:go-to-stage 1)
     )
   ))
   (glgui-widget-set! rrate:cont rrate:nobutton 'button-normal-color Orange)
   (glgui-widget-set! rrate:cont rrate:nobutton 'button-selected-color DarkOrange)
   (glgui-widget-set! rrate:cont rrate:nobutton 'hidden #t)
     
   (set! rrate:restartbutton (glgui-button-string rrate:cont 6 6 120 32 (local-get-text "RESTART") text_20.fnt
     (lambda (g . x)
       (rrate-reset)
     )
   ))
   (glgui-widget-set! rrate:cont rrate:restartbutton 'button-normal-color Green)
   (glgui-widget-set! rrate:cont rrate:restartbutton 'button-selected-color DarkGreen)
   (glgui-widget-set! rrate:cont rrate:restartbutton 'hidden #t)
     
   ;; Confirmation question about animation
   (set! rrate:confirm (glgui-label-wrapped rrate:cont 65 2 (- (glgui-width-get) 65 65) 36
     (local-get-text "RR_MATCH") text_14.fnt White))
   (glgui-widget-set! rrate:cont rrate:confirm 'hidden #t)
     
   ;; Remove the confirm question and show the other buttons instead, run done procedure
   (set! rrate:yesbutton (glgui-button-string rrate:cont (- (glgui-width-get) 55 6) 6 55 32 (local-get-text "YES") text_20.fnt
     (lambda (g . x)
       ;; Save the RR to the store
       (if rrate:store (store-set! rrate:store "RR" (glgui-widget-get rrate:cont rrate:value 'label)))
       (glgui-widget-set! rrate:cont rrate:confirm 'hidden #t)
       (glgui-widget-set! rrate:cont rrate:nobutton 'hidden #t)
       (glgui-widget-set! rrate:cont rrate:yesbutton 'hidden #t)
       (glgui-widget-set! rrate:cont rrate:restartbutton 'hidden #f)
       (if rrate:cancelproc (glgui-widget-set! rrate:cont rrate:exitbutton 'hidden #f))
        (if rrate:doneproc (rrate:doneproc))
     )
   ))
   (glgui-widget-set! rrate:cont rrate:yesbutton 'button-normal-color Green)
   (glgui-widget-set! rrate:cont rrate:yesbutton 'button-selected-color DarkGreen)
   (glgui-widget-set! rrate:cont rrate:yesbutton 'hidden #t)
     
   (set! rrate:exitbutton (glgui-button-string rrate:cont (- (glgui-width-get) 120 6) 6 120 32 (local-get-text "EXIT") text_20.fnt
     (lambda (g . x)
       ;; Prepare for next time
       (rrate-reset)   
       (if rrate:cancelproc 
         (rrate:cancelproc)
         (glgui-widget-set! rrate:cont rrate:exitbutton 'hidden #t)
       )
     )
   ))
   (glgui-widget-set! rrate:cont rrate:exitbutton 'button-normal-color Green)
   (glgui-widget-set! rrate:cont rrate:exitbutton 'button-selected-color DarkGreen)
   (glgui-widget-set! rrate:cont rrate:exitbutton 'hidden #t)

   ;; Animated parts of the baby with ghosted parts on top
   (set! rrate:rarm (glgui-sprite rrate:cont 'x rrate:position:rarmx  'y rrate:position:rarmy  'image right_arm.img 'rendercallback rrate:animate-right-arm))
   (glgui-widget-set! rrate:cont rrate:rarm 'hidden #t)
   (set! rrate:body (glgui-sprite rrate:cont 'x rrate:position:bodyx  'y rrate:position:bodyy  'image body.img 'rendercallback rrate:animate-body))
   (glgui-widget-set! rrate:cont rrate:body 'hidden #t)
   (set! rrate:dbody (glgui-pixmap rrate:cont rrate:position:bodyx (- rrate:position:bodyy 1) dotted_body.img))
   (glgui-widget-set! rrate:cont rrate:dbody 'hidden #t)
   (set! rrate:larm (glgui-sprite rrate:cont 'x rrate:position:larmx 'y rrate:position:larmy 'image left_arm.img 'rendercallback rrate:animate-left-arm))
   (glgui-widget-set! rrate:cont rrate:larm 'hidden #t)

   ;; Dots that show how many taps have been done so far
   (let ((tw (/ (- w 20 (car dot_light.img)) 11))
         (ty 73))
     (let dloop ((tx 10) (icons '()))
       (if (fx< (length icons) 12)
         (dloop (+ tx tw) (append icons (list (glgui-pixmap rrate:cont tx ty dot_light.img))))
         (set! rrate:tapicons icons))))

   ;; Circle, head and bubble
   (set! rrate:toplayer (glgui-pixmap rrate:cont x (+ y 43) top_layer.img))
   (set! rrate:toplayer_rrate (glgui-label-wrapped rrate:cont (+ x 5) (+ y 360) 100 40 (local-get-text "RRATE") text_14.fnt DarkBlue)) 
   (set! rrate:toplayer_unit (glgui-label rrate:cont (+ x 5) (+ y 295) 100 20 (local-get-text "RRATE_UNIT") text_14.fnt DarkBlue)) 
   (for-each (lambda (w) (glgui-widget-set! rrate:cont w 'hidden #t))
     (list rrate:toplayer rrate:toplayer_rrate rrate:toplayer_unit))
  
   ;; Extra space above the top layer
   (let ((ey (+ y 43 (cadr top_layer.img))))
     (set! rrate:extratop (glgui-box rrate:cont x ey w (- h ey) rrate:sidecolor))
     (glgui-widget-set! rrate:cont rrate:extratop 'hidden #t))

   ;; Animated mouth
   (set! rrate:mouth (glgui-sprite rrate:cont 'x 149 'y 254 'image mouth.img 'rendercallback rrate:animate-mouth))
   (glgui-widget-set! rrate:cont rrate:mouth 'hidden #t)

   (set! rrate:value (glgui-label rrate:cont 9 310 150 55 "" numbers_56.fnt rrate:textcolor))
   (glgui-widget-set! rrate:cont rrate:value 'hidden #t)

   ;; Message about synchronizing the animation
   (set! rrate:tapmessage (glgui-label-wrapped rrate:cont 5 105 75 40
     (local-get-text "TAP_TO_SYNC") text_11.fnt White))
   (glgui-widget-set! rrate:cont rrate:tapmessage 'color Black)
   (glgui-widget-set! rrate:cont rrate:tapmessage 'hidden #t)

   ;; Make quality lines
   (set! rrate:qualitybg (glgui-pixmap rrate:cont 0 53 quality_lines.img))
   (glgui-widget-set! rrate:cont rrate:qualitybg 'hidden #t)
   (let ((x (- (glgui-width-get) 78)) (w 75) (h 20))
     (set! rrate:qualitybg_high (glgui-label rrate:cont x 86 w h (local-get-text "FAST") text_14.fnt Black))
     (set! rrate:qualitybg_consistent (glgui-label rrate:cont x 68 w h (local-get-text "CONSISTENT") text_14.fnt Black))
     (set! rrate:qualitybg_low (glgui-label rrate:cont x 49 w h (local-get-text "SLOW") text_14.fnt Black))
   )
   (for-each (lambda (w) (glgui-widget-set! rrate:cont w 'align GUI_ALIGNRIGHT)
     (glgui-widget-set! rrate:cont w 'hidden #t)) 
     (list rrate:qualitybg_high rrate:qualitybg_consistent rrate:qualitybg_low))
 
   ;; Make 12 quality dot icons for the taps
   (let ((tw (/ (- w 20 (car dot_light.img)) 11))
         (ty -20))
     (let dloop ((tx 10) (icons '()))
       (if (fx< (length icons) 12)
         (dloop (+ tx tw) (append icons (list (glgui-pixmap rrate:cont tx ty dot_dark.img))))
         (set! rrate:qualitydots icons))))
        
   ;; Make invisible button for syncing animation
   (set! rrate:trigger (glgui-box rrate:cont 0 60 w (- h 60) (color-fade White 0)))
   (glgui-widget-set! rrate:cont rrate:trigger 'callback 
      (lambda (g wgt . x)
        ;; Make animation start at inhalation again
        (rrate:set-animate-offset)))
   (glgui-widget-set! rrate:cont rrate:trigger 'hidden #t)
     
   ;; Tap button
   (set! rrate:tapbutton (glgui-button-string rrate:cont 3 165 (- w 6) (- h 165) (local-get-text "TAP_INHALATION") text_40.fnt
     (lambda (g wgt . x)
       (rrate:tapcb g wgt)
     )
   ))
   (glgui-widget-set! rrate:cont rrate:tapbutton 'multiline #t)
   (glgui-widget-set! rrate:cont rrate:tapbutton 'button-normal-color (color-rgb 213 233 238))
   (glgui-widget-set! rrate:cont rrate:tapbutton 'button-selected-color (color-rgb 42 54 146))
     
   ;; Create popup background and message
   (set! rrate:popup:cont (glgui-container rrate:gui 0 0 w h))
   (glgui-widget-set! rrate:gui rrate:popup:cont 'modal #t)
   (set! rrate:popup:bg (glgui-box rrate:popup:cont 30 113 260 105 (color-fade Black 0.8)))
   (glgui-widget-set! rrate:popup:cont rrate:popup:bg 'rounded #t)
   (glgui-widget-set! rrate:popup:cont rrate:popup:bg 'modal #t)

   ;; Error messages about respiratory rate
   (set! rrate:popup:inconsistent (glgui-label-wrapped rrate:popup:cont 40 166 240 50
     (local-get-text "TAPS_INCONSISTENT") text_20.fnt White))
   (glgui-widget-set! rrate:popup:cont rrate:popup:inconsistent 'hidden #t)
   (glgui-widget-set! rrate:popup:cont rrate:popup:inconsistent 'modal #t)
   (set! rrate:popup:notenough (glgui-label-wrapped rrate:popup:cont 40 166 240 50
      (local-get-text "NOT_ENOUGH_TAPS") text_20.fnt White))
   (glgui-widget-set! rrate:popup:cont rrate:popup:notenough 'hidden #t)
   (glgui-widget-set! rrate:popup:cont rrate:popup:notenough 'modal #t)
   (set! rrate:popup:toofast (glgui-label-wrapped rrate:popup:cont 40 166 240 50
     (local-get-text "TAPS_TOO_FAST") text_20.fnt White))
   (glgui-widget-set! rrate:popup:cont rrate:popup:toofast 'hidden #t)
   (glgui-widget-set! rrate:popup:cont rrate:popup:toofast 'modal #t)

   ;; Popup buttons

   ;; Make retry button with the same callback as the no button
   (set! rrate:popup:retrybutton (glgui-button-string rrate:popup:cont 43 125 109 32 (local-get-text "RETRY") text_20.fnt
     (lambda (g wgt . x) 
       (rrate:hide-popup)
       ;; Reset trend text colour
       (glgui-widget-set! rrate:cont rrate:value 'color rrate:textcolor)
       ;; Clear interval and scale values
       (set! rrate:calc:medinterval #f)
       (set! rrate:calc:yscale #f)
       ;; Go back to stage 1 to try again
       (rrate:go-to-stage 1)
     )
   ))
   (glgui-widget-set! rrate:popup:cont rrate:popup:retrybutton 'button-normal-color Green)
   (glgui-widget-set! rrate:popup:cont rrate:popup:retrybutton 'button-selected-color DarkGreen)
   (glgui-widget-set! rrate:popup:cont rrate:popup:retrybutton 'modal #t)
     
   ;; Make Ignore button for rejecting retry, use same callback as Yes button for matching animation
   (set! rrate:popup:ignorebutton (glgui-button-string rrate:popup:cont 159 125 109 32 (local-get-text "IGNORE") text_20.fnt
     (lambda (g wgt . x)
       ;; Exit modal mode and go to animation
       (rrate:hide-popup)
       (rrate:go-to-stage 2)
     )
   ))
   (glgui-widget-set! rrate:popup:cont rrate:popup:ignorebutton 'button-normal-color Gray)
   (glgui-widget-set! rrate:popup:cont rrate:popup:ignorebutton 'button-selected-color DarkGray)
   (glgui-widget-set! rrate:popup:cont rrate:popup:ignorebutton 'modal #t)
)

;; Running procedure for the rrate module - should be called continuously when on the RRate page.
;; This checks if the page should time out, too long between taps.
(define (rrate-run t)
   
   ;; If timer active and a minute since starting, going to animation
   (if rrate:starttime
     (if (> (- (current-time-seconds) rrate:starttime) 60.0)
       (begin
         ;; If no median interval yet, then just use median of all taps
         (if (not rrate:calc:medinterval)
           (begin
             (if (fx> (length rrate:times) 1)
               (let loop ((ts rrate:times) (diffs '()))
                 (if (fx> (length ts) 1)
                   (loop (cdr ts) (append diffs (list (- (cadr ts) (car ts)))))
                   ;; At the end of the loop, compute median interval and determine scale from this
                   (let ((consistency (/ (settings-ref "Consistency" 12) 100))) 
                     (set! rrate:calc:medinterval (median diffs))
                     (set! rrate:calc:yscale (/ 9.5 (* rrate:calc:medinterval consistency))))))
               ;; If less than 2 taps, just set median to 60 seconds
               (begin
                 (set! rrate:calc:medinterval 60.)
                 (set! rrate:calc:yscale 32.)
               )
             )
             ;; Set rate to be the calculated median rate
             (let* ((medrate (/ 60. rrate:calc:medinterval))
                    (valstr (number->string (fix (round medrate)))))
               (glgui-widget-set! rrate:cont rrate:value 'label (local-get-text valstr))
               (glgui-widget-set! rrate:cont rrate:value 'x (if (fx= (string-length valstr) 3) 2 9))
               (set! rrate:rate medrate)
               ;; Show popup, and change RR colour to red for if we go to the animation page
               (glgui-widget-set! rrate:cont rrate:value 'color Grey)
               (rrate:show-popup (if (fx< (length rrate:times) 4) rrate:popup:notenough 
                 (if (>= medrate 140) rrate:popup:toofast rrate:popup:inconsistent)) (< medrate 200)
               )
             )
           ))
         ;; Show the quality feedback as a artificial horizon
         (rrate:show-quality)

         ;; Play the beep if the volume is turned on, otherwise if vibrating it will now have stopped
         (rrate:fail-feedback)

         ;; Delay the breathing sounds during animation
         (set! rrate:skipbreath ##now)

         ;; Set animate offset
         (rrate:set-animate-offset)
         (set! rrate:starttime #f)
         (glgui-modal-set! #t))))

  ;; Display either the main RRate page or the settings page
  (if (fx= t EVENT_REDRAW)
    (begin
      (glgui-widget-set! rrate:gui rrate:settings:cont 'hidden (not rrate:settings:viewing))
      (glgui-widget-set! rrate:gui rrate:cont 'hidden rrate:settings:viewing)))     
)


;; eof
