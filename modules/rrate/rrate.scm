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
;; Christian Leth Petersen 2012, Dustin Dunsmuir 2018, Matthias Görges 2015
(define rrate:no-settings? #f)
(define (rrate-use-settings use?) (set! rrate:no-settings? (not use?)))
(define rrate:no-language? #f)
(define (rrate-use-language-settings use?) (set! rrate:no-language? (not use?)))
(define rrate:muteheadset #f)
(define (rrate-set-mute-headset mute?) (set! rrate:muteheadset mute?))
(define (rrate-is-muted-headset) (and rrate:muteheadset (audioaux-headphonepresent)))
;; Tap for one minute instead of applying consistency check algorithm
(define rrate:oneminute #f)
(define (rrate-set-oneminute oneminute?) (set! rrate:oneminute oneminute?))
;; Turn on timestamp for button callback set to when button is pressed down
(define rrate:timeonbuttondown? #f)
(define (rrate-set-timeonbuttondown onbuttondown?) (set! rrate:timeonbuttondown? onbuttondown?))

;; Standard fonts, to switch back to if switching languages
(define rrate:stfnt_12.fnt text_12.fnt)
(define rrate:stfnt_14.fnt text_14.fnt)
(define rrate:stfnt_20.fnt text_20.fnt)
(define rrate:stfnt_40.fnt text_40.fnt)

;; Load the localization support
(define rrate:setup? #f)
(define (rrate-setup)
  (local-load "rrate-local.csv")
  (local-index-set! 1);; 1 English, 2 Lunganda, 3 Khmer, 4 Amharic, 5 Dinka
  (set! rrate:setup? #t)
  ;; Initialize the settings
  (settings-init (list (cons "Taps" 5)
                       (cons "Consistency" 13)
                       (cons "VibrateSound" #f)
                       (cons "HOST" "")
                       (cons "URL" "/redcap/api/")
                       (cons "TOKEN" "")
                       (cons "FORM" "")
                       (cons "EVENT" "")
                       (cons "REDCAP_USE?" #f)))
)

;; Settings page for configuring number of taps and consistency percent for threshold
(define rrate:settings:bg #f)
(define rrate:settings:language #f)
(define rrate:settings:languagelist #f)
(define rrate:settings:taps #f)
(define rrate:settings:tapslist #f)
(define rrate:settings:consistency #f)
(define rrate:settings:percentlist #f)
(define rrate:settings:redcap #f)
(define rrate:settings:redcap:boxcontainer #f)
(define rrate:settings:redcap:focusedbox #f)
(define rrate:settings:redcap:uploadbutton #f)
(define rrate:settings:keypad #f)
(define rrate:settings:backbutton #f)
(define rrate:settings:nextbutton #f)

;; Flag for whether on settings pages or not, index of settings page
(define rrate:settings:viewing #f)
(define rrate:settings:page 0)

;; Setting options to chose from
(define rrate:settings:tapchoices (list "3" "4" "5" "6"))
(define rrate:settings:consistency #f)
(define rrate:settings:percentchoices (list "10" "11" "12" "13" "14"))
(define rrate:settings:vibrate_trigger #f)
(define rrate:settings:vibrate_box #f)

;; List of language pairs of the form (<Language Index> <Language Name>) sorted alphabetically
(define rrate:settings:languagechoices #f)

;; Showing vibrate setting option
(define rrate:settings:show_vibrate #f)

(define (rrate:setting-init x y w h)
  (set! rrate:settings:bg (glgui-container rrate:gui x y w h))
  ;; Black background behind everything
  (glgui-box rrate:settings:bg 0 0 w h Black)
  (glgui-pixmap rrate:settings:bg 0 43 settings_bg.img w (- h 43))

  ;; Only show option for vibrating the phone with sound if on Android
  (set! rrate:settings:show_vibrate (string=? (system-platform) "android"))

  ;; If not using the languages settings page, skip it
  (if rrate:no-language? (set! rrate:settings:page 1))

  ;; Go back to previous settings page or out of settings completely
  (set! rrate:settings:backbutton (glgui-button rrate:settings:bg 12 6 100 32 left_arrow.img
    (lambda (g . x)
      (if rrate:settings:keypad (set-keypad-hidden #t))
      (if (or (fx= rrate:settings:page 0) (and rrate:no-language? (fx= rrate:settings:page 1)))
        (set! rrate:settings:viewing #f)
        (set! rrate:settings:page (- rrate:settings:page 1))))))
  (glgui-widget-set! rrate:settings:bg rrate:settings:backbutton 'button-normal-color Green)
  (glgui-widget-set! rrate:settings:bg rrate:settings:backbutton 'button-selected-color DarkGreen)

  ;; The first page of settings, the language
  (set! rrate:settings:language (glgui-container rrate:gui x y w h))
  (glgui-widget-set! rrate:settings:language (glgui-box rrate:settings:language 10 (- h (if rrate:settings:show_vibrate 285 380)) (- w 20) (if rrate:settings:show_vibrate 275 370) (color:shuffle #xd7eaefff)) 'rounded #t)
  (glgui-label rrate:settings:language 30 (- h 50) (- w 60) 23 "Select language" text_20.fnt Black)
  (rrate-setup-language-choices)
  (let ((oldoff (if rrate:settings:languagelist (fix (glgui-widget-get rrate:settings:language rrate:settings:languagelist 'offset)) 0)))
    (set! rrate:settings:languagelist (glgui-list rrate:settings:language 50 (- h (if rrate:settings:show_vibrate 285 380)) 165 (if rrate:settings:show_vibrate 230 325) 46
      (map (lambda (lan)
             (lambda (g wgt bx by bw bh selected?)
               (glgui:draw-pixmap-center (+ bx 5) (+ by 8) 30 29 (if selected? checkedcircle.img uncheckedcircle.img) White)
               (glgui:draw-text-left (+ bx 42) (+ by 11) (- bw 52) 23 (cdr lan) text_20.fnt Black)))
           rrate:settings:languagechoices)
      (lambda (g wgt type mx my)
        ;; Save the new settings
        (let* ((cur (glgui-widget-get rrate:settings:language rrate:settings:languagelist 'current))
               (lindex (car (list-ref rrate:settings:languagechoices cur))))
          (settings-set! "Language" lindex)
          (local-index-set! lindex)
          (rrate-init x y w h rrate:store rrate:cancelproc rrate:doneproc)))))
    (glgui-widget-set! rrate:settings:language rrate:settings:languagelist 'autohidebar #t)
    (glgui-widget-set! rrate:settings:language rrate:settings:languagelist 'bgcol1 (color:shuffle #xc0d1d5ff))
    (glgui-widget-set! rrate:settings:language rrate:settings:languagelist 'bgcol2 (color:shuffle #xd7eaefff))
    (glgui-widget-set! rrate:settings:language rrate:settings:languagelist 'offset oldoff)
    (let ((cur (list-pos rrate:settings:languagechoices (assoc (settings-ref "Language" 1) rrate:settings:languagechoices)))
          (listlengthdiff (if rrate:settings:show_vibrate 4 6)))
      (glgui-widget-set! rrate:settings:language rrate:settings:languagelist 'current cur)
      (if (fx> (- cur oldoff) listlengthdiff)
        (glgui-widget-set! rrate:settings:language rrate:settings:languagelist 'offset (- cur listlengthdiff)))))

  ;; The second page of settings, number of taps
  (set! rrate:settings:taps (glgui-container rrate:gui x y w h))

  ;; Setting for how many taps are needed
  (glgui-widget-set! rrate:settings:taps (glgui-box rrate:settings:taps 10 53 (- w 20) (- h 63) (color:shuffle #xd7eaefff)) 'rounded #t)
  (glgui-label-local rrate:settings:taps 25 (- h 150) (- w 50) 110 "CONSISTENCY_NUM_TAPS" text_20.fnt Black)
  (set! rrate:settings:tapslist (glgui-list rrate:settings:taps 20 (- h 360) (- w 40) 200 50
    (map (lambda (n) (lambda (g wgt bx by bw bh selected?)
      (let ((cx (+ bx (- (/ bw 2) 50))))
        (glgui:draw-pixmap-center cx (+ by 8) 30 29 (if selected? checkedcircle.img uncheckedcircle.img) White)
        (glgui:draw-text-left (+ cx 42) (+ by 9) 40 23 (local-get-text n) text_20.fnt Black))
    )) rrate:settings:tapchoices)
    (lambda (g wgt type mx my)
      ;; Save the new settings
      (let* ((tindex (glgui-widget-get rrate:settings:taps rrate:settings:tapslist 'current))
             (tstr (list-ref rrate:settings:tapchoices (max tindex 0)))
             (tvalue (string->number tstr)))
        (settings-set! "Taps" tvalue)
      )
    )
  ))
  (glgui-widget-set! rrate:settings:taps rrate:settings:tapslist 'autohidebar #t)
  (glgui-widget-set! rrate:settings:taps rrate:settings:tapslist 'bgcol1 (color-fade White 0))
  (glgui-widget-set! rrate:settings:taps rrate:settings:tapslist 'bgcol2 (color-fade White 0))
  (glgui-widget-set! rrate:settings:taps rrate:settings:tapslist 'current 2)

  ;; The third page of settings, consistency threshold
  (let ((leftx (+ (- w 320) 20)))
    (set! rrate:settings:consistency (glgui-container rrate:gui x y w h))
    (glgui-widget-set! rrate:settings:consistency (glgui-box rrate:settings:consistency 10 53 (- w 20) (- h 63) (color:shuffle #xd7eaefff)) 'rounded #t)
    (glgui-label-local rrate:settings:consistency 20 (- h 73) (- w 10) 60 "CONSISTENCY_THRESH" text_20.fnt Black)
    (glgui-widget-set! rrate:settings:consistency (glgui-label-local rrate:settings:consistency 10 (- h 110) (- w 20) 40 "M_MEDIAN" text_14.fnt Black) 'align GUI_ALIGNCENTER)
    (glgui-widget-set! rrate:settings:consistency (glgui-label-local rrate:settings:consistency 10 (- h 130) (- w 20) 20 "C_CONSISTENCY" text_14.fnt Black) 'align GUI_ALIGNCENTER)
    (glgui-widget-set! rrate:settings:consistency (glgui-label-local rrate:settings:consistency leftx (- h 163) 55 20 "M_pC" text_14.fnt Black) 'align GUI_ALIGNRIGHT)
    (glgui-widget-set! rrate:settings:consistency (glgui-label-local rrate:settings:consistency leftx (- h 178) 55 20 "M" text_14.fnt Black) 'align GUI_ALIGNRIGHT)
    (glgui-widget-set! rrate:settings:consistency (glgui-label-local rrate:settings:consistency leftx (- h 193) 55 20 "M_mC" text_14.fnt Black) 'align GUI_ALIGNRIGHT)
    (glgui-pixmap rrate:settings:consistency (+ leftx 58) (- h 198) diagram.img)
    (glgui-label-local rrate:settings:consistency (+ leftx 103) (- h 158) (- w 10) 20 "INCONSISTENT" text_14.fnt Black)
    (glgui-label-local rrate:settings:consistency (+ leftx 106) (- h 178) (- w 10) 20 "CONSISTENTD" text_14.fnt Black)
    (glgui-label-local rrate:settings:consistency (+ leftx 103) (- h 198) (- w 10) 20 "INCONSISTENT" text_14.fnt Black))
  (set! rrate:settings:percentlist (glgui-list rrate:settings:consistency 20 (- h 390) (- w 40) 180 35
    (map (lambda (p) (lambda (g wgt bx by bw bh selected?)
      (let ((cx (+ bx (- (/ bw 2) 50))))
        (glgui:draw-pixmap-center cx (+ by 8) 30 29 (if selected? checkedcircle.img uncheckedcircle.img) White)
        (glgui:draw-text-left (+ cx 42) (+ by 9) 50 23 (string-append (local-get-text p) "%") text_20.fnt Black))
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

  ;; The fourth page of settings, REDCap info
  (set! rrate:settings:redcap (glgui-container rrate:gui x y w h))
  (glgui-widget-set! rrate:settings:redcap (glgui-box rrate:settings:redcap 10 53 (- w 20) (- h 63) (color:shuffle #xd7eaefff)) 'rounded #t)
  (glgui-label-local rrate:settings:redcap 25 (- h 50) (- w 50) 30 "REDCAP" text_20.fnt Black)

  (checkbox rrate:settings:redcap 25 (- h 70) (- w 50) "REDCAP_USE?"
    (lambda (label checked? g wgt . xargs)
      (settings-set! label checked?)
      (set-boxcontainer-hidden (not checked?))
      (if (not checked?) (set-keypad-hidden #t))))

  (set! rrate:settings:redcap:boxcontainer (glgui-container rrate:settings:redcap 25 (- h 260) (- w 50) 180))
  (let ((aftercharcb (lambda (label g wgt . xargs)
          (settings-set! label (glgui-widget-get g wgt 'label))))
        (onfocuscb (lambda (g wgt . xargs)
          (set! rrate:settings:redcap:focusedbox wgt)
          (set-keypad-hidden #f))))
    (textboxes-hor rrate:settings:redcap:boxcontainer '("HOST" "URL")   (- w 50) 140 aftercharcb onfocuscb)
    (textboxes-ver rrate:settings:redcap:boxcontainer '("TOKEN")        (- w 50) 90  aftercharcb onfocuscb)
    (textboxes-hor rrate:settings:redcap:boxcontainer '("FORM" "EVENT") (- w 50) 40  aftercharcb onfocuscb))
  (set-boxcontainer-hidden (not (settings-ref "REDCAP_USE?")))

  (set! rrate:settings:redcap:uploadbutton
    (glgui-button-local rrate:settings:redcap:boxcontainer 0 0 (- w 50) 30 "UPLOAD" text_20.fnt
      (lambda (g wgt . xargs)
        (if (not (rrate:redcap-upload))
            (rrate:show-popup rrate:popup:redcap #f))
        (set-uploadbutton-hidden))))
  (glgui-widget-set! rrate:settings:redcap:boxcontainer rrate:settings:redcap:uploadbutton 'button-normal-color Red)
  (glgui-widget-set! rrate:settings:redcap:boxcontainer rrate:settings:redcap:uploadbutton 'button-selected-color DarkRed)
  (set-uploadbutton-hidden)

  (set! rrate:settings:keypad (glgui-keypad rrate:gui 0 43 w 160 text_14.fnt))
  (glgui-widget-set! rrate:gui rrate:settings:keypad 'hideonreturn defocus-focusedbox)
  (set-keypad-hidden #t)

  ;; Show vibrate option on first page under language options
  (if rrate:settings:show_vibrate
    ;; Show checkbox for turning on and off vibration with sound (only Android)
    (let ((vbottom 53)
          ;; Put below language options
          (vh (- h 348)))
      (glgui-widget-set! rrate:settings:language (glgui-box rrate:settings:language 10 vbottom (- w 20) vh (color:shuffle #xd7eaefff)) 'rounded #t)
      (glgui-label rrate:settings:language 95 (+ vbottom (- (/ vh 2) 15)) (- w 105) 25 (local-get-text "VIBRATE_SOUND") text_20.fnt Black)
      (set! rrate:settings:vibrate_box (glgui-pixmap rrate:settings:language 55 (+ vbottom (- (/ vh 2) 15)) checkedbox.img))
      (set! rrate:settings:vibrate_trigger (glgui-box rrate:settings:language 42 (+ vbottom (- (/ vh 2) 25)) (- w 52) 50 (color-fade White 0)))
      (glgui-widget-set! rrate:settings:language rrate:settings:vibrate_trigger 'callback
          (lambda (g . x)
             (if (settings-ref "VibrateSound")
               (begin
                 (settings-set! "VibrateSound" #f)
                 (glgui-widget-set! rrate:settings:language rrate:settings:vibrate_box 'image uncheckedbox.img))
               (begin
                 (settings-set! "VibrateSound" #t)
                 (glgui-widget-set! rrate:settings:language rrate:settings:vibrate_box 'image checkedbox.img)))))))

  ;; Go to the next page or finish settings
  (set! rrate:settings:nextbutton (glgui-button rrate:settings:bg (- w 107) 6 100 32 right_arrow.img
    (lambda (g . x)
      (if rrate:settings:keypad (set-keypad-hidden #t))
      (if (fx= rrate:settings:page 3)
        ;; Leave the settings page
        (begin
          (set! rrate:settings:page (if rrate:no-language? 1 0))
          (set! rrate:settings:viewing #f))
        (set! rrate:settings:page (+ rrate:settings:page 1))))))
  (glgui-widget-set! rrate:settings:bg rrate:settings:nextbutton 'button-normal-color Green)
  (glgui-widget-set! rrate:settings:bg rrate:settings:nextbutton 'button-selected-color DarkGreen)
)

;; Setup list of languages choices in alphabetical order but with original indices
(define (rrate-setup-language-choices)
  (set! rrate:settings:languagechoices '())
  (let* ((lans (table-ref local:table "Key" '()))
         (len (length lans)))
    (let lloop ((i 0))
      (if (fx< i len)
        (begin
          ;; Add pair of language index and then language name
          (set! rrate:settings:languagechoices (append rrate:settings:languagechoices (list (cons (+ i 1) (list-ref lans i)))))
          (lloop (+ i 1)))
        ;; Sort pairs alphabetically
        (set! rrate:settings:languagechoices (sort rrate:settings:languagechoices (lambda (a b) (string<=? (cdr a) (cdr b))))))))
  rrate:settings:languagechoices
)

;; Upload data to REDCap
;; This uploads:
;;  - the integral respiratory rate to variable "rrate_rate";
;;  - when the save button was pressed to variable "rrate_time"; and
;;  - as a string, the timestamp for the first tap,
;;    then the time passed (s) since the first tap for each subsequent tap
;;    to variable "rrate_taps"
;; The given form will be set to "Complete" (2)
;; TODO: Error handling
(define (rrate:redcap-upload)
  (let* ((host  (settings-ref "HOST" #f))
         (url   (settings-ref "URL" #f))
         (token (settings-ref "TOKEN" #f))
         (event (settings-ref "EVENT" ""))
         (form  (settings-ref "FORM" #f))
         (success #t))
    (redcap-url-set! url)
    (table-for-each (lambda (recordno sessions)
      (let* ((repeatable? (redcap-repeatable? host token))
             (get-data (lambda (s) `(("rrate_rate" . ,(session-rate s))
                                     ("rrate_time" . ,(session-time s))
                                     ("rrate_taps" . ,(session-taps s)))))
             (upload (lambda (session instrument instance)
                (set! success (and success
                  (redcap-import-record host token recordno (get-data session) 'event event 'instrument instrument 'instance instance))))))
        (if repeatable?
            (let loop ((i (- (length sessions) 1))
                       (instance (redcap-get-next-instance host token recordno form)))
              (upload (list-ref sessions i) form (number->string instance))
              (if (> i 0) (loop (- i 1) (+ instance 1))))
            (upload (car sessions) #f #f))))
      rrate:datatable)
    (if success (rrate:erase-data))
    success))

;; Set REDCap upload button visibility
;; (It should really take a boolean parameter but the only time it's hidden is when the datatable is empty so I cheated a little)
(define (set-uploadbutton-hidden)
  (glgui-widget-set! rrate:settings:redcap:boxcontainer rrate:settings:redcap:uploadbutton 'hidden (= (table-length rrate:datatable) 0)))

;; Set textboxes' container's visibility
(define (set-boxcontainer-hidden b)
  (glgui-widget-set! rrate:settings:redcap rrate:settings:redcap:boxcontainer 'hidden b))

;; Set keyboard visibility and defocus textbox if keypad is closed
(define (set-keypad-hidden b)
  (glgui-widget-set! rrate:settings:redcap rrate:settings:keypad 'hidden b)
  (if b (defocus-focusedbox)))

;; Defocus currently focussed textbox
(define (defocus-focusedbox)
  (if rrate:settings:redcap:focusedbox
      (begin (glgui-widget-set! rrate:settings:redcap:boxcontainer rrate:settings:redcap:focusedbox 'focus #f)
             (set! rrate:settings:redcap:focusedbox #f))))

;; Draw checkbox with label
;; g: parent GUI of checkbox
;; x, y, w: (x, y) position of lower-left corner, width in pixels
;; label: label shown next to checkbox
;; callback: callback for checkbox toggle; takes a string (the label), a boolean (checkbox state),
;;  followed by standard callback arguments
;; Returns the pressable button part of the checkbox
;; For the moment, font is fixed at text_14.fnt and checkbox dimensions are fixed accordingly
(define (checkbox g x y w label callback)
  (let* ((d 17) (b 1)
         (d:inner (- d (* 2 b)))
         (padding (* 3 b))
         (d:button (- d (* 2 padding))))
    (glgui-label-local g (+ x d 4) y (- w d 4) d label text_14.fnt Black)
    (glgui-box g x y d d Black)
    (glgui-box g (+ x b) (+ y b) d:inner d:inner White)
    (let* ((set-checkbutton! (lambda (g wgt checked?)
              (let ((newcolour (if checked? Black White)))
                (glgui-widget-set! g wgt 'value (if checked? 1 0))
                (glgui-widget-set! g wgt 'button-normal-color newcolour)
                (glgui-widget-set! g wgt 'button-selected-color newcolour))))
           (cb (lambda (g wgt . xargs)
              (let* ((newvalue (if (= 0 (glgui-widget-get g wgt 'value)) 1 0))
                     (checked? (= 1 newvalue)))
                (set-checkbutton! g wgt checked?)
                (if (procedure? callback) (apply callback (append (list label checked? g wgt) xargs))))))
           (checkbutton (glgui-button-string g (+ x padding) (+ y padding) d:button d:button "" text_14.fnt cb)))
      (glgui-widget-set! g checkbutton 'solid-color #t)
      (glgui-widget-set! g checkbutton 'rounded #f)
      (set-checkbutton!  g checkbutton (settings-ref label))
      checkbutton)))

;; Draw empty textbox with border and label above
;; g: parent GUI of box
;; x, y, w: (x, y) position of lower-left corner, width in pixels
;; label: label shown above textbox
;; aftercharcb: callback for keypress; takes a string (the label) followed by standard callback arguments
;; onfocuscb: callback for focus obtained; takes standard callback arguments
;; Returns the editable label part of the textbox
;; For the moment, font is fixed at text_14.fnt and height is fixed accordingly
(define (textbox g x y w label aftercharcb onfocuscb)
  (let* ((h 23) (h:label 17) (b 1)
         (x:inner (+ x b))
         (y:inner (+ y b))
         (w:inner (- w (* 2 b)))
         (h:inner (- h (* 2 b))))
    (glgui-label-local g x (+ y h) w h:label label text_14.fnt Black)
    (glgui-box g x y w h Black)
    (glgui-box g x:inner y:inner w:inner h:inner White)
    (let ((inputlabel (glgui-inputlabel g (+ x:inner 2) y:inner (- w:inner 4) h:inner (settings-ref label) text_14.fnt Black White)))
      (glgui-widget-set! g inputlabel 'aftercharcb
        (lambda args (if (procedure? aftercharcb) (apply aftercharcb (cons label args)))))
      (glgui-widget-set! g inputlabel 'onfocuscb onfocuscb)
      inputlabel)))

;; Draw textboxes vertically
;; g: parent GUI of textboxes
;; labels: list of labels for which to draw a textbox each
;; width: width in pixels of parent GUI
;; y: y-position of bottom of the textboxes
;; aftercharcb, onfocuscb: see `textbox`
(define (textboxes-ver g labels width y aftercharcb onfocuscb)
  (let loop ((i 0))
    (if (< i (length labels))
        ;; 50 px = 10 for padding + 17 for the label + 23 for the textbox
        (let* ((top (+ y (* 50 (length labels))))
               (y:box (- top (* (+ i 1) 50))))
          (textbox g 0 y:box width (list-ref labels i) aftercharcb onfocuscb)
          (loop (+ i 1))))))

;; Draw textboxes horizontally
;; g: parent GUI of textboxes
;; labels: list of labels for which to draw a textbox each
;; width: width in pixels of parent GUI
;; y: y-position of bottom of the textboxes
;; aftercharcb, onfocuscb: see `textbox`
(define (textboxes-hor g labels width y aftercharcb onfocuscb)
  (let loop ((i 0))
    (if (< i (length labels))
      ;; Add 4px of padding between each box
      ;; First box has no offset and will be flush against the left side
      ;; Last box is flush against the right side and needs 4px offset
      ;; Other boxes need 2px padding on both sides so offset is 2px
      (let* ((box-width (quotient width (length labels)))
             (w (- box-width 4))
             (x (+ (* i box-width)
                   (cond
                     ((zero? i) 0)
                     ((< i (- (length labels) 1)) 2)
                     (else 4)))))
        (textbox g x y w (list-ref labels i) aftercharcb onfocuscb)
        (loop (+ i 1))))))

(define rrate:gui #f)
(define rrate:cont #f)
(define rrate:tapbg #f)
(define rrate:timer #f)
(define rrate:animationbg #f)
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
(define rrate:redcapsave #f)
(define rrate:redcapsave:ratelabel #f)
(define rrate:redcapsave:timeslabel #f)
(define rrate:redcapsave:keypad #f)
(define rrate:redcapsave:recordnobox #f)
(define rrate:redcapsave:backbutton #f)
(define rrate:redcapsave:savebutton #f)
;;(define rrate:tapmessage #f)

;; The popup with error messages
(define rrate:popup:cont #f)
(define rrate:popup:inconsistent #f)
(define rrate:popup:bg #f)
(define rrate:popup:inconsistent #f)
(define rrate:popup:notenough #f)
(define rrate:popup:toofast #f)
(define rrate:popup:retrybutton #f)
(define rrate:popup:ignorebutton #f)
(define rrate:popup:redcap #f)

;; The procedures for what to do when done with the module
(define rrate:cancelproc #f)
(define rrate:doneproc #f)

;; The store to save the values to
(define rrate:store #f)

;; Defines position of baby body parts
(define rrate:position:larmx_def 105)
(define rrate:position:larmx 105)
(define rrate:position:larmy_def 77)
(define rrate:position:larmy 77)
(define rrate:position:rarmx_def 105)
(define rrate:position:rarmx 105)
(define rrate:position:rarmy_def 74)
(define rrate:position:rarmy 74)
(define rrate:position:mouthx_def 184)
(define rrate:position:mouthx 184)
(define rrate:position:mouthy_def 264)
(define rrate:position:mouthy 264)
(define rrate:position:bodyx_def 105)
(define rrate:position:bodyx 105)
(define rrate:position:bodyy_def 74)
(define rrate:position:bodyy 74)

(define rrate:times '())
(define rrate:rate 0.)
(define rrate:animateoffset 0.0)

;; Offset in x and y for baby animation to center in available space
(define rrate:xoffset 0)
(define rrate:yoffset 0)

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

;; CDB for storing data to be uploaded to REDCap
;; The datatable will have the (key -> value) structure
;;  (recordno: integer -> sessions: session[])
;; While a session has the structure
;;  (rrate: string, time: string, taps: string)
;; where
;;  rate is the integral breathing rate in breaths/minute as a string
;;  time is the timestamp in seconds since epoch of when save was triggered
;;  taps is a string corresponding to the start time
;;    followed by time elapsed since start for each tap, separated by semicolons
(define rrate:filepath (string-append (system-directory) (system-pathseparator) "data.cdb"))
(define rrate:datatable #f)
(define-structure session rate time taps)

;; If a CDB exists, load it into datatable; otherwise, create empty table
;; N.B. Should only be called once on init
(define (rrate:loadcdb)
  (if (file-exists? rrate:filepath)
      (set! rrate:datatable (cdb->table rrate:filepath)))
  (if (not (table? rrate:datatable))
      (set! rrate:datatable (make-table))))

;; Save datatable to cdb
;; N.B. Should be called at least on exit
(define (rrate:writecdb) (table->cdb rrate:datatable rrate:filepath))

;; Save data to datatable
;; This copies part of `init-rrate` from parts/apps/PneumOx/sandbox/main.sx
(define (rrate:savedata recordno rrate times)
  (let ((sessions (table-ref rrate:datatable recordno #f)))
    (table-set! rrate:datatable recordno
        (cons (make-session
                rrate
                (seconds->string (current-time-seconds) "%Y-%m-%d %H:%M:%S")
                (let ((starttime (car times))
                     (nexttimes (cdr times))
                     (roundtostring (lambda (n) (number->string (round-decimal n 4)))))
                  (string-append
                    (seconds->string starttime "%Y-%m-%d %H:%M:%S")
                    (roundtostring (- starttime (floor starttime))) ";"
                    (string-mapconcat nexttimes ";" (lambda (n) (roundtostring (- n starttime)))))))
              (if sessions sessions '())))))

;; Get the next available record number; return 1 if none used
;; N.B. Will take O(n); if table is expected to be very large,
;;  call this once and keep track of largest record number in a variable
(define (rrate:get-next-recordno)
  (let ((recordno 0))
    (table-for-each (lambda (k v)
      (let ((key (string->number k)))
        (if (> key recordno) (set! recordno key)))) rrate:datatable)
    (number->string (+ recordno 1))))

;; Erase all existing data
;; N.B. Actual CDB file will not be written over until rrate:writecdb is called
(define (rrate:erase-data) (set! rrate:datatable (make-table)))

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

(define (rrate:tapcb g wgt time . x)
  (let* ((now (if rrate:timeonbuttondown?
                  time
                  (time->seconds (current-time))))
         (count (length rrate:times))
         (taplimit (settings-ref "Taps" 4))
         (playbreath #t))
    (set! rrate:times (append rrate:times (list now)))

    ;; If this is the first tap, set start time
    (if (fx= count 0)
      (set! rrate:starttime (car rrate:times)))

    ;; Change the colour of the icon for the most recent tap
    (if (not rrate:oneminute)
      (glgui-widget-set! rrate:cont (list-ref rrate:tapicons count) 'image dot_dark.img))
    (set! count (+ count 1))

    ;; After first tap show cancel button
    (glgui-widget-set! rrate:cont rrate:cancelbutton 'hidden #f)

    ;; Don't do check for ending if recording for one minute
    (if (and (fx>= count taplimit) (not rrate:oneminute))
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
              (glgui-widget-set! rrate:cont rrate:value 'x (+ (if (fx= (string-length valstr) 3) 35 44) rrate:xoffset))
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
                       (glgui-widget-set! rrate:cont rrate:value 'x (+ (if (fx= (string-length valstr) 3) 35 44) rrate:xoffset)))

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

;; Goes to:
;;  - the first  stage (1) which displays the tap icons;
;;  - the second stage (2) which displays the RR and animation; or
;;  - the third  stage (3) which displays the REDCap saving page.
(define (rrate:go-to-stage stage)
  (let ((stage1? (fx= stage 1))
        (stage2? (fx= stage 2))
        (stage3? (fx= stage 3)))
    (glgui-widget-set! rrate:cont rrate:cancelbutton          'hidden (or (not stage1?) (not rrate:cancelproc)))
    (glgui-widget-set! rrate:cont rrate:settingsbutton        'hidden (or (not stage1?) rrate:no-settings?))
    (glgui-widget-set! rrate:cont rrate:nobutton              'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:confirm               'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:yesbutton             'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:rarm                  'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:body                  'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:dbody                 'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:larm                  'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:mouth                 'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:trigger               'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:tapbutton             'hidden (not stage1?))
;;  (glgui-widget-set! rrate:cont rrate:tapmessage            'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:qualitybg             'hidden (or (not stage2?) rrate:oneminute))
    (glgui-widget-set! rrate:cont rrate:qualitybg_high        'hidden (or (not stage2?) rrate:oneminute))
    (glgui-widget-set! rrate:cont rrate:qualitybg_consistent  'hidden (or (not stage2?) rrate:oneminute))
    (glgui-widget-set! rrate:cont rrate:qualitybg_low         'hidden (or (not stage2?) rrate:oneminute))
    (glgui-widget-set! rrate:cont rrate:value                 'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:toplayer              'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:tapbg                 'hidden (not stage1?))
    (glgui-widget-set! rrate:cont rrate:timer                 'hidden (or (not stage1?) (not rrate:oneminute)))
    (glgui-widget-set! rrate:cont rrate:animationbg           'hidden (not stage2?))
    (glgui-widget-set! rrate:cont rrate:redcapsave            'hidden (not stage3?))
    (glgui-widget-set! rrate:cont rrate:redcapsave:backbutton 'hidden (not stage3?))
    (glgui-widget-set! rrate:cont rrate:redcapsave:savebutton 'hidden (not stage3?))

    ;; If leaving stage 3, hide keypad and defocus recordnobox
    (if (not stage3?)
      (begin
        (if rrate:redcapsave:recordnobox (glgui-widget-set! rrate:redcapsave rrate:redcapsave:recordnobox 'focus #f))
        (glgui-widget-set! rrate:redcapsave rrate:redcapsave:keypad 'hidden #t)))

    ;; Reset skipping breath sound if going back to stage 1
    (if stage1?
      (set! rrate:skipbreath #f))

    ;; Reset timer if going back to stage 1 and doing one minute tapping
    (if (and rrate:oneminute stage1?)
      (glgui-widget-set! rrate:cont rrate:timer 'label "0:00"))

    ;; Hide or show the tap icons
    (let loop ((ws rrate:tapicons))
       (if (> (length ws) 0)
         (begin
            (glgui-widget-set! rrate:cont (car ws) 'hidden (or (not stage1?) rrate:oneminute))
            (loop (cdr ws)))))

    ;; If going to the first stage, reset the taps
    (if stage1?
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
                     ((eq? message rrate:popup:notenough)
                        "Not enough taps")
                     (else
                        "Upload to REDCap failed")))

   ;; If the ignore option is used, show this button, otherwise hide it and center the retry button
   (if ignore?
     (begin
        (glgui-widget-set! rrate:popup:cont rrate:popup:ignorebutton 'hidden #f)
        (glgui-widget-set! rrate:popup:cont rrate:popup:retrybutton 'x (+ 52 rrate:xoffset)))
     (begin
        (glgui-widget-set! rrate:popup:cont rrate:popup:ignorebutton 'hidden #t)
        (glgui-widget-set! rrate:popup:cont rrate:popup:retrybutton 'x (+ 125 rrate:xoffset))))

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
  (if (or (= (audiofile-getvolume) 0.) (rrate-is-muted-headset))
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
  (if (and (> (audiofile-getvolume) 0.) (not (rrate-is-muted-headset)))
    (begin
      (if (not rrate:sound_on)
        (begin
           (set! rrate:sound_on #t)
           (audiofile-start)))
      (audiofile-play chimes)))
)

;; Give feedback that the RRate has not been successfully measured by playing a sound or lack of vibrating
(define (rrate:fail-feedback)
  (if (and (> (audiofile-getvolume) 0.) (not (rrate-is-muted-headset)))
    (begin
      (if (not rrate:sound_on)
        (begin
           (set! rrate:sound_on #t)
           (audiofile-start)))
      (audiofile-play beep)))
)

;; Resets the taps by clearing the tap icons and the data quality
;; and hiding the data quality
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

  ;; Show timer clock
  (if rrate:oneminute
    (begin
      (glgui-widget-set! rrate:cont rrate:timer 'hidden #f)
      (glgui-widget-set! rrate:cont rrate:timer 'label "0:00")))

  ;; Go back to stage 1 to try again
  (rrate:go-to-stage 1)
)

;; Clears the RRate related values from the store
(define (rrate-store-clear)
  (store-clear! rrate:store "RR")
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
   (if (not rrate:setup?) (rrate-setup))
   (set! rrate:store store)
   (set! rrate:cancelproc cancelproc)
   (set! rrate:doneproc doneproc)
   (rrate:loadcdb)

   (set! rrate:gui (make-glgui))
   (set! rrate:cont (glgui-container rrate:gui x y w h))

   ;; Switch which font is being used depending on the language
   (if (fx= (settings-ref "Language" 1) 4)
     (begin
       (set! text_12.fnt textEng_12.fnt)
       (set! text_14.fnt textAm_14.fnt)
       (set! text_20.fnt textAm_20.fnt)
       (set! text_40.fnt textEng_40.fnt))
     (begin
       (set! text_12.fnt textEng_12.fnt)
       (set! text_14.fnt textEng_14.fnt)
       (set! text_20.fnt textEng_20.fnt)
       (set! text_40.fnt textEng_40.fnt)))

   ;; Initialize the settings page and set the settings
   (rrate:setting-init x y w h)
   (let* ((taps (settings-ref "Taps" 5))
          (tindex (list-pos rrate:settings:tapchoices (number->string taps)))
          (consistency (settings-ref "Consistency" 13))
          (pindex (list-pos rrate:settings:percentchoices (number->string consistency))))
     (glgui-widget-set! rrate:settings:taps rrate:settings:tapslist 'current (if tindex tindex 0))
     (glgui-widget-set! rrate:settings:consistency rrate:settings:percentlist 'current (if pindex pindex 0))
     (if rrate:settings:show_vibrate
       (glgui-widget-set! rrate:settings:language rrate:settings:vibrate_box 'image (if (settings-ref "VibrateSound") checkedbox.img uncheckedbox.img))))

   ;; initialize audio - must come before audiofile-load!
   (audiofile-init)

   ;; Load sound effect
   (set! rrate:sound:breath (audiofile-load "breath"))
   (set! beep (audiofile-load "beep"))
   (set! chimes (audiofile-load "chimes"))

   ;; Black background behind everything
   (glgui-box rrate:cont 0 0 w h Black)

   (set! rrate:tapbg (glgui-pixmap rrate:cont 0 43 stage1_bg.img w (cadr stage1_bg.img)))

   ;; Timer for a one minute tapping session
   (set! rrate:timer (glgui-label rrate:cont 40 70 (- w 80) 60 "0:00" numbers_56.fnt Black))
   (glgui-widget-set! rrate:cont rrate:timer 'align GUI_ALIGNCENTER)
   (glgui-widget-set! rrate:cont rrate:timer 'hidden (not rrate:oneminute))

   (set! rrate:cancelbutton (glgui-button-local rrate:cont 12 6 145 32 "CANCEL" text_20.fnt
     (lambda (g . x)
       ;; Determine if there are any taps, if none then cancel out of module, otherwise just reset
       (if (fx= (length rrate:times) 0)
         (begin
           (rrate:writecdb)
           (if rrate:cancelproc (rrate:cancelproc)))
         (begin
           ;; Clear interval and scale values
           (set! rrate:calc:medinterval #f)
           (set! rrate:calc:yscale #f)
           (set! rrate:starttime #f)
           (rrate:go-to-stage 1)
         )
       )
     )
   ))
   (glgui-widget-set! rrate:cont rrate:cancelbutton 'button-normal-color Gold)
   (glgui-widget-set! rrate:cont rrate:cancelbutton 'button-selected-color Goldenrod)
   (glgui-widget-set! rrate:cont rrate:cancelbutton 'hidden (not rrate:cancelproc))

   ;; Go to the settings page after cancelling the current tapping
   (set! rrate:settingsbutton (glgui-button rrate:cont (- w 107) 6 100 32 icon_setup.img
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
   (if rrate:no-settings? (glgui-widget-set! rrate:cont rrate:settingsbutton 'hidden #t))

   ;; Remove the confirm question and show the other buttons instead
   (set! rrate:nobutton (glgui-button-local rrate:cont 6 6 68 32 "NO" text_20.fnt
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

   (set! rrate:restartbutton (glgui-button-local rrate:cont 6 6 140 32 "RESTART" text_20.fnt
     (lambda (g . x)
       (rrate-reset)
     )
   ))
   (glgui-widget-set! rrate:cont rrate:restartbutton 'button-normal-color Green)
   (glgui-widget-set! rrate:cont rrate:restartbutton 'button-selected-color DarkGreen)
   (glgui-widget-set! rrate:cont rrate:restartbutton 'hidden #t)

   ;; Confirmation question about animation
   (set! rrate:confirm (glgui-label-local rrate:cont 79 2 (- w 75 72) 36
     "RR_MATCH" text_14.fnt White))
   (glgui-widget-set! rrate:cont rrate:confirm 'hidden #t)

   ;; Remove the confirm question and show the other buttons instead, run done procedure
   (set! rrate:yesbutton (glgui-button-local rrate:cont (- w 65 6) 6 65 32 "YES" text_20.fnt
     (lambda (g . x)
       ;; Save the RR to the store
       (if rrate:store (store-set! rrate:store "RR" (glgui-widget-get rrate:cont rrate:value 'label)))
       (glgui-widget-set! rrate:cont rrate:confirm 'hidden #t)
       (glgui-widget-set! rrate:cont rrate:nobutton 'hidden #t)
       (glgui-widget-set! rrate:cont rrate:yesbutton 'hidden #t)

       ;; Go to stage 3 if REDCap is enabled in settings
       (if (settings-ref "REDCAP_USE?")
        (begin
          (glgui-widget-set! rrate:redcapsave rrate:redcapsave:recordnobox 'label (rrate:get-next-recordno))
          (glgui-widget-set! rrate:redcapsave rrate:redcapsave:ratelabel 'label
            (string-append
              (local-get-text "RRATE") " "
              (number->string (fix (round rrate:rate))) " "
              (local-get-text "RRATE_UNIT")))
          (glgui-widget-set! rrate:redcapsave rrate:redcapsave:timeslabel 'label
            (string-append
              (local-get-text "TAPS") " "
              (number->string (length rrate:times))))
          (rrate:go-to-stage 3))
        (begin
          (glgui-widget-set! rrate:cont rrate:restartbutton 'hidden #f)
          (if rrate:cancelproc (glgui-widget-set! rrate:cont rrate:exitbutton 'hidden #f))
          (if rrate:doneproc (rrate:doneproc))))
     )
   ))
   (glgui-widget-set! rrate:cont rrate:yesbutton 'button-normal-color Green)
   (glgui-widget-set! rrate:cont rrate:yesbutton 'button-selected-color DarkGreen)
   (glgui-widget-set! rrate:cont rrate:yesbutton 'hidden #t)

   (set! rrate:exitbutton (glgui-button-local rrate:cont (- w 146) 6 140 32 "EXIT" text_20.fnt
     (lambda (g . x)
       ;; Prepare for next time
       (rrate-reset)
       (rrate:writecdb)
       (if rrate:cancelproc
         (rrate:cancelproc)
         (glgui-widget-set! rrate:cont rrate:exitbutton 'hidden #t)
       )
     )
   ))
   (glgui-widget-set! rrate:cont rrate:exitbutton 'button-normal-color Green)
   (glgui-widget-set! rrate:cont rrate:exitbutton 'button-selected-color DarkGreen)
   (glgui-widget-set! rrate:cont rrate:exitbutton 'hidden #t)

   ;; Back and Save buttons for REDCap save page
   (set! rrate:redcapsave:backbutton (glgui-button-local rrate:cont 6 6 140 32 "BACK" text_20.fnt
      (lambda (g . x) (rrate:go-to-stage 2))))
   (glgui-widget-set! rrate:cont rrate:redcapsave:backbutton 'button-normal-color Green)
   (glgui-widget-set! rrate:cont rrate:redcapsave:backbutton 'button-selected-color DarkGreen)
   (glgui-widget-set! rrate:cont rrate:redcapsave:backbutton 'hidden #t)

   (set! rrate:redcapsave:savebutton (glgui-button-local rrate:cont (- w 146) 6 140 32 "SAVE" text_20.fnt
     (lambda (g . x)
       (rrate:savedata (glgui-widget-get rrate:redcapsave rrate:redcapsave:recordnobox 'label) (glgui-widget-get rrate:cont rrate:value 'label) rrate:times)
       (set-uploadbutton-hidden)
       (rrate:go-to-stage 2)
       (glgui-widget-set! rrate:cont rrate:confirm 'hidden #t)
       (glgui-widget-set! rrate:cont rrate:nobutton 'hidden #t)
       (glgui-widget-set! rrate:cont rrate:yesbutton 'hidden #t)
       (glgui-widget-set! rrate:cont rrate:restartbutton 'hidden #f)
       (if rrate:cancelproc (glgui-widget-set! rrate:cont rrate:exitbutton 'hidden #f))
       (if rrate:doneproc (rrate:doneproc)))))
   (glgui-widget-set! rrate:cont rrate:redcapsave:savebutton 'button-normal-color Green)
   (glgui-widget-set! rrate:cont rrate:redcapsave:savebutton 'button-selected-color DarkGreen)
   (glgui-widget-set! rrate:cont rrate:redcapsave:savebutton 'hidden #t)

   ;; Dots that show how many taps have been done so far
   (let ((tw (/ (- w 20 (car dot_light.img)) 11))
         (ty 73))
     (let dloop ((tx 10) (icons '()))
       (if (fx< (length icons) 12)
         (dloop (+ tx tw) (append icons (list (glgui-pixmap rrate:cont tx ty dot_light.img))))
         (set! rrate:tapicons icons))))
   ;; Don't show these if recording for one minute
   (if rrate:oneminute
     (for-each (lambda (t) (glgui-widget-set! rrate:cont t 'hidden #t)) rrate:tapicons))

   ;; Animated parts of the baby with ghosted parts on top
  (let* ((wspace (/ (- w (car top_layer.img)) 2))
         (hspace (/ (- h (cadr top_layer.img) 92) 2))
         (yimage (+ hspace 92))
         (yabove (floor (+ yimage (cadr top_layer.img)))))
     (set! rrate:xoffset wspace)
     (set! rrate:yoffset (+ hspace 1))
     (set! rrate:animationbg (glgui-pixmap rrate:cont (+ rrate:xoffset 35) (+ 43 (max rrate:yoffset 0)) stage2_bg.img w (cadr stage2_bg.img)))
     (glgui-widget-set! rrate:cont rrate:animationbg 'hidden #t)
     (set! rrate:position:rarmx (+ rrate:position:rarmx_def rrate:xoffset))
     (set! rrate:position:rarmy (+ rrate:position:rarmy_def rrate:yoffset))
     (set! rrate:rarm (glgui-sprite rrate:cont 'x rrate:position:rarmx  'y rrate:position:rarmy  'image right_arm.img 'rendercallback rrate:animate-right-arm))
     (glgui-widget-set! rrate:cont rrate:rarm 'hidden #t)
     (set! rrate:position:bodyx (+ rrate:position:bodyx_def rrate:xoffset))
     (set! rrate:position:bodyy (+ rrate:position:bodyy_def rrate:yoffset))
     (set! rrate:body (glgui-sprite rrate:cont 'x rrate:position:bodyx  'y rrate:position:bodyy  'image body.img 'rendercallback rrate:animate-body))
     (glgui-widget-set! rrate:cont rrate:body 'hidden #t)
     (set! rrate:dbody (glgui-pixmap rrate:cont rrate:position:bodyx (- rrate:position:bodyy 1) dotted_body.img))
     (glgui-widget-set! rrate:cont rrate:dbody 'hidden #t)
     (set! rrate:position:larmx (+ rrate:position:larmx_def rrate:xoffset))
     (set! rrate:position:larmy (+ rrate:position:larmy_def rrate:yoffset))
     (set! rrate:larm (glgui-sprite rrate:cont 'x rrate:position:larmx 'y rrate:position:larmy 'image left_arm.img 'rendercallback rrate:animate-left-arm))
     (glgui-widget-set! rrate:cont rrate:larm 'hidden #t)

     ;; Circle, head and bubble
     (set! rrate:toplayer (glgui-container rrate:cont 0 0 w h))
     ;; Put circle in center vertically
     (glgui-pixmap rrate:toplayer rrate:xoffset yimage top_layer.img)
     (glgui-box rrate:toplayer 0 43 w (- yimage 43) rrate:sidecolor)
     (glgui-box rrate:toplayer 0 yabove w hspace rrate:sidecolor)
     (if (> rrate:xoffset 0)
       (begin
         (glgui-box rrate:toplayer 0 43 rrate:xoffset (- h 43) rrate:sidecolor)
         (glgui-box rrate:toplayer (- w rrate:xoffset) 43 rrate:xoffset (- h 43) rrate:sidecolor)))
     (set! rrate:toplayer_rrate (glgui-label-local rrate:toplayer (+ 40 rrate:xoffset) (+ 362 rrate:yoffset) 100 40 "RRATE" text_14.fnt DarkBlue GUI_ALIGNLEFT GUI_ALIGNTOP))
     (glgui-label-local rrate:toplayer (+ 40 rrate:xoffset) (+ 295 rrate:yoffset) 105 40 "RRATE_UNIT" text_14.fnt DarkBlue GUI_ALIGNLEFT GUI_ALIGNBOTTOM)
     (glgui-widget-set! rrate:cont rrate:toplayer 'hidden #t))

   ;; Animated mouth
   (set! rrate:position:mouthx (+ rrate:position:mouthx_def rrate:xoffset))
   (set! rrate:position:mouthy (+ rrate:position:mouthy_def rrate:yoffset))
   (set! rrate:mouth (glgui-sprite rrate:cont 'x rrate:position:mouthx 'y rrate:position:mouthy 'image mouth.img 'rendercallback rrate:animate-mouth))
   (glgui-widget-set! rrate:cont rrate:mouth 'hidden #t)

   (set! rrate:value (glgui-label rrate:cont (+ 44 rrate:xoffset) (- (glgui-widget-get rrate:toplayer rrate:toplayer_rrate 'y) 60) 150 55 "" numbers_56.fnt rrate:textcolor))
   (glgui-widget-set! rrate:cont rrate:value 'hidden #t)

   ;; Message about synchronizing the animation
;;   (set! rrate:tapmessage (glgui-label-local rrate:cont 2 105 80 55 "TAP_TO_SYNC" text_12.fnt White GUI_ALIGNLEFT GUI_ALIGNBOTTOM))
;;   (glgui-widget-set! rrate:cont rrate:tapmessage 'color Black)
;;   (glgui-widget-set! rrate:cont rrate:tapmessage 'hidden #t)

   ;; Make quality lines
   (set! rrate:qualitybg (glgui-pixmap rrate:cont 0 53 quality_lines.img w (cadr quality_lines.img)))
   (glgui-widget-set! rrate:cont rrate:qualitybg 'hidden #t)
   (let ((x (- w 128)) (w1 125) (h1 20))
     (set! rrate:qualitybg_high (glgui-label-local rrate:cont x 87 w1 h1 "FAST" text_14.fnt Black GUI_ALIGNRIGHT GUI_ALIGNCENTER))
     (set! rrate:qualitybg_consistent (glgui-label-local rrate:cont x 69 w1 h1 "CONSISTENT" text_14.fnt Black GUI_ALIGNRIGHT GUI_ALIGNCENTER))
     (set! rrate:qualitybg_low (glgui-label-local rrate:cont x 50 w1 h1 "SLOW" text_14.fnt Black GUI_ALIGNRIGHT GUI_ALIGNCENTER)))
   (for-each (lambda (w) (glgui-widget-set! rrate:cont w 'hidden #t))
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
   ;; Callback is void because input-handle of button is overwritten below
   (set! rrate:tapbutton (glgui-button-local rrate:cont 3 165 (- w 6) (- h 165) "TAP_INHALATION" text_40.fnt #f))
   (glgui-widget-set! rrate:cont rrate:tapbutton 'multiline #t)
   (glgui-widget-set! rrate:cont rrate:tapbutton 'button-normal-color (color-rgb 213 233 238))
   (glgui-widget-set! rrate:cont rrate:tapbutton 'button-selected-color (color-rgb 42 54 146))
   (glgui-widget-set! rrate:cont rrate:tapbutton 'input-handle
     (lambda (g wgt type mx my)
       (let* ((x (glgui-widget-get-dyn g wgt 'x))
              (y (glgui-widget-get-dyn g wgt 'y))
              (w (glgui-widget-get-dyn g wgt 'w))
              (h (glgui-widget-get-dyn g wgt 'h))
              (armed (glgui-widget-get g wgt 'armed))
              (time  (glgui-widget-get g wgt 'time))
              (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
          (cond
            ((and (= type EVENT_BUTTON1DOWN) inside)
               (glgui-widget-set! g wgt 'armed #t)
               (glgui-widget-set! g wgt 'time (time->seconds (current-time))))
            ((= type EVENT_BUTTON1UP)
               (if (and armed inside) (begin
               (rrate:tapcb g wgt time)))
               (glgui-widget-set! g wgt 'armed #f))
          )
       inside
   )))

   ;; REDCap save page
   (set! rrate:redcapsave (glgui-container rrate:cont x 43 w (- h 43)))
   (glgui-box rrate:redcapsave x 0 w h White)
   (glgui-label rrate:redcapsave 25 (- h 98) (- w 50) 30 (local-get-text "REDCAP_SAVE") text_20.fnt Black)
   (set! rrate:redcapsave:ratelabel  (glgui-label rrate:redcapsave 25 (- h 123) (- w 50) 25 "" text_14.fnt Black))
   (set! rrate:redcapsave:timeslabel (glgui-label rrate:redcapsave 25 (- h 148) (- w 50) 25 "" text_14.fnt Black))
   (set! rrate:redcapsave:keypad (glgui-keypad rrate:redcapsave 0 0 w 200 text_14.fnt keypad:numeric))
   (set! rrate:redcapsave:recordnobox (textbox rrate:redcapsave 25 (- h 198) (- w 50) "RECORD_NO"
      (lambda (label g wgt . xargs)
        (let ((recordno (glgui-widget-get rrate:redcapsave rrate:redcapsave:recordnobox 'label)))
          (glgui-widget-set! rrate:cont rrate:redcapsave:savebutton 'hidden (not (string->number recordno)))))
      (lambda (g wgt . xargs)
        (glgui-widget-set! rrate:redcapsave rrate:redcapsave:keypad 'hidden #f))))
   (glgui-widget-set! rrate:cont rrate:redcapsave 'hidden #t)

   ;; Create popup background and message
   (set! rrate:popup:cont (glgui-container rrate:gui x y w h))
   (glgui-widget-set! rrate:gui rrate:popup:cont 'modal #t)
   (set! rrate:popup:bg (glgui-box rrate:popup:cont (+ 45 rrate:xoffset) (+ 113 rrate:yoffset) 300 160 (color-fade Black 0.8)))
   (glgui-widget-set! rrate:popup:cont rrate:popup:bg 'rounded #t)
   (glgui-widget-set! rrate:popup:cont rrate:popup:bg 'modal #t)

   ;; Error messages about respiratory rate
   (set! rrate:popup:inconsistent (glgui-label-local rrate:popup:cont (+ 75 rrate:xoffset) (+ 166 rrate:yoffset) 240 100
     "TAPS_INCONSISTENT" text_20.fnt White))
   (glgui-widget-set! rrate:popup:cont rrate:popup:inconsistent 'hidden #t)
   (glgui-widget-set! rrate:popup:cont rrate:popup:inconsistent 'modal #t)
   (set! rrate:popup:notenough (glgui-label-local rrate:popup:cont (+ 75 rrate:xoffset) (+ 166 rrate:yoffset) 240 100
      "NOT_ENOUGH_TAPS" text_20.fnt White))
   (glgui-widget-set! rrate:popup:cont rrate:popup:notenough 'hidden #t)
   (glgui-widget-set! rrate:popup:cont rrate:popup:notenough 'modal #t)
   (set! rrate:popup:toofast (glgui-label-local rrate:popup:cont (+ 75 rrate:xoffset) (+ 166 rrate:yoffset) 240 100
     "TAPS_TOO_FAST" text_20.fnt White))
   (glgui-widget-set! rrate:popup:cont rrate:popup:toofast 'hidden #t)
   (glgui-widget-set! rrate:popup:cont rrate:popup:toofast 'modal #t)
   (set! rrate:popup:redcap (glgui-label-local rrate:popup:cont (+ 75 rrate:xoffset) (+ 166 rrate:yoffset) 240 100
     "REDCAP_UPLOAD_FAILED" text_20.fnt White))
   (glgui-widget-set! rrate:popup:cont rrate:popup:toofast 'hidden #t)
   (glgui-widget-set! rrate:popup:cont rrate:popup:toofast 'modal #t)

   ;; Popup buttons

   ;; Make retry button with the same callback as the no button
   (set! rrate:popup:retrybutton (glgui-button-local rrate:popup:cont (+ 52 rrate:xoffset) (+ 125 rrate:yoffset) 139 32 "RETRY" text_20.fnt
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
   (set! rrate:popup:ignorebutton (glgui-button-local rrate:popup:cont (+ 198 rrate:xoffset) (+ 125 rrate:yoffset) 139 32 "IGNORE" text_20.fnt
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
     (if (>= (- (current-time-seconds) rrate:starttime) 60.0)
       (begin
         ;; If no median interval yet, then just use median of all taps
         (if (and (not rrate:calc:medinterval) (not rrate:oneminute))
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
               (glgui-widget-set! rrate:cont rrate:value 'x (+ (if (fx= (string-length valstr) 3) 35 44) rrate:xoffset))
               (set! rrate:rate medrate)
               ;; Show popup, and change RR colour to red for if we go to the animation page
               (glgui-widget-set! rrate:cont rrate:value 'color Grey)
               (rrate:show-popup (if (fx< (length rrate:times) 4) rrate:popup:notenough
                 (if (>= medrate 140) rrate:popup:toofast rrate:popup:inconsistent)) (< medrate 200)
               )
             )
           )
           ;; If using full minute of taps, just count them
           (if rrate:oneminute
             (let* ((count (length rrate:times))
                    (valstr (number->string count)))
               (set! rrate:rate (fixnum->flonum count))
               (glgui-widget-set! rrate:cont rrate:value 'label (local-get-text valstr))
               (glgui-widget-set! rrate:cont rrate:value 'x (+ (if (fx= (string-length valstr) 3) 35 44) rrate:xoffset)))))

         (if (not rrate:oneminute)
           ;; Show the quality feedback as a artificial horizon
           (rrate:show-quality))

         ;; Play the chimes/beep if the volume is turned on, otherwise if vibrating it will now have stopped
         (if rrate:oneminute
           (rrate:success-feedback)
           (rrate:fail-feedback))

         ;; Delay the breathing sounds during animation
         (set! rrate:skipbreath ##now)

         ;; Set animate offset
         (rrate:set-animate-offset)
         (set! rrate:starttime #f)
         (if rrate:oneminute
           (rrate:go-to-stage 2)
           (glgui-modal-set! #t)))
       ;; Otherwise if recording for one minute, update timer
       (if rrate:oneminute
         (let* ((seconds (number->string (fix (- (current-time-seconds) rrate:starttime))))
                (str (string-append (if (fx= (string-length seconds) 1) "0:0" "0:") seconds)))
           (glgui-widget-set! rrate:cont rrate:timer 'label str)))))

  ;; Display either the main RRate page or the settings page
  (if (fx= t EVENT_REDRAW)
    (begin
      (glgui-widget-set! rrate:gui rrate:settings:bg 'hidden (not rrate:settings:viewing))
      (glgui-widget-set! rrate:gui rrate:settings:language 'hidden (or (not rrate:settings:viewing) (not (fx= rrate:settings:page 0))))
      (glgui-widget-set! rrate:gui rrate:settings:taps 'hidden (or (not rrate:settings:viewing) (not (fx= rrate:settings:page 1))))
      (glgui-widget-set! rrate:gui rrate:settings:consistency 'hidden (or (not rrate:settings:viewing) (not (fx= rrate:settings:page 2))))
      (glgui-widget-set! rrate:gui rrate:settings:redcap 'hidden (or (not rrate:settings:viewing) (not (fx= rrate:settings:page 3))))
      (glgui-widget-set! rrate:gui rrate:cont 'hidden rrate:settings:viewing)))
)


;; eof
