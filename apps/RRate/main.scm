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

;; RRate - an app for measuring and confirming respiratory rate
;; This is a thin wrapper around the rrate module
(define gui #f)
(define gui:lang #f)
(define gui:langlist #f)
(define language? #f)
(define rrate:svsmode:rr #f)
(define rrate:svsmode:rrtaps #f)

(define (rrate-checksvsreg)
  (svs-register-vitalsign VITALSIGN_RRATE)
  (let ((vitalsign (svs-get-vitalsign)))
    (set! rrate:svsmode:rr     (or (= VITALSIGN_RR     vitalsign) (= VITALSIGN_RRATE vitalsign)))
    (set! rrate:svsmode:rrtaps (or (= VITALSIGN_RRTAPS vitalsign) (= VITALSIGN_RRATE vitalsign))))
)

(define (rrate-sendvitalsign)
  (let ((hasrr (and rrate:calc:medinterval rrate:svsmode:rr))
        (hasrrtaps (and rrate:times rrate:svsmode:rrtaps)))
    (if hasrr
        (svs-pass-vitalsign (round (/ 60. rrate:calc:medinterval)) 100 VITALSIGN_RR))
    (if hasrrtaps
        (svs-pass-vitalsign-string (taptimes->string rrate:times) 100 VITALSIGN_RRTAPS))
    (if (or hasrr hasrrtaps) (svs-finish)))
)

(define (rrate-terminate)
  (if (or rrate:svsmode:rr rrate:svsmode:rrtaps) (svs-cancel))
  (terminate))

;; main loop
(main
 (lambda (w h)
   (make-window 320 480)
   (glgui-orientation-set! GUI_PORTRAIT)
   (rrate-checksvsreg)
   (set! gui (make-glgui))
   (set! gui:lang (make-glgui))
   (let ((w (glgui-width-get))
         (h (glgui-height-get)))
     ;; Setup the menubar
     (glgui-menubar gui 0 (- h 44) w 44)
     (glgui-pixmap gui 8 (- h 32) RRATE.img)
     (glgui-widget-set! gui (glgui-label-wrapped gui 120 (- h 44) 200 39 
       (string-append "RRate " (system-appversion) "\nCopyright \302\251 2017\nUniversity of British Columbia") sans_10.fnt White)
       'align GUI_ALIGNRIGHT)

     ;; Use the rrate module with no store (no saving data),
     ;; terminate if cancelled or exited and do nothing extra if RRate confirmed 
     (rrate-setup)
     (let ((l (settings-ref "Language" #f)))
       (if l
         (begin
           (set! language? l)
           (local-index-set! l)
           (rrate-init 0 0 w 433 #f rrate-terminate rrate-sendvitalsign)
         )
         ;; Language selection
         (let ((lanlist (rrate-setup-language-choices)))
           (glgui-widget-set! gui:lang (glgui-box gui:lang 10 10 (- w 20) (- h 64) (color:shuffle #xd7eaefff)) 'rounded #t)
           (glgui-label gui:lang 30 (- h 87) (- w 60) 23 "Select language" textEng_20.fnt Black)
           (let loop ((i 0) (by (- h 157)))
             (if (fx< i (length lanlist))
               (let* ((entryl (list-ref lanlist i))
                      (lindexl (car entryl))
                      (buttonl (glgui-button-string gui:lang 30 by 120 32 (cdr entryl) textEng_20.fnt
                                (lambda (g wgt type mx my)
                                  (settings-set! "Language" lindexl)
                                  (set! language? #t)
                                  (local-index-set! lindexl)
                                  (rrate-init 0 0 w 433 #f rrate-terminate rrate-sendvitalsign)))))
                 (glgui-widget-set! gui:lang buttonl 'button-normal-color Black)
                 (glgui-widget-set! gui:lang buttonl 'button-selected-color Gray)
                 (if (fx< (+ i 1) (length lanlist))
                   (let* ((entryr (list-ref lanlist (+ i 1)))
                          (lindexr (car entryr))
                          (buttonr (glgui-button-string gui:lang 170 by 120 32 (cdr entryr) textEng_20.fnt
                                    (lambda (g wgt type mx my)
                                      (settings-set! "Language" lindexr)
                                      (set! language? #t)
                                      (local-index-set! lindexr)
                                      (rrate-init 0 0 w 433 #f rrate-terminate rrate-sendvitalsign)))))
                   (glgui-widget-set! gui:lang buttonr 'button-normal-color Black)
                   (glgui-widget-set! gui:lang buttonr 'button-selected-color Gray)))
                 (loop (+ i 2) (- by 52)))))
         )
       )
     )
   )
 )
 (lambda (t x y)
   ;; Call RRate main loop
   (if language? (rrate-run t))

   (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYESCAPE)) (rrate-terminate))
   (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYBACK))   (rrate-terminate))
   (glgui-event (list (if language? rrate:gui gui:lang) gui) t x y)
  )
 (lambda () #t)
 (lambda () (glgui-suspend))
 (lambda ()
    (rrate-checksvsreg)
    (let ((state (table-ref (svs-get-extras) 'state)))
      (if (and (or rrate:svsmode:rr rrate:svsmode:rrtaps)
               (= state VITALSIGN_STATE_NEW)
               (procedure? rrate-reset))
          (rrate-reset)))
    (glgui-resume))
)

;; eof
