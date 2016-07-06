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

;; main loop
(main
 (lambda (w h)
   (make-window 320 480)
   (glgui-orientation-set! GUI_PORTRAIT)
   (set! gui (make-glgui))
   (set! gui:lang (make-glgui))
   (let ((w (glgui-width-get))
         (h (glgui-height-get)))
     ;; Setup the menubar     
     (glgui-menubar gui 0 (- h 44) w 44)
     (glgui-pixmap gui 8 (- h 32) RRATE.img)
     (glgui-widget-set! gui (glgui-label-wrapped gui 120 (- h 44) 200 39 
       (string-append "RRate " (system-appversion) "\nCopyright \302\251 2016\nUniversity of British Columbia") sans_10.fnt White)
       'align GUI_ALIGNRIGHT)
     
     ;; Use the rrate module with no store (no saving data),
     ;; terminate if cancelled or exited and do nothing extra if RRate confirmed 
     (rrate-setup)
     (let ((l (settings-ref "Language" #f)))
       (if l
         (begin
           (set! language? l)
           (local-index-set! l)
           (rrate-init 0 0 w 433 #f terminate #f)
         )
         ;; Language selection
         (let ((lanlist (rrate-setup-language-choices)))
           (glgui-widget-set! gui:lang (glgui-box gui:lang 10 10 (- w 20) (- h 64) (color:shuffle #xd7eaefff)) 'rounded #t)
           (glgui-label gui:lang 30 (- h 87) (- w 60) 23 "Select language" textEng_20.fnt Black)
           (let loop ((i 0) (by (- h 137)))
             (if (fx< i (length lanlist))
               (let* ((entry (list-ref lanlist i))
                      (lindex (car entry))
                      (button (glgui-button-string gui:lang 85 by 150 32 (cdr entry) textEng_20.fnt
                                (lambda (g wgt type mx my)
                                  (settings-set! "Language" lindex)
                                  (set! language? #t)
                                  (local-index-set! lindex)
                                  (rrate-init 0 0 w 433 #f terminate #f)))))
                 (glgui-widget-set! gui:lang button 'button-normal-color Black)
                 (glgui-widget-set! gui:lang button 'button-selected-color Gray)
                 (loop (+ i 1) (- by 46)))))
         )
       )
     )
   )
 )
 (lambda (t x y)
   ;; Call RRate main loop
   (if language? (rrate-run t))
   
   (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYESCAPE)) (terminate))
   (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYBACK))
     (terminate))
   (glgui-event (list (if language? rrate:gui gui:lang) gui) t x y)
  )   
 (lambda () #t)
 (lambda () (glgui-suspend))
 (lambda () (glgui-resume))
)

;; eof
