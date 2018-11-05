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
;; minimal RRate requestor app, uses ShareVitalSigns library

(define gui #f)
(define val #f)


(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (glgui-menubar gui 0 (- (glgui-height-get) 44) (glgui-width-get) 44)
    (glgui-pixmap  gui 8 (- (glgui-height-get) 32) title.img)
    (let* ((bw 160) (bh 50)
           (bx (/ (- (glgui-width-get) bw) 2.))
           (by (/ (- (glgui-height-get) bh) 2.)))
        (glgui-button gui bx (+ by (* bh 2) 5) bw bh button1.img
          (lambda (un . used) (svs-request-vitalsign VITALSIGN_RR 'state VITALSIGN_STATE_NEW)))
        (glgui-button gui bx (+ by bh) bw bh button2.img
          (lambda (un . used) (svs-request-vitalsign VITALSIGN_RR 'state VITALSIGN_STATE_RESUME)))
        (glgui-widget-set! gui (glgui-label gui bx (- by bh) bw bh "RRate Value: " ascii_18.fnt White) 'align GUI_ALIGNCENTER)
        (set! val (glgui-label gui bx (- by (* bh 2)) bw bh "" ascii_18.fnt Black White))
        (glgui-widget-set! gui val 'align GUI_ALIGNCENTER))
  )

;; events
  (lambda (t x y) 
    (if (= t EVENT_KEYPRESS) (begin 
      (if (= x EVENT_KEYESCAPE) (terminate))))
    
    (let ((rr (svs-retrieve-vitalsign VITALSIGN_RR)))
      (if (and rr (not (or (= rr -1) (= rr 0))))
        (glgui-widget-set! gui val 'label (number->string rr))))
    
    (glgui-event gui t x y))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof
