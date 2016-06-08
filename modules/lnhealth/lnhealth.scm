#|
lnHealth - Health related apps using the LambdaNative framework
Copyright (c) 2009-2016, University of British Columbia
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

(define lnhealth:gui #f)
(define lnhealth:background #f)
(define lnhealth:uiform #f)
(define lnhealth:store #f)
;; Hooks
(define lnhealth:onresume #f)
(define lnhealth:onsuspend #f)
(define lnhealth:ondestroy #f)
(define lnhealth:onevent #f)
(define lnhealth:onscheduler #f)

(define app:debuglevel 0)
(define (app:log level . x)
     (if (>= app:debuglevel level) (apply log-system (append (list "app: ") x))))

;; -------------
;; functions

(define (lnhealth-sxrun name . args)
  (app:log 2 "sxrun " name " " args)
  (let ((sxtable (glgui-widget-get lnhealth:gui lnhealth:uiform 'uiform)))
    (if (table? sxtable)
      (apply (car (table-ref sxtable name '(#f))) args)
      #f
    )
  ))


;; -------------
;; update background color + pixmap
(define lnhealth:default-background background.img)

(define (lnhealth:background-update)
  (let* ((uiformtable (glgui-widget-get lnhealth:gui lnhealth:uiform 'uiform))
         (sandbox (glgui-widget-get lnhealth:gui lnhealth:uiform 'sandbox))
         (bgcol (car (table-ref uiformtable 'background-color `(,DimGray))))
         (bgimgentry (car (table-ref uiformtable 'background-image '(#f))))
         (imgfile (if bgimgentry (string-append sandbox (system-pathseparator) bgimgentry) #f))
         (imgload (if (and imgfile (file-exists? imgfile)) (png->img imgfile) #f))
         (img (if imgload imgload lnhealth:default-background)))
    (glgui-widget-set! lnhealth:gui lnhealth:background 'color bgcol)
    (glgui-widget-set! lnhealth:gui lnhealth:background 'image img)
  ))

;; -------------

(define lnhealth:uiform-error (list->table `(
  (background-color ,Red)
  (main
    "ERROR"
    #f
    #f
    (spacer height 200)
    (label text "Expression Error:")
    (spacer height 20)
    (label align left wrap #t text ,(lambda () (uiget 'loaderr "??")))
    (spacer)
  )
 )))

(define lnhealth:uiform-fallback (list->table `(
  (background-color ,Red)
  (main
    "ERROR"
    #f
    #f
    (spacer)
    (label text "Failed to access payload")
    (spacer)
  )
 )))

;; -------------
(define (lnhealth:loadscript)
  (app:log 2 "sandbox-load ")
  (let* ((oldsxtable (glgui-widget-get lnhealth:gui lnhealth:uiform 'lnhealth:uiform))
         (sandbox (string-append (system-directory) (system-pathseparator) "sandbox"))
         (sxfile (string-append sandbox (system-pathseparator) "main.sx"))
         (sxbfile (string-append sandbox (system-pathseparator) "main.sxb"))
         (key (u8vector 61 1 77 214 50 161 103 142 124 112 117 125 7 152 44 175 38 220 71 37 115 173 174 19))
         (sxtable (if (file-exists? sxfile)
           (let ((t (list->table (eval (with-input-from-file sxfile (lambda () (read)))))))
             (table->cdb t sxbfile key)
             (cdb->table sxbfile key)
           )
           (if (file-exists? sxbfile)
             (cdb->table sxbfile key)
             lnhealth:uiform-fallback
           )
         )))
    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'fnt uiformfont_18.fnt)
    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'smlfnt uiformfont_14.fnt)
    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'hdfnt uiformfont_24.fnt)
    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'bigfnt uiformfont_40.fnt)

    (let ((db (glgui-widget-get lnhealth:gui lnhealth:uiform 'database)))
      (if (not (table? db))
        (glgui-widget-set! lnhealth:gui lnhealth:uiform 'database (make-table))
      )
    )
    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'uiform sxtable)
    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'uuid (system-uuid))
    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'sandbox sandbox)

    (lnhealth:background-update)

    (if (not lnhealth:store) (begin
      (set! lnhealth:store (make-store "main"))
      (glgui-widget-set! lnhealth:gui lnhealth:uiform 'store lnhealth:store)
    ))

    (if (and oldsxtable lnhealth:ondestroy) (lnhealth:ondestroy))

    ;; prepare hooks
    (set! lnhealth:oncreate (car (table-ref sxtable 'oncreate '(#f))))
    (set! lnhealth:onresume (car (table-ref sxtable 'onresume '(#f))))
    (set! lnhealth:ondestroy (car (table-ref sxtable 'ondestroy '(#f))))
    (set! lnhealth:onevent (car (table-ref sxtable 'onevent '(#f))))
    (set! lnhealth:onscheduler (car (table-ref sxtable 'onscheduler '(#f))))

    (if lnhealth:oncreate (lnhealth:oncreate))
))

(define (lnhealth-init w h . customloader)
  (make-window w h)
  (glgui-orientation-set! GUI_PORTRAIT)
  (set! lnhealth:gui (make-glgui))

  ;; create a log directory and sandbox if needed
  (let* ((dir (system-directory))
         (logdir (string-append dir (system-pathseparator) "/log"))
         (sandbox (string-append dir (system-pathseparator) "sandbox")))
    (if (not (file-exists? dir)) (create-directory dir))
    (if (not (file-exists? sandbox)) (create-directory sandbox))
    (if (not (file-exists? logdir)) (create-directory logdir))
  )

  (set! lnhealth:background (glgui-pixmap lnhealth:gui 0 0 lnhealth:default-background w h))
  (set! lnhealth:uiform (glgui-uiform lnhealth:gui 0 0 w h))

  (if (pair? customloader)
    ((car customloader))
    (lnhealth:loadscript)
  )

  (scheduler-init)
  (audiofile-init)
)
;; events
(define (lnhealth-events t x y . autoload)
  (orientation-event t x y GUI_PORTRAIT GUI_UPSIDEDOWN)
  (if lnhealth:onevent (lnhealth:onevent t x y))
  (if (= t EVENT_BATTERY) (store-set! lnhealth:store "BATTERY" x))
  (scheduler-iterate (lambda ()
    (if (pair? autoload) ((car autoload)))
    (if lnhealth:onscheduler (lnhealth:onscheduler)))
  )
  (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYESCAPE))
    (terminate)
  )
  (glgui-event lnhealth:gui t x y)
)
;; termination
(define (lnhealth-terminate)
  (scheduler-cleanup)
  (if lnhealth:ondestroy (lnhealth:ondestroy))
)
;; suspend
(define (lnhealth-suspend)
  (if lnhealth:onsuspend (lnhealth:onsuspend))
  (glgui-suspend)
)
;; resume
(define (lnhealth-resume)
  (glgui-resume)
  (if lnhealth:onresume (lnhealth:onresume))
)
;; eof
