#|
lnHealth - Health related apps using the LambdaNative framework
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

(include "embed.scm")

(define app:debuglevel 0)
(define (app:log level . x)
   (if (>= app:debuglevel level) (apply log-system (append (list "app: ") x))))

(define devplatform? (or
  (string=? (system-platform) "macosx")
  (string=? (system-platform) "linux")
  (string=? (system-platform) "win32")))

(define gui #f)
(define background #f)
(define uiform #f)
(define store #f)

(define hook:onresume #f)
(define hook:onsuspend #f)
(define hook:ondestroy #f)
(define hook:onevent #f)
(define hook:onscheduler #f)

;; -------------
;; functions

(define (sxrun name . args)
  (app:log 2 "sxrun " name " " args)
  (let ((sxtable (glgui-widget-get gui uiform 'uiform)))
    (if (table? sxtable)
      (apply (car (table-ref sxtable name '(#f))) args) 
      #f)))

;; -------------
;; sandbox management

(define sandbox (string-append (system-directory) (system-pathseparator) "sandbox"))

(define (modtime file)
  (let* ((fileinfo (if (file-exists? file) (file-info file) #f))
         (mt (if fileinfo (time->seconds (file-info-last-modification-time fileinfo)) 0.)))
    (app:log 2 "modtime " file ": " mt)
    mt))

(define (sandbox-destroy)
  (app:log 2 "sandbox-destroy")
  (let* ((files (if (file-exists? sandbox) (directory-files sandbox) #f)))
    (if files (begin 
       (for-each (lambda (f) (delete-file (string-append sandbox (system-pathseparator) f))) files)
       (delete-directory sandbox)))))
      
(define (sandbox-create)
  (app:log 2 "sandbox-create")
  (if (not (file-exists? sandbox)) (create-directory sandbox)))

(define (sandbox-modtime)
  (app:log 2 "sandbox-modtime")
  (let ((sandboxfile (string-append sandbox (system-pathseparator) "main.sxb")))
    (if (file-exists? sandboxfile) (modtime sandboxfile) 0.)))

(define (sxz-list)
  (app:log 2 "sxz-list")
  (let ((files (directory-files (system-directory))))
    (let loop ((fs files)(res '()))
       (if (fx= (length fs) 0) res 
         (loop (cdr fs) (append res 
            (if (pregexp-match ".[sS][xX][zZ]$" (car fs))  
            (list (list (car fs) (modtime (string-append (system-directory) 
              (system-pathseparator) (car fs))))) '())))))))

(define (sxz->sandbox sxzfile)
  (app:log 2 "sxz->sandbox " sxzfile)
  (sandbox-destroy)
  (sandbox-create)
  (let ((sxzpath (string-append (system-directory) (system-pathseparator) sxzfile)))
    (with-output-to-file (string-append (system-directory) (system-pathseparator) "CURRENT")
      (lambda () (display sxzfile) (newline)))
    (let loop ((fs (zip-dir sxzpath)))
      (if (> (length fs) 0)
        (let* ((src (car fs))
               (tgt (string-append sandbox (system-pathseparator) (car (reverse (string-split src #\/))))))
          (zipentry->file sxzpath src tgt)
          (loop (cdr fs)))))))

;; -------------
;; update background color + pixmap 

;;(define default:background (list 4 4 (glCoreTextureCreate 4 4 (make-u8vector 16 #xff)) 0.1 0.1 .9 .9))
(define default:background background.img)

(define (background-update)
  (let* ((uiformtable (glgui-widget-get gui uiform 'uiform))
         (sandbox (glgui-widget-get gui uiform 'sandbox))
         (bgcol (car (table-ref uiformtable 'background-color `(,DimGray))))
         (bgimgentry (car (table-ref uiformtable 'background-image '(#f))))
         (imgfile (if bgimgentry (string-append sandbox (system-pathseparator) bgimgentry) #f))
         (imgload (if (and imgfile (file-exists? imgfile)) (png->img imgfile) #f))
         (img (if imgload imgload default:background)))
    (glgui-widget-set! gui background 'color bgcol)
    (glgui-widget-set! gui background 'image img)
  ))

;; -------------
;; automatic reload of uiform

(define lastmodtime #f)

(define (autoload)
  (let* ((filename (string-append sandbox (system-pathseparator) "main.sx"))
         (mtime (modtime filename)))
    (if (and lastmodtime mtime (> mtime lastmodtime)) (begin
      (sandbox-load #f)
      (set! lastmodtime mtime) 
 ))))

;; -------------

(define uiform:error (list->table `(
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

(define uiform:fallback (list->table `(
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

(define uiform:menu (list->table `(
   (background-color ,Orange)
   (main
     "Package Menu"
     #f
     ("Start" ,(lambda () 
        (let ((sxz (uiget 'sxzsrc #f)))
          (if (and sxz (> (length sxz) 0)) (begin
            (if (not (string=? (car sxz) "Do not reload"))
              (sxz->sandbox (car sxz)))
            (sandbox-load #f) 
            #f) '("Please select a package" ("OK" #f))))))
     (spacer)
     (label text "Select Package:" align center)
     (spacer)
     ,(lambda () 
         (let ((entries (append (map car (sxz-list)) (list "Do not reload"))))
           (if (> (length entries) 0)
           `(checklist id sxzsrc location ui default ,entries radio #t)
           '(label text "No packages found" align center))))
     (spacer)
     (spacer)
     (button text "Delete Package" action ,(lambda () 
       (let* ((sxz (uiget 'sxzsrc #f))
              (file (if (and sxz (> (length sxz) 0)) (string-append (system-directory) (system-pathseparator) (car sxz)) #f)))
         (if file `(,(string-append "Delete " (car sxz) "?") ("OK" ,(lambda () 
             (uiset 'sxzsrc '()) (delete-file file) #f)) ("Cancel" #f))
           '("Please select a package" ("OK" #f))))))
     (spacer)
     (spacer)
     (label text ,(string-append "Version " (system-appversion)) align center)
     (spacer height 10)
     (label text ,(system-buildhash) align center)
     (spacer height 10)
     (label text ,(system-builddate) align center)
     (spacer height 10)
     (label wrap #t text "Copyright (c) 2015 Pediatric Anesthesia Research Team, University of British Columbia. All rights reserved." )
   )
 )))

;; -------------

(define (string->key24 s)
  (let ((i (string=?-hash s)))
    (random-source-pseudo-randomize! default-random-source i i)
    (let ((res (random-u8vector 24)))
      (app:log 2 "string->key24 " s " " res)
      res)))

(define (sandbox-load deftable)
  (app:log 2 "sandbox-load " deftable)
  (let* ((oldsxtable (glgui-widget-get gui uiform 'uiform))
         (key (if (not deftable) (string->key24 (with-input-from-file 
           (string-append (system-directory) (system-pathseparator) "CURRENT")
           (lambda () (read-line)))) #f))
         (sxfile (string-append sandbox (system-pathseparator) "main.sx"))
         (sxbfile (string-append sandbox (system-pathseparator) "main.sxb"))
         (sxtable (if deftable deftable 
           (if (file-exists? sxfile) 
             (let ((t (list->table (eval (with-input-from-file sxfile (lambda () (read)))))))
                (table->cdb t sxbfile key) (cdb->table sxbfile key))
             (if (file-exists? sxbfile) (cdb->table sxbfile key) uiform:fallback)))))

    (glgui-widget-set! gui uiform 'fnt uiformfont_18.fnt)
    (glgui-widget-set! gui uiform 'smlfnt uiformfont_14.fnt)
    (glgui-widget-set! gui uiform 'bigfnt uiformfont_40.fnt)
   
    (let ((db (glgui-widget-get gui uiform 'database)))
      (if (not (table? db)) (glgui-widget-set! gui uiform 'database (make-table))))

    (glgui-widget-set! gui uiform 'uiform sxtable)
    (glgui-widget-set! gui uiform 'uuid (system-uuid))
    (glgui-widget-set! gui uiform 'sandbox sandbox)

    (background-update)
    
    (if (not store) (begin
      (set! store (make-store "main"))
      (glgui-widget-set! gui uiform 'store store)
    ))

    (if (and oldsxtable hook:ondestroy) (hook:ondestroy))

    ;; prepare hooks
    (set! hook:oncreate (car (table-ref sxtable 'oncreate '(#f))))
    (set! hook:onresume (car (table-ref sxtable 'onresume '(#f))))
    (set! hook:ondestroy (car (table-ref sxtable 'ondestroy '(#f))))
    (set! hook:onevent (car (table-ref sxtable 'onevent '(#f))))
    (set! hook:onscheduler (car (table-ref sxtable 'onscheduler '(#f))))

    (if hook:oncreate (hook:oncreate))
)) 

(main
;; initialization
  (lambda (w h)
    (set! lastmodtime (time->seconds (current-time)))
    (make-window 480 800)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))

    ;; create a log directory if needed
      (let* ((dir (system-directory))
             (logdir (string-append dir "/log")))
        (if (not (file-exists? dir)) (create-directory dir))
        (if (not (file-exists? logdir)) (create-directory logdir)))

    (let* ((w (glgui-width-get))
           (h (glgui-height-get)))

      (set! background (glgui-pixmap gui 0 0 default:background w h))
      (set! uiform (glgui-uiform gui 0 0 w h))

      (let* ((t0 (sandbox-modtime))
             (t1 (apply max (map cadr (sxz-list)))))
        (sandbox-load (if (or (= t0 0) (> t1 t0)) uiform:menu #f))
      )

      (scheduler-init)
  ))
;; events
  (lambda (t x y) 
     (orientation-event t x y GUI_PORTRAIT GUI_UPSIDEDOWN)
    ;;(if hook:onevent (hook:onevent))
    (scheduler-iterate (lambda ()
      (if devplatform? (autoload))
      (if hook:onscheduler (hook:onscheduler))))
    (if (= t EVENT_KEYPRESS) (begin 
      (if (= x EVENT_KEYESCAPE) (terminate))))
    (glgui-event gui t x y))
;; termination
  (lambda () (scheduler-cleanup) (if hook:ondestroy (hook:ondestroy)))
;; suspend
  (lambda () (if hook:onsuspend (hook:onsuspend)) (glgui-suspend))
;; resume
  (lambda () (glgui-resume) (if hook:onresume (hook:onresume)))
)

;; eof
