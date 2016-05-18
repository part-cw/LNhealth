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

(include "download.scm")

(define devplatform? (or
  (string=? (system-platform) "macosx")
  (string=? (system-platform) "linux")
  (string=? (system-platform) "win32")))

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
     ,(lambda () (if (uiget 'web-sxz #f)
       '(label text "Install additional packages:" align center)
       `(button text "Get list of additional packages" action
         ,(lambda () (let ((host (dbget 'web-host "ecem.ece.ubc.ca")) (folder (dbget 'web-url "/LNhealth/")))
           (uiset 'web-sxz (download-list host folder))
           (uiset 'nodemap '())
           #f
         ))
       )
     ))
     ,(lambda () (if (uiget 'web-sxz #f)
       (let ((entries (uiget 'web-sxz '())))
         (if (> (length entries) 0)
           `(checklist id web-sxz-selected location ui default ,entries)
           '(label text "No files found")
         )
       )
       '(spacer height 0)
     ))
     ,(lambda () (if (uiget 'web-sxz #f)
       `(button text "Download selected" action
         ,(lambda () (let ((host (dbget 'web-host "ecem.ece.ubc.ca")) (folder (dbget 'web-url "/LNhealth/")) (lst (uiget 'web-sxz-selected '())))
           (for-each (lambda (l) (download-getfile host folder l)) lst)
           (uiclear 'web-sxz)
           (uiset 'nodemap '())
           #f
         ))
       )
       '(spacer height 0)
     ))
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
  (let* ((oldsxtable (glgui-widget-get lnhealth:gui lnhealth:uiform 'uiform))
         (key (if (not deftable) (string->key24 (with-input-from-file
           (string-append (system-directory) (system-pathseparator) "CURRENT")
           (lambda () (read-line)))) #f))
         (sxfile (string-append sandbox (system-pathseparator) "main.sx"))
         (sxbfile (string-append sandbox (system-pathseparator) "main.sxb"))
         (sxtable (if deftable deftable
           (if (file-exists? sxfile)
             (let ((t (list->table (eval (with-input-from-file sxfile (lambda () (read)))))))
                (table->cdb t sxbfile key) (cdb->table sxbfile key))
             (if (file-exists? sxbfile) (cdb->table sxbfile key) lnhealth:uiform-fallback)))))

    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'fnt uiformfont_18.fnt)
    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'hdfnt uiformfont_26.fnt)
    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'smlfnt uiformfont_14.fnt)
    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'bigfnt uiformfont_40.fnt)

    (let ((db (glgui-widget-get lnhealth:gui lnhealth:uiform 'database)))
      (if (not (table? db)) (glgui-widget-set! lnhealth:gui lnhealth:uiform 'database (make-table))))

    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'uiform sxtable)
    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'uuid (system-uuid))
    (glgui-widget-set! lnhealth:gui lnhealth:uiform 'sandbox sandbox)
    (lnhealth:background-update)

    (if (not lnhealth:store) (begin
      (set! lnhealth:store (make-store "main"))
      (glgui-widget-set! lnhealth:gui lnhealth:uiform 'store lnhealth:store)
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
    (let* ((t0 (sandbox-modtime))
           (t1 (apply max (map cadr (sxz-list))))
           (customloader (lambda () (sandbox-load (if (or (= t0 0) (> t1 t0)) uiform:menu #f)))))
      (lnhealth-init 480 800 customloader)
    )
  )
;; events
  (lambda (t x y) (lnhealth-events t x y (lambda () (if devplatform? (autoload)))))
;; termination
  (lambda () (lnhealth-terminate))
;; suspend
  (lambda () (lnhealth-suspend))
;; resume
  (lambda () (lnhealth-resume))
)

;; eof
