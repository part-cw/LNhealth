(main
;; initialization
  (lambda (w h) (lnhealth-init 480 800 (lambda ()
    (lnhealth:loadscript)
    (let ((mainfile (string-append
      (system-directory) (system-pathseparator)
      "sandbox" (system-pathseparator)
      "main.sx")))
     (if (file-exists? mainfile)
         (delete-file mainfile))))))
;; events
  (lambda (t x y) (lnhealth-events t x y))
;; termination
  (lambda () (lnhealth-terminate))
;; suspend
  (lambda () (lnhealth-suspend))
;; resume
  (lambda () (lnhealth-resume))
)
;; eof
