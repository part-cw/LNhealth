(define para-key '#u8(134 102 233 27 13 87 122 120 92 13 83 192 70 93 200 53 121 102 33 85 28 157 73 219))

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
