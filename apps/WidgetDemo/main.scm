(main
;; initialization
  (lambda (w h) (lnhealth-init 480 800))
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
