#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2024, University of British Columbia
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

;; Credentials for initial setup of the app are Username: admin, Password: DemoIGH!2024
;; main.sx must be manually transfered to the PARA/sandbox folder as it gets compiled and deleted automatically each run
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
