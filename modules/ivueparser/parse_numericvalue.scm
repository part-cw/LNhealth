#|
lnHealth - Health related apps for the LambdaNative framework
Copyright (c) 2009-2018, University of British Columbia
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

;; Helper for quick name retrieval
(define (ivueparser:getname handle_id)
  (let* ((label (table-ref ivueparser:labellut handle_id))
         (physio_id (bitwise-and label #xffff)))
    (ivueparser:findphys physio_id label)
  ))

;; Parse Compound Numeric Observed Value
(define (ivueparser:parseNuObsValueCmp handle_id buf)
  (let ((count (u8data-u16 (subu8data buf 0 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (let loop ((n 0)(p (u8data-skip buf 4)))
      (if (fx= n count)
        p
        (loop (fx+ n 1) (ivueparser:parseNuObsValue handle_id p))
      )
    )
  ))

;; Parse Numeric Observed Value
(define (ivueparser:parseNuObsValue handle_id buf)
  (let* ((physio_id (u8data-u16 (subu8data buf 0 2)))
         (state (u8data-u16 (subu8data buf 2 4)))
         (unit_code (u8data-u16 (subu8data buf 4 6)))
         (value (ivueparser:parseFLOATType (subu8data buf 6 10)))
         (name (ivueparser:findphys physio_id (table-ref ivueparser:labellut handle_id))))
    (if (and value (fx= (bitwise-and state #xf800) 0)) ;; ignore invalid data but allow demo
      (if name
        (store-set! ivueparser:store name value "ivue")
        (ivueparser:log 1 "ivueparser: failed to lookup code: " (number->string physio_id 16))
      )
      (if name (store-clear! ivueparser:store name))
    )
    (u8data-skip buf 10)
  ))

;; Display Resolution
(define (ivueparser:parseDispResolution handle_id buf)
  (let ((pre_point (u8data-u8 (subu8data buf 0 1)))
        (post_point (u8data-u8 (subu8data buf 1 2)))
        (name (ivueparser:getname handle_id)))
    (if name (store-set! ivueparser:store
      (string-append name "_resolution") (list pre_point post_point)
      "ivue_display"
    ))
  ))

;; Unit Code
(define (ivueparser:parseUnitCode handle_id buf)
  (let* ((unit_code (u8data-u16 (subu8data buf 0 2)))
         (unit (car (table-ref ivueparser:unittable unit_code '(#f))))
         (name (ivueparser:getname handle_id)))
   (if (and name unit) (store-set! ivueparser:store
     (string-append name "_unit") unit
     "ivue"
   ))
    unit_code
  ))

;; eof
