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

(define (u8vector->u16vector u8v)
  (let* ((len (u8vector-length u8v))
         (len2 (fix (/ len 2)))
         (u16v (make-u16vector len2)))
    (let loop ((n 0) (n2 0))
      (if (fx= n len)
        u16v
        (begin
          (u16vector-set! u16v n2
            (bitwise-ior (arithmetic-shift (u8vector-ref u8v n) 8) (u8vector-ref u8v (fx+ n 1))))
          (loop (fx+ n 2) (fx+ n2 1))
        )
      )
    )
  ))


(define (u16vector->f32vector u16v)
  (let* ((len (u16vector-length u16v))
         (f32v (make-f32vector len)))
    (let loop ((n 0))
      (if (fx= n len)
        f32v
        (begin
          (f32vector-set! f32v n (flo (u16vector-ref u16v n)))
          (loop (fx+ n 1))
        )
      )
    )
  ))

(define (ivueparser:paired_u8vector->f32vector u8v)
  (let* ((len (u8vector-length u8v))
         (len2 (fix (/ len 2)))
         (f32v (make-f32vector len2)))
    (let loop ((n 0) (n2 0))
      (if (fx= n len)
        f32v
        (begin
          (f32vector-set! f32v n2
            (flo (bitwise-ior (arithmetic-shift (u8vector-ref u8v n) 8) (u8vector-ref u8v (fx+ n 1)))))
          (loop (fx+ n 2) (fx+ n2 1))
        )
      )
    )
  ))

(define (ivueparser:u8vector->string v)
  (let loop ((l (u8vector->list v)) (s '()))
    (if (fx= (length l) 0)
      (list->string (map integer->char s))
      (loop (cddr l) (if (fx<= (cadr l) 32) s (append s (list (cadr l)))))
    )
  ))
(define (ivueparser:u8vector->stringKeepSpaces v)
  (let loop ((l (u8vector->list v)) (s '()))
    (if (fx= (length l) 0)
      (list->string (map integer->char s))
      (loop (cddr l) (if (fx< (cadr l) 32) s (append s (list (cadr l)))))
    )
  ))

;; Parse Helpers
(define (ivueparser:parseGlbHandle buf)
  (let ((context_id (u8data-u16 (subu8data buf 0 2)))
        (handle (u8data-u16 (subu8data buf 2 4))))
    (list context_id handle)
  ))

(define (ivueparser:parseManagedObjectId buf)
  (let ((m_obj_class (u8data-u16 (subu8data buf 0 2)))
        ;;(m_obj_inst (ivueparser:parseGlbHandle buf))
        (context_id (u8data-u16 (subu8data buf 2 4)))
        (handle (u8data-u16 (subu8data buf 4 6))))
    (list m_obj_class context_id handle)
  ))

(define (ivueparser:parseTYPE buf)
  (let ((partition (u8data-u16 (subu8data buf 0 2)))
        (code (u8data-u16 (subu8data buf 2 4))))
    (list partition code)
  ))

(define (ivueparser:parseFLOATType val)
  (let ((exponent (u8data-ref val 0))
        (mantissa (bitwise-ior (arithmetic-shift (u8data-ref val 1) 16)
          (arithmetic-shift (u8data-ref val 2) 8) (u8data-ref val 3))))
    (if (or (fx= mantissa #x800000) (fx= mantissa #x7fffff)
            (fx= mantissa #x7ffffe) (fx= mantissa #x800002))
      #f
      (* (u8data:u24->s24 mantissa) (expt 10.0 (u8data:u8->s8 exponent)))
    )
  ))

(define (ivueparser:parseBCD val)
  (if (and (fx<= (arithmetic-shift val -4) 9)
           (fx<= (modulo val 16) 9))
    (string-append (number->string (arithmetic-shift val -4))
                   (number->string (modulo val 16)))
    #f
  ))

(define (ivueparser:parseString buf)
  (let* ((len (u8data-u16 (subu8data buf 0 2)))
         (value (u8vector->u16vector (u8data->u8vector (subu8data buf 2 (fx+ len 2))))))
    value
  ))

(define (ivueparser:parseSimpleColour buf)
  (let ((SimpleColour (u8data-u16 (subu8data buf 0 2))))
    (cond
      ((fx= SimpleColour COL_BLACK) Black)
      ((fx= SimpleColour COL_RED) Red)
      ((fx= SimpleColour COL_GREEN) Green)
      ((fx= SimpleColour COL_YELLOW) Yellow)
      ((fx= SimpleColour COL_BLUE) Blue)
      ((fx= SimpleColour COL_MAGENTA) Magenta)
      ((fx= SimpleColour COL_CYAN) Cyan)
      ((fx= SimpleColour COL_WHITE) White)
      ((fx= SimpleColour COL_PINK) Pink)
      ((fx= SimpleColour COL_ORANGE) Orange)
      ((fx= SimpleColour COL_LIGHT_GREEN) LightGreen)
      ((fx= SimpleColour COL_LIGHT_RED) (color:shuffle #xff474cff))
      (else Gray)
    )
  ))
;;eof
