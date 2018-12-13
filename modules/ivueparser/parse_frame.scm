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

(define (ivueparser:parseFrameHdr buf)
  (let ((protocol_id (u8data-u8 (subu8data buf 0 1)))
        (msg_type (u8data-u8 (subu8data buf 1 2)))
        (len (u8data-u16 (subu8data buf 2 4))))
    (if (and (fx= protocol_id #x11) (fx= msg_type #x01)
             (fx= len (fx- (u8data-length buf) 6))) ;; 4 hdr + 2 crc
      len
      (begin
        (ivueparser:log 1 "ivueparser: bad frame header")
        #f
      )
    )
  ))

(define (ivueparser:verifyFCS buf)
  (let* ((len (u8data-length buf))
         ;; The protocol specified the CRC to be send LSB first ...
         (fcs (bitwise-ior (arithmetic-shift (u8data-ref buf (fx- len 1)) 8)
              (u8data-ref buf (fx- len 2))))
         (crc (ivueparser:crc (u8data->u8vector buf) 0 (fx- len 2))))
    ;; the bitwise and's are needed to eliminate sign!
    (if (fx= (bitwise-and fcs #xffff) (bitwise-and crc #xffff))
      (bitwise-and fcs #xffff)
      (begin
        (ivueparser:log 1 "ivueparser: frame check sequence failed" fcs crc)
        #f
      )
    )
  ))

(define (ivueparser:decodeframe buf)
  (let ((len (u8data-length buf)))
    (if (and (fx= (u8data-ref buf 0) #xc0)
             (fx= (u8data-ref buf (fx- len 1))) #xc1)
      (let loop ((i 1) (obuf (u8vector)) (escaped #f))
        (if (fx= i (fx- len 1))
          (u8vector->u8data obuf)
          (let* ((c (u8data-ref buf i))
                 (newescaped (fx= c #x7d)))
            (loop (fx+ i 1) (u8vector-append obuf (if newescaped (u8vector)
              (if escaped (u8vector (bitwise-xor c #x20)) (u8vector c)))) newescaped)
           )
         )
       )
       (begin
         (ivueparser:log 2 "ivueparser: invalid BOF or EOF")
         #f
       )
     )
  ))

;; parse a raw (unvalidated) frame from the monitor
(define (ivueparser:parseframe buf)
  (let ((data (ivueparser:decodeframe buf)))
    (if (u8data:sane? data)
      (let ((len (ivueparser:parseFrameHdr data))
            (fcs (ivueparser:verifyFCS data)))
        (if (and fcs len)
          (subu8data data 4 (fx+ len 4))
          #f
        )
      )
      #f
    )
  ))
;; eof
