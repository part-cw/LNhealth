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

;; crc check

;; crc lookup table
(define ivueparser:crctable  (u32vector
   #x0000 #x1189 #x2312 #x329b #x4624 #x57ad #x6536 #x74bf
   #x8c48 #x9dc1 #xaf5a #xbed3 #xca6c #xdbe5 #xe97e #xf8f7 #x1081 #x0108 #x3393
   #x221a #x56a5 #x472c #x75b7 #x643e #x9cc9 #x8d40 #xbfdb #xae52 #xdaed #xcb64
   #xf9ff #xe876 #x2102 #x308b #x0210 #x1399 #x6726 #x76af #x4434 #x55bd #xad4a
   #xbcc3 #x8e58 #x9fd1 #xeb6e #xfae7 #xc87c #xd9f5 #x3183 #x200a #x1291 #x0318
   #x77a7 #x662e #x54b5 #x453c #xbdcb #xac42 #x9ed9 #x8f50 #xfbef #xea66 #xd8fd
   #xc974 #x4204 #x538d #x6116 #x709f #x0420 #x15a9 #x2732 #x36bb #xce4c #xdfc5
   #xed5e #xfcd7 #x8868 #x99e1 #xab7a #xbaf3 #x5285 #x430c #x7197 #x601e #x14a1
   #x0528 #x37b3 #x263a #xdecd #xcf44 #xfddf #xec56 #x98e9 #x8960 #xbbfb #xaa72
   #x6306 #x728f #x4014 #x519d #x2522 #x34ab #x0630 #x17b9 #xef4e #xfec7 #xcc5c
   #xddd5 #xa96a #xb8e3 #x8a78 #x9bf1 #x7387 #x620e #x5095 #x411c #x35a3 #x242a
   #x16b1 #x0738 #xffcf #xee46 #xdcdd #xcd54 #xb9eb #xa862 #x9af9 #x8b70 #x8408
   #x9581 #xa71a #xb693 #xc22c #xd3a5 #xe13e #xf0b7 #x0840 #x19c9 #x2b52 #x3adb
   #x4e64 #x5fed #x6d76 #x7cff #x9489 #x8500 #xb79b #xa612 #xd2ad #xc324 #xf1bf
   #xe036 #x18c1 #x0948 #x3bd3 #x2a5a #x5ee5 #x4f6c #x7df7 #x6c7e #xa50a #xb483
   #x8618 #x9791 #xe32e #xf2a7 #xc03c #xd1b5 #x2942 #x38cb #x0a50 #x1bd9 #x6f66
   #x7eef #x4c74 #x5dfd #xb58b #xa402 #x9699 #x8710 #xf3af #xe226 #xd0bd #xc134
   #x39c3 #x284a #x1ad1 #x0b58 #x7fe7 #x6e6e #x5cf5 #x4d7c #xc60c #xd785 #xe51e
   #xf497 #x8028 #x91a1 #xa33a #xb2b3 #x4a44 #x5bcd #x6956 #x78df #x0c60 #x1de9
   #x2f72 #x3efb #xd68d #xc704 #xf59f #xe416 #x90a9 #x8120 #xb3bb #xa232 #x5ac5
   #x4b4c #x79d7 #x685e #x1ce1 #x0d68 #x3ff3 #x2e7a #xe70e #xf687 #xc41c #xd595
   #xa12a #xb0a3 #x8238 #x93b1 #x6b46 #x7acf #x4854 #x59dd #x2d62 #x3ceb #x0e70
   #x1ff9 #xf78f #xe606 #xd49d #xc514 #xb1ab #xa022 #x92b9 #x8330 #x7bc7 #x6a4e
   #x58d5 #x495c #x3de3 #x2c6a #x1ef1 #x0f78 ))

;; calculate the crc
(define (ivueparser:crc buf ofs len)
  (let loop ((i ofs)(n 0)(fcs #x0000ffff))
    (if (fx= n len)
      (bitwise-not (bitwise-and fcs #x0000FFFF))
      (let ((index (bitwise-and (bitwise-xor fcs (u8vector-ref buf i)) #x000000FF)))
        (loop (fx+ i 1) (fx+ n 1)
          (bitwise-xor (arithmetic-shift fcs -8)
          (bitwise-and (u32vector-ref ivueparser:crctable index) #x0000ffff))
        )
      )
    )
  )
)

;;eof
