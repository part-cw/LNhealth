;; Philips Monitor plug-in helper functions

;;
;; Lowlevel encoders
;;
(define (ivue:encodeu8 v) 
  (u8vector v))

(define (ivue:encodeu16 v) (u8vector
  (bitwise-and (arithmetic-shift v -8) #xff) 
  (bitwise-and v #xff)))

(define (ivue:encodeu32 v) (u8vector
  (bitwise-and (arithmetic-shift v -24) #xff) 
  (bitwise-and (arithmetic-shift v -16) #xff)
  (bitwise-and (arithmetic-shift v -8) #xff) 
  (bitwise-and v #xff)))

;;
;; Miscellenous encoders
;;
(define (ivue:encodeassociation buf)
  (let* ((v (u8vector-length buf))
         (asnlen (if (fx< v 128) (u8vector v)
            (u8vector #x82 (bitwise-and (arithmetic-shift v -8) #xff) 
            (bitwise-and v #xff)))))
  (u8vector-append asnlen buf)))

(define (ivue:encodesession type buf)
  (let* ((len (u8vector-length buf))
         (li (if (< len 255) (u8vector len)
             (u8vector #xff (bitwise-and (arithmetic-shift len -8) #xff)
                (bitwise-and len #xff)))))
  (u8vector-append (u8vector type) li buf)))

(define (ivue:encodeattribute id buf)
  (u8vector-append (ivue:encodeu16 id)
    (ivue:encodeu16 (u8vector-length buf)) buf))

(define (ivue:encodeattributelist . alist)
  (let* ((anum (length alist))
         (abuf (apply u8vector-append alist)))
    (ivue:encodeattribute anum abuf)))

(define (ivue:encodeSPpdu id context_id)
  (u8vector-append 
    (ivue:encodeu16 id) 
    (ivue:encodeu16 context_id)
  ))

(define (ivue:encodeROapdus type len)
  (u8vector-append 
    (ivue:encodeu16 type) 
    (ivue:encodeu16 len)
  ))

(define (ivue:encodeROIVapdu id type len)
  (u8vector-append 
    (ivue:encodeu16 id) 
    (ivue:encodeu16 type) 
    (ivue:encodeu16 len)))

(define (ivue:encodeManagedObjectId class context handle)
  (u8vector-append 
    (ivue:encodeu16 class) 
    (ivue:encodeu16 context) 
    (ivue:encodeu16 handle)
  ))

(define (ivue:encodeActionArgument scope type len)
  (u8vector-append
     (ivue:encodeu32 scope) 
     (ivue:encodeu16 type) 
     (ivue:encodeu16 len) 
   ))

(define (ivue:encodePollMdibDataReq num partition code group)
  (u8vector-append
     (ivue:encodeu16 num) 
     (ivue:encodeu16 partition) 
     (ivue:encodeu16 code) 
     (ivue:encodeu16 group) 
   ))

;;
;; frame encoders
;;

(define (ivue:encodeframe buf)
  (let ((buflen (u8vector-length buf)))
    (let loop ((i 0)(res (u8vector #xC0)))
      (if (fx= i buflen) (u8vector-append res (u8vector #xC1))
       (let ((c (u8vector-ref buf i)))
          (loop (fx+ i 1) (u8vector-append res
             (if (or (fx= c #xC0) (fx= c #xC1) (fx= c #x7D))
               (u8vector #x7D (bitwise-xor c #x20))
               (u8vector c)))))))))

(define (ivue:encodeheader buf) 
  (let ((buflen (u8vector-length buf)))
    (u8vector-append (u8vector #x11 #x01
             (bitwise-and (arithmetic-shift buflen -8) #xff)
             (bitwise-and buflen #xff)) buf)))

(define (ivue:encodecrc buf) 
  (let* ((buflen (u8vector-length buf))
        (crc (ivueparser:crc buf 0 buflen)))
    (u8vector-append buf 
       (u8vector (bitwise-and crc #xff)
        (bitwise-and (arithmetic-shift crc -8) #xff)))))

(define (ivue:encodemessage buf)
  (ivue:encodeframe (ivue:encodecrc 
        (ivue:encodeheader buf))))

;; eof
