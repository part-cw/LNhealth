(unit-test "s5parser-waveforms" "Test S5 Waveform package disassembly"
  (lambda ()
    (let ((data (u8vector->u8data (u8vector 
#x88 #x02 #xe3 #x08 #xd1 #x8a #x58 #x9b #x9f #x4a #x01 #x00 #x00 #x00 #x01 #x00
#x00 #x00 #x01 #x7e #x00 #x02 #xfc #x00 #x03 #x7a #x01 #x04 #xa8 #x01 #x05 #xd6
#x01 #x06 #x04 #x02 #x07 #x32 #x02 #x08 #x3c #x00 #x92 #x2c #x02 #x00 #x4f #xff
#x52 #xff #x58 #xff #x60 #xff #x67 #xff #x6d #xff #x70 #xff #x73 #xff #x77 #xff
#x7d #xff #x86 #xff #x8e #xff #x96 #xff #x9e #xff #xa7 #xff #xb1 #xff #xbe #xff
#xcb #xff #xda #xff #xe8 #xff #xf2 #xff #xfb #xff #x04 #x00 #x0e #x00 #x1b #x00
#x29 #x00 #x37 #x00 #x43 #x00 #x4d #x00 #x52 #x00 #x56 #x00 #x5a #x00 #x5c #x00
#x5f #x00 #x61 #x00 #x62 #x00 #x62 #x00 #x64 #x00 #x66 #x00 #x6b #x00 #x6f #x00
#x73 #x00 #x75 #x00 #x75 #x00 #x6f #x00 #x62 #x00 #x51 #x00 #x39 #x00 #x1f #x00
#x01 #x00 #xe5 #xff #xc7 #xff #xac #xff #x95 #xff #x80 #xff #x6f #xff #x60 #xff
#x53 #xff #x46 #xff #x3c #xff #x3c #x00 #x9a #x2c #x00 #x00 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x3c #x00 #x9a #x2c #x00 #x00 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
#x01 #x80 #x14 #x00 #x90 #x2c #x01 #x00 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80
#x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80
#x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80
#x14 #x00 #x90 #x2c #x02 #x00 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80
#x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80
#x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x14 #x00
#x90 #x2c #x0b #x00 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80
#x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80
#x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x14 #x00 #x90 #x2c
#x03 #x00 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80
#x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80
#x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x14 #x00 #x90 #x2c #x00 #x00
#x98 #xfe #x88 #xfe #x70 #xfe #x60 #xfe #x48 #xfe #x38 #xfe #x20 #xfe #x08 #xfe
#xf8 #xfd #xe8 #xfd #xd8 #xfd #xc8 #xfd #xb8 #xfd #xa8 #xfd #x98 #xfd #x90 #xfd
#x80 #xfd #x80 #xfd #x90 #xfd #xb8 #xfd
          )))
         (store (make-store "test")))
      (s5parser store data)
      (store:waveform-dispatch store)
      (and (fx= (store-ref store "plug_id" 0) 35537)
           (fl= (car (store-ref store "ECG1" (list 0.))) -.1770000010728836)
           (fl= (car (store-ref store "PLETH" (list 0.))) -3.5999999046325684)
           (fl= (list-ref (store-ref store "PLETH" (list 0. 0. 0.)) 2) -4.)
      )
    )
  ))

(unit-test "s5parser-trends" "Test S5 Trend package disassembly"
  (lambda ()
    (let ((data (u8vector->u8data (u8vector 
 #x80 #x04 #x11 #x08 #x04 #x8b #xc3 #x9a #x9f #x4a #x02 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x01 #x16 #x01 #x01 #x2c #x02 #x01 #x42 #x03 #x01 #x58 #x04 #xff #x04
 #x00 #xff #xd0 #x9a #xa8 #x00 #x04 #x00 #xc3 #x9a #x9f #x4a #x4b #x02 #x00 #x00
 #x75 #x32 #x00 #x00 #x03 #x80 #x03 #x80 #x03 #x80 #x01 #x80 #x0b #x00 #x00 #x00
 #x01 #x00 #x84 #x03 #x5a #x00 #xe6 #x01 #x00 #x00 #x0b #x00 #x00 #x00 #x02 #x00
 #x05 #x01 #x88 #x00 #xdd #x00 #x00 #x00 #x0b #x00 #x00 #x00 #x03 #x00 #x02 #x80
 #x02 #x80 #x02 #x80 #x00 #x00 #x08 #x00 #x00 #x00 #x06 #x00 #x02 #x80 #x02 #x80
 #x02 #x80 #x01 #x80 #x03 #x00 #x00 #x00 #x0b #x01 #x01 #x80 #x01 #x80 #x01 #x80
 #x01 #x80 #x03 #x00 #x00 #x00 #x02 #x00 #x08 #x07 #x03 #x00 #x00 #x00 #x04 #x00
 #x93 #x0a #x03 #x00 #x00 #x00 #x01 #x00 #x95 #x06 #x03 #x00 #x00 #x00 #x0e #x00
 #x04 #x80 #x03 #x00 #x00 #x00 #x00 #x00 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
 #x07 #x00 #x00 #x00 #x09 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x7f #x1d #x03 #x00
 #x00 #x00 #x00 #x00 #x7f #x0b #x7f #x0b #x03 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x03 #x00 #x00 #x00 #x04 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x03 #x02
 #x00 #x00 #x01 #x00 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
 #x01 #x80 #x01 #x80 #x00 #x00 #x00 #x00 #x07 #x00 #x01 #x80 #x01 #x80 #x01 #x80
 #x01 #x80 #x23 #x00 #x00 #x00 #x00 #x00 #x01 #x80 #x01 #x80 #xff #x8d #x00 #x00
 #x01 #x80 #x01 #x80 #x20 #x00 #x00 #x00 #x01 #x80 #x01 #x80 #x09 #x00 #x00 #x00
 #x0d #x00 #x02 #x80 #x02 #x80 #x02 #x80 #x01 #x80 #x09 #x00 #x00 #x00 #x0e #x00
 #x02 #x80 #x02 #x80 #x02 #x80 #x01 #x80 #x00 #x00 #x02 #x40 #x81 #x00 #xc3 #x9a
 #x9f #x4a #x4b #x80 #x00 #x00 #x75 #x02 #x00 #x00 #x01 #x80 #x00 #x00 #x01 #x00
 #x00 #x00 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80
 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80
 #x02 #x80 #x03 #x00 #x00 #x00 #x75 #x32 #x03 #x80 #x03 #x80 #x03 #x80 #x03 #x80
 #x03 #x80 #x03 #x80 #x03 #x80 #x03 #x80 #x03 #x80 #x03 #x80 #x03 #x80 #x03 #x80
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x02 #x40 #x81 #x01 #xc3 #x9a #x9f #x4a #x03 #x00 #x00 #x00 #x01 #x80 #x01 #x80
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x02 #x80 #x02 #x80 #x02 #x80 #x02 #x80
 #x80 #x01 #x40 #x00 #x00 #x00 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x03 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x5c #x00 #x23 #x00 #x64 #x00 #x00 #x00 #x20 #x20 #x00 #x00 #x00 #x00 #x01 #x80
 #x01 #x80 #x01 #x80 #x05 #x00 #x00 #x00 #xaf #x8c #x37 #x00 #x46 #x00 #x01 #x80
 #x08 #x05 #x00 #x00 #x08 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01 #x00 #x01 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x0d #x00 #x00 #x01 #x09 #x03 #x0f #x00 #x00 #x00 #x00 #x04 #x00 #x00
 #x72 #x9b #xa8 #x00 #x02 #x97 #xa8 #x00 #x00 #x00 #x00 #x00 #xd4 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x02 #x40 #x81 #x02 #xc3 #x9a #x9f #x4a #x00 #x00
 #x00 #x00 #x03 #x00 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x03 #x00 #x00 #x00
 #x01 #x80 #x01 #x80 #x01 #x80 #x04 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80
 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x02 #x80 #x02 #x80
 #x80 #x01 #x40 #x00 #x00 #x00 #x01 #x80 #x01 #x80 #x03 #x00 #x00 #x00 #x00 #x00
 #x91 #x1b #x91 #x1b #x00 #x00 #x00 #x00 #x00 #x00 #x01 #x80 #x01 #x80 #x01 #x80
 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x01 #x80 #x03 #x00 #x00 #x00 #x04 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x02 #x40 #x81 #x03
          )))
         (store (make-store "test")))
      (s5parser store data)
      (and (fx= (store-ref store "plug_id" 0) 35588)
           (fx= (store-ref store "hr" -1) 0)
           (fl= (store-ref store "p1_sys" -1.) 9.)
           (string=? (store-ref store "p1_name" "") "ART")
           (fl= (store-ref store "o2_fi" -1.) 29.43)
           (fl= (store-ref store "bis_sqi" -1.) 92.)
           (fx= (store-ref store "timestamp" -1) 1251973827) 
           (fx= (store-ref store "marker" -1) 2)
           (string=? (store-ref store "ecg1_label" "") "II")
      )
    )
  ))
