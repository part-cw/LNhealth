;; belly breath johnny in the hot air!
;; Christian Leth Petersen 2012, 2014, 2015, 2016

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "LNCONFIG.h"

void rtaudio_register(void (*)(int), void (*)(float), void (*)(float*,float*));

double t_input=0;
double t_output=0;

double srate=0;
double orate=800.;

// -----------------------
// breath cycle

#define BREATH_IN	0 
#define BREATH_HOLD	1
#define BREATH_OUT	2

//static double breath_duration[4]={2.,2.,4.};
static double breath_duration[4]={3.,3.,4.};
static int breath_state=BREATH_IN;
static double breath_t=0;
static double breath_cycle_t=0;

static void update_breath(double t)
{
  double delta = t-breath_cycle_t;
  if (t-breath_t>breath_duration[breath_state]) {
     breath_t=t;
     breath_state++;
     if (breath_state==3) { breath_cycle_t=t; breath_state=0; }
  }
} 

// -----------------------
// recording

#define RECLEN 120
static char        *rec_name=0;
static unsigned int rec_size=0;
static float 	    *rec_buf=0;
static unsigned int rec_idx=0;

static void start_recording(char *filename)
{
  rec_size = (unsigned int)(RECLEN*srate*2);
  rec_buf = (float *)malloc(rec_size*sizeof(float));
  if (rec_buf) rec_name = strdup(filename);
  rec_idx=0;
  t_input = t_output = 0; 
  breath_t = breath_cycle_t=0; breath_state=0;
}

static void stop_recording()
{
  if (rec_name) {
    if (rec_idx>=rec_size/4) {
      FILE *fd=fopen(rec_name,"wb");
      fprintf(fd,"%s %s\n(RAW INPUT:FLOAT,BREATH STATE:FLOAT)\nSRATE=%06i\nORATE=%06i\n",
        SYS_APPNAME, SYS_APPVERSION, (int)srate,(int)orate);
      //fwrite(rec_buf,sizeof(float),rec_size,fd);
      fwrite(rec_buf,sizeof(float),rec_idx,fd);
      fclose(fd);
    }
    char *tmp = rec_name;
    rec_name=0;
    free(tmp);
    free(rec_buf); 
    rec_buf=0;
  }
}

// -----------------------
// real time audio

void my_realtime_init(int samplerate) { 
  srate=(double)samplerate; 
}

double POWER=0;

#define AMPL_HI 1
#define AMPL_LO 2

int ampl = AMPL_HI;

extern void belly_input(double,double,int);

void my_realtime_input(float v)
{
  if (rec_name&&rec_idx<rec_size) {
    rec_buf[rec_idx]=v;
    rec_buf[rec_idx+1]=(float)breath_state;
    rec_idx+=2;
  }
  double pwr = v*v;
  if (pwr>POWER) POWER=pwr; else POWER=pwr*1./srate + POWER*(1-1./srate);
  if (POWER<0.1) { ampl = AMPL_HI; }
  if (POWER>0.9) { ampl = AMPL_LO; }
  belly_input(t_input-breath_cycle_t,POWER,(POWER>0.85||POWER<0.005?1:0));
  update_breath(t_input);
  t_input+=1./srate;
}

void my_realtime_output(float *v1,float *v2)
{
  float buffer = (float)sin(2*M_PI*orate*t_output);
  double a=(ampl==AMPL_HI?1:0.8);
  *v1 = (buffer<0?a*0.95:a*0.70)*buffer; // red:ir
  *v2 = -*v1;
  t_output+=1./srate;
}

end-of-c-declare
)

(c-initialize "rtaudio_register(my_realtime_init,my_realtime_input,my_realtime_output);")

(define (start-recording) ((c-lambda (char-string) void "start_recording")
   (string-append (system-directory) (system-pathseparator) (time->string (current-time) "%y%m%d-%H%M%S") ".bin")))
(define stop-recording (c-lambda () void "stop_recording"))

(define DYNBREATH (c-lambda () int "___result = breath_state;"))
(define DYNDIR belly-dir)
(define DYNHEIGHT belly-height)

(define BREATH_IN ((c-lambda () int "___result=BREATH_IN;")))
(define BREATH_HOLD ((c-lambda () int "___result=BREATH_HOLD;")))
(define BREATH_OUT ((c-lambda () int "___result=BREATH_OUT;")))
;;(define BREATH_PAUSE ((c-lambda () int "___result=BREATH_PAUSE;")))

(define GOING_UP 0)
(define GOING_NOWHERE 1)
(define GOING_DOWN 2) 

(define SPEED_UP 0.5)
(define SPEED_DOWN -0.5)
(define SPEED_NOWHERE 0.)

(define speed 0.)

(define gui #f)

(define color_cloud (color-fade White 0.5))
(define color_balloon White)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; world

(define world #f)
(define world-ofs-max 0)

;; our world is four screens high
;; 1: ground level
;; 2: rainbow level
;; 3: sun level
;; 4: space level

(define (make-world w h)
  (set! world (make-glgui))
  (set! world-ofs-max (* 3 h))
  (let ((dh h))
     (glgui-box world 0 (* 0 dh) w dh (list LightCyan LightCyan White White))
     (glgui-box world 0 (* 1 dh) w dh (list LightGoldenrod LightGoldenrod LightCyan LightCyan))
     (glgui-box world 0 (* 2 dh) w dh (list RoyalBlue RoyalBlue LightGoldenrod LightGoldenrod))
     (glgui-box world 0 (* 3 dh) w dh (list Black Black RoyalBlue RoyalBlue))
     
     (glgui-pixmap world 0 0 pixmap_field.img w (/ h 5.))

     (glgui-pixmap world 0 (* 0.8 h) pixmap_rainbow.img w (/ h 2.))

     (let ((wgt (glgui-pixmap world 0 (- (* 2 h) w) pixmap_rays.img w (* 2 w))))
       (glgui-widget-set! world wgt 'color (color-fade White 0.25)))
     (glgui-pixmap world (- w (/ w 3.)) (- (* 2 h) (/ w 4.)) pixmap_sun.img (/ w 2.) (/ w 2.))

     (let ((wgt (glgui-pixmap world 0 (- (* 3 h) w) pixmap_rings.img w (* 2 w))))
       (glgui-widget-set! world wgt 'color (color-fade White 0.15)))
     (glgui-pixmap world (- (/ w 4.)) (- (* 3 h) (/ w 4.))  pixmap_moon.img (/ w 2.) (/ w 2.))

     (glgui-pixmap world 0 (- (* 4 h) (/ h 3)) pixmap_galaxy.img w (/ h 3.))

   ))

(define (update-world now state)
  (let* ((oldo (glgui-get world 'yofs))
         (speed_alpha (/ 2. (+ 30. 1.)))
         (newspeed (+ (* speed_alpha (cond  
           ((= state GOING_UP) SPEED_UP)
           ((= state GOING_NOWHERE) SPEED_NOWHERE)
           ((= state GOING_DOWN) SPEED_DOWN))) (* (- 1. speed_alpha) speed)))
         (newo (+ oldo (- newspeed)))
         (clipo (if (<= newo (- world-ofs-max)) (- world-ofs-max) 
           (if (> newo 0) 0 newo))))
    (if (not (= oldo clipo)) (glgui-set! world 'yofs clipo))
    (set! speed newspeed)
  #t
))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; clouds

(define clouds #f)

(define (make-cloud w h)
  (set! clouds (make-glgui))
;;  (glgui-sprite  clouds 'y (* 0.2 h) 'x -10 'image pixmap_cloud.img 'color (color-fade White 0.7))
  (glgui-sprite  clouds 'y (* 0.75 h) 'x (- w (car pixmap_cloud.img) -10) 'image pixmap_cloud.img 'color (color-fade White 0.7))
  (glgui-sprite  clouds 'y (* 1.25 h) 'x -10 'image pixmap_cloud.img 'color (color-fade White 0.7))
  (glgui-sprite  clouds 'y (* 1.7 h) 'x (- w (car pixmap_cloud.img) -10) 'image pixmap_cloud.img 'color (color-fade White 0.7))
  (glgui-sprite  clouds 'y (* 2.2 h) 'x -10 'image pixmap_cloud.img 'color (color-fade White 0.7))
  (glgui-sprite  clouds 'y (* 2.75 h) 'x (- w (car pixmap_cloud.img) -10) 'image pixmap_cloud.img 'color (color-fade White 0.7))
  (glgui-sprite  clouds 'y (* 3.25 h) 'x -10 'image pixmap_cloud.img 'color (color-fade White 0.7))
  (glgui-sprite  clouds 'y (* 3.7 h) 'x (- w (car pixmap_cloud.img) -10) 'image pixmap_cloud.img 'color (color-fade White 0.7))
  (glgui-sprite  clouds 'y (* 4.25 h) 'x -10 'image pixmap_cloud.img 'color (color-fade White 0.7))
  (glgui-sprite  clouds 'y (* 4.7 h) 'x (- w (car pixmap_cloud.img) -10) 'image pixmap_cloud.img 'color (color-fade White 0.7))
  (glgui-sprite  clouds 'y (* 5.25 h) 'x -10 'image pixmap_cloud.img 'color (color-fade White 0.7))
  (glgui-sprite  clouds 'y (* 5.8 h) 'x (- w (car pixmap_cloud.img) -10) 'image pixmap_cloud.img 'color (color-fade White 0.7))
  (glgui-set! clouds 'cloudcolor  (color-fade White 0.7))
)

(define (update-cloud state)
  (glgui-set! clouds 'yofs (* 2  (glgui-get world 'yofs)))
  (let ((newcolor (color-fade (if (= state 2) Gray White) 0.7))
        (oldcolor (glgui-get clouds 'cloudcolor)))
    (if (not (= newcolor oldcolor)) (begin
      (glgui-set! clouds 'cloudcolor  newcolor)
      (glgui-widget-setglobal! clouds 'color newcolor)
  ))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; balloon

(define balloon #f)

(define balloon_ampl 0.)

(define AMPL_UP 2.5)
(define AMPL_NOWHERE 0.)
(define AMPL_DOWN 0.)

(define balloons (list 
  balloon_bear.img
  balloon_frog.img
  balloon_dino.img
  balloon_robot.img
  balloon_bat.img
  balloon_skull.img
  balloon_shark.img
))

(define (select-balloon idx)
    (let* ((w (glgui-width-get)) 
           (h (glgui-height-get))
           (img (list-ref balloons idx))
           (gw (car img))
           (gh (cadr img)) 
           (x (/ (- w gw) 2.))
           (y (/ (- h gh) 2.)))
      (if balloon  (begin
        (glgui-widget-set! gui balloon 'x x)
        (glgui-widget-set! gui balloon 'y y)
        (glgui-widget-set! gui balloon 'image img)
      ) (set! balloon (glgui-sprite gui 'x x 'y y 'image img 'color White)))
 ))

(define (make-balloon w h)
  (select-balloon 3)
 ) 

(define (update-balloon now state)
  (let* ((ampl_alpha (/ 2. (+ 30. 1.)))
        (newampl (+ (* ampl_alpha (cond 
          ((= state GOING_UP) AMPL_UP)
          ((= state GOING_NOWHERE) AMPL_NOWHERE)
          ((= state GOING_DOWN) AMPL_DOWN)))
             (* (- 1. ampl_alpha) balloon_ampl)))
        (angle (* newampl (sin (* 6.28 0.5 now)))))
    (set! balloon_ampl newampl)
    (glgui-widget-set! gui balloon 'angle angle)
    (glgui-widget-set! gui balloon 'color (if (< speed 0.) Gray White))
  ))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; johnny

(define johnny #f)
(define johnny-eyes #f)
(define johnny-avatar #f)

(define johnny:last 0.)

(define johnnies (list
  ;; going up
  (list johnny-happy.img johnny-smile.img)
  ;; going nowhere
  (list johnny-happy.img) 
  ;; going down
  (list johnny-worry.img johnny-sad.img)
  ;; breathe in
  (list johnny-breathe.img)
  ;; breathe out
  (list johnny-blow.img)
))

(define (make-johnny w h)
  (let* ((jw (car johnny-happy.img)) 
         (jh (cadr johnny-happy.img))
        ;; (x (+ (/ (- w jw) 2.0) 10))
        ;; (y (- (/ (- h jh) 2.) 60))
         (x (+ (/ (- w jw) 2.0) 0))
         (y (- (/ (- h jh) 2.) 100))
        )
  (set! johnny  (glgui-sprite gui 'x x 'y y 'y0 y 'image johnny-happy.img 'color White))
  (set! johnny-eyes  (glgui-sprite gui 'x x 'y y 'y0 y 'image eyes-straight.img 'color White))
  (set! johnny-avatar  (glgui-sprite gui 'x x 'y y 'y0 y 'image avatar-nerd.img 'color White))
))

(define (update-johnny now state)
  (if (> (- now johnny:last) 1.) 
    (let* ((idx (cond
             ((eq? breath-state 'IN) 3)
             ((eq? breath-state 'OUT) 4)
             ((> speed 0.01) 0)
             ((< speed -0.01) 2)
             (else 1)))
           (eye? (> (random-real) 0.5))
           (js (list-ref johnnies idx))
           (j (list-ref js (random-integer (length js))))
           (img (if (= (length j) 2) (car j) j))
           (ofs (if (= (length j) 2) (cadr j) 0))
           (y0 (glgui-widget-get gui johnny 'y0)))
      (glgui-widget-set! gui johnny 'image img)
      (glgui-widget-set! gui johnny-eyes 'image (if (and eye? (= idx 0)) eyes-up.img (if (and eye? (= idx 2)) eyes-down.img eyes-straight.img)))
      (glgui-widget-set! gui johnny 'y (+ y0 ofs))
      (set! johnny:last now))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; stars

(define score 5)
(define stars #f)

(define (make-stars w h)
  (let* ((dx (/ w 11.)) (y0  (- h 32)) (x0 (/ dx 2.)))
    (set! stars
      (let loop ((n 0)(x x0)(res '()))
        (if (fx= n 10) res 
          (loop (fx+ n 1) (+ x dx) (append res (list
            (glgui-sprite gui 'x x 'y y0 'image pixmap_stargrey.img)))))))))

(define (update-stars)
  (let loop ((n 0)(s stars))
    (if (and (> (length s) 0)) (begin
       (glgui-widget-set! gui (car s) 'image (if (> score n) pixmap_star.img pixmap_stargrey.img))
         (loop (fx+ n 1) (cdr s))))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; blink

(define blink:last 0.)

(define (make-blink w h) #t)

(define (update-blink now)
  (let ((blinking? (glgui-widget-get gui johnny-eyes 'hidden)))
    (cond ((and (not blinking?) (> (- now blink:last) 3.0)) 
        (glgui-widget-set! gui johnny-eyes 'hidden #t) 
        (glgui-widget-set! gui splash-johnny-eyes 'hidden #t) 
        (set! blink:last now))
      ((and blinking? (> (- now blink:last) 0.1))
        (glgui-widget-set! gui johnny-eyes 'hidden #f) 
        (glgui-widget-set! gui splash-johnny-eyes 'hidden #f) 
        (set! blink:last now)))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; avatars

(define avatars (list
  (list avatar-nerd.img "Belly Brains" 3)
  (list avatar-redhead.img "Ms Belly" 0)
  (list avatar-super.img "FLASH BELLY!" 2)
  (list avatar-pigtail.img "Piggie Belly" 1)
  (list avatar-emo.img "Belly Man" 5)
  (list avatar-royal.img "Stud Belly" '(4 6))
  (list avatar-emogirl.img "Belly Grrl" 5)
 ))

(define avatar-idx 0)

(define (avatar-rotate)
  (let* ((idx (if (= avatar-idx 0) (- (length avatars) 1) (- avatar-idx 1)))
         (entry (list-ref avatars idx))
         (img (car entry))
         (txt (cadr entry))
         (balloon (caddr entry)))
    (glgui-widget-set! gui johnny-avatar 'image img)
    (glgui-widget-set! gui splash-johnny-avatar 'image img)
    (glgui-widget-set! gui splash-johnny-label 'label txt)
    (select-balloon (if (list? balloon) (list-ref balloon (random-integer (length balloon))) balloon))
    (set! avatar-idx idx)
  ))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; splash menu

(define menu-mode 'MENU)

(define splash #f)

(define splash-johnny #f)
(define splash-johnny-eyes #f)
(define splash-johnny-avatar #f)
(define splash-johnny-label #f)

(define (make-splash w h)
  (set! splash (make-glgui))
  (let* ((pwh (* 0.9 w))
         (px (/ (- w pwh) 2.))
         (py (+ (/ (- h pwh) 2.) 225.))
         (bw 300) (bh 50)
         (bx (/ (- w bw) 2.))
         (by (- (/ (- h bh) 2.) 50.)))

  (glgui-image splash 0 (- h 60 (cadr title.img)) w (cadr title.img) title.img White)

    (let* ((jw (car johnny-happy.img)) 
           (jh (cadr johnny-happy.img))
           (x (+ (/ (- w jw) 2.0) 0))
           (y (+ (/ (- h jh) 2.) 100)))
      (set! splash-johnny  (glgui-sprite splash 'x x 'y y 'y0 y 'image johnny-happy.img 'color White))
      (set! splash-johnny-eyes  (glgui-sprite splash 'x x 'y y 'y0 y 'image eyes-straight.img 'color White))
      (set! splash-johnny-avatar  (glgui-sprite splash 'x x 'y y 'y0 y 'image (car (car avatars)) 'color White))
      (set! splash-johnny-label (glgui-label splash 0 (- y 60) w 64 (cadr (car avatars)) measure_24.fnt Black))
      (glgui-widget-set! splash splash-johnny-label 'align GUI_ALIGNCENTER)
      (glgui-widget-set! splash splash-johnny-avatar 'releasecallback (lambda (x . y) (avatar-rotate)))
     )

    (glgui-button-string splash bx by bw bh "Start" measure_24.fnt (lambda (x . y) 
      (glgui-set! world 'yofs 0) 
      (set! menu-mode 'FEEDBACK)
      (belly-init)
      (reset-time)
      (start-recording)))
    (set! by (- by (* 1.5 bh)))
    (glgui-button-string splash bx by bw bh "Trainer" measure_24.fnt (lambda (x . y) 
      (set! state GOING_NOWHERE)
      (glgui-set! world 'yofs 0)
      (set! menu-mode 'TEACH)
      (reset-time)
      (start-recording)))
    (set! by (- by (* 1.5 bh)))
    (glgui-button-string splash bx by bw bh "Blank Recorder" measure_24.fnt (lambda (x . y) 
      (set! menu-mode 'MEASURE)
      (reset-time)
      (start-recording)))
    (set! by (- by (* 1.5 bh)))
    (glgui-button-string splash bx by bw bh "Demo" measure_24.fnt (lambda (x . y) 
      (glgui-set! world 'yofs 0) 
      (set! menu-mode 'DEMO)))
))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; height

(define height #f)
(define height-label #f)

(define (make-height w h)
  (set! height (make-glgui))
  ;;(set! height-label (glgui-label height 0 0 w 64 "1m" height_64.fnt Black))
  (set! height-label (glgui-label height 0 32 w 64 "1m" height_64.fnt Black))
  (glgui-widget-set! height height-label 'align GUI_ALIGNCENTER)
)

(define (update-height)
  (let* ((y (glgui-get world 'yofs))
         (idx (/ y (- world-ofs-max)))
         (hf (exp (* idx 13.815510557964275)))
         (hstr (number->string (if (< hf 100.) (/ (fix (* 10. hf)) 10.) (fix hf))))
         (hlen (string-length hstr))
         (fnlstr (string-append hstr (if (string=? (substring hstr (- hlen 1) hlen) ".") "0m" "m"))))
    (glgui-widget-set! height height-label 'label fnlstr))) 

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; measure

(define measure #f)
(define measure-label #f)

(define (make-measure w h)
  (set! measure (make-glgui))
  (set! measure-label (glgui-label measure 0 (- h 32) w 24 "" measure_24.fnt Gray))
  (glgui-widget-set! measure measure-label 'align GUI_ALIGNCENTER)
)

(define (update-measure)
#|
  (let* ((mhr (DYNHR))
         (mhrstr (number->string (/ (fix (* 10. mhr)) 10.)))
         (mhrv (DYNBELLY))
         (mhrvstr (number->string (/ (fix (* 10. mhrv)) 10.))))
    (glgui-widget-set! measure measure-label 'label (string-append mhrstr " " mhrvstr))
  )
|# 
  #t)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; bubbles

(define bubble-outx #f)
(define bubble-outy #f)
(define bubble-inx #f)
(define bubble-iny #f)

(define cm-last 0.)
(define cm-space #f)
(define cm-bubble-bodies #f)
(define cm-bubble-shapes #f)

(define bubbles #f)

(define (make-bubbles w h)

  (set! bubble-outx (flo (- (/ w 2.) 0.)))
  (set! bubble-outy (flo (+ (/ h 2.) -90. -40.)))
  (set! bubble-inx (flo (- (/ w 2.) 0.)))
  (set! bubble-iny (flo (+ (/ h 2.) -80. -40.)))

  ;; physics
  (set! cm-space (cpSpaceNew))
  (let* ((radius 1.) (mass 1.) (moment (cpMomentForCircle mass 0. radius cpvzero)))
    (let loop ((n 0)(bodies '())(shapes '()))
      (if (fx= n 10) (begin
         (set! cm-bubble-bodies bodies)
         (set! cm-bubble-shapes shapes))
       (let* ((b (cpSpaceAddBody cm-space (cpBodyNew mass moment)))
              (s (cpSpaceAddShape cm-space (cpCircleShapeNew b radius cpvzero)))
              (vx (* 100. (- (random-real) 0.5)))
              (vy (* 100. (- (random-real) 0.5))))
         (cpShapeSetFriction s 0.7)
         (cpBodySetPos b (cpv bubble-outx bubble-outy))
         (cpBodySetVel b (cpv vx vy))
         (loop (fx+ n 1) (append bodies (list b)) (append shapes (list s)))))))

  (set! bubbles (let loop ((n 0)(res '()))
    (if (fx= n 10) res (loop (fx+ n 1)
      (append res (list
        (glgui-sprite gui 'x (random-integer (fix w)) 'y (random-integer (fix h)) 'image bubble.img 'color (color-fade White 0.5))
      )))))))

;; in - 2 3
;; hold - 2 3
;; out - 2 3

(define lastbreath 0.)
(define breath-state 'OUT)

(define (reset-bubbles)
   (let loop ((bs cm-bubble-bodies)(ws bubbles))
     (if (fx> (length bs) 0)
        (let* ((b (car bs))
               (vx (* 100. (- (random-real) 0.5)))
               (vy (* 100. (- (random-real) 0.5))))
           (cpBodySetPos b (cpv bubble-outx bubble-outy))
           (cpBodySetVel b (cpv vx vy))
           (glgui-widget-set! gui (car ws) 'w 1.)
           (glgui-widget-set! gui (car ws) 'h 1.)
           (loop (cdr bs) (cdr ws))
        ))))

(define (return-bubbles)
   (let loop ((bs cm-bubble-bodies))
     (if (fx> (length bs) 0)
        (let* ((b (car bs))
               (pos (cpBodyGetPos b))
               (x (cpVect.x pos))
               (y (cpVect.y pos))
               (vx (- (- x bubble-inx)))
               (vy (- (- y bubble-iny))))
           (cpBodySetVel b (cpv (fl* 0.5 vx) (fl* 0.5 vy)))
           (loop (cdr bs))
        ))))

(define (update-breath t)
  (let* ((prvstate breath-state)
         (dynstate (DYNBREATH))
         (curstate (cond
           ((fx= dynstate BREATH_IN) 'IN)
           ((fx= dynstate BREATH_HOLD) 'HOLD)
           ((fx= dynstate BREATH_OUT) 'OUT)
         ;;   ((fx= dynstate BREATH_PAUSE) 'PAUSE)
           )))
     (if (not (eq? prvstate curstate)) (begin
       (case prvstate ((OUT) (return-bubbles)) ((HOLD) (reset-bubbles)))
       (set! breath-state curstate)
       (set! lastbreath t)
       (set! johnny:last (- t 40.)) ;; force johnny to update
   ))))

(define (update-bubbles t)
   (let loop ((ws bubbles)(bs cm-bubble-bodies)(ss cm-bubble-shapes))
      (if (fx> (length ws) 0)
        (let* ((wgt (car ws))
               (b (car bs))
               (s (car ss))
               (pos (cpBodyGetPos b))
               (x (cpVect.x pos))
               (y (cpVect.y pos))
               (vel (cpBodyGetVel b))
               (vx (cpVect.x vel))
               (vy (cpVect.y vel))
               (curd (glgui-widget-get gui wgt 'w))
               (d (flmax 1. (flmin 100. (flsqrt (fl+ 
                    (flexpt (fl- x bubble-outx) 2.) (flexpt (fl- y bubble-outy) 2.)))))))
          (if (eq? breath-state 'OUT) 
            (cpBodySetVel b (cpv (fl+ vx (fl* 50. (fl- (random-real) 0.5))) (fl+ vy (fl* 50. (fl- (random-real) 0.5))))))
          (glgui-widget-set! gui wgt 'x x)
          (glgui-widget-set! gui wgt 'y y)
          (glgui-widget-set! gui wgt 'w d)
          (glgui-widget-set! gui wgt 'h d)
          (if (eq? breath-state 'OUT) (glgui-widget-set! gui wgt 'hidden #f))
          (if (eq? breath-state 'HOLD) (glgui-widget-set! gui wgt 'hidden #t))
          (if (and (eq? breath-state 'IN) (< curd 15) (> d curd)) (glgui-widget-set! gui wgt 'hidden #t))
          (loop (cdr ws) (cdr bs) (cdr ss)))))
   (if cm-last (cpSpaceStep cm-space (- t cm-last)))
   (set! cm-last t))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; instructions

(define instructions #f)
;;(define instructions-label #f)
(define instructions-breathe #f)
(define instructions-breathe-fader 0.)
(define instructions-hold #f)
(define instructions-hold-fader 0.)
(define instructions-blow #f)
(define instructions-blow-fader 0.)

(define (make-instructions w h)
  (set! instructions (make-glgui))
 ;; (set! instructions-label (glgui-label instructions 0 (* 0.8 h) w 64 "" height_64.fnt Black))
 ;; (glgui-widget-set! instructions instructions-label 'align GUI_ALIGNCENTER)
  (set! instructions-breathe (glgui-label instructions 0 (* 0.8 h) w 64 "Breathe-2-3" height_64.fnt (color-fade Black 1.)))
  (set! instructions-hold (glgui-label instructions 0 (* 0.8 h) w 64 "Hold-2-3" height_64.fnt (color-fade Black 1.)))
  (set! instructions-blow (glgui-label instructions 0 (* 0.8 h) w 64 "Blow-2-3" height_64.fnt (color-fade Black 1.)))
  (glgui-widget-set! instructions instructions-breathe 'align GUI_ALIGNCENTER)
  (glgui-widget-set! instructions instructions-hold 'align GUI_ALIGNCENTER)
  (glgui-widget-set! instructions instructions-blow 'align GUI_ALIGNCENTER)
)

(define (update-instructions)
  (let ((factor 0.1)
        (breathe-factor (if (eq? breath-state 'IN) 1. 0.))
        (hold-factor    (if (eq? breath-state 'HOLD) 1. 0.))
        (blow-factor    (if (eq? breath-state 'OUT) 1. 0.)))
    (set! instructions-breathe-fader (+ (* factor breathe-factor) (* (- 1. factor) instructions-breathe-fader)))
    (set! instructions-hold-fader    (+ (* factor hold-factor) (* (- 1. factor) instructions-hold-fader)))
    (set! instructions-blow-fader    (+ (* factor blow-factor) (* (- 1. factor) instructions-blow-fader)))
    (glgui-widget-set! instructions instructions-breathe 'color (color-fade Black instructions-breathe-fader))
    (glgui-widget-set! instructions instructions-hold 'color (color-fade Black instructions-hold-fader))
    (glgui-widget-set! instructions instructions-blow 'color (color-fade Black instructions-blow-fader))
  ))

#|
  (glgui-widget-set! instructions instructions-label 'label
    (case breath-state
      ((IN) "Breathe-2-3")
      ((HOLD) "Hold-2-3")
      ((OUT) "Blow-2-3")
     ;; ((PAUSE) "")
    ))
|#

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; touch element

(define touch-gui #f)

(define (make-touch-gui w h)
  (set! touch-gui (make-glgui))
  (let ((wgt (glgui-box touch-gui 0 0 w h (color-fade Red 0.5))))
    (glgui-widget-set! touch-gui wgt 'draw-handle #f)
    (glgui-widget-set! touch-gui wgt 'callback (lambda (x . y) 
       (stop-recording)
       (set! menu-mode 'MENU) 
       (glgui-set! world 'yofs 0)
       (set! time-active #f)
       (set! state GOING_UP)))
  ))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; time element

(define time-duration 120.)

(define time-gui #f)
(define time-bar #f)
(define time-start 0.)
(define time-x 0.)
(define time-y 0.)
(define time-w 0.)
(define time-h 0.)
(define time-active #f)

(define (make-time-gui w h)
  (set! time-gui (make-glgui))
  (set! time-x 10.)
  (set! time-y 10.)
  (set! time-h 32.)
  (set! time-w (- w 10. 10.))
  (glgui-box time-gui time-x time-y time-w time-h (color-fade White 0.25))
  (set! time-bar (glgui-box time-gui 
    (+ time-x 2.) (+ time-y 2.) (- time-w 4.) (- time-h 4.) (color-fade Green 0.5)))
  )

(define (update-time n)
  (let* ((dt (- n time-start))
         (dur (* (if (eq? menu-mode 'MEASURE) 0.5 1.) time-duration))
         (r (min 1. (max 0. (/ dt dur)))))
    (glgui-widget-set! time-gui time-bar 'w (* r time-w))
    (if (and (= r 1.) time-active) (begin
      (stop-recording)
      (set! menu-mode 'MENU)
      (glgui-set! world 'yofs 0)
      (set! state GOING_UP)
      (set! time-active #f)
  ))))

(define (reset-time)
  (set! time-start (time->seconds (current-time)))
  (set! time-active #t))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; clock element

(define clock-gui #f)
(define clock-label #f)
(define clock-lastupdate 0.)

(define (make-clock-gui w h)
  (let* ((bw 300) (bh 50)
         (bx (/ (- w bw) 2.)))
  (set! clock-gui (make-glgui))
  (set! clock-label 
     (glgui-label clock-gui bx 24 bw bh "" measure_24.fnt Black))
  (glgui-widget-set! clock-gui clock-label 'align GUI_ALIGNCENTER)
  (glgui-widget-set! clock-gui clock-label 'bgcolor (color-fade White 0.5))
))

(define (update-clock n)
  (if (>= (fl- n clock-lastupdate) 1.0)
    (let ((tstr (seconds->string n "%H:%M:%S")))
      (glgui-widget-set! clock-gui clock-label 'label tstr)
      (set! clock-lastupdate n))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; status element

(define status-gui #f)
(define status-label #f)
(define status-lastupdate 0.)

(define (make-status-gui w h)
  (let* ((bw 300) (bh 50)
         (bx (/ (- w bw) 2.)))
  (set! status-gui (make-glgui))
  (set! status-label
     (glgui-label status-gui 0 (- h bh) w bh "" measure_24.fnt Black))
  (glgui-widget-set! status-gui status-label 'align GUI_ALIGNCENTER)
  (glgui-widget-set! status-gui status-label 'bgcolor (color-fade Red 0.5))
  (glgui-widget-set! status-gui status-label 'hidden #t)
))

(define (update-status n)
  (if (>= (fl- n status-lastupdate) 1.0)
    (let* ((pwr ((c-lambda () double "___result=POWER;")))
           (str (cond
             ((fl< pwr 0.005) "SENSOR ERROR")
             ((fl> pwr 0.85) "NO FINGER")
             (else #f))))
      (if (string? str) (begin
        (glgui-widget-set! status-gui status-label 'label str)
        (glgui-widget-set! status-gui status-label 'bgcolor (color-fade 
          (if (string=? str "SENSOR ERROR") Red Yellow) 0.5))))
      (glgui-widget-set! status-gui status-label 'hidden (not (string? str)))
      (set! status-lastupdate n)
   )))


;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; main program

(define timestamp (time->seconds (current-time)))
(define state GOING_NOWHERE)

(define (drag-callback g wgt t x y)
  (let ((ox (glgui-widget-get g wgt 'offsetx))
        (oy (glgui-widget-get g wgt 'offsety)))
;;    (log-system "ox=" ox " oy=" oy)
    (if (> oy 10) (set! state GOING_UP))
    (if (< oy -10) (set! state GOING_DOWN))
    (if (or (> ox 10) (< ox -10)) (set! state GOING_NOWHERE))
  ))

(main
 (lambda (w h)
   (make-window 480 800)
   (glgui-orientation-set! GUI_PORTRAIT)
   (set! gui (make-glgui))
   (let ((w (glgui-width-get)) (h (glgui-height-get)))
     (make-world w h)
     (make-balloon w h)
     (make-johnny w h)
     (make-blink w h)
     (make-bubbles w h)
     (make-cloud w h)
     (make-splash w h)
     (make-height w h)
     (make-measure w h)
     (make-instructions w h)
     (make-touch-gui w h)
     (make-time-gui w h)
     (make-clock-gui w h)
     (make-status-gui w h)
   )
   (let ((logdir (string-append (system-directory) "/log")))
     (if (not (file-exists? logdir)) (create-directory logdir)))
   (rtaudio-start 8000 1.0)
 ;;  (uploader-init "ecem.ece.ubc.ca" "/cgi-bin/bellybreath.cgi" "bin" 100000)
 ;;  (uploader-trigger)
 )
 (lambda (t x y)
   (let* ((now (time->seconds (current-time))))
     ;; (scheduler-iterate (lambda () #t))
     (update-blink now)
     (update-johnny now state)
     (update-balloon now state)
     (update-cloud state)
     (update-world now state)
     (update-height)
     (update-measure)
     (update-bubbles now)
     (update-breath now)
     (update-time now)
     (update-clock now)
     (update-status now)
     (if (eq? menu-mode 'TEACH) (update-instructions))
     (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYESCAPE)) (terminate))
     (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYUP)) (set! state GOING_UP))
     (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYDOWN)) (set! state GOING_DOWN))
     (if (and (= t EVENT_KEYPRESS) (= x 32)) (set! state GOING_NOWHERE))
     (if (eq? menu-mode 'FEEDBACK)
        (let ((ypos (- (glgui-get world 'yofs)))
              (newypos (* 1. (DYNHEIGHT) (- world-ofs-max)))
              (newstate (let ((d (DYNDIR))) (if (= d 1) GOING_UP (if (= d -1) GOING_DOWN GOING_NOWHERE)))))
         ;; (log-system "state=" newstate)
          (glgui-set! world 'yofs newypos)
          (set! state newstate)
     ))
     (if (or (eq? menu-mode 'DEMO) (eq? menu-mode 'MENU))
       (let ((yofs (glgui-get world 'yofs)))
         (if (= yofs 0) (set! state GOING_UP))
         (if (= yofs (- world-ofs-max)) (set! state GOING_DOWN))))
     (glgui-event 
        (case menu-mode
          ((MENU)  (list world clock-gui status-gui splash))
          ((TEACH) (list world gui instructions time-gui status-gui touch-gui))
          ((DEMO) (list world gui clouds touch-gui))
          ((FEEDBACK) (list world gui clouds height measure time-gui status-gui touch-gui))
          ((MEASURE) (list time-gui status-gui touch-gui))) t x y)
  ))
 (lambda () #t)
 (lambda () (glgui-suspend) (stop-recording) (terminate))
 (lambda () (glgui-resume))
)

;; eof
