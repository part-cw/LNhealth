;; belly breath johnny in the hot air!
;; Christian Leth Petersen 2012, 2014

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void rtaudio_register(void (*)(int), void (*)(float), void (*)(float*,float*));

double srate=0;

double belly_idx=0;
static FILE *fd=0;

static void start_recording(char *filename)
{
  belly_idx=0;
  fd=fopen(filename,"w");
}

static void stop_recording()
{
  FILE *tmp = fd;
  fd=0; 
  if (tmp) fclose(tmp);
}

#define OFREQ 800.

#define BUFSIZE 200
static double buf[8000*BUFSIZE];
int bufidx=0;

struct ma_filter {
  int n, idx;
  double *x,*y;
  double sx, sy, sxx, syy, sxy;
  double min,max;
  int count;
  int useminmax;
};

void ma_init(struct ma_filter *f, int n, int useminmax)
{
  f->n = n; f->idx = 0;
  f->y = (double *)malloc(n*sizeof(double));
  f->x = (double *)malloc(n*sizeof(double));
  f->count=0;
  f->min=f->max=0;
  f->useminmax=useminmax;
  f->sx=f->sy=f->sxy=f->sxx=f->syy=0;
}

void ma_input(struct ma_filter *f, double y)
{
  double x = (double)f->count;
  double y_old=(f->count<f->n?0:f->y[f->idx]);
  double x_old=(f->count<f->n?0:f->x[f->idx]);

  f->y[f->idx]=y;
  f->x[f->idx]=x;

  if (f->count>=f->n) {
    f->sy-=y_old;
    f->syy-=y_old*y_old;
    f->sx-=x_old;
    f->sxx-=x_old*x_old;
    f->sxy-=x_old*y_old;
  }

  f->sx+=x;
  f->sxx+=x*x;
  f->sy+=y;
  f->syy+=y*y;
  f->sxy+=x*y;

  f->idx++; if (f->idx==f->n) f->idx=0;
  f->count++;
  if (f->useminmax) {
    if (y_old==f->min||y_old==f->max) {
      f->min=f->max=y;
      int i;
      for (i=0;i<(f->count<f->n?f->count:f->n);i++) {
        if (f->y[i]<f->min) f->min=f->y[i];
        if (f->y[i]>f->max) f->max=f->y[i];
      }
    } else {
      if (y<f->min) f->min=y;
      if (y>f->max) f->max=y;
    }
  }
}

double ma_avg(struct ma_filter *f)
{
  if (f->count==0) return 0;
  return (f->sy/(double)(f->count<f->n?f->count:f->n));
}

double ma_avg2(struct ma_filter *f)
{
  if (f->count==0) return 0;
  return (f->syy/(double)(f->count<f->n?f->count:f->n));
}

double ma_var(struct ma_filter *f)
{
  if (f->count==0) return 0;
  double avg = ma_avg(f);
  double avg2 = ma_avg2(f);
  return avg2-avg*avg;
}

double ma_std(struct ma_filter *f)
{
  if (f->count==0) return 0;
  return sqrt(ma_var(f));
}

double hrv_alpha = 2./(10.+1.);
struct ma_filter hrv;
double hrv_val = 0;

void my_realtime_init(int samplerate) { 
  srate=(double)samplerate; 
  ma_init(&hrv,5,0);
}

double phasefilter(double v)
{
  static double pf[8000];
  double pf_alpha = 2. / (100 + 1.);
  static int idx = 0;
  int stride = (int)(8000./OFREQ+0.5);
  if (idx>=stride) idx=0;
  if (!pf[idx]) pf[idx]=v; else pf[idx] = pf_alpha * v + (1.-pf_alpha)*pf[idx];
  double res=pf[idx];
  idx++;
  return res;
}

double hr=0;
int my_state=1;
double hr_t=0;

void my_hr(double v, double t)
{
  static double hr_pos, hr_neg;
  double threshold=1e-3;
  static double prv_v=0, prv_f=0.5, prv_t_pos=0, prv_t_neg, f=0.5;
  if (!prv_v) prv_v=v;
  if (v>prv_v) f = 0.5*(f+1); else f = 0.5*f;
  if (f<0.6&&prv_f>1.-threshold) {
    double newhr;
    if (prv_t_pos) { hr_pos = 60./(t-prv_t_pos); }
    prv_t_pos=t;
  }
  if (f>0.4&&prv_f<threshold) {
    if (prv_t_neg) { 
      hr_neg = 60./(t-prv_t_neg); 
      if (hr_neg>30&&hr_neg<250&&hr_pos>30&&hr_pos<250) {
        double newhr = (hr_neg+hr_pos)/2.;
        if (!hr||fabs(newhr-hr)<10) {
          ma_input(&hrv,newhr);
          double a = 100.*ma_std(&hrv)/ma_avg(&hrv); 
          if (!hrv_val) hrv_val=a;
            else hrv_val = hrv_alpha*a + (1-hrv_alpha)*hrv_val;
          if (hrv_val>6) my_state = 0;
            else if (hrv_val>3) my_state = 1;
              else my_state = 2;
          hr = newhr;
          hr_t = t;
        }
      }
    }
    prv_t_neg=t;  
  }
  prv_v=v;
  prv_f=f;
}

#define MAX(a,b) (a>b?a:b)
#define MIN(a,b) (a<b?a:b)

static double breath_factor=0;
static double breath_cycle_t=0;

void belly_hrv(double ppg_raw, double t)
{
  static double ppg=0;
  static int needs_init=1;
  static struct ma_filter ma;
  if (needs_init) { ma_init(&ma,40,0); needs_init=0; } // 20
  double alpha = 2/(20+1.);
  double alpha2 = 2/(50+1.);
  static double ppglp=0;
  static double hr_neg=0, hr_pos=0;
  static int arm_pos=0, arm_neg=0;
  static double ppg_pos, ppg_pos_t=0, ppg_pos_t_prv=0;
  static double ppg_neg, ppg_neg_t=0, ppg_neg_t_prv=0;
  ppglp = alpha * ppg_raw + (1-alpha)*ppglp;
  double tmp_ppg = ppg_raw - ppglp;
  ppg = alpha2*tmp_ppg + (1-alpha2)*ppg;
  if (ppg>0) {
    if (arm_pos) {
      ppg_pos=0;
      if (ppg_pos_t_prv) {
        hr_pos = 60/(ppg_pos_t-ppg_pos_t_prv);
        ma_input(&ma, ppg_pos_t-ppg_pos_t_prv);
      }
      ppg_pos_t_prv=ppg_pos_t;
      if (hr_pos>30&&hr_pos<250&&hr_neg>30&&hr_neg<250&&fabs(hr_pos-hr_neg)<10) {
        double tmp = hr;
        hr = (hr_pos + hr_neg)/2;
        belly_idx+=(hr-tmp>0?1.:-1.)*breath_factor;
        if (belly_idx<0) belly_idx=0;
        hr_t = 0;
      }
      arm_pos=0;
    }
    if (ppg>ppg_pos) { ppg_pos=ppg; ppg_pos_t=t; }
    arm_neg=1;
  }
  if (ppg<0) {
    if (arm_neg) {
      ppg_neg=0;
      if (ppg_neg_t_prv) {
        hr_neg = 60/(ppg_neg_t-ppg_neg_t_prv);
        ma_input(&ma, ppg_neg_t-ppg_neg_t_prv);
      }
      ppg_neg_t_prv=ppg_neg_t;
      arm_neg=0;
    }
    if (ppg<ppg_neg) { ppg_neg=ppg; ppg_neg_t=t; }
    arm_pos=1;
  }
  if (hr) {
    double tmp = 100.*ma_std(&ma)/ma_avg(&ma);
    hrv_val = (tmp>20?5:tmp);
    if (hrv_val>9) my_state = 0;
      else if (hrv_val>4) my_state = 1;
        else my_state = 2;
  }
}

#define BREATH_IN	0 
#define BREATH_HOLD	1
#define BREATH_OUT	2

static double breath_duration[3]={3.,3.,3.};
static int breath_state=BREATH_IN;
static double breath_t=0;

static void update_breath(double t)
{
  double delta = t-breath_cycle_t;
  breath_factor=(delta>1.5&&delta<4.5?1:-1);
  if (t-breath_t>breath_duration[breath_state]) {
     breath_t=t;
     breath_state++;
     if (breath_state==3) { breath_cycle_t=t; breath_state=0; }
  }
} 

void my_realtime_input(float v) 
{
  static int n;
  static double prv_t, prv_r, prv_r1;
  static int arm=0;
  static double t=0;
  static double m1, m2, r1, ppg;
  double s = sin(2*M_PI*OFREQ*t);
  double c = cos(2*M_PI*OFREQ*t);
  double alpha = 2. / ( 200. + 1. );
  double alpha2 = 2. / ( 8000. + 1. );
//  double alpha3 = 2. / ( 10. + 1. );
  double alpha3 = 2. / ( 10. + 1. ); // 1000
  // double vd = phasefilter((double)v);
  double vd = (double)v;
  if (!m1) m1=vd * s; else m1 = alpha*(vd * s)+(1-alpha)*m1;
  if (!m2) m2=vd * c; else m2 = alpha*(vd * s)+(1-alpha)*m2;
  double r = sqrt(m1*m1+m2*m2);

  if (!r1) r1=r; else r1 = alpha2*r+(1-alpha2)*r1;
  if (r<prv_r) { 
     if (arm>1) {
        if (!ppg) ppg=prv_r-prv_r1; else ppg = alpha3*(prv_r-prv_r1)+(1-alpha3)*ppg;
     } 
     arm=0;
  }
  if (r>prv_r) arm++;
  if (++n>=100) {
    if (fd) fprintf(fd,"%e %e %f %f %f %i\n",t,ppg,hr, hrv_val, belly_idx, breath_state);
//    my_hr(ppg,t);
    belly_hrv(ppg,t);
    n=0;
  }
//  if (t-hr_t>3.) { my_state=1; hr=0; hrv_val=0; }
  prv_r=r;
  prv_t=t;
  prv_r1=r1;
  update_breath(t);
  t+=1/srate;
}

void my_realtime_output(float *v1,float *v2)
{
  float buffer;
  static double t=0;
  buffer = (float)0.95*sin(2*M_PI*OFREQ*t);
  if (buffer>0.5) {
    *v1=buffer;
    *v2=-buffer;
  } else {
    *v1=*v2=0;
  }
  t+=1/srate;
}

end-of-c-declare
)

(c-initialize "rtaudio_register(my_realtime_init,my_realtime_input,my_realtime_output);")

(define (start-recording) ((c-lambda (char-string) void "start_recording")
   (string-append (system-directory) (system-pathseparator) (time->string (current-time) "%y%m%d-%H%M%S") ".txt")))
(define stop-recording (c-lambda () void "stop_recording"))

(define DYNSTATE (c-lambda () int "___result = my_state;"))
(define DYNHR (c-lambda () double "___result = hr;"))
(define DYNHRV (c-lambda () double "___result = hrv_val;"))
(define DYNBREATH (c-lambda () int "___result = breath_state;"))
(define DYNBELLY (c-lambda () double "___result = belly_idx;"))

(define BREATH_IN ((c-lambda () int "___result=BREATH_IN;")))
(define BREATH_HOLD ((c-lambda () int "___result=BREATH_HOLD;")))
(define BREATH_OUT ((c-lambda () int "___result=BREATH_OUT;")))

(define GOING_UP 0)
(define GOING_NOWHERE 1)
(define GOING_DOWN 2) 

(define SPEED_UP 0.5)
(define SPEED_DOWN -0.5)
(define SPEED_NOWHERE 0.)

(define speed 0.)

(define gui #f)

(define color_cloud (color-fade White 0.5))
(define color_gondola White)

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
;; gondola

(define gondola #f)

(define gondola_ampl 0.)

(define AMPL_UP 2.5)
(define AMPL_NOWHERE 0.)
(define AMPL_DOWN 0.)

(define (make-gondola w h)
  (let ((gw (car pixmap_gondola.img))
        (gh (cadr pixmap_gondola.img)))
  (set! gondola (glgui-sprite gui 'x (/ (- w gw) 2.) 'y (/ (- h gh) 2.) 
    'image pixmap_gondola.img 'color White))
 )) 

(define (update-gondola now state)
  (let* ((ampl_alpha (/ 2. (+ 30. 1.)))
        (newampl (+ (* ampl_alpha (cond 
          ((= state GOING_UP) AMPL_UP)
          ((= state GOING_NOWHERE) AMPL_NOWHERE)
          ((= state GOING_DOWN) AMPL_DOWN)))
             (* (- 1. ampl_alpha) gondola_ampl)))
        (angle (* newampl (sin (* 6.28 0.5 now)))))
    (set! gondola_ampl newampl)
    (glgui-widget-set! gui gondola 'angle angle)
    (glgui-widget-set! gui gondola 'color (if (< speed 0.) Gray White))
  ))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; johnny

(define johnny #f)

(define johnny:last 0.)

(define johnnies (list
  ;; going up
  (list pixmap_happy_up.img pixmap_smile_up.img pixmap_smile.img pixmap_happy.img) ;; (list pixmap_whistle_left.img -16) (list pixmap_whistle_right.img -13)
  ;; going nowhere
  (list pixmap_smile.img) ;;  (list pixmap_whistle_left.img -16) (list pixmap_whistle_right.img -13))
  ;; going down
  (list pixmap_worried.img pixmap_unhappy_down.img)
  ;; breathe in
  (list pixmap_nostrils.img)
  ;; breathe out
  (list pixmap_blow.img)
))

(define (make-johnny w h)
  (let ((jw (car pixmap_happy_up.img)) 
        (jh (cadr pixmap_happy_up.img)))
  (set! johnny  (glgui-sprite gui 'x (+ (/ (- w jw) 2.0) 10) 'y (- (/ (- h jh) 2.) 60) 'y0 (- (/ (- h jh) 2.) 60) 
     'image pixmap_happy_up.img 'color White))
))

(define (update-johnny now state)
  (if (> (- now johnny:last) 1.) 
    (let* ((idx (cond
             ((eq? breath-state 'IN) 3)
             ((eq? breath-state 'OUT) 4)
             ((> speed 0.01) 0)
             ((< speed -0.01) 2)
             (else 1)))
           (js (list-ref johnnies idx))
           (j (list-ref js (random-integer (length js))))
           (img (if (= (length j) 2) (car j) j))
           (ofs (if (= (length j) 2) (cadr j) 0))
           (y0 (glgui-widget-get gui johnny 'y0)))
      (glgui-widget-set! gui johnny 'image img)
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

(define blink #f)
(define blink:last 0.)

(define (make-blink w h)
  (let ((jw (car pixmap_happy_up.img)) 
        (jh (cadr pixmap_happy_up.img)))
    (set! blink  (glgui-sprite gui 'x (+ (/ (- w jw) 2.) 10) 'y (- (/ (- h jh) 2.) 60.) 
       'image pixmap_blink.img 'color White))))
  
(define (update-blink now)
  (let ((blinking? (not (glgui-widget-get gui blink 'hidden))))
    (cond ((and (not blinking?) (> (- now blink:last) 3.0)) 
        (glgui-widget-set! gui blink 'hidden #f) (set! blink:last now))
      ((and blinking? (> (- now blink:last) 0.1))
        (glgui-widget-set! gui blink 'hidden #t) (set! blink:last now)))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; splash menu

(define menu-mode 'MENU)

(define splash #f)

(define (make-splash w h)
  (set! splash (make-glgui))
  (let* ((pwh (* 0.9 w))
         (px (/ (- w pwh) 2.))
         (py (+ (/ (- h pwh) 2.) 225.))
         (bw 300) (bh 50)
         (bx (/ (- w bw) 2.))
         (by (- (/ (- h bh) 2.) 50.)))
    (glgui-pixmap splash px py pixmap_title.img pwh (* 0.8 pwh))
    (glgui-button-string splash bx by bw bh "Start" measure_24.fnt (lambda (x . y) 
      (glgui-set! world 'yofs 0) 
      (set! menu-mode 'FEEDBACK)
      (start-recording)))
    (set! by (- by (* 1.5 bh)))
    (glgui-button-string splash bx by bw bh "Trainer" measure_24.fnt (lambda (x . y) 
      (set! state GOING_NOWHERE)
      (glgui-set! world 'yofs 0)
      (set! menu-mode 'TEACH)
      (start-recording)))
    (set! by (- by (* 1.5 bh)))
    (glgui-button-string splash bx by bw bh "Blank Recorder" measure_24.fnt (lambda (x . y) 
      (set! menu-mode 'MEASURE)
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
  (set! height-label (glgui-label height 0 0 w 64 "1m" height_64.fnt Black))
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
  (let* ((mhr (DYNHR))
         (mhrstr (number->string (/ (fix (* 10. mhr)) 10.)))
         (mhrv (DYNBELLY))
         (mhrvstr (number->string (/ (fix (* 10. mhrv)) 10.))))
    (glgui-widget-set! measure measure-label 'label (string-append mhrstr " " mhrvstr))
 ))

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
  (set! bubble-outy (flo (+ (/ h 2.) -90.)))
  (set! bubble-inx (flo (- (/ w 2.) 0.)))
  (set! bubble-iny (flo (+ (/ h 2.) -80.)))

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
           ((fx= dynstate BREATH_OUT) 'OUT))))
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
(define instructions-label #f)

(define (make-instructions w h)
  (set! instructions (make-glgui))
  (set! instructions-label (glgui-label instructions 0 (* 0.8 h) w 64 "" height_64.fnt Black))
  (glgui-widget-set! instructions instructions-label 'align GUI_ALIGNCENTER)
)

(define (update-instructions)
  (glgui-widget-set! instructions instructions-label 'label
    (case breath-state
      ((IN) "Breathe-2-3")
      ((HOLD) "Hold-2-3")
      ((OUT) "Blow-2-3"))
    ))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; touch element

(define touch #f)

(define (make-touch w h)
  (set! touch (make-glgui))
  (let ((wgt (glgui-box touch 0 0 w h (color-fade Red 0.5))))
    (glgui-widget-set! touch wgt 'draw-handle #f)
    (glgui-widget-set! touch wgt 'callback (lambda (x . y) 
       (stop-recording)
       (set! menu-mode 'MENU) 
       (glgui-set! world 'yofs 0)
       (set! state GOING_UP)))
  ))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; main program

(define timestamp (time->seconds (current-time)))
(define state GOING_NOWHERE)

(define (drag-callback g wgt t x y)
  (let ((ox (glgui-widget-get g wgt 'offsetx))
        (oy (glgui-widget-get g wgt 'offsety)))
    (log-system "ox=" ox " oy=" oy)
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
     (make-gondola w h)
     (make-johnny w h)
     (make-blink w h)
     (make-bubbles w h)
     (make-cloud w h)
     (make-splash w h)
     (make-height w h)
     (make-measure w h)
     (make-instructions w h)
     (make-touch w h)

     (rtaudio-start 8000 1.0)
   )
 )
 (lambda (t x y)
   (let* ((now (time->seconds (current-time))))
     (update-blink now)
     (update-johnny now state)
     (update-gondola now state)
     (update-cloud state)
     (update-world now state)
     (update-height)
     (update-measure)
     (update-bubbles now)
     (update-breath now)
     (if (eq? menu-mode 'TEACH) (update-instructions))
     (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYESCAPE)) (terminate))
     (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYUP)) (set! state GOING_UP))
     (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYDOWN)) (set! state GOING_DOWN))
     (if (and (= t EVENT_KEYPRESS) (= x 32)) (set! state GOING_NOWHERE))
     (if (eq? menu-mode 'FEEDBACK)
        (let ((ypos (- (glgui-get world 'yofs)))
              (bellyidx (* 10. (DYNBELLY))))
          (set! state (cond
            ((> bellyidx ypos) GOING_UP)
            ((> bellyidx (- ypos 50)) GOING_NOWHERE)
            (else GOING_DOWN)))
     ;;  (set! state (DYNSTATE))
     ))
     (if (or (eq? menu-mode 'DEMO) (eq? menu-mode 'MENU))
       (let ((yofs (glgui-get world 'yofs)))
         (if (= yofs 0) (set! state GOING_UP))
         (if (= yofs (- world-ofs-max)) (set! state GOING_DOWN))))
     (glgui-event 
        (case menu-mode
          ((MENU)  (list world splash))
          ((TEACH) (list world gui instructions touch))
          ((DEMO) (list world gui clouds touch))
          ((FEEDBACK) (list world gui clouds height measure touch))
          ((MEASURE) (list touch))) t x y)
  ))
 (lambda () #t)
 (lambda () (glgui-suspend) (stop-recording) (terminate))
 (lambda () (glgui-resume))
)

;; eof
