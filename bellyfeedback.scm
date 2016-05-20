;; Belly Breathing feedback engine
;; Christian Leth Petersen 2016
;; Assumes 8000 Hz input signal

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define FEEDBACK_THRESHOLD 1.1

// --------------
// pre-processing filter

#define LR_HIPASS 1
#define LR_LOPASS 0
#define BELLYARRAY_SIZE 128

struct lr_filter  {
 double a0,a1,a2,a3,a4;
 double b1,b2,b3,b4;
 double xm1,xm2,xm3,xm4;
 double ym1,ym2,ym3,ym4;
};

static void lr_init(struct lr_filter *lp, double srate, double fc,int hipass)
{
  double pi=M_PI;
  double wc=2*pi*fc;
  double wc2=wc*wc;
  double wc3=wc2*wc;
  double wc4=wc2*wc2;
  double k=wc/tan(pi*fc/srate);
  double k2=k*k;
  double k3=k2*k;
  double k4=k2*k2;
  double sqrt2=sqrt(2);
  double sq_tmp1=sqrt2*wc3*k;
  double sq_tmp2=sqrt2*wc*k3;
  double a_tmp=4*wc2*k2+2*sq_tmp1+k4+2*sq_tmp2+wc4;
  lp->b1=(4*(wc4+sq_tmp1-k4-sq_tmp2))/a_tmp;
  lp->b2=(6*wc4-8*wc2*k2+6*k4)/a_tmp;
  lp->b3=(4*(wc4-sq_tmp1+sq_tmp2-k4))/a_tmp;
  lp->b4=(k4-2*sq_tmp1+wc4-2*sq_tmp2+4*wc2*k2)/a_tmp;
  if (hipass) {
    lp->a0=k4/a_tmp;
    lp->a1=-4*k4/a_tmp;
    lp->a2=6*k4/a_tmp;
  } else {
    lp->a0=wc4/a_tmp;
    lp->a1=4*wc4/a_tmp;
    lp->a2=6*wc4/a_tmp;
  }
  lp->a3=lp->a1;
  lp->a4=lp->a0;
  lp->xm1=lp->xm2=lp->xm3=lp->xm4=0;
  lp->ym1=lp->ym2=lp->ym3=lp->ym4=0;
}

static void lr_input(struct lr_filter *lp, double val)
{
  double tempx=val;
  double tempy=lp->a0*tempx+lp->a1*lp->xm1+lp->a2*lp->xm2+lp->a3*lp->xm3+lp->a4*lp->xm4-lp->b1*lp->ym1-lp->b2*lp->ym2-lp->b3*lp->ym3-lp->b4*lp->ym4;
  lp->xm4=lp->xm3;
  lp->xm3=lp->xm2;
  lp->xm2=lp->xm1;
  lp->xm1=tempx;
  lp->ym4=lp->ym3;
  lp->ym3=lp->ym2;
  lp->ym2=lp->ym1;
  lp->ym1=tempy;
}

static double lr_output(struct lr_filter *lp)
{
  return lp->ym1;
}

// ---------------------
// function that returns the median from a sorted array

static double median(double array[], int size){
	static int i, pos, temp;

	for( i = 1; i <= size - 1; i++ ) {
		temp = array[i];
		pos = size - 1;
		while( pos >= 0 && array[pos] > temp ) {
			array[pos+1] = array[pos];
			pos--;
		}
		array[pos+1] = temp;
	}
	if( size % 2 == 0 )
		return (array[(size/2)-1] + array[size/2])/2;
	else
		return array[(size/2)+1];
}

// ---------------------
// heart rate extraction

// how close consecutive HR's need to be
#define HR_THRESHOLDA 0.75

// how close min/max values need to be
#define HR_THRESHOLDB 0.15

static double heartrate(double t, double x)
{
  static double hr=0, hr_a=0, hr_b=0, hr_1a=0,hr_1b=0;
  static double x_1=0;
  static double t_1a=0, t_1b=0;
  if (x<0&&x_1>0) {
    if (t_1a) {
      double tmp = 60./(t-t_1a);
      if (tmp>30&&tmp<250) {
         if (hr_1a&&fabs(tmp-hr_1a)/hr_1a<HR_THRESHOLDA) {
           hr_a=(tmp+hr_1a)/2;
         }
         hr_1a=tmp;
      }
    }
    t_1a=t;
  }
  if (x>0&&x_1<0) {
    if (t_1b) {
      double tmp = 60./(t-t_1b);
      if (tmp>30&&tmp<250) {
         if (hr_1b&&fabs(tmp-hr_1b)/hr_1b<HR_THRESHOLDA) {
           hr_b=(tmp+hr_1b)/2;
           if (hr_a&&hr_b&&fabs(hr_a-hr_b)/(hr_a+hr_b)<HR_THRESHOLDB) hr=(hr_a+hr_b)/2.;
         }
         hr_1b=tmp;
      }
    }
    t_1b=t;
  }
  x_1=x;
  return hr;
}

// --------------
// feedback

static double belly_t=0;
static double belly_height=0;
static double belly_setpoint=0;
//static int ratio_array_index=0;
//static double ratio_array[BELLYARRAY_SIZE];

static int FORCE_CONFORM=0; // set to 1 to simulate conformance

static void belly_feedback(double dt, double hr,int sigerr)
{
  static int arm_rsa=0;
  static double inspiration_hr=0, expiration_hr=0;
  static double inspiration_cnt=0, expiration_cnt=0;

  if (sigerr) { belly_setpoint=0; } else {
    if (dt>=2.5&&dt<=3.5) { arm_rsa=1; inspiration_hr+=hr; inspiration_cnt++; }
    if (dt>3.5&&dt<7.5&&inspiration_hr&&expiration_hr) {
      if (inspiration_cnt) { inspiration_hr/=inspiration_cnt; inspiration_cnt=0; }
      if (expiration_cnt) { expiration_hr/=expiration_cnt; expiration_cnt=0; }
      belly_setpoint+=((FORCE_CONFORM||inspiration_hr>FEEDBACK_THRESHOLD*expiration_hr)?1:0);
      if (!FORCE_CONFORM&&inspiration_hr<0.9*expiration_hr&&belly_setpoint>0) belly_setpoint-=1;
      expiration_hr=0;
    }
    if (dt>=7.5&&dt<=8.5) { expiration_hr+=hr; expiration_cnt++; }
    if (dt>8.5&&inspiration_hr&&expiration_hr) {
      if (inspiration_cnt) { inspiration_hr/=inspiration_cnt; inspiration_cnt=0; }
      if (expiration_cnt) { expiration_hr/=expiration_cnt; expiration_cnt=0; }
      belly_setpoint+=((FORCE_CONFORM||inspiration_hr>FEEDBACK_THRESHOLD*expiration_hr)?1:0);
      if (!FORCE_CONFORM&&inspiration_hr<0.9*expiration_hr&&belly_setpoint>0) belly_setpoint-=1;
      inspiration_hr=0;

/*
      if  (arm_rsa&&ratio_array_index < BELLYARRAY_SIZE) {
       //ratio_array[ratio_array_index] = expiration_hr / inspiration_hr;
        double rsa = inspiration_hr/expiration_hr;
        belly_rsa_var=0.1*rsa+0.9*belly_rsa_var;
	ratio_array[ratio_array_index] = rsa;
        ratio_array_index++;
        arm_rsa=0;
      }
*/

    }
  }
  double a= 1./(5*64);
  belly_height = a*(belly_setpoint/19.) + (1.-a)*belly_height;
  if (belly_height>1.) belly_height=1.;
}

// --------------
// api

static struct lr_filter lp,hp;

void belly_init(int hard_init)
{
  if (hard_init) {
    // ratio_array_index=0;
    lr_init(&hp,64.,0.5,LR_HIPASS);
    lr_init(&lp,64.,4.0,LR_LOPASS);
    belly_t=0;
  }
  belly_setpoint=belly_height=0;
}

void belly_input(double dt, double POWER,int sigerr)
{
  static int c=0;
  if (c++==125) {
    lr_input(&hp,POWER);
    lr_input(&lp,lr_output(&hp));
    double ppg = lr_output(&lp);
    double hr = heartrate(belly_t,ppg);
    belly_feedback(dt,hr,sigerr);
#ifdef BELLY_STANDALONE
    printf("%lf %lf %lf\n", belly_t, ppg, belly_height);
#endif
    c=0;
  }
  belly_t+=1./8000.;
}

int belly_dir()
{
  if (belly_setpoint==0&&belly_height==0) return 0;
  return (belly_setpoint>belly_height?1:-1);
}

#define MAX(a,b) (a>b?a:b)

/*
double belly_rsa()
{ 
  if (ratio_array_index>0) {
    return MAX(0.,median(ratio_array,ratio_array_index)-1.);
  } else {
    return 0;
  }
}

void belly_rsa_array(double *data, int maxlen)
{
  int i;
  for (i=0;i<(maxlen>ratio_array_index?ratio_array_index:maxlen);i++) {
    data[i]=ratio_array[i];
  }
}
*/

// --------------

#ifdef BELLY_STANDALONE

#define MOD(a,b) ((a)-((b)*floor((a)/(b))))

int main()
{
  double norm=0;
  double srate, orate;
  char buf[128];
  fgets(buf,128,stdin);
  if (strncmp(buf,"BellyBreath",10)) {
    fprintf(stderr,"ERROR: incompatible magic\n");
    exit(1);
  }
  fprintf(stderr,buf);
  fgets(buf,128,stdin);
  fprintf(stderr,buf);
  fgets(buf,128,stdin);
  fprintf(stderr,buf);
  sscanf(buf,"SRATE=%lf\n",&srate);
  fgets(buf,128,stdin);
  fprintf(stderr,buf);
  sscanf(buf,"ORATE=%lf\n",&orate);

  double POWER=0;
  belly_init(1);
  double t=0;
  while (1) {
    float buf[2];
    if (feof(stdin)) break;
    if (!fread(buf,sizeof(float),2,stdin)==2) break;
    double pwr = buf[0]*buf[0];
    if (pwr>POWER) { POWER=pwr; } else POWER=pwr*1./srate + POWER*(1-1./srate);
    belly_input(MOD(t,10.),POWER,0);
    t+=1./8000;
  }
  return 0;
}

#endif //BELLY_STANDALONE

end-of-c-declare
)

(define belly-init (c-lambda () void "belly_init(1);"))
(define belly-reinit (c-lambda () void "belly_init(0);"))
(define belly-height (c-lambda () double "___result=belly_height;"))
(define belly-dir (c-lambda () int "belly_dir"))

#|
;;(define belly-rsa (c-lambda () double "belly_rsa"))
(define belly-rsa (c-lambda () double "___result=belly_rsa_var;"))

(define (belly-rsa-array len)
  (let ((f32v (make-f32vector len)))
    ((c-lambda (scheme-object int) void "belly_rsa_array(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
       f32v len)
    f32v))
|#

;; eof
