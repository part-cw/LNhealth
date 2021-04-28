#|
lnHealth - Health related apps for the LambdaNative framework
Copyright (c) 2009-2021, University of British Columbia
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

;; Belly Breathing feedback engine
;; Christian Leth Petersen 2016
;; Assumes 8000 Hz input signal
;; AUTOMATICALLY GENERATED, DO NOT EDIT

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

static double belly_dt=0;
static double belly_t=0;
static double belly_height=0;
static double belly_setpoint=0;

static double ihr=0, ehr=0;

static int FORCE_CONFORM=0; // set to 1 to simulate conformance

static void belly_feedback(double dt, double hr,int sigerr)
{
  static double inspiration_hr=0, expiration_hr=0;
  static double inspiration_cnt=0, expiration_cnt=0;
  if (sigerr) { belly_setpoint=0; } else {
    if (dt>=2.5&&dt<=3.5) { inspiration_hr+=hr; inspiration_cnt++; }
    if (dt>3.5&&dt<7.5&&inspiration_cnt) {
      ihr = inspiration_hr/inspiration_cnt;
      inspiration_hr = inspiration_cnt=0;
    }
    if (dt>3.5&&dt<7.5&&ihr&&ehr) {
      belly_setpoint+=((FORCE_CONFORM||ihr>FEEDBACK_THRESHOLD*ehr)?1:0);
      if (!FORCE_CONFORM&&ihr<0.9*ehr&&belly_setpoint>0) belly_setpoint-=1;
      ehr=0;
    }
    if (dt>=7.5&&dt<=8.5) { expiration_hr+=hr; expiration_cnt++; }
    if (dt>8.5&&expiration_cnt) {
      ehr = expiration_hr/expiration_cnt;
      expiration_hr=expiration_cnt=0;
    }
    if (dt>8.5&&ihr&&ehr) {
      belly_setpoint+=((FORCE_CONFORM||ihr>FEEDBACK_THRESHOLD*ehr)?1:0);
      if (!FORCE_CONFORM&&ihr<0.9*ehr&&belly_setpoint>0) belly_setpoint-=1;
      ihr=0;
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
    lr_init(&hp,64.,0.5,LR_HIPASS);
    lr_init(&lp,64.,4.0,LR_LOPASS);
    belly_t=0;
    belly_dt=0;
  }
  belly_setpoint=belly_height=0;
}

void belly_input(double dt, double POWER,int sigerr)
{
  belly_dt = dt;
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

;; eof
