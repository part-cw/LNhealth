#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2017, University of British Columbia
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

;; ShareVitalSigns module
(c-declare  #<<end-of-c-declare

#ifdef ANDROID
  void android_passVitalSign(float value, int qual, int sign);
  void android_passVitalSignString(char* str, int qual, int sign);
  void android_finishVitalSign(void);
  int android_getVitalSign(void);
  int android_getState(void);
  void android_showConfirmationDialog(char* msg_message, char* msg_ok, char* msg_cancel);
  void android_registerVitalSign(int sign);
  void android_requestVitalSign(int sign);
  void android_requestVitalSignWithState(int sign, int state);
  int android_retrieveVitalSign(int sign);
  char* android_retrieveVitalSignString(int sign);
  void android_retrieveVitalSignStringRelease(void);
#endif

void svs_pass_vitalsign(float value, int qual, int sign ){
#ifdef ANDROID
  android_passVitalSign(value,qual,sign);
#endif
}

void svs_pass_vitalsign_string(char* value, int qual, int sign ){
#ifdef ANDROID
  android_passVitalSignString(value,qual,sign);
#endif
}

void svs_confirmationdialog(char* msg_message, char* msg_ok, char* msg_cancel){
#ifdef ANDROID
 android_showConfirmationDialog(msg_message, msg_ok, msg_cancel);
#endif
}

void svs_finish(void){
#ifdef ANDROID
 android_finishVitalSign();
#endif
}

int svs_get_vitalsign(void){
#ifdef ANDROID
  return android_getVitalSign();
#endif
}

int svs_get_state(void){
#ifdef ANDROID
  return android_getState();
#endif
}

void svs_register_vitalsign(int sign){
#ifdef ANDROID
  android_registerVitalSign(sign);
#endif
}

void svs_request_vitalsign(int sign){
#ifdef ANDROID
  android_requestVitalSign(sign);
#endif
}

void svs_request_vitalsign_with_state(int sign, int state){
#ifdef ANDROID
  android_requestVitalSignWithState(sign, state);
#endif
}

int svs_retrieve_vitalsign(int sign){
#ifdef ANDROID
  return android_retrieveVitalSign(sign);
#else
  return -1;
#endif
}

char* svs_retrieve_vitalsign_string(int sign){
#ifdef ANDROID
  return android_retrieveVitalSignString(sign);
#else
  return NULL;
#endif
}

void svs_retrieve_vitalsign_string_release(){
#ifdef ANDROID
  android_retrieveVitalSignStringRelease();
#endif
}


end-of-c-declare
)

;; Vital Sign definitions. These should match the definitions of the android library
(define VITALSIGN_HR 1)
(define VITALSIGN_RR 2)
(define VITALSIGN_SPO2 4)
(define VITALSIGN_TEMP 8)
(define VITALSIGN_RRTAPS 64)
(define VITALSIGN_PO (bitwise-ior VITALSIGN_HR VITALSIGN_SPO2))
(define VITALSIGN_RRATE (bitwise-ior VITALSIGN_RR VITALSIGN_RRTAPS))

;; Vital Sign state definitions. These should match the definitions of the android library
(define VITALSIGN_STATE_NEW 1)
(define VITALSIGN_STATE_RESUME 2)

;; type definitions
(c-define-type SVS_MSG char-string)
(c-define-type SVS_OK char-string)
(c-define-type SVS_CANCEL char-string)

(c-define-type SVS_VALUE float)
(c-define-type SVS_VALUE_STRING char-string)
(c-define-type SVS_QUALITY int)
(c-define-type SVS_VITAL int)
(c-define-type SVS_SIGN int)
(c-define-type SVS_STATE int)

;; Send a result of measured vital sign to the android runtime so it can be shared to other apps
;; Example: (svs-pass-vitalsign 120 100 VITALSIGN_HR) would send a hr of 120 with 100% confidence
(define svs-pass-vitalsign (c-lambda (SVS_VALUE SVS_QUALITY SVS_VITAL) void "svs_pass_vitalsign"))

;; Send a result as a string
(define svs-pass-vitalsign-string (c-lambda (SVS_VALUE_STRING SVS_QUALITY SVS_VITAL) void "svs_pass_vitalsign_string"))

;; Open a dialog window to end measurement send vital sign and close app
;; Example: (svs-confirmationdialog "Measure finished. Send data?" "OK" "Cancel")
(define svs-confirmationdialog (c-lambda (SVS_MSG SVS_OK SVS_CANCEL) void "svs_confirmationdialog"))

;; send vital sign and close app
(define svs-finish (c-lambda () void "svs_finish"))

;; Ask for vital sign requested
(define svs-get-vitalsign (c-lambda () int "svs_get_vitalsign"))

;; Ask for state required
(define svs-get-state (c-lambda () int "svs_get_state"))

;; Register a provided vitalsign that we can export
(define svs-register-vitalsign (c-lambda (SVS_SIGN) void "svs_register_vitalsign"))

;; Send an intent to request a given vitalsign
(define svs-request-vitalsign (c-lambda (SVS_SIGN) void "svs_request_vitalsign"))

;; Send an intent to request a given vitalsign with provider in given state
(define svs-request-vitalsign-with-state (c-lambda (SVS_SIGN SVS_STATE) void "svs_request_vitalsign_with_state"))

;; Retrieve requested vitalsign
;; Returns -1 if unsuccessful, 0 if in progress
(define svs-retrieve-vitalsign (c-lambda (SVS_SIGN) int "svs_retrieve_vitalsign"))

;; Retrieve requested string vitalsign
;; Returns #f if unsuccessful, "" if in progress
(define (svs-retrieve-vitalsign-string sign)
  (let ((str ((c-lambda (SVS_SIGN) char-string "svs_retrieve_vitalsign_string") sign)))
    ((c-lambda () void "svs_retrieve_vitalsign_string_release"))
    str))

;;eof
