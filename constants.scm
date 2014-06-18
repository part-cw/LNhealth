;; subset of philips monitor constants that we care about

;; protocol id
(define ROIV_APDU 1)
(define RORS_APDU 2)
(define ROER_APDU 3)
(define ROLRS_APDU 5)

;; command types
(define CMD_CONFIRMED_EVENT_REPORT 1)
(define CMD_CONFIRMED_ACTION 7)
(define CMD_CONFIRMED_SET 5)
(define CMD_GET 3)

;; attributes 
(define NOM_ATTR_NU_VAL_OBS 		#x0950)
(define NOM_ATTR_NU_CMPD_VAL_OBS 	#x094B)
(define NOM_ATTR_SA_VAL_OBS 		#x096E)
(define NOM_ATTR_SA_CMPD_VAL_OBS 	#x0967)
(define NOM_ATTR_SCALE_SPECN_I16 	#x096F)
(define NOM_ATTR_SA_SPECN 		2413)
(define NOM_ATTR_TIME_PD_SAMP 		#x098D)
(define NOM_ATTR_TIME_STAMP_ABS  	#x0990)
(define NOM_ATTR_TIME_STAMP_REL		#x0991)
(define NOM_ATTR_TIME_PD_POLL 		#xF13E)
(define NOM_ATTR_ID_LABEL		#x0924)
(define NOM_ATTR_METRIC_INFO_LABEL	2364)
(define NOM_ATTR_ID_BED_LABEL 		#x091E)
(define NOM_ATTR_POLL_RTSA_PRIO_LIST 62010)
(define NOM_ATTR_ALL_GROUPS 0)
(define NOM_SAT_O2_TONE_FREQ      #xF008)
(define NOM_ATTR_DEV_AL_COND      #x0916)
(define NOM_ATTR_AL_MON_P_AL_LIST #x0902)
(define NOM_ATTR_AL_MON_T_AL_LIST #x0904)

;; poll object types
(define NOM_MOC_VMO_METRIC_NU 6)
(define NOM_MOC_VMO_METRIC_SA_RT 9)
(define NOM_MOC_VMS_MDS 33)
(define NOM_MOC_VMS_T_DEMOG 42)
(define NOM_MOC_VMP_AL_MON 54)

;; misc request variables
(define NOM_ACT_POLL_MDIB_DATA 3094)
(define NOM_ACT_POLL_MDIB_DATA_EXT 61755)
(define NOM_ATTR_POLL_OBJ_PRIO_NUM #xF228)
(define NOM_PART_OBJ 1)

;; alarm related
(define NO_ALERT 0)
(define LOW_PRI_T_AL 1)
(define MED_PRI_T_AL 2)
(define HI_PRI_T_AL 4)
(define LOW_PRI_P_AL 256)
(define MED_PRI_P_AL 512)
(define HI_PRI_P_AL 1024)
(define GEN_ALMON_INFO 513)
(define STR_ALMON_INFO 516)

;; eof
