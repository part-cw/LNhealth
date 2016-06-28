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

;; Attributes
(define NOM_ATTR_ALL_GROUPS 0)
(define NOM_ATTR_PT_NAME_MIDDLE   #x095F) ;;Middle Name
(define NOM_ATTR_PT_ENCOUNTER_ID  #xFFFF) ;;Encounter Id
;; attributes defined in Data Export Interface Program Guide
(define NOM_ATTR_AL_MON_P_AL_LIST   #x0902) ;;Device P-Alarm List
(define NOM_ATTR_AL_MON_T_AL_LIST   #x0904) ;;Device T-Alarm List
(define NOM_ATTR_ALTITUDE     #x090C) ;;Altitude
(define NOM_ATTR_AREA_APPL    #x090D) ;;Application Area
(define NOM_ATTR_COLOR      #x0911) ;;Color
(define NOM_ATTR_DEV_AL_COND    #x0916) ;;Device Alert Condition
(define NOM_ATTR_DISP_RES     #x0917) ;;Display Resolution
(define NOM_ATTR_GRID_VIS_I16     #x091A) ;;Visual Grid
(define NOM_ATTR_ID_ASSOC_NO    #x091D) ;;Association Invoke Id
(define NOM_ATTR_ID_BED_LABEL     #x091E) ;;Bed Label
(define NOM_ATTR_ID_HANDLE    #x0921) ;;Object Handle
(define NOM_ATTR_ID_LABEL     #x0924) ;;Label
(define NOM_ATTR_ID_LABEL_STRING  #x0927) ;;Label String
(define NOM_ATTR_ID_MODEL     #x0928) ;;System Model
(define NOM_ATTR_ID_PROD_SPECN    #x092D) ;;Product Specification
(define NOM_ATTR_ID_TYPE    #x092F) ;;Object Type
(define NOM_ATTR_LINE_FREQ    #x0935) ;;Line Frequency
(define NOM_ATTR_LOCALIZN     #x0937) ;;System Localization
(define NOM_ATTR_METRIC_INFO_LABEL  #x093C) ;;Metric Info Label
(define NOM_ATTR_METRIC_INFO_LABEL_STR  #x093D) ;;Metric Info Label String
(define NOM_ATTR_METRIC_SPECN     #x093F) ;;Metric Specification
(define NOM_ATTR_METRIC_STAT     #x0940) ;;Metric State
(define NOM_ATTR_MODE_MSMT    #x0945) ;;Measure Mode
(define NOM_ATTR_MODE_OP    #x0946) ;;Operating Mode
(define NOM_ATTR_NOM_VERS     #x0948) ;;Nomenclature Version
(define NOM_ATTR_NU_CMPD_VAL_OBS  #x094B) ;;Compound Numeric Observed Value
(define NOM_ATTR_NU_VAL_OBS     #x0950) ;;Numeric Observed Value
(define NOM_ATTR_PT_BSA     #x0956) ;;Patient BSA
(define NOM_ATTR_PT_DEMOG_ST    #x0957) ;;Pat Demo State
(define NOM_ATTR_PT_DOB     #x0958) ;;Patient Date of Birth
(define NOM_ATTR_PT_ID      #x095A) ;;Patient ID
(define NOM_ATTR_PT_NAME_FAMILY   #x095C) ;;Family Name
(define NOM_ATTR_PT_NAME_GIVEN    #x095D) ;;Given Name
(define NOM_ATTR_PT_SEX     #x0961) ;;Patient Sex
(define NOM_ATTR_PT_TYPE    #x0962) ;;Patient Type
(define NOM_ATTR_SA_CALIB_I16     #x0964) ;;Sample Array Calibration Specification
(define NOM_ATTR_SA_CMPD_VAL_OBS  #x0967) ;;Compound Sample Array Observed Value
(define NOM_ATTR_SA_RANGE_PHYS_I16  #x096A) ;;Sample Array Physiological Range
(define NOM_ATTR_SA_SPECN     #x096D) ;;Sample Array Specification
(define NOM_ATTR_SA_VAL_OBS     #x096E) ;;Sample Array Observed Value
(define NOM_ATTR_SCALE_SPECN_I16  #x096F) ;;Scale and Range Specification
(define NOM_ATTR_STD_SAFETY     #x0982) ;;Safety Standard
(define NOM_ATTR_SYS_ID     #x0984) ;;System ID
(define NOM_ATTR_SYS_SPECN    #x0985) ;;System Specification
(define NOM_ATTR_SYS_TYPE     #x0986) ;;System Type
(define NOM_ATTR_TIME_ABS     #x0987) ;;Date and Time
(define NOM_ATTR_TIME_PD_SAMP     #x098D) ;;Sample Period
(define NOM_ATTR_TIME_REL     #x098F) ;;Relative Time
(define NOM_ATTR_TIME_STAMP_ABS   #x0990) ;;Absolute Time Stamp
(define NOM_ATTR_TIME_STAMP_REL   #x0991) ;;Relative Time Stamp
(define NOM_ATTR_UNIT_CODE    #x0996) ;;Unit Code
(define NOM_ATTR_VAL_ENUM_OBS     #x099E) ;;Enumeration Observed Value
(define NOM_ATTR_VMS_MDS_STAT     #x09A7) ;;MDS Status
(define NOM_ATTR_PT_AGE     #x09D8) ;;Patient Age
(define NOM_ATTR_PT_HEIGHT    #x09DC) ;;Patient Height
(define NOM_ATTR_PT_WEIGHT    #x09DF) ;;Patient Weight
(define NOM_ATTR_SA_FIXED_VAL_SPECN   #x0A16) ;;Sample Array Fixed Values Specification
(define NOM_ATTR_PT_PACED_MODE    #x0A1E) ;;Patient Paced Mode
(define NOM_ATTR_PT_ID_INT    #xF001) ;;Internal Patient ID
(define NOM_SAT_O2_TONE_FREQ    #xF008) ;;Private Attribute
(define NOM_ATTR_CMPD_REF_LIST    #xF009) ;;Private Attribute
(define NOM_ATTR_NET_ADDR_INFO    #xF100) ;;IP Address Information
(define NOM_ATTR_PCOL_SUPPORT     #xF101) ;;Protocol Support
(define NOM_ATTR_PT_NOTES1    #xF129) ;;Notes1
(define NOM_ATTR_PT_NOTES2    #xF12A) ;;Notes2
(define NOM_ATTR_TIME_PD_POLL     #xF13E) ;;Time for Periodic Polling
(define NOM_ATTR_PT_BSA_FORMULA   #xF1EC) ;;Patient BSA Formula
(define NOM_ATTR_MDS_GEN_INFO     #xF1FA) ;;Mds General System Info
(define NOM_ATTR_POLL_OBJ_PRIO_NUM  #xF228) ;;no of prioritized objects for poll request
(define NOM_ATTR_POLL_NU_PRIO_LIST  #xF239) ;;Numeric Object Priority List
(define NOM_ATTR_POLL_RTSA_PRIO_LIST  #xF23A) ;;Wave Object Priority List
(define NOM_ATTR_METRIC_MODALITY  #xF294) ;;Metric Modality
;; The attributes are arranged in the following attribute groups:
(define NOM_ATTR_GRP_AL_MON     #x0801) ;;Alert Monitor Group
(define NOM_ATTR_GRP_METRIC_VAL_OBS   #x0803) ;;Metric Observed Value Group
(define NOM_ATTR_GRP_PT_DEMOG     #x0807) ;;Patient Demographics Attribute Group
(define NOM_ATTR_GRP_SYS_APPL     #x080A) ;;System Application Attribute Group
(define NOM_ATTR_GRP_SYS_ID     #x080B) ;;System Identification Attribute Group
(define NOM_ATTR_GRP_SYS_PROD     #x080C) ;;System Production Attribute Group
(define NOM_ATTR_GRP_VMO_DYN    #x0810) ;;VMO Dynamic Attribute Group
(define NOM_ATTR_GRP_VMO_STATIC   #x0811) ;;VMO Static Attribute Group

;; patient sex
(define SEX_UNKNOWN 0)
(define MALE 1)
(define FEMALE 2)
(define SEX_UNSPECIFIED 9)
;; patient type
(define PAT_TYPE_UNSPECIFIED 0)
(define ADULT 1)
(define PEDIATRIC 2)
(define NEONATAL 3)

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
