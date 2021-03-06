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

;; subset of philips monitor constants that we care about

;; protocol id
(define ROIV_APDU 1)
(define RORS_APDU 2)
(define ROER_APDU 3)
(define ROLRS_APDU 5)

;; command types
(define CMD_EVENT_REPORT 0)
(define CMD_CONFIRMED_EVENT_REPORT 1)
(define CMD_GET 3)
(define CMD_SET 4)
(define CMD_CONFIRMED_SET 5)
(define CMD_CONFIRMED_ACTION 7)

;; action_type method identifier
(define NOM_ACT_POLL_MDIB_DATA 3094)
(define NOM_ACT_POLL_MDIB_DATA_EXT 61755)

;; field choice of structure EnumVal
(define ENUM_OBJ_ID_CHOSEN 1)
(define ENUM_OBJ_ID_VAL_CHOSEN 4)

;; field choice for MdsGenSystemInfoEntry
(define MDS_GEN_SYSTEM_INFO_SYSTEM_PULSE_CHOSEN 1)

;; Operating Mode bit field
(define OPMODE_UNSPEC #x8000)
(define MONITORING #x4000)
(define DEMO #x2000)
(define SERVICE #x1000)
(define OPMODE_STANDBY #x0002)
(define CONFIG #x0001)

;; error values
(define NO_SUCH_OBJECT_CLASS 0)
(define NO_SUCH_OBJECT_INSTANCE 1)
(define ACCESS_DENIED 2)
(define GET_LIST_ERROR 7)
(define SET_LIST_ERROR 8)
(define NO_SUCH_ACTION 9)
(define PROCESSING_FAILURE 10)
(define INVALID_ARGUMENT_VALUE 15)
(define INVALID_SCOPE 16)
(define INVALID_OBJECT_INSTANCE 17)

;; Session types
(define CN_SPDU_SI #x0D)
(define AC_SPDU_SI #x0E)
(define RF_SPDU_SI #x0C)
(define FN_SPDU_SI #x09)
(define DN_SPDU_SI #x0A)
(define AB_SPDU_SI #x19)

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

;; Object Classes
(define NOM_MOC_VMO 1)
(define NOM_MOC_VMO_METRIC_NU 6)
(define NOM_MOC_VMO_METRIC_SA_RT 9)
(define NOM_MOC_VMS_MDS 33)
(define NOM_MOC_VMS_MDS_COMPOS_SINGLE_BED 35)
(define NOM_MOC_VMS_MDS_SIMP 37)
(define NOM_MOC_BATT 41)
(define NOM_MOC_PT_DEMOG 42)
(define NOM_MOC_VMO_AL_MON 54)
(define NOM_ACT_POLL_MDIB_DATA 3094)
(define NOM_NOTI_MDS_CREAT 3334)
(define NOM_NOTI_CONN_INDIC 3351)
(define NOM_DEV_METER_CONC_SKIN_GAS 4264)
(define NOM_DEV_METER_FLOW_BLD 4284)
(define NOM_DEV_ANALY_CONC_GAS_MULTI_PARAM_MDS 4113)
(define NOM_DEV_ANALY_CONC_GAS_MULTI_PARAM_VMD 4114)
(define NOM_DEV_METER_CONC_SKIN_GAS_MDS 4265)
(define NOM_DEV_MON_PHYSIO_MULTI_PARAM_MDS 4429)
(define NOM_DEV_PUMP_INFUS_MDS 4449)
(define NOM_DEV_SYS_PT_VENT_MDS 4465)
(define NOM_DEV_SYS_PT_VENT_VMD 4466)
(define NOM_DEV_SYS_MULTI_MODAL_MDS 4493)
(define NOM_DEV_SYS_MULTI_MODAL_VMD 4494)
(define NOM_DEV_SYS_VS_CONFIG_MDS 5209)
(define NOM_DEV_SYS_VS_UNCONFIG_MDS 5213)
(define NOM_DEV_ANALY_SAT_O2_VMD 4106)
(define NOM_DEV_ANALY_CONC_GAS_MULTI_PARAM_VMD 4114)
(define NOM_DEV_ANALY_FLOW_AWAY_VMD 4130)
(define NOM_DEV_ANALY_CARD_OUTPUT_VMD 4134)
(define NOM_DEV_ANALY_PRESS_BLD_VMD 4174)
(define NOM_DEV_ANALY_RESP_RATE_VMD 4186)
(define NOM_DEV_CALC_VMD 4206)
(define NOM_DEV_ECG_VMD 4262)
(define NOM_DEV_METER_CONC_SKIN_GAS_VMD 4266)
(define NOM_DEV_EEG_VMD 4274)
(define NOM_DEV_METER_TEMP_BLD_VMD 4350)
(define NOM_DEV_METER_TEMP_VMD 4366)
(define NOM_DEV_MON_BLD_CHEM_MULTI_PARAM_VMD 4398)
(define NOM_DEV_SYS_PT_VENT_VMD 4466)
(define NOM_DEV_SYS_MULTI_MODAL_VMD 4494)
(define NOM_DEV_SYS_ANESTH_VMD 4506)
(define NOM_DEV_GENERAL_VMD 5122)
(define NOM_DEV_ECG_RESP_VMD 5130)
(define NOM_DEV_ARRHY_VMD 5134)
(define NOM_DEV_PULS_VMD 5138)
(define NOM_DEV_ST_VMD 5142)
(define NOM_DEV_CO2_VMD 5146)
(define NOM_DEV_PRESS_BLD_NONINV_VMD 5150)
(define NOM_DEV_CEREB_PERF_VMD 5154)
(define NOM_DEV_CO2_CTS_VMD 5158)
(define NOM_DEV_CO2_TCUT_VMD 5162)
(define NOM_DEV_O2_VMD 5166)
(define NOM_DEV_O2_CTS_VMD 5170)
(define NOM_DEV_O2_TCUT_VMD 5174)
(define NOM_DEV_TEMP_DIFF_VMD 5178)
(define NOM_DEV_CNTRL_VMD 5182)
(define NOM_DEV_WEDGE_VMD 5190)
(define NOM_DEV_O2_VEN_SAT_VMD 5194)
(define NOM_DEV_CARD_RATE_VMD 5202)
(define NOM_DEV_METER_PRESS_VMD 5230)
(define NOM_DEV_PLETH_VMD 5238)
(define NOM_DEV_MON_FETAL_MULTI_PARAM_MDS 61833)
(define NOM_DEV_ANALY_USOUND_VMD 61838)
(define NOM_DEV_ECG_FETAL_VMD 61842)
(define NOM_DEV_ANALY_TOCO_VMD 61846)
(define NOM_DEV_RECORDER_VMD 61862)
(define NOM_DEV_STATISTICS_VMD 61870)
(define NOM_OBJ_BATT_SENSOR 62200)
(define NOM_DEV_ANALY_OB_TI 62244)
(define NOM_DEV_ANALY_OB_TI_VMD 62246)
(define NOM_DEV_ECG_MATERNAL_VMD 62310)
(define NOM_OBJ_BATT_SENSOR_CLSPO2 62523)
(define NOM_OBJ_BATT_SENSOR_CLNBP 62524)
(define NOM_OBJ_BATT_CHARGER_CLSPO2 62526)
(define NOM_OBJ_BATT_CHARGER_CLNBP 62527)
(define NOM_OBJ_XMTR_CLSPO2 62529)
(define NOM_OBJ_XMTR_CLNBP 62530)
(define NOM_OBJ_SETTINGS_CLSPO2 62532)
(define NOM_OBJ_SETTINGS_CLNBP 62533)
(define NOM_DEV_METER_TEMP_INFRARED 62556)
(define NOM_DEV_METER_TEMP_INFRARED_MDS 62557)
(define NOM_DEV_METER_TEMP_INFRARED_VMD 62558)
(define NOM_DEV_METER_TEMP_INFRARED_CHAN 62559)
(define NOM_OBJ_OBR_IF_1 63899)
(define NOM_OBJ_OBR_CONFIG 63900)
(define NOM_OBJ_OBR_CONFIG_MDS 63901)
(define NOM_OBJ_OBR_CONFIG_VMD 63902)
(define NOM_OBJ_OBR_CONFIG_CHAN 63903)
(define NOM_OBJ_HIF_KEY 61584)
(define NOM_OBJ_DISP 61616)
(define NOM_OBJ_SOUND_GEN 61648)
(define NOM_OBJ_SETTING 61649)
(define NOM_OBJ_PRINTER 61650)
(define NOM_OBJ_EVENT 61683)
(define NOM_OBJ_BATT_CHARGER 61690)
(define NOM_OBJ_ECG_OUT 61691)
(define NOM_OBJ_INPUT_DEV 61692)
(define NOM_OBJ_NETWORK 61693)
(define NOM_OBJ_QUICKLINK 61694)
(define NOM_OBJ_SPEAKER 61695)
(define NOM_OBJ_PUMP 61716)
(define NOM_OBJ_IR 61717)
(define NOM_ACT_POLL_MDIB_DATA_EXT 61755)
(define NOM_DEV_ANALY_PULS_CONT 61800)
(define NOM_DEV_ANALY_BISPECTRAL_INDEX_VMD 61806)
(define NOM_DEV_HIRES_TREND 61820)
(define NOM_DEV_HIRES_TREND_MDS 61821)
(define NOM_DEV_HIRES_TREND_VMD 61822)
(define NOM_DEV_MON_PT_EVENT_VMD 61826)
(define NOM_DEV_DERIVED_MSMT 61828)
(define NOM_DEV_DERIVED_MSMT_MDS 61829)
(define NOM_DEV_DERIVED_MSMT_VMD 61830)
(define NOM_OBJ_SENSOR 61902)
(define NOM_OBJ_XDUCR 61903)
(define NOM_OBJ_CHAN_1 61916)
(define NOM_OBJ_CHAN_2 61917)
(define NOM_OBJ_AWAY_AGENT_1 61918)
(define NOM_OBJ_AWAY_AGENT_2 61919)
(define NOM_OBJ_HIF_MOUSE 61983)
(define NOM_OBJ_HIF_TOUCH 61984)
(define NOM_OBJ_HIF_SPEEDPOINT 61985)
(define NOM_OBJ_HIF_ALARMBOX 61986)
(define NOM_OBJ_BUS_I2C 61987)
(define NOM_OBJ_CPU_SEC 61988)
(define NOM_OBJ_LED 61990)
(define NOM_OBJ_RELAY 61991)
(define NOM_OBJ_BATT_1 61996)
(define NOM_OBJ_BATT_2 61997)
(define NOM_OBJ_DISP_SEC 61998)
(define NOM_OBJ_AGM 61999)
(define NOM_OBJ_TELEMON 62014)
(define NOM_OBJ_XMTR 62015)
(define NOM_OBJ_CABLE 62016)
(define NOM_OBJ_TELEMETRY_XMTR 62053)
(define NOM_OBJ_MMS 62070)
(define NOM_OBJ_DISP_THIRD 62073)
(define NOM_OBJ_BATT 62078)
(define NOM_OBJ_BATT_TELE 62091)
(define NOM_DEV_PROT_WATCH_CHAN 62095)
(define NOM_OBJ_PROT_WATCH_1 62097)
(define NOM_OBJ_PROT_WATCH_2 62098)
(define NOM_OBJ_PROT_WATCH_3 62099)
(define NOM_OBJ_ECG_SYNC 62147)
(define NOM_DEV_METAB_VMD 62162)
(define NOM_OBJ_SENSOR_O2_CO2 62165)
(define NOM_OBJ_SRR_IF_1 62208)
(define NOM_OBJ_DISP_REMOTE 62228)

;; misc request variables
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

;;colors
(define COL_BLACK 0)
(define COL_RED 1)
(define COL_GREEN 2)
(define COL_YELLOW 3)
(define COL_BLUE 4)
(define COL_MAGENTA 5)
(define COL_CYAN 6)
(define COL_WHITE 7)
(define COL_PINK 20)
(define COL_ORANGE 35)
(define COL_LIGHT_GREEN 50)
(define COL_LIGHT_RED 65)

;; eof
