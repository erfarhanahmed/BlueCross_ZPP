*&---------------------------------------------------------------------*
*& Report ZDMS6_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZDMS6_UPLOAD.

TABLES: ZDMS6.

DATA: IT_DATA          TYPE STANDARD TABLE OF ZDMS6,
      WA_DATA          TYPE ZDMS6,
      IT_RAW           TYPE TRUXS_T_TEXT_DATA,
      LV_FILENAME      TYPE RLGRAP-FILENAME,
      LV_CLEANED_VALUE TYPE STRING.
DATA : IT_EXCEL TYPE TABLE OF ZALSMEX_TABLINE.
*DATA : wa_excel TYPE alsmex_tabline.

PARAMETERS: P_FILE TYPE RLGRAP-FILENAME OBLIGATORY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = ' '
    IMPORTING
      FILE_NAME     = P_FILE.

START-OF-SELECTION.
  LV_FILENAME  =  P_FILE.
  BREAK CTPLABAP.
*  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = LV_FILENAME
      I_BEGIN_COL             = '1'
      I_BEGIN_ROW             = '2'
      I_END_COL               = '99'
      I_END_ROW               = '99999'
    TABLES
      INTERN                  = IT_EXCEL
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


  LOOP AT IT_EXCEL INTO DATA(WA_EXCEL).
    CASE WA_EXCEL-COL.
      WHEN 1.
        WA_DATA-MJAHR =   WA_EXCEL-VALUE.
      WHEN 2.
        WA_DATA-MBLNR  = WA_EXCEL-VALUE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT         =  WA_DATA-MBLNR
         IMPORTING
           OUTPUT        =  WA_DATA-MBLNR
                  .
      WHEN 3.
        WA_DATA-BTRTL = WA_EXCEL-VALUE  .
      WHEN 4.
        WA_DATA-UPERNR = WA_EXCEL-VALUE .
      WHEN 5.
*        lv_cleaned_value = wa_excel-value.
*        REPLACE ALL OCCURRENCES OF '-' IN lv_cleaned_value WITH space .
*        CONDENSE lv_cleaned_value.
*        wa_data-CPUDT  = lv_cleaned_value.
*        CLEAR lv_cleaned_value.
*
        IF WA_EXCEL-VALUE IS NOT INITIAL.
          CONCATENATE WA_EXCEL-VALUE+6(4) WA_EXCEL-VALUE+3(2) WA_EXCEL-VALUE+0(2) INTO WA_EXCEL-VALUE .
          WA_DATA-CPUDT = WA_EXCEL-VALUE .
        ENDIF.
      WHEN 6.
        LV_CLEANED_VALUE = WA_EXCEL-VALUE.
        REPLACE ALL OCCURRENCES OF ':' IN LV_CLEANED_VALUE WITH SPACE .
        CONDENSE LV_CLEANED_VALUE.
        WA_DATA-UZEIT  = LV_CLEANED_VALUE.
        CLEAR LV_CLEANED_VALUE.

*        IF WA_EXCEL-VALUE IS NOT INITIAL.
*          CONCATENATE WA_EXCEL-VALUE+6(4) WA_EXCEL-VALUE+3(2) WA_EXCEL-VALUE+0(2) INTO WA_EXCEL-VALUE .
*          WA_DATA-UZEIT = WA_EXCEL-VALUE .
*        ENDIF.
      WHEN 7.
        WA_DATA-UNAME  = WA_EXCEL-VALUE.
      WHEN 8.
        WA_DATA-APERNR  = WA_EXCEL-VALUE.
      WHEN 9.
         IF WA_EXCEL-VALUE IS NOT INITIAL.
          CONCATENATE WA_EXCEL-VALUE+6(4) WA_EXCEL-VALUE+3(2) WA_EXCEL-VALUE+0(2) INTO WA_EXCEL-VALUE .
          WA_DATA-ACCEPTDT = WA_EXCEL-VALUE .
        ENDIF.
*        LV_CLEANED_VALUE = WA_EXCEL-VALUE.
*        REPLACE ALL OCCURRENCES OF '-' IN LV_CLEANED_VALUE WITH SPACE .
*        CONDENSE LV_CLEANED_VALUE.
*        WA_DATA-ACCEPTDT  = LV_CLEANED_VALUE.
*        CLEAR LV_CLEANED_VALUE.
      WHEN 10.
        LV_CLEANED_VALUE = WA_EXCEL-VALUE.
        REPLACE ALL OCCURRENCES OF ':' IN LV_CLEANED_VALUE WITH SPACE .
        CONDENSE LV_CLEANED_VALUE.
        WA_DATA-ACCEPTTM  = LV_CLEANED_VALUE.
        CLEAR LV_CLEANED_VALUE.
      WHEN 11.
        WA_DATA-ACCEPTUNAME  = WA_EXCEL-VALUE.

    ENDCASE.

    AT END OF ROW.
      WA_DATA-MANDT = SY-MANDT.
      APPEND WA_DATA TO IT_DATA .
*      MODIFY zincvaldata FROM wa_data.
      CLEAR : WA_DATA.
    ENDAT.

  ENDLOOP.
  MODIFY ZDMS6 FROM TABLE IT_DATA.
  IF sy-subrc  = 0.
  MESSAGE 'Upload finished. Data inserted/updated into ZDMS6.' TYPE 'S'.
  CLEAR WA_DATA.
  COMMIT WORK.
  ENDIF.
