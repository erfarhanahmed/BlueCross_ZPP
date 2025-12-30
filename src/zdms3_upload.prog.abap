*&---------------------------------------------------------------------*
*& Report ZDMS3_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZDMS3_UPLOAD.

TABLES: ZDMS3.

DATA: IT_DATA          TYPE STANDARD TABLE OF ZDMS3,
      WA_DATA          TYPE ZDMS3,
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
  BREAK ctplabap.
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
        WA_DATA-DEPT = WA_EXCEL-VALUE  .
      WHEN 4.
        WA_DATA-ZEILE = WA_EXCEL-VALUE .
      WHEN 5.
         IF WA_EXCEL-VALUE IS NOT INITIAL.
          CONCATENATE WA_EXCEL-VALUE+6(4) WA_EXCEL-VALUE+3(2) WA_EXCEL-VALUE+0(2) INTO WA_EXCEL-VALUE .
          WA_DATA-BUDAT = WA_EXCEL-VALUE .
        ENDIF.

*        lv_cleaned_value = wa_excel-value.
*        REPLACE ALL OCCURRENCES OF '-' IN lv_cleaned_value WITH space .
*        CONDENSE lv_cleaned_value.
*        WA_DATA-BUDAT  = lv_cleaned_value.
*        CLEAR lv_cleaned_value.
      WHEN 6.
         lv_cleaned_value = wa_excel-value.
        REPLACE ALL OCCURRENCES OF ':' IN lv_cleaned_value WITH space .
        CONDENSE lv_cleaned_value.
        WA_DATA-UZEIT  = lv_cleaned_value.
        CLEAR lv_cleaned_value.
      WHEN 7.
        WA_DATA-UNAME  = WA_EXCEL-VALUE.
      WHEN 8.
        WA_DATA-PERNR  = WA_EXCEL-VALUE.
      WHEN 9.
        WA_DATA-ATEXT1  = WA_EXCEL-VALUE.
      WHEN 10.
         IF WA_EXCEL-VALUE IS NOT INITIAL.
          CONCATENATE WA_EXCEL-VALUE+6(4) WA_EXCEL-VALUE+3(2) WA_EXCEL-VALUE+0(2) INTO WA_EXCEL-VALUE .
          WA_DATA-TDATE1 = WA_EXCEL-VALUE .
        ENDIF.

*         lv_cleaned_value = wa_excel-value.
*        REPLACE ALL OCCURRENCES OF '-' IN lv_cleaned_value WITH space .
*        CONDENSE lv_cleaned_value.
*        WA_DATA-TDATE1  = lv_cleaned_value.
*        CLEAR lv_cleaned_value.
      WHEN 11.
        WA_DATA-REM1  = WA_EXCEL-VALUE.
      WHEN 12.
         IF WA_EXCEL-VALUE IS NOT INITIAL.
          CONCATENATE WA_EXCEL-VALUE+6(4) WA_EXCEL-VALUE+3(2) WA_EXCEL-VALUE+0(2) INTO WA_EXCEL-VALUE .
          WA_DATA-CTDATE1 = WA_EXCEL-VALUE .
        ENDIF.

*         lv_cleaned_value = wa_excel-value.
*        REPLACE ALL OCCURRENCES OF '-' IN lv_cleaned_value WITH space .
*        CONDENSE lv_cleaned_value.
*        WA_DATA-CTDATE1  = lv_cleaned_value.
*        CLEAR lv_cleaned_value.
      WHEN 13.
        WA_DATA-FOLW1  = WA_EXCEL-VALUE.
      WHEN 14.
         lv_cleaned_value = wa_excel-value.
        REPLACE ALL OCCURRENCES OF ':' IN lv_cleaned_value WITH space .
        CONDENSE lv_cleaned_value.
        WA_DATA-CTUZEIT  = lv_cleaned_value.
        CLEAR lv_cleaned_value.
      WHEN 15.
         IF WA_EXCEL-VALUE IS NOT INITIAL.
          CONCATENATE WA_EXCEL-VALUE+6(4) WA_EXCEL-VALUE+3(2) WA_EXCEL-VALUE+0(2) INTO WA_EXCEL-VALUE .
          WA_DATA-CTCPUDT = WA_EXCEL-VALUE .
        ENDIF.
*
*        lv_cleaned_value = wa_excel-value.
*        REPLACE ALL OCCURRENCES OF '-' IN lv_cleaned_value WITH space .
*        CONDENSE lv_cleaned_value.
*        WA_DATA-CTCPUDT  = lv_cleaned_value.
*        CLEAR lv_cleaned_value.
*        WA_DATA-CTCPUDT  = WA_EXCEL-VALUE.
      WHEN 16.
        WA_DATA-CTUNAME  = WA_EXCEL-VALUE.
      WHEN 17.
        WA_DATA-CTPERNR  = WA_EXCEL-VALUE.
    ENDCASE.

    AT END OF ROW.
      WA_DATA-MANDT = SY-MANDT.
      APPEND WA_DATA TO IT_DATA .
*      MODIFY zincvaldata FROM wa_data.
      CLEAR : WA_DATA.
    ENDAT.

  ENDLOOP.
  MODIFY ZDMS3 FROM TABLE IT_DATA.
  CLEAR WA_DATA.
  COMMIT WORK.
  MESSAGE 'Upload finished. Data inserted/updated into ZDMS3.' TYPE 'S'.
