*&---------------------------------------------------------------------*
*& Report ZDMS6_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZINSP_UPLOAD.

TABLES: ZINSP.

DATA: IT_DATA          TYPE STANDARD TABLE OF ZINSP,
      WA_DATA          TYPE ZINSP,
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
        WA_DATA-MATNR =   WA_EXCEL-VALUE.
*         CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            INPUT         =  WA_DATA-MATNR
*         IMPORTING
*           OUTPUT        =  WA_DATA-MATNR.
         CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
           EXPORTING
             INPUT              = WA_DATA-MATNR
          IMPORTING
            OUTPUT             = WA_DATA-MATNR
          EXCEPTIONS
            LENGTH_ERROR       = 1
            OTHERS             = 2
                   .
         IF SY-SUBRC <> 0.
* Implement suitable error handling here
         ENDIF.


      WHEN 2.
        WA_DATA-CHARG  = WA_EXCEL-VALUE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT         =  WA_DATA-CHARG
         IMPORTING
           OUTPUT        =  WA_DATA-CHARG
                  .
      WHEN 3.
        WA_DATA-WERKS = WA_EXCEL-VALUE  .
      WHEN 4.
        WA_DATA-LGORT = WA_EXCEL-VALUE .
      WHEN 5.
         WA_DATA-LIFNR = WA_EXCEL-VALUE .
         CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT         =  WA_DATA-LIFNR
         IMPORTING
           OUTPUT        =  WA_DATA-LIFNR
                  .

      WHEN 6.
        WA_DATA-FKIMG  = WA_EXCEL-VALUE.
      WHEN 7.
        WA_DATA-PRUEFLOS  = WA_EXCEL-VALUE.
*


    ENDCASE.

    AT END OF ROW.
      WA_DATA-MANDT = SY-MANDT.
      APPEND WA_DATA TO IT_DATA .
*      MODIFY zincvaldata FROM wa_data.
      CLEAR : WA_DATA.
    ENDAT.

  ENDLOOP.
  MODIFY ZINSP FROM TABLE IT_DATA.
  IF sy-subrc  = 0.
  MESSAGE 'Upload finished. Data inserted/updated into ZINSP.' TYPE 'S'.
  CLEAR WA_DATA.
  COMMIT WORK.
  ENDIF.
