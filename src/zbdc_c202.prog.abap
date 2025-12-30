REPORT ZBDC_C202
       NO STANDARD PAGE HEADING LINE-SIZE 255.

* Include bdcrecx1_s:
* The call transaction using is called WITH AUTHORITY-CHECK!
* If you have own auth.-checks you can use include bdcrecx1 instead.
TABLES : SSCRFIELDS.
TYPES: BEGIN OF TY_ITAB,
         PLNNR TYPE STRING, " Recipe Group
         PLNAL TYPE STRING, " Group Counter
         VORNR TYPE STRING , "Operation
         VGW01 TYPE STRING, " Activity1
         VGW02 TYPE STRING, "Activity2
         VGW03 TYPE STRING, "Activity3
         VGW04 TYPE STRING, "Activity4
       END OF TY_ITAB.
TYPES : BEGIN OF TY_FINAL,
          PLNNR   TYPE I,
          PLNAL   TYPE C,
          MESSAGE TYPE CHAR220,
        END OF TY_FINAL.


DATA : RECIPE_GRP TYPE PLNNR.
DATA :  GROUP_CNTR TYPE PLNAL.


"Added New****************

TYPES: BEGIN OF TY_LOG,
         PLNNR   TYPE RC271-PLNNR,
         PLNAL   TYPE RC271-PLNAL,
         VORNR   TYPE PLPOD-VORNR,
         STATUS  TYPE CHAR10,
         MESSAGE TYPE STRING,
       END OF TY_LOG.

DATA: IT_ITAB2 TYPE STANDARD TABLE OF TY_ITAB,
      WA_DATA2 TYPE TY_ITAB.

DATA: GT_LOG TYPE STANDARD TABLE OF TY_LOG,
      GS_LOG TYPE TY_LOG.


"Added New**********


DATA : IT_FINAL TYPE STANDARD TABLE OF TY_FINAL,
       WA_FINAL TYPE TY_FINAL.
DATA : GT_FCAT TYPE  SLIS_T_FIELDCAT_ALV,
       GS_FCAT LIKE LINE OF GT_FCAT.

DATA: GS_LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : LV_FIELD TYPE BDC_FVAL.
DATA : LV_INDEX TYPE N LENGTH 2 .

DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA: IT_DATA TYPE STANDARD TABLE OF ZST_C202,
      WA_DATA TYPE ZST_C202.


DATA LV_INPT TYPE STRING.
DATA LV_OUT TYPE STRING.
DATA LV_VAR  TYPE STRING.
DATA: H_EXCEL TYPE OLE2_OBJECT,        " EXCEL OBJECT
      H_MAPL  TYPE OLE2_OBJECT,        " LIST OF WORKBOOKS
      H_MAP   TYPE OLE2_OBJECT,        " WORKBOOK
      H_ZL    TYPE OLE2_OBJECT,        " CELL
      H_F     TYPE OLE2_OBJECT,        " FONT
      H       TYPE I,
      INPUT   TYPE SY-UCOMM.
DATA : LV_NUM TYPE I.
DATA : IT_ITAB TYPE STANDARD TABLE OF TY_ITAB.
DATA : WA_ITAB  TYPE TY_ITAB.
DATA : IT_ITEM TYPE STANDARD TABLE OF TY_ITAB.
DATA : WA_ITEM TYPE TY_ITAB.
DATA : LV_FNAME TYPE LOCALFILE.
DATA : IT_EXCEL TYPE TABLE OF ALSMEX_TABLINE.
DATA : WA_EXCEL TYPE ALSMEX_TABLINE.
*DATA : BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.
DATA : LV_CTU_PARAMS TYPE CTU_PARAMS.
DATA:   LV_MESTEXT TYPE CHAR100.
DATA : LT_MSGTAB TYPE STANDARD TABLE OF BDCMSGCOLL,
       WA_MSGTAB TYPE BDCMSGCOLL.

DATA: IT_BDCDATA TYPE TABLE OF BDCDATA,
      WA_BDCDATA TYPE BDCDATA.
DATA: PV_TEXT   TYPE STRING.
DATA: PV_STATUS   TYPE CHAR1.
DATA : PT_MSG    TYPE BDCMSGCOLL.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
  PARAMETERS: P_FILE   TYPE LOCALFILE .
*              p_down TYPE rlgrap-filename LOWER CASE.
*  PARAMETERS: P_DOWN AS CHECKBOX .
  SELECTION-SCREEN SKIP 2.
  SELECTION-SCREEN PUSHBUTTON 10(20) TEXT-002 USER-COMMAND CL1.
SELECTION-SCREEN END OF BLOCK B1.


AT SELECTION-SCREEN." ON VALUE-REQUEST FOR P_TEMP .
  CASE SY-UCOMM.
    WHEN 'CL1'.
      PERFORM DOWNLOAD_TEMPLATE .
  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE .
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = ' '
    IMPORTING
      FILE_NAME     = P_FILE.



*include bdcrecx1_s.

START-OF-SELECTION.
  PERFORM UPLOAD_FILE.
  IF IT_ITAB IS NOT INITIAL.
*    PERFORM CHECK_TEMPLATE.
*    PERFORM BDC_EXECUTATION.
    PERFORM BDC.
  ENDIF.

  PERFORM : F_POP_MSG_FCAT. "fieldcat populate
  PERFORM : F_DISP_MSG_ALV.
FORM UPLOAD_FILE .
  LV_FNAME = P_FILE.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = LV_FNAME
      I_BEGIN_COL             = '1'
      I_BEGIN_ROW             = '2'
      I_END_COL               = '43'
      I_END_ROW               = '9999'
    TABLES
      INTERN                  = IT_EXCEL
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
*  DATA : IT_COPY TYPE STANDARD TABLE OF ALSMEX_TABLINE,
*         WA_COPY LIKE LINE OF IT_COPY.
*  IT_COPY[] = IT_EXCEL[].
*  BREAK-POINT .
  LOOP AT IT_EXCEL INTO WA_EXCEL.

    CASE WA_EXCEL-COL.
      WHEN '0001'.
        WA_ITAB-PLNNR     = WA_EXCEL-VALUE.
      WHEN '0002'.
        WA_ITAB-PLNAL     = WA_EXCEL-VALUE.
      WHEN '0003'.
        WA_ITAB-VORNR     = WA_EXCEL-VALUE.
      WHEN '0004'.
        WA_ITAB-VGW01     = WA_EXCEL-VALUE.
      WHEN '0005'.
        WA_ITAB-VGW02     = WA_EXCEL-VALUE.
      WHEN '0006'.
        WA_ITAB-VGW03     = WA_EXCEL-VALUE.
      WHEN '0007'.
        WA_ITAB-VGW04     = WA_EXCEL-VALUE.
    ENDCASE.
    AT END OF ROW.
      APPEND WA_ITAB TO IT_ITAB.
      CLEAR WA_ITAB.
    ENDAT.
    CLEAR WA_EXCEL.
  ENDLOOP.
*
*    ENDLOOP.
*    APPEND WA_ITAB TO IT_ITAB.
*  ENDLOOP.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form bdc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BDC .

  LOOP AT IT_ITAB INTO DATA(WA_DATA2).

    PERFORM BDC_DYNPRO      USING 'SAPLCPDI' '4000'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RC271-PLNNR'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RC271-PLNNR'
                                  WA_DATA2-PLNNR ."'50000040'.
    PERFORM BDC_FIELD       USING 'RC271-PLNAL'
                                  WA_DATA2-PLNAL."'1'.
    PERFORM BDC_FIELD       USING 'RC271-STTAG'
                                  '22.12.2025'.

    PERFORM BDC_DYNPRO      USING 'SAPLCPDI' '4400'.

*   lv_out = lv_inpt+0(2).
    LV_INPT = WA_DATA2-VORNR.

*    lv_out = lv_inpt+1(2).
*     CONCATENATE WA_DATA2-PLNAL.
*    CONCATENATE 'PLPOD-VORNR(' '0' LV_OUT ')' INTO LV_VAR .
    IF WA_DATA2-VORNR = '0040'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'PLPOD-VORNR(04)'.
    ELSEIF WA_DATA2-VORNR = '0020'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'PLPOD-VORNR(02)'.
    ENDIF.


    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=PICK'.
    PERFORM BDC_FIELD       USING 'RC27X-ENTRY_ACT'
                                  '1'.
    PERFORM BDC_DYNPRO      USING 'SAPLCPDO' '4410'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'PLPOD-VGW04'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'PLPOD-VORNR'
                                    WA_DATA2-VORNR."'0040'.
    PERFORM BDC_FIELD       USING 'PLPOD-STEUS'
                                  'YPI1'.
    PERFORM BDC_FIELD       USING 'PLPOD-CKSELKZ'
                                  'X'.
    PERFORM BDC_FIELD       USING 'PLPOD-LTXA1'
                                  'OK'.
    PERFORM BDC_FIELD       USING 'PLPOD-BMSCH'
                                  '300,000'.
    PERFORM BDC_FIELD       USING 'PLPOD-MEINH'
                                  'PC'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGW01'
                                  WA_DATA2-VGW01."'10.02'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGE01'
                                  'HR'.
    PERFORM BDC_FIELD       USING 'PLPOD-LAR01'
                                  '1002'.
    PERFORM BDC_FIELD       USING 'PLPOD-UMREZ'
                                  '1'.
    PERFORM BDC_FIELD       USING 'PLPOD-UMREN'
                                  '1'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGW02'
                                  WA_DATA2-VGW02."'12.02'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGE02'
                                  'HR'.
    PERFORM BDC_FIELD       USING 'PLPOD-LAR02'
                                  '1001'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGW03'
                                  WA_DATA2-VGW03."'13'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGE03'
                                  'EA'.
    PERFORM BDC_FIELD       USING 'PLPOD-LAR03'
                                  '1003'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGW04'
                                  WA_DATA2-VGW04."'14'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGE04'
                                  'EA'.
    PERFORM BDC_FIELD       USING 'PLPOD-LAR04'
                                  '1005'.
    PERFORM BDC_DYNPRO      USING 'SAPLCPDO' '4410'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'PLPOD-VORNR'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM BDC_FIELD       USING 'PLPOD-VORNR'
                                  WA_DATA2-VORNR."'0040'.
    PERFORM BDC_FIELD       USING 'PLPOD-STEUS'
                                  'YPI1'.
    PERFORM BDC_FIELD       USING 'PLPOD-CKSELKZ'
                                  'X'.
    PERFORM BDC_FIELD       USING 'PLPOD-LTXA1'
                                  'OK'.
    PERFORM BDC_FIELD       USING 'PLPOD-BMSCH'
                                  '300,000'.
    PERFORM BDC_FIELD       USING 'PLPOD-MEINH'
                                  'PC'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGW01'
                                  WA_DATA2-VGW01."'10.020'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGE01'
                                  'HR'.
    PERFORM BDC_FIELD       USING 'PLPOD-LAR01'
                                  '1002'.
    PERFORM BDC_FIELD       USING 'PLPOD-UMREZ'
                                  '1'.
    PERFORM BDC_FIELD       USING 'PLPOD-UMREN'
                                  '1'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGW02'
                                  WA_DATA2-VGW02."'12.020'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGE02'
                                  'HR'.
    PERFORM BDC_FIELD       USING 'PLPOD-LAR02'
                                  '1001'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGW03'
                                  WA_DATA2-VGW03."'13'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGE03'
                                  'EA'.
    PERFORM BDC_FIELD       USING 'PLPOD-LAR03'
                                  '1003'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGW04'
                                  WA_DATA2-VGW04."'14'.
    PERFORM BDC_FIELD       USING 'PLPOD-VGE04'
                                  'EA'.
    PERFORM BDC_FIELD       USING 'PLPOD-LAR04'
                                  '1005'.
*    PERFORM BDC_TRANSACTION USING 'C202'.

    CALL TRANSACTION 'C202' USING IT_BDCDATA MODE 'A' UPDATE 'A'
                            MESSAGES INTO LT_MSGTAB.


    RECIPE_GRP = WA_DATA2-PLNNR.
    GROUP_CNTR =  WA_DATA2-PLNAL.
    PERFORM PROCESS_BDC_MESSAGES.



*    LOOP AT LT_MSGTAB INTO WA_MSGTAB WHERE MSGTYP = 'E' OR MSGTYP = 'S'.
*   CALL FUNCTION 'FORMAT_MESSAGE'
*    EXPORTING
*      ID              =  WA_MSGTAB-MSGID
*      LANG            = 'sy-langu'
*      plnnr              =  WA_MSGTAB-plnnr
*      V1              =  WA_MSGTAB-MSGV1
*      V2              =  WA_MSGTAB-MSGV2
*      V3              = WA_MSGTAB-MSGV3
*      V4              = WA_MSGTAB-MSGV4
*    IMPORTING
*      MSG             = LV_MESTEXT
*    EXCEPTIONS
*      NOT_FOUND       = 1
*      OTHERS          = 2
*             .
*   IF SY-SUBRC <> 0.
** Implement suitable error handling here
*     WA_FINAL-PLNNR = WA_ITEM-PLNNR.
*     WA_FINAL-PLNAL = WA_ITEM-PLNAL.
*     WA_FINAL-MESSAGE = WA_ITEM-MESSAGE.
*     APPEND wa_final to it_final.
*     clear : wa_final.
*   ENDIF.
*
    CLEAR:  IT_BDCDATA.
*
*
*
**perform close_group.
  ENDLOOP.


  PERFORM DISPLAY_ALV.

ENDFORM.


FORM BDC_DYNPRO  USING PROGRAM DYNPRO.

  CLEAR WA_BDCDATA.
  WA_BDCDATA-PROGRAM  = PROGRAM.
  WA_BDCDATA-DYNPRO   = DYNPRO.
  WA_BDCDATA-DYNBEGIN = 'X'.
  APPEND WA_BDCDATA TO IT_BDCDATA.


ENDFORM.
FORM BDC_FIELD USING FNAM FVAL.
  IF FVAL <> ''.
    CLEAR WA_BDCDATA.
    WA_BDCDATA-FNAM = FNAM.
    WA_BDCDATA-FVAL = FVAL.
    APPEND WA_BDCDATA TO IT_BDCDATA .    .
  ENDIF.
ENDFORM.
FORM DOWNLOAD_TEMPLATE .
  CASE  SSCRFIELDS-UCOMM.
    WHEN 'CL1'.
      INPUT = 'CL1'.
      CLEAR SY-UCOMM.
      PERFORM SHOW_EXCEL.
  ENDCASE.
ENDFORM.
FORM SHOW_EXCEL.

  CREATE OBJECT H_EXCEL 'EXCEL.APPLICATION'.

  SET PROPERTY OF H_EXCEL 'Visible' = 1.

  CALL METHOD OF H_EXCEL 'Workbooks' = H_MAPL.

  CALL METHOD OF H_MAPL 'Add' = H_MAP.

  IF INPUT = 'CL1'.
    PERFORM FILL_CELL USING 1 1   1 TEXT-003.
    PERFORM FILL_CELL USING 1 2   1 TEXT-004.
    PERFORM FILL_CELL USING 1 3   1 TEXT-005.
    PERFORM FILL_CELL USING 1 4   1 TEXT-006.
    PERFORM FILL_CELL USING 1 5   1 TEXT-007.
    PERFORM FILL_CELL USING 1 6   1 TEXT-008.
    PERFORM FILL_CELL USING 1 7   1 TEXT-009.
  ENDIF.
  CLEAR INPUT.


  CALL METHOD OF H_EXCEL 'Worksheets' = H_MAPL.

  FREE OBJECT H_EXCEL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_CELL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_1
*&      --> P_1
*&      --> P_1
*&      --> TEXT_003
*&---------------------------------------------------------------------*
FORM FILL_CELL  USING   I J BOLD VAL.

  CALL METHOD OF H_EXCEL 'CELLS' = H_ZL
  EXPORTING
    #1 = I
    #2 = J.
  SET PROPERTY OF H_ZL 'Value' = VAL.
  GET PROPERTY OF H_ZL 'Font'  = H_F.
  SET PROPERTY OF H_F  'Bold'  = BOLD.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form download_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DOWNLOAD_FILE .

  DATA : LV_FILE1 TYPE STRING.

  LV_FILE1 = P_FILE.

  CLEAR WA_DATA.
  APPEND WA_DATA TO IT_DATA.


  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME              = LV_FILE1
      FILETYPE              = 'ASC'
      WRITE_FIELD_SEPARATOR = 'X'
    TABLES
      DATA_TAB              = IT_DATA
    EXCEPTIONS
      OTHERS                = 1.

  IF SY-SUBRC = 0.
    MESSAGE 'Downloaded Successfully' TYPE 'S'.
  ELSE.
    MESSAGE 'Download failed' TYPE 'E'.
  ENDIF.
*ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISP_MSG_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_DISP_MSG_ALV .
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*   EXPORTING
*     I_CALLBACK_PROGRAM                = SY-REPID
*     IS_LAYOUT                         = GS_LAYOUT
*     IT_FIELDCAT                       = GT_FCAT
*
*    TABLES
*      T_OUTTAB                          = GT_MSG
*   EXCEPTIONS
*     PROGRAM_ERROR                     = 1
**     OTHERS                            = 2
*            .
*  IF SY-SUBRC <> 0.
** Implement suitable error handling here
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_POP_MSG_FCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_POP_MSG_FCAT .
  DATA : COUNT     TYPE CHAR4.
  CLEAR: COUNT.
  COUNT = COUNT + 1.
  GS_FCAT-COL_POS = COUNT.
  GS_FCAT-FIELDNAME = 'PLNNR'.
  GS_FCAT-TABNAME = 'GT_MSG'.
  GS_FCAT-SELTEXT_M = 'RECEIPT GROUP'.
  APPEND GS_FCAT TO GT_FCAT.
  CLEAR: GS_FCAT.

  GS_FCAT-COL_POS = COUNT.
  GS_FCAT-FIELDNAME = 'PLNAL'.
  GS_FCAT-TABNAME = 'GT_MSG'.
  GS_FCAT-SELTEXT_M = 'GROUP COUNTER'.
  APPEND GS_FCAT TO GT_FCAT.
  CLEAR: GS_FCAT.

  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GS_LAYOUT-ZEBRA = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_BDC_MESSAGES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESS_BDC_MESSAGES .



  DATA: LV_ERROR TYPE ABAP_BOOL VALUE ABAP_FALSE,
        LV_TEXT  TYPE STRING.

  " Check for error messages
  LOOP AT LT_MSGTAB INTO DATA(LS_MSG)
       WHERE MSGTYP = 'E' OR MSGTYP = 'A'.

    LV_ERROR = ABAP_TRUE.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        MSGID               = LS_MSG-MSGID
        MSGNR               = LS_MSG-MSGNR
        MSGV1               = LS_MSG-MSGV1
        MSGV2               = LS_MSG-MSGV2
        MSGV3               = LS_MSG-MSGV3
        MSGV4               = LS_MSG-MSGV4
      IMPORTING
        MESSAGE_TEXT_OUTPUT = LV_TEXT.
    EXIT.
  ENDLOOP.

  " Fill ALV log
  CLEAR GS_LOG.

  GS_LOG-PLNNR = RECIPE_GRP.
  GS_LOG-PLNAL = GROUP_CNTR.
  GS_LOG-VORNR = WA_DATA2-VORNR.

  IF LV_ERROR = ABAP_FALSE.
    GS_LOG-STATUS  = 'SUCCESS'.
    GS_LOG-MESSAGE = 'Recipe group has been changed'.
  ELSE.
    GS_LOG-STATUS  = 'ERROR'.
    GS_LOG-MESSAGE = LV_TEXT.
  ENDIF.

  APPEND GS_LOG TO GT_LOG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV .



  DATA: GT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
        GS_FCAT   TYPE SLIS_FIELDCAT_ALV,
        GS_LAYOUT TYPE SLIS_LAYOUT_ALV.

  CLEAR GT_FCAT.

  " Recipe Group
  GS_FCAT-FIELDNAME = 'PLNNR'.
  GS_FCAT-SELTEXT_M = 'Recipe Group'.
  APPEND GS_FCAT TO GT_FCAT.
  CLEAR GS_FCAT.
*
*  " Group Counter
  GS_FCAT-FIELDNAME = 'PLNAL'.
  GS_FCAT-SELTEXT_M = 'Group Counter'.
  APPEND GS_FCAT TO GT_FCAT.
  CLEAR GS_FCAT.

  " Operation
*  gs_fcat-fieldname = 'VORNR'.
*  gs_fcat-seltext_m = 'Operation'.
*  APPEND gs_fcat TO gt_fcat.
*  CLEAR gs_fcat.

  " Status
  GS_FCAT-FIELDNAME = 'STATUS'.
  GS_FCAT-SELTEXT_M = 'Status'.
  APPEND GS_FCAT TO GT_FCAT.
  CLEAR GS_FCAT.

  " Message
  GS_FCAT-FIELDNAME = 'MESSAGE'.
  GS_FCAT-SELTEXT_M = 'Message'.
  GS_FCAT-OUTPUTLEN = 60.
  APPEND GS_FCAT TO GT_FCAT.
  CLEAR GS_FCAT.

  " Layout
  GS_LAYOUT-ZEBRA = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  " Display ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
      IS_LAYOUT          = GS_LAYOUT
      IT_FIELDCAT        = GT_FCAT
    TABLES
      T_OUTTAB           = GT_LOG
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

ENDFORM.
