*REPORT Z_GOODSISSUE_LABELS LINE-COUNT 89 NO STANDARD PAGE HEADING LINE-SIZE 114.
*table declerations-------------------------
* cHANGES DONE BY ANJALI.
report z_goodsissue_labels line-count 65(4) no standard page heading
line-size 114.
*report z_goodsissue_labels line-count 65(4) no standard page heading
*line-size 105.
TABLES:
  S026,
  MKPF,
  MSEG,
  LFA1,
  MCHA,
  MAKT,
  QALS,
  QAMB,
  S021,
  AFPO,
  AFKO,
  AUFM,
  AUFK,
  MAST,
  STPO,
  T001W,
  STZU,
  STXL,
  THEAD,
  STXH,
  RESB,
  ADRC,
  MARA,
  ZPMS_ART_TABLE,
  JEST.

*data declerations---------------------------
DATA: IT_RESB TYPE TABLE OF RESB,
      WA_RESB TYPE RESB.

DATA : BEGIN OF T_HEADER OCCURS 0,
         AUFNR     LIKE AFPO-AUFNR,   "order no
         CHARG     LIKE AFPO-CHARG,   "batch no material
         PSMNG     LIKE AFPO-PSMNG,   "qty batch
         MEINS     LIKE AFPO-MEINS,   "unit for batch
         MATNR     LIKE MAST-MATNR,   "Material no
         STLNR     LIKE MAST-STLNR,   "bom no
         MAKTXM    LIKE MAKT-MAKTX,   "Material Descr
         ANDAT     LIKE MAST-ANDAT,   "date of order
*       name1  like t001w-name1,  "plant name1
         NAME1(33) TYPE C,
         NAME2     LIKE T001W-NAME2,  "plant name2
         NAME3     LIKE ADRC-NAME3,   "plant name3
         STRAS     LIKE T001W-STRAS,  "street
         PFACH     LIKE T001W-PFACH,  "Post box no
         PSTLZ     LIKE T001W-PSTLZ,  "postal code
         ORT01     LIKE T001W-ORT01,  "city name
         ZTEXT     LIKE STZU-ZTEXT,   "text element
       END OF T_HEADER.
DATA : BEGIN OF T_DETAILS OCCURS 0,
         COUNTER(3) TYPE N,           "counter
         STLNR      LIKE MAST-STLNR,   "bom no
*       idnrk  LIKE stpo-idnrk,   "Component no
         POSNR      LIKE RESB-POSNR,   "item no
         AUFNR      LIKE RESB-AUFNR,    "orderno
         MATNR      LIKE RESB-MATNR,   "component no
         MAKTXC     LIKE MAKT-MAKTX,   "description
         MENGE      LIKE RESB-BDMNG,   "qty
         MEINS      LIKE RESB-MEINS,   "units
         CHARG      LIKE RESB-CHARG,   "comp batc no
         SORTF      LIKE RESB-SORTF,    "sort string
         LGORT      LIKE RESB-LGORT,    "storage location
         PRUEFLOS   LIKE QALS-PRUEFLOS, "inspection lot
       END OF T_DETAILS.
DATA BEGIN OF THEADER.
        INCLUDE STRUCTURE THEAD.
DATA END OF THEADER.
DATA: MTART TYPE MARA-MTART.
DATA: BEGIN OF DTEXT OCCURS 1.
        INCLUDE STRUCTURE TLINE.
      DATA: END OF DTEXT.

DATA : NAME1(35) TYPE C.
DATA: IT_QALS TYPE TABLE OF QALS,
      WA_QALS TYPE QALS.
DATA: IT_QALS1 TYPE TABLE OF QALS,
      WA_QALS1 TYPE QALS.

DATA: COMPONENT_H LIKE STPO-IDNRK.   "Component no
DATA : TEXT(40) TYPE C.
DATA : COUNTER(3) TYPE N .
DATA : COUNTER_NO(3) TYPE N .
DATA : LICOUN(3) TYPE N.
DATA : PG_COUNT TYPE I .
DATA : W_MBLNR LIKE MSEG-MBLNR.
DATA : W_BWART LIKE MSEG-BWART.
DATA : W_MM(1) TYPE C.
COUNTER = 0 .
COUNTER_NO = 0.
W_MM  = '1'.
DATA: WERKS TYPE MSEG-WERKS.
*selection screen-------------------------------------
* The selection Screen will now accept the MATERIAL DOC NO. ALSO
* This provision is done if matl. issued extra on the order.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002 .
PARAMETERS : ORDISS RADIOBUTTON GROUP R1,
             EXTISS RADIOBUTTON GROUP R1 DEFAULT 'X',
             ISSRET RADIOBUTTON GROUP R1.
SELECTION-SCREEN END OF BLOCK B2.


SELECTION-SCREEN BEGIN OF BLOCK INPUT WITH FRAME TITLE TEXT-001 .
PARAMETERS :     P_AUFNR LIKE AFPO-AUFNR. "  OBLIGATORY.
PARAMETERS :     P_MBLNR LIKE MSEG-MBLNR.
PARAMETERS :     P_BUDAT LIKE MKPF-BUDAT OBLIGATORY.
SELECTION-SCREEN END OF BLOCK INPUT.
*top-of-page-----------------------------------------
TOP-OF-PAGE.
  SKIP 5.

END-OF-PAGE.
*BREAK-POINT .
  SELECT SINGLE * FROM MSEG WHERE MBLNR EQ P_MBLNR.
  IF SY-SUBRC EQ 0.
    WERKS = MSEG-WERKS.
  ENDIF.
  IF WERKS EQ '1001'.
    WRITE : / 'PK/GM/020-F1'.
  ENDIF.
*ULINE.
*WRITE : /1 'Issued By :',
*         75 'Checked & Received By :'.
*
*** changes for having no foot note :

*IF ISSRET = 'X'.
*write : /1 'Packing Material Returned:'.
*ELSE.
*write : /1 'Packing Material Issue:'.
*ENDIF.
*skip 2.
*write : /1 'Authorised by :'.

AT SELECTION-SCREEN.
  PERFORM  VALIDATION.

* start of selection---------------------------------
START-OF-SELECTION.
  CLEAR T_HEADER.
  REFRESH T_HEADER.
  PERFORM T_HEADER.
  IF EXTISS = 'X' OR ISSRET = 'X'.
    PERFORM T_DETAILS1.
  ELSE.
    PERFORM T_DETAILS.
  ENDIF.
  PERFORM WRITE_MHEADER.
  IF ISSRET = 'X'.
    PERFORM WRITE_HEADER1.
  ELSE.
    PERFORM WRITE_HEADER.
  ENDIF.
  PERFORM WRITE_DETAILS.

*&---------------------------------------------------------------------*
*&      Form  T_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM T_HEADER.
  IF EXTISS = 'X' OR ISSRET = 'X'.
    SELECT SINGLE AUFNR FROM MSEG INTO P_AUFNR WHERE  MBLNR = P_MBLNR.
  ENDIF.
  SELECT * FROM AFPO WHERE AUFNR = P_AUFNR.
    MOVE-CORRESPONDING AFPO TO T_HEADER.
    SELECT SINGLE * FROM MAST WHERE MATNR = AFPO-MATNR
                              AND STLAN = '1'.
    T_HEADER-STLNR = MAST-STLNR.
    SELECT SINGLE * FROM RESB WHERE AUFNR = AFPO-AUFNR.

    T_HEADER-ANDAT = RESB-BDTER.
* changes done by anjali as the addresses are not picked up properly.
*    SELECT SINGLE * FROM t001w WHERE werks = mast-werks.
    SELECT SINGLE * FROM T001W WHERE WERKS = AFPO-PWERK.
    IF AFPO-PWERK EQ '1000' OR AFPO-PWERK EQ '1001'.
      NAME1 = 'BLUE CROSS LABORATORIES PVT LTD.'.
      T_HEADER-NAME1 = NAME1.
    ELSE.
      T_HEADER-NAME1 = T001W-NAME1.
    ENDIF.
    T_HEADER-NAME2 = T001W-NAME2.
    T_HEADER-STRAS = T001W-STRAS.
    T_HEADER-PFACH = T001W-PFACH.
    T_HEADER-PSTLZ = T001W-PSTLZ.
    T_HEADER-ORT01 = T001W-ORT01.
    SELECT SINGLE * FROM MAKT WHERE MATNR = MAST-MATNR.
    T_HEADER-MAKTXM = MAKT-MAKTX.
    SELECT SINGLE * FROM ADRC WHERE NAME1 = T001W-NAME1.
    T_HEADER-NAME3 = ADRC-NAME3.
    SELECT SINGLE * FROM STZU WHERE STLNR = MAST-STLNR.
    T_HEADER-ZTEXT = STZU-ZTEXT.
    APPEND T_HEADER.
  ENDSELECT.
ENDFORM.                    " T_HEADER

*&---------------------------------------------------------------------*
*&      Form  T_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM T_DETAILS.


*  LOOP AT T_HEADER.
*    SELECT * FROM RESB WHERE AUFNR = T_HEADER-AUFNR.
*      IF RESB-BDMNG NE 0 .
**       t_details-counter = t_details-counter + 1.
*        T_DETAILS-POSNR = RESB-POSNR.
*        T_DETAILS-MATNR = RESB-MATNR.
*        T_DETAILS-AUFNR = RESB-AUFNR.
*        T_DETAILS-MENGE = RESB-BDMNG.
*        T_DETAILS-MEINS = RESB-MEINS.
*        T_DETAILS-CHARG = RESB-CHARG.
*        T_DETAILS-SORTF = RESB-SORTF.
*        T_DETAILS-LGORT = RESB-LGORT.
*        SELECT SINGLE * FROM MAKT WHERE MATNR = RESB-MATNR.
*        T_DETAILS-MAKTXC = MAKT-MAKTX.
*
*
*        IF RESB-POSTP = 'L'.
*          SELECT SINGLE * FROM QALS WHERE MATNR = RESB-MATNR
*                                    AND CHARG = RESB-CHARG
*            AND WERK EQ RESB-WERKS.  "added on 28.10.21
*          IF SY-SUBRC = 0.
*            T_DETAILS-PRUEFLOS = QALS-PRUEFLOS.
*          ELSE.
*            T_DETAILS-PRUEFLOS = RESB-CHARG.
*          ENDIF.
*        ENDIF.
**        IF resb-postp = 'N'.
**          SELECT SINGLE * FROM qals WHERE matnr = resb-matnr
**                                    AND charg = resb-charg.
**          t_details-prueflos = 1.
**        ENDIF.
*
*        APPEND T_DETAILS.
*      ENDIF.
*    ENDSELECT.
*  ENDLOOP.
*

  IF T_HEADER IS NOT INITIAL.
    SELECT * FROM RESB INTO TABLE IT_RESB FOR ALL ENTRIES IN T_HEADER WHERE AUFNR = T_HEADER-AUFNR.
    SELECT * FROM QALS INTO TABLE IT_QALS1 FOR ALL ENTRIES IN IT_RESB WHERE MATNR EQ IT_RESB-MATNR AND CHARG EQ IT_RESB-CHARG AND WERK EQ IT_RESB-WERKS.
  ENDIF.

   SORT IT_QALS1 DESCENDING BY ENSTEHDAT.

    LOOP AT IT_QALS1 INTO WA_QALS1.
      SELECT SINGLE * FROM JEST WHERE OBJNR EQ WA_QALS1-OBJNR AND STAT EQ 'I0224'.
      IF SY-SUBRC EQ 0.
        DELETE IT_QALS1 WHERE PRUEFLOS EQ WA_QALS1-PRUEFLOS.
      ENDIF.
    ENDLOOP.


  IF IT_RESB IS NOT INITIAL.
    LOOP AT IT_RESB INTO WA_RESB WHERE BDMNG NE 0.
*      IF RESB-BDMNG NE 0 .
*       t_details-counter = t_details-counter + 1.
      T_DETAILS-POSNR = WA_RESB-POSNR.
      T_DETAILS-MATNR = WA_RESB-MATNR.
      T_DETAILS-AUFNR = WA_RESB-AUFNR.
      T_DETAILS-MENGE = WA_RESB-BDMNG.
      T_DETAILS-MEINS = WA_RESB-MEINS.
      T_DETAILS-CHARG = WA_RESB-CHARG.
      T_DETAILS-SORTF = WA_RESB-SORTF.
      T_DETAILS-LGORT = WA_RESB-LGORT.
      SELECT SINGLE * FROM MAKT WHERE MATNR = WA_RESB-MATNR AND SPRAS EQ 'EN'.
      IF SY-SUBRC EQ 0.
        T_DETAILS-MAKTXC = MAKT-MAKTX.
      ENDIF.


      IF WA_RESB-POSTP = 'L'.
        READ TABLE IT_QALS1 INTO WA_QALS1 WITH KEY MATNR = WA_RESB-MATNR  CHARG = WA_RESB-CHARG WERK = WA_RESB-WERKS.  "added on 28.10.21
        IF SY-SUBRC = 0.
          T_DETAILS-PRUEFLOS = WA_QALS1-PRUEFLOS.
*        ELSE.
*          T_DETAILS-PRUEFLOS = WA_RESB-CHARG.
        ENDIF.
      ENDIF.
*        IF resb-postp = 'N'.
*          SELECT SINGLE * FROM qals WHERE matnr = resb-matnr
*                                    AND charg = resb-charg.
*          t_details-prueflos = 1.
*        ENDIF.

      APPEND T_DETAILS.
      CLEAR : T_DETAILS .
    ENDLOOP.
  ENDIF.
*ENDLOOP.



ENDFORM.                    " T_DETAILS

*&---------------------------------------------------------------------*
*&      Form  T_DETAILS1.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM T_DETAILS1.

  SELECT * FROM MSEG  WHERE MBLNR = P_MBLNR.
*       t_details-counter = t_details-counter + 1.
    T_DETAILS-POSNR = MSEG-ZEILE.
    T_DETAILS-MATNR = MSEG-MATNR.
    T_DETAILS-AUFNR = MSEG-AUFNR.
    T_DETAILS-MENGE = MSEG-MENGE.
    T_DETAILS-MEINS = MSEG-MEINS.
    T_DETAILS-CHARG = MSEG-CHARG.
    SELECT SINGLE * FROM MAKT WHERE MATNR = MSEG-MATNR.
    T_DETAILS-MAKTXC = MAKT-MAKTX.

    SELECT * FROM QALS INTO TABLE IT_QALS WHERE WERK EQ MSEG-WERKS AND MATNR = MSEG-MATNR AND CHARG = MSEG-CHARG.
    SORT IT_QALS DESCENDING BY ENSTEHDAT.

    LOOP AT IT_QALS INTO WA_QALS.
      SELECT SINGLE * FROM JEST WHERE OBJNR EQ WA_QALS-OBJNR AND STAT EQ 'I0224'.
      IF SY-SUBRC EQ 0.
        DELETE IT_QALS WHERE PRUEFLOS EQ WA_QALS-PRUEFLOS.
      ENDIF.
    ENDLOOP.
*    select single * from qals where matnr = mseg-matnr
*                              and charg = mseg-charg.
    READ TABLE IT_QALS INTO WA_QALS WITH KEY WERK = MSEG-WERKS MATNR = MSEG-MATNR CHARG = MSEG-CHARG.
    IF SY-SUBRC = 0.
      T_DETAILS-PRUEFLOS = WA_QALS-PRUEFLOS.
    ELSE.
      SELECT SINGLE PRUEFLOS , MATNR , CHARG FROM ZINSP INTO @DATA(WA_INSP) WHERE MATNR = @MSEG-MATNR AND CHARG = @MSEG-CHARG.
       IF SY-SUBRC = 0.
      T_DETAILS-PRUEFLOS = WA_INSP-PRUEFLOS.

      ENDIF.
      ENDIF.
 "T_DETAILS-PRUEFLOS = MSEG-CHARG.
*    ENDIF.
    APPEND T_DETAILS.
  ENDSELECT.
ENDFORM.                    " T_DETAILS1.

*&---------------------------------------------------------------------*
*&      Form  write_mheader
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM WRITE_MHEADER.
  DATA TEXT(100).
  CONCATENATE T_HEADER-NAME2 T_HEADER-NAME3 INTO
   TEXT SEPARATED BY SPACE.

  WRITE :/45  T_HEADER-NAME1,
         /45   TEXT.
  SKIP.
  CLEAR :  MTART.
  SELECT SINGLE * FROM MARA WHERE MATNR EQ T_HEADER-MATNR.
  IF SY-SUBRC EQ 0.
    MTART = MARA-MTART.
  ENDIF.
  IF EXTISS = 'X'.
    IF MTART EQ 'ZHLB'.
      WRITE :/40 'Raw Material Issue Note'.
    ELSE.
      WRITE :/40 'Packing Material Issue Note'.
    ENDIF.
  ELSEIF ISSRET = 'X'.
    IF MTART EQ 'ZHLB'.
      WRITE :/45 'Raw Material Return Note'.
    ELSE.
      WRITE :/45 'Packing Material Return Note'.
    ENDIF.
  ELSE.
    IF MTART EQ 'ZHLB'.
      WRITE :/45 'Raw Material Issue Note'.
    ELSE.
      WRITE :/45 'Packing Material Issue Note'.
    ENDIF.
  ENDIF.


*  write:/80 'Page No.', sy-pagno.
  SKIP 2.
  WRITE :/5  'Product : ',
          15  T_HEADER-MAKTXM.
*  SKIP.
*  WRITE :/5  'Batch No: ',
*          16  T_HEADER-CHARG,
*          60 'Order No : ',
*          72  T_HEADER-AUFNR.
  WRITE : 60 'Order No : ',
          72  T_HEADER-AUFNR.
  SKIP.
  WRITE :/5  'Batch No: ',
         16  T_HEADER-CHARG.
  IF ISSRET EQ 'X'.
    WRITE :60 'Date of Return : ',
          78  P_BUDAT.
  ELSE.
    WRITE :60 'Date of Issue : ',
           78  P_BUDAT.
  ENDIF.

  SKIP.
  WRITE :/5  'Batch Size: ',
          17  T_HEADER-PSMNG,
          34  T_HEADER-MEINS.
  IF EXTISS = 'X' OR ISSRET = 'X'.
    WRITE : 60 'Matl. Doc. Ref: ',
         78 P_MBLNR.
  ENDIF.

  SKIP.


ENDFORM.                    "write_mheader



*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_HEADER.
  WRITE :/4  SY-ULINE.
  WRITE :/5  'Sr',
             9  SY-VLINE,
             10  'Description',
             52  SY-VLINE,
             53  'Insp Lot No',
             66  SY-VLINE,
             67  '   Qty Issued',
             89  SY-VLINE,
             90  'Issued',
             98  SY-VLINE,
             99  'Recd.',
             114  SY-VLINE.
  WRITE :/5  'No',
           9 SY-VLINE,
*           10  'Material code',';','Artwork code',
 10  'Material code',
           52 SY-VLINE,
*            53 'Batch No.',
*           53 'Strg.Loc. ',
           66 SY-VLINE,
           67 '   I. D. No.',
           89 SY-VLINE,
           90 '  by',
           98 SY-VLINE,
           99 '  by',
           114 SY-VLINE.
  WRITE :/9 SY-VLINE,
           52 SY-VLINE,
           66 SY-VLINE,
           89 SY-VLINE,
           90 'Stores',
           98 SY-VLINE,
           99 'Packing',
           114 SY-VLINE.
  WRITE :/4 SY-ULINE.

ENDFORM.                    " WRITE_HEADER


*&---------------------------------------------------------------------*
*&      Form  write_header1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM WRITE_HEADER1.
  WRITE :/4  SY-ULINE.
  WRITE :/5  'Sr',
            9  SY-VLINE,
            10  'Description',
            52  SY-VLINE,
            53  'Insp Lot No',
            66  SY-VLINE,
            67  ' Qty Returned',
            89  SY-VLINE,
            90  'Returned',
            98  SY-VLINE,
            99 'Checked',
            106 SY-VLINE,
            107  'Recd.',
            114  SY-VLINE.

  WRITE :/5  'No',
           9 SY-VLINE,
*           10  'Material code',';','Artwork code',
   10  'Material code',
           52 SY-VLINE,
*            53 'Batch No.',
*           53 'Strg.Loc. ',
           66 SY-VLINE,
           67 '   I. D. No.',
           89 SY-VLINE,
           90 '  by',
           98 SY-VLINE,
           99 '  by',
           106 SY-VLINE,
           107 '  by',
           114 SY-VLINE.
  WRITE :/9 SY-VLINE,
           52 SY-VLINE,
           66 SY-VLINE,
           89 SY-VLINE,
           90 'Packing',
           98 SY-VLINE,
           99 'IPQA',
           106 SY-VLINE,
           107 'Stores',
           114 SY-VLINE.
  WRITE :/4 SY-ULINE.

ENDFORM.                    " WRITE_HEADER

*&---------------------------------------------------------------------*
*&      Form  WRITE_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_DETAILS.

  SORT T_DETAILS BY COUNTER POSNR.
  LOOP AT T_DETAILS WHERE AUFNR = T_HEADER-AUFNR.
*******************CHECK NOT TO PINT HALB AS PER REQUESTED BY skm 11.6.2013-------------
    SELECT SINGLE * FROM MARA WHERE MATNR EQ T_DETAILS-MATNR AND MTART NE 'ZHLB'.
    IF SY-SUBRC EQ 0.

*    WRITE : t_details-mtart.
      COUNTER_NO = COUNTER_NO + 1.

      WRITE :/5  COUNTER_NO,
              9 SY-VLINE,
              10  T_DETAILS-MAKTXC,
              52 SY-VLINE.
      IF T_DETAILS-PRUEFLOS NE 0.

        WRITE : 53  T_DETAILS-PRUEFLOS.
      ENDIF.
      WRITE : 66 SY-VLINE,
              67(16)  T_DETAILS-MENGE,
              85(3) T_DETAILS-MEINS,
              89 SY-VLINE,
              98 SY-VLINE.
      IF ORDISS EQ 'X'.
      ELSE.
        IF EXTISS NE 'X'.
          WRITE : 106 SY-VLINE.
        ENDIF.
      ENDIF.
      WRITE : 114 SY-VLINE.
      WRITE : /9 SY-VLINE,
               10(8) T_DETAILS-MATNR LEFT-JUSTIFIED.
      SELECT SINGLE * FROM ZPMS_ART_TABLE WHERE MATNR EQ T_DETAILS-MATNR AND FROM_DT LE SY-DATUM AND TO_DT GE SY-DATUM.
      IF SY-SUBRC EQ 0.
        CONCATENATE '(' ZPMS_ART_TABLE-ART_NO ')' INTO TEXT.
*        WRITE : 20 zpms_art_table-art_no LEFT-JUSTIFIED.
        WRITE : 20 TEXT LEFT-JUSTIFIED.
      ENDIF.

      WRITE :  52 SY-VLINE,
*             53 t_details-charg,
               53 T_DETAILS-LGORT,
               66 SY-VLINE,
               70 T_DETAILS-CHARG,
               89 SY-VLINE,
               98 SY-VLINE.
      IF ORDISS EQ 'X'.
      ELSE.
        IF EXTISS NE 'X'.
          WRITE:   106 SY-VLINE.
        ENDIF.
      ENDIF.
      WRITE :  114 SY-VLINE.

      WRITE :/4 SY-ULINE.
    ENDIF.
  ENDLOOP.
  LICOUN = 65 - SY-LINNO.
  DO LICOUN TIMES.
    SKIP 1.
  ENDDO.

ENDFORM.                    " WRITE_DETAILS

*&---------------------------------------------------------------------*
*&      Form  validation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VALIDATION.

  IF ( ORDISS = 'X' AND  P_AUFNR = ' ' ).
    MESSAGE E001(ZSD) WITH 'Give  Order Ref. '.
  ENDIF.

  IF ( EXTISS = 'X'  AND  P_MBLNR = ' ' ).
    MESSAGE E001(ZSD) WITH 'Give  Material Document  Ref. '.
  ENDIF.
  IF ( ISSRET = 'X'  AND  P_MBLNR = ' ' ).
    MESSAGE E001(ZSD) WITH 'Give  Material Document  Ref. '.
  ENDIF.
  IF P_MBLNR NE ' ' AND EXTISS = 'X'.
    SELECT SINGLE MBLNR BWART FROM MSEG INTO (W_MBLNR,W_BWART) WHERE
MBLNR  = P_MBLNR.
    IF SY-SUBRC <> 0.
      MESSAGE E001(ZSD) WITH 'Matl.Doc Ref.Doesnot exist.'.
    ENDIF.
    IF W_BWART <> '261'.
      MESSAGE E001(ZSD) WITH 'Mvmt type is not 261'.
    ENDIF.
  ENDIF.
  IF P_MBLNR NE ' ' AND ISSRET = 'X'.
    SELECT SINGLE MBLNR BWART FROM MSEG INTO (W_MBLNR,W_BWART) WHERE
MBLNR  = P_MBLNR.
    IF SY-SUBRC <> 0.
      MESSAGE E001(ZSD) WITH 'Matl.Doc Ref.Doesnot exist.'.
    ENDIF.
    IF W_BWART <> '262'.
      MESSAGE E001(ZSD) WITH 'Mvmt type is not 262'.
    ENDIF.
  ENDIF.

ENDFORM.                    "validation
