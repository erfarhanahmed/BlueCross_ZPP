REPORT zbcllpp_prod_stk_review2 NO STANDARD PAGE HEADING LINE-SIZE 250.
*LINE-SIZE 120.
*--------------------------------------------------------------------*
*--This report is for the product stock review                       *
* it is required for planning the batch for the production           *
*--Request given by Mr. Mahuli 01/2005 packaging & devlped by Anjali *
*--Started on 27.04.05                                               *
* CHANGED TO INCL STAS TABLE                                         *
*--------------------------------------------------------------------*

*--Table Declerations -----------------------------------------------*

TABLES : makt,
         marc,
         mara,
         mast,
         mchb,
         resb,
         stko,
         stas,
         stpo,
*         zpms_art_table,
         afru,
         afpo,
         zqspecification.

TYPE-POOLS:  slis.

DATA: g_repid     LIKE sy-repid,
      fieldcat    TYPE slis_t_fieldcat_alv,
      wa_fieldcat LIKE LINE OF fieldcat,
      sort        TYPE slis_t_sortinfo_alv,
      wa_sort     LIKE LINE OF sort,
      layout      TYPE slis_layout_alv.

*--Data Declerations ------------------------------------------------*
** to take the info for bom
DATA : it_afpo TYPE TABLE OF afpo,
       wa_afpo TYPE afpo,
       it_ekpo TYPE TABLE OF ekpo,
       wa_ekpo TYPE ekpo,
       it_eket TYPE TABLE OF eket,
       wa_eket TYPE eket.
DATA: it_afko TYPE TABLE OF afko,
      wa_afko TYPE afko,
      it_aufm TYPE TABLE OF aufm,
      wa_aufm TYPE aufm.

TYPES : BEGIN OF po1,
          matnr TYPE ekpo-matnr,
          menge TYPE p,
        END OF po1.

TYPES : BEGIN OF bat1,
          matnr TYPE ekpo-matnr,
          charg TYPE afpo-charg,
          mblnr TYPE aufm-mblnr,
          gstrp TYPE afko-gstrp,
        END OF bat1.

TYPES: BEGIN OF bom1,
         matnr      TYPE mast-matnr,
         maktx      TYPE makt-maktx,
         werks      TYPE mast-werks,
         stlal      TYPE mast-stlal,
         status(10) TYPE c,
         bmeng      TYPE stko-bmeng,
         bmein      TYPE stko-bmein,
         stktx      TYPE stko-stktx,
       END OF bom1.

TYPES: BEGIN OF bom2,
         matnr      TYPE mast-matnr,
         maktx      TYPE makt-maktx,
         werks      TYPE mast-werks,
         stlal      TYPE mast-stlal,
         status(10) TYPE c,
         bmeng      TYPE stko-bmeng,
         bmein      TYPE stko-bmein,
         stktx      TYPE stko-stktx,
         adatu      TYPE mkal-adatu,
         bdatu      TYPE mkal-bdatu,
         text1      TYPE mkal-text1,
       END OF bom2.

TYPES : BEGIN OF mrn1,
          idnrk TYPE mchb-matnr,
          clabs TYPE mchb-clabs,
        END OF mrn1.

TYPES : BEGIN OF mrn11,
          idnrk TYPE mchb-matnr,
          charg TYPE mchb-charg,
          clabs TYPE mchb-clabs,
        END OF mrn11.

DATA : it_po1 TYPE TABLE OF po1,
       wa_po1 TYPE po1.
DATA: it_bat1 TYPE TABLE OF bat1,
      wa_bat1 TYPE bat1.
DATA: it_bom1   TYPE TABLE OF bom1,
      wa_bom1   TYPE bom1,
      it_bom2   TYPE TABLE OF bom2,
      wa_bom2   TYPE bom2,
      it_mchb1  TYPE TABLE OF mchb,
      wa_mchb1  TYPE mchb,
      it_mrn1   TYPE TABLE OF mrn1,
      wa_mrn1   TYPE mrn1,
      it_mrn4   TYPE TABLE OF mrn1,
      wa_mrn4   TYPE mrn1,
      it_mrn11  TYPE TABLE OF mrn11,
      wa_mrn11  TYPE mrn11,
      it_mseg11 TYPE TABLE OF mseg,
      wa_mseg11 TYPE mseg,
      it_mrn14  TYPE TABLE OF mrn11,
      wa_mrn14  TYPE mrn11,
      it_mseg14 TYPE TABLE OF mseg,
      wa_mseg14 TYPE mseg.

DATA : BEGIN OF t_details OCCURS 0,
         stlnr LIKE stpo-stlnr,
         idnrk LIKE stpo-idnrk,
         meins LIKE stpo-meins,
         menge LIKE stpo-menge,
         cinsm LIKE mchb-cinsm,
         clabs LIKE mchb-clabs,
         cspem LIKE mchb-cspem,
         bdmng LIKE resb-bdmng,
       END OF t_details.

** to take the stk for a matnr.
DATA: BEGIN OF t_mchb OCCURS 0,
        matnr LIKE mchb-matnr,
        cinsm LIKE mchb-cinsm,
        clabs LIKE mchb-clabs,
        cspem LIKE mchb-cspem,
      END OF t_mchb.
** to take the reserved stk for a matnr
DATA : BEGIN OF t_resb OCCURS 0,
         matnr LIKE resb-matnr,
         werks LIKE resb-werks,
         bdmng LIKE resb-bdmng,
         enmng LIKE resb-enmng,
       END OF t_resb.

DATA : w_bmeng    LIKE stko-bmeng,
       w_stlnr    LIKE mast-stlnr,
       w_stlal    LIKE mast-stlal,
       w_menge    LIKE stpo-menge,
       w_avamenge LIKE stpo-menge,
       qty        TYPE resb-bdmng.

DATA : sdate1 TYPE sy-datum,
       sdate2 TYPE sy-datum,
       sdate3 TYPE sy-datum.

DATA : batch    TYPE afpo-charg,
       w_menge1 TYPE p,
       w_menge2 TYPE p.

DATA: it_mast TYPE TABLE OF mast,
      wa_mast TYPE mast.

DATA: ser_mast    TYPE TABLE OF mast,
      wa_ser_mast TYPE mast,
      it_mkal     TYPE TABLE OF mkal,
      wa_mkal     TYPE mkal.

DATA: count TYPE i.

TYPES: BEGIN OF typ_t001w,
         werks TYPE werks_d,
         name1 TYPE name1,
       END OF typ_t001w.

DATA : itab_t001w TYPE TABLE OF typ_t001w,
       wa_t001w   TYPE typ_t001w.
DATA :
*      mesg(40) type c,
      msg TYPE string.


*--Selection screen --------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001 .
PARAMETERS : s_matnr LIKE mast-matnr OBLIGATORY.
PARAMETERS : p_werks LIKE mast-werks OBLIGATORY.
PARAMETERS : p_bmeng LIKE stko-bmeng OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS : bom LIKE mast-stlal.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS : avlbom AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b3.

*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001 .
*PARAMETERS : PDATE1 LIKE sy-datum,
*             PDATE2 TYPE SY-DATUM.
*SELECTION-SCREEN END OF BLOCK b2.


INITIALIZATION.
  g_repid = sy-repid.

*at selection-screen.
*  perform authorization.
*
AT SELECTION-SCREEN.
  PERFORM authorization.
  PERFORM bomsearch.
*
*  PDATE2 = sy-datum.
*  PDATE1+6(2) = '01'.
*  PDATE1+4(2) = '04'.
*  PDATE1+0(4) = PDATE2+0(4) - 1.




*--Start-of-selection ------------------------------------------------*
START-OF-SELECTION.
  IF avlbom EQ 'X'.
    PERFORM avlbom1.
  ELSE.
    sdate1 = sy-datum.
    sdate2 = sy-datum - 180.
    sdate3 = sy-datum - 365.

*WRITE : / 'PO DATE',PDATE1,PDATE2.

    IF bom EQ space.
      MESSAGE 'ENTER BOM NUMBER' TYPE 'E'.
    ENDIF.

    PERFORM collect_data.
    PERFORM collect_stock.
    PERFORM batch_requirement.
    PERFORM print_para.
  ENDIF.
*---------------------------------------------------------------------*
*       FORM collect_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM collect_data.
  SELECT SINGLE * FROM mast WHERE matnr = s_matnr AND werks = p_werks AND stlan = '1'  AND stlal EQ bom.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM stko WHERE stlnr EQ mast-stlnr AND stlal EQ mast-stlal AND stlst EQ '02'.
    IF sy-subrc EQ 0.
      MESSAGE 'THIS BOM IS INACTIVE, SELECT OTHER BOM NUMBER' TYPE 'E'.
    ENDIF.
    SELECT SINGLE * FROM stko WHERE stlnr EQ mast-stlnr AND stlal EQ mast-stlal AND stlst EQ '01'.
    IF sy-subrc EQ 4.
      MESSAGE 'INVALID BOM NUMBER' TYPE 'E'.
    ENDIF.
  ELSE.
    MESSAGE 'INVALID BOM NUMBER' TYPE 'E'.
  ENDIF.

*  select * from mast where matnr = s_matnr and werks = p_werks and stlan = '1'  and stlal eq bom.
  SELECT * FROM mast INTO TABLE it_mast  WHERE matnr = s_matnr AND werks = p_werks AND stlan = '1'  AND stlal EQ bom.
  LOOP AT it_mast INTO wa_mast.
    SELECT SINGLE * FROM stko WHERE stlnr EQ wa_mast-stlnr AND stlal EQ wa_mast-stlal AND stlst EQ '01'.
    IF sy-subrc EQ 0.
      w_stlnr = wa_mast-stlnr.
      w_stlal = wa_mast-stlal.
      SELECT SINGLE *  FROM marc  WHERE matnr = wa_mast-matnr AND werks =  p_werks.
      IF marc-lvorm = 'X'.
      ELSE.
        SELECT * FROM stas WHERE stlnr = wa_mast-stlnr AND stlal = wa_mast-stlal.
          IF sy-subrc EQ 0.
            SELECT SINGLE * FROM stpo WHERE stlnr = wa_mast-stlnr AND stlkn =  stas-stlkn.
            IF sy-subrc EQ 0.
              SELECT SINGLE * FROM mara WHERE matnr = stpo-idnrk.
              IF mara-mtart = 'ZHLB'.
              ELSE.
                MOVE-CORRESPONDING stpo TO t_details.
                COLLECT t_details.
              ENDIF.
              CLEAR t_details.
            ENDIF.
          ENDIF.
        ENDSELECT.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "collect_data


*---------------------------------------------------------------------*
*       FORM collect_stock                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM collect_stock.
*  if t_details IS INITIAL.
*    MESSAGE 'BOM IS INACTIVE' TYPE 'E'.
*  ENDIF.

  LOOP AT t_details.
    SELECT * FROM mchb WHERE matnr = t_details-idnrk AND werks = p_werks AND lgort NE 'MRN1' AND lgort NE 'MRN4'.
      IF sy-subrc = '0'.
        MOVE-CORRESPONDING mchb TO t_mchb.
        COLLECT t_mchb.
      ENDIF.
      CLEAR t_mchb.
    ENDSELECT.
    SELECT *  FROM resb WHERE matnr = t_details-idnrk AND werks =
 p_werks AND kzear NE 'X'  AND xloek NE 'X' AND bdart  = 'AR'  AND charg
 NE '         '.
      IF sy-subrc EQ 0.
        SELECT SINGLE * FROM afru WHERE aufnr EQ resb-aufnr.
        IF sy-subrc NE 0.
*          WRITE : / '*',RESB-AUFNR.
          MOVE-CORRESPONDING resb TO t_resb.
          COLLECT t_resb.
          CLEAR t_resb.
        ENDIF.
      ENDIF.
    ENDSELECT.
  ENDLOOP.
ENDFORM.                    "collect_stock


*---------------------------------------------------------------------*
*       FORM batch_requirement                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM batch_requirement.
  SELECT SINGLE  bmeng FROM stko INTO w_bmeng WHERE  stlnr = w_stlnr AND STLAL EQ BOM.
  LOOP AT t_details.
    w_menge = ( t_details-menge / w_bmeng ) * p_bmeng.
    MOVE w_menge TO t_details-menge.
    MODIFY  t_details.
  ENDLOOP.
ENDFORM.                    "batch_requirement


*---------------------------------------------------------------------*
*       FORM print_para                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM print_para.
***************** PENDING ORDER*************************


  IF t_details IS NOT INITIAL.
    SELECT * FROM ekpo INTO TABLE it_ekpo FOR ALL ENTRIES IN t_details WHERE matnr EQ t_details-idnrk
      AND werks EQ p_werks AND elikz NE 'X' AND loekz EQ ' ' AND statu NE 'A'..
    IF sy-subrc EQ 0.
*        write : / ekpo-ebeln,EKPO-EBELP,ekpo-matnr,ekpo-menge.
      SELECT * FROM eket INTO TABLE it_eket FOR ALL ENTRIES IN it_ekpo WHERE ebeln EQ it_ekpo-ebeln AND
         ebelp EQ it_ekpo-ebelp.
    ENDIF.
  ENDIF.

  IF it_eket IS NOT INITIAL.
    LOOP AT it_eket INTO wa_eket.
      CLEAR : w_menge1,w_menge2.
*    WRITE : / 'PO',WA_EKET-EBELN,WA_EKET-EBELP.
      w_menge1 = wa_eket-menge - wa_eket-wemng.
      w_menge2 = w_menge2 + w_menge1.
      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebelp = wa_eket-ebelp ebeln = wa_eket-ebeln.
      IF sy-subrc EQ 0.
*       WRITE : WA_EKPO-MATNR.
        wa_po1-matnr = wa_ekpo-matnr.
      ENDIF.
      wa_po1-menge = w_menge2.
*   WRITE : W_MENGE2.
      COLLECT wa_po1 INTO it_po1.
      CLEAR wa_po1.
    ENDLOOP.
  ENDIF.
*WRITE : / 'PENDING ORDER', W_MENGE2.

*****************LAST BATCH******************
*  select * from afpo into table it_afpo where matnr eq s_matnr
**    dnrel ne 'X'
*    and pwerk eq p_werks and ltrmi le sdate1 and ltrmi ge sdate2.
*  if sy-subrc ne 0.
*    select * from afpo into table it_afpo where matnr eq s_matnr and pwerk eq p_werks and ltrmi le sdate1 and ltrmi ge sdate3.
*  endif.
*  sort it_afpo descending by ltrmi.
*  sort it_afpo descending by ltrmi charg.  "6.8.20
*  read table it_afpo into wa_afpo with key matnr = s_matnr.
*  if sy-subrc eq 0.
**    BATCH = WA_AFPO-CHARG.
*  endif.
***********************
  SELECT * FROM afko INTO TABLE it_afko WHERE plnbez EQ s_matnr AND gstrp LE sdate1 AND gstrp GE sdate2.
  IF sy-subrc NE 0.
    SELECT * FROM afko INTO TABLE it_afko WHERE plnbez EQ s_matnr AND gstrp LE sdate1 AND gstrp GE sdate3.
  ENDIF.
  IF it_afko IS NOT INITIAL.
    SELECT * FROM aufm INTO TABLE it_aufm FOR ALL ENTRIES IN it_afko WHERE aufnr EQ it_afko-aufnr AND bwart EQ '261'.
  ENDIF.
  SORT it_afko DESCENDING .
  SORT it_aufm DESCENDING .  "6.8.20
  LOOP AT it_afko INTO wa_afko.
    SELECT SINGLE * FROM afpo WHERE aufnr EQ wa_afko-aufnr .
*      and dnrel eq space.
    IF sy-subrc EQ 0.
      READ TABLE it_aufm INTO wa_aufm WITH KEY aufnr = wa_afko-aufnr.
      IF sy-subrc EQ 0.
        wa_bat1-matnr = afpo-matnr.
        wa_bat1-charg = afpo-charg.
        wa_bat1-mblnr = wa_aufm-mblnr.
        wa_bat1-gstrp = wa_afko-gstrp.
        COLLECT wa_bat1 INTO it_bat1.
        CLEAR wa_bat1.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  SORT IT_BAT1 DESCENDING BY MBLNR.
  SORT it_bat1 DESCENDING BY gstrp.  "START DATE 27.9.22

  READ TABLE it_bat1 INTO wa_bat1 WITH KEY matnr = s_matnr.
  IF sy-subrc EQ 0.
    batch = wa_bat1-charg.
  ENDIF.


*  READ TABLE IT_AFkO INTO WA_AFPO WITH KEY MATNR = S_MATNR.
*  IF SY-SUBRC EQ 0.
*    BATCH = WA_AFPO-CHARG.
*  ENDIF.

**************************************************

  SORT t_details BY idnrk.
  WRITE :/1 'PRODUCT STOCK REVIEW. ' , '        BOM No:',bom,80 'Date:',sy-datum, ', Time:',sy-uzeit.
  SELECT SINGLE * FROM makt WHERE matnr = s_matnr.
  WRITE :/1 'PRODUCT     :',
          18 s_matnr LEFT-JUSTIFIED,
          40(30)  makt-maktx LEFT-JUSTIFIED.


  WRITE :/1 'STD.BATCH SZ:',
          18 w_bmeng LEFT-JUSTIFIED,
         40 'REQUIRED BATCH SZ: ' ,
         60 p_bmeng,
          / 'LAST BATCH DISPENSED  :', batch LEFT-JUSTIFIED.
  ULINE.
  WRITE :/1 'MATL.CD',
          11 'ARTWORK NO.',
          24 'PMS. NO.',
          38 '    DESCRIPTION',
          75 'QTY FOR',
          87 '       STOCK AVAILABLE ',
            185 'PART_OF_UNRESTRICTED_STOCK',
           219 'PART_OF_UNRESTRICTED_STOCK'.


  WRITE :/75 'REQ.BATCH',
          97 'UNRESTRICT',
          109 'QUALITY',
          121 'BLOCK  ',
          133 'RESERVED',
          146 'AVAILABLE',
          159 'ON ORDER STOCK',
           186 'MRN1 STOCK',
           199 'MRN1 BATCH',
          219 'MRN4 STOCK',
          239 'MRN4 BATCH'.


  ULINE.
  PERFORM mrnstock.
  LOOP AT t_details.
    SELECT SINGLE * FROM makt WHERE matnr = t_details-idnrk.
    READ TABLE t_mchb WITH KEY matnr = t_details-idnrk.
    IF sy-subrc = '4'.
      t_mchb-clabs = 0.
      t_mchb-clabs = 0.
      t_mchb-cinsm = 0.
      t_mchb-cspem = 0.

    ENDIF.
    READ TABLE t_resb WITH KEY matnr = t_details-idnrk.
    IF sy-subrc = '4'.
      t_resb-bdmng = 0.
      t_resb-enmng = 0.
    ENDIF.
    w_avamenge = t_mchb-clabs - ( t_resb-bdmng - t_resb-enmng ).
    IF t_details-menge > w_avamenge.
      FORMAT COLOR COL_NEGATIVE.
    ELSE.
      FORMAT COLOR COL_NORMAL.
    ENDIF.
    WRITE :/1(8) t_details-idnrk.
*    select single * from zpms_art_table where matnr eq t_details-idnrk and to_dt ge sy-datum.
    SELECT SINGLE * FROM zqspecification WHERE matnr EQ t_details-idnrk AND werks EQ p_werks AND effectenddt GE sy-datum.
    IF sy-subrc EQ 0.
      WRITE : 10(12) zqspecification-artwork,24(12) zqspecification-specification.
    ENDIF.
    CLEAR : qty.
    qty = t_resb-bdmng - t_resb-enmng.
    WRITE : 38(35) makt-maktx,
***   75(10) T_DETAILS-MENGE,
***   87(11) T_MCHB-CLABS,
***   99(10) T_MCHB-CINSM,
***   111(10) T_MCHB-CSPEM,
****   123(10) T_RESB-BDMNG,
***   123(10) QTY,
***   136(10) W_AVAMENGE.
***    READ TABLE IT_PO1 INTO WA_PO1 WITH KEY MATNR = T_DETAILS-IDNRK.
***    IF SY-SUBRC EQ 0.
***      WRITE : 149 WA_PO1-MENGE.
***    ENDIF.

      75(20) t_details-menge,
   97(11) t_mchb-clabs,
   109(10) t_mchb-cinsm,
   121(10) t_mchb-cspem,
*   123(10) T_RESB-BDMNG,
   133(10) qty,
   146(10) w_avamenge.
    READ TABLE it_po1 INTO wa_po1 WITH KEY matnr = t_details-idnrk.
    IF sy-subrc EQ 0.
      WRITE : 159 wa_po1-menge.
    ENDIF.
    READ TABLE it_mrn1 INTO wa_mrn1 WITH KEY idnrk = t_details-idnrk.
    IF sy-subrc EQ 0.
      WRITE : 179 wa_mrn1-clabs.
      READ TABLE it_mseg11 INTO wa_mseg11 WITH KEY werks = p_werks.
      IF sy-subrc EQ 0.
        SELECT SINGLE * FROM afpo WHERE aufnr EQ wa_mseg11-aufnr.
        IF sy-subrc EQ 0.
          WRITE : 199 afpo-charg.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE it_mrn4 INTO wa_mrn4 WITH KEY idnrk = t_details-idnrk.
    IF sy-subrc EQ 0.
      WRITE : 219 wa_mrn4-clabs.
      READ TABLE it_mseg14 INTO wa_mseg14 WITH KEY werks = p_werks.
      IF sy-subrc EQ 0.
        SELECT SINGLE * FROM afpo WHERE aufnr EQ wa_mseg14-aufnr.
        IF sy-subrc EQ 0.
          WRITE : 239 afpo-charg.
        ENDIF.
      ENDIF.
    ENDIF.

*    write : 24(35) MAKT-MAKTX,
*    62(10) T_DETAILS-MENGE,
*    75(10) T_MCHB-CLABS,
*    88(10) T_MCHB-CINSM,
*    101(10) T_MCHB-CSPEM,
*    113(10) T_RESB-BDMNG,
*    126(10) w_avamenge.
  ENDLOOP.
ENDFORM.                    "print_para
*&---------------------------------------------------------------------*
*&      Form  BOMSEARCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bomsearch .
  IF avlbom IS INITIAL.
    IF bom IS INITIAL.
      SELECT * FROM mast INTO TABLE ser_mast WHERE matnr = s_matnr AND werks = p_werks AND stlan = '1' .
      CLEAR : count.
      count = 0.
      LOOP AT ser_mast INTO wa_ser_mast.
        SELECT SINGLE * FROM stko WHERE stlnr EQ wa_ser_mast-stlnr AND stlal EQ wa_ser_mast-stlal AND stlst EQ '01'.
        IF sy-subrc EQ 0.
          bom = wa_ser_mast-stlal.
          count = count + 1.
        ENDIF.
      ENDLOOP.
      IF count GT 1.
        bom = space.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AVLBOM1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM avlbom1 .
  SELECT * FROM mast INTO TABLE ser_mast WHERE matnr = s_matnr AND werks = p_werks AND stlan = '1' .
  IF sy-subrc EQ 0.
    SELECT * FROM mkal INTO TABLE it_mkal FOR ALL ENTRIES IN ser_mast WHERE matnr EQ ser_mast-matnr AND werks = ser_mast-werks AND stlan = '1' AND
      stlal EQ ser_mast-stlal.
  ENDIF.
  LOOP AT ser_mast INTO wa_ser_mast.
    wa_bom1-matnr = wa_ser_mast-matnr.
    SELECT SINGLE * FROM makt WHERE matnr EQ wa_ser_mast-matnr AND spras EQ 'EN'.
    IF sy-subrc EQ 0.
      wa_bom1-maktx = makt-maktx.
    ENDIF.
    wa_bom1-werks = wa_ser_mast-werks.
    wa_bom1-stlal = wa_ser_mast-stlal.
    SELECT SINGLE * FROM stko WHERE stlnr EQ wa_ser_mast-stlnr AND stlal EQ wa_ser_mast-stlal AND stlst EQ '01'.
    IF sy-subrc EQ 0.
      wa_bom1-status = 'ACTIVE'.
      wa_bom1-bmeng = stko-bmeng.
      wa_bom1-bmein = stko-bmein.
      wa_bom1-stktx = stko-stktx.
    ELSE.
      wa_bom1-status = 'INACTIVE'.
    ENDIF.
    COLLECT wa_bom1 INTO it_bom1.
    CLEAR wa_bom1.
  ENDLOOP.

*  LOOP AT it_bom1 INTO wa_bom1.
**    WRITE : / WA_BOM1-MATNR,WA_BOM1-WERKS,WA_BOM1-STLAL,WA_BOM1-STATUS,WA_BOM1-BMENG,WA_BOM1-BMEIN,WA_BOM1-STKTX.
*    LOOP AT it_mkal INTO wa_mkal WHERE matnr EQ wa_bom1-matnr AND werks EQ wa_bom1-werks AND stlal EQ wa_bom1-stlal.
**      WRITE : / WA_MKAL-ADATU,WA_MKAL-BDATU,WA_MKAL-TEXT1.
*      wa_bom2-matnr = wa_bom1-matnr.
*      wa_bom2-maktx = wa_bom1-maktx.
*      wa_bom2-werks = wa_bom1-werks.
*      wa_bom2-stlal = wa_bom1-stlal.
*      wa_bom2-status = wa_bom1-status.
*      wa_bom2-bmeng = wa_bom1-bmeng.
*      wa_bom2-bmein = wa_bom1-bmein.
*      wa_bom2-stktx = wa_bom1-stktx.
*      wa_bom2-adatu = wa_mkal-adatu.
*      wa_bom2-bdatu = wa_mkal-bdatu.
*      wa_bom2-text1 = wa_mkal-text1.
*      COLLECT wa_bom2 INTO it_bom2.
*      CLEAR wa_bom2.
*    ENDLOOP.
*  ENDLOOP.  cmt by sakshi

  LOOP AT it_bom1 INTO wa_bom1.
*    READ TABLE it_mkal INTO wa_mkal WITH KEY matnr = wa_bom1-matnr werks = wa_bom1-werks stlal = wa_bom1-stlal.
*    IF sy-subrc EQ 4.
      wa_bom2-matnr = wa_bom1-matnr.
      wa_bom2-maktx = wa_bom1-maktx.
      wa_bom2-werks = wa_bom1-werks.
      wa_bom2-stlal = wa_bom1-stlal.
      wa_bom2-status = wa_bom1-status.
      wa_bom2-bmeng = wa_bom1-bmeng.
      wa_bom2-bmein = wa_bom1-bmein.
      wa_bom2-stktx = wa_bom1-stktx.
      wa_bom2-adatu = space.
      wa_bom2-bdatu = space.
      wa_bom2-text1 = space.
      COLLECT wa_bom2 INTO it_bom2.
      CLEAR wa_bom2.
*    ENDIF.
  ENDLOOP.

*  LOOP AT IT_BOM2 INTO WA_BOM2.
*    WRITE : /'***', WA_BOM2-MATNR,WA_BOM2-MAKTX,WA_BOM2-WERKS,WA_BOM2-STLAL,WA_BOM2-STATUS,WA_BOM2-BMENG,WA_BOM2-BMEIN,
*    WA_BOM2-STKTX,WA_BOM2-ADATU, WA_BOM2-BDATU,WA_BOM2-TEXT1.
*  ENDLOOP.

  wa_fieldcat-fieldname = 'MATNR'.
  wa_fieldcat-seltext_l = 'PRODUCT CODE'.
  APPEND wa_fieldcat TO fieldcat.

  wa_fieldcat-fieldname = 'MAKTX'.
  wa_fieldcat-seltext_l = 'PRODUCT NAME'.
  APPEND wa_fieldcat TO fieldcat.

  wa_fieldcat-fieldname = 'WERKS'.
  wa_fieldcat-seltext_l = 'PLANT'.
  APPEND wa_fieldcat TO fieldcat.

  wa_fieldcat-fieldname = 'STLAL'.
  wa_fieldcat-seltext_l = 'BOM No.'.
  APPEND wa_fieldcat TO fieldcat.

  wa_fieldcat-fieldname = 'STATUS'.
  wa_fieldcat-seltext_l = 'BOM Status'.
  APPEND wa_fieldcat TO fieldcat.

  wa_fieldcat-fieldname = 'BMENG'.
  wa_fieldcat-seltext_l = 'Base Qty'.
  APPEND wa_fieldcat TO fieldcat.

  wa_fieldcat-fieldname = 'BMEIN'.
  wa_fieldcat-seltext_l = 'Qty Unit'.
  APPEND wa_fieldcat TO fieldcat.

  wa_fieldcat-fieldname = 'STKTX'.
  wa_fieldcat-seltext_l = 'BOM Text'.
  APPEND wa_fieldcat TO fieldcat.

  wa_fieldcat-fieldname = 'ADATU'.
  wa_fieldcat-seltext_l = 'BOM Valid From'.
  APPEND wa_fieldcat TO fieldcat.

  wa_fieldcat-fieldname = 'BDATU'.
  wa_fieldcat-seltext_l = 'BOM Valid To'.
  APPEND wa_fieldcat TO fieldcat.

  wa_fieldcat-fieldname = 'TEXT1'.
  wa_fieldcat-seltext_l = 'Effective Batch'.
  APPEND wa_fieldcat TO fieldcat.



  layout-zebra = 'X'.
  layout-colwidth_optimize = 'X'.
  layout-window_titlebar  = 'BOM DETAILS'.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK       = ' '
*     I_BYPASSING_BUFFER      = ' '
*     I_BUFFER_ACTIVE         = ' '
      i_callback_program      = g_repid
*     I_CALLBACK_PF_STATUS_SET          = ' '
      i_callback_user_command = 'USER_COMM'
      i_callback_top_of_page  = 'TOP'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME        =
*     I_BACKGROUND_ID         = ' '
*     I_GRID_TITLE            =
*     I_GRID_SETTINGS         =
      is_layout               = layout
      it_fieldcat             = fieldcat
*     IT_EXCLUDING            =
*     IT_SPECIAL_GROUPS       =
*     IT_SORT                 =
*     IT_FILTER               =
*     IS_SEL_HIDE             =
*     I_DEFAULT               = 'X'
      i_save                  = 'A'
*     IS_VARIANT              =
*     IT_EVENTS               =
*     IT_EVENT_EXIT           =
*     IS_PRINT                =
*     IS_REPREP_ID            =
*     I_SCREEN_START_COLUMN   = 0
*     I_SCREEN_START_LINE     = 0
*     I_SCREEN_END_COLUMN     = 0
*     I_SCREEN_END_LINE       = 0
*     I_HTML_HEIGHT_TOP       = 0
*     I_HTML_HEIGHT_END       = 0
*     IT_ALV_GRAPHICS         =
*     IT_HYPERLINK            =
*     IT_ADD_FIELDCAT         =
*     IT_EXCEPT_QINFO         =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER =
*     ES_EXIT_CAUSED_BY_USER  =
    TABLES
      t_outtab                = it_bom2
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "SUMMARY

*&---------------------------------------------------------------------*
*&      Form  TOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top.

  DATA: comment    TYPE slis_t_listheader,
        wa_comment LIKE LINE OF comment.

  wa_comment-typ = 'A'.
  wa_comment-info = 'BOM DETAILS'.
*  WA_COMMENT-INFO = P_FRMDT.
  APPEND wa_comment TO comment.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = comment
*     I_LOGO             = 'ENJOYSAP_LOGO'
*     I_END_OF_LIST_GRID =
*     I_ALV_FORM         =
    .

  CLEAR comment.

ENDFORM.                    "TOP



*&---------------------------------------------------------------------*
*&      Form  USER_COMM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
FORM user_comm USING ucomm LIKE sy-ucomm
                     selfield TYPE slis_selfield.



  CASE selfield-fieldname.
    WHEN 'MATNR'.
*      SET PARAMETER ID 'MAT' FIELD SELFIELD-VALUE.
*      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      SET PARAMETER ID 'MAT' FIELD selfield-value.
      CALL TRANSACTION 'CS03' AND SKIP FIRST SCREEN.
    WHEN 'VBELN1'.
      SET PARAMETER ID 'BV' FIELD selfield-value.
      CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "USER_COMM



*&---------------------------------------------------------------------*
*&      Form  authorization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM authorization .

  SELECT werks name1 FROM t001w INTO TABLE itab_t001w WHERE werks EQ p_werks.

  LOOP AT itab_t001w INTO wa_t001w.
    AUTHORITY-CHECK OBJECT 'M_BCO_WERK'
           ID 'WERKS' FIELD wa_t001w-werks.
    IF sy-subrc <> 0.
      CONCATENATE 'No authorization for Plant' wa_t001w-werks INTO msg
      SEPARATED BY space.
      MESSAGE msg TYPE 'E'.
    ENDIF.
  ENDLOOP.


ENDFORM.                    "authorization


*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MRNSTOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mrnstock.
  CLEAR : it_mchb1,wa_mchb1,it_mrn1,wa_mrn1,it_mrn4,wa_mrn4.
  SELECT * FROM mchb INTO TABLE it_mchb1 FOR ALL ENTRIES IN t_details WHERE matnr = t_details-idnrk AND werks EQ p_werks AND clabs GT 0.

  LOOP AT it_mchb1 INTO wa_mchb1 WHERE lgort EQ 'MRN1'.
    wa_mrn1-idnrk = wa_mchb1-matnr.
    wa_mrn1-clabs = wa_mchb1-clabs.
    COLLECT wa_mrn1 INTO it_mrn1.
    CLEAR wa_mrn1.
  ENDLOOP.

  LOOP AT it_mchb1 INTO wa_mchb1 WHERE lgort EQ 'MRN4'.
    wa_mrn4-idnrk = wa_mchb1-matnr.
    wa_mrn4-clabs = wa_mchb1-clabs.
    COLLECT wa_mrn4 INTO it_mrn4.
    CLEAR wa_mrn4.
  ENDLOOP.

*************CHECK MRN BATCH************************
  LOOP AT it_mchb1 INTO wa_mchb1 WHERE lgort EQ 'MRN1'.
    wa_mrn11-idnrk = wa_mchb1-matnr.
    wa_mrn11-charg = wa_mchb1-charg.
    wa_mrn11-clabs = wa_mchb1-clabs.
    COLLECT wa_mrn11 INTO it_mrn11.
    CLEAR wa_mrn11.
  ENDLOOP.

  IF it_mrn11 IS NOT INITIAL.
    SELECT * FROM mseg INTO TABLE it_mseg11 FOR ALL ENTRIES IN it_mrn11 WHERE matnr EQ it_mrn11-idnrk AND charg EQ it_mrn11-charg AND bwart EQ '262'.
  ENDIF.
  SORT it_mseg11 DESCENDING BY mblnr.
*******************************************MRN******
  LOOP AT it_mchb1 INTO wa_mchb1 WHERE lgort EQ 'MRN4'.
    wa_mrn14-idnrk = wa_mchb1-matnr.
    wa_mrn14-charg = wa_mchb1-charg.
    wa_mrn14-clabs = wa_mchb1-clabs.
    COLLECT wa_mrn14 INTO it_mrn14.
    CLEAR wa_mrn14.
  ENDLOOP.

  IF it_mrn14 IS NOT INITIAL.
    SELECT * FROM mseg INTO TABLE it_mseg14 FOR ALL ENTRIES IN it_mrn14 WHERE matnr EQ it_mrn14-idnrk AND charg EQ it_mrn14-charg AND bwart EQ '262'.
  ENDIF.
  SORT it_mseg14 DESCENDING BY mblnr.




ENDFORM.
