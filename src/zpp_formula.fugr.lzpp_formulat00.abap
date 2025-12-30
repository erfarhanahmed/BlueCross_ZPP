*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPP_FORMULA.....................................*
DATA:  BEGIN OF STATUS_ZPP_FORMULA                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPP_FORMULA                   .
CONTROLS: TCTRL_ZPP_FORMULA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPP_FORMULA                   .
TABLES: ZPP_FORMULA                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
