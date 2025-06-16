      ******************************************************************
      * DCLGEN TABLE(CLIENTES_MAPFRE)                                  *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBCLMFIN))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE CLIENTES_MAPFRE TABLE
           ( DNI                            CHAR(9) NOT NULL,
             NOMBRE                         CHAR(75) NOT NULL,
             DIRECCION                      CHAR(100) NOT NULL,
             TELEFONO                       CHAR(10) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CLIENTES_MAPFRE                    *
      ******************************************************************
       01  DCLCLIENTES-MAPFRE.
      *    *************************************************************
      *                       DNI
           10 TB-DNI               PIC X(9).
      *    *************************************************************
      *                       NOMBRE
           10 TB-NOMBRE            PIC X(75).
      *    *************************************************************
      *                       DIRECCION
           10 TB-DIRECCION         PIC X(100).
      *    *************************************************************
      *                       TELEFONO
           10 TB-TELEFONO          PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
