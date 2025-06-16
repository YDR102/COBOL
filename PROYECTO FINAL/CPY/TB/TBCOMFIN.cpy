      ******************************************************************
      * DCLGEN TABLE(COMPANIAS_SEGUROS)                                *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBCOMFIN))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE COMPANIAS_SEGUROS TABLE
           ( ID                             INTEGER NOT NULL,
             NUMERO_POLIZA                  CHAR(9) NOT NULL,
             NOMBRE_COMPANIA                CHAR(25) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE COMPANIAS_SEGUROS                  *
      ******************************************************************
       01  DCLCOMPANIAS-SEGUROS.
      *    *************************************************************
      *                       ID
           10 TB-ID                PIC S9(9) USAGE COMP.
      *    *************************************************************
      *                       NUMERO_POLIZA
           10 TB-NUMERO-POLIZA     PIC X(9).
      *    *************************************************************
      *                       NOMBRE_COMPANIA
           10 TB-NOMBRE-COMPANIA   PIC X(25).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
