      ******************************************************************
      * DCLGEN TABLE(PROVINCIAS)                                       *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBPROVIN))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE PROVINCIAS TABLE
           ( PREFIJO                        CHAR(2) NOT NULL,
             PROVINCIA                      CHAR(20) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE PROVINCIAS                         *
      ******************************************************************
       01  DCLPROVINCIAS.
      *    *************************************************************
      *                       PREFIJO
           10 TB-PREFIJO           PIC X(2).
      *    *************************************************************
      *                       PROVINCIA
           10 TB-PROVINCIA         PIC X(20).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
