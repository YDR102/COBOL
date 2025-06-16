      ******************************************************************
      * DCLGEN TABLE(DIRELEC)                                          *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBDIRELE))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DIRELEC TABLE
           ( TIPO_DIR_ELEC                  CHAR(3) NOT NULL,
             VALOR                          CHAR(20) NOT NULL,
             COD_CLIENTE                    CHAR(9) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DIRELEC                            *
      ******************************************************************
       01  DCLDIRELEC.
      *    *************************************************************
      *                       TIPO_DIR_ELEC
           10 TB-TIPO-DIR-ELEC     PIC X(3).
      *    *************************************************************
      *                       VALOR
           10 TB-VALOR             PIC X(20).
      *    *************************************************************
      *                       COD_CLIENTE
           10 TB-COD-CLIENTE       PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
