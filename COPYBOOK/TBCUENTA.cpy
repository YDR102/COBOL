      ******************************************************************
      * DCLGEN TABLE(CUENTAS)                                          *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBCUENTA))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE CUENTAS TABLE
           ( BANCO                          CHAR(4) NOT NULL,
             OFICINA                        CHAR(4) NOT NULL,
             DC                             CHAR(2) NOT NULL,
             NUM_CUENTA                     CHAR(10) NOT NULL,
             TITULAR                        CHAR(10) NOT NULL,
             SALDO                          DECIMAL(17, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CUENTAS                            *
      ******************************************************************
       01  DCLCUENTAS.
      *    *************************************************************
      *                       BANCO
           10 TB-BANCO             PIC X(4).
      *    *************************************************************
      *                       OFICINA
           10 TB-OFICINA           PIC X(4).
      *    *************************************************************
      *                       DC
           10 TB-DC                PIC X(2).
      *    *************************************************************
      *                       NUM_CUENTA
           10 TB-NUM-CUENTA        PIC X(10).
      *    *************************************************************
      *                       TITULAR
           10 TB-TITULAR           PIC X(10).
      *    *************************************************************
      *                       SALDO
           10 TB-SALDO             PIC S9(15)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
