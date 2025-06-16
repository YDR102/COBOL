      ******************************************************************
      * DCLGEN TABLE(BANCO_PICHINCHA)                                  *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBBANCO))                   *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE BANCO_PICHINCHA TABLE
           ( NUM_CUENTA                     CHAR(20) NOT NULL,
             ID_CLIENTE                     CHAR(10) NOT NULL,
             NOMBRE                         CHAR(20) NOT NULL,
             APELLIDO1                      CHAR(20) NOT NULL,
             APELLIDO2                      CHAR(20),
             IMPORTE                        DECIMAL(17, 2) NOT NULL,
             DIVISA                         CHAR(3) NOT NULL,
             CIUDAD                         CHAR(20) NOT NULL,
             PAIS                           CHAR(20) NOT NULL,
             EDAD                           DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BANCO_PICHINCHA                    *
      ******************************************************************
       01  DCLBANCO-PICHINCHA.
      *    *************************************************************
      *                       NUM_CUENTA
           10 TB-NUM-CUENTA        PIC X(20).
      *    *************************************************************
      *                       ID_CLIENTE
           10 TB-ID-CLIENTE        PIC X(10).
      *    *************************************************************
      *                       NOMBRE
           10 TB-NOMBRE            PIC X(20).
      *    *************************************************************
      *                       APELLIDO1
           10 TB-APELLIDO1         PIC X(20).
      *    *************************************************************
      *                       APELLIDO2
           10 TB-APELLIDO2         PIC X(20).
      *    *************************************************************
      *                       IMPORTE
           10 TB-IMPORTE           PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
      *                       DIVISA
           10 TB-DIVISA            PIC X(3).
      *    *************************************************************
      *                       CIUDAD
           10 TB-CIUDAD            PIC X(20).
      *    *************************************************************
      *                       PAIS
           10 TB-PAIS              PIC X(20).
      *    *************************************************************
      *                       EDAD
           10 TB-EDAD              PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
