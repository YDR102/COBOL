      ******************************************************************
      * DCLGEN TABLE(SINIESTROS_PEPITO_SEG)                            *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBSINFIN))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE SINIESTROS_PEPITO_SEG TABLE
           ( ID_SINIETRO                    INTEGER NOT NULL,
             FECHA_SINIESTRO                DATE NOT NULL,
             CAUSAS                         CHAR(55) NOT NULL,
             ACPTADO                        CHAR(1) NOT NULL,
             INDEMNIZACION                  DECIMAL(15, 2) NOT NULL,
             NUMERO_POLIZA                  CHAR(9) NOT NULL,
             DNI_PERITO                     CHAR(9) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SINIESTROS_PEPITO_SEG              *
      ******************************************************************
       01  DCLSINIESTROS-PEPITO-SEG.
      *    *************************************************************
      *                       ID_SINIETRO
           10 TB-ID-SINIETRO       PIC S9(9) USAGE COMP.
      *    *************************************************************
      *                       FECHA_SINIESTRO
           10 TB-FECHA-SINIESTRO   PIC X(10).
      *    *************************************************************
      *                       CAUSAS
           10 TB-CAUSAS            PIC X(55).
      *    *************************************************************
      *                       ACPTADO
           10 TB-ACPTADO           PIC X(1).
      *    *************************************************************
      *                       INDEMNIZACION
           10 TB-INDEMNIZACION     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
      *                       NUMERO_POLIZA
           10 TB-NUMERO-POLIZA     PIC X(9).
      *    *************************************************************
      *                       DNI_PERITO
           10 TB-DNI-PERITO        PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
