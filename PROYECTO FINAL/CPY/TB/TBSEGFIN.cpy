      ******************************************************************
      * DCLGEN TABLE(SEGUROS_PEPITO_SEG)                               *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBSEGFIN))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE SEGUROS_PEPITO_SEG TABLE
           ( NUMERO_POLIZA                  CHAR(9) NOT NULL,
             TIPO                           CHAR(2) NOT NULL,
             FECHA_INICIO                   DATE NOT NULL,
             FECHA_VENCIMIENTO              DATE NOT NULL,
             COND_PART                      VARCHAR(2000) NOT NULL,
             OBSERVACIONES                  VARCHAR(500),
             DNI_CL                         CHAR(9) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SEGUROS_PEPITO_SEG                 *
      ******************************************************************
       01  DCLSEGUROS-PEPITO-SEG.
      *    *************************************************************
      *                       NUMERO_POLIZA
           10 TB-NUMERO-POLIZA     PIC X(9).
      *    *************************************************************
      *                       TIPO
           10 TB-TIPO              PIC X(2).
      *    *************************************************************
      *                       FECHA_INICIO
           10 TB-FECHA-INICIO      PIC X(10).
      *    *************************************************************
      *                       FECHA_VENCIMIENTO
           10 TB-FECHA-VENCIMIENTO
              PIC X(10).
      *    *************************************************************
           10 TB-COND-PART.
      *                       COND_PART LENGTH
              49 TB-COND-PART-LEN
                 PIC S9(4) USAGE COMP.
      *                       COND_PART
              49 TB-COND-PART-TEXT
                 PIC X(2000).
      *    *************************************************************
           10 TB-OBSERVACIONES.
      *                       OBSERVACIONES LENGTH
              49 TB-OBSERVACIONES-LEN
                 PIC S9(4) USAGE COMP.
      *                       OBSERVACIONES
              49 TB-OBSERVACIONES-TEXT
                 PIC X(500).
      *    *************************************************************
      *                       DNI_CL
           10 TB-DNI-CL            PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
