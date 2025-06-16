      ******************************************************************
      * DCLGEN TABLE(HOGAR_MAPFRE)                                     *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBHOGFIN))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE HOGAR_MAPFRE TABLE
           ( POLIZA                         CHAR(9) NOT NULL,
             PRIMA                          DECIMAL(15, 2) NOT NULL,
             CONTENIENTE                    DECIMAL(15, 2) NOT NULL,
             CONTENIDO                      DECIMAL(15, 2) NOT NULL,
             COBERTURAS                     VARCHAR(500),
             FECHA_INICIO                   DATE NOT NULL,
             FECHA_VENCIMIENTO              DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE HOGAR_MAPFRE                       *
      ******************************************************************
       01  DCLHOGAR-MAPFRE.
      *    *************************************************************
      *                       POLIZA
           10 TB-POLIZA            PIC X(9).
      *    *************************************************************
      *                       PRIMA
           10 TB-PRIMA             PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
      *                       CONTENIENTE
           10 TB-CONTENIENTE       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
      *                       CONTENIDO
           10 TB-CONTENIDO         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 TB-COBERTURAS.
      *                       COBERTURAS LENGTH
              49 TB-COBERTURAS-LEN
                 PIC S9(4) USAGE COMP.
      *                       COBERTURAS
              49 TB-COBERTURAS-TEXT
                 PIC X(500).
      *    *************************************************************
      *                       FECHA_INICIO
           10 TB-FECHA-INICIO      PIC X(10).
      *    *************************************************************
      *                       FECHA_VENCIMIENTO
           10 TB-FECHA-VENCIMIENTO
              PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
