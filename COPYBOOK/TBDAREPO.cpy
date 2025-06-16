      ******************************************************************
      * DCLGEN TABLE(DAREPOS)                                          *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBDAREPO))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DAREPOS TABLE
           ( NOMBRE_PGM                     CHAR(8) NOT NULL,
             ESTADO                         CHAR(2) NOT NULL,
             VALOR_CLAVE                    VARCHAR(1000)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DAREPOS                            *
      ******************************************************************
       01  DCLDAREPOS.
      *    *************************************************************
      *                       NOMBRE_PGM
           10 TB-NOMBRE-PGM        PIC X(8).
      *    *************************************************************
      *                       ESTADO
           10 TB-ESTADO            PIC X(2).
      *    *************************************************************
           10 TB-VALOR-CLAVE.
      *                       VALOR_CLAVE LENGTH
              49 TB-VALOR-CLAVE-LEN
                 PIC S9(4) USAGE COMP.
      *                       VALOR_CLAVE
              49 TB-VALOR-CLAVE-TEXT
                 PIC X(1000).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
