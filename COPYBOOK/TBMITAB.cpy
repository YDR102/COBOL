      ******************************************************************
      * DCLGEN TABLE(MITABLA)                                          *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBMITAB))                   *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MITABLA TABLE
           ( DNI                            CHAR(9) NOT NULL,
             NOMBRE                         CHAR(10),
             APELLIDOS                      CHAR(30),
             FECNAC                         DATE,
             SEXO                           CHAR(1)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MITABLA                            *
      ******************************************************************
       01  DCLMITABLA.
      *    *************************************************************
      *                       DNI
           10 TB-DNI               PIC X(9).
      *    *************************************************************
      *                       NOMBRE
           10 TB-NOMBRE            PIC X(10).
      *    *************************************************************
      *                       APELLIDOS
           10 TB-APELLIDOS         PIC X(30).
      *    *************************************************************
      *                       FECNAC
           10 TB-FECNAC            PIC X(10).
      *    *************************************************************
      *                       SEXO
           10 TB-SEXO              PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
