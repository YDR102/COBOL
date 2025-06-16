      ******************************************************************
      * DCLGEN TABLE(AGENTES_MAPFRE)                                   *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBAGEFIN))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE AGENTES_MAPFRE TABLE
           ( NUM_AGENTE                     CHAR(9) NOT NULL,
             DNI_AG                         CHAR(9) NOT NULL,
             NOMBRE                         CHAR(25) NOT NULL,
             APE_1                          CHAR(25) NOT NULL,
             APE_2                          CHAR(25) NOT NULL,
             TELEFONO                       CHAR(10) NOT NULL,
             DNI_CLI                        CHAR(9) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE AGENTES_MAPFRE                     *
      ******************************************************************
       01  DCLAGENTES-MAPFRE.
      *    *************************************************************
      *                       NUM_AGENTE
           10 TB-NUM-AGENTE        PIC X(9).
      *    *************************************************************
      *                       DNI_AG
           10 TB-DNI-AG            PIC X(9).
      *    *************************************************************
      *                       NOMBRE
           10 TB-NOMBRE            PIC X(25).
      *    *************************************************************
      *                       APE_1
           10 TB-APE-1             PIC X(25).
      *    *************************************************************
      *                       APE_2
           10 TB-APE-2             PIC X(25).
      *    *************************************************************
      *                       TELEFONO
           10 TB-TELEFONO          PIC X(10).
      *    *************************************************************
      *                       DNI_CLI
           10 TB-DNI-CLI           PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
