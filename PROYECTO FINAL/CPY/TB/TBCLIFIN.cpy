      ******************************************************************
      * DCLGEN TABLE(CLIENTES_PEPITO_SEG)                              *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBCLIFIN))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE CLIENTES_PEPITO_SEG TABLE
           ( DNI_CL                         CHAR(9) NOT NULL,
             NOMBRE_CL                      CHAR(25) NOT NULL,
             APELLIDO_1                     CHAR(25) NOT NULL,
             APELLIDO_2                     CHAR(25) NOT NULL,
             CLASE_VIA                      CHAR(25) NOT NULL,
             NOMBRE_VIA                     CHAR(55) NOT NULL,
             NUMERO_VIA                     INTEGER NOT NULL,
             COD_POSTAL                     CHAR(5) NOT NULL,
             CIUDAD                         CHAR(25) NOT NULL,
             TELEFONO                       CHAR(10) NOT NULL,
             OBSERVACIONES                  VARCHAR(500)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CLIENTES_PEPITO_SEG                *
      ******************************************************************
       01  DCLCLIENTES-PEPITO-SEG.
      *    *************************************************************
      *                       DNI_CL
           10 TB-DNI-CL            PIC X(9).
      *    *************************************************************
      *                       NOMBRE_CL
           10 TB-NOMBRE-CL         PIC X(25).
      *    *************************************************************
      *                       APELLIDO_1
           10 TB-APELLIDO-1        PIC X(25).
      *    *************************************************************
      *                       APELLIDO_2
           10 TB-APELLIDO-2        PIC X(25).
      *    *************************************************************
      *                       CLASE_VIA
           10 TB-CLASE-VIA         PIC X(25).
      *    *************************************************************
      *                       NOMBRE_VIA
           10 TB-NOMBRE-VIA        PIC X(55).
      *    *************************************************************
      *                       NUMERO_VIA
           10 TB-NUMERO-VIA        PIC S9(9) USAGE COMP.
      *    *************************************************************
      *                       COD_POSTAL
           10 TB-COD-POSTAL        PIC X(5).
      *    *************************************************************
      *                       CIUDAD
           10 TB-CIUDAD            PIC X(25).
      *    *************************************************************
      *                       TELEFONO
           10 TB-TELEFONO          PIC X(10).
      *    *************************************************************
           10 TB-OBSERVACIONES.
      *                       OBSERVACIONES LENGTH
              49 TB-OBSERVACIONES-LEN
                 PIC S9(4) USAGE COMP.
      *                       OBSERVACIONES
              49 TB-OBSERVACIONES-TEXT
                 PIC X(500).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
