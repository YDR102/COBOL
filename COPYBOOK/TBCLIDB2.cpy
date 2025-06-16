      ******************************************************************
      * DCLGEN TABLE(CLIENTES_DB2)                                     *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBCLIDB2))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE CLIENTES_DB2 TABLE
           ( ID_CLIENTE                     CHAR(10) NOT NULL,
             NOMBRE                         CHAR(50) NOT NULL,
             TIPO_CLIENTE                   CHAR(1) NOT NULL,
             DNI_CIF                        CHAR(15),
             TELEFONO                       CHAR(12),
             EMAIL                          CHAR(50),
             DIRECCION                      CHAR(100)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CLIENTES_DB2                       *
      ******************************************************************
       01  DCLCLIENTES-DB2.
      *    *************************************************************
      *                       ID_CLIENTE
           10 TB-ID-CLIENTE        PIC X(10).
      *    *************************************************************
      *                       NOMBRE
           10 TB-NOMBRE            PIC X(50).
      *    *************************************************************
      *                       TIPO_CLIENTE
           10 TB-TIPO-CLIENTE      PIC X(1).
      *    *************************************************************
      *                       DNI_CIF
           10 TB-DNI-CIF           PIC X(15).
      *    *************************************************************
      *                       TELEFONO
           10 TB-TELEFONO          PIC X(12).
      *    *************************************************************
      *                       EMAIL
           10 TB-EMAIL             PIC X(50).
      *    *************************************************************
      *                       DIRECCION
           10 TB-DIRECCION         PIC X(100).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************