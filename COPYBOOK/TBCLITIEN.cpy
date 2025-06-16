      ******************************************************************
      * DCLGEN TABLE(CLIENTES_TIENDA)                                  *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBCLIENT))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      ******************************************************************
           EXEC SQL DECLARE CLIENTES_TIENDA TABLE
           ( ID_CLIENTE       INTEGER NOT NULL,
             NOMBRE           CHAR(30),
             APELLIDO1_CLI    CHAR(20),
             APELLIDO2_CLI    CHAR(20),
             TELEFONO         CHAR(15),
             FECHA_ALTA       DATE
           )
           END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CLIENTES_TIENDA                    *
      ******************************************************************
       01 DCLCLIENTES-TIENDA.
           10 TB-ID-CLIENTE      PIC S9(9) COMP-4.
           10 TB-NOMBRE          PIC X(30).
           10 TB-APELLIDO1-CLI   PIC X(20).
           10 TB-APELLIDO2-CLI   PIC X(20).
           10 TB-TELEFONO        PIC X(15).
           10 TB-FECHA-ALTA      PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
