      ******************************************************************
      * DCLGEN TABLE(PEDIDOS_CLIENTE)                                  *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBPEDCLI))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      ******************************************************************
           EXEC SQL DECLARE PEDIDOS_CLIENTE TABLE
           ( ID_PEDIDO              INTEGER NOT NULL,
             ID_CLIENTE             INTEGER,
             FECHA_PEDIDO           DATE,
             IMPORTE_TOTAL          DECIMAL(10,2),
             ESTADO                 CHAR(9),
             TIPO_ENVIO             CHAR(8),
             COMENTARIOS            CHAR(50)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE PEDIDOS_CLIENTE                    *
      ******************************************************************
       01  DCLPEDIDOS-CLIENTE.
           10  TB-ID-PEDIDO           PIC S9(9) COMP.
           10  TB-ID-CLIENTE          PIC S9(9) COMP.
           10  TB-FECHA-PEDIDO        PIC X(10).
           10  TB-IMPORTE-TOTAL       PIC S9(7)V99 COMP-3.
           10  TB-ESTADO              PIC X(9).
           10  TB-TIPO-ENVIO          PIC X(8).
           10  TB-COMENTARIOS         PIC X(50).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************

