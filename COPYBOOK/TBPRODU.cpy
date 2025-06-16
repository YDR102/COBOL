      ******************************************************************
      * DCLGEN TABLE(PRODUCTOS)                                        *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBPRODU))                   *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE PRODUCTOS TABLE
           ( ID_PRODUCTO                    DECIMAL(5, 0) NOT NULL,
             NOMBRE                         CHAR(50),
             CATEGORIA                      CHAR(30),
             PRECIO                         DECIMAL(10, 2),
             FECHA_ALTA                     DATE,
             STOCK                          DECIMAL(3, 0),
             DESCRIPCION                    CHAR(100)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE PRODUCTOS                          *
      ******************************************************************
       01  DCLPRODUCTOS.
      *    *************************************************************
      *                       ID_PRODUCTO
           10 TB-ID-PRODUCTO       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
      *                       NOMBRE
           10 TB-NOMBRE            PIC X(50).
      *    *************************************************************
      *                       CATEGORIA
           10 TB-CATEGORIA         PIC X(30).
      *    *************************************************************
      *                       PRECIO
           10 TB-PRECIO            PIC S9(8)V9(2) USAGE COMP-3.
      *    *************************************************************
      *                       FECHA_ALTA
           10 TB-FECHA-ALTA        PIC X(10).
      *    *************************************************************
      *                       STOCK
           10 TB-STOCK             PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
      *                       DESCRIPCION
           10 TB-DESCRIPCION       PIC X(100).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
