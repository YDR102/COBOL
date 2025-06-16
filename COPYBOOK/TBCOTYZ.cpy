      ******************************************************************
      * DCLGEN TABLE(COTIZACIONES)                                     *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBCOTYZ))                   *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE COTIZACIONES TABLE
           ( TIPOCOT                DECIMAL(1,0) NOT NULL,
            DESCRIPCION             CHAR(40),
            FECHAVIG                DATE,
            PORCENTAGE              DECIMAL(2,2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE PRODUCTOS                          *
      ******************************************************************
       01  DCLCOTIZACIONES.
      *    *************************************************************
      *                       TIPOCOT
           10 TB-TIPOCOT           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
      *                       DESCRIPCION
           10 TB-DESCRIPCION       PIC X(40).
      *    *************************************************************
      *                       FECHAVIG
           10 TB-FECHAVIG          PIC X(10).
      *    *************************************************************
      *                       PORCENTAGE
           10 TB-PORCENTAGE        PIC S9(2)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
