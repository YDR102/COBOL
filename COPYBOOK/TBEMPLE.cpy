      ******************************************************************
      * DCLGEN TABLE(EMPLEADOS)                                        *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBEMPLE))                   *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE EMPLEADOS TABLE
           ( MATRICULA                      CHAR(4) NOT NULL,
             APELLIDO                       CHAR(30) NOT NULL,
             NOMBRE                         CHAR(20) NOT NULL,
             CATEGORIA                      CHAR(40) NOT NULL,
             DEPARTAMENTO                   CHAR(40) NOT NULL,
             SECCION                        CHAR(30) NOT NULL,
             SALARIO                        DECIMAL(7, 3),
             FECHA_INGRESO                  DATE,
             FECHA_NACIMIENTO               DATE,
             ACCION                         CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE EMPLEADOS                          *
      ******************************************************************
       01  DCLEMPLEADOS.
      *    *************************************************************
      *                       MATRICULA
           10 TB-MATRICULA         PIC X(4).
      *    *************************************************************
      *                       APELLIDO
           10 TB-APELLIDO          PIC X(30).
      *    *************************************************************
      *                       NOMBRE
           10 TB-NOMBRE            PIC X(20).
      *    *************************************************************
      *                       CATEGORIA
           10 TB-CATEGORIA         PIC X(40).
      *    *************************************************************
      *                       DEPARTAMENTO
           10 TB-DEPARTAMENTO      PIC X(40).
      *    *************************************************************
      *                       SECCION
           10 TB-SECCION           PIC X(30).
      *    *************************************************************
      *                       SALARIO
           10 TB-SALARIO           PIC S9(4)V9(3) USAGE COMP-3.
      *    *************************************************************
      *                       FECHA_INGRESO
           10 TB-FECHA-INGRESO     PIC X(10).
      *    *************************************************************
      *                       FECHA_NACIMIENTO
           10 TB-FECHA-NACIMIENTO  PIC X(10).
      *    *************************************************************
      *                       ACCION
           10 TB-ACCION            PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
