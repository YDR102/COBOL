      ******************************************************************
      * DCLGEN TABLE(MUNDIAL)                                          *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBMUNDIA))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MUNDIAL TABLE
           ( SELECCION                      CHAR(30) NOT NULL,
             ENTRENADOR                     CHAR(30) NOT NULL,
             NUM_JUGADORES_CONV             DECIMAL(3, 0) NOT NULL,
             FECHA_PRIMER_PARTIDO           DATE NOT NULL,
             GRUPO                          CHAR(1) NOT NULL,
             FASE_ELIMINACION               CHAR(20),
             TIMESTAMP_MODIF                TIMESTAMP NOT NULL,
             USUARIO_MODIF                  CHAR(30) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MUNDIAL                            *
      ******************************************************************
       01  DCLMUNDIAL.
      *    *************************************************************
      *                       SELECCION
           10 TB-SELECCION         PIC X(30).
      *    *************************************************************
      *                       ENTRENADOR
           10 TB-ENTRENADOR        PIC X(30).
      *    *************************************************************
      *                       NUM_JUGADORES_CONV
           10 TB-NUM-JUGADORES-CONV
              PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
      *                       FECHA_PRIMER_PARTIDO
           10 TB-FECHA-PRIMER-PARTIDO
              PIC X(10).
      *    *************************************************************
      *                       GRUPO
           10 TB-GRUPO             PIC X(1).
      *    *************************************************************
      *                       FASE_ELIMINACION
           10 TB-FASE-ELIMINACION  PIC X(20).
      *    *************************************************************
      *                       TIMESTAMP_MODIF
           10 TB-TIMESTAMP-MODIF   PIC X(26).
      *    *************************************************************
      *                       USUARIO_MODIF
           10 TB-USUARIO-MODIF     PIC X(30).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
