      *****************************
      *  IDENTIFICATION DIVISION  *
      *****************************                                     *******
       IDENTIFICATION DIVISION.
      ******************************************************************
      *          PRIMER PROGRAMA COBOL DONDE APRENDERMOS               *
      *             LA ESTRUCTURA DE UN PROGRAMA                       *
      ******************************************************************
       PROGRAM-ID. FINALPGM.
       AUTHOR DAVID.
      *
      **************************
      *  ENVIRONMENT DIVISION  *
      ******************************************************************
      *   DEFINIR FICHEROS QUE VAMOS A UTILIZAR                        *
      *   RELACIONAR NOMBRES LOGICOS DE LOS ARCHIVOS                   *
      *   DEL PROGRAMA CON LOS NOMBRES DEL JCL                         *
      *   2 SECIONES:                                                  *
      *     -CONFIGURATION DE SECTION                                  *
      *     -INPUT-OUTPUT SECTION                                      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
      * SE DECLARAN LOS FICHEROS DE ENTRADA Y SALIDA
      * OBLIGATORIO (CUANDO HAY)
       FILE-CONTROL.
           SELECT FENTRADA
              ASSIGN TO ENTRADA
              FILE STATUS IS FS-FENTRADA.
           SELECT FSALIDA
              ASSIGN TO SALIDA
              FILE STATUS IS FS-FSALIDA.
      *
      *******************
      *  DATA DIVISION  *
      ******************************************************************
      * IDENTIFICA TODOS LOS NOMBRES DE DATOS   *
      * SECCIONES:  *
      *   - FILE SECTION  *
      *     DESCRIBE FICHEROS DE ENTRADA Y SALIDA QUE USAMOS  *
      *   - WORKING STOREGE SECTION*
      *     DEFINIMOS LOS CAMPOS DE TRABAJO      *
      *   - LINKAGE SECTION  *
      *     SE USA CUANDO EL PROGRAMA SEA UNA RUTINA YA QUE ES LA ZONA *
      *     QUE SIRVE PARA COMUNICARSE CON EL PROGRAMA  *
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD FENTRADA
            BLOCK CONTAINS 0 RECORDS
            LABEL RECORD ARE STANDARD
            RECORDING MODE IS F
            DATA RECORD IS REG-ENT.
       01 REG-ENT        PIC X(01).
      *
       FD FSALIDA
            BLOCK CONTAINS 0 RECORDS
            LABEL RECORD ARE STANDARD
            RECORDING MODE IS F
            DATA RECORD IS REG-SAL.
       01 REG-SAL        PIC X(01).
      *
      *********************
      *  WORKING STORAGE  *
      ******************************************************************
      *  LUGAR DONDE DECLARAREMOS CONTANTES, VARIAVLES, COPYS, ...     *
      ******************************************************************
      *
       WORKING-STORAGE SECTION.
       01 CA-CONTANTES-ALF.
          05 CA-PGM      PIC X(06) VALUE 'PGMCOB'.
          05 CA-00       PIC X(02) VALUE '00'.
          05 CA-HOLA     PIC X(04) VALUE 'HOLA'.
          05 CA-ESPACIO  PIC X(01) VALUE ' '.
          05 CA-MUNDO    PIC X(05) VALUE 'MUNDO'.
      *
       01 CA-CONTANTES-NUM.
          05 CA-2        PIC 9(01) VALUE 2.
          05 CA-2        PIC S9(02) VALUE -2.
          05 CA-100      PIC 9(03) VALUE 100.
      *
       01 FS-FILE-STATUS.
          05 FS-FENTRADA PIC X(02).
          05 FS-FSALIDA  PIC X(02).
      *
       01 OTRA-VARIAVLE  PIC X(02).
      *
       01 ALQUILERES.
          05 DNI-SOCIO   PIC 9(10).
          05 NOMBRE      PIC X(20).
          05 PRECIO-ALQ  PIC 9(15)V9(02).
          05 FECHA-ALQ   PIC X(10).
      *
      ************************
      *  PROCEDURE DIVISION  *
      ************************
      *
       PROCEDURE DIVISION.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
      *
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT.
      *
      *************************
      * *** 1000 - INICIO ***
      *************************
       1000-INICIO.
           DISPLAY '1000-INICIO'
           .
       1000-INICIO-EXIT.
           EXIT.
      **************************
      * *** 2000 - PROCESO ***
      **************************
       2000-PROCESO.
           DISPLAY '2000-PROCESO'
           .
       2000-PROCESO-EXIT.
           EXIT.
      ************************
      * ***  3000 - FIN  ***
      ************************
       3000-FIN.
           DISPLAY '3000-FIN'
           STOP RUN.
       3000-FIN-EXIT.
           EXIT.
      ******************************************************************
