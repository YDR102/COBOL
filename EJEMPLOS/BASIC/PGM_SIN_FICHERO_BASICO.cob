      *****************************
      *  IDENTIFICATION DIVISION  *
      *****************************                                     *******
       IDENTIFICATION DIVISION.
      ******************************************************************
      *          PRIMER PROGRAMA COBOL DONDE APRENDERMOS               *
      *             LA ESTRUCTURA DE UN PROGRAMA                       *
      ******************************************************************
       PROGRAM-ID. ESQUELE1.
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
           STOP RUN.
