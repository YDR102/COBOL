      ******************************************************************
      *     I D E  N T I F I C A T I O N        D I V I S I O N        *
      *                                                                *
      *  PRIMER PROGRAMA COBOL DONDE APRENDERMOS                       *
      *  LA ESTRUCTURA DE UN PROGRAMA                                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.    PGMCOB01.
       AUTHOR         DAVID.
       INSTALLATION.  IBMUSER.
       DATE-WRITTEN.  11-04-2025.
       DATE-COMPILED. 11-04-2025.
      *
      ******************************************************************
      *     E N V I R O N M E N T         D I V I S I O N              *
      *                                                                *
      *  DEFINIR FICHEROS QUE VAMOS A UTILIZAR                         *
      *  RELACIONAR NOMBRES LOGICOS DE LOS ARCHIVOS                    *
      *  DEL PROGRAMA CON LOS NOMBRES DEL JCL                          *
      *  2 SECIONES:                                                   *
      *    - CONFIGURATION SECTION                                     *
      *    - INPUT-OUTPUT SECTION                                      *
      ******************************************************************
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
         SOURCE-COMPUTER.   IBM-3090.
         OBJECT-COMPUTER.   IBM-3090.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      * SE DECLARAN LOS FICHEROS DE ENTRADA Y SALIDA DEL PGM
      * OBLIGATORIO (CUANDO HAY)
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHERO-ENTRADA ASSIGN TO ENTRADA
           FILE STATUS IS FS-FICHERO-ENTRADA.
           SELECT FICHERO-SALIDA ASSIGN TO SALIDA
           FILE STATUS IS FS-FICHERO-SALIDA.
      *
      ******************************************************************
      *     D A T A        D I V I S I O N                             *
      *                                                                *
      *  IDENTIFICA TODOS LOS NOMBRES DE DATOS                         *
      *  SECCIONES:                                                    *
      *   - FILE SECTION                                               *
      *     DESCRIBE FICHEROS DE ENTRADA Y SALIDA QUE USAMOS EN EL PGM *
      *                                                                *
      *   - WORKING STOREGE SECTION                                    *
      *     DEFINIMOS LOS CAMPOS DE TRABAJO DEL PGM                    *
      *                                                                *
      *   - LINKAGE SECTION                                            *
      *     SE USA CUANDO EL PROGRAMA SEA UNA RUTINA YA QUE ES LA ZONA *
      *     QUE SIRVE PARA COMUNICARSE CON EL PROGRAMA                 *
      *   * RUTINA: SE EJECUTAN DESDE OTROS PGM COBOL (FUNCION DE JAVA)*
      *   * PRINCIPAL: SE EJECUTAN DESDE UN JCL (MAIN DE JAVA)         *
      ******************************************************************
       DATA DIVISION.
      *
       FILE SECTION.
       FD FICHERO-ENTRADA
            BLOCK CONTAINS 0 RECORDS
            LABEL RECORD ARE STANDARD
      *     SI EL SICHERO ES FIJO (F) O VARIABLE (V)
            RECORDING MODE IS F
            DATA RECORD IS REG-ENT.
      *                        EL TAMANNO (01) SON LAS COLUMNAS (ANCHO)
       01 REG-ENT        PIC X(01).
      *
       FD FICHERO-SALIDA
            BLOCK CONTAINS 0 RECORDS
            LABEL RECORD ARE STANDARD
      *     SI EL SICHERO ES FIJO (F) O VARIABLE (V)
            RECORDING MODE IS F
            DATA RECORD IS REG-SAL.
      *                        EL TAMANNO (01) SON LAS COLUMNAS (ANCHO)
       01 REG-SAL        PIC X(01).
      *
      ******************************************************************
      *     W O R K I N G        S T O R A G E                         *
      *                                                                *
      *  LUGAR DONDE DECLARAREMOS CONTANTES, VARIAVLES, COPYS,         *
      *  SWITCHES, FILE STATUS, CONTADORES, ACUMULADORES               *
      ******************************************************************
      *
       WORKING-STORAGE SECTION.
      *
       01 FS-FILE-STATUS.
          05 FS-FICHERO-ENTRADA     PIC X(02).
          05 FS-FICHERO-SALIDA      PIC X(02).
      *
       01 CA-CONTANTES-ALF.
          05 CA-00       PIC X(02)  VALUE '00'.
          05 CA-HOLA     PIC X(04)  VALUE 'HOLA'.
          05 CA-ESPACIO  PIC X(04)  VALUE ' '.
          05 CA-MUNDO    PIC X(05)  VALUE 'MUNDO'.
      *
       01 CA-CONTANTES-NUM.
          05 CA-2        PIC 9(01)  VALUE 2.
          05 CA-2N       PIC S9(02) VALUE -2.
          05 CA-100      PIC 9(03)  VALUE 100.
      *
       01 OTRA-VARIAVLE  PIC X(02).
      *
       01 VN-VARIABLES-NUM.
          05 VAR-NUM-1   PIC 9(17).
          05 VAR-NUM-2   PIC 9(01).
          05 VAR-NUM-2   PIC 9(10)V9(02).
      *
       01 VA-VARIABLES-ALF.
          05 VAR-ALF-1   PIC X(99).
          05 VAR-ALF-2   PIC X(01).
      *
       01 CN-CONTADORES.
          05 CN-REG-LEIDOS-FICHERO-ENTRADA  PIC 9(03).
          05 CN-REG-ESCRITOS-FICHERO-SALIDA PIC 9(03).
      *
       01 AC-ACUMULADORES.
          05 AC-ACUM-IMPORTE                PIC 9(06).
      *
       01 SW-SWITCHES.
          05 SW-SEMAFORO        PIC X(01).
             88 SW-VERDE        VALUE 'V'.
             88 SW-AMBAR        VALUE 'A'.
             88 SW-ROJO         VALUE 'R'.
      *
      * COPY DE FICHERO DE ENTRADA
      *
       01 CPY-FICHERO-ENTRADA.
          05 NOMBRE-E           PIC X(50).
          05 APELLIDOS-E        PIC X(50).
          05 DIRECCION-E        PIC X(43).
      *
       01 CPY-FICHERO-SALIDA.
          05 NOMBRE-S           PIC X(50).
          05 APELLIDOS-S        PIC X(50).
          05 DIRECCION-S        PIC X(43).
      *
      ******************************************************************
      *     P R O C E D U R E     D I V I S I O N                      *
      *                                                                *
      *  DONDE SE HAYA LA LOGUICA DEL PROGRAMA                         *
      ******************************************************************
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
      ******************************************************************
      *                         1000 - INICIO                          *
      ******************************************************************
      *
       1000-INICIO.
           DISPLAY '1000-INICIO'
           .
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      *                        2000 - PROCESO                          *
      ******************************************************************
      *
       2000-PROCESO.
           DISPLAY '2000-PROCESO'
           .
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      *                         3000 - FIN                             *
      ******************************************************************
      *
       3000-FIN.
           DISPLAY '3000-FIN'
           STOP RUN
           .
       3000-FIN-EXIT.
           EXIT.
