      ******************************************************************
      *                   PROGRAMA PRINCIPAL                           *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   EJPGM02.
       AUTHOR.       DAVID.
       DATE-WRITTEN. 06/05/2025.
       DATE-COMPILED.
      *
      ******************************************************************
      *     ENVIRONMENT DIVISION                                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
         SOURCE-COMPUTER.  IBM-3090.
         OBJECT-COMPUTER.  IBM-3090.
      *
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT FENTRADA
               ASSIGN TO FENTRADA
               FILE STATUS IS FS-FENTRADA.
      *
           SELECT FSALIDA
               ASSIGN TO FSALIDA
               FILE STATUS IS FS-FSALIDA.
      *
      ******************************************************************
      *     DATA DIVISION                                              *
      ******************************************************************
       DATA DIVISION.
      *
       FILE SECTION.
      *
      ******************************************************************
      *     F I L E  S E C T I O N                                     *
      ******************************************************************
      *
       FD  FENTRADA
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD ARE STANDARD
           RECORDING MODE IS F
           DATA RECORD IS REG-ENT.

       01  REG-FENTRADA               PIC X(006).
      *
       FD  FSALIDA
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD ARE STANDARD
           RECORDING MODE IS F
           DATA RECORD IS REG-SAL.

       01  REG-FSALIDA                PIC X(004).
      *
      ******************************************************************
      *     W O R K I N G   S T O R A G E                              *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
       01 FS-FILE-STATUS.
          05 FS-FENTRADA               PIC X(02).
          05 FS-FSALIDA                PIC X(02).
      *
       01 SWITCHES.
          05 SW-FIN-FICHERO            PIC X(01) VALUE 'N'.
             88 SW-SI-FIN-FICHERO                VALUE 'S'.
             88 SW-NO-FIN-FICHERO                VALUE 'N'.
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA              PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA               PIC 9(03).
      *
       01 CA-CONSTANTES-ALF.
          05 CT-PGM                    PIC X(07) VALUE 'EJPGM02'.
          05 CT-RUT                    PIC X(07) VALUE 'EJRUT02'.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-1                      PIC 9(01) VALUE 1.
          05 CT-10                     PIC X(02) VALUE '10'.
      *
      * -- COPY DE COMUNICACION CON LA RUTINA
       01 CPY-RUT.
          05 RUT-ENTRADA.
             10 TEM-MAX-R              PIC S9(02) VALUE ZEROS.
             10 TEM-MIN-R              PIC S9(02) VALUE ZEROS.
          05 RUT-SALIDA.
             10 RETORNO                PIC X(02) VALUE SPACES.
             10 RESPUESTA              PIC S9(03) VALUE ZEROS.
      *
      * -- COPY DE ENTRADA
       01 CPY-ENTRADA.
          05 DIA-E                     PIC X(02).
          05 TEM-MAX                   PIC S9(02).
          05 TEM-MIN                   PIC S9(02).
      *
      * -- COPY DE SALIDA
       01 CPY-SALIDA.
          05 DIA-S                     PIC X(02).
          05 TEM-MEDIA                 PIC S9(03).
      *
      ******************************************************************
      *     PROCEDURE DIVISION                                         *
      ******************************************************************
       PROCEDURE DIVISION.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
             UNTIL SW-SI-FIN-FICHERO
      *
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT.
      *
      ******************************************************************
      *     1000-INICIO                                                *
      ******************************************************************
       1000-INICIO.
      *
           INITIALIZE CPY-RUT
                      CPY-ENTRADA
                      CPY-SALIDA

           SET SW-NO-FIN-FICHERO      TO TRUE
      *
           PERFORM 1050-ABRIR-FICHEROS
              THRU 1050-ABRIR-FICHEROS-EXIT
      *
           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
      *
           .
      *
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      *   1050-ABRIR-ENTRADA
      ******************************************************************
      *
       1050-ABRIR-FICHEROS.
           OPEN OUTPUT FSALIDA
           OPEN INPUT FENTRADA
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FENTRADA'
              DISPLAY 'PARRAFO: 1050-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FSALIDA'
              DISPLAY 'PARRAFO: 1050-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           .
      *
       1050-ABRIR-FICHEROS-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2000-PROCESO                                               *
      ******************************************************************
      *
       2000-PROCESO.
      * -- PARA LLAMAR A RUTINA LOS PASOS SON:
      * 1- INICIALIZAR COPY DE COMUNICACION
           INITIALIZE CPY-RUT
      * 2- INFORMAR LA ENTRADA DE LA COPY DE COMUNICACION
           MOVE TEM-MIN               TO TEM-MIN-R
           MOVE TEM-MAX               TO TEM-MAX-R
      * 3- INVOCAR A LA RUTINA
           CALL CT-RUT USING CPY-RUT
      * 4- EVALUAR EL RETORNO
           EVALUATE RETORNO
               WHEN CT-00
                    DISPLAY RESPUESTA
      *
                    PERFORM 2100-INFORMAR-SALIDA
                       THRU 2100-INFORMAR-SALIDA-EXIT
      *
                    PERFORM 2200-ESCRIBIR-FSALIDA
                       THRU 2200-ESCRIBIR-FSALIDA-EXIT
      *
                    PERFORM 9000-LEER-FENTRADA
                       THRU 9000-LEER-FENTRADA-EXIT
      *
               WHEN CT-10
                    DISPLAY 'ERROR: FALTAN DATOS'
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR DESCONOCIDO'
           END-EVALUATE
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA                                           *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA.
      *
           MOVE DIA-E           TO DIA-S
           MOVE RESPUESTA       TO TEM-MEDIA
      *
           .
      *
       2100-INFORMAR-SALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA                                          *
      ******************************************************************
      *
       2200-ESCRIBIR-FSALIDA.
      *
           WRITE REG-FSALIDA        FROM CPY-SALIDA
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CPY-SALIDA
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3000-FIN                                                   *
      ******************************************************************
       3000-FIN.
      *
           PERFORM 3100-CERRAR-FICHEROS
              THRU 3100-CERRAR-FICHEROS-EXIT
      *
           PERFORM 3200-MOSTRAR-ESTADISTICAS
              THRU 3200-MOSTRAR-ESTADISTICAS-EXIT
      *
           STOP RUN.
      *
       3000-FIN-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3100-CERRAR-FICHEROS                                           *
      ******************************************************************
      *
       3100-CERRAR-FICHEROS.
      *
           CLOSE FENTRADA
                 FSALIDA
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR ENTRADA1'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
           END-IF
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FSALIDA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
           END-IF
      *
           .
      *
       3100-CERRAR-FICHEROS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3200-MOSTRAR-ESTADISTICAS                                      *
      ******************************************************************
      *
       3200-MOSTRAR-ESTADISTICAS.
      *
           DISPLAY '***************************************************'
           DISPLAY '*       ESTADISTICAS DEL PGM PGMFICH              *'
           DISPLAY '***************************************************'
           DISPLAY '*REG FENTRADA: ' CN-REG-LEIDOS-FENTRADA '          '
                   '                      *'
           DISPLAY '*REG  FSALIDA: ' CN-REG-ESCRIT-FSALIDA  '          '
                   '                      *'
           DISPLAY '***************************************************'
      *
           .
      *
       3200-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 9000-LEER-ENTRADA                                              *
      ******************************************************************
      *
       9000-LEER-FENTRADA.
      *
           READ FENTRADA INTO CPY-ENTRADA
      *
           EVALUATE FS-FENTRADA
               WHEN CT-00
                    ADD CT-1               TO CN-REG-LEIDOS-FENTRADA
               WHEN CT-10
                    SET SW-SI-FIN-FICHERO TO TRUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL FICHERO ENTRADA'
                    DISPLAY 'PARRAFO: 9000-LEER-ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-FENTRADA
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       9000-LEER-FENTRADA-EXIT.
           EXIT.
