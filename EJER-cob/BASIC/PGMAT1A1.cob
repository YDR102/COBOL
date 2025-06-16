       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  PGMAT1A1.
       AUTHOR.      DAVID.
       DATE-WRITTEN 24/04/2025.
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA1 ASSIGN TO ENTRADA1
           FILE STATUS FS-ENTRADA1.
      *
           SELECT ENTRADA2  ASSIGN TO ENTRADA2
           FILE STATUS FS-ENTRADA2.
      *
           SELECT FSALIDA ASSIGN TO FSALIDA
           FILE STATUS FS-FSALIDA.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD ENTRADA1
           RECORDING MODE IS F.
       01  REG-ENTRADA1                                       PIC X(60).
      *
       FD ENTRADA2
           RECORDING MODE IS F.
       01  REG-ENTRADA2                                       PIC X(33).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                                        PIC X(60).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-ENTRADA1                         PIC X(02).
           05  FS-ENTRADA2                         PIC X(02).
           05  FS-FSALIDA                          PIC X(02).
      *
       01  WK-VARIABLES.
           05 CLAVE1          PIC X(05).
           05 CLAVE2          PIC X(05).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-ENTRADA1              PIC 9(03).
           05  CN-REG-LEIDOS-ENTRADA2              PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA               PIC 9(03).
      *
       01  CT-CONTANTES.
           05  CT-00                               PIC X(02) VALUE '00'.
           05  CT-NOCONT                   PIC X(15) VALUE 'DIFERENCIA'.
           05  CT-CONT                     PIC X(15) VALUE 'COINCIDEN'.
           05  CT-1                                PIC 9(02) VALUE 1.
      *
       01  SW-SWITCHES.
           05  SW-FIN-ENTRADA1                     PIC X(01).
               88  SW-SI-FIN-ENTRADA1                         VALUE 'S'.
               88  SW-NO-FIN-ENTRADA1                         VALUE 'N'.
           05  SW-FIN-ENTRADA2                     PIC X(01).
               88  SW-SI-FIN-ENTRADA2                         VALUE 'S'.
               88  SW-NO-FIN-ENTRADA2                         VALUE 'N'.
      *
      *COPY DEL FICHERO DE ENTRADA ENTRADA1
      *
      *COPY MAT1A1E1
       01 EMP-RH-REG.
           05 COD-EMP-1       PIC X(05).
           05 NOM-EMP-1       PIC X(20).
           05 DEPTO-1         PIC X(20).
           05 CARGO-1         PIC X(15).
      *
      *COPY DEL FICHERO DE ENTRADA ENTRADA2
      *
      *COPY MAT1A1E2
       01 EMP-NOM-REG.
          05 COD-EMP-2       PIC X(05).
          05 NOM-EMP-2       PIC X(20).
          05 SALARIO-2       PIC 9(6)V99.
      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA
      *
      *COPY MAT1A1S
       01 INF-VERIF-REG.
          05 COD-EMP-3       PIC X(05).
          05 NOM-RH-3        PIC X(20).
          05 NOM-NOMINA-3    PIC X(20).
          05 RESULTADO-3     PIC X(15).
      *
      ******************************************************************
      ** PROCEDURE DIVISION                                           **
      ******************************************************************
      *
       PROCEDURE DIVISION.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
             UNTIL SW-SI-FIN-ENTRADA1 OR SW-SI-FIN-ENTRADA2
      *
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT
      *
           .
      *
      ******************************************************************
      * 1000-INICIO                                                    *
      ******************************************************************
      *
       1000-INICIO.
      *
           INITIALIZE FS-FILE-STATUS
                      CN-CONTADORES
                      EMP-RH-REG
                      EMP-NOM-REG
                      INF-VERIF-REG
      *
           SET SW-NO-FIN-ENTRADA1               TO TRUE
           SET SW-NO-FIN-ENTRADA2               TO TRUE
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT
           PERFORM 9000-LEER-ENTRADA1
              THRU 9000-LEER-ENTRADA1-EXIT
           PERFORM 9100-LEER-ENTRADA2
              THRU 9100-LEER-ENTRADA2-EXIT
      *
           .
      *
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 1100-ABRIR-FICHEROS                                            *
      ******************************************************************
      *
       1100-ABRIR-FICHEROS.
      *
           OPEN INPUT ENTRADA1
           OPEN OUTPUT FSALIDA
           OPEN INPUT ENTRADA2
      *
           IF FS-ENTRADA1 NOT = CT-00
              DISPLAY 'ERROR AL ABRIR ENTRADA1'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ENTRADA1
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FSALIDA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-ENTRADA2 NOT = CT-00
              DISPLAY 'ERROR AL ABRIR ENTRADA2'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ENTRADA2
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           .
      *
       1100-ABRIR-FICHEROS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2000-PROCESO                                                   *
      *                                                                *
      *    Leer ambos ficheros simultáneamente                         *
      *    Verificar si los códigos de empleado coinciden              *
      *    Generar un registro en el informe con el resultado          *
      *    Terminar cuando uno de los ficheros llegue a fin            *
      ******************************************************************
      *
       2000-PROCESO.
      *
           IF COD-EMP-1 = COD-EMP-2
              IF NOM-EMP-1 = NOM-EMP-2
                 MOVE CT-CONT               TO RESULTADO-3
              ELSE
                 MOVE CT-NOCONT             TO RESULTADO-3
              END-IF
              PERFORM 2100-INFORMAR-SALIDA
                 THRU 2100-INFORMAR-SALIDA-EXIT
      *
              PERFORM 2200-ESCRIBIR-FSALIDA
                 THRU 2200-ESCRIBIR-FSALIDA-EXIT
      *
              PERFORM 9000-LEER-ENTRADA1
                 THRU 9000-LEER-ENTRADA1-EXIT
      *
              PERFORM 9100-LEER-ENTRADA2
                 THRU 9100-LEER-ENTRADA2-EXIT
           ELSE
              IF COD-EMP-1 < COD-EMP-2
                 DISPLAY 'CLAVE1 NO EXISTE EN FICHERO2'
                 PERFORM 9000-LEER-ENTRADA1
                    THRU 9000-LEER-ENTRADA1-EXIT
              ELSE
                 DISPLAY 'CLAVE2 NO EXISTE EN FICHERO1'
                 PERFORM 9100-LEER-ENTRADA2
                    THRU 9100-LEER-ENTRADA2-EXIT
              END-IF
           END-IF
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA                                           *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA.
      *
           MOVE COD-EMP-1           TO COD-EMP-3
           MOVE NOM-EMP-1           TO NOM-RH-3
           MOVE NOM-EMP-2           TO NOM-NOMINA-3
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
           WRITE REG-FSALIDA        FROM INF-VERIF-REG
      *
           IF FS-FSALIDA NOT = '00'
              DISPLAY 'ERROR AL ESCRIBIR ENTRADA1'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE INF-VERIF-REG
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3000-FIN                                                       *
      ******************************************************************
      *
       3000-FIN.
      *
           PERFORM 3100-CERRAR-FICHEROS
              THRU 3100-CERRAR-FICHEROS-EXIT
           PERFORM 3200-MOSTRAR-ESTADISTICAS
              THRU 3200-MOSTRAR-ESTADISTICAS-EXIT
           STOP RUN
      *
           .
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
           CLOSE ENTRADA1
           CLOSE FSALIDA
           CLOSE ENTRADA2
      *
           IF FS-ENTRADA1 NOT = CT-00
              DISPLAY 'ERROR AL CERRAR ENTRADA1'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ENTRADA1
           END-IF
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FSALIDA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
           END-IF
      *
           IF FS-ENTRADA2 NOT = CT-00
              DISPLAY 'ERROR AL CERRAR ENTRADA2'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ENTRADA2
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
           DISPLAY '*REG ENTRADA1: ' CN-REG-LEIDOS-ENTRADA1 '          '
                   '                      *'
           DISPLAY '*REG ENTRADA2: ' CN-REG-LEIDOS-ENTRADA2 '          '
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
      * 9000-LEER-ENTRADA1                                             *
      ******************************************************************
      *
       9000-LEER-ENTRADA1.
      *
           READ ENTRADA1 INTO EMP-RH-REG
      *
           EVALUATE FS-ENTRADA1
               WHEN '00'
                    ADD CT-1               TO CN-REG-LEIDOS-ENTRADA1
               WHEN '10'
                    SET SW-SI-FIN-ENTRADA1 TO TRUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL FICHERO ENTRADA1'
                    DISPLAY 'PARRAFO: 9000-LEER-ENTRADA1'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA1
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       9000-LEER-ENTRADA1-EXIT.
           EXIT.
      *
      ******************************************************************
      * 9100-LEER-ENTRADA2                                             *
      ******************************************************************
      *
       9100-LEER-ENTRADA2.
      *
           READ ENTRADA2 INTO EMP-NOM-REG
      *
           EVALUATE FS-ENTRADA2
               WHEN '00'
                    ADD CT-1               TO CN-REG-LEIDOS-ENTRADA2
               WHEN '10'
                    SET SW-SI-FIN-ENTRADA2 TO TRUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL FICHERO ENTRADA2'
                    DISPLAY 'PARRAFO: 9100-LEER-ENTRADA2'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA2
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       9100-LEER-ENTRADA2-EXIT.
           EXIT.