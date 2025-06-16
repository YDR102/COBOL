       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  PGMAT1AN.
       AUTHOR.      DAVID.
       DATE-WRITTEN 25/04/2025.
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAESTRO ASSIGN TO MAESTRO
           FILE STATUS FS-MAESTRO.
      *
           SELECT ESCLAVO  ASSIGN TO ESCLAVO
           FILE STATUS FS-ESCLAVO.
      *
           SELECT FSALIDA ASSIGN TO FSALIDA
           FILE STATUS FS-FSALIDA.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD MAESTRO
           RECORDING MODE IS F.
       01  REG-MAESTRO                                       PIC X(54).
      *
       FD ESCLAVO
           RECORDING MODE IS F.
       01  REG-ESCLAVO                                       PIC X(25).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                                       PIC X(53).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-MAESTRO                         PIC X(02).
           05  FS-ESCLAVO                         PIC X(02).
           05  FS-FSALIDA                         PIC X(02).
      *
       01  WK-VARIABLES.
           05 CLAVE1                              PIC X(05).
           05 CLAVE2                              PIC X(05).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-MAESTRO              PIC 9(03).
           05  CN-REG-LEIDOS-ESCLAVO              PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA              PIC 9(03).
      *
       01  CT-CONTANTES.
           05  CT-00                              PIC X(02) VALUE '00'.
           05  CT-10                              PIC X(02) VALUE '10'.
           05  CT-1                               PIC 9(02) VALUE 1.
      *
       01  SW-SWITCHES.
           05  SW-FIN-MAESTRO                     PIC X(01).
               88  SW-SI-FIN-MAESTRO                         VALUE 'S'.
               88  SW-NO-FIN-MAESTRO                         VALUE 'N'.
           05  SW-FIN-ESCLAVO                     PIC X(01).
               88  SW-SI-FIN-ESCLAVO                         VALUE 'S'.
               88  SW-NO-FIN-ESCLAVO                         VALUE 'N'.
      *
      *COPY DEL FICHERO DE ENTRADA MAESTRO
      *
      *COPY MAT1ANE1.
       01 CLIENTES.
           05 ID-CLIENT-M                         PIC X(04).
           05 NOMBRE-M                            PIC X(20).
           05 APELLIDO-M                          PIC X(20).
           05 F-NACIMIENTO-M                      PIC X(10).
      *
      *COPY DEL FICHERO DE ENTRADA ESCLAVO
      *
      *COPY MAT1ANE2.
       01 TRANSACCIONES.
           05 ID-TRANSACCION-E                    PIC X(04).
           05 ID-CLIENT-E                         PIC X(04).
           05 F-TRANSACCION-E                     PIC X(10).
           05 IMPORTE-E                           PIC 9(05)V99.
      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA
      *
      *COPY MAT1ANS.
       01 CPY-FSALIDA.
           05 ID-CLIENT-F                         PIC X(04).
           05 NOMBRE-F                            PIC X(20).
           05 APELLIDO-F                          PIC X(20).
           05 TOTAL-F                             PIC 9(07)V99.
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
             UNTIL SW-SI-FIN-MAESTRO
      *
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT
      *
           .
      *
      ******************************************************************
      ** 1000-INICIO                                                  **
      ******************************************************************
      *
       1000-INICIO.
      *
           INITIALIZE FS-FILE-STATUS
                      CN-CONTADORES
                      CLIENTES
                      TRANSACCIONES
                      CPY-FSALIDA
                      WK-VARIABLES
      *
           SET SW-NO-FIN-MAESTRO               TO TRUE
           SET SW-NO-FIN-ESCLAVO               TO TRUE
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT
      *
           PERFORM 9000-LEER-MAESTRO
              THRU 9000-LEER-MAESTRO-EXIT
      *
           PERFORM 9100-LEER-ESCLAVO
              THRU 9100-LEER-ESCLAVO-EXIT
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
           OPEN INPUT MAESTRO
           OPEN INPUT ESCLAVO
           OPEN OUTPUT FSALIDA
      *
           IF FS-MAESTRO NOT = CT-00
              DISPLAY 'ERROR AL ABRIR MAESTRO'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-MAESTRO
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
           IF FS-ESCLAVO NOT = CT-00
              DISPLAY 'ERROR AL ABRIR ESCLAVO'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ESCLAVO
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
      *    Verificar si los códigos de cliente coinciden               *
      *    Generar un registro en el informe con el resultado          *
      *    Calcular el total de transacciones                          *
      ******************************************************************
      *
       2000-PROCESO.
      *
           DISPLAY 'CLE1= ' CLAVE1
           DISPLAY 'CLE2= 'CLAVE2
      *
           IF CLAVE1 = CLAVE2
              DISPLAY 'CLAVE1 = CLAVE2'
              COMPUTE TOTAL-F = TOTAL-F + IMPORTE-E
              PERFORM 9100-LEER-ESCLAVO
                 THRU 9100-LEER-ESCLAVO-EXIT
           ELSE
              IF CLAVE1 < CLAVE2
                DISPLAY 'CLAVE1 < CLAVE2'
                PERFORM 2100-MOVER-SALIDA
                   THRU 2100-MOVER-SALIDA-EXIT
      *
                PERFORM 2200-ESCRIBIR-FSALIDA
                   THRU 2200-ESCRIBIR-FSALIDA-EXIT
      *
                PERFORM 9000-LEER-MAESTRO
                   THRU 9000-LEER-MAESTRO-EXIT
              ELSE
                DISPLAY 'CLAVE1 > CLAVE2'
                PERFORM 9100-LEER-ESCLAVO
                   THRU 9100-LEER-ESCLAVO-EXIT
              END-IF

           END-IF
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-MOVER-SALIDA                                           *
      ******************************************************************
      *
       2100-MOVER-SALIDA.
      *
           MOVE ID-CLIENT-M         TO ID-CLIENT-F
           MOVE NOMBRE-M            TO NOMBRE-F
           MOVE APELLIDO-M          TO APELLIDO-F
      *
           .
      *
       2100-MOVER-SALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA                                          *
      ******************************************************************
      *
       2200-ESCRIBIR-FSALIDA.
      *
           WRITE REG-FSALIDA        FROM CPY-FSALIDA
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR MAESTRO'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CPY-FSALIDA
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
      *
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
           CLOSE MAESTRO
           CLOSE FSALIDA
           CLOSE ESCLAVO
      *
           IF FS-MAESTRO NOT = CT-00
              DISPLAY 'ERROR AL CERRAR MAESTRO'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-MAESTRO
           END-IF
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FSALIDA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
           END-IF
      *
           IF FS-ESCLAVO NOT = CT-00
              DISPLAY 'ERROR AL CERRAR ESCLAVO'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ESCLAVO
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
           DISPLAY '*REG MAESTRO: ' CN-REG-LEIDOS-MAESTRO  '           '
                   '                      *'
           DISPLAY '*REG ESCLAVO: ' CN-REG-LEIDOS-ESCLAVO  '           '
                   '                      *'
           DISPLAY '*REG FSALIDA: ' CN-REG-ESCRIT-FSALIDA  '           '
                   '                      *'
           DISPLAY '***************************************************'
      *
           .
      *
       3200-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 9000-LEER-MAESTRO                                             *
      ******************************************************************
      *
       9000-LEER-MAESTRO.
      *
           READ MAESTRO INTO CLIENTES
      *
           EVALUATE FS-MAESTRO
               WHEN CT-00
                    ADD CT-1               TO CN-REG-LEIDOS-MAESTRO
                    MOVE ID-CLIENT-M           TO CLAVE1
               WHEN CT-10
                    SET SW-SI-FIN-MAESTRO  TO TRUE
                    MOVE HIGH-VALUES       TO CLAVE1
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL FICHERO MAESTRO'
                    DISPLAY 'PARRAFO: 9000-LEER-MAESTRO'
                    DISPLAY 'FILE STATUS: ' FS-MAESTRO
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       9000-LEER-MAESTRO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 9100-LEER-ESCLAVO                                             *
      ******************************************************************
      *
       9100-LEER-ESCLAVO.
      *
           READ ESCLAVO INTO TRANSACCIONES
      *
           EVALUATE FS-ESCLAVO
               WHEN CT-00
                    ADD CT-1              TO CN-REG-LEIDOS-ESCLAVO
                    MOVE ID-CLIENT-E           TO CLAVE2
               WHEN CT-10
                    SET SW-SI-FIN-ESCLAVO TO TRUE
                    MOVE HIGH-VALUES      TO CLAVE2
               WHEN OTHER
                    DISPLAY 'ERROR AL LERR EL FICHERO ESCLAVO'
                    DISPLAY 'PARRAFO: 9100-LEER-ESCLAVO'
                    DISPLAY 'FILE STATUS: ' FS-ESCLAVO
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       9100-LEER-ESCLAVO-EXIT.
           EXIT.
