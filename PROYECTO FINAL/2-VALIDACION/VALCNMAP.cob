       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  VALCNMAP.
       AUTHOR.      DAVID.
       DATE-WRITTEN 16/06/2025.
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
           SELECT DESCARTE ASSIGN TO DESCARTE
           FILE STATUS FS-DESCARTE.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD ENTRADA1
           RECORDING MODE IS F.
       01  REG-ENTRADA1                                     PIC X(2540).
      * SEGUROS
      *
       FD ENTRADA2
           RECORDING MODE IS F.
       01  REG-ENTRADA2                                     PIC X(713).
      * CLIENTES
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                                      PIC X(713).
      *
       FD DESCARTE
           RECORDING MODE IS F.
       01  REG-DESCARTE                                     PIC X(713).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-ENTRADA1                         PIC X(02).
           05  FS-ENTRADA2                         PIC X(02).
           05  FS-FSALIDA                          PIC X(02).
           05  FS-DESCARTE                         PIC X(02).
      *
       01  WK-VARIABLES.
           05 CLAVE1                               PIC X(09).
           05 CLAVE2                               PIC X(09).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-ENTRADA1              PIC 9(03).
           05  CN-REG-LEIDOS-ENTRADA2              PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA               PIC 9(03).
           05  CN-REG-ESCRIT-DESCARTES             PIC 9(03).
      *
       01  CT-CONTANTES.
           05  CT-00                               PIC X(02) VALUE '00'.
           05  CT-10                               PIC X(02) VALUE '10'.
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
      *COPY CLAVE1
       COPY CPYCLISA.
      *
      *COPY DEL FICHERO DE ENTRADA ENTRADA2
      *
      *COPY CLAVE2
       COPY CPYSEGFI.
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
             UNTIL SW-SI-FIN-ENTRADA1 AND SW-SI-FIN-ENTRADA2
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
                      DATOS-SEG
                      CPYCLISA
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
           OPEN INPUT ENTRADA2
           OPEN OUTPUT FSALIDA
           OPEN OUTPUT DESCARTE
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
           IF FS-ENTRADA2 NOT = CT-00
              DISPLAY 'ERROR AL ABRIR ENTRADA2'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ENTRADA2
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
           IF FS-DESCARTE NOT = CT-00
              DISPLAY 'ERROR AL ABRIR DESCARTE'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-DESCARTE
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
      ******************************************************************
      *
       2000-PROCESO.
      *
           IF CLAVE1 = CLAVE2

              PERFORM 2200-ESCRIBIR-DESCARTE
                 THRU 2200-ESCRIBIR-DESCARTE-EXIT

              PERFORM 9100-LEER-ENTRADA2
                 THRU 9100-LEER-ENTRADA2-EXIT
           ELSE
              IF CLAVE1 < CLAVE2
                 DISPLAY 'CLAVE1 NO EXISTE EN FICHERO2'
                 PERFORM 9000-LEER-ENTRADA1
                    THRU 9000-LEER-ENTRADA1-EXIT
              ELSE
                 DISPLAY 'CLAVE2 NO EXISTE EN FICHERO1'

                 PERFORM 2100-ESCRIBIR-FSALIDA
                    THRU 2100-ESCRIBIR-FSALIDA-EXIT

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
      * 2100-ESCRIBIR-FSALIDA                                          *
      ******************************************************************
      *
       2100-ESCRIBIR-FSALIDA.
      *
           WRITE REG-FSALIDA        FROM CPYCLISA
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA'
              DISPLAY 'PARRAFO: 2100-ESCRIBIR-FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CPYCLISA
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA
           END-IF
      *
           .
      *
       2100-ESCRIBIR-FSALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-DESCARTE                                         *
      ******************************************************************
      *
       2200-ESCRIBIR-DESCARTE.
      *
           WRITE REG-DESCARTE        FROM CPYCLISA
      *
           IF FS-DESCARTE NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR DESCARTES'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-DESCARTE'
              DISPLAY 'FILE STATUS: ' FS-DESCARTE
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CPYCLISA
              ADD CT-1                 TO CN-REG-ESCRIT-DESCARTES
           END-IF
      *
           .
      *
       2200-ESCRIBIR-DESCARTE-EXIT.
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
           CLOSE ENTRADA2
           CLOSE FSALIDA
           CLOSE DESCARTE
      *
           IF FS-ENTRADA1 NOT = CT-00
              DISPLAY 'ERROR AL CERRAR ENTRADA1'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ENTRADA1
           END-IF
      *
           IF FS-ENTRADA2 NOT = CT-00
              DISPLAY 'ERROR AL CERRAR ENTRADA2'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ENTRADA2
           END-IF
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FSALIDA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
           END-IF
      *
           IF FS-DESCARTE NOT = CT-00
              DISPLAY 'ERROR AL CERRAR DESCARTE'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-DESCARTE
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
           DISPLAY '**********************'
           DISPLAY '*    ESTADISTICAS    *'
           DISPLAY '**********************'
           DISPLAY '*REG  ENTRADA1:      *' CN-REG-LEIDOS-ENTRADA1
           DISPLAY '*REG  ENTRADA2:      *' CN-REG-LEIDOS-ENTRADA2
           DISPLAY '*REG   FSALIDA:      *' CN-REG-ESCRIT-FSALIDA
           DISPLAY '*REG DESCARTES:      *' CN-REG-ESCRIT-DESCARTES
           DISPLAY '**********************'
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
           READ ENTRADA1 INTO DATOS-SEG
      *
           EVALUATE FS-ENTRADA1
               WHEN CT-00
                    ADD CT-1                TO CN-REG-LEIDOS-ENTRADA1
                    MOVE DNI-CL-SEG         TO CLAVE1
               WHEN CT-10
                    MOVE HIGH-VALUES        TO CLAVE1
                    SET SW-SI-FIN-ENTRADA1  TO TRUE
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
           READ ENTRADA2 INTO CPYCLISA
      *
           EVALUATE FS-ENTRADA2
               WHEN CT-00
                    ADD CT-1               TO CN-REG-LEIDOS-ENTRADA2
                    MOVE DNI-CL-S          TO CLAVE2
               WHEN CT-10
                    MOVE HIGH-VALUES       TO CLAVE2
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
