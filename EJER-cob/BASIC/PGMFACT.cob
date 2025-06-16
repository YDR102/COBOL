       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  PGMFACT.
       AUTHOR.      DAVID.
       DATE-WRITTEN 23/04/2025.
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FENTRADA ASSIGN TO FENTRADA
           FILE STATUS FS-FENTRADA.
      *
           SELECT FESTADO  ASSIGN TO FESTADO
           FILE STATUS FS-FESTADO.
      *
           SELECT FSALIDA ASSIGN TO FSALIDA
           FILE STATUS FS-FSALIDA.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD FENTRADA
           RECORDING MODE IS F.
       01  REG-FENTRADA                           PIC X(20).
      *
       FD FESTADO
           RECORDING MODE IS F.
       01  REG-FESTADO                            PIC X(35).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                            PIC X(14).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA                        PIC X(02).
           05  FS-FSALIDA                         PIC X(02).
           05  FS-FESTADO                         PIC X(02).
      *
       01  VARIAVLES.
           05  CLI-ANTE                           PIC X(05).
           05  IMPOT-CAUMULADO                    PIC 9(05).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA             PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA              PIC 9(03).
           05  CN-REG-ESCRIT-FESTADO              PIC 9(03).
      *
       01  CT-CONTANTES.
           05  CT-00                              PIC X(02) VALUE '00'.
           05  CT-MY1000                   PIC X(10) VALUE 'MAYOR 1000'.
           05  CT-MN1000                   PIC X(10) VALUE 'MANOR 1000'.
           05  CT-1000                     PIC 9(04) VALUE 1000.
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA                    PIC X(01).
               88  SW-SI-FIN-FENTRADA                       VALUE 'S'.
               88  SW-NO-FIN-FENTRADA                       VALUE 'N'.
      *
      *COPY DEL FICHERO DE ENTRADA FENTRADA
      *
       01  CPY-FACTURA.
           05  C-CLIENTE                          PIC X(05).
           05  NUM-FACTURA                        PIC X(08).
           05  IMPORT-FACTURA                     PIC 9(05)V9(02).
      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA
      *
       01  CPY-FACTURA-S1.
           05  C-CLIENTE-S1                       PIC X(05).
           05  TOTAL-FACTURADO-S1                 PIC 9(07)V9(02).
      *
      *COPY DEL FICHERO DE ENTRADA FESTADO
      *
       01 CPY-FACTURA-S2.
           05 C-CLIENTE-S2                        PIC X(05).
           05 NUM-FACTURA-S2                      PIC X(08).
           05 IMPORT-FACTURA-S2                   PIC 9(05)V9(02).
           05 ESTADO-S2                           PIC X(15).
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
             UNTIL SW-SI-FIN-FENTRADA
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
                      CPY-FACTURA
                      CPY-FACTURA-S1
                      CPY-FACTURA-S2
                      IMPOT-CAUMULADO
      *
           SET SW-NO-FIN-FENTRADA               TO TRUE
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT
           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
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
           OPEN INPUT FENTRADA
           OPEN OUTPUT FSALIDA
           OPEN OUTPUT FESTADO
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FENTRADA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
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
           IF FS-FESTADO NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FESTADO'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FESTADO
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
           PERFORM 9100-GUARDAR-DATOS
              THRU 9100-GUARDAR-DATOS-EXIT
      *
           PERFORM 2100-INFORMAR-SALIDA
              THRU 2100-INFORMAR-SALIDA-EXIT
           PERFORM 2300-ESCRIBIR-FSALIDA
              THRU 2300-ESCRIBIR-FSALIDA-EXIT
           PERFORM 2400-ESCRIBIR-FESTADO
              THRU 2400-ESCRIBIR-FESTADO-EXIT
      *
           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
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
           MOVE C-CLIENTE                    TO C-CLIENTE-S1
           MOVE C-CLIENTE                    TO C-CLIENTE-S2
           MOVE NUM-FACTURA                  TO NUM-FACTURA-S2
           MOVE IMPORT-FACTURA               TO NUM-FACTURA-S2
      *
           PERFORM 2500-ESTADO
              THRU 2500-ESTADO-EXIT
           PERFORM 2200-TATAL-FACTURADO
              THRU 2200-TATAL-FACTURADO-EXIT
      *
           .
      *
       2100-INFORMAR-SALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-TATAL-FACTURADO                                           *
      ******************************************************************
      *
       2200-TATAL-FACTURADO.
      *
           IF CLI-ANTE NOT = C-CLIENTE
                ADD IMPORT-FACTURA TO IMPOT-CAUMULADO
                GIVING IMPORT-FACTURA
           ELSE
                INITIALIZE IMPOT-CAUMULADO
           END-IF
      *
           .
      *
       2200-TATAL-FACTURADO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2300-ESCRIBIR-FSALIDA                                          *
      ******************************************************************
      *
       2300-ESCRIBIR-FSALIDA.
      *
           WRITE REG-FSALIDA        FROM CPY-FACTURA-S1
      *
           IF FS-FSALIDA NOT = '00'
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA'
              DISPLAY 'PARRAFO: 2300-ESCRIBIR-FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CPY-FACTURA-S1
              ADD 1                  TO CN-REG-ESCRIT-FSALIDA
           END-IF
      *
           .
      *
       2300-ESCRIBIR-FSALIDA-EXIT.
           EXIT.

      *
      ******************************************************************
      * 2400-ESCRIBIR-FESTADO                                          *
      ******************************************************************
      *
       2400-ESCRIBIR-FESTADO.
      *
           WRITE REG-FESTADO        FROM CPY-FACTURA-S2
      *
           IF FS-FSALIDA NOT = '00'
              DISPLAY 'ERROR AL ESCRIBIR FENTRADA'
              DISPLAY 'PARRAFO: 2400-ESCRIBIR-FESTADO'
              DISPLAY 'FILE STATUS: ' FS-FESTADO
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CPY-FACTURA-S1
              ADD 1                  TO CN-REG-ESCRIT-FESTADO
           END-IF
           .
      *
       2400-ESCRIBIR-FESTADO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2500-ESTADO                                                    *
      ******************************************************************
      *
       2500-ESTADO.
      *
           IF IMPORT-FACTURA > CT-1000
              MOVE CT-MY1000                 TO ESTADO-S2
           ELSE
              MOVE CT-MN1000                 TO ESTADO-S2
           END-IF

      *
           .
      *
       2500-ESTADO-EXIT.
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
           CLOSE FENTRADA
           CLOSE FSALIDA
           CLOSE FESTADO
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FENTRADA'
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
           IF FS-FESTADO NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FESTADO'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FESTADO
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
           DISPLAY '*REG.ENTRADA: ' CN-REG-LEIDOS-FENTRADA '           '
                   '                      *'
           DISPLAY '*REG.FSALIDA: ' CN-REG-ESCRIT-FSALIDA  '           '
                   '                      *'
           DISPLAY '*REG.FESTADO: ' CN-REG-ESCRIT-FESTADO'             '
                   '                      *'
           DISPLAY '***************************************************'
      *
           .
      *
       3200-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 9000-LEER-FENTRADA                                             *
      ******************************************************************
      *
       9000-LEER-FENTRADA.
      *
           READ FENTRADA INTO CPY-FACTURA
      *
           EVALUATE FS-FENTRADA
               WHEN '00'
                    ADD 1                  TO CN-REG-LEIDOS-FENTRADA
               WHEN '10'
                    SET SW-SI-FIN-FENTRADA TO TRUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL FICHERO FENTRADA'
                    DISPLAY 'PARRAFO: 9000-LEER-FENTRADA'
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
      *
      ******************************************************************
      * 9100-GUARDAR-DATOS                                             *
      ******************************************************************
      *
       9100-GUARDAR-DATOS.
           MOVE C-CLIENTE                    TO CLI-ANTE
      *
           .
      *
       9100-GUARDAR-DATOS-EXIT.
           EXIT.


