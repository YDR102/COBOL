       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  PGMPREST.
       AUTHOR.      DAVID.
       DATE-WRITTEN 22/04/2025.
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
           SELECT FINCIDE  ASSIGN TO FINCIDE
           FILE STATUS FS-FINCIDE.
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
       01  REG-FENTRADA                                       PIC X(77).
      *
       FD FINCIDE
           RECORDING MODE IS F.
       01  REG-FINCIDE                                        PIC X(77).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                                        PIC X(93).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA                         PIC X(02).
           05  FS-FSALIDA                          PIC X(02).
           05  FS-FINCIDE                          PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA              PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA               PIC 9(03).
           05  CN-REG-ESCRIT-FINCIDE               PIC 9(03).
      *
       01  CT-CONTANTES.
           05  CT-00                               PIC X(02) VALUE '00'.
           05  CT-0                                PIC 9(02) VALUE 0.
           05  CT-3                                PIC 9(01) VALUE 3.
           05  CT-100                              PIC 9(03) VALUE 100.
           05  CT-S                                PIC X(01) VALUE 'S'.
           05  CT-8                                PIC 9(03) VALUE 8.
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA                     PIC X(01).
               88  SW-SI-FIN-FENTRADA                         VALUE 'S'.
               88  SW-NO-FIN-FENTRADA                         VALUE 'N'.
      *
      *COPY DEL FICHERO DE ENTRADA FENTRADA
      *
       01  COPY-FENTRADA.
           05  NIF                                PIC X(09).
           05  NOMBRE                             PIC X(20).
           05  APELL1                             PIC X(20).
           05  APELL2                             PIC X(20).
           05  DATOS-CUOTA-PRESTAMO.
               10 NUM-CUOTA                       PIC 9(03).
               10 IMPORTE-CUOTA-BASE              PIC 9(04).
               10 IND-PENALIZACION                PIC X(01).
      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA
      *
       01  COPY-FSALIDA.
           05  NIF-S                              PIC X(09).
           05  NOMBRE-S                           PIC X(20).
           05  APELL1-S                           PIC X(20).
           05  APELL2-S                           PIC X(20).
           05  DATOS-CUOTA-PRESTAMO-S.
               10 NUM-CUOTA-S                     PIC 9(03).
               10 IMPORTE-CUOTA-BASE-S            PIC 9(04).
           05  CALCULOS-PRESTAMOS-S.
               10 INTERES-S                       PIC 9(03)V9(02).
               10 COMISION-S                      PIC 9(03)V9(02).
               10 TOTAL-PREST-S                   PIC 9(05)V9(02).
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
                      COPY-FENTRADA
                      COPY-FSALIDA
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
           OPEN OUTPUT FINCIDE
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
           IF FS-FINCIDE NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FINCIDE'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FINCIDE
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
           IF IND-PENALIZACION = CT-S
                WRITE REG-FINCIDE FROM COPY-FENTRADA
                IF FS-FINCIDE NOT = CT-00
                     DISPLAY 'ERROR AL ESCRIBIR FINCIDE'
                     DISPLAY 'PARRAFO: 2000-PROCESO'
                     DISPLAY 'FILE STATUS: ' FS-FINCIDE
      *
                     PERFORM 3000-FIN
                     THRU 3000-FIN-EXIT
               ELSE
                    ADD 1 TO CN-REG-ESCRIT-FINCIDE
                END-IF
           ELSE
                PERFORM 2100-INFORMAR-SALIDA
                   THRU 2100-INFORMAR-SALIDA-EXIT
                PERFORM 2500-ESCRIBIR-FSALIDA
                   THRU 2500-ESCRIBIR-FSALIDA-EXIT
           END-IF
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
           MOVE NIF                          TO NIF-S
           MOVE NOMBRE                       TO NOMBRE-S
           MOVE APELL1                       TO APELL1-S
           MOVE APELL2                       TO APELL2-S
           MOVE NUM-CUOTA                    TO NUM-CUOTA-S
           MOVE IMPORTE-CUOTA-BASE           TO IMPORTE-CUOTA-BASE-S
      *
           PERFORM 2200-CALCULAR-INTERES
              THRU 2200-CALCULAR-INTERES-EXIT
           PERFORM 2300-COMISION
              THRU 2300-COMISION-EXIT
           PERFORM 2400-TOTAL
              THRU 2400-TOTAL-EXIT

      *
           .
      *
       2100-INFORMAR-SALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-CALCULAR-INTERES                                          *
      ******************************************************************
      *
       2200-CALCULAR-INTERES.
      *
           MULTIPLY IMPORTE-CUOTA-BASE BY CT-3 GIVING INTERES-S
           DIVIDE INTERES-S BY CT-100 GIVING INTERES-S
      *
           .
      *
       2200-CALCULAR-INTERES-EXIT.
           EXIT.
      *
      ******************************************************************
      *  2300-COMISION                                                 *
      ******************************************************************
      *
       2300-COMISION.
      *
           IF IND-PENALIZACION = CT-S
             MULTIPLY IMPORTE-CUOTA-BASE BY CT-8 GIVING COMISION-S
             DIVIDE COMISION-S BY CT-100 GIVING COMISION-S
           ELSE
             MOVE CT-0 TO COMISION-S
           END-IF
      *
           .
      *
       2300-COMISION-EXIT.
           EXIT.

      *
      ******************************************************************
      * 2400-TOTAL                                                     *
      ******************************************************************
      *
       2400-TOTAL.
      *
           COMPUTE TOTAL-PREST-S=IMPORTE-CUOTA-BASE+INTERES-S+COMISION-S
      *
           .
      *
       2400-TOTAL-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2500-ESCRIBIR-FSALIDA                                          *
      ******************************************************************
      *
       2500-ESCRIBIR-FSALIDA.
      *
           WRITE REG-FSALIDA        FROM COPY-FSALIDA
      *
           IF FS-FSALIDA NOT = '00'
              DISPLAY 'ERROR AL ESCRIBIR FENTRADA'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE COPY-FSALIDA
              ADD 1                  TO CN-REG-ESCRIT-FSALIDA
           END-IF
      *
           .
      *
       2500-ESCRIBIR-FSALIDA-EXIT.
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
           CLOSE FINCIDE
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
           IF FS-FINCIDE NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FINCIDE'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FINCIDE
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
           DISPLAY '*REG.LEIDOS FENTRADA : ' CN-REG-LEIDOS-FENTRADA '  '
                   '                      *'
           DISPLAY '*REG.ESCRITOS FSALIDA: ' CN-REG-ESCRIT-FSALIDA  '  '
                   '                      *'
           DISPLAY '*REG.ESCRITOS FINCIDE  : ' CN-REG-ESCRIT-FINCIDE'  '
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
           READ FENTRADA INTO COPY-FENTRADA
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
