       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  PGMCONCI.
       AUTHOR.      DAVID.
       DATE-WRITTEN 17/06/2025.
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
           SELECT FSALIDA1 ASSIGN TO FSALIDA1
           FILE STATUS FS-FSALIDA1.
      *
           SELECT FSALIDA2 ASSIGN TO FSALIDA2
           FILE STATUS FS-FSALIDA2.
      *
           SELECT FSALIDA3 ASSIGN TO FSALIDA3
           FILE STATUS FS-FSALIDA3.
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD ENTRADA1
           RECORDING MODE IS F.
       01  REG-ENTRADA1                           PIC X(2540).
      *
       FD FSALIDA1
           RECORDING MODE IS F.
       01  REG-FSALIDA1                           PIC X(66).
      *
       FD FSALIDA2
           RECORDING MODE IS F.
       01  REG-FSALIDA2                           PIC X(61).
      *
       FD FSALIDA3
           RECORDING MODE IS F.
       01  REG-FSALIDA3                           PIC X(55).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-ENTRADA1                        PIC X(02).
           05  FS-FSALIDA1                        PIC X(02).
           05  FS-FSALIDA2                        PIC X(02).
           05  FS-FSALIDA3                        PIC X(02).
      *
       01  WK-VARIABLES.
           05 CLAVE1                              PIC X(10).
           05 CLAVE2                              PIC X(10).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-ENTRADA1             PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA1             PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA2             PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA3             PIC 9(03).
      *
       01  CT-CONTANTES.
           05  CT-00                              PIC X(02) VALUE '00'.
           05  CT-10                              PIC X(02) VALUE '10'.
           05  CT-1                               PIC 9(02) VALUE 1.
      *
       01  SW-SWITCHES.
           05  SW-FIN-ENTRADA1                    PIC X(01).
               88  SW-SI-FIN-ENTRADA1                         VALUE 'S'.
               88  SW-NO-FIN-ENTRADA1                         VALUE 'N'.
      *
      *COPY DEL FICHERO DE ENTRADA ENTRADA1
      *
       COPY CPYSEGFI.

      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA1
      *
      *COPY CPYCONC3.

      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA2
      *
      *COPY CPYCONC4.

      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA3
      *
      *COPY CPYCONC5.
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
             UNTIL SW-SI-FIN-ENTRADA1
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
                      WK-VARIABLES
                      SOLO-CENTRAL
                      SOLO-NACIONAL
                      REG-CENTRAL
                      REG-NACIONAL
                      REG-MATCHING
      *
           SET SW-NO-FIN-ENTRADA1               TO TRUE
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT
           PERFORM 9000-LEER-ENTRADA1
              THRU 9000-LEER-ENTRADA1-EXIT
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
           OPEN OUTPUT FSALIDA1
           OPEN OUTPUT FSALIDA2
           OPEN OUTPUT FSALIDA3
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
           IF FS-FSALIDA1 NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FSALIDA1'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA1
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FSALIDA2 NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FSALIDA2'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA2
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FSALIDA3 NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FSALIDA3'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA3
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

      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA-1                                         *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA-1.
      *
      *    MOVE ID-CLIENTE-CEN         TO ID-CLIENTE-MAT
      *
           .
      *
       2100-INFORMAR-SALIDA-1-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA-1                                        *
      ******************************************************************
      *
       2200-ESCRIBIR-FSALIDA-1.
      *
           WRITE REG-FSALIDA1        FROM REG-MATCHING
      *
           IF FS-FSALIDA1 NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA1'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA-1'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA1
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE REG-MATCHING
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA1
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-1-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA-2                                         *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA-2.
      *
      *    MOVE ID-CLIENTE-CEN         TO ID-CLIENTE-SC
      *
           .
      *
       2100-INFORMAR-SALIDA-2-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA-2                                        *
      ******************************************************************
      *
       2200-ESCRIBIR-FSALIDA-2.
      *
           WRITE REG-FSALIDA2        FROM SOLO-CENTRAL
      *
           IF FS-FSALIDA1 NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA-2'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA-2'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA2
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE SOLO-CENTRAL
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA2
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-2-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA-3                                         *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA-3.
      *
      *    MOVE ID-CLIENTE-NAC         TO ID-CLIENTE-SN
      *
           .
      *
       2100-INFORMAR-SALIDA-3-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA-3                                        *
      ******************************************************************
      *
       2200-ESCRIBIR-FSALIDA-3.
      *
           WRITE REG-FSALIDA3        FROM SOLO-NACIONAL
      *
           IF FS-FSALIDA3 NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA3'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA-3'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA3
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE SOLO-NACIONAL
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA3
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-3-EXIT.
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
           CLOSE FSALIDA1
           CLOSE FSALIDA2
           CLOSE FSALIDA3
      *
           IF FS-ENTRADA1 NOT = CT-00
              DISPLAY 'ERROR AL CERRAR ENTRADA1'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ENTRADA1
           END-IF
      *
           IF FS-FSALIDA1 NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FSALIDA1'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA1
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FSALIDA2 NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FSALIDA2'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA2
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FSALIDA3 NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FSALIDA3'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA3
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
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
           DISPLAY '*REG FSALIDA1: ' CN-REG-ESCRIT-FSALIDA1 '          '
                   '                      *'
           DISPLAY '*REG FSALIDA2: ' CN-REG-ESCRIT-FSALIDA2 '          '
                   '                      *'
           DISPLAY '*REG FSALIDA3: ' CN-REG-ESCRIT-FSALIDA3 '          '
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
           READ ENTRADA1 INTO REG-CENTRAL
      *
           EVALUATE FS-ENTRADA1
               WHEN CT-00
                    ADD CT-1               TO CN-REG-LEIDOS-ENTRADA1
                    MOVE ID-CLIENTE-CEN    TO CLAVE1
               WHEN CT-10
                    SET SW-SI-FIN-ENTRADA1  TO TRUE
                    MOVE HIGH-VALUES       TO CLAVE1
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