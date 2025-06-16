       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  PGMCONCI.
       AUTHOR.      DAVID.
       DATE-WRITTEN 29/04/2025.
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
       01  REG-ENTRADA1                           PIC X(61).
      *
       FD ENTRADA2
           RECORDING MODE IS F.
       01  REG-ENTRADA2                           PIC X(55).
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
           05  FS-ENTRADA2                        PIC X(02).
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
           05  CN-REG-LEIDOS-ENTRADA2             PIC 9(03).
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
           05  SW-FIN-ENTRADA2                    PIC X(01).
               88  SW-SI-FIN-ENTRADA2                         VALUE 'S'.
               88  SW-NO-FIN-ENTRADA2                         VALUE 'N'.
      *
      *COPY DEL FICHERO DE ENTRADA ENTRADA1
      *
      *COPY CPYCONC1.
       01 REG-CENTRAL.
           05 ID-CLIENTE-CEN                      PIC X(10).
           05 NOMBRE-CEN                          PIC X(30).
           05 FECHA-ALTA-CEN.
                10  DIA                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  MES                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  ANNO                          PIC X(04).
           05 SALDO-CEN                           PIC 9(8)V99.
           05 TIPO-CUENTA-CEN                     PIC X(01).
      *
      *COPY DEL FICHERO DE ENTRADA ENTRADA2
      *
      *COPY CPYCONC2.
       01 REG-NACIONAL.
           05 ID-CLIENTE-NAC                      PIC X(10).
           05 NOMBRE-NAC                          PIC X(30).
           05 FECHA-ALTA-NAC.
                10  DIA                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  MES                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  ANNO                          PIC X(04).
           05 SUCURSAL-NAC                        PIC 9(4).
           05 ESTADO-NAC                          PIC X(01).
      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA1
      *
      *COPY CPYCONC3.
       01 REG-MATCHING.
           05 ID-CLIENTE-MAT                      PIC X(10).
           05 NOMBRE-MAT                          PIC X(30).
           05 FECHA-ALTA-MAT.
                10  DIA                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  MES                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  ANNO                          PIC X(04).
           05 SALDO-MAT                           PIC 9(8)V99.
           05 SUCURSAL-MAT                        PIC 9(4).
           05 ESTADO-MAT                          PIC X(01).
           05 TIPO-CUENTA-MAT                     PIC X(01).
      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA2
      *
      *COPY CPYCONC4.
       01 SOLO-CENTRAL.
           05 ID-CLIENTE-SC                       PIC X(10).
           05 NOMBRE-SC                           PIC X(30).
           05 FECHA-ALTA-SC.
                10  DIA                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  MES                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  ANNO                          PIC X(04).
           05 SALDO-SC                            PIC 9(8)V99.
           05 TIPO-CUENTA-SC                      PIC X(01).
      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA3
      *
      *COPY CPYCONC5.
       01 SOLO-NACIONAL.
           05 ID-CLIENTE-SN                       PIC X(10).
           05 NOMBRE-SN                           PIC X(30).
           05 FECHA-ALTA-SN.
                10  DIA                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  MES                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  ANNO                          PIC X(04).
           05 SUCURSAL-SN                         PIC 9(4).
           05 ESTADO-SN                           PIC X(01).
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
                      REG-CENTRAL
                      REG-NACIONAL
                      REG-MATCHING
                      WK-VARIABLES
                      SOLO-CENTRAL
                      SOLO-NACIONAL
      *
           MOVE ID-CLIENTE-CEN    TO CLAVE1
           MOVE ID-CLIENTE-NAC    TO CLAVE2
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
           IF FS-ENTRADA2 NOT = CT-00
              DISPLAY 'ERROR AL ABRIR ENTRADA2'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ENTRADA2
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
      * Mover la calve a su variable correspondiente
      *
           DISPLAY 'CLAVE1: ' CLAVE1
           DISPLAY 'CLAVE2: ' CLAVE2
           IF CLAVE1 = CLAVE2
              DISPLAY 'MATCH'
              PERFORM 2100-INFORMAR-SALIDA-1
                 THRU 2100-INFORMAR-SALIDA-1-EXIT
      *
              PERFORM 2200-ESCRIBIR-FSALIDA-1
                 THRU 2200-ESCRIBIR-FSALIDA-1-EXIT
      *
              PERFORM 9000-LEER-ENTRADA1
                 THRU 9000-LEER-ENTRADA1-EXIT
      *
              PERFORM 9100-LEER-ENTRADA2
                 THRU 9100-LEER-ENTRADA2-EXIT
           ELSE
              IF CLAVE1 < CLAVE2
                 DISPLAY ' CLAVE1 MENOR QUE CLAVE2'
                 DISPLAY 'Clientes solo en la Sucursal Central '
                 '(posibles altas pendientes de propagar)'
                 PERFORM 2100-INFORMAR-SALIDA-2
                    THRU 2100-INFORMAR-SALIDA-2-EXIT
      *
                 PERFORM 2200-ESCRIBIR-FSALIDA-2
                    THRU 2200-ESCRIBIR-FSALIDA-2-EXIT
      *
                 PERFORM 9000-LEER-ENTRADA1
                    THRU 9000-LEER-ENTRADA1-EXIT
              ELSE
                 DISPLAY ' CLAVE1 MAYOR QUE CLAVE2'
                 DISPLAY 'Clientes solo en el Sistema Nacional '
                 '(posibles bajas no registradas)'
                 PERFORM 2100-INFORMAR-SALIDA-3
                    THRU 2100-INFORMAR-SALIDA-3-EXIT
      *
                 PERFORM 2200-ESCRIBIR-FSALIDA-3
                    THRU 2200-ESCRIBIR-FSALIDA-3-EXIT
      *
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
      * 2100-INFORMAR-SALIDA-1                                         *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA-1.
      *
           MOVE ID-CLIENTE-CEN         TO ID-CLIENTE-MAT
           MOVE NOMBRE-CEN             TO NOMBRE-MAT
           MOVE FECHA-ALTA-CEN         TO FECHA-ALTA-MAT
           MOVE SALDO-CEN              TO SALDO-MAT
           MOVE SUCURSAL-NAC           TO SUCURSAL-MAT
           MOVE ESTADO-NAC             TO ESTADO-MAT
           MOVE TIPO-CUENTA-CEN        TO TIPO-CUENTA-MAT
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
           MOVE ID-CLIENTE-CEN         TO ID-CLIENTE-SC
           MOVE NOMBRE-CEN             TO NOMBRE-SC
           MOVE FECHA-ALTA-CEN         TO FECHA-ALTA-SC
           MOVE SALDO-CEN              TO SALDO-SC
           MOVE TIPO-CUENTA-CEN        TO TIPO-CUENTA-SC
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
           MOVE ID-CLIENTE-NAC         TO ID-CLIENTE-SN
           MOVE NOMBRE-NAC             TO NOMBRE-SN
           MOVE FECHA-ALTA-NAC         TO FECHA-ALTA-SN
           MOVE SUCURSAL-NAC           TO SUCURSAL-SN
           MOVE ESTADO-NAC             TO ESTADO-SN
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
           CLOSE ENTRADA2
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
           IF FS-ENTRADA2 NOT = CT-00
              DISPLAY 'ERROR AL CERRAR ENTRADA2'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ENTRADA2
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
           DISPLAY '*REG ENTRADA2: ' CN-REG-LEIDOS-ENTRADA2 '          '
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
      *
      ******************************************************************
      * 9100-LEER-ENTRADA2                                             *
      ******************************************************************
      *
       9100-LEER-ENTRADA2.
      *
           READ ENTRADA2 INTO REG-NACIONAL
      *
           EVALUATE FS-ENTRADA2
               WHEN CT-00
                    ADD CT-1              TO CN-REG-LEIDOS-ENTRADA2
                    MOVE ID-CLIENTE-NAC   TO CLAVE2
               WHEN CT-10
                    SET SW-SI-FIN-ENTRADA2 TO TRUE
                    MOVE HIGH-VALUES       TO CLAVE2
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