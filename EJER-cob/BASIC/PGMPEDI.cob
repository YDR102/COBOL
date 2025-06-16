       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  PGMPEDI.
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
           SELECT FSALIDA1 ASSIGN TO FSALIDA1
           FILE STATUS FS-FSALIDA1.
      *
           SELECT FSALIDA2 ASSIGN TO FSALIDA2
           FILE STATUS FS-FSALIDA2.
      *
           SELECT FSALIDA3 ASSIGN TO FSALIDA3
           FILE STATUS FS-FSALIDA3.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD MAESTRO
           RECORDING MODE IS F.
       01  REG-MAESTRO                            PIC X(068).
      *
       FD ESCLAVO
           RECORDING MODE IS F.
       01  REG-ESCLAVO                            PIC X(074).
      *
       FD FSALIDA1
           RECORDING MODE IS F.
       01  REG-FSALIDA1                           PIC X(073).
      *
       FD FSALIDA2
           RECORDING MODE IS F.
       01  REG-FSALIDA2                           PIC X(058).
      *
       FD FSALIDA3
           RECORDING MODE IS F.
       01  REG-FSALIDA3                           PIC X(061).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-MAESTRO                         PIC X(02).
           05  FS-ESCLAVO                         PIC X(02).
           05  FS-FSALIDA1                        PIC X(02).
           05  FS-FSALIDA2                        PIC X(02).
           05  FS-FSALIDA3                        PIC X(02).
      *
       01  WK-VARIABLES.
           05 CLAVE1                              PIC X(08).
           05 CLAVE2                              PIC X(08).
           05 ACUMULADO-COMPROBAR                 PIC 9(10)V99.
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-MAESTRO              PIC 9(03).
           05  CN-REG-LEIDOS-ESCLAVO              PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA1             PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA2             PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA3             PIC 9(03).
           05  CN-LIENAS                          PIC 9(03).
      *
       01  CT-CONTANTES.
           05  CT-00                              PIC X(02) VALUE '00'.
           05  CT-10                              PIC X(02) VALUE '10'.
           05  CT-1                               PIC 9(02) VALUE 1.
           05  CT-0                               PIC 9(02) VALUE 0.
           05  CT-MAX                          PIC 9(08) VALUE 99999999.
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
      *COPY CPYPEDI1.
       01 REG-PEDIDO-MAESTRO.
           05 NRO-PEDIDO-M                        PIC 9(08).
           05 FECHA-ALTA-CEN-M.
                10  ANNO                          PIC X(04).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  MES                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  DIA                           PIC X(02).
           05 COD-CLIENTE-M                       PIC X(10).
           05 NOMBRE-CLIENTE-M                    PIC X(30).
           05 TOTAL-PEDIDO-M                      PIC 9(08)V99.
      *
      *COPY DEL FICHERO DE ENTRADA ESCLAVO
      *
      *COPY CPYPEDI2.
       01 REG-PEDIDO-ESCLAVO.
           05 NRO-PEDIDO-E                        PIC 9(08).
           05 NUM-LINEA-E                         PIC 9(03).
           05 COD-PRODUCTO-E                      PIC X(10).
           05 DESCRIPCION-E                       PIC X(30).
           05 CANTIDAD-E                          PIC 9(05).
           05 PRECIO-UNITARIO-E                   PIC 9(06)V99.
           05 IMPORTE-LINEA-E                     PIC 9(08)V99.
      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA1
      *
      *COPY CPYPEDI3.
       01 REG-PEDIDO-COMPLETO.
           05 NRO-PEDIDO-COMP                     PIC 9(08).
           05 FECHA-COMP.
                10  ANNO                          PIC X(04).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  MES                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  DIA                           PIC X(02).
           05 COD-CLIENTE-COMP                    PIC X(10).
           05 NOMBRE-CLIENTE-COMP                 PIC X(30).
           05 TOTAL-CALCULADO-COMP                PIC 9(10)V99.
           05 CANT-LINEAS-COMP                    PIC 9(03).
      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA2
      *
      *COPY CPYPEDI4.
      *
       01 REG-PEDIDO-SIN-LINEAS.
           05 NRO-PEDIDO-SL                       PIC 9(08).
           05 FECHA-SL.
                10  ANNO                          PIC X(04).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  MES                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  DIA                           PIC X(02).
           05 COD-CLIENTE-SL                      PIC X(10).
           05 NOMBRE-CLIENTE-SL                   PIC X(30).
      *COPY DEL FICHERO DE ENTRADA FSALIDA3
      *
      *COPY CPYPEDI5.
       01 REG-LINEA-ERRONEA.
           05 NRO-PEDIDO-ERR                      PIC 9(08).
           05 NUM-LINEA-ERR                       PIC 9(03).
           05 COD-PRODUCTO-ERR                    PIC X(10).
           05 DESCRIPCION-ERR                     PIC X(30).
           05 CANTIDAD-ERR                        PIC 9(05).
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
                      REG-PEDIDO-MAESTRO
                      REG-PEDIDO-ESCLAVO
                      REG-PEDIDO-COMPLETO
                      REG-PEDIDO-SIN-LINEAS
                      REG-LINEA-ERRONEA
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
           OPEN OUTPUT FSALIDA1
           OPEN OUTPUT FSALIDA2
           OPEN OUTPUT FSALIDA3
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
           IF FS-ESCLAVO NOT = CT-00
              DISPLAY 'ERROR AL ABRIR ESCLAVO'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ESCLAVO
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
      *    Validar que todos los pedidos tengan al menos una línea     *
      *    Identificar pedidos sin líneas (error)                      *
      *    Identificar líneas de pedido sin cabecera (error)           *
      *    Generar un informe consolidado con el total por pedido      *
      *    Validar que el TOTAL-PEDIDO coincida con de IMPORTE-LINEA   *
      *    Para cada pedido, leer todas sus líneas correspondientes    *
      ******************************************************************
      *
       2000-PROCESO.
      *
           IF CLAVE1 = CLAVE2
              COMPUTE TOTAL-CALCULADO-COMP = TOTAL-CALCULADO-COMP
                                           + IMPORTE-LINEA-E
              ADD CT-1                     TO CANT-LINEAS-COMP
              PERFORM 9100-LEER-ESCLAVO
                 THRU 9100-LEER-ESCLAVO-EXIT
           ELSE
              IF CLAVE1 < CLAVE2
                IF CANT-LINEAS-COMP = CT-0
                   PERFORM 2100-INFORMAR-SALIDA-PEDI
                      THRU 2100-INFORMAR-SALIDA-PEDI-EXIT
      *
                   PERFORM 2200-ESCRIBIR-SALIDA-PEDI
                      THRU 2200-ESCRIBIR-SALIDA-PEDI-EXIT
                ELSE
                    PERFORM 2100-INFORMAR-SALIDA-COMP
                       THRU 2100-INFORMAR-SALIDA-COMP-EXIT
      *
                    PERFORM 2200-ESCRIBIR-SALIDA-COMP
                       THRU 2200-ESCRIBIR-SALIDA-COMP-EXIT
                END-IF
                PERFORM 2100-INFORMAR-SALIDA-COMP
                   THRU 2100-INFORMAR-SALIDA-COMP-EXIT
      *
                PERFORM 2200-ESCRIBIR-SALIDA-COMP
                   THRU 2200-ESCRIBIR-SALIDA-COMP-EXIT
      *
                PERFORM 9000-LEER-MAESTRO
                   THRU 9000-LEER-MAESTRO-EXIT
              ELSE

                PERFORM 2100-INFORMAR-SALIDA-ERR
                   THRU 2100-INFORMAR-SALIDA-ERR-EXIT
      *
                PERFORM 2200-ESCRIBIR-SALIDA-ERR
                   THRU 2200-ESCRIBIR-SALIDA-ERR-EXIT
      *
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
      * 2100-INFORMAR-SALIDA-COMP                                      *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA-COMP.
      *
           MOVE NRO-PEDIDO-M         TO NRO-PEDIDO-COMP
           MOVE FECHA-ALTA-CEN-M     TO FECHA-COMP
           MOVE COD-CLIENTE-M        TO COD-CLIENTE-COMP
           MOVE NOMBRE-CLIENTE-M     TO NOMBRE-CLIENTE-COMP

      *
           .
      *
       2100-INFORMAR-SALIDA-COMP-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA-COMP                                     *
      ******************************************************************
      *
       2200-ESCRIBIR-SALIDA-COMP.
      *
           WRITE REG-FSALIDA1        FROM REG-PEDIDO-COMPLETO
      *
           IF FS-FSALIDA1 NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA1'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA-1'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA1
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE REG-PEDIDO-COMPLETO
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA1
           END-IF
      *
           .
      *
       2200-ESCRIBIR-SALIDA-COMP-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA-PEDIDOS                                   *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA-PEDI.
      *
           MOVE NRO-PEDIDO-M         TO NRO-PEDIDO-SL
           MOVE FECHA-ALTA-CEN-M     TO FECHA-SL
           MOVE COD-CLIENTE-M        TO COD-CLIENTE-SL
           MOVE NOMBRE-CLIENTE-M     TO NOMBRE-CLIENTE-SL
      *
           .
      *
       2100-INFORMAR-SALIDA-PEDI-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA-PEDIDOS                                  *
      ******************************************************************
      *
       2200-ESCRIBIR-SALIDA-PEDI.
      *
           WRITE REG-FSALIDA2        FROM REG-PEDIDO-SIN-LINEAS
      *
           IF FS-FSALIDA1 NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA-2'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA-2'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA2
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE REG-PEDIDO-SIN-LINEAS
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA2
           END-IF
      *
           .
      *
       2200-ESCRIBIR-SALIDA-PEDI-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA-ERROR                                     *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA-ERR.
      *
           MOVE NRO-PEDIDO-E        TO NRO-PEDIDO-ERR
           MOVE NUM-LINEA-E         TO NUM-LINEA-ERR
           MOVE COD-PRODUCTO-E      TO COD-PRODUCTO-ERR
           MOVE DESCRIPCION-E       TO DESCRIPCION-ERR
           MOVE CANTIDAD-E          TO CANTIDAD-ERR
      *
           .
      *
       2100-INFORMAR-SALIDA-ERR-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA-ERROR                                    *
      ******************************************************************
      *
       2200-ESCRIBIR-SALIDA-ERR.
      *
           WRITE REG-FSALIDA3        FROM REG-LINEA-ERRONEA
      *
           IF FS-FSALIDA3 NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA3'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA-3'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA3
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE REG-LINEA-ERRONEA
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA3
           END-IF
      *
           .
      *
       2200-ESCRIBIR-SALIDA-ERR-EXIT.
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
           CLOSE ESCLAVO
           CLOSE FSALIDA1
           CLOSE FSALIDA2
           CLOSE FSALIDA3
      *
           IF FS-MAESTRO NOT = CT-00
              DISPLAY 'ERROR AL CERRAR MAESTRO'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-MAESTRO
           END-IF
      *
           IF FS-ESCLAVO NOT = CT-00
              DISPLAY 'ERROR AL CERRAR ESCLAVO'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-ESCLAVO
           END-IF
      *
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
           DISPLAY 'REG  MAESTRO:  ' CN-REG-LEIDOS-MAESTRO
           DISPLAY 'REG  ESCLAVO:  ' CN-REG-LEIDOS-ESCLAVO
           DISPLAY 'REG FSALIDA1:  ' CN-REG-ESCRIT-FSALIDA1
           DISPLAY 'REG SIN LINIA: ' CN-REG-ESCRIT-FSALIDA2
           DISPLAY 'REG ERROR:     ' CN-REG-ESCRIT-FSALIDA3
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
           READ MAESTRO INTO REG-PEDIDO-MAESTRO
      *
           EVALUATE FS-MAESTRO
               WHEN CT-00
                    ADD CT-1               TO CN-REG-LEIDOS-MAESTRO
                    MOVE NRO-PEDIDO-M      TO CLAVE1
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
           READ ESCLAVO INTO REG-PEDIDO-ESCLAVO
      *
           EVALUATE FS-ESCLAVO
               WHEN CT-00
                    ADD CT-1              TO CN-REG-LEIDOS-ESCLAVO
                    MOVE NRO-PEDIDO-E     TO CLAVE2
               WHEN CT-10
                    SET SW-SI-FIN-ESCLAVO TO TRUE
                    MOVE CT-MAX           TO CLAVE2
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
