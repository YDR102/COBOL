       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  TRASEGM.
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
           SELECT FENTRADA ASSIGN TO FENTRADA
           FILE STATUS FS-FENTRADA.
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
       FD FENTRADA
           RECORDING MODE IS F.
       01  REG-FENTRADA                           PIC X(2540).
      *    SEGURO
      *
       FD FSALIDA1
           RECORDING MODE IS F.
       01  REG-FSALIDA1                           PIC X(0549).
      *    VIDA
      *
       FD FSALIDA2
           RECORDING MODE IS F.
       01  REG-FSALIDA2                           PIC X(0579).
      *    AUTO
      *
       FD FSALIDA3
           RECORDING MODE IS F.
       01  REG-FSALIDA3                           PIC X(0580).
      *    HOGAR
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA                        PIC X(02).
           05  FS-FSALIDA1                        PIC X(02).
           05  FS-FSALIDA2                        PIC X(02).
           05  FS-FSALIDA3                        PIC X(02).
      *
       01  WK-VARIABLES.
           05 PALABRA-ACU                         PIC X(10).
           05 COBERTURA1-ACU                      PIC X(125).
           05 COBERTURA2-ACU                      PIC X(125).
           05 COBERTURA3-ACU                      PIC X(125).
           05 COBERTURA4-ACU                      PIC X(125).
           05 PRIMA-ACU                           PIC X(21).
           05 CONTENIDO-ACU                       PIC X(21).
           05 CONTINENTE-ACU                      PIC X(21).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA             PIC 9(03).
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
           05  SW-FIN-FENTRADA                    PIC X(01).
               88  SW-SI-FIN-FENTRADA                       VALUE 'S'.
               88  SW-NO-FIN-FENTRADA                       VALUE 'N'.
      *
      *COPY DEL FICHERO DE FENTRADA ENTRADA1
      *
       COPY CPYSEGFI.

      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA1 - VIDA
      *
       COPY MCPVIDFI.

      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA2 - AUTO
      *
       COPY MCPAUTFI.

      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA3 - HOGAR
      *
       COPY MCPHOGFI.
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
                      WK-VARIABLES
                      DATOS-AUT
                      DATOS-HOG
                      DATOS-SEG
                      DATOS-VID
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
           OPEN OUTPUT FSALIDA1
           OPEN OUTPUT FSALIDA2
           OPEN OUTPUT FSALIDA3
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
      ******************************************************************
      *
       2000-PROCESO.
      *
           EVALUATE TIPO-SEG
               WHEN  '01'
                   PERFORM 2100-INFORMAR-SALIDA-1
                      THRU 2100-INFORMAR-SALIDA-1-EXIT

                   PERFORM 2200-ESCRIBIR-FSALIDA-1
                      THRU 2200-ESCRIBIR-FSALIDA-1-EXIT
                   CONTINUE
               WHEN  '02'
                   PERFORM 2100-INFORMAR-SALIDA-3
                      THRU 2100-INFORMAR-SALIDA-3-EXIT

                   PERFORM 2200-ESCRIBIR-FSALIDA-3
                      THRU 2200-ESCRIBIR-FSALIDA-3-EXIT
                   CONTINUE
               WHEN  '03'
                   PERFORM 2100-INFORMAR-SALIDA-2
                      THRU 2100-INFORMAR-SALIDA-2-EXIT

                   PERFORM 2200-ESCRIBIR-FSALIDA-2
                      THRU 2200-ESCRIBIR-FSALIDA-2-EXIT
                   CONTINUE
               WHEN OTHER
                     DISPLAY 'TIPO DE SEGURO NO VÁLIDO: ' TIPO-SEG
                     DISPLAY 'PARRAFO: 2000-PROCESO'
                     PERFORM 3000-FIN
                         THRU 3000-FIN-EXIT
           END-EVALUATE

           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA-1      -- VIDA --                         *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA-1.
      *
           INITIALIZE PRIMA-ACU
                      COBERTURA1-ACU
                      COBERTURA2-ACU
                      COBERTURA3-ACU

           UNSTRING COND-PART-SEG DELIMITED BY ',' OR ': '
           INTO PALABRA-ACU, PRIMA-ACU,
                PALABRA-ACU, EDAD-VID,
                PALABRA-ACU, COBERTURA1-ACU,
                COBERTURA2-ACU, COBERTURA3-ACU
           END-UNSTRING.

           STRING COBERTURA1-ACU, COBERTURA2-ACU, COBERTURA3-ACU
           DELIMITED BY '  '
           INTO COBERTURAS-VID
           END-STRING

           UNSTRING PRIMA-ACU DELIMITED BY ' '
           INTO PRIMA-VID
           END-UNSTRING.

           MOVE NUMERO-POLIZA-SEG         TO POLIZA-VID
           MOVE FECHA-INICIO-SEG          TO FECHA-INICIO-VID
           MOVE FECHA-VENCIMIENTO-SEG     TO FECHA-VENCIMIENTO-VID

           DISPLAY POLIZA-VID
           DISPLAY PRIMA-VID
           DISPLAY EDAD-VID
           DISPLAY COBERTURAS-VID
           DISPLAY FECHA-INICIO-VID
           DISPLAY FECHA-VENCIMIENTO-VID
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
           WRITE REG-FSALIDA1        FROM DATOS-VID
      *
           IF FS-FSALIDA1 NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA1'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA-1'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA1
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE DATOS-VID
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA1
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-1-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA-2          -- AUTO --                     *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA-2.
      *
           INITIALIZE PRIMA-ACU
                      COBERTURA1-ACU
                      COBERTURA2-ACU
                      COBERTURA3-ACU

           DISPLAY COND-PART-SEG

           UNSTRING COND-PART-SEG DELIMITED BY ',' OR ': '
           INTO PALABRA-ACU, PRIMA-ACU,
                PALABRA-ACU, EDAD-AUT,
                PALABRA-ACU, CATEGORIA-AUT,
                PALABRA-ACU, COBERTURA1-ACU,
                COBERTURA2-ACU, COBERTURA3-ACU
           END-UNSTRING.

           DISPLAY COBERTURA1-ACU
           DISPLAY COBERTURA2-ACU
           DISPLAY COBERTURA3-ACU

           STRING COBERTURA1-ACU, COBERTURA2-ACU, COBERTURA3-ACU
           DELIMITED BY '  '
           INTO COBERTURAS-AUT
           END-STRING

           UNSTRING PRIMA-ACU DELIMITED BY ' '
           INTO PRIMA-AUT
           END-UNSTRING.

           MOVE NUMERO-POLIZA-SEG         TO POLIZA-AUT
           MOVE FECHA-INICIO-SEG          TO FECHA-INICIO-AUT
           MOVE FECHA-VENCIMIENTO-SEG     TO FECHA-VENCIMIENTO-AUT

           DISPLAY POLIZA-AUT
           DISPLAY PRIMA-AUT
           DISPLAY EDAD-AUT
           DISPLAY CATEGORIA-AUT
           DISPLAY COBERTURAS-AUT
           DISPLAY FECHA-INICIO-AUT
           DISPLAY FECHA-VENCIMIENTO-AUT
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
           WRITE REG-FSALIDA2        FROM DATOS-AUT
      *
           IF FS-FSALIDA1 NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA-2'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA-2'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA2
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE DATOS-AUT
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA2
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-2-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA-3              -- HOGAR --                *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA-3.
      *
           INITIALIZE PRIMA-ACU
                      CONTENIDO-ACU
                      CONTINENTE-ACU

           UNSTRING COND-PART-SEG DELIMITED BY ',' OR ': '
           INTO PALABRA-ACU, PRIMA-ACU,
                PALABRA-ACU, CONTINENTE-ACU,
                PALABRA-ACU, CONTENIDO-ACU,
                PALABRA-ACU, COBERTURA1-ACU,
                COBERTURA2-ACU, COBERTURA3-ACU, COBERTURA4-ACU
           END-UNSTRING.

           STRING COBERTURA1-ACU, COBERTURA2-ACU,
                  COBERTURA3-ACU, COBERTURA4-ACU
           DELIMITED BY '  '
           INTO COBERTURAS-HOG
           END-STRING

           UNSTRING CONTINENTE-ACU DELIMITED BY ' '
           INTO CONTINENTE-HOG
           END-UNSTRING.

           UNSTRING CONTENIDO-ACU DELIMITED BY ' '
           INTO CONTENIDO-HOG
           END-UNSTRING.

           UNSTRING PRIMA-ACU DELIMITED BY ' '
           INTO PRIMA-HOG
           END-UNSTRING.

           MOVE NUMERO-POLIZA-SEG         TO POLIZA-HOG
           MOVE FECHA-INICIO-SEG          TO FECHA-INICIO-HOG
           MOVE FECHA-VENCIMIENTO-SEG     TO FECHA-VENCIMIENTO-HOG

           DISPLAY POLIZA-HOG
           DISPLAY PRIMA-HOG
           DISPLAY CONTINENTE-HOG
           DISPLAY CONTENIDO-HOG
           DISPLAY COBERTURAS-HOG
           DISPLAY FECHA-INICIO-HOG
           DISPLAY FECHA-VENCIMIENTO-HOG
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
           WRITE REG-FSALIDA3        FROM DATOS-HOG
      *
           IF FS-FSALIDA3 NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA3'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA-3'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA3
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE DATOS-HOG
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
           CLOSE FENTRADA
           CLOSE FSALIDA1
           CLOSE FSALIDA2
           CLOSE FSALIDA3
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FENTRADA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
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
           DISPLAY '**************************'
           DISPLAY '*      ESTADISTICAS      *'
           DISPLAY '**************************'
           DISPLAY '*REG FENTRADA:           *' CN-REG-LEIDOS-FENTRADA
           DISPLAY '*REG FSALIDA1:           *' CN-REG-ESCRIT-FSALIDA1
           DISPLAY '*REG FSALIDA2:           *' CN-REG-ESCRIT-FSALIDA2
           DISPLAY '*REG FSALIDA3:           *' CN-REG-ESCRIT-FSALIDA3
           DISPLAY '**************************'
      *
           .
      *
       3200-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 9000-LEER-FENTRADA                                              *
      ******************************************************************
      *
       9000-LEER-FENTRADA.
      *
           READ FENTRADA INTO DATOS-SEG
      *
           EVALUATE FS-FENTRADA
               WHEN CT-00
                    ADD CT-1                TO CN-REG-LEIDOS-FENTRADA
               WHEN CT-10
                    SET SW-SI-FIN-FENTRADA  TO TRUE
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
