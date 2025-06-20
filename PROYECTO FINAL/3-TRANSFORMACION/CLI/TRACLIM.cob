       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  TRASEGM.
       AUTHOR.      DAVID.
       DATE-WRITTEN 18/06/2025.
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
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD FENTRADA
           RECORDING MODE IS F.
       01  REG-FENTRADA                           PIC X(0713).
      *    SEGURO
      *
       FD FSALIDA1
           RECORDING MODE IS F.
       01  REG-FSALIDA1                           PIC X(0694).
      *
       FD FSALIDA2
           RECORDING MODE IS F.
       01  REG-FSALIDA2                           PIC X(0579).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA                        PIC X(02).
           05  FS-FSALIDA1                        PIC X(02).
           05  FS-FSALIDA2                        PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA             PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA1             PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA2             PIC 9(03).
      *
       01  CT-CONTANTES.
           05  CT-00                              PIC X(02) VALUE '00'.
           05  CT-10                              PIC X(02) VALUE '10'.
           05  CT-1                               PIC 9(02) VALUE 1.
           05  CT-SPACE                           PIC X(01) VALUE SPACE.
           05  CT-RUT                         PIC X(07) VALUE 'RUTAGEN'.
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA                    PIC X(01).
               88  SW-SI-FIN-FENTRADA                       VALUE 'S'.
               88  SW-NO-FIN-FENTRADA                       VALUE 'N'.
      *
      *COPY DEL FICHERO DE FENTRADA ENTRADA
      *
       COPY CPYCLISA.

      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA1
      *
       COPY MCPCLIFI.

      *
      *COPY DEL FICHERO DE ENTRADA FSALIDA2
      *
       COPY MCPAGRFI.
      *
      *COPY DEL FICHERO DE ENTRADA RUTINA
      *
       COPY CPRUTCO.
      *
      ******************************************************************
      *  PROCEDURE DIVISION                                            *
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
                      DATOS-CLI-M
                      CPYCLISA
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
           PERFORM 2100-INFORMAR-SALIDA1
              THRU 2100-INFORMAR-SALIDA1-EXIT

           PERFORM 2200-ESCRIBIR-FSALIDA1
              THRU 2200-ESCRIBIR-FSALIDA1-EXIT

           PERFORM 2500-LLAMAR-RUTINA
              THRU 2500-LLAMAR-RUTINA-EXIT

           PERFORM 2300-INFORMAR-SALIDA2
              THRU 2300-INFORMAR-SALIDA2-EXIT

           PERFORM 2400-ESCRIBIR-FSALIDA2
              THRU 2400-ESCRIBIR-FSALIDA2-EXIT

           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA1                                          *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA1.
      *
           STRING NOMBRE-CL-S, APELLIDO-1-S, APELLIDO-2-S
           DELIMITED BY SIZE
           INTO NOMBRE-CLI-M
           END-STRING

           STRING CLASE-VIA-S, CT-SPACE,
                  NOMBRE-VIA-S, CT-SPACE, NUMERO-VIA-S, CT-SPACE,
                  COD-POSTAL-S, CT-SPACE, CIUDAD-S
           DELIMITED BY '  '
           INTO DIRECCION-CLI-M
           END-STRING

           MOVE DNI-CL-S            TO DNI-CLI-M
           MOVE TELEFONO-S          TO TELEFONO-CLI-M
           MOVE OBSERVACIONES-S     TO OBSERVACIONES-CLI-M
      *
           .
      *
       2100-INFORMAR-SALIDA1-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA1                                         *
      ******************************************************************
      *
       2200-ESCRIBIR-FSALIDA1.
      *
           WRITE REG-FSALIDA1         FROM DATOS-CLI-M
      *
           IF FS-FSALIDA1 NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA1'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA1'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA1
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE DATOS-CLI-M
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA1
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA1-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2300-INFORMAR-SALIDA2                                          *
      ******************************************************************
      *
       2300-INFORMAR-SALIDA2.
      *
           MOVE NUM-RUT             TO NUM-AGENTE-AGR
           MOVE DNI-RUT             TO DNI-AGR
           MOVE NOMBRE-RUT          TO NOMBRE-AGR
           MOVE APE-1-RUT           TO APE-1-AGR
           MOVE APE-2-RUT           TO APE-2-AGR
           MOVE TLF-RUT             TO TLF-AGR
           MOVE DNI-CL-S            TO DNI-CLI-AGR
      *
           .
      *
       2300-INFORMAR-SALIDA2-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2400-ESCRIBIR-FSALIDA2                                         *
      ******************************************************************
      *
       2400-ESCRIBIR-FSALIDA2.
      *
           WRITE REG-FSALIDA2         FROM DATOS-AGR
      *
           IF FS-FSALIDA2 NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FSALIDA2'
              DISPLAY 'PARRAFO: 2400-ESCRIBIR-FSALIDA2'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA2
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE DATOS-CLI-M
              ADD CT-1                 TO CN-REG-ESCRIT-FSALIDA2
           END-IF
      *
           .
      *
       2400-ESCRIBIR-FSALIDA2-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2500-LLAMAR-RUTINA                                         *
      * ****************************************************************
       2500-LLAMAR-RUTINA.
      *
           DISPLAY 'CALL A LA RUTINA'

           MOVE DNI-CL-S(4:1)       TO NUMERO-ALEA
           DISPLAY 'DNI COMPLETO:  ' DNI-CL-S
           DISPLAY 'NUMERO ALEATORIO: ' NUMERO-ALEA
      *
           CALL CT-RUT USING CPRUTCO
      *
           EVALUATE COD-RETORNO
              WHEN CT-00
                   CONTINUE
              WHEN OTHER
                   DISPLAY COD-RETORNO
                   DISPLAY COD-SUBRETORNO
                   DISPLAY PARRAFO
                   DISPLAY DESCRIPCION
                   DISPLAY TABLA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2500-LLAMAR-RUTINA-EXIT.
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
           DISPLAY '**************************'
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
           DISPLAY '                                  '
           DISPLAY '**********************************'
           DISPLAY '                                  '

           READ FENTRADA INTO CPYCLISA
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
