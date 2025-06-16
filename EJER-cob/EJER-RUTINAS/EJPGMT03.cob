      ******************************************************************
      ** IDENTIFICATION DIVISION                                      **
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. EJPGM02.
       AUTHOR. DAVID.
       DATE-WRITTEN. 06/05/2025.
      *
      ******************************************************************
      ** ENVIRONMENT DIVISION                                         **
      ******************************************************************
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
           SELECT FSALIDA ASSIGN TO FSALIDA
           FILE STATUS FS-FSALIDA.
      *
           SELECT FSALIDA1 ASSIGN TO FSALIDA1
           FILE STATUS FS-FSALIDA1.
      *
      ******************************************************************
      ** DATA DIVISION                                                **
      ******************************************************************
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD FENTRADA
           RECORDING MODE IS F.
       01  REG-FENTRADA               PIC X(010).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                PIC X(010).
      *
       FD FSALIDA1
           RECORDING MODE IS F.
       01  REG-FSALIDA1               PIC X(032).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA            PIC X(02).
           05  FS-FSALIDA             PIC X(02).
           05  FS-FSALIDA1            PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA  PIC 9(03).
           05  CN-REG-ESCRIT-INCIDE   PIC 9(03).
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA        PIC X(01).
               88  SW-SI-FIN-FENTRADA VALUE 'S'.
               88  SW-NO-FIN-FENTRADA VALUE 'N'.
      *
       01 CA-CONSTANTES-ALF.
          05 CT-RUT                    PIC X(07) VALUE 'EJRUT03'.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-1                      PIC 9(01) VALUE 1.
          05 CT-10                     PIC X(02) VALUE '10'.
          05 CT-66                     PIC X(02) VALUE '66'.
          05 CT-77                     PIC X(02) VALUE '77'.
          05 CT-88                     PIC X(02) VALUE '88'.
      *
      * -- COPY DE COMUNICACION CON LA RUTINA
       01 CPY-RUT.
          05 RUT-ENTRADA.
             10 FECHA-R                PIC 9(10).
          05 RUT-SALIDA.
             10 RETORNO                PIC X(02).
             10 RESPUESTA              PIC X(30).
      *
      *COPY DEL FICHERO DE ENTRADA FENTRADA
       01  COPY-FENTRADA.
           05  FECHA-E                PIC X(10).
      *
      *COPY DEL FICHERO DE ENTRADA FENTRADA
       01  COPY-FSALIDA.
           05  FECHA-S                PIC X(10).
      *
      *COPY DEL FICHERO DE SALIDA FSALIDA1
       01  COPY-FSALIDA1.
           05  RETORNO-S              PIC X(02).
           05  DESCRIPCION-S          PIC X(30).
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
      * 1000-INICIO.                                                   *
      * EN ESTE PARRAFO INICIALIZAREMOS EN PRIMER LUGAR LOS CAMPOS DE  *
      * TRABAJO, SEGUIDAMENTE ABRIREMOS LOS FICHEROS INVOLUCRADOS EN   *
      * EL PGM Y POR ULTIMO REALIZAREMOS LA LECTURA DEL PRIMER REGISTRO*
      * DEL FICHERO DE ENTRADA.                                        *
      ******************************************************************
      *
       1000-INICIO.
      *
           INITIALIZE FS-FILE-STATUS
                      CN-CONTADORES
                      COPY-FENTRADA
                      COPY-FSALIDA
                      COPY-FSALIDA1
      *
           SET SW-NO-FIN-FENTRADA        TO TRUE
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT
      *
           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
      *
           .
      *
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 1100-ABRIR-FICHEROS.                                           *
      * EN ESTE PARRAFO REALIZAMOS LA APERTURA DE LOS FICHEROS DE      *
      * ENTRADA Y SALIDA DEL PGM CONTROLANDO SU FILE STATUS.           *
      ******************************************************************
      *
       1100-ABRIR-FICHEROS.
      *
           OPEN INPUT FENTRADA
           OPEN OUTPUT FSALIDA
           OPEN OUTPUT FSALIDA1
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL ABRIR EL FICHERO FENTRADA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FSALIDA  NOT = CT-00
              DISPLAY 'ERROR AL ABRIR EL FICHERO FSALIDA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
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
      * 2000-PROCESO.                                                  *
      * EN ESTE PARRAFO REALIZAMOS EL PROCESO DEL PGM QUE CONSITIRA    *
      * EN POR CADA REGISTRO LEIDO ESCRIBIREMOS DICHO REGISTRO         *
      * FORMATEADO EN EL FICHERO DE SALIDA. A CONTINUACION SALDREMOS   *
      * DEL PROCESO CON UNA NUEVA LECTURA DEL FICHERO DE ENTRADA.      *
      ******************************************************************
      *
       2000-PROCESO.
      * -- PARA LLAMAR A RUTINA LOS PASOS SON:
      * 1- INICIALIZAR COPY DE COMUNICACION
           INITIALIZE CPY-RUT
      * 2- INFORMAR LA ENTRADA DE LA COPY DE COMUNICACION
           MOVE FECHA-E             TO FECHA-R
      * 3- INVOCAR A LA RUTINA
           CALL CT-RUT USING CPY-RUT
      * 4- EVALUAR EL RETORNO
           EVALUATE RETORNO
               WHEN CT-00
                    DISPLAY RESPUESTA
      *
                    PERFORM 2100-INFORMAR-SALIDA
                       THRU 2100-INFORMAR-SALIDA-EXIT
      *
                    PERFORM 2200-ESCRIBIR-FSALIDA
                       THRU 2200-ESCRIBIR-FSALIDA-EXIT
      *
                    PERFORM 9000-LEER-FENTRADA
                       THRU 9000-LEER-FENTRADA-EXIT
      *
              WHEN CT-10
              WHEN CT-66
              WHEN CT-77
              WHEN CT-88
                    DISPLAY 'ERROR: ' RETORNO
                    DISPLAY 'DESCRIPCION DEL ERROR: ' RESPUESTA
      *
                    PERFORM 2400-INFORMAR-SALIDA1
                       THRU 2400-INFORMAR-SALIDA1-EXIT
      *
                    PERFORM 2300-ESCRIBIR-FSALIDA1
                       THRU 2300-ESCRIBIR-FSALIDA1-EXIT
      *
                    PERFORM 9000-LEER-FENTRADA
                       THRU 9000-LEER-FENTRADA-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR DESCONOCIDO'
           END-EVALUATE
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA.                                          *
      * SE RELLENA LA INFORMACION DE LA COPY DE SALIDA PARA PODER      *
      * ESCRIBIR DICHA COPY A CONTINUACION EN EL FICHERO DE SALIDA.    *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA.
      *
           MOVE FECHA-E       TO FECHA-S

      *
           .
      *
       2100-INFORMAR-SALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA.                                         *
      * EN ESTE PARRAFO ESCRIBIREMOS EL REGISTRO EN EL FICHERO DE      *
      * SALIDA UTILIZANDO LA COPY INFORMADA DEL PARRAFO ANTERIOR.      *
      ******************************************************************
      *
       2200-ESCRIBIR-FSALIDA.
      *
           WRITE REG-FSALIDA        FROM COPY-FSALIDA
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR EN FSALIDA'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA'
              DISPLAY 'FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE COPY-FSALIDA
              ADD CT-1                  TO CN-REG-ESCRIT-FSALIDA
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2300-ESCRIBIR-FSALIDA1.                                        *
      * EN ESTE PARRAFO ESCRIBIREMOS EL REGISTRO EN EL FICHERO DE      *
      * SALIDA UTILIZANDO LA COPY INFORMADA DEL PARRAFO ANTERIOR.      *
      ******************************************************************
      *
       2300-ESCRIBIR-FSALIDA1.
      *
           WRITE REG-FSALIDA1        FROM COPY-FSALIDA1
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR EN FSALIDA'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA'
              DISPLAY 'FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE COPY-FSALIDA1
              ADD CT-1                  TO CN-REG-ESCRIT-INCIDE
           END-IF
      *
           .
      *
       2300-ESCRIBIR-FSALIDA1-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2400-INFORMAR-SALIDA1.                                         *
      * SE RELLENA LA INFORMACION DE LA COPY DE SALIDA PARA PODER      *
      * ESCRIBIR DICHA COPY A CONTINUACION EN EL FICHERO DE SALIDA.    *
      ******************************************************************
      *
       2400-INFORMAR-SALIDA1.
      *
           MOVE RETORNO       TO RETORNO-S
           MOVE RESPUESTA     TO DESCRIPCION-S

      *
           .
      *
       2400-INFORMAR-SALIDA1-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3000-FIN.                                                      *
      * EN ESTE PARRAFO REALIZAMOS EL CIERRE DE LOS FICHEROS USADOS    *
      * EN EL PGM, A CONTINUACION MOSTRAREMOS ESTADISTICAS DEL PGM     *
      * Y POR ULTIMO EJECUTAREMOS EL STOP RUN.                         *
      ******************************************************************
      *
       3000-FIN.
      *
           PERFORM 3100-CERRAR-FICHEROS
              THRU 3100-CERRAR-FICHEROS-EXIT
      *
           PERFORM 3200-MOSTRAR-ESTADISTICAS
              THRU 3200-MOSTRAR-ESTADISTICAS-EXIT
      *
           STOP RUN
      *
           .
      *
       3000-FIN-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3100-CERRAR-FICHEROS.                                          *
      * EN ESTE PARRAFO REALIZAMOS EL CIERRE DE LOS FICHEROS USADOS    *
      * EN EL PGM CONTROLANDO SU FILE STATUS CORRESPONDIENTE.          *
      ******************************************************************
      *
       3100-CERRAR-FICHEROS.
      *
           CLOSE FENTRADA
                 FSALIDA
                 FSALIDA1
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FICHERO FENTRADA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
           END-IF
      *
           IF FS-FSALIDA  NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FICHERO FSALIDA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
           END-IF
      *
           .
      *
       3100-CERRAR-FICHEROS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3200-MOSTRAR-ESTADISTICAS.                                     *
      * EN ESTE PARRAFO MOSTRAMOS COMO ESTADISTICAS EL CONTADOR  DE    *
      * REG. LEIDOS DEL FICHERO DE ENTRADA Y EL CONTADOR DE REG.       *
      * ESCRITOS DEL FICHERO DE SALIDA.                                *
      ******************************************************************
      *
       3200-MOSTRAR-ESTADISTICAS.
      *
           DISPLAY '***************************************************'
           DISPLAY '*       ESTADISTICAS DEL PGM PGMFICH              *'
           DISPLAY '***************************************************'
           DISPLAY '*REG.LEIDOS  FENTRADA: ' CN-REG-LEIDOS-FENTRADA '  '
                   '                      *'
           DISPLAY '*REG.ESCRITOS FSALIDA: ' CN-REG-ESCRIT-FSALIDA  '  '
                   '                      *'
           DISPLAY '*REG.ESCRITOS  INCIDE: ' CN-REG-ESCRIT-INCIDE   '  '
                   '                      *'
           DISPLAY '***************************************************'
      *
           .
      *
       3200-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 9000-LEER-FENTRADA.                                            *
      * EN ESTE PARRAFO REALIZAMOS LA LECTURA DE CADA UNO DE LOS       *
      * REGISTROS DEL FICHERO DE ENTRADA CONTROLANDO SU FILE STATUS.   *
      ******************************************************************
      *
       9000-LEER-FENTRADA.
      *
           READ FENTRADA INTO COPY-FENTRADA
      *
           EVALUATE FS-FENTRADA
               WHEN CT-00
                    ADD CT-1               TO CN-REG-LEIDOS-FENTRADA
               WHEN CT-10
                    SET SW-SI-FIN-FENTRADA TO TRUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL FICHERO FENTRADA'
                    DISPLAY 'PARRAFO: 9000-LEER-FENTRADA'
                    DISPLAY 'FICHERO: FENTRADA'
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
