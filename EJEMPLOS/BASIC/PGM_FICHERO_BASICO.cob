      ******************************************************************
      ** P G M F I C H .- PGM DE EJEMPLO PARA EL TRATAMIENTO DE UN    **
      **                  FICHERO DE ENTRADA Y UN FICHERO DE SALIDA.  **
      ******************************************************************
      *
      ******************************************************************
      ** IDENTIFICATION DIVISION                                      **
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PGMFICH.
       AUTHOR. CRISTIAN.
       INSTALLATION. EOI.
       DATE-WRITTEN. 16/04/2025.
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
       01  REG-FENTRADA               PIC X(143).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                PIC X(093).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA            PIC X(02).
           05  FS-FSALIDA             PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA  PIC 9(03).
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA        PIC X(01).
               88  SW-SI-FIN-FENTRADA VALUE 'S'.
               88  SW-NO-FIN-FENTRADA VALUE 'N'.
      *
      *COPY DEL FICHERO DE ENTRADA FENTRADA
       01  COPY-FENTRADA.
           05  NOMBRE-E               PIC X(40).
           05  APELLIDOS-E            PIC X(50).
           05  DIRECCION-E            PIC X(53).
      *
      *COPY DEL FICHERO DE SALIDA FSALIDA
       01  COPY-FSALIDA.
           05  ID-PERSONA             PIC 9(03).
           05  NOMBRE-S               PIC X(40).
           05  APELLIDOS-S            PIC X(50).
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
      *
           IF FS-FENTRADA NOT = '00'
              DISPLAY 'ERROR AL ABRIR EL FICHERO FENTRADA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FSALIDA  NOT = '00'
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
           MOVE CN-REG-LEIDOS-FENTRADA       TO ID-PERSONA
           MOVE NOMBRE-E                     TO NOMBRE-S
           MOVE APELLIDOS-E                  TO APELLIDOS-S
      *
           .
      *
       2100-INFORMAR-SALIDA-EXIT.
           EXIT.
      *
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
           IF FS-FSALIDA NOT = '00'
              DISPLAY 'ERROR AL ESCRIBIR EN FSALIDA'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA'
              DISPLAY 'FICHERO: FSALIDA'
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
       2200-ESCRIBIR-FSALIDA-EXIT.
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
           CLOSE FSALIDA
      *
           IF FS-FENTRADA NOT = '00'
              DISPLAY 'ERROR AL CERRAR FICHERO FENTRADA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
           END-IF
      *
           IF FS-FSALIDA  NOT = '00'
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
           DISPLAY '*REG.LEIDOS FENTRADA : ' CN-REG-LEIDOS-FENTRADA '  '
                   '                      *'
           DISPLAY '*REG.ESCRITOS FSALIDA: ' CN-REG-ESCRIT-FSALIDA  '  '
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
               WHEN '00'
                    ADD 1                  TO CN-REG-LEIDOS-FENTRADA
               WHEN '10'
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
