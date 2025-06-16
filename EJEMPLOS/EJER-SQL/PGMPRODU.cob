      ******************************************************************
      ** P G M P R O D U  PGM DEL EJERCICIO DE PRESTAMOS CON FICHERO  **
      **                  FICHERO DE ENTRADA Y UN FICHERO DE SALIDA.  **
      ******************************************************************
      *
      ******************************************************************
      ** IDENTIFICATION DIVISION                                      **
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PGMPRODU.
       AUTHOR. DAVID.
       DATE-WRITTEN. 15/05/2025.
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
       01  REG-FENTRADA               PIC X(201).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                PIC X(033).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA            PIC X(02).
           05  FS-FSALIDA             PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-LEIDOS              PIC 9(03).
           05  CN-ESCRITOS            PIC 9(03).
           05  CN-INSERTADOS          PIC 9(03).
      *
       01  WK-VARIABLES.
           05  WK-SQLCODE             PIC -999.
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA        PIC X(01).
               88  SW-SI-FIN-FENTRADA VALUE 'S'.
               88  SW-NO-FIN-FENTRADA VALUE 'N'.
      *
      *-- INCLUIMOS DCLGEN TABLA PRODUCTOS
           EXEC SQL
                  INCLUDE TBPRODU
           END-EXEC.
      *
      *-- INCLUIMOS COPY DE COMUNICACION CON DB2
           EXEC SQL
                  INCLUDE SQLCA
           END-EXEC.
      *
      *
      *COPY DEL FICHERO DE SALIDA FSALIDA
       01  WK-SALIDA.
           05  WK-ID-PRODUCTO         PIC S9(5)V USAGE COMP-3.
           05  WK-DESCRIPCION         PIC X(30).
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
                      DCLPRODUCTOS
                      WK-SALIDA
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
      * EN POR CADA REGISTRO LEIDO INTENTAREMOS INSERTARLO EN LA TABLA *
      * PRODUCTOS. SI NO ES POSIBLE ESCRIBIRA UNA INCIDENCIA EN EL     *
      * FICHERO DE SALIDA.                                             *
      ******************************************************************
      *
       2000-PROCESO.
      *
           EXEC SQL
               INSERT INTO PRODUCTOS
                VALUES (:TB-ID-PRODUCTO
                       ,:TB-NOMBRE
                       ,:TB-CATEGORIA
                       ,:TB-PRECIO
                       ,:TB-FECHA-ALTA
                       ,:TB-STOCK
                       ,:TB-DESCRIPCION)
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   ADD 1                      TO CN-INSERTADOS
              WHEN -803
                   MOVE TB-ID-PRODUCTO        TO WK-ID-PRODUCTO
                   MOVE 'CLAVE DUPLICADA'     TO WK-DESCRIPCION

                   PERFORM 2200-ESCRIBIR-FSALIDA
                      THRU 2200-ESCRIBIR-FSALIDA-EXIT

              WHEN OTHER
                   MOVE SQLCODE               TO WK-SQLCODE
                   DISPLAY 'ERROR DESCONOCIDO ' WK-SQLCODE
                   MOVE TB-ID-PRODUCTO        TO WK-ID-PRODUCTO
                   MOVE 'ERROR DESCONOCIDO'   TO WK-DESCRIPCION

                   PERFORM 2200-ESCRIBIR-FSALIDA
                      THRU 2200-ESCRIBIR-FSALIDA-EXIT

           END-EVALUATE
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
      * 2200-ESCRIBIR-FSALIDA.                                         *
      * EN ESTE PARRAFO ESCRIBIREMOS EL REGISTRO EN EL FICHERO DE      *
      * SALIDA UTILIZANDO LA COPY INFORMADA DEL PARRAFO ANTERIOR.      *
      ******************************************************************
      *
       2200-ESCRIBIR-FSALIDA.
      *
           WRITE REG-FSALIDA        FROM WK-SALIDA
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
              INITIALIZE WK-SALIDA
              ADD 1                  TO CN-ESCRITOS
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
           DISPLAY '*       ESTADISTICAS DEL PGM PGMPRODU             *'
           DISPLAY '***************************************************'
           DISPLAY '*REG.PRODUCTOS LEIDOS: ' CN-LEIDOS              '  '
                   '                      *'
           DISPLAY '*REG.INCIDENTADOS    : ' CN-ESCRITOS            '  '
                   '                      *'
           DISPLAY '*REG.INSERTADOS    OK: ' CN-INSERTADOS          '  '
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
           READ FENTRADA INTO DCLPRODUCTOS
      *
           EVALUATE FS-FENTRADA
               WHEN '00'
                    ADD 1                  TO CN-LEIDOS
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
