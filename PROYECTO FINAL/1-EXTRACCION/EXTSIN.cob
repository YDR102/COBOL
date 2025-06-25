       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. EXTSIN.
       AUTHOR. DAVID.
       DATE-WRITTEN. 11/06/2025.
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
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                PIC X(108).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FSALIDA             PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-LEIDOS              PIC 9(03).
           05  CN-ESCRITOS            PIC 9(03).
           05  CN-EXTRAIDOS           PIC 9(03).
      *
       01  CT-CONSTANTES.
           05 CT-00                   PIC X(02) VALUE '00'.
           05 CT-10                   PIC X(02) VALUE '10'.
           05 CT-99                   PIC X(02) VALUE '99'.
      *
       01  WK-VARIABLES.
           05  WK-SQLCODE             PIC -999.
      *
       01 SW-SWITCHES.
          05 SW-FIN-CURSOR             PIC X(01).
             88 SI-FIN-CURSOR          VALUE 'S'.
             88 NO-FIN-CURSOR          VALUE 'N'.
      *
       01 ERRORES.
             05 COD-RETORNO            PIC X(02).
             05 COD-SUBRETORNO         PIC S9(09).
             05 PARRAFO                PIC X(30).
             05 TABLA                  PIC X(25).
             05 DESCRIPCION            PIC X(30).
             05 SQLCODE-E              PIC -999.
      *
      *COPY DEL FICHERO DE SALIDA FSALIDA
       COPY CPYSINFI.
      *
      *-- INCLUIMOS DCLGEN TABLA PRODUCTOS
           EXEC SQL
                  INCLUDE TBSINFIN
           END-EXEC.
      *
      *-- INCLUIMOS COPY DE COMUNICACION CON DB2
           EXEC SQL
                  INCLUDE SQLCA
           END-EXEC.
      *
      *-------------- DEFINIMOS LOS CURSORES ---------
           EXEC SQL
               DECLARE CUR-SINIESTROS-PEPITO-SEG CURSOR FOR
                  SELECT ID_SINIETRO,
                         FECHA_SINIESTRO,
                         CAUSAS,
                         ACPTADO,
                         INDEMNIZACION,
                         NUMERO_POLIZA,
                         DNI_PERITO
                    FROM SINIESTROS_PEPITO_SEG
                    ORDER BY ID_SINIETRO
           END-EXEC.
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
             UNTIL SI-FIN-CURSOR
      *
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT
      *
           .
      *
      ******************************************************************
      * 1000-INICIO.                                                   *
      * INICIALIZAR VARIABLES                                          *
      * APERTURA DE FICHEROS                                           *
      ******************************************************************
      *
       1000-INICIO.
      *
           INITIALIZE FS-FILE-STATUS
                      CN-CONTADORES
                      DCLSINIESTROS-PEPITO-SEG
                      DATOS-SIN
      *
           SET NO-FIN-CURSOR       TO TRUE
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT

           PERFORM 1200-ABRIR-CURSOR
              THRU 1200-ABRIR-CURSOR-EXIT

           PERFORM 9000-LEER-CURSOR
              THRU 9000-LEER-CURSOR-EXIT
      *
           .
      *
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 1100-ABRIR-FICHEROS.                                           *
      *  ABRIMOS EL FICHERO Y SALIDA COMPROBANDO SU FILE STATUS.       *
      ******************************************************************
      *
       1100-ABRIR-FICHEROS.
      *
           OPEN OUTPUT FSALIDA
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
      *     1200-ABRIR-CURSOR                                          *
      * ABRE EL CURSOR PARA LEER REGISTROS DE LA TABLA EN LA BASE      *
      * DE DATOS. SI HAY ERROR AL ABRIR, GUARDA INFORMACION DEL        *
      * ERROR Y FINALIZA EL PROGRAMA.                                  *
      ******************************************************************
       1200-ABRIR-CURSOR.
      *
           SET NO-FIN-CURSOR TO TRUE
      *
           DISPLAY '-------------OPEN CURSOR'
           EXEC SQL
               OPEN CUR-SINIESTROS-PEPITO-SEG
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CT-99                  TO COD-RETORNO
                   MOVE CT-99                  TO COD-SUBRETORNO
                   MOVE 'OPEN'                 TO PARRAFO
                   MOVE 'COMPANIAS-SEGUROS'    TO TABLA
                   MOVE '1200-ABRIR-CURSOR'    TO DESCRIPCION
                   MOVE SQLCODE                TO SQLCODE-E

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       1200-ABRIR-CURSOR-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2000-PROCESO.                                                  *
      * LLAMA AL PARRAFO 2200-ESCRIBIR-FSALIDA PARA ESCRIBIR.          *
      * EL REGISTRO ACTUAL EN FSALIDA DESPUES LLAMA A                  *
      * 9000-LEER-CURSOR PARA LEER EL SIGUIENTE REGISTRO               *
      * DE LA BASE DE DATOS                                            *
      ******************************************************************
      *
       2000-PROCESO.
      *
           PERFORM 2200-ESCRIBIR-FSALIDA
              THRU 2200-ESCRIBIR-FSALIDA-EXIT

           PERFORM 9000-LEER-CURSOR
              THRU 9000-LEER-CURSOR-EXIT
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA.                                         *
      * MUEVE LOS DATOS DEL REGISTRO ACTUAL DE LA TABLA A LA           *
      * ESTRUCTURA DE SALIDA. ESCRIBE EL REGISTRO EN FSALIDA,          *
      * CONTROLA ERRORES, INICIALIZA DATOS Y ACTUALIZA EL              *
      * CONTADOR DE REGISTROS                                          *
      ******************************************************************
      *
       2200-ESCRIBIR-FSALIDA.
      *
           MOVE TB-ID-SINIETRO     TO ID-SINIETRO-SIN
           MOVE TB-FECHA-SINIESTRO TO FECHA-SINIESTRO-SIN
           MOVE TB-CAUSAS          TO CAUSAS-SIN
           MOVE TB-ACPTADO         TO ACPTADO-SIN
           MOVE TB-INDEMNIZACION   TO INDEMNIZACION-SIN
           MOVE TB-NUMERO-POLIZA   TO NUMERO-POLIZA-SIN
           MOVE TB-DNI-PERITO      TO DNI-PERITO-SIN

           WRITE REG-FSALIDA        FROM DATOS-SIN
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
              INITIALIZE DATOS-SIN
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
      * CIERRA LOS FICHEROS Y CURSORES, MUESTRA ESTADISTICAS Y FINALIZA*
      ******************************************************************
      *
       3000-FIN.
      *
           PERFORM 3100-CERRAR-FICHEROS
              THRU 3100-CERRAR-FICHEROS-EXIT
      *
           PERFORM 3200-CERRAR-CURSOR
              THRU 3200-CERRAR-CURSOR-EXIT
      *
           PERFORM 3300-MOSTRAR-ESTADISTICAS
              THRU 3300-MOSTRAR-ESTADISTICAS-EXIT
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
      ******************************************************************
      *
       3100-CERRAR-FICHEROS.
      *
           CLOSE FSALIDA
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
      *     3200-CERRAR-CURSOR                                         *
      * CIERRA EL CURSOR CUR-COMPANIAS-SEGUROS USADO PARA              *
      * LEER LA TABLA EN LA BASE DE DATOS. SI HAY ERROR AL CERRAR,     *
      * GUARDA INFORMACION DEL ERROR Y FINALIZA EL PROGRAMA.           *
      ******************************************************************
       3200-CERRAR-CURSOR.
      *
           DISPLAY '---------CERRAR CURSOR'
      *
           EXEC SQL
              CLOSE CUR-SINIESTROS-PEPITO-SEG
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CT-99                  TO COD-RETORNO
                   MOVE CT-99                  TO COD-SUBRETORNO
                   MOVE 'CLOSE'                TO PARRAFO
                   MOVE 'COMPANIAS-SEGUROS'    TO TABLA
                   MOVE '3200-CERRAR-CURSOR'   TO DESCRIPCION
                   MOVE SQLCODE                TO SQLCODE-E

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       3200-CERRAR-CURSOR-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3300-MOSTRAR-ESTADISTICAS.                                     *
      ******************************************************************
      *
       3300-MOSTRAR-ESTADISTICAS.
      *
           DISPLAY '*****************************'
           DISPLAY '*       ESTADISTICAS        *'
           DISPLAY '*****************************'
           DISPLAY '*REG.EXTRAIDOS:             *' CN-EXTRAIDOS
           DISPLAY '*REG.ESCRITOS:              *' CN-ESCRITOS
           DISPLAY '*****************************'
           DISPLAY '                             '
           DISPLAY '*****************************'
           DISPLAY '* SE HA PRODUCIDO UN ERROR  *'
           DISPLAY '*****************************'
           DISPLAY '* DESCRIPCION DEL ERROR:    *' COD-RETORNO
           DISPLAY '* DESCRIPCION DEL ERROR:    *' COD-SUBRETORNO
           DISPLAY '* DESCRIPCION DEL ERROR:    *' PARRAFO
           DISPLAY '* DESCRIPCION DEL ERROR:    *' DESCRIPCION
           DISPLAY '* DESCRIPCION DEL ERROR:    *' TABLA
           DISPLAY '* DESCRIPCION DEL ERROR:    *' SQLCODE-E
           DISPLAY '*****************************'
      *
           .
      *
       3300-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.
      *
      ******************************************************************
      *     9000-LEER-CURSOR                                           *
      * LEE EL SIGUIENTE REGISTRO DE LA TABLA USANDO EL CURSOR.        *
      * SI LLEGA AL FINAL, ACTIVA EL FIN DE CURSOR. SI HAY ERROR,      *
      * GUARDA INFORMACION Y FINALIZA EL PROGRAMA.                     *
      ******************************************************************
       9000-LEER-CURSOR.
      *
           DISPLAY '-----------FETCH CURSOR'
      *
           EXEC SQL
              FETCH CUR-SINIESTROS-PEPITO-SEG
               INTO :TB-ID-SINIETRO,
                    :TB-FECHA-SINIESTRO,
                    :TB-CAUSAS,
                    :TB-ACPTADO,
                    :TB-INDEMNIZACION,
                    :TB-NUMERO-POLIZA,
                    :TB-DNI-PERITO
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN 100
                   SET SI-FIN-CURSOR           TO TRUE
              WHEN OTHER
                   MOVE CT-99                  TO COD-RETORNO
                   MOVE CT-99                  TO COD-SUBRETORNO
                   MOVE 'FETCH'                TO PARRAFO
                   MOVE 'COMPANIAS-SEGUROS'    TO TABLA
                   MOVE '9000-LEER-CURSOR'     TO DESCRIPCION
                   MOVE SQLCODE                TO SQLCODE-E

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       9000-LEER-CURSOR-EXIT.
           EXIT.