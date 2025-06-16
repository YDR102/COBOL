      ******************************************************************
      ** IDENTIFICATION DIVISION                                      **
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PGMFACAC.
       AUTHOR. DAVID.
       DATE-WRITTEN. 05/05/2025.
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
           SELECT FINCIDE ASSIGN TO FINCIDE
           FILE STATUS FS-FINCIDE.
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
       01  REG-FENTRADA               PIC X(059).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                PIC X(061).
      *
       FD FINCIDE
           RECORDING MODE IS F.
       01  REG-FINCIDE                PIC X(059).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA            PIC X(02).
           05  FS-FSALIDA             PIC X(02).
           05  FS-FINCIDE             PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA  PIC 9(03).
           05  CN-REG-ESCRIT-FINCIDE PIC 9(03).
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA        PIC X(01).
               88  SW-SI-FIN-FENTRADA VALUE 'S'.
               88  SW-NO-FIN-FENTRADA VALUE 'N'.
           05  SW-FIN-TABLA           PIC X(01).
               88  SW-SI-FIN-TABLA    VALUE 'S'.
               88  SW-NO-FIN-TABLA    VALUE 'N'.
      *
       01  CT-CONTANTES.
           05  CT-00                              PIC X(02) VALUE '00'.
           05  CT-10                              PIC X(02) VALUE '10'.
           05  CT-1                               PIC 9(02) VALUE 1.
           05  CT-TABLA                           PIC 9(02) VALUE 5.
           05  CT-0                               PIC 9(02) VALUE 0.
      *
      *COPY DEL FICHERO DE ENTRADA FENTRADA
      *COPY CPYFACA1.
       01  REG-FACTURA-ENTRADA.
           05 ID-CLIENTE-IN                       PIC X(10).
           05 NOMBRE-CLIENTE-IN                   PIC X(30).
           05 FECHA-FACTURA-IN.
                10  ANNO                          PIC X(04).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  MES                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  DIA                           PIC X(02).
           05 IMPORTE-FACTURA-IN                  PIC 9(07)V9(02).
      *
      *COPY DEL FICHERO DE SALIDA FSALIDA
      *COPY CPYFACA2.
       01  REG-FACTURA-SALIDA.
           05 ID-CLIENTE-OUT                      PIC X(10).
           05 NOMBRE-CLIENTE-OUT                  PIC X(30).
           05 FECHA-FACTURA-OUT.
                10  ANNO                          PIC X(04).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  MES                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  DIA                           PIC X(02).
           05 IMPORTE-ACOMULADO-OUT               PIC 9(09)V9(02).
      *
      *COPY DEL FICHERO DE ENTRADA FINCIDE
      *COPY CPYFACA3.
       01  REG-FACTURA-INCIDE.
           05 ID-CLIENTE-ERR                     PIC X(10).
           05 NOMBRE-CLIENTE-ERR                 PIC X(30).
           05 FECHA-FACTURA-ERR.
                10  ANNO                          PIC X(04).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  MES                           PIC X(02).
                10  FILLER                        PIC X(01) VALUE '-'.
                10  DIA                           PIC X(02).
           05 IMPORTE-FACTURA-ERR                 PIC 9(07)V9(02).
      *
      ******************************************************************
      *    T A B L A S         W O R K I N G / I N T E R N A S         *
      ******************************************************************
      *
       01  TB-TABLAS.
           05  TABLA-FACTURACION    OCCURS 5 TIMES
                                    INDEXED BY TB-INDICE.
               10  ID-CLI-TABLA                  PIC X(10).
               10  NOMBRE-CLI-TABLA              PIC X(30).
               10  IMPORTE-TOTAL-TABLA           PIC 9(09)V9(02).
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
      ** 1000-INICIO                                                  **
      ** INICIALIZAR VARIABLES                                        **
      ** APERTURA DE FICHEROS                                         **
      ******************************************************************
      *
       1000-INICIO.
      *
           INITIALIZE FS-FILE-STATUS
                      CN-CONTADORES
                      REG-FACTURA-ENTRADA
                      REG-FACTURA-SALIDA
                      REG-FACTURA-INCIDE
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
      ** 1100-ABRIR-FICHEROS                                          **
      ** ABRIMOS LOS FICHEROS COMPROBANDO SU FILE STATUS.             **
      ******************************************************************
      *
       1100-ABRIR-FICHEROS.
      *
           OPEN INPUT  FENTRADA
                OUTPUT FSALIDA
                OUTPUT FINCIDE
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL ABRIR EL FICHERO FENTRADA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FSALIDA NOT = CT-00
      *
              DISPLAY 'ERROR AL ABRIR EL FICHERO FSALIDA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FINCIDE NOT = CT-00
      *
              DISPLAY 'ERROR AL ABRIR EL FICHERO FINCIDE'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FINCIDE'
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
      ** 2000-PROCESO                                                 **
      ** LEER REGISTROS DEL FICHERO DE ENTRADA CON INFORMACIÓN        **
      ** DE CLIENTE Y FACTURA.                                        **
      ** MANTENER ACUMULADOS EN UNA TABLA INTERNA,                    **
      ** LIMITADA A 200 CLIENTES.                                     **
      ** EVITAR DUPLICADOS: SI UN CLIENTE YA ESTÁ EN LA TABLA,        **
      ** ACUMULAR SU IMPORTE.                                         **
      ** AL FINALIZAR, GRABAR LOS DATOS ACUMULADOS                    **
      ** EN UN FICHERO DE SALIDA CON ID, NOMBRE Y TOTAL FACTURADO.    **
      ** SI SE SUPERA LA CAPACIDAD DE LA TABLA,                       **
      ** NOTIFICAR EN UN FICHERO DE INCIDENCIAS.                      **
      ******************************************************************
      *
       2000-PROCESO.
      *
           SET TB-INDICE TO CT-1
           SEARCH TABLA-FACTURACION
                AT END
                     IF CT-TABLA < 5
                          MOVE ID-CLIENTE-IN
                          TO ID-CLI-TABLA(TB-INDICE)
                          MOVE NOMBRE-CLIENTE-IN
                          TO NOMBRE-CLI-TABLA(TB-INDICE)
                          MOVE IMPORTE-FACTURA-IN
                          TO IMPORTE-TOTAL-TABLA(TB-INDICE)
                     ELSE
                          SET SW-SI-FIN-TABLA TO TRUE
                          PERFORM 2300-ESCRIBIR-FINCIDE
                             THRU 2300-ESCRIBIR-FINCIDE-EXIT
                     END-IF

                WHEN ID-CLIENTE-IN = ID-CLI-TABLA(TB-INDICE)
                     ADD IMPORTE-FACTURA-IN
                     TO IMPORTE-TOTAL-TABLA(TB-INDICE)
           END-SEARCH
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
      ** 2200-ESCRIBIR-FSALIDA                                        **
      ** ESCRIBIREMOS EL REGISTRO DE SALIDA EN EL FICHERO EVALUANDO   **
      ** POSTERIORMENTE SU FILE STATUS.                               **
      ******************************************************************
      *
       2200-ESCRIBIR-FSALIDA.
      *
           MOVE ID-CLIENTE-IN       TO ID-CLIENTE-OUT
           MOVE NOMBRE-CLIENTE-IN   TO NOMBRE-CLIENTE-OUT
           MOVE FECHA-FACTURA-IN    TO FECHA-FACTURA-OUT
      *
           WRITE REG-FSALIDA FROM REG-FACTURA-SALIDA
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR EL FICHERO FSALIDA'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA'
              DISPLAY 'NOMBRE FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE REG-FACTURA-SALIDA
              ADD CT-1                TO CN-REG-ESCRIT-FSALIDA
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 2200-ESCRIBIR-FINCIDE                                        **
      ** ESCRIBIREMOS EL REGISTRO DE INCIDE EN EL FICHERO EVALUANDO   **
      ** POSTERIORMENTE SU FILE STATUS.                               **
      ******************************************************************
      *
       2300-ESCRIBIR-FINCIDE.
      *
           MOVE ID-CLIENTE-IN       TO ID-CLIENTE-ERR
           MOVE NOMBRE-CLIENTE-IN   TO NOMBRE-CLIENTE-ERR
           MOVE FECHA-FACTURA-IN    TO FECHA-FACTURA-ERR
      *
           WRITE REG-FINCIDE FROM REG-FACTURA-INCIDE
      *
           IF FS-FINCIDE NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR EL FICHERO FINCIDE'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FINCIDE'
              DISPLAY 'NOMBRE FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FINCIDE
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE REG-FACTURA-INCIDE
              ADD CT-1                TO CN-REG-ESCRIT-FINCIDE
           END-IF
      *
           .
      *
       2300-ESCRIBIR-FINCIDE-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 3000-FIN                                                     **
      ** MOSTRAMOS LOS DATOS.                                         **
      ** FINALIZAMOS EL PGM.                                          **
      ******************************************************************
      *
       3000-FIN.
      *
           PERFORM 2200-ESCRIBIR-FSALIDA
              THRU 2200-ESCRIBIR-FSALIDA-EXIT
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
      ** 3100-CERRAR-FICHEROS                                         **
      ** CERRAMOS LOS FICHEROS DE CONTROLANDO SU FILE STATUS.         **
      ******************************************************************
      *
       3100-CERRAR-FICHEROS.
      *
           CLOSE FENTRADA
                 FSALIDA
                 FINCIDE
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR EL FICHERO FENTRADA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
           END-IF
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR EL FICHERO FSALIDA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
           END-IF
      *
           .
      *
       3100-CERRAR-FICHEROS-EXIT.
           EXIT.
      *
      *
      ******************************************************************
      ** 3200-MOSTRAR-ESTADISTICAS                                    **
      ** MOSTRAMOS ESTADISTICAS DEL PGM IMPRIMIENTO LOS REG. LEIDOS   **
      ** DEL FICHERO DE ENTRADA Y LOS REG. ESCRITOS DEL FICH. DE SALIDA*
      ** E INCIDENCIAS.                                                *
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
           DISPLAY '*REG.ESCRITOS FINCIDE: ' CN-REG-ESCRIT-FINCIDE  '  '
                   '                      *'
           DISPLAY '***************************************************'
      *
           .
      *
       3200-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 9000-LEER-FENTRADA                                           **
      ** LEEMOS REGISTRO DEL FICHERO FENTRADA CONTROLANDO SU FILE     **
      ** STATUS.                                                      **
      ******************************************************************
      *
       9000-LEER-FENTRADA.
      *
           READ FENTRADA INTO REG-FACTURA-ENTRADA
      *
           EVALUATE FS-FENTRADA
               WHEN CT-00
                    ADD CT-1                   TO CN-REG-LEIDOS-FENTRADA
               WHEN CT-10
                    SET SW-SI-FIN-FENTRADA     TO TRUE
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL FICHERO FENTRADA'
                    DISPLAY 'PARRAFO: 9000-LEER-FENTRADA'
                    DISPLAY 'NOMBRE FICHERO: FENTRADA'
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
