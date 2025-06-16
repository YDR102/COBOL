      ******************************************************************
      ** IDENTIFICATION DIVISION                                      **
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PGMTBAFH.
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
           SELECT FTABLA ASSIGN TO FTABLA
           FILE STATUS FS-FTABLA.
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
       01  REG-FENTRADA               PIC X(101).
      *
       FD FTABLA
           RECORDING MODE IS F.
       01  REG-FTABLA                 PIC X(036).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                PIC X(101).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA            PIC X(02).
           05  FS-FSALIDA             PIC X(02).
           05  FS-FTABLA            PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA  PIC 9(03).
           05  IN-IND                 PIC 9(03).
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
           05  CT-0                               PIC 9(02) VALUE 0.
           05  CT-MAX                          PIC 9(08) VALUE 99999999.
      *
      *COPY DEL FICHERO DE ENTRADA FENTRADA
      *COPY CPYDIVI1.
       01 CPY-EXTRAN.
           05 TITILAR-E                           PIC X(40).
           05 CUENTA-ORIGEN-E                     PIC X(25).
           05 CUENTA-DESTINO-E                    PIC X(25).
           05 IMPORTE-E                           PIC 9(05)V9(03).
           05 DIVISA-E                            PIC X(03).
      *
      *COPY DEL FICHERO DE SALIDA FSALIDA
      *COPY CPYDIVI2.
       01 CPY-LOCAL.
           05 TITILAR-S                           PIC X(40).
           05 CUENTA-ORIGEN-S                     PIC X(25).
           05 CUENTA-DESTINO-S                    PIC X(25).
           05 IMPORTE-S                           PIC 9(05)V9(03).
           05 DIVISA-S                            PIC X(03).
      *
      *COPY DEL FICHERO DE ENTRADA FTABLA
      *COPY CPYDIVI3.
       01 CPY-TABLA.
           05 DIVISA-S1                           PIC X(03).
           05 NOM-DIVISA-S1                       PIC X(25).
           05 CAMBIO-S1                           PIC 9(03)V9(05).
      *
      ******************************************************************
      *    T A B L A S         W O R K I N G / I N T E R N A S         *
      ******************************************************************
      *
       01  TB-TABLAS.
           05  TB-TABLA-DIVISA      OCCURS 15 TIMES
                                    INDEXED BY TB-INDICE.
               10  TB-DIVISA        PIC X(03).
               10  TB-NOM-DIVISA    PIC X(25).
               10  TB-CAMBIO        PIC 9(03)V9(05).
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
                      CPY-EXTRAN
                      CPY-LOCAL
      *
           SET SW-NO-FIN-FENTRADA        TO TRUE
           SET SW-NO-FIN-TABLA           TO TRUE
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT
      *
           PERFORM 1200-INF-TABLA-WORKING
              THRU 1200-INF-TABLA-WORKING-EXIT
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
      ** ABRIMOS LOS FICHEROS DE ENTRADA Y SALIDA COMPROBANDO SU      **
      ** FILE STATUS.                                                 **
      ******************************************************************
      *
       1100-ABRIR-FICHEROS.
      *
           OPEN INPUT  FENTRADA
                OUTPUT FSALIDA
                INPUT  FTABLA
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
           .
      *
       1100-ABRIR-FICHEROS-EXIT.
           EXIT.
      *
      *
      ******************************************************************
      ** 1200-INF-TABLA-WORKING                                       **
      ** RELLENAMOS LA TABLA WORKING CON LA INFORMACION DE LOS        **
      ** PRODUCTOS.                                                   **
      ******************************************************************
      *
       1200-INF-TABLA-WORKING.
      *
           PERFORM VARYING IN-IND FROM 1 BY 1
           UNTIL SW-SI-FIN-TABLA
      *
                READ FTABLA INTO CPY-TABLA

                EVALUATE FS-FTABLA
                    WHEN CT-00
                         MOVE DIVISA-S1         TO TB-DIVISA(IN-IND)
                         MOVE NOM-DIVISA-S1     TO TB-NOM-DIVISA(IN-IND)
                         MOVE CAMBIO-S1         TO TB-CAMBIO(IN-IND)
                    WHEN CT-10
                         SET SW-SI-FIN-TABLA TO TRUE
                    WHEN OTHER
                         PERFORM 3000-FIN
                            THRU 3000-FIN-EXIT
                END-EVALUATE
           END-PERFORM
      *
           .
      *
       1200-INF-TABLA-WORKING-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 2000-PROCESO                                                 **
      ** VALIDAR EL VALOR DEL INDICADOR DE PENALIZACION PARA SABER    **
      ** SI TENGO QUE CALCULAR LA COMISION Y CALCULAR EL IMPORTE TOTAL**
      ******************************************************************
      *
       2000-PROCESO.
      *
           IF DIVISA-E NOT = 'EUR'
                PERFORM 2100-BUS-DIV-EN-TW
                   THRU 2100-BUS-DIV-EN-TW-EXIT
      *
                PERFORM 2200-ESCRIBIR-FSALIDA
                   THRU 2200-ESCRIBIR-FSALIDA-EXIT
      *
           ELSE
                PERFORM 2200-ESCRIBIR-FSALIDA
                   THRU 2200-ESCRIBIR-FSALIDA-EXIT
           END-IF
      *
           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
      *
           MOVE IMPORTE-E          TO IMPORTE-S
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 2100-BUSCAR-DIVISA-EN-TW                                     **
      ** HACEMOS SEARCH PARA BUSCAR EL PRODUCTO LEIDO EN EL FICHERO   **
      ** DENTRO DE LA TW Y SABER EL DTO QUE APLICA EN CASO DE QUE     **
      ** APLIQUE.                                                     **
      ******************************************************************
      *
       2100-BUS-DIV-EN-TW.
      *
           SET TB-INDICE             TO CT-1
      *
           SEARCH TB-TABLA-DIVISA
               AT END
                    DISPLAY 'DIVISA NO ENCOTRADA'
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
      *
               WHEN DIVISA-E = TB-DIVISA(TB-INDICE)
      *
                    COMPUTE IMPORTE-S = IMPORTE-E/TB-CAMBIO(TB-INDICE)
           END-SEARCH
      *
           .
      *
       2100-BUS-DIV-EN-TW-EXIT.
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
           MOVE TITILAR-E           TO TITILAR-S
           MOVE CUENTA-ORIGEN-E     TO CUENTA-ORIGEN-S
           MOVE CUENTA-DESTINO-E    TO CUENTA-DESTINO-S
           MOVE 'EUR'               TO DIVISA-S
      *
           WRITE REG-FSALIDA FROM CPY-LOCAL
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
              INITIALIZE CPY-LOCAL
              ADD CT-1                TO CN-REG-ESCRIT-FSALIDA
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 3000-FIN                                                     **
      ** MOSTRAMOS LOS DATOS DEL PRESTAMO.                            **
      ** FINALIZAMOS EL PGM.                                          **
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
      ** 3100-CERRAR-FICHEROS                                         **
      ** CERRAMOS LOS FICHEROS DE ENTRADA Y SALIDA CONTROLANDO SU     **
      ** FILE STATUS.                                                 **
      ******************************************************************
      *
       3100-CERRAR-FICHEROS.
      *
           CLOSE FENTRADA
                 FSALIDA
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
           READ FENTRADA INTO CPY-EXTRAN
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
