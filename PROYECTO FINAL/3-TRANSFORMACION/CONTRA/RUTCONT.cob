      ******************************************************************
      ** IDENTIFICATION DIVISION                                      **
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PGMDIVIS.
       AUTHOR. DAVID.
       DATE-WRITTEN. 02/05/2025.
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
      ******************************************************************
      ** DATA DIVISION                                                **
      ******************************************************************
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
      *
       01  CT-CONTANTES.
           05  CT-00                              PIC X(02) VALUE '00'.
           05  CT-10                              PIC X(02) VALUE '10'.
           05  CT-1                               PIC 9(02) VALUE 1.
           05  CT-0                               PIC 9(02) VALUE 0.
           05  CT-MAX                          PIC 9(08) VALUE 99999999.
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
      ******************************************************************
      *     L I N K A G E   S E C T I O N                              *
      ******************************************************************
       LINKAGE SECTION.
      * ESTRUCTURA DE COMUNICACION DE MI RUTINA
       COPY RUTCONT.
      *
      ******************************************************************
      ** PROCEDURE DIVISION                                           **
      ******************************************************************
      *
       PROCEDURE DIVISION USING RUTCONT.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
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
           INITIALIZE ERRORES-RUT
                      SALIDA-RUT
      *
           PERFORM 1200-INF-TABLA-WORKING
              THRU 1200-INF-TABLA-WORKING-EXIT
      *
           .
      *
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 1200-INF-TABLA-WORKING                                       **
      ** RELLENAMOS LA TABLA WORKING CON LA INFORMACION DE LOS        **
      ** PRODUCTOS.                                                   **
      ******************************************************************
      *
       1200-INF-TABLA-WORKING.
      *
           MOVE 'USD'                  TO TB-DIVISA(1)
           MOVE 'US DOLLAR'            TO TB-NOM-DIVISA(1)
           MOVE 1,0452                 TO TB-CAMBIO(1)
      *
           MOVE 'JPY'                  TO TB-DIVISA(2)
           MOVE 'JAPANESE YEN'         TO TB-NOM-DIVISA(2)
           MOVE 140,62                 TO TB-CAMBIO(2)
      *
           MOVE 'GBP'                  TO TB-DIVISA(3)
           MOVE 'POUND STERLING'       TO TB-NOM-DIVISA(3)
           MOVE 0,86578                TO TB-CAMBIO(3)
      *
           MOVE 'SEK'                  TO TB-DIVISA(4)
           MOVE 'SWEDISH KRONA'        TO TB-NOM-DIVISA(4)
           MOVE 10,6220                TO TB-CAMBIO(4)
      *
           MOVE 'TRY'                  TO TB-DIVISA(5)
           MOVE 'TURKISH LIRA'         TO TB-NOM-DIVISA(5)
           MOVE 18,0600                TO TB-CAMBIO(5)
      *
           MOVE 'BRL'                  TO TB-DIVISA(6)
           MOVE 'BRAZILIAN REAL'       TO TB-NOM-DIVISA(6)
           MOVE 5,3329                 TO TB-CAMBIO(6)
      *
           MOVE 'INR'                  TO TB-DIVISA(7)
           MOVE 'INDIAN RUPE'          TO TB-NOM-DIVISA(7)
           MOVE 81,4832                TO TB-CAMBIO(7)
      *
           MOVE 'MXN'                  TO TB-DIVISA(8)
           MOVE 'MAXICAN PESO'         TO TB-NOM-DIVISA(8)
           MOVE 21,4832                TO TB-CAMBIO(8)
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
           IF DIV-ORIG NOT = 'EUR'
                PERFORM 2100-BUS-DIV-EN-TW
                   THRU 2100-BUS-DIV-EN-TW-EXIT
      *
           MOVE IMPORT-ORIG          TO IMPORT-DEST
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
               WHEN DIV-ORIG = TB-DIVISA(TB-INDICE)
      *
                    DISPLAY 'TB-CAMBIO ' TB-CAMBIO(TB-INDICE)
                    DISPLAY 'IMPORT-ORIG ' IMPORT-ORIG
                    COMPUTE IMPORT-DEST =
                            IMPORT-ORIG/TB-CAMBIO(TB-INDICE)
           END-SEARCH
      *
           .
      *
       2100-BUS-DIV-EN-TW-EXIT.
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
           GOBACK.
      *
           .
      *
       3000-FIN-EXIT.
           EXIT.
