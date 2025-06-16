      ******************************************************************
      ** P G M T B W O R.-PGM DONDE A PARTIR DE UN FICHERO DE ENTRADA **
      **                  CON INFORMACION DE PRODUCTOS, CALCULAREMOS  **
      **                  LOS TOTALES APLICANDO LOS DTOS QUE BUSCARE- **
      **                  MOS EN UNA TABLA WORKING. SI NO ENCONTRAMOS **
      **                  DICHOS PRODUCTOS EN LA TABLA WORKING,       **
      **                  ESCRIBIREMOS EL REGISTRO EN EL FICHERO DE   **
      **                  INCIDENCIAS.                                **
      ******************************************************************
      *
      ******************************************************************
      ** IDENTIFICATION DIVISION                                      **
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PGMTBWOR.
       AUTHOR. CRISTIAN.
       INSTALLATION. EOI.
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
           SELECT FENTRADA ASSIGN TO FENTRADA
           FILE STATUS FS-FENTRADA.
      *
           SELECT FSALIDA ASSIGN TO FSALIDA
           FILE STATUS FS-FSALIDA.
      *
           SELECT FINCIDEN ASSIGN TO FINCIDEN
           FILE STATUS FS-FINCIDEN.
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
       01  REG-FENTRADA               PIC X(43).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                PIC X(55).
      *
       FD FINCIDEN
           RECORDING MODE IS F.
       01  REG-FINCIDEN               PIC X(43).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA            PIC X(02).
           05  FS-FSALIDA             PIC X(02).
           05  FS-FINCIDEN            PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA  PIC 9(03).
           05  CN-REG-ESCRIT-FINCIDEN PIC 9(03).
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA        PIC X(01).
               88  SW-SI-FIN-FENTRADA VALUE 'S'.
               88  SW-NO-FIN-FENTRADA VALUE 'N'.
      *
      *COPY DEL FICHERO DE ENTRADA FENTRADA CON LA INFO DE PRODUCTOS
       01 CPY-CPYINCID.
           05 COD-PRODUCTO-I PIC X(03).
           05 EMPRESA-PRODUCTO-I PIC X(30).
           05 CANTIDAD-PRODUCTO-I PIC 9(05).
           05 PRECIO-PRODUCTO-I PIC 9(03)V9(02).
      *
      *COPY DEL FICHERO DE SALIDA FSALIDA CON LA INFO DE LOS TOTALES
       01 CPY-CPYTOTPR.
           05 COD-PRODUCTO-S PIC X(03).
           05 EMPRESA-PRODUCTO-S PIC X(30).
           05 CANTIDAD-PRODUCTO-S PIC 9(05).
           05 PRECIO-PRODUCTO-S PIC 9(03)V9(02).
           05 DTO-APLI-S PIC X(01).
           05 TOTAL-PRECIO-S PIC 9(09)V9(02).
      *
      *COPY DEL FICHERO DE INCIDENCIAS CON LOS PRODUCTOS NO ENCONTRADOS
       01 CPY-CPYPRODU.
           05 COD-PRODUCTO-E PIC X(03).
           05 EMPRESA-PRODUCTO-E PIC X(30).
           05 CANTIDAD-PRODUCTO-E PIC 9(05).
           05 PRECIO-PRODUCTO-E PIC 9(03)V9(02).
      *
      ******************************************************************
      *    T A B L A S         W O R K I N G / I N T E R N A S         *
      ******************************************************************
      *
       01  TB-TABLAS.
           05  TB-TABLA-PRODUCTOS   OCCURS 15 TIMES
                                    INDEXED BY TB-INDICE.
               10  TB-COD-PRODUCTO  PIC X(03).
               10  TB-APLICA-DTO    PIC X(01).
               10  TB-DTO           PIC 9(02).
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
                      CPY-CPYPRODU
                      CPY-CPYTOTPR
                      CPY-CPYINCID
      *
           SET SW-NO-FIN-FENTRADA        TO TRUE
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
                       FINCIDEN
      *
           IF FS-FENTRADA NOT = '00'
              DISPLAY 'ERROR AL ABRIR EL FICHERO FENTRADA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FSALIDA NOT = '00'
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
           IF FS-FINCIDEN NOT = '00'
      *
              DISPLAY 'ERROR AL ABRIR EL FICHERO FINCIDEN'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FINCIDEN'
              DISPLAY 'FILE STATUS: ' FS-FINCIDEN
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
           MOVE '100'                  TO TB-COD-PRODUCTO(1)
           MOVE 'N'                    TO TB-APLICA-DTO(1)
           MOVE 0                      TO TB-DTO(1)
      *
           MOVE '101'                  TO TB-COD-PRODUCTO(2)
           MOVE 'S'                    TO TB-APLICA-DTO(2)
           MOVE 3                      TO TB-DTO(2)
      *
           MOVE '102'                  TO TB-COD-PRODUCTO(3)
           MOVE 'N'                    TO TB-APLICA-DTO(3)
           MOVE 0                      TO TB-DTO(3)
      *
           MOVE '103'                  TO TB-COD-PRODUCTO(4)
           MOVE 'S'                    TO TB-APLICA-DTO(4)
           MOVE 7                      TO TB-DTO(4)
      *
           MOVE '104'                  TO TB-COD-PRODUCTO(5)
           MOVE 'S'                    TO TB-APLICA-DTO(5)
           MOVE 5                      TO TB-DTO(5)
      *
           MOVE '105'                  TO TB-COD-PRODUCTO(6)
           MOVE 'S'                    TO TB-APLICA-DTO(6)
           MOVE 10                     TO TB-DTO(6)
      *
           MOVE '106'                  TO TB-COD-PRODUCTO(7)
           MOVE 'N'                    TO TB-APLICA-DTO(7)
           MOVE 0                      TO TB-DTO(7)
      *
           MOVE '107'                  TO TB-COD-PRODUCTO(8)
           MOVE 'N'                    TO TB-APLICA-DTO(8)
           MOVE 0                      TO TB-DTO(8)
      *
           MOVE '108'                  TO TB-COD-PRODUCTO(9)
           MOVE 'S'                    TO TB-APLICA-DTO(9)
           MOVE 9                      TO TB-DTO(9)
      *
           MOVE '109'                  TO TB-COD-PRODUCTO(10)
           MOVE 'S'                    TO TB-APLICA-DTO(10)
           MOVE 20                     TO TB-DTO(10)
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
           PERFORM 2100-BUSCAR-PROD-EN-TW
              THRU 2100-BUSCAR-PROD-EN-TW-EXIT
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
      ** 2100-BUSCAR-PROD-EN-TW                                       **
      ** HACEMOS SEARCH PARA BUSCAR EL PRODUCTO LEIDO EN EL FICHERO   **
      ** DENTRO DE LA TW Y SABER EL DTO QUE APLICA EN CASO DE QUE     **
      ** APLIQUE.                                                     **
      ******************************************************************
      *
       2100-BUSCAR-PROD-EN-TW.
      *
           SET TB-INDICE             TO 1
      *
           SEARCH TB-TABLA-PRODUCTOS
               AT END
                  PERFORM 2300-ESCRIBIR-FINCIDEN
                     THRU 2300-ESCRIBIR-FINCIDEN-EXIT
      *
               WHEN COD-PRODUCTO-E = TB-COD-PRODUCTO(TB-INDICE)
                    MOVE TB-APLICA-DTO(TB-INDICE)   TO DTO-APLI-S
                    IF TB-APLICA-DTO(TB-INDICE) = 'S'
                       COMPUTE TOTAL-PRECIO-S =
                       (CANTIDAD-PRODUCTO-E * PRECIO-PRODUCTO-E) -
                       ((CANTIDAD-PRODUCTO-E * PRECIO-PRODUCTO-E *
                        TB-DTO(TB-INDICE))/100)
                    ELSE
                       COMPUTE TOTAL-PRECIO-S =
                       CANTIDAD-PRODUCTO-E * PRECIO-PRODUCTO-E
                    END-IF
      *
                    PERFORM 2200-ESCRIBIR-FSALIDA
                       THRU 2200-ESCRIBIR-FSALIDA-EXIT
           END-SEARCH
      *
           .
      *
       2100-BUSCAR-PROD-EN-TW-EXIT.
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
           MOVE COD-PRODUCTO-E         TO COD-PRODUCTO-S
           MOVE EMPRESA-PRODUCTO-E     TO EMPRESA-PRODUCTO-S
           MOVE CANTIDAD-PRODUCTO-E    TO CANTIDAD-PRODUCTO-S
           MOVE PRECIO-PRODUCTO-E      TO PRECIO-PRODUCTO-S
      *
           WRITE REG-FSALIDA FROM CPY-CPYTOTPR
      *
           IF FS-FSALIDA NOT = '00'
              DISPLAY 'ERROR AL ESCRIBIR EL FICHERO FSALIDA'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA'
              DISPLAY 'NOMBRE FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CPY-CPYTOTPR
              ADD 1                TO CN-REG-ESCRIT-FSALIDA
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 2300-ESCRIBIR-FINCIDEN                                       **
      ** ESCRIBIREMOS EL REGISTRO EN EL FICHERO DE INCIDENCIAS AL NO  **
      ** HABER ENCONTRADO EL PRODUCTO, POSTERIORMENTE CONTROLAREMOS SU**
      ** FILE STATUS.                                                 **
      ******************************************************************
      *
       2300-ESCRIBIR-FINCIDEN.
      *
           MOVE COD-PRODUCTO-E         TO COD-PRODUCTO-I
           MOVE EMPRESA-PRODUCTO-E     TO EMPRESA-PRODUCTO-I
           MOVE CANTIDAD-PRODUCTO-E    TO CANTIDAD-PRODUCTO-I
           MOVE PRECIO-PRODUCTO-E      TO PRECIO-PRODUCTO-I
      *
           WRITE REG-FINCIDEN FROM CPY-CPYINCID
      *
           IF FS-FINCIDEN NOT = '00'
              DISPLAY 'ERROR AL ESCRIBIR EL FICHERO FINCIDEN'
              DISPLAY 'PARRAFO: 2300-ESCRIBIR-FINCIDEN'
              DISPLAY 'NOMBRE FICHERO: FINCIDEN'
              DISPLAY 'FILE STATUS: ' FS-FINCIDEN
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CPY-CPYINCID
              ADD 1                TO CN-REG-ESCRIT-FINCIDEN
           END-IF
      *
           .
      *
       2300-ESCRIBIR-FINCIDEN-EXIT.
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
                 FINCIDEN
      *
           IF FS-FENTRADA NOT = '00'
              DISPLAY 'ERROR AL CERRAR EL FICHERO FENTRADA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
           END-IF
      *
           IF FS-FSALIDA NOT = '00'
              DISPLAY 'ERROR AL CERRAR EL FICHERO FSALIDA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
           END-IF
      *
           IF FS-FINCIDEN NOT = '00'
              DISPLAY 'ERROR AL CERRAR EL FICHERO FSALIDA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FINCIDEN'
              DISPLAY 'FILE STATUS: ' FS-FINCIDEN
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
           DISPLAY '*******************************'
           DISPLAY '**D A T O S   P G M T B W O R**'
           DISPLAY '*******************************'
           DISPLAY '*REG.LEIDOS FENTRA' CN-REG-LEIDOS-FENTRADA '   *'
                   '*'
           DISPLAY '*REG.ESCRIT FSALID' CN-REG-ESCRIT-FSALIDA '*'
           DISPLAY '*REG.ESCRIT FINCID' CN-REG-ESCRIT-FINCIDEN '*'
           DISPLAY '*******************************'
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
           READ FENTRADA INTO CPY-CPYPRODU
      *
           EVALUATE FS-FENTRADA
               WHEN '00'
                    ADD 1               TO CN-REG-LEIDOS-FENTRADA
               WHEN '10'
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
