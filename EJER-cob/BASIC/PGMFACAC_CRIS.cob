      ******************************************************************
      ** P G M F A C A C.-PGM QUE LEE UN FICHERO DE MOVIMIENTOS DE    **
      **                  FACTURACION, DONDE CADA REGISTRO REPRESENTA **
      **                  UNA LINEA DE FACTURACION ASOCIADA A UN      **
      **                  CLIENTE, E INCLUYTE SU IDENTIFICADOR, NOMBRE**
      **                  FECHA E IMPORTE FACTURADO. EL OBJETIVO ES   **
      **                  ACUMULAR MEDIANTE UNA TABLA INTERNA/WORKING **
      **                  EL IMPORTE TOTAL FACTURADO POR CADA CLIENTE.**
      ******************************************************************
      *
      ******************************************************************
      ** IDENTIFICATION DIVISION                                      **
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PGMFACAC.
       AUTHOR. CRISTIAN.
       INSTALLATION. EOI.
       DATE-WRITTEN. 12/05/2025.
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
       01  REG-FENTRADA               PIC X(57).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                PIC X(51).
      *
       FD FINCIDEN
           RECORDING MODE IS F.
       01  REG-FINCIDEN               PIC X(57).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA            PIC X(02).
           05  FS-FSALIDA             PIC X(02).
           05  FS-FINCIDEN            PIC X(02).
      *
       01  CN-CONSTANTES-NUMERICAS.
           05  CN-TAM-TW              PIC 9(03) VALUE 005.
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA  PIC 9(03).
           05  CN-REG-ESCRIT-FINCIDEN PIC 9(03).
           05  CN-CONTADOR            PIC 9(03).
           05  CN-IND-TABLA           PIC 9(03).
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA        PIC X(01).
               88  SW-SI-FIN-FENTRADA VALUE 'S'.
               88  SW-NO-FIN-FENTRADA VALUE 'N'.
      *
      *COPY DEL FICHERO DE ENTRADA FENTRADA Y DE INCICENDIAS FINCIDEN
       COPY CPYFACIN.
      *
      *COPY DEL FICHERO DE SALIDA FSALIDA CON LA INFO DE LAS FACTURAS
      *ACUMULADAS POR CLIENTE
       COPY CPYFAOUT.
      *
      ******************************************************************
      *    T A B L A S         W O R K I N G / I N T E R N A S         *
      ******************************************************************
      *
       01  TB-TABLAS.
           05  TB-TABLA-FACTURACION OCCURS 05 TIMES
                                    INDEXED BY TB-INDICE.
               10  TB-ID-CLIENTE    PIC X(10).
               10  TB-NOM-CLIENTE   PIC X(30).
               10  TB-IMPORTE-TOTAL PIC 9(09)V9(02).
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
                      CPY-CPYFACIN
                      CPY-CPYFAOUT
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
      ******************************************************************
      ** 2000-PROCESO                                                 **
      ** VALIDAR EL VALOR DEL INDICADOR DE PENALIZACION PARA SABER    **
      ** SI TENGO QUE CALCULAR LA COMISION Y CALCULAR EL IMPORTE TOTAL**
      ******************************************************************
      *
       2000-PROCESO.
      *
           PERFORM 2100-BUSCAR-CLI-EN-TW
              THRU 2100-BUSCAR-CLI-EN-TW-EXIT
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
      ** 2100-BUSCAR-CLI-EN-TW                                        **
      ** HACEMOS SEARCH PARA BUSCAR EL CLIENTE LEIDO EN FICHERO DE    **
      ** ENTRADA, CON EL OBJETIVO DE SI SE TRATA DE UN NUEVO CLIENTE  **
      ** QUE NO HEMOS LEIDO HASTA EL MOMENTO, REGISTRARLO EN LA TW O  **
      ** BIEN SI YA EXISTE DICHO CLIENTE EN LA TW, ACTUALIZAR EL      **
      ** ACUMULADO DE LOS IMPORTES DE SUS FACTURAS.                   **
      ******************************************************************
      *
       2100-BUSCAR-CLI-EN-TW.
      *
           SET TB-INDICE             TO 1
      *
           SEARCH TB-TABLA-FACTURACION
               AT END
                  IF CN-IND-TABLA >= CN-TAM-TW
                     PERFORM 2200-ESCRIBIR-FINCIDEN
                        THRU 2200-ESCRIBIR-FINCIDEN-EXIT
                  ELSE
                     ADD 1            TO CN-IND-TABLA
                     MOVE ID-CLIENTE-E
                                      TO TB-ID-CLIENTE(CN-IND-TABLA)
                     MOVE NOMBRE-CLIENTE-E
                                      TO TB-NOM-CLIENTE(CN-IND-TABLA)
                     MOVE IMPORTE-FACTURA-E
                                      TO TB-IMPORTE-TOTAL(CN-IND-TABLA)
                  END-IF
      *
               WHEN ID-CLIENTE-E = TB-ID-CLIENTE(TB-INDICE)
                    ADD IMPORTE-FACTURA-E
                                 TO TB-IMPORTE-TOTAL(TB-INDICE)
           END-SEARCH
      *
           .
      *
       2100-BUSCAR-CLI-EN-TW-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 2200-ESCRIBIR-FINCIDEN                                       **
      ** ESCRIBIREMOS EL REGISTRO EN EL FICHERO DE INCIDENCIAS CON EL **
      ** MISMO FORMATO QUE EL FICHERO DE ENTRADA, LA INFORMACION QUE  **
      ** CONTENDRA EL FICHERO DE INCIDENCIAS SON LA DE AQUELLOS       **
      ** CLIENTES QUE NO HAYAMOS PODIDO GUARDAR EN LA TW POR TAMAÑO.  **
      ******************************************************************
      *
       2200-ESCRIBIR-FINCIDEN.
      *
           WRITE REG-FINCIDEN FROM CPY-CPYFACIN
      *
           IF FS-FINCIDEN NOT = '00'
              DISPLAY 'ERROR AL ESCRIBIR EL FICHERO FINCIDEN'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FINCIDEN'
              DISPLAY 'NOMBRE FICHERO: FINCIDEN'
              DISPLAY 'FILE STATUS: ' FS-FINCIDEN
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CPY-CPYFACIN
              ADD 1                TO CN-REG-ESCRIT-FINCIDEN
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FINCIDEN-EXIT.
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
           PERFORM 3100-MOVER-INF-TW-FSALIDA
              THRU 3100-MOVER-INF-TW-FSALIDA-EXIT
      *
           PERFORM 3300-CERRAR-FICHEROS
              THRU 3300-CERRAR-FICHEROS-EXIT
      *
           PERFORM 3400-MOSTRAR-ESTADISTICAS
              THRU 3400-MOSTRAR-ESTADISTICAS-EXIT
      *
           STOP RUN
      *
           .
      *
       3000-FIN-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 3100-MOVER-INF-TW-FSALIDA                                    **
      ** RECORRER LA TW PARA ESCRIBIR CADA OCURRENCIA DE LA MISMA EN  **
      ** EL FICHERO DE SALIDA FSALIDA.                                **
      ******************************************************************
      *
       3100-MOVER-INF-TW-FSALIDA.
      *
           PERFORM VARYING CN-CONTADOR FROM 1 BY 1
                   UNTIL CN-CONTADOR > CN-TAM-TW OR
                       TB-ID-CLIENTE(CN-CONTADOR) = SPACES OR LOW-VALUES
                   PERFORM 3200-ESCRIBIR-FSALIDA
                      THRU 3200-ESCRIBIR-FSALIDA-EXIT
           END-PERFORM
      *
           .
      *
       3100-MOVER-INF-TW-FSALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 3200-ESCRIBIR-FSALIDA                                        **
      ** ESCRIBIREMOS EL REGISTRO DE SALIDA EN EL FICHERO EVALUANDO   **
      ** POSTERIORMENTE SU FILE STATUS.                               **
      ******************************************************************
      *
       3200-ESCRIBIR-FSALIDA.
      *
           MOVE TB-ID-CLIENTE(CN-CONTADOR)    TO ID-CLIENTE-S
           MOVE TB-NOM-CLIENTE(CN-CONTADOR)   TO NOMBRE-CLIENTE-S
           MOVE TB-IMPORTE-TOTAL(CN-CONTADOR) TO IMPORTE-ACUMULADO-S
      *
           WRITE REG-FSALIDA FROM CPY-CPYFAOUT
      *
           IF FS-FSALIDA NOT = '00'
              DISPLAY 'ERROR AL ESCRIBIR EL FICHERO FSALIDA'
              DISPLAY 'PARRAFO: 3200-ESCRIBIR-FSALIDA'
              DISPLAY 'NOMBRE FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CPY-CPYFAOUT
              ADD 1                TO CN-REG-ESCRIT-FSALIDA
           END-IF
      *
           .
      *
       3200-ESCRIBIR-FSALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 3300-CERRAR-FICHEROS                                         **
      ** CERRAMOS LOS FICHEROS DE ENTRADA Y SALIDA CONTROLANDO SU     **
      ** FILE STATUS.                                                 **
      ******************************************************************
      *
       3300-CERRAR-FICHEROS.
      *
           CLOSE FENTRADA
                 FSALIDA
                 FINCIDEN
      *
           IF FS-FENTRADA NOT = '00'
              DISPLAY 'ERROR AL CERRAR EL FICHERO FENTRADA'
              DISPLAY 'PARRAFO: 3300-CERRAR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
           END-IF
      *
           IF FS-FSALIDA NOT = '00'
              DISPLAY 'ERROR AL CERRAR EL FICHERO FSALIDA'
              DISPLAY 'PARRAFO: 3300-CERRAR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
           END-IF
      *
           IF FS-FINCIDEN NOT = '00'
              DISPLAY 'ERROR AL CERRAR EL FICHERO FSALIDA'
              DISPLAY 'PARRAFO: 3300-CERRAR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FINCIDEN'
              DISPLAY 'FILE STATUS: ' FS-FINCIDEN
           END-IF
      *
           .
      *
       3300-CERRAR-FICHEROS-EXIT.
           EXIT.
      *
      *
      ******************************************************************
      ** 3400-MOSTRAR-ESTADISTICAS                                    **
      ** MOSTRAMOS ESTADISTICAS DEL PGM IMPRIMIENTO LOS REG. LEIDOS   **
      ** DEL FICHERO DE ENTRADA Y LOS REG. ESCRITOS DEL FICH. DE SALIDA*
      ** E INCIDENCIAS.                                                *
      ******************************************************************
      *
       3400-MOSTRAR-ESTADISTICAS.
      *
           DISPLAY '*******************************'
           DISPLAY '**D A T O S   P G M F A C A C**'
           DISPLAY '*******************************'
           DISPLAY '*REG.LEIDOS FENTRADA: 'CN-REG-LEIDOS-FENTRADA '   '
                   '  *'
           DISPLAY '*REG.ESCRIT FSALIDA:  'CN-REG-ESCRIT-FSALIDA '   '
                   '  *'
           DISPLAY '*REG.ESCRIT FINCIDEN: 'CN-REG-ESCRIT-FINCIDEN '   '
                   '  *'
           DISPLAY '*******************************'
      *
           .
      *
       3400-MOSTRAR-ESTADISTICAS-EXIT.
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
           READ FENTRADA INTO CPY-CPYFACIN
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
