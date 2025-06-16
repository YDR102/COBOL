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
      *
           SELECT FSALIDA ASSIGN TO FSALIDA
           FILE STATUS FS-FSALIDA.
      *
           SELECT FINCIDE  ASSIGN TO FINCIDE
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
       FD FINCIDE
           RECORDING MODE IS F.
       01  REG-FINCIDE                PIC X(77).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                PIC X(033).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FSALIDA             PIC X(02).
           05  FS-FINCIDE             PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-LEIDOS              PIC 9(03).
           05  CN-ESCRITOS            PIC 9(03).
           05  CN-ERRONEOS            PIC 9(03).
      *
       01  WK-VARIABLES.
           05  WK-SQLCODE             PIC -999.
      *
       01  CONSTANTES.
           05  CT-00                  PIC X(02) VALUE '00'.
           05  CT-01                  PIC 9(02) VALUE 1.
           05  CT-10                  PIC X(02) VALUE '10'.
           05  CT-88                  PIC X(02) VALUE '88'.
           05  CT-99                  PIC X(02) VALUE '99'.
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA        PIC X(01).
               88  SW-SI-FIN-FENTRADA VALUE 'S'.
               88  SW-NO-FIN-FENTRADA VALUE 'N'.
      *
      *-- INCLUIMOS DCLGEN TABLA PRODUCTOS
           EXEC SQL
                  INCLUDE TBCOTYZ
           END-EXEC.
      *
      *-- INCLUIMOS COPY DE COMUNICACION CON DB2
           EXEC SQL
                  INCLUDE SQLCA
           END-EXEC.
      *
      *COPY DEL FICHERO DE SALIDA FSALIDA
       01  WK-SALIDA.
           05  WK-TIPOCOT             PIC S9(1)V USAGE COMP-3.
           05  WK-DESCRIPCION         PIC X(40).
           05  WK-FECHAVIG            PIC X(10).
           05  WK-PORCENTAGE          PIC S9(2)V9(2) USAGE COMP-3.
      *
      *COPY DEL FICHERO DE SALIDA FSALIDA
       01  ERR-INCIDE.
           05  EER-CODIGO             PIC X(02).
           05  EER-DESCRIPCION        PIC X(40).
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
                      DCLCOTIZACIONES
                      WK-SALIDA
      *
           PERFORM 1200-LEER-SYSIN
              THRU 1200-LEER-SYSIN-EXIT
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT
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
           OPEN OUTPUT FSALIDA
           OPEN OUTPUT FINCIDE
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
           IF FS-FINCIDE  NOT = CT-00
              DISPLAY 'ERROR AL ABRIR EL FICHERO FINCIDE'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FICHERO: FINCIDE'
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
      ******************************************************************
      * 1200-LEER-SYSIN.                                               *
      ******************************************************************
       1200-LEER-SYSIN.

           ACCEPT TB-TIPOCOT FROM SYSIN.

           IF TB-TIPOCOT = SPACES OR TB-TIPOCOT = LOW-VALUES
               MOVE CT-10 TO EER-CODIGO
               MOVE 'TIPO COTIZACION NO INFORMADO' TO EER-DESCRIPCION
               PERFORM 2300-ESCRIBIR-FINCIDE
                  THRU 2300-ESCRIBIR-FINCIDE-EXIT
           END-IF
      *
           .
      *
       1200-LEER-SYSIN-EXIT.
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
               SELECT TIPOCOT, DESCRIPCION, FECHAVIG, PORCENTAJE
               INTO
               :WK-TIPOCOT,
               :WK-DESCRIPCION,
               :WK-FECHAVIG,
               :WK-PORCENTAGE
               FROM COTIZACIONES
               WHERE TIPOCOT = :TB-TIPOCOT
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   PERFORM 2200-ESCRIBIR-FSALIDA
                      THRU 2200-ESCRIBIR-FSALIDA-EXIT

               WHEN 100
                   MOVE CT-88 TO EER-CODIGO
                   MOVE 'COTIZACION NO ENCONTRADA'  TO EER-DESCRIPCION
                   PERFORM 2300-ESCRIBIR-FINCIDE
                      THRU 2300-ESCRIBIR-FINCIDE-EXIT

               WHEN OTHER
                   MOVE CT-99 TO EER-CODIGO
                   MOVE 'ERROR DESCONOCIDO EN BBDD' TO EER-DESCRIPCION
                   PERFORM 2300-ESCRIBIR-FINCIDE
                      THRU 2300-ESCRIBIR-FINCIDE-EXIT
           END-EVALUATE
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
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR EN FSALIDA'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA'
              DISPLAY 'FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE WK-SALIDA
              ADD CT-01            TO CN-ESCRITOS
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA.                                         *
      * EN ESTE PARRAFO ESCRIBIREMOS EL REGISTRO EN EL FICHERO DE      *
      * SALIDA UTILIZANDO LA COPY INFORMADA DEL PARRAFO ANTERIOR.      *
      ******************************************************************
      *
       2300-ESCRIBIR-FINCIDE.
      *
           WRITE REG-FINCIDE        FROM ERR-INCIDE
      *
           IF FS-FINCIDE NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR EN FINCIDE'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA'
              DISPLAY 'FICHERO: FINCIDE'
              DISPLAY 'FILE STATUS: ' FS-FINCIDE
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE WK-SALIDA
              ADD CT-01            TO CN-ESCRITOS
           END-IF
      *
           .
      *
       2300-ESCRIBIR-FINCIDE-EXIT.
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
           CLOSE FSALIDA
           CLOSE FINCIDE
      *
           IF FS-FSALIDA  NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FICHERO FSALIDA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FICHERO: FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
           END-IF
      *
           IF FS-FINCIDE  NOT = CT-00
              DISPLAY 'ERROR AL CERRAR EL FICHERO FINCIDE'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FICHERO: FINCIDE'
              DISPLAY 'FILE STATUS: ' FS-FINCIDE
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
           DISPLAY '*REG.INSERTADOS    OK: ' CN-ERRONEOS            '  '
                   '                      *'
           DISPLAY '***************************************************'
      *
           .
      *
       3200-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.

