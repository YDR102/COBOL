      ******************************************************************
      *                     E  X  T  C  L  I                           *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   EJPGMC01.
       AUTHOR.       DAVID.
       DATE-WRITTEN. 23/05/2025.
       DATE-COMPILED.
      *
      ******************************************************************
      *     ENVIRONMENT DIVISION                                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
         SOURCE-COMPUTER.  IBM-3090.
         OBJECT-COMPUTER.  IBM-3090.
      *
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
      * SE DECLARAN LOS FICHEROS DE FENTRADA Y FSALIDA
      *
       FILE-CONTROL.
      *
           SELECT FENTRADA  ASSIGN TO FENTRADA
           FILE STATUS FS-FENTRADA.
      *
           SELECT FSALIDA ASSIGN TO FSALIDA
           FILE STATUS FS-FSALIDA.
      *
      ******************************************************************
      *     DATA DIVISION                                              *
      ******************************************************************
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD FENTRADA
            BLOCK CONTAINS 0 RECORDS
            LABEL RECORD ARE STANDARD
            RECORDING MODE IS F
            DATA RECORD IS REG-FENTRADA.
       01 REG-FENTRADA                                       PIC X(068).
      *
       FD FSALIDA
            BLOCK CONTAINS 0 RECORDS
            LABEL RECORD ARE STANDARD
            RECORDING MODE IS F
            DATA RECORD IS REG-FSALIDA.
       01  REG-FSALIDA                                       PIC X(114).
      *
      ******************************************************************
      *     W O R K I N G   S T O R A G E                              *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA                          PIC X(02).
           05  FS-EMPRESA                          PIC X(02).
           05  FS-FSALIDA                           PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA               PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA                PIC 9(03).
           05  CN-REG-LEIDOS-CURSOR                 PIC 9(03).
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA                                PIC X(01).
               88  SW-SI-FIN-FENTRADA              VALUE 'S'.
               88  SW-NO-FIN-FENTRADA              VALUE 'N'.
          05 SW-FIN-CURSOR                                    PIC X(01).
               88 SW-SI-FIN-CURSOR                    VALUE 'S'.
               88 SW-NO-FIN-CURSOR                    VALUE 'N'.
      *
       01 CT-CONSTANTES.
          05 CT-RUT                    PIC X(08) VALUE 'EJRUTC01'.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-10                     PIC X(02) VALUE '10'.
          05 CT-01                     PIC 9(01) VALUE 1.
          05 CT-17                     PIC X(02) VALUE '17'.
          05 CT-18                     PIC X(02) VALUE '18'.
          05 CT-19                     PIC X(02) VALUE '19'.
      *
       01 WK-INDICES.
          05 WK-SQLCODE                   PIC -999.

      *
      *COPY DE COMUNICACION CON LA RUTINA
       COPY EJCPYC01.
      *
      *COPY DE ENTRADA
       01 CPY-ENTRADA.
           05 COD-CLIENTE-E            PIC X(09).
           05 NIF-E                    PIC X(09).
           05 NOMBRE-E                 PIC X(20).
           05 APELLIDOS-E              PIC X(30).
      *
      *COPY DE FSALIDA
       01 CPY-SALIDA.
           05 COD-CLIENTE-S            PIC 9(09).
           05 NIF-S                    PIC X(09).
           05 NOMBRE-S                 PIC X(20).
           05 APELLIDOS-S              PIC X(30).
           05 TIPO-DIR-ELEC-S          PIC X(04).
           05 VALOR-DIR-S              PIC X(20).
           05 PREFIJO-S                PIC X(02).
           05 PROVINCIA-S              PIC X(20).
      *---------------- SQLCA ------------------------
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *---------------- DCLGEN -----------------------
           EXEC SQL
               INCLUDE TBDIRELE
           END-EXEC.
      *-------------- DEFINIMOS LOS CURSORES ---------
           EXEC SQL
               DECLARE CURSOR_DIRELEC CURSOR FOR
                  SELECT TIPO_DIR_ELEC
                         ,VALOR
                         ,COD_CLIENTE
                    FROM DIRELEC WHERE COD_CLIENTE = :COD-CLIENTE-E
                    ORDER BY COD_CLIENTE
           END-EXEC.
      *
      ******************************************************************
      *     PROCEDURE DIVISION                                         *
      ******************************************************************
       PROCEDURE DIVISION.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
             UNTIL SW-SI-FIN-CURSOR
      *
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT.
      *
      ******************************************************************
      *     1000-INICIO                                                *
      ******************************************************************
       1000-INICIO.
      *
           INITIALIZE EJCPYC01
                      WK-SQLCODE
                      CPY-SALIDA
                      CPY-ENTRADA
      *
           SET SW-NO-FIN-FENTRADA TO TRUE
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT
      *
           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
      *
           .
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 1100-ABRIR-FICHEROS                                            *
      ******************************************************************
      *
       1100-ABRIR-FICHEROS.
      *
           OPEN OUTPUT FSALIDA
           OPEN INPUT  FENTRADA
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FENTRADA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FSALIDA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
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
      ******************************************************************
       1200-ABRIR-CURSOR.
      *
           SET SW-NO-FIN-CURSOR TO TRUE
           MOVE COD-CLIENTE-E TO TB-COD-CLIENTE
      *
           EXEC SQL
               OPEN CURSOR_DIRELEC
           END-EXEC
      *
           MOVE SQLCODE TO WK-SQLCODE
      *
           EVALUATE SQLCODE
              WHEN 0
                   DISPLAY '-------------OPEN CURSOR'
                   CONTINUE
              WHEN OTHER
                   MOVE SQLCODE               TO WK-SQLCODE
                   MOVE CT-17                 TO RETORNO-ERR
                   MOVE 'ABRIENDO'            TO DESCRIPCION-ERR
                   MOVE '1100-ABRIR-CURSOR'   TO PARRAFO-ERR
      *
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       1200-ABRIR-CURSOR-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2000-PROCESO                                               *
      ******************************************************************
       2000-PROCESO.
      *
           PERFORM 1200-ABRIR-CURSOR
              THRU 1200-ABRIR-CURSOR-EXIT
      *
           PERFORM 9100-LEER-CURSOR
              THRU 9100-LEER-CURSOR-EXIT
             UNTIL SW-SI-FIN-CURSOR
      *
           PERFORM 3400-CERRAR-CURSOR
              THRU 3400-CERRAR-CURSOR-EXIT
      *
           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2100-LLAMAR-RUTINA                                         *
      ******************************************************************
       2100-LLAMAR-RUTINA.
      *
           DISPLAY 'CALL A LA RUTINA'
           MOVE TB-VALOR(1:2)   TO PREFIJO-E-RUT
      *
           CALL CT-RUT USING EJCPYC01
      *
           EVALUATE RETORNO-ERR
              WHEN CT-00
                     MOVE PREFIJO-E-RUT TO PREFIJO-S
                     MOVE PROVINCIA-S-RUT TO PROVINCIA-S
                     CONTINUE
              WHEN OTHER
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2100-LLAMAR-RUTINA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA                                         *
      ******************************************************************
      *
       2200-ESCRIBIR-FSALIDA.
      *
           MOVE COD-CLIENTE-E    TO COD-CLIENTE-S
           MOVE NOMBRE-E         TO NOMBRE-S
           MOVE APELLIDOS-E      TO APELLIDOS-S
           MOVE TB-TIPO-DIR-ELEC TO TIPO-DIR-ELEC-S
           MOVE TB-VALOR         TO VALOR-DIR-S
      *
           WRITE REG-FSALIDA FROM CPY-SALIDA
      *
           IF FS-FSALIDA NOT = CT-00
                DISPLAY 'ERROR AL ESCRIBIR FSALIDA'
                DISPLAY 'PARRAFO: 2300-ESCRIBIR-FSALIDA'
                DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
                PERFORM 3000-FIN
                   THRU 3000-FIN-EXIT
           ELSE
                INITIALIZE CPY-SALIDA
                ADD CT-01 TO CN-REG-ESCRIT-FSALIDA
           END-IF

      *
           .
      *
       2200-ESCRIBIR-FSALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3000-FIN                                                   *
      ******************************************************************
       3000-FIN.
      *
           IF RETORNO-ERR = CT-00
                PERFORM 3200-CERRAR-FICHEROS
                   THRU 3200-CERRAR-FICHEROS-EXIT
                PERFORM 3400-CERRAR-CURSOR
                   THRU 3400-CERRAR-CURSOR-EXIT
      *
                PERFORM 3300-MOSTRAR-ESTADISTICAS
                   THRU 3300-MOSTRAR-ESTADISTICAS-EXIT
           ELSE
                PERFORM 3100-MOSTRAR-ERROR
                   THRU 3100-MOSTRAR-ERROR-EXIT
           END-IF
      *

           STOP RUN.
      *
       3000-FIN-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3100-MOSTRAR-ERROR                                           *
      ******************************************************************
       3100-MOSTRAR-ERROR.
      *
           DISPLAY '****************************'
           DISPLAY '* SE HA PRODUCIDO UN ERROR *'
           DISPLAY '* RETORNO     DEL ERROR:   *' RETORNO-ERR
           DISPLAY '* SUBRETORNO  DEL ERROR:   *' SUBRETORNO-ERR
           DISPLAY '* DESCRIPCION DEL ERROR:   *' DESCRIPCION-ERR
           DISPLAY '* SQLCODE RUT DEL ERROR:   *' SQLCODE-ERR
           DISPLAY '* SQLCODE PGM DEL ERROR:   *' WK-SQLCODE
           DISPLAY '* PARRAFO     DEL ERROR:   *' PARRAFO-ERR
           DISPLAY '****************************'
      *
           .
       3100-MOSTRAR-ERROR-EXIT.
           EXIT.
      *
      *
      ******************************************************************
      * 3200-CERRAR-FICHEROS                                           *
      ******************************************************************
      *
       3200-CERRAR-FICHEROS.
      *
           CLOSE FSALIDA
           CLOSE FENTRADA
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FENTRADA'
              DISPLAY 'PARRAFO: 3200-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
           END-IF
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FSALIDA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
           END-IF
      *
           .
      *
       3200-CERRAR-FICHEROS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3300-MOSTRAR-ESTADISTICAS                                      *
      ******************************************************************
      *
       3300-MOSTRAR-ESTADISTICAS.
      *
           DISPLAY '***************************************************'
           DISPLAY '*       ESTADISTICAS DEL PGM PGMFICH              *'
           DISPLAY '***************************************************'
           DISPLAY '*REG.ESCRITOS FSALIDA: ' CN-REG-ESCRIT-FSALIDA  '  '
                   '                      *'
           DISPLAY '*REG.LEIDOS FENTRADA:  ' CN-REG-LEIDOS-FENTRADA '  '
                   '                      *'
           DISPLAY '*REG.LEIDOS CURSOR:    ' CN-REG-LEIDOS-CURSOR   '  '
                  '                       *'
           DISPLAY '***************************************************'
      *
           .
      *
       3300-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3400-CERRAR-CURSOR                                         *
      ******************************************************************
       3400-CERRAR-CURSOR.
      *
      *
           EXEC SQL
              CLOSE CURSOR_DIRELEC
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   DISPLAY '---------CERRAR CURSOR'
                   CONTINUE
              WHEN OTHER
                   MOVE SQLCODE               TO WK-SQLCODE
                   MOVE CT-18                 TO RETORNO-ERR
                   MOVE 'CERRANDO'            TO DESCRIPCION-ERR
                   MOVE '3100-CERRAR-CURSOR'  TO PARRAFO-ERR
           END-EVALUATE
           INITIALIZE EJCPYC01
      *
           .
       3400-CERRAR-CURSOR-EXIT.
           EXIT.
      *
      ******************************************************************
      * 9000-LEER-FENTRADA                                             *
      ******************************************************************
      *
       9000-LEER-FENTRADA.
      *
           READ FENTRADA INTO CPY-ENTRADA
      *
           EVALUATE FS-FENTRADA
               WHEN CT-00
                    ADD CT-01              TO CN-REG-LEIDOS-FENTRADA
               WHEN CT-10
                    SET SW-SI-FIN-FENTRADA  TO TRUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL FICHERO FENTRADA'
                    DISPLAY 'PARRAFO: 9000-LEER-FENTRADA'
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
      *
      ******************************************************************
      *     9100-LEER-CURSOR                                           *
      ******************************************************************
       9100-LEER-CURSOR.
      *
           DISPLAY '-----------FETCH CURSOR'
      *
           EXEC SQL
              FETCH CURSOR_DIRLECT
               INTO :TB-TIPO-DIR-ELEC
                   ,:TB-VALOR
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   ADD CT-01                    TO CN-REG-LEIDOS-CURSOR
      *
           IF TB-TIPO-DIR-ELEC = 'TLF'
                PERFORM 2100-LLAMAR-RUTINA
                   THRU 2100-LLAMAR-RUTINA-EXIT
      *
                PERFORM 2200-ESCRIBIR-FSALIDA
                   THRU 2200-ESCRIBIR-FSALIDA-EXIT
           ELSE
                PERFORM 2200-ESCRIBIR-FSALIDA
                   THRU 2200-ESCRIBIR-FSALIDA-EXIT
           END-IF
              WHEN 100
                   SET SW-SI-FIN-CURSOR            TO TRUE
              WHEN OTHER
                   MOVE SQLCODE                 TO WK-SQLCODE
                   MOVE CT-19                   TO RETORNO-ERR
                   MOVE 'LEYENDO'               TO DESCRIPCION-ERR
                   MOVE '9000-LEER-CURSOR'      TO PARRAFO-ERR
      *
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       9100-LEER-CURSOR-EXIT.
           EXIT.

