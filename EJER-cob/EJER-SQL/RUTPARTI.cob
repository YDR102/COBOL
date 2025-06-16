      ******************************************************************
      *                    E  X  T  R  C  L  I                         *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   RUTPARTI.
       AUTHOR.       DAVID.
       DATE-WRITTEN. 22/05/2025.
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
      *
       FILE-CONTROL.
      * OBLIGATORIO, PARA DECLARAR LOS FICHEROS DE ENTRADA Y SALIDA
      *
      ******************************************************************
      *     DATA DIVISION                                              *
      ******************************************************************
       DATA DIVISION.
      *
       FILE SECTION.
      *
      ******************************************************************
      *     F I L E  S E C T I O N                                     *
      ******************************************************************
      *
      *
      ******************************************************************
      *     W O R K I N G   S T O R A G E                              *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
       01 CA-CONSTANTES-NUM.
          05 CA-0                      PIC 9(01) VALUE 0.
       01 CA-CONSTANTES.
          05 CT-RUT                    PIC X(08) VALUE 'RUTPARTI'.
          05 CT-10                     PIC X(02) VALUE '10'.
          05 CT-01                     PIC 9(01) VALUE 1.
          05 CT-L                      PIC X(01) VALUE 'L'.
          05 CT-R                      PIC X(01) VALUE 'R'.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-99                     PIC X(02) VALUE '99'.
      *
       01 SW-SWITCHES.
          05 SW-FIN-CURSOR             PIC X(01).
             88 SI-FIN-CURSOR          VALUE 'S'.
             88 NO-FIN-CURSOR          VALUE 'N'.
      *
       01 WK-CONTADORES.
          05 CONT-LEIDOS               PIC 9(3).
      *
       01 WK-SQLCODE                   PIC -999.
      *---------------- SQLCA ------------------------
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *---------------- DCLGEN -----------------------
           EXEC SQL
               INCLUDE TBCLIDB2
           END-EXEC.
      *-------------- DEFINIMOS LOS CURSORES ---------
           EXEC SQL
               DECLARE CURSOR_CLI CURSOR FOR
                  SELECT ID_CLIENTE
                         ,NOMBRE
                         ,TIPO_CLIENTE
                         ,DNI_CIF
                         ,TELEFONO
                         ,EMAIL
                         ,DIRECCION
                    FROM CLIENTES_DB2
                    ORDER BY ID_CLIENTE
           END-EXEC.
      *
      ******************************************************************
      *     L I N K A G E   S E C T I O N                              *
      ******************************************************************
       LINKAGE SECTION.
      *ESTRUCTURA DE COMUNICACION DE MI RUTINA
       COPY CPYPARTI.
      *
      ******************************************************************
      *     PROCEDURE DIVISION                                         *
      ******************************************************************
       PROCEDURE DIVISION USING CPYPARTI.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
             UNTIL SI-FIN-CURSOR
      *
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT.
      *
      ******************************************************************
      *     1000-INICIO                                                *
      ******************************************************************
       1000-INICIO.
      *
           INITIALIZE ERRORES-RUT
                      DCLCLIENTES-DB2
      *
           MOVE CT-00              TO RETORNO-ERR
           MOVE CT-00              TO SUBRETORNO-ERR
           MOVE CT-00              TO DESCRIPCION-ERR
      *
           SET NO-FIN-CURSOR       TO TRUE
      *
           PERFORM 1100-ABRIR-CURSOR
              THRU 1100-ABRIR-CURSOR-EXIT
      *
           PERFORM 9000-LEER-CURSOR
              THRU 9000-LEER-CURSOR-EXIT
      *
           .
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     1100-ABRIR-CURSOR                                          *
      ******************************************************************
       1100-ABRIR-CURSOR.
      *
           SET NO-FIN-CURSOR TO TRUE
      *
           EXEC SQL
               OPEN CURSOR_CLI
           END-EXEC
      *
           MOVE SQLCODE TO WK-SQLCODE
           DISPLAY WK-SQLCODE
      *
           EVALUATE SQLCODE
              WHEN 0
                   DISPLAY '-------------OPEN CURSOR'
                   CONTINUE
              WHEN OTHER
                   MOVE SQLCODE               TO WK-SQLCODE
                   MOVE CT-99                 TO RETORNO-ERR
                   MOVE 'ABRIENDO'            TO DESCRIPCION-ERR
                   MOVE '1100-ABRIR-CURSOR'   TO PARRAFO-ERR
      *
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       1100-ABRIR-CURSOR-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2000-PROCESO                                               *
      ******************************************************************
       2000-PROCESO.
      *
           PERFORM 9000-LEER-CURSOR
              THRU 9000-LEER-CURSOR-EXIT
      *
           MOVE CONT-LEIDOS    TO REG-RECUPERADOS
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2300-INFORMAR-SALIDA                                       *
      ******************************************************************
       2300-INFORMAR-SALIDA.
      *
           ADD CT-01               TO CONT-LEIDOS
           MOVE TB-ID-CLIENTE      TO ID-CLIENTE   (CONT-LEIDOS)
           MOVE TB-NOMBRE          TO NOMBRE       (CONT-LEIDOS)
           MOVE TB-TIPO-CLIENTE    TO TIPO-CLIENTE (CONT-LEIDOS)
           MOVE TB-DNI-CIF         TO DNI-CIF      (CONT-LEIDOS)
           MOVE TB-TELEFONO        TO TELEFONO     (CONT-LEIDOS)
           MOVE TB-EMAIL           TO EMAIL        (CONT-LEIDOS)
           MOVE TB-DIRECCION       TO DIRECCION    (CONT-LEIDOS)
      *
           .
       2300-INFORMAR-SALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3000-FIN                                                   *
      ******************************************************************
       3000-FIN.
      *
           PERFORM 3100-CERRAR-CURSOR
              THRU 3100-CERRAR-CURSOR-EXIT
      *
           GOBACK.
       3000-FIN-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3100-CERRAR-CURSOR                                         *
      ******************************************************************
       3100-CERRAR-CURSOR.
      *
      *
           EXEC SQL
              CLOSE CURSOR_CLI
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   DISPLAY '---------CERRAR CURSOR'
                   CONTINUE
              WHEN OTHER
                   MOVE SQLCODE               TO WK-SQLCODE
                   MOVE CT-99                 TO RETORNO-ERR
                   MOVE 'CERRANDO'            TO DESCRIPCION-ERR
                   MOVE '3100-CERRAR-CURSOR'  TO PARRAFO-ERR
           END-EVALUATE
      *
           .
       3100-CERRAR-CURSOR-EXIT.
           EXIT.
      *
      ******************************************************************
      *     9000-LEER-CURSOR                                     *
      ******************************************************************
       9000-LEER-CURSOR.
      *
           DISPLAY '-----------FETCH CURSOR'
      *
           EXEC SQL
              FETCH CURSOR_CLI
               INTO :TB-ID-CLIENTE
                   ,:TB-NOMBRE
                   ,:TB-TIPO-CLIENTE
                   ,:TB-DNI-CIF
                   ,:TB-TELEFONO
                   ,:TB-EMAIL
                   ,:TB-DIRECCION
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   DISPLAY 'HAGO FETCH OK'
                   PERFORM 2300-INFORMAR-SALIDA
                      THRU 2300-INFORMAR-SALIDA-EXIT
              WHEN 100
                   SET SI-FIN-CURSOR            TO TRUE
              WHEN OTHER
                   MOVE SQLCODE                 TO WK-SQLCODE
                   MOVE CT-99                   TO RETORNO-ERR
                   MOVE 'LEYENDO'               TO DESCRIPCION-ERR
                   MOVE '9000-LEER-CURSOR'      TO PARRAFO-ERR
      *
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       9000-LEER-CURSOR-EXIT.
           EXIT.
