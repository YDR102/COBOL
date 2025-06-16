      ******************************************************************
      *                    E  X  T  R  C  L  I                         *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   RUTCURS.
       AUTHOR.       DAVID.
       DATE-WRITTEN. 19/05/2025.
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
       01 CA-CONSTANTES.
          05 CT-RUT                    PIC X(08) VALUE 'RUTCURS'.
          05 CT-10                     PIC X(02) VALUE '10'.
          05 CT-01                     PIC 9(01) VALUE 1.
          05 CT-L                      PIC X(01) VALUE 'L'.
          05 CT-R                      PIC X(01) VALUE 'R'.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-99                     PIC X(02) VALUE '99'.
          05 CT-PARR-PRO    PIC X(12) VALUE '2000-PROCESO'.
          05 CT-DESC-ERR    PIC X(12) VALUE 'ERROR SELECT'.
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
               INCLUDE TBPROCLI
           END-EXEC.
      *-------------- DEFINIMOS LOS CURSORES ---------
           EXEC SQL
               DECLARE CURSOR_PROD CURSOR FOR
                  SELECT ID_PEDIDO
                        ,ID_CLIENTE
                        ,FECHA_PEDIDO
                        ,IMPORTE_TOTAL
                        ,ESTADO
                        ,TIPO_ENVIO
                        ,COMENTARIOS
                    FROM PEDIDOS_CLIENTE
                    ORDER BY ID_PEDIDO
           END-EXEC.
      *
      ******************************************************************
      *     L I N K A G E   S E C T I O N                              *
      ******************************************************************
       LINKAGE SECTION.
      *ESTRUCTURA DE COMUNICACION DE MI RUTINA
       COPY 'CURSORES'.
      *
      ******************************************************************
      *     PROCEDURE DIVISION                                         *
      ******************************************************************
       PROCEDURE DIVISION USING CPY-CPYCURS.
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
                      DCLPEDIDOS-CLIENTE
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
           DISPLAY '-------------OPEN CURSOR'
           EXEC SQL
               OPEN CURSOR_PROD
           END-EXEC
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CT-99                 TO RETORNO-ERR
                   MOVE CT-DESC-ERR           TO DESCRIPCION-ERR
                   MOVE CT-PARR-PRO           TO PARRAFO-ERR
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
           MOVE CONT-LEIDOS        TO REG-RECUPERADOS
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
            MOVE TB-ID-PEDIDO       TO  ID-PEDIDO     (CONT-LEIDOS)
            MOVE TB-ID-CLIENTE      TO  ID-CLIENTE    (CONT-LEIDOS)
            MOVE TB-FECHA-PEDIDO    TO  FECHA-PEDIDO  (CONT-LEIDOS)
            MOVE TB-IMPORTE-TOTAL   TO  IMPORTE-TOTAL (CONT-LEIDOS)
            MOVE TB-ESTADO          TO  ESTADO        (CONT-LEIDOS)
            MOVE TB-TIPO-ENVIO      TO  TIPO-ENVIO    (CONT-LEIDOS)
            MOVE TB-COMENTARIOS     TO  COMENTARIOS   (CONT-LEIDOS)
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
           DISPLAY '---------CERRAR CURSOR'
      *
           EXEC SQL
              CLOSE CURSOR_PROD
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CT-99                 TO RETORNO-ERR
                   MOVE CT-DESC-ERR           TO DESCRIPCION-ERR
                   MOVE CT-PARR-PRO           TO PARRAFO-ERR
           END-EVALUATE
      *
           .
       3100-CERRAR-CURSOR-EXIT.
           EXIT.
      *
      ******************************************************************
      *     9000-LEER-CURSOR-PROD                                      *
      ******************************************************************
       9000-LEER-CURSOR.
      *
           DISPLAY '-----------FETCH CURSOR'
      *
           EXEC SQL
              FETCH CURSOR_PROD
               INTO :TB-ID-PEDIDO
                   ,:TB-ID-CLIENTE
                   ,:TB-FECHA-PEDIDO
                   ,:TB-IMPORTE-TOTAL
                   ,:TB-ESTADO
                   ,:TB-TIPO-ENVIO
                   ,:TB-COMENTARIOS
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   PERFORM 2300-INFORMAR-SALIDA
                      THRU 2300-INFORMAR-SALIDA-EXIT
              WHEN 100
                   SET SI-FIN-CURSOR           TO TRUE
              WHEN OTHER
                   MOVE CT-99                 TO RETORNO-ERR
                   MOVE CT-DESC-ERR           TO DESCRIPCION-ERR
                   MOVE CT-PARR-PRO           TO PARRAFO-ERR
      *
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       9000-LEER-CURSOR-EXIT.
           EXIT.
