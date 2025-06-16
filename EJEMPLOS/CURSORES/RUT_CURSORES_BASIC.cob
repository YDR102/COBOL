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
       01 CA-CONSTANTES-NUM.
          05 CA-0                      PIC 9(01) VALUE 0.
       01 CA-CONSTANTES-ALF.
          05 CA-PGM                    PIC X(07) VALUE 'PGMCURS'.
          05 CA-L                      PIC X(01) VALUE 'L'.
          05 CA-R                      PIC X(01) VALUE 'R'.
          05 CA-S                      PIC X(01) VALUE 'S'.
          05 CA-00                     PIC X(02) VALUE '00'.
          05 CA-99                     PIC X(02) VALUE '99'.
          05 CA-PARR-PRO    PIC X(12) VALUE '2000-PROCESO'.
          05 CA-DESC-ERR    PIC X(12) VALUE 'ERROR SELECT'.
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
               INCLUDE TBPRODU
           END-EXEC.
      *-------------- DEFINIMOS LOS CURSORES ---------
           EXEC SQL
               DECLARE CURSOR-PROD CURSOR FOR
                  SELECT ID_PRODUCTO
                        ,NOMBRE
                        ,CATEGORIA
                        ,PRECIO
                        ,FECHA_ALTA
                        ,STOCK
                        ,DESCRIPCION
                    FROM PRODUCTOS
                    ORDER BY ID_PRODUCTO
           END-EXEC.
      *
      ******************************************************************
      *     L I N K A G E   S E C T I O N                              *
      ******************************************************************
       LINKAGE SECTION.
      *ESTRUCTURA DE COMUNICACION DE MI RUTINA
       COPY CPYCURS.
      *
      ******************************************************************
      *     PROCEDURE DIVISION                                         *
      ******************************************************************
       PROCEDURE DIVISION USING CPYCURS.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
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
                      DCLPRODUCTOS
      *
           MOVE CA-00              TO COD-RETORNO
           MOVE CA-00              TO COD-SUBRETORNO
           MOVE CA-0               TO CONT-LEIDOS
      *
           SET NO-FIN-CURSOR       TO TRUE
      *
           .
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2000-PROCESO                                               *
      ******************************************************************
       2000-PROCESO.
      *
           PERFORM 2100-ABRIR-CURSOR
              THRU 2100-ABRIR-CURSOR-EXIT
      *
           PERFORM 2200-LEER-CURSOR
              THRU 2200-LEER-CURSOR-EXIT
             UNTIL SI-FIN-CURSOR
      *
           MOVE CONT-LEIDOS    TO SAL-LEIDOS
      *
           PERFORM 2300-CERRAR-CURSOR
              THRU 2300-CERRAR-CURSOR-EXIT
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2100-ABRIR-CURSOR                                          *
      ******************************************************************
       2100-ABRIR-CURSOR.
      *
           SET NO-FIN-CURSOR TO TRUE
      *
           DISPLAY '-------------OPEN CURSOR'
           EXEC SQL
               OPEN CURSOR-PROD
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                  TO COD-RETORNO
                   MOVE CA-99                  TO COD-SUBRETORNO
                   MOVE 'OPEN      '           TO PARRAFO
                   MOVE 'PRODUCTOS'            TO TABLA
                   MOVE '2100-ABRIR-CURSOR'    TO DESCRIPCION
                   MOVE SQLCODE                TO SQLCODE-E

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2100-ABRIR-CURSOR-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2200-LEER-CURSOR-PROD                                      *
      ******************************************************************
       2200-LEER-CURSOR.
      *
           DISPLAY '-----------FETCH CURSOR'
      *
           EXEC SQL
              FETCH CURSOR-PROD
               INTO :TB-ID-PRODUCTO
                   ,:TB-NOMBRE
                   ,:TB-CATEGORIA
                   ,:TB-PRECIO
                   ,:TB-FECHA-ALTA
                   ,:TB-STOCK
                   ,:TB-DESCRIPCION
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   PERFORM 2250-INFORMAR-SALIDA
                      THRU 2250-INFORMAR-SALIDA-EXIT
              WHEN 100
                   SET SI-FIN-CURSOR           TO TRUE
              WHEN OTHER
                   MOVE CA-99                  TO COD-RETORNO
                   MOVE CA-99                  TO COD-SUBRETORNO
                   MOVE 'FETCH     '           TO PARRAFO
                   MOVE 'PRODUCTOS          '  TO TABLA
                   MOVE '2200-LEER-CURSOR '    TO DESCRIPCION
                   MOVE SQLCODE                TO SQLCODE-E

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2200-LEER-CURSOR-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2300-CERRAR-CURSOR                                         *
      ******************************************************************
       2300-CERRAR-CURSOR.
      *
           DISPLAY '---------CERRAR CURSOR'
      *
           EXEC SQL
              CLOSE CURSOR-PROD
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                  TO COD-RETORNO
                   MOVE CA-99                  TO COD-SUBRETORNO
                   MOVE 'CLOSE     '           TO PARRAFO
                   MOVE 'PRODUCTOS          '  TO TABLA
                   MOVE '2300-CERRAR-CURSOR'   TO DESCRIPCION
                   MOVE SQLCODE                TO SQLCODE-E

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2300-CERRAR-CURSOR-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2250-INFORMAR-SALIDA                                       *
      ******************************************************************
       2250-INFORMAR-SALIDA.
      *
            ADD 1                   TO CONT-LEIDOS
            DISPLAY '*****************************************'
            DISPLAY 'REGISTRO NUM: 'CONT-LEIDOS
            DISPLAY '*****************************************'
            DISPLAY 'ID-PROD: '    TB-ID-PRODUCTO
            DISPLAY 'NOMBRE: '     TB-NOMBRE
            DISPLAY 'CATEGORIA: '  TB-CATEGORIA
            DISPLAY 'PRECIO: '     TB-PRECIO
            DISPLAY 'FECHA-ALTA: ' TB-FECHA-ALTA
            DISPLAY 'STOCK: '      TB-STOCK
            DISPLAY 'DESCRIPCION: 'TB-DESCRIPCION
            DISPLAY '*****************************************'
      *
      *
           .
       2250-INFORMAR-SALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3000-FIN                                                   *
      ******************************************************************
       3000-FIN.
      *
           GOBACK.
       3000-FIN-EXIT.
           EXIT.
      *
