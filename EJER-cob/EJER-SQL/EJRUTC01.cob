      ******************************************************************
      ** E J R U T C 0 1.- RUTINA QUE DEVUELVE EL NOMBRE DE LA PROVIN-**
      **                   CIA A PARTIR DEL PREFIJO DE ENTRADA.       **
      ******************************************************************
      *
      ******************************************************************
      ** IDENTIFICATION DIVISION                                      **
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. EJRUTC01.
       AUTHOR. CRISTIAN.
       INSTALLATION. EOI.
       DATE-WRITTEN. 23/05/2025.
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
       01 CA-CONSTANTES.
          05 CT-10                     PIC X(02) VALUE '10'.
          05 CT-01                     PIC X(02) VALUE '01'.
          05 CT-1                     PIC 9(01) VALUE 1.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-88                     PIC X(02) VALUE '88'.
          05 CT-99                     PIC X(02) VALUE '99'.
      *
       01  WK-SQLCODE                   PIC -999.
      *
      *---------------SQLCA---------------*
      *
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
      *
      *--------DCLGEN PROVINCIAS----------*
      *
           EXEC SQL
              INCLUDE TBPROVIN
           END-EXEC.
      *
       LINKAGE SECTION.
      *
      *COPY DE LA RUTINA EJRUTC01
      *
       COPY EJCPYC01.
      ******************************************************************
      ** PROCEDURE DIVISION                                           **
      ******************************************************************
      *
       PROCEDURE DIVISION USING EJCPYC01.
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
      ******************************************************************
      *
       1000-INICIO.
      *
           INITIALIZE SALIDA-RUT
                      ERRORES-RUT
      *
           MOVE CT-00                     TO RETORNO-ERR
           MOVE CT-00                     TO SUBRETORNO-ERR
      *
           IF PREFIJO-E-RUT = SPACES OR LOW-VALUES
              MOVE CT-88                  TO RETORNO-ERR
              MOVE CT-01                  TO SUBRETORNO-ERR
              MOVE SQLCODE                TO SQLCODE-ERR
              MOVE 'PREFIJO NO INFORMADO'
                                          TO DESCRIPCION-ERR
              MOVE 'PREFIJO-E'            TO CAMPO-ERR
              MOVE '1000-INICIO'          TO PARRAFO-ERR
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           .
      *
       1000-INICIO-EXIT.
           EXIT.
      ******************************************************************
      ** 2000-PROCESO                                                 **
      ** REALIZAMOS LA SELECT CON EL PREJIJO PARA OBTENER SU PROVINCIA**
      ** ASOCIADA.                                                    **
      ******************************************************************
      *
       2000-PROCESO.
      *
           PERFORM 2100-OBTENER-PROVIN
              THRU 2100-OBTENER-PROVIN-EXIT
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 2100-OBTENER-PROVIN                                          **
      ** REALIZAMOS LA SELECT CON EL PREJIJO PARA OBTENER SU PROVINCIA**
      ** ASOCIADA.                                                    **
      ******************************************************************
      *
       2100-OBTENER-PROVIN.
      *
           DISPLAY 'SELECT TABLA PROVINCIAS'
      *
           MOVE PREFIJO-E-RUT           TO TB-PREFIJO
      *
           EXEC SQL
               SELECT PROVINCIA
                 INTO :TB-PROVINCIA
                 FROM PROVINCIAS
                 WHERE PREFIJO = :TB-PREFIJO
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                    MOVE TB-PROVINCIA   TO PROVINCIA-S-RUT
               WHEN 100
                    MOVE CT-88          TO RETORNO-ERR
                    MOVE CT-88          TO SUBRETORNO-ERR
                    MOVE SQLCODE        TO SQLCODE-ERR
                    MOVE 'EL PREFIJO NO EXISTE EN LA TABLA PROVINCIAS'
                                        TO DESCRIPCION-ERR
                    MOVE 'PROVINCIAS'
                                        TO CAMPO-ERR
                    MOVE '2100-OBTENER-PROVIN'
                                        TO PARRAFO-ERR
                    MOVE SQLCODE        TO SQLCODE-ERR
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
               WHEN OTHER
                    MOVE CT-99          TO RETORNO-ERR
                    MOVE CT-99          TO SUBRETORNO-ERR
                    MOVE 'ERROR TECNICO EN SELECT A TABLA PROVINCIAS'
                                        TO DESCRIPCION-ERR
                    MOVE 'PROVINCIAS'
                                        TO CAMPO-ERR
                    MOVE '2100-OBTENER-PROVIN'
                                        TO PARRAFO-ERR
                    MOVE SQLCODE        TO SQLCODE-ERR
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2100-OBTENER-PROVIN-EXIT.
           EXIT.
      *
      *
      ******************************************************************
      ** 3000-FIN                                                     **
      ** FINALIZAMOS EL PGM.                                          **
      ******************************************************************
      *
       3000-FIN.
      *
           GOBACK
      *
           .
      *
       3000-FIN-EXIT.
           EXIT.
      *
