      ******************************************************************
      *                     RUTINA O MODULO                            *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   REPOSRUT.
       AUTHOR.       DAVID.
       DATE-WRITTEN. 09/06/2025.
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
       01 CA-CONSTANTES-ALF.
          05 CT-RUT                    PIC X(07) VALUE 'REPOSRUT'.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-10                     PIC X(02) VALUE '10'.
      *
      *---------------SQLCA---------------*
      *
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
      *
      *--------DCLGEN DAREPOS-------------*
      *
           EXEC SQL
              INCLUDE TBDAREPO
           END-EXEC.
      *
      *--------DCLGEN CUENTAS-------------*
      *
           EXEC SQL
              INCLUDE TBCUENTA
           END-EXEC.
      *
      ******************************************************************
      *     L I N K A G E   S E C T I O N                              *
      ******************************************************************
       LINKAGE SECTION.
       COPY RUTREPOS.
      *
      ******************************************************************
      *     PROCEDURE DIVISION                                         *
      ******************************************************************
       PROCEDURE DIVISION USING CPY-RUT.
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
      ** 1000-INICIO                                                  **
      ******************************************************************
      *
       1000-INICIO.
      *
           INITIALIZE CAMPO-ERR
                      DCLCUENTAS
      *
           PERFORM 1200-CONSULTAR-DAREPOS
              THRU 1200-CONSULTAR-DAREPOS-EXIT
      *
           .
      *
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 1200-CONSULTAR-DAREPOS                                       **
      ******************************************************************
      *
       1200-CONSULTAR-DAREPOS.
      *
           MOVE 'REPOS'                  TO TB-NOMBRE-PGM
      *
           EXEC SQL
               SELECT ESTADO
                     ,VALOR_CLAVE
                 INTO :TB-ESTADO
                     ,:TB-VALOR-CLAVE
                 FROM DAREPOS
                WHERE NOMBRE_PGM = :TB-NOMBRE-PGM
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                    EVALUATE TB-ESTADO
                        WHEN 'KO'

                        WHEN 'OK'

                        WHEN OTHER
                             DISPLAY 'ERROR: ESTADO INCORRECTO EN DAREP'
                             DISPLAY 'PARRAFO: 1200-CONSULTAR-DAREPOS'
                             DISPLAY 'TABLA: DAREPOS'
      *
                             PERFORM 3000-FIN
                                THRU 3000-FIN-EXIT
                    END-EVALUATE
               WHEN 100
                    PERFORM 1210-INSERTAR-DAREPOS
                       THRU 1210-INSERTAR-DAREPOS-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR TECNICO EN BBDD'
                    DISPLAY 'PARRAFO: 1200-CONSULTAR-DAREPOS'
                    DISPLAY 'TABLA: DAREPOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       1200-CONSULTAR-DAREPOS-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 1210-INSERTAR-DAREPOS                                        **
      ******************************************************************
      *
       1210-INSERTAR-DAREPOS.
      *
           MOVE 'KO'                        TO TB-ESTADO
           INITIALIZE TB-VALOR-CLAVE
      *
           EXEC SQL
               INSERT INTO DAREPOS
                      (NOMBRE_PGM
                      ,ESTADO
                      ,VALOR_CLAVE)
                      VALUES(
                       :TB-NOMBRE-PGM
                      ,:TB-ESTADO
                      ,:TB-VALOR-CLAVE)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE
               WHEN -803
                    DISPLAY 'ERROR: REG. DUPLICADO EN BBDD'
                    DISPLAY 'PARRAFO: 1210-INSERTAR-DAREPOS'
                    DISPLAY 'TABLA: DAREPOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR TECNICO EN BBDD'
                    DISPLAY 'PARRAFO: 1210-INSERTAR-DAREPOS'
                    DISPLAY 'TABLA: DAREPOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       1210-INSERTAR-DAREPOS-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 2000-PROCESO                                                 **
      ******************************************************************
      *
       2000-PROCESO.
      *
           PERFORM 2200-UPDATE-DAREPOS
              THRU 2200-UPDATE-DAREPOS-EXIT
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 2200-UPDATE-DAREPOS                                          **
      ******************************************************************
      *
       2200-UPDATE-DAREPOS.
      *
           MOVE 'KO'                        TO TB-ESTADO
           MOVE NUM-CUENTA-RUT              TO TB-VALOR-CLAVE-TEXT
           COMPUTE TB-VALOR-CLAVE-LEN =
                   FUNCTION LENGTH(TB-VALOR-CLAVE-TEXT)
      *
           EXEC SQL
               UPDATE DAREPOS
                  SET ESTADO = :TB-ESTADO
                     ,VALOR_CLAVE = :TB-VALOR-CLAVE
                WHERE NOMBRE_PGM = :TB-NOMBRE-PGM
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                    EXEC SQL
                        COMMIT
                    END-EXEC
               WHEN 100
                    DISPLAY 'ERROR: REG. NO ENCONTRADO EN BBDD'
                    DISPLAY 'PARRAFO: 2200-UPDATE-DAREPOS'
                    DISPLAY 'TABLA: EMPLEADOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR TECNICO EN BBDD'
                    DISPLAY 'PARRAFO: 2200-UPDATE-DAREPOS'
                    DISPLAY 'TABLA: EMPLEADOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2200-UPDATE-DAREPOS-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3000-FIN                                                   *
      ******************************************************************
       3000-FIN.
      *
           GOBACK.
      *
       3000-FIN-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 9100-UPDATE-DAREPOS-OK                                       **
      ******************************************************************
      *
       9100-UPDATE-DAREPOS-OK.
      *
           MOVE 'OK'                        TO TB-ESTADO
           INITIALIZE TB-VALOR-CLAVE
      *
           EXEC SQL
               UPDATE DAREPOS
                  SET ESTADO = :TB-ESTADO
                     ,VALOR_CLAVE = :TB-VALOR-CLAVE
                WHERE NOMBRE_PGM = :TB-NOMBRE-PGM
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE
               WHEN 100
                    DISPLAY 'ERROR: REG. NO ENCONTRADO EN BBDD'
                    DISPLAY 'PARRAFO: 9100-UPDATE-DAREPOS-OK'
                    DISPLAY 'TABLA: EMPLEADOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR TECNICO EN BBDD'
                    DISPLAY 'PARRAFO: 9100-UPDATE-DAREPOS-OK'
                    DISPLAY 'TABLA: EMPLEADOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       9100-UPDATE-DAREPOS-OK-EXIT.
           EXIT.
