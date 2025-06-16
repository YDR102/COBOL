      ******************************************************************
      *                    E  X  T  R  C  L  I                         *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   RUTCRUD3.
       AUTHOR.       DAVID.
       DATE-WRITTEN. 04/04/2025.
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
          05 CA-0                      PIC 9(01) VALUE 0.
          05 CA-00                     PIC X(02) VALUE '00'.
          05 CA-99                     PIC X(02) VALUE '99'.
          05 CA-88                     PIC X(02) VALUE '88'.
          05 CA-77                     PIC X(02) VALUE '77'.
          05 CA-66                     PIC X(02) VALUE '66'.
          05 CA-55                     PIC X(02) VALUE '55'.
          05 CA-44                     PIC X(02) VALUE '44'.
      *
       01 SW-SWITCHES.
          05 SW-FIN-CURSOR             PIC X(01).
             88 SI-FIN-CURSOR          VALUE 'S'.
             88 NO-FIN-CURSOR          VALUE 'N'.
      *---------------- SQLCA ------------------------
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *---------------- DCLGEN -----------------------
           EXEC SQL
               INCLUDE TBEMPLE
           END-EXEC.
      *
      ******************************************************************
      *     L I N K A G E   S E C T I O N                              *
      ******************************************************************
       LINKAGE SECTION.
      *ESTRUCTURA DE COMUNICACION DE MI RUTINA
       COPY CRUDCPY3.
      *
      ******************************************************************
      *     PROCEDURE DIVISION                                         *
      ******************************************************************
       PROCEDURE DIVISION USING CRUDCPY3.
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
           INITIALIZE ERRORES-3
                      DCLEMPLEADOS
      *
           MOVE CA-00              TO RETORNO-ERR-3
           MOVE SPACES             TO DESCRIPCION-ERR-3
           MOVE SPACES             TO PARRAFO-ERR-3
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
           MOVE MATRICULA-3        TO TB-MATRICULA
           EXEC SQL
               DELETE FROM EMPLEADOS
               WHERE MATRICULA = :TB-MATRICULA
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                   DISPLAY 'Delete OK'
              WHEN 100
                   MOVE CA-88                  TO RETORNO-ERR-3
                   MOVE '2000-PROCESO'         TO PARRAFO-ERR-3
                   MOVE 'Delete vacio'         TO DESCRIPCION-ERR-3
                   MOVE SQLCODE                TO SQLCODE-ERR-3

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
              WHEN -803
                   MOVE CA-77                  TO RETORNO-ERR-3
                   MOVE '2000-PROCESO'         TO PARRAFO-ERR-3
                   MOVE 'Delete duplicado'     TO DESCRIPCION-ERR-3
                   MOVE SQLCODE                TO SQLCODE-ERR-3

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
              WHEN -305
                   MOVE CA-66                  TO RETORNO-ERR-3
                   MOVE '2000-PROCESO'         TO PARRAFO-ERR-3
                   MOVE 'Delete nulo'          TO DESCRIPCION-ERR-3
                   MOVE SQLCODE                TO SQLCODE-ERR-3

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
              WHEN OTHER
                   MOVE CA-44                  TO RETORNO-ERR-3
                   MOVE '2000-PROCESO'         TO PARRAFO-ERR-3
                   MOVE 'Delete'               TO DESCRIPCION-ERR-3
                   MOVE SQLCODE                TO SQLCODE-ERR-3

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2000-PROCESO-EXIT.
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