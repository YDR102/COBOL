      ******************************************************************
      *                     RUTINA O MODULO                            *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   EJRUT02.
       AUTHOR.       DAVID.
       DATE-WRITTEN. 06/05/2025.
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
          05 CT-RUT                    PIC X(07) VALUE 'EJRUT02'.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-10                     PIC X(02) VALUE '10'.
      *
       01 CA-CONSTANTES-NUM.
          05 CN-ANYO                   PIC 9(04) VALUE 2025.
      *
       01 WK-ANYO                      PIC X(04) VALUE SPACES.
       01 WK-ANYO-N REDEFINES WK-ANYO  PIC 9(04).
      *
      ******************************************************************
      *     L I N K A G E   S E C T I O N                              *
      ******************************************************************
       LINKAGE SECTION.
      * -- COPY DE COMUNICACION DE LA RUTINA
       01 CPY-RUT.
          05 RUT-ENTRADA.
             10 TEM-MAX                PIC S9(02).
             10 TEM-MIN                PIC S9(02).
          05 RUT-SALIDA.
             10 RETORNO                PIC X(02).
             10 RESPUESTA              PIC S9(03).
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
      *     1000-INICIO                                                *
      ******************************************************************
       1000-INICIO.
      *
           INITIALIZE RUT-SALIDA
      *
           MOVE CT-00           TO RETORNO
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
      * 1- VALIDAR LA ENTRADA
           IF TEM-MAX = ZEROS OR TEM-MIN = ZEROS

              MOVE CT-10              TO RETORNO
              MOVE ZEROS              TO RESPUESTA

           ELSE
      * 2- SI LA ENTRADA ES CORRECTA INFORMAMOS LA SALIDA
              COMPUTE RESPUESTA = TEM-MAX + TEM-MIN / 2
           END-IF
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
      *
       3000-FIN-EXIT.
           EXIT.
      *
