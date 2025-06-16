      ******************************************************************
      *                   PROGRAMA PRINCIPAL                           *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   EJPGM01.
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
          05 CT-PGM                    PIC X(07) VALUE 'EJPGM01'.
          05 CT-RUT                    PIC X(07) VALUE 'EJRUT01'.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-10                     PIC X(02) VALUE '10'.
      *
       01  VR-AREA                PIC X(04).
      *
      * -- COPY DE COMUNICACION CON LA RUTINA
       01 CPY-RUT.
          05 RUT-ENTRADA.
             10 N-1                    PIC 9(02) VALUE ZEROS.
             10 N-2                    PIC 9(02) VALUE ZEROS.
          05 RUT-SALIDA.
             10 RETORNO                PIC X(02) VALUE SPACES.
             10 RESPUESTA              PIC X(99) VALUE SPACES.
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
      *
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT.
      *
      ******************************************************************
      *     1000-INICIO                                                *
      ******************************************************************
       1000-INICIO.
      *
           INITIALIZE CPY-RUT
      *
           .
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2000-PROCESO                                               *
      ******************************************************************
       2000-PROCESO.
      * -- PARA LLAMAR A RUTINA LOS PASOS SON:
      * 1- INICIALIZAR COPY DE COMUNICACION
           INITIALIZE CPY-RUT
      * 2- INFORMAR LA ENTRADA DE LA COPY DE COMUNICACION
           ACCEPT VR-AREA           FROM SYSIN
           MOVE VR-AREA(1:2) TO N-1
           MOVE VR-AREA(3:2) TO N-2
           DISPLAY 'N-1:'N-1
           DISPLAY 'N-2:'N-2
      * 3- INVOCAR A LA RUTINA
           CALL CT-RUT USING CPY-RUT
      * 4- EVALUAR EL RETORNO
           EVALUATE RETORNO
               WHEN CT-00
                    DISPLAY RESPUESTA
               WHEN CT-10
                    DISPLAY 'ERROR: FALTAN DATOS'
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR DESCONOCIDO'
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
           STOP RUN.
      *
       3000-FIN-EXIT.
           EXIT.
      *
