      ******************************************************************
      *                     RUTINA O MODULO                            *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   EJRUT03.
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
          05 CT-RUT                    PIC X(07) VALUE 'EJRUT03'.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-10                     PIC X(02) VALUE '10'.
          05 CT-66                     PIC X(02) VALUE '66'.
          05 CT-77                     PIC X(02) VALUE '77'.
          05 CT-88                     PIC X(02) VALUE '88'.
      *
      ******************************************************************
      *     L I N K A G E   S E C T I O N                              *
      ******************************************************************
       LINKAGE SECTION.
      * -- COPY DE COMUNICACION DE LA RUTINA
       01 CPY-RUT.
          05 RUT-ENTRADA.
             10 FECHA                  PIC 9(10).
          05 RUT-SALIDA.
             10 RETORNO                PIC X(02).
             10 RESPUESTA              PIC X(30).
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
           IF FECHA = SPACES
                MOVE 'FECHA SIN INFORMAR'            TO RESPUESTA
                MOVE CT-10                           TO RETORNO
           END-IF
           EVALUATE FECHA(6:2)
                WHEN '01'
                     IF FECHA(8:2) >= 1 AND FECHA(8:2) <= 31
                          CONTINUE
                     ELSE
                          MOVE 'DIA INCORRECTO'      TO RESPUESTA
                          MOVE CT-66                 TO RETORNO
                     END-IF
                WHEN '02'
                     IF FECHA(8:2) >= 1 AND FECHA(8:2) <= 28
                          CONTINUE
                     ELSE
                          MOVE 'DIA INCORRECTO'      TO RESPUESTA
                          MOVE CT-66                 TO RETORNO
                     END-IF
                WHEN '03'
                     IF FECHA(8:2) >= 1 AND FECHA(8:2) <= 30
                          CONTINUE
                     ELSE
                          MOVE 'DIA INCORRECTO'      TO RESPUESTA
                          MOVE CT-66                 TO RETORNO
                     END-IF
                WHEN '04'
                     IF FECHA(8:2) >= 1 AND FECHA(8:2) <= 31
                          CONTINUE
                     ELSE
                          MOVE 'DIA INCORRECTO'      TO RESPUESTA
                          MOVE CT-66                 TO RETORNO
                     END-IF
                WHEN '05'
                     IF FECHA(8:2) >= 1 AND FECHA(8:2) <= 30
                          CONTINUE
                     ELSE
                          MOVE 'DIA INCORRECTO'      TO RESPUESTA
                          MOVE CT-66                 TO RETORNO
                     END-IF
                WHEN '06'
                     IF FECHA(8:2) >= 1 AND FECHA(8:2) <= 31
                          CONTINUE
                     ELSE
                          MOVE 'DIA INCORRECTO'      TO RESPUESTA
                          MOVE CT-66                 TO RETORNO
                     END-IF
                WHEN '07'
                     IF FECHA(8:2) >= 1 AND FECHA(8:2) <= 30
                          CONTINUE
                     ELSE
                          MOVE 'DIA INCORRECTO'      TO RESPUESTA
                          MOVE CT-66                 TO RETORNO
                     END-IF
                WHEN '08'
                     IF FECHA(8:2) >= 1 AND FECHA(8:2) <= 31
                          CONTINUE
                     ELSE
                          MOVE 'DIA INCORRECTO'      TO RESPUESTA
                          MOVE CT-66                 TO RETORNO
                     END-IF
                WHEN '09'
                     IF FECHA(8:2) >= 1 AND FECHA(8:2) <= 30
                          CONTINUE
                     ELSE
                          MOVE 'DIA INCORRECTO'      TO RESPUESTA
                          MOVE CT-66                 TO RETORNO
                     END-IF
                WHEN '10'
                     IF FECHA(8:2) >= 1 AND FECHA(8:2) <= 31
                          CONTINUE
                     ELSE
                          MOVE 'DIA INCORRECTO'      TO RESPUESTA
                          MOVE CT-66                 TO RETORNO
                     END-IF
                WHEN '11'
                     IF FECHA(8:2) >= 1 AND FECHA(8:2) <= 30
                          CONTINUE
                     ELSE
                          MOVE 'DIA INCORRECTO'      TO RESPUESTA
                          MOVE CT-66                 TO RETORNO
                     END-IF
                WHEN '12'
                     IF FECHA(8:2) >= 1 AND FECHA(8:2) <= 31
                          CONTINUE
                     ELSE
                          MOVE 'DIA INCORRECTO'      TO RESPUESTA
                          MOVE CT-66                 TO RETORNO
                     END-IF
           END-EVALUATE
           IF FECHA(6:2) <= 12
                MOVE 'MES INCORRECTO'                TO RESPUESTA
                MOVE CT-77                           TO RETORNO
           END-IF
           IF FECHA(1:4) <= 1900
                MOVE 'ANNO INCORRECTO'               TO RESPUESTA
                MOVE CT-88                           TO RETORNO
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
