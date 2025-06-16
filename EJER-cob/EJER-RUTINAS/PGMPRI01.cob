      ******************************************************************
      **   E J P G M 0 1 .- PGM QUE RECIBE POR SYSIN DOS NUMEROS, A   **
      **                    CONTINUACION LLAMA A LA RUTINA EJRUT01    **
      **                    PARA COMPROBAR SI NUM1 ES MULTIPLO DE NUM2**
      **                    O NO.                                     **
      ******************************************************************
      *
      ******************************************************************
      * IDENTIFICATION DIVISION                                        *
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. EJPGM01.
       AUTHOR. DAVID.
       DATE-WRITTEN. 07/05/2025.
      *
      ******************************************************************
      * ENVIRONMENT DIVISION                                           *
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
      * DATA DIVISION                                                  *
      ******************************************************************
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
      *
       01  VN-VARIABLES-NUMERICAS.
           05  NUM1-SYSIN                  PIC 9(03).
           05  NUM2-SYSIN                  PIC 9(03).
      *
       01  CA-CONSTANTES.
           05  CT-RUTINA                   PIC X(07) VALUE 'EJRUT01'.
           05  CT-00                       PIC X(07) VALUE '00'.
           05  CT-10                       PIC X(07) VALUE '10'.
           05  CT-1                        PIC X(07) VALUE 1.
      *
       01 CPY-CPYRUT01.
           05 ENTRADA-RUT.
                10 NUM1-E                  PIC 9(03).
                10 NUM2-E                  PIC 9(03).
           05 SALIDA-RUT.
                10 IND-MULTIPLO-S          PIC X(01).
           05 ERRORES-RUT.
                10 RETORNO-ERR             PIC X(02).
                10 SUBRETORNO-ERR          PIC X(02).
                10 DESCRIPCION-ERR         PIC X(40).
                10 CAMPO-ERR               PIC X(20).
                10 PARRAFO-ERR             PIC X(40).
      *
      ******************************************************************
      * PROCEDURE DIVISION                                             *
      ******************************************************************
      *
       PROCEDURE DIVISION.
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
      * 1000-INICIO                                                    *
      * INICIALIZAMOS LOS CAMPOS DE TRABAJO Y DEFINIMOS LOS ACCEPTS    *
      * PARA RECOGER LOS NUMEROS POR SYSIN.                            *
      ******************************************************************
      *
       1000-INICIO.
      *
           INITIALIZE CPY-CPYRUT01
                      VN-VARIABLES-NUMERICAS
      *
           ACCEPT NUM1-SYSIN        FROM SYSIN
           ACCEPT NUM2-SYSIN        FROM SYSIN
      *
           .
      *
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2000-PROCESO                                                   *
      * REALIZAMOS LA LLAMADA A LA RUTINA EJRUT01 PASANDOLE LOS NUM1   *
      * Y NUM2 RECIBIDOS POR SYSIN, CON EL FIN DE VALIDAR SI NUM1 ES   *
      * MULTIPLO DE NUM2 O NO.                                         *
      ******************************************************************
      *
       2000-PROCESO.
      *
           MOVE NUM1-SYSIN              TO NUM1-E
           MOVE NUM2-SYSIN              TO NUM2-E
      *
           CALL CT-RUTINA            USING CPY-CPYRUT01
      *
           EVALUATE RETORNO-ERR
               WHEN CT-00
                    IF IND-MULTIPLO-S = 'S'
                       DISPLAY 'EL NUMERO ' NUM1-SYSIN ' ES MULTIPLO DE'
                               ' ' NUM2-SYSIN
                    ELSE
                       DISPLAY 'EL NUMERO ' NUM1-SYSIN ' NO ES MULTIPLO'
                               ' DE ' NUM2-SYSIN
                    END-IF

      *
               WHEN OTHER
                    DISPLAY 'RETORNO:         ' RETORNO-ERR
                    DISPLAY 'SUBRETORNO:      ' SUBRETORNO-ERR
                    DISPLAY 'DESCRIPCION-ERR: ' DESCRIPCION-ERR
                    DISPLAY 'ELEMENTO-ERR:    ' CAMPO-ERR
                    DISPLAY 'PARRAFO-ERR:     ' PARRAFO-ERR
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3000-FIN                                                       *
      * FINALIZAMOS LA EJECUCION DEL PROCESO CON EL STOP RUN.          *
      ******************************************************************
      *
       3000-FIN.
      *
           STOP RUN
      *
           .
      *
       3000-FIN-EXIT.
           EXIT.
      *
