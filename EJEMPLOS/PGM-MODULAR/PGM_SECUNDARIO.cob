      ******************************************************************
      ** E J R U T 0 1.- RUTINA QUE RECIBE DOS NUMEROS Y DEVUELVE SI  **
      **                 EL PRIMERO ES MULTIPLO DEL SEGUNDO O NO      **
      ******************************************************************
      *
      ******************************************************************
      ** IDENTIFICATION DIVISION                                      **
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. EJRUT01.
       AUTHOR. DAVID.
       DATE-WRITTEN. 07/05/2025.
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
       01  VN-VARIABLES.
           05  VR-RESULTADO           PIC 9(03).
           05  VR-RESTO               PIC 9(03).
      *
       LINKAGE SECTION.
      *
      *COPY DE LA RUTINA EJRUT01
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
      ******************************************************************
      ** PROCEDURE DIVISION                                           **
      ******************************************************************
      *
       PROCEDURE DIVISION USING CPY-CPYRUT01.
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
           MOVE '00'                     TO RETORNO-ERR
           MOVE '00'                     TO SUBRETORNO-ERR
      *
           IF NUM1-E   = ZEROES
              MOVE '99'                  TO RETORNO-ERR
              MOVE '99'                  TO SUBRETORNO-ERR
              MOVE '1000-INICIO'         TO PARRAFO-ERR
              MOVE 'EL NUM1 NO PUEDE VALER 0'
                                         TO DESCRIPCION-ERR
              MOVE 'NUM1-E'              TO CAMPO-ERR
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF NUM2-E   = ZEROES
              MOVE '99'                  TO RETORNO-ERR
              MOVE '99'                  TO SUBRETORNO-ERR
              MOVE '1000-INICIO'         TO PARRAFO-ERR
              MOVE 'EL NUM2 NO PUEDE VALER 0'
                                         TO DESCRIPCION-ERR
              MOVE 'NUM2-E'              TO CAMPO-ERR
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           .
      *
       1000-INICIO-EXIT.
           EXIT.
      *
      *
      ******************************************************************
      ** 2000-PROCESO                                                 **
      ** CALCULAR EL RESTO DE LA DIVISION DE NUM1 ENTRE NUM2, PARA    **
      ** COMPROBAR SI NUM1 ES MULTIPLO DE NUM2 O NO.   .             **
      ******************************************************************
      *
       2000-PROCESO.
      *
           DIVIDE NUM2-E INTO NUM1-E GIVING VR-RESULTADO
           REMAINDER VR-RESTO
      *
           IF VR-RESTO = ZEROS
              MOVE 'S'                      TO IND-MULTIPLO-S
           ELSE
              MOVE 'N'                      TO IND-MULTIPLO-S
           END-IF
      *
           .
      *
       2000-PROCESO-EXIT.
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
