       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   RUTAGEN.
       AUTHOR.       DAVID.
       DATE-WRITTEN. 20/06/2025.
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
          05 CT-01                     PIC 9(01) VALUE 1.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-99                     PIC X(02) VALUE '99'.
          05 CT-88                     PIC X(02) VALUE '88'.
      *
       01 TABLAS.
           05 SALIDA-OCC                OCCURS 9 TIMES
                                        INDEXED BY INDEX-TB.
                10 NUM-AGE-RUT                PIC X(09).
                10 DNI-AGE-RUT                PIC X(09).
                10 NOMBRE-AGE-RUT             PIC X(25).
                10 APE-1-AGE-RUT              PIC X(25).
                10 APE-2-AGE-RUT              PIC X(25).
                10 TLF-AGE-RUT                PIC X(10).
      *
      ******************************************************************
      *     L I N K A G E   S E C T I O N                              *
      ******************************************************************
       LINKAGE SECTION.
      *
           COPY CPRUTCO.
      ******************************************************************
      *     PROCEDURE DIVISION                                         *
      ******************************************************************
       PROCEDURE DIVISION USING CPRUTCO.
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
                      SALIDA-RUT
      *
           MOVE CT-00              TO COD-RETORNO
           MOVE CT-00              TO COD-SUBRETORNO
           MOVE 'AGENTES'          TO TABLA
           .
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     1100-VALIDAR                                                *
      ******************************************************************
       1100-VALIDAR.
      *
           EVALUATE NUMERO-ALEA
               WHEN 0
                     MOVE CT-99                        TO COD-RETORNO
                     MOVE 'VALIDACION'                 TO PARRAFO
                     MOVE 'NUM ALEA ES 0, CORRIGIENDO' TO DESCRIPCION
                     MOVE 1                            TO NUMERO-ALEA
               WHEN SPACES
                     MOVE CT-88                        TO COD-RETORNO
                     MOVE 'VALIDACION'                 TO PARRAFO
                     MOVE 'NO PUEDE SER VACIO'         TO DESCRIPCION
               WHEN OTHER
                     CONTINUE
                     DISPLAY 'NUMERO ALEATORIO RUT: ' NUMERO-ALEA
           END-EVALUATE
           .
      *
       1100-VALIDAR-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2000-PROCESO                                               *
      ******************************************************************
       2000-PROCESO.
      *
               PERFORM 2200-INFORMAR-TABLA
                  THRU 2200-INFORMAR-TABLA-EXIT
      *
               PERFORM 2300-INFORMAR-SALIDA
                  THRU 2300-INFORMAR-SALIDA-EXIT
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2200-INFORMAR-TABLA                                        *
      ******************************************************************
       2200-INFORMAR-TABLA.
      *

            MOVE '1'           TO NUM-AGE-RUT(1)
            MOVE '42345240X'   TO DNI-AGE-RUT(1)
            MOVE 'JUAN'        TO NOMBRE-AGE-RUT(1)
            MOVE 'PEREZ'       TO APE-1-AGE-RUT(1)
            MOVE 'GARCIA'      TO APE-2-AGE-RUT(1)
            MOVE '1234567895'  TO TLF-AGE-RUT(1)
      *
            MOVE '2'           TO NUM-AGE-RUT(2)
            MOVE '14830157Y'   TO DNI-AGE-RUT(2)
            MOVE 'RAMON'       TO NOMBRE-AGE-RUT(2)
            MOVE 'BOTA'        TO APE-1-AGE-RUT(2)
            MOVE 'FUMEIRO'     TO APE-2-AGE-RUT(2)
            MOVE '1234567895'  TO TLF-AGE-RUT(2)
      *
            MOVE '3'           TO NUM-AGE-RUT(3)
            MOVE '10947269P'   TO DNI-AGE-RUT(3)
            MOVE 'ELBA'        TO NOMBRE-AGE-RUT(3)
            MOVE 'GIJON'       TO APE-1-AGE-RUT(3)
            MOVE 'ROSADO'      TO APE-2-AGE-RUT(3)
            MOVE '1234567895'  TO TLF-AGE-RUT(3)
      *
            MOVE '4'           TO NUM-AGE-RUT(4)
            MOVE '12345678Z'   TO DNI-AGE-RUT(4)
            MOVE 'JULIAN'      TO NOMBRE-AGE-RUT(4)
            MOVE 'GARCIA'      TO APE-1-AGE-RUT(4)
            MOVE 'PEREZ'       TO APE-2-AGE-RUT(4)
            MOVE '1234567895'  TO TLF-AGE-RUT(4)
      *
            MOVE '5'           TO NUM-AGE-RUT(5)
            MOVE '98765432A'   TO DNI-AGE-RUT(5)
            MOVE 'MARIA'       TO NOMBRE-AGE-RUT(5)
            MOVE 'LOPEZ'       TO APE-1-AGE-RUT(5)
            MOVE 'MARTINEZ'    TO APE-2-AGE-RUT(5)
            MOVE '1234567895'  TO TLF-AGE-RUT(5)
      *
            MOVE '6'           TO NUM-AGE-RUT(6)
            MOVE '45678912B'   TO DNI-AGE-RUT(6)
            MOVE 'CARLOS'      TO NOMBRE-AGE-RUT(6)
            MOVE 'GOMEZ'       TO APE-1-AGE-RUT(6)
            MOVE 'HERRERA'     TO APE-2-AGE-RUT(6)
            MOVE '1234567895'  TO TLF-AGE-RUT(6)
      *
            MOVE '7'           TO NUM-AGE-RUT(7)
            MOVE '32165498C'   TO DNI-AGE-RUT(7)
            MOVE 'LAURA'       TO NOMBRE-AGE-RUT(7)
            MOVE 'TORO'        TO APE-1-AGE-RUT(7)
            MOVE 'REYES'       TO APE-2-AGE-RUT(7)
            MOVE '1234567895'  TO TLF-AGE-RUT(7)
      *
            MOVE '8'           TO NUM-AGE-RUT(8)
            MOVE '65432109D'   TO DNI-AGE-RUT(8)
            MOVE 'PEDRO'       TO NOMBRE-AGE-RUT(8)
            MOVE 'ALVAREZ'     TO APE-1-AGE-RUT(8)
            MOVE 'CASTELLANO'  TO APE-2-AGE-RUT(8)
            MOVE '1234567895'  TO TLF-AGE-RUT(8)
      *
            MOVE '9'           TO NUM-AGE-RUT(9)
            MOVE '78912345E'   TO DNI-AGE-RUT(9)
            MOVE 'SARA'        TO NOMBRE-AGE-RUT(9)
            MOVE 'MORENO'      TO APE-1-AGE-RUT(9)
            MOVE 'VILLALBA'    TO APE-2-AGE-RUT(9)
            MOVE '1234567895'  TO TLF-AGE-RUT(9)
      *
           .
      *
       2200-INFORMAR-TABLA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2300-INFORMAR-SALIDA                                       *
      ******************************************************************
       2300-INFORMAR-SALIDA.
      *
            MOVE NUM-AGE-RUT    (NUMERO-ALEA) TO NUM-RUT
            MOVE DNI-AGE-RUT    (NUMERO-ALEA) TO DNI-RUT
            MOVE NOMBRE-AGE-RUT (NUMERO-ALEA) TO NOMBRE-RUT
            MOVE APE-1-AGE-RUT  (NUMERO-ALEA) TO APE-1-RUT
            MOVE APE-2-AGE-RUT  (NUMERO-ALEA) TO APE-2-RUT
            MOVE TLF-AGE-RUT    (NUMERO-ALEA) TO TLF-RUT
      *
            DISPLAY 'NUMERO AGR: ' NUM-RUT
            DISPLAY 'DNI AGR: ' DNI-RUT
            DISPLAY 'NOMBRE AGR: ' NOMBRE-RUT
            DISPLAY 'APELLIDO 1 AGR: ' APE-1-RUT
            DISPLAY 'APELLIDO 2 AGR: ' APE-2-RUT
            DISPLAY 'TELEFONO AGR: ' TLF-RUT
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
           GOBACK.
       3000-FIN-EXIT.
           EXIT.
