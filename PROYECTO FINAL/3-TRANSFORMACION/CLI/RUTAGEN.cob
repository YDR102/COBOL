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
          05 CT-10                     PIC X(02) VALUE '10'.
          05 CT-01                     PIC 9(01) VALUE 1.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-99                     PIC X(02) VALUE '99'.
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
           IF NUM-AGE-RUT = CT-00
                MOVE CT-10                           TO COD-RETORNO
                MOVE CT-99                           TO COD-SUBRETORNO
                MOVE 'VALIDACION'                    TO PARRAFO
                MOVE 'NUM-AGE-RUT NO PUEDE SER CERO' TO DESCRIPCION

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
            MOVE '1'           TO NUM-AGE-RUT(INDEX-TB)
            MOVE '42345240X'   TO DNI-AGE-RUT(INDEX-TB)
            MOVE 'JUAN'        TO NOMBRE-AGE-RUT(INDEX-TB)
            MOVE 'PEREZ'       TO APE-1-AGE-RUT(INDEX-TB)
            MOVE 'GARCIA'      TO APE-2-AGE-RUT(INDEX-TB)
            MOVE '1234567895'  TO TLF-AGE-RUT(INDEX-TB)
      *
            MOVE '2'           TO NUM-AGE-RUT(INDEX-TB)
            MOVE '14830157Y'   TO DNI-AGE-RUT(INDEX-TB)
            MOVE 'RAMON'       TO NOMBRE-AGE-RUT(INDEX-TB)
            MOVE 'BOTA'        TO APE-1-AGE-RUT(INDEX-TB)
            MOVE 'FUMEIRO'     TO APE-2-AGE-RUT(INDEX-TB)
            MOVE '1234567895'  TO TLF-AGE-RUT(INDEX-TB)
      *
            MOVE '3'           TO NUM-AGE-RUT(INDEX-TB)
            MOVE '10947269P'   TO DNI-AGE-RUT(INDEX-TB)
            MOVE 'ELBA'        TO NOMBRE-AGE-RUT(INDEX-TB)
            MOVE 'GIJON'       TO APE-1-AGE-RUT(INDEX-TB)
            MOVE 'ROSADO'      TO APE-2-AGE-RUT(INDEX-TB)
            MOVE '1234567895'  TO TLF-AGE-RUT(INDEX-TB)
      *
            MOVE '4'           TO NUM-AGE-RUT(INDEX-TB)
            MOVE '12345678Z'   TO DNI-AGE-RUT(INDEX-TB)
            MOVE 'JULIAN'      TO NOMBRE-AGE-RUT(INDEX-TB)
            MOVE 'GARCIA'      TO APE-1-AGE-RUT(INDEX-TB)
            MOVE 'PEREZ'       TO APE-2-AGE-RUT(INDEX-TB)
            MOVE '1234567895'  TO TLF-AGE-RUT(INDEX-TB)
      *
            MOVE '5'           TO NUM-AGE-RUT(INDEX-TB)
            MOVE '98765432A'   TO DNI-AGE-RUT(INDEX-TB)
            MOVE 'MARIA'       TO NOMBRE-AGE-RUT(INDEX-TB)
            MOVE 'LOPEZ'       TO APE-1-AGE-RUT(INDEX-TB)
            MOVE 'MARTINEZ'    TO APE-2-AGE-RUT(INDEX-TB)
            MOVE '1234567895'  TO TLF-AGE-RUT(INDEX-TB)
      *
            MOVE '6'           TO NUM-AGE-RUT(INDEX-TB)
            MOVE '45678912B'   TO DNI-AGE-RUT(INDEX-TB)
            MOVE 'CARLOS'      TO NOMBRE-AGE-RUT(INDEX-TB)
            MOVE 'GOMEZ'       TO APE-1-AGE-RUT(INDEX-TB)
            MOVE 'HERRERA'     TO APE-2-AGE-RUT(INDEX-TB)
            MOVE '1234567895'  TO TLF-AGE-RUT(INDEX-TB)
      *
            MOVE '7'           TO NUM-AGE-RUT(INDEX-TB)
            MOVE '32165498C'   TO DNI-AGE-RUT(INDEX-TB)
            MOVE 'LAURA'       TO NOMBRE-AGE-RUT(INDEX-TB)
            MOVE 'TORO'        TO APE-1-AGE-RUT(INDEX-TB)
            MOVE 'REYES'       TO APE-2-AGE-RUT(INDEX-TB)
            MOVE '1234567895'  TO TLF-AGE-RUT(INDEX-TB)
      *
            MOVE '8'           TO NUM-AGE-RUT(INDEX-TB)
            MOVE '65432109D'   TO DNI-AGE-RUT(INDEX-TB)
            MOVE 'PEDRO'       TO NOMBRE-AGE-RUT(INDEX-TB)
            MOVE 'ALVAREZ'     TO APE-1-AGE-RUT(INDEX-TB)
            MOVE 'CASTAÑO'     TO APE-2-AGE-RUT(INDEX-TB)
            MOVE '1234567895'  TO TLF-AGE-RUT(INDEX-TB)
      *
            MOVE '9'           TO NUM-AGE-RUT(INDEX-TB)
            MOVE '78912345E'   TO DNI-AGE-RUT(INDEX-TB)
            MOVE 'SARA'        TO NOMBRE-AGE-RUT(INDEX-TB)
            MOVE 'MORENO'      TO APE-1-AGE-RUT(INDEX-TB)
            MOVE 'VILLALBA'    TO APE-2-AGE-RUT(INDEX-TB)
            MOVE '1234567895'  TO TLF-AGE-RUT(INDEX-TB)
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
