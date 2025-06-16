      *****************************************************************
      *       PGM COBOL DE EJEMPLO - ESQUELETO                         *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      *                                                                *
      * SE DEFINE EL NOMBRE DEL PROGRAMA FUENTE                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   RUTMITAB.
       AUTHOR.       DAVID.
       DATE-WRITTEN. 30/05/2022.
       DATE-COMPILED.
      *
      ******************************************************************
      *     ENVIRONMENT DIVISION                                       *
      *                                                                *
      * DEFINIR FICHEROS QUE VAN A SER USADOS DURANTE EL PROGRAMA      *
      * RELACIONAR NOMBRES LOGICOS DE LOS ARCHIVOS POR EL PROGRAMA     *
      * CON LOS NOMBRES REALES DEL JCL                                 *
      * DOS DIVISIONES:                                                *
      *    - CONFIGURATION SECTION                                     *
      *      SE DECLARA EL NOMBRE DE LOS PROCESADORES DONDE SE COMPILA *
      *      Y EJECUTA EL PROGRAMA(NO ES OBLIGATORIO)                  *
      *    - INPUT-OUTPUT SECTION                                      *
      *      SE DECLARAN DISPOSITIVOS PERIFERICOS (IMPRESORAS,...)     *
      *      TIENE FILE CONTROL QUE ES OBLIGATORIO CUANDO SE USAN      *
      *      FICHEROS EN EL PROGRAMA, YA QUE LOS DESCRIBE.             *
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
      * SE DECLARAN LOS FICHEROS DE ENTRADA Y SALIDA
      *
       FILE-CONTROL.
      * OBLIGATORIO, PARA DECLARAR LOS FICHEROS DE ENTRADA Y SALIDA
      *
      ******************************************************************
      *     DATA DIVISION                                              *
      *IDENTIFICA TODOS LOS NOMBRES DE DATOS UTILIZADOS EN EL PROGRAMA *
      *DIVISIONES:                                                     *
      *   - FILE SECTION                                               *
      *     DESCRIBE FICHEROS DE ENTRADA Y SALIDA QUE SE USAN EN EL    *
      *     PROGRAMA                                                   *
      *   - WORKING STORAGE SECTION                                    *
      *     DEFINE CAMPOS DE TRABAJO NECESARIOS.                       *
      *   - LINKAGE SECTION                                            *
      *          SE USA CUANDO EL PROGRAMA SEA UNA RUTINA, YA QUE ES   *
      *     LA ZONA QUE SIRVE PARA COMUNICARSE CON EL PROGRAMA QUE     *
      *     LA INVOCA                                                  *
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
      * LUGAR DONDE DECLARAMOS CONSTANTES, VARIABLES, COPYS,SWITCHES,...
      * CONSTANTES ALFANUMERICAS
       01 CA-CONSTANTES-ALF.
          05 CA-PGM                    PIC X(08) VALUE 'RUTMITAB'.
          05 CA-TABLA                  PIC X(10) VALUE 'MITABLA'.
          05 CA-L                      PIC X(01) VALUE 'L'.
          05 CA-R                      PIC X(01) VALUE 'R'.
          05 CA-S                      PIC X(01) VALUE 'S'.
          05 CA-00                     PIC X(02) VALUE '00'.
          05 CA-99                     PIC X(02) VALUE '99'.
          05 CA-PARR-PRO     PIC X(12) VALUE '2000-PROCESO'.
          05 CA-DESC-ERR     PIC X(12) VALUE 'ERROR SELECT'.
      *
       01 SW-SWITCHES.
          05 SW-FIN-CURSOR             PIC X(01).
             88 SI-FIN-CURSOR          VALUE 'S'.
             88 NO-FIN-CURSOR          VALUE 'N'.
      *
       01 WK-CONTADORES.
          05 CONT-LEIDOS               PIC 9(3).
      *
       01 WK-SQLCODE                   PIC -999.
      *---------------- SQLCA ------------------------
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *---------------- DCLGEN -----------------------
           EXEC SQL
               INCLUDE TBMITAB
           END-EXEC.
      *---------------- CURSOR DE LISTREG -----------
           EXEC SQL
               DECLARE LISTREG CURSOR FOR
                  SELECT DNI
                        ,NOMBRE
                        ,APELLIDOS
                        ,FECNAC
                        ,SEXO
                    FROM MITABLA
                   WHERE DNI  > :TB-DNI
                   ORDER BY DNI
           END-EXEC.
      ******************************************************************
      *     L I N K A G E   S E C T I O N                              *
      ******************************************************************
       LINKAGE SECTION.
      * ESTRUCTURA DE COMUNICACION DE MI RUTINA
       COPY CPYMITAB.
      *
      ******************************************************************
      *     PROCEDURE DIVISION                                         *
      ******************************************************************
       PROCEDURE DIVISION USING CPYBRLR.
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
           INITIALIZE SALIDA
                      ERRORES
                      SALIDA-CONTROL
                      DCLMITABLA

           MOVE CA-00              TO RETORNO-ERR
           MOVE 1                  TO CONT-LEIDOS
           MOVE CA-S               TO MAS-DATOS
      *
           SET NO-FIN-CURSOR       TO TRUE
      *-- VALIDACION DE LOS CAMPOS DE ENTRADA
           PERFORM 1100-VALIDAR-OPCION
              THRU 1100-VALIDAR-OPCION-EXIT
      *
           PERFORM 1200-VALIDAR-NUM-ELEM-E
              THRU 1200-VALIDAR-NUM-ELEM-E-EXIT
      *
           .
      *
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     1100-VALIDAR-OPCION                                        *
      ******************************************************************
       1100-VALIDAR-OPCION.
      *
           IF OPCION NOT EQUAL TO CA-L
           AND OPCION NOT EQUAL TO CA-R
              DISPLAY 'OPCION INCORRECTA'
              MOVE '10'                   TO RETORNO-ERR
              MOVE '11'                   TO SUBRETORNO-ERR
              MOVE 'VALIDACION'           TO ACCION-ERR
              MOVE SPACES                 TO TABLA-ERR
              MOVE '1100-VALIDAR-OPCION'  TO PARRAFO-ERR
              MOVE CA-PGM                 TO NOMRUTINA-ERR
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           .
      *
       1100-VALIDAR-OPCION-EXIT.
           EXIT.
      *
      ******************************************************************
      *     1200-VALIDAR-NUM-ELEM-E                                    *
      ******************************************************************
       1200-VALIDAR-NUM-ELEM-E.
      *
           IF NUM-ELEM-E <= 0
              DISPLAY 'NUM-ELEM-E INCORRECTO'
              MOVE '10'                          TO RETORNO-ERR
              MOVE '01'                          TO SUBRETORNO-ERR
              MOVE 'VALIDACION'                  TO ACCION-ERR
              MOVE SPACES                        TO TABLA-ERR
              MOVE '1200-VALIDAR-NUM-ELEM-E'     TO PARRAFO-ERR
              MOVE CA-PGM                        TO NOMRUTINA-ERR
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF NUM-ELEM-E > 3
              DISPLAY 'NUM-ELEM-E INCORRECTO'
              MOVE '10'                          TO RETORNO-ERR
              MOVE '02'                          TO SUBRETORNO-ERR
              MOVE 'VALIDACION'                  TO ACCION-ERR
              MOVE SPACES                        TO TABLA-ERR
              MOVE '1200-VALIDAR-NUM-ELEM-E'     TO PARRAFO-ERR
              MOVE CA-PGM                        TO NOMRUTINA-ERR

              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT

           END-IF
      *
           .
      *
       1200-VALIDAR-NUM-ELEM-E-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2000-PROCESO                                               *
      ******************************************************************
       2000-PROCESO.
      *
           DISPLAY 'VALIDO OPCION'
           EVALUATE OPCION
               WHEN CA-L
                    DISPLAY 'OPCION -L-'
                    MOVE SPACES           TO TB-DNI
                    PERFORM 2100-ABRIR-CURSOR-LISTREG
                       THRU 2100-ABRIR-CURSOR-LISTREG-EXIT
      *
                     PERFORM UNTIL CONT-LEIDOS > NUM-ELEM-E
                     OR SI-FIN-CURSOR
                          PERFORM 2200-LEER-CURSOR-LISTREG
                             THRU 2200-LEER-CURSOR-LISTREG-EXIT
                          DISPLAY 'HAGO FETCH AL CURSOR DE -L-'
                     END-PERFORM

      *
                    PERFORM 2300-CERRAR-CURSOR-LISTREG
                       THRU 2300-CERR-CURSOR-LISTREG
               WHEN CA-R
      *
                    DISPLAY 'OPCION -R-'
                    MOVE DNI-REP           TO TB-DNI
                    PERFORM 2100-ABRIR-CURSOR-LISTREG
                       THRU 2100-ABRIR-CURSOR-LISTREG-EXIT
      *
                     PERFORM UNTIL CONT-LEIDOS > NUM-ELEM-E
                     OR SI-FIN-CURSOR
                          PERFORM 2200-LEER-CURSOR-LISTREG
                             THRU 2200-LEER-CURSOR-LISTREG-EXIT
                          DISPLAY 'HAGO FETCH AL CURSOR DE -R-'
                     END-PERFORM
      *
                    PERFORM 2300-CERRAR-CURSOR-LISTREG
                       THRU 2300-CERR-CURSOR-LISTREG
      *
           END-EVALUATE
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2100-ABRIR-CURSOR-LISTREG                                   *
      ******************************************************************
       2100-ABRIR-CURSOR-LISTREG.
      *
           SET NO-FIN-CURSOR TO TRUE
      *
           DISPLAY 'ABRO CURSOR DE LISTREG'
           EXEC SQL
               OPEN LISTREG
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO-ERR
                   MOVE CA-99                         TO SUBRETORNO-ERR
                   MOVE 'OPEN'                        TO ACCION-ERR
                   MOVE CA-TABLA                      TO TABLA-ERR
                   MOVE '2100-ABRIR-CURSOR-LISTREG'   TO PARRAFO-ERR
                   MOVE SQLCODE                       TO SQLCODE-ERR
                   MOVE CA-PGM                        TO NOMRUTINA-ERR
      *
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2100-ABRIR-CURSOR-LISTREG-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2200-LEER-CURSOR-LISTREG                                   *
      ******************************************************************
       2200-LEER-CURSOR-LISTREG.
      *
           EXEC SQL
              FETCH LISTREG
               INTO :TB-DNI
                   ,:TB-NOMBRE
                   ,:TB-APELLIDOS
                   ,:TB-FECNAC
                   ,:TB-SEXO
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   IF CONT-LEIDOS <= NUM-ELEM-E
                      PERFORM 2250-INFORMAR-SALIDA
                         THRU 2250-INFORMAR-SALIDA-EXIT
                   END-IF
              WHEN 100
                   SET SI-FIN-CURSOR                  TO TRUE
                   MOVE 'N'                           TO MAS-DATOS
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO-ERR
                   MOVE CA-99                         TO SUBRETORNO-ERR
                   MOVE 'FETCH'                       TO ACCION-ERR
                   MOVE CA-TABLA                      TO TABLA-ERR
                   MOVE '2200-LEER-CURSOR-LISTREG'    TO PARRAFO-ERR
                   MOVE SQLCODE                       TO SQLCODE-ERR
                   MOVE CA-PGM                        TO NOMRUTINA-ERR
      *
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2200-LEER-CURSOR-LISTREG-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2300-CERRAR-CURSOR-LISTREG                                 *
      ******************************************************************
       2300-CERRAR-CURSOR-LISTREG.
      *
           EXEC SQL
              CLOSE LISTREG
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO-ERR
                   MOVE CA-99                         TO SUBRETORNO-ERR
                   MOVE 'CLOSE'                       TO ACCION-ERR
                   MOVE CA-TABLA                      TO TABLA-ERR
                   MOVE '2300-CERRAR-CURSOR-LISTREG'  TO PARRAFO-ERR
                   MOVE SQLCODE                       TO SQLCODE-ERR
                   MOVE CA-PGM                        TO NOMRUTINA-ERR
      *
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2300-CERR-CURSOR-LISTREG.
           EXIT.
      *
      ******************************************************************
      *     2250-INFORMAR-SALIDA                                       *
      ******************************************************************
       2250-INFORMAR-SALIDA.
      *
      *-- INFORMAR EL OCCURS DE SALIDA CON LOS DATOS DEL FETCH

            MOVE TB-DNI          TO DNI(CONT-LEIDOS)
            MOVE TB-NOMBRE       TO NOMBRE(CONT-LEIDOS)
            MOVE TB-APELLIDOS    TO APELLIDOS(CONT-LEIDOS)
            MOVE TB-FECNAC       TO FECNAC(CONT-LEIDOS)
            MOVE TB-SEXO         TO SEXO(CONT-LEIDOS)
      *
            MOVE TB-DNI          TO DNI-REP
      *
            ADD 1                TO CONT-LEIDOS
            ADD 1                TO NUM-ELEM-S
      *
           .
      *
       2250-INFORMAR-SALIDA-EXIT.
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