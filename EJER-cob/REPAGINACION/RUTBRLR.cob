      *****************************************************************
      *       PGM COBOL DE EJEMPLO - ESQUELETO                         *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      *                                                                *
      * SE DEFINE EL NOMBRE DEL PROGRAMA FUENTE                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   RUTBRLR.
       AUTHOR.       DAVID.
       DATE-WRITTEN. 29/02/2025.
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
          05 CA-PGM                    PIC X(08) VALUE 'RUTBRLR'.
          05 CA-L                      PIC X(01) VALUE 'L'.
          05 CA-R                      PIC X(01) VALUE 'R'.
          05 CA-S                      PIC X(01) VALUE 'S'.
          05 CA-00                     PIC X(02) VALUE '00'.
          05 CA-99                     PIC X(02) VALUE '99'.
          05 CA-PARR-PRO    PIC X(12) VALUE '2000-PROCESO'.
          05 CA-DESC-ERR    PIC X(12) VALUE 'ERROR SELECT'.
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
               INCLUDE TBBREXIT
           END-EXEC.
      *-------------- DEFINIMOS LOS CURSORES ---------
           EXEC SQL
               DECLARE CURSOR-LISTA CURSOR FOR
                  SELECT BANK
                        ,OFFICE
                        ,CD
                        ,COUNT_NUMBER
                        ,CUSTOMER
                        ,TYPE
                        ,BALANCE
                        ,CURRENCY
                        ,NAME
                        ,SURNAME
                        ,CITY
                        ,COUNTRY
                        ,BIRTHDAY
                    FROM BREXIT_BANK
                   ORDER BY BANK
                           ,OFFICE
                           ,CD
                           ,COUNT_NUMBER
                           ,CUSTOMER
           END-EXEC.
      *
      *---------------- CURSOR DE REPAG1 -----------
           EXEC SQL
               DECLARE REPAG1 CURSOR FOR
                  SELECT BANK
                        ,OFFICE
                        ,CD
                        ,COUNT_NUMBER
                        ,CUSTOMER
                        ,TYPE
                        ,BALANCE
                        ,CURRENCY
                        ,NAME
                        ,SURNAME
                        ,CITY
                        ,COUNTRY
                        ,BIRTHDAY
                    FROM BREXIT_BANK
                   WHERE BANK         = :TB-BANK
                     AND OFFICE       = :TB-OFFICE
                     AND CD           = :TB-CD
                     AND COUNT_NUMBER = :TB-COUNT-NUMBER
                     AND CUSTOMER     > :TB-CUSTOMER
                   ORDER BY BANK
                           ,OFFICE
                           ,CD
                           ,COUNT_NUMBER
                           ,CUSTOMER
           END-EXEC.
      *
      *---------------- CURSOR DE REPAG2 -----------
           EXEC SQL
               DECLARE REPAG2 CURSOR FOR
                  SELECT BANK
                        ,OFFICE
                        ,CD
                        ,COUNT_NUMBER
                        ,CUSTOMER
                        ,TYPE
                        ,BALANCE
                        ,CURRENCY
                        ,NAME
                        ,SURNAME
                        ,CITY
                        ,COUNTRY
                        ,BIRTHDAY
                    FROM BREXIT_BANK
                   WHERE BANK         = :TB-BANK
                     AND OFFICE       = :TB-OFFICE
                     AND CD           = :TB-CD
                     AND COUNT_NUMBER > :TB-COUNT-NUMBER
                   ORDER BY BANK
                           ,OFFICE
                           ,CD
                           ,COUNT_NUMBER
                           ,CUSTOMER
           END-EXEC.
      *---------------- CURSOR DE REPAG3 -----------
           EXEC SQL
               DECLARE REPAG3 CURSOR FOR
                  SELECT BANK
                        ,OFFICE
                        ,CD
                        ,COUNT_NUMBER
                        ,CUSTOMER
                        ,TYPE
                        ,BALANCE
                        ,CURRENCY
                        ,NAME
                        ,SURNAME
                        ,CITY
                        ,COUNTRY
                        ,BIRTHDAY
                    FROM BREXIT_BANK
                   WHERE BANK         = :TB-BANK
                     AND OFFICE       = :TB-OFFICE
                     AND CD           > :TB-CD
                   ORDER BY BANK
                           ,OFFICE
                           ,CD
                           ,COUNT_NUMBER
                           ,CUSTOMER
           END-EXEC.
      *---------------- CURSOR DE REPAG4 -----------
           EXEC SQL
               DECLARE REPAG4 CURSOR FOR
                  SELECT BANK
                        ,OFFICE
                        ,CD
                        ,COUNT_NUMBER
                        ,CUSTOMER
                        ,TYPE
                        ,BALANCE
                        ,CURRENCY
                        ,NAME
                        ,SURNAME
                        ,CITY
                        ,COUNTRY
                        ,BIRTHDAY
                    FROM BREXIT_BANK
                   WHERE BANK         = :TB-BANK
                     AND OFFICE       > :TB-OFFICE
                   ORDER BY BANK
                           ,OFFICE
                           ,CD
                           ,COUNT_NUMBER
                           ,CUSTOMER
           END-EXEC.
      *---------------- CURSOR DE REPAG5 -----------
           EXEC SQL
               DECLARE REPAG5 CURSOR FOR
                  SELECT BANK
                        ,OFFICE
                        ,CD
                        ,COUNT_NUMBER
                        ,CUSTOMER
                        ,TYPE
                        ,BALANCE
                        ,CURRENCY
                        ,NAME
                        ,SURNAME
                        ,CITY
                        ,COUNTRY
                        ,BIRTHDAY
                    FROM BREXIT_BANK
                   WHERE BANK         > :TB-BANK
                   ORDER BY BANK
                           ,OFFICE
                           ,CD
                           ,COUNT_NUMBER
                           ,CUSTOMER
           END-EXEC.
      ******************************************************************
      *     L I N K A G E   S E C T I O N                              *
      ******************************************************************
       LINKAGE SECTION.
      * ESTRUCTURA DE COMUNICACION DE MI RUTINA
       COPY CPYREPA.
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
                      DCLBREXIT-BANK

           MOVE CA-00              TO RETORNO
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
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     1100-VALIDAR-OPCION                                        *
      ******************************************************************
       1100-VALIDAR-OPCION.
      *
           IF OPCION NOT EQUAL CA-L
           AND OPCION NOT EQUAL CA-R
              DISPLAY 'OPCION INCORRECTA'
              MOVE '10'                   TO RETORNO
              MOVE '11'                   TO SUBRETORNO
              MOVE 'VALIDACION'           TO ACCION
              MOVE SPACES                 TO TABLA
              MOVE '1100-VALIDAR-OPCION'  TO PARRAFO
              MOVE CA-PGM                 TO NOMRUTINA

              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT

           END-IF
      *
           .
       1100-VALIDAR-OPCION-EXIT.
           EXIT.
      *
      ******************************************************************
      *     1200-VALIDAR-NUM-ELEM-E                                    *
      ******************************************************************
       1200-VALIDAR-NUM-ELEM-E.
      *
           IF NUM-ELEM-E < 0
              DISPLAY 'NUM-ELEM-E INCORRECTO'
              MOVE '10'                          TO RETORNO
              MOVE '01'                          TO SUBRETORNO
              MOVE 'VALIDACION'                  TO ACCION
              MOVE SPACES                        TO TABLA
              MOVE '1200-VALIDAR-NUM-ELEM-E'     TO PARRAFO
              MOVE CA-PGM                        TO NOMRUTINA

              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT

           END-IF
      *
           IF NUM-ELEM-E > 3
              DISPLAY 'NUM-ELEM-E INCORRECTO'
              MOVE '10'                          TO RETORNO
              MOVE '02'                          TO SUBRETORNO
              MOVE 'VALIDACION'                  TO ACCION
              MOVE SPACES                        TO TABLA
              MOVE '1200-VALIDAR-NUM-ELEM-E'     TO PARRAFO
              MOVE CA-PGM                        TO NOMRUTINA

              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT

           END-IF
      *
           .
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
                    PERFORM 2100-ABRIR-CURSOR-LISTA
                       THRU 2100-ABRIR-CURSOR-LISTA-EXIT

                    PERFORM 2200-LEER-CURSOR-LISTA
                       THRU 2200-LEER-CURSOR-LISTA-EXIT
                      UNTIL CONT-LEIDOS > NUM-ELEM-E
                         OR SI-FIN-CURSOR

                    PERFORM 2300-CERRAR-CURSOR-LISTA
                       THRU 2300-CERRAR-CURSOR-LISTA-EXIT
               WHEN CA-R
      *             MOVE BANK-REP           TO TB-BANK
      *             MOVE OFFICE-REP         TO TB-OFFICE
      *             MOVE CD-REP             TO TB-CD
      *             MOVE COUNT-NUMBER-REP   TO TB-COUNT-NUMBER
      *             MOVE CUSTOMER-REP       TO TB-CUSTOMER
                    PERFORM 2D00-INFORMAR-REPAG
                       THRU 2D00-INFORMAR-REPAG-EXIT

                    PERFORM 2400-ABRIR-CURSOR-REPAG1
                       THRU 2400-ABRIR-CURSOR-REPAG1-EXIT

                    PERFORM 2500-LEER-CURSOR-REPAG1
                       THRU 2500-LEER-CURSOR-REPAG1-EXIT
                      UNTIL CONT-LEIDOS > NUM-ELEM-E
                         OR SI-FIN-CURSOR

                    PERFORM 2600-CERRAR-CURSOR-REPAG1
                       THRU 2600-CERRAR-CURSOR-REPAG1-EXIT

                    IF SI-FIN-CURSOR

                       SET NO-FIN-CURSOR         TO TRUE

                       PERFORM 2700-ABRIR-CURSOR-REPAG2
                          THRU 2700-ABRIR-CURSOR-REPAG2-EXIT

                       PERFORM 2800-LEER-CURSOR-REPAG2
                          THRU 2800-LEER-CURSOR-REPAG2-EXIT
                         UNTIL CONT-LEIDOS > NUM-ELEM-E
                            OR SI-FIN-CURSOR

                       PERFORM 2900-CERRAR-CURSOR-REPAG2
                          THRU 2900-CERRAR-CURSOR-REPAG2-EXIT

                       IF SI-FIN-CURSOR

                          SET NO-FIN-CURSOR         TO TRUE

                          PERFORM 2A00-ABRIR-CURSOR-REPAG3
                             THRU 2A00-ABRIR-CURSOR-REPAG3-EXIT

                          PERFORM 2A00-LEER-CURSOR-REPAG3
                             THRU 2A00-LEER-CURSOR-REPAG3-EXIT
                            UNTIL CONT-LEIDOS > NUM-ELEM-E
                               OR SI-FIN-CURSOR

                          PERFORM 2A00-CERRAR-CURSOR-REPAG3
                             THRU 2A00-CERRAR-CURSOR-REPAG3-EXIT

                          IF SI-FIN-CURSOR

                             SET NO-FIN-CURSOR         TO TRUE

                             PERFORM 2B00-ABRIR-REPAG4
                                THRU 2B00-ABRIR-REPAG4-EXIT

                             PERFORM 2B00-LEER-REPAG4
                                THRU 2B00-LEER-REPAG4-EXIT
                               UNTIL CONT-LEIDOS > NUM-ELEM-E
                                  OR SI-FIN-CURSOR

                             PERFORM 2B00-CERRAR-REPAG4
                                THRU 2B00-CERRAR-REPAG4-EXIT

                             IF SI-FIN-CURSOR

                                SET NO-FIN-CURSOR         TO TRUE

                                PERFORM 2C00-ABRIR-REPAG5
                                   THRU 2C00-ABRIR-REPAG5-EXIT

                                PERFORM 2C00-LEER-REPAG5
                                   THRU 2C00-LEER-REPAG5-EXIT
                                  UNTIL CONT-LEIDOS > NUM-ELEM-E
                                     OR SI-FIN-CURSOR

                                PERFORM 2C00-CERRAR-REPAG5
                                   THRU 2C00-CERRAR-REPAG5-EXIT

                             END-IF
                          END-IF
                       END-IF
                    END-IF

           END-EVALUATE
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2100-ABRIR-CURSOR-LISTA                                    *
      ******************************************************************
       2100-ABRIR-CURSOR-LISTA.
      *
           SET NO-FIN-CURSOR TO TRUE
      *
           DISPLAY 'ABRO CURSOR DE LISTA'
           EXEC SQL
               OPEN CURSOR-LISTA
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO
                   MOVE CA-99                         TO SUBRETORNO
                   MOVE 'OPEN'                        TO ACCION
                   MOVE 'BREXIT_BANK'                 TO TABLA
                   MOVE '2100-ABRIR-CURSOR-LISTA'     TO PARRAFO
                   MOVE CA-PGM                        TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2100-ABRIR-CURSOR-LISTA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2200-LEER-CURSOR-LISTA                                     *
      ******************************************************************
       2200-LEER-CURSOR-LISTA.
      *
           DISPLAY 'HAGO FETCH AL CURSOR DE -L-'
      *
           EXEC SQL
              FETCH CURSOR-LISTA
               INTO :TB-BANK
                   ,:TB-OFFICE
                   ,:TB-CD
                   ,:TB-COUNT-NUMBER
                   ,:TB-CUSTOMER
                   ,:TB-TYPE
                   ,:TB-BALANCE
                   ,:TB-CURRENCY
                   ,:TB-NAME
                   ,:TB-SURNAME
                   ,:TB-CITY
                   ,:TB-COUNTRY
                   ,:TB-BIRTHDAY
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   IF CONT-LEIDOS <= NUM-ELEM-E
                      PERFORM 2250-INFORMAR-SALIDA
                         THRU 2250-INFORMAR-SALIDA-EXIT
                   END-IF
              WHEN 100
                   SET SI-FIN-CURSOR           TO TRUE
                   MOVE 'N'                    TO MAS-DATOS
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO
                   MOVE CA-99                         TO SUBRETORNO
                   MOVE 'FETCH'                       TO ACCION
                   MOVE 'BREXIT_BANK'                 TO TABLA
                   MOVE '2200-LEER-CURSOR-LISTA '     TO PARRAFO
                   MOVE CA-PGM                        TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2200-LEER-CURSOR-LISTA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2300-CERRAR-CURSOR-LISTA                                   *
      ******************************************************************
       2300-CERRAR-CURSOR-LISTA.
      *
           EXEC SQL
              CLOSE CURSOR-LISTA
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO
                   MOVE CA-99                         TO SUBRETORNO
                   MOVE 'CLOSE'                       TO ACCION
                   MOVE 'BREXIT_BANK'                 TO TABLA
                   MOVE '2300-CERRAR-CURSOR-LISTA'    TO PARRAFO
                   MOVE CA-PGM                        TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2300-CERRAR-CURSOR-LISTA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2250-INFORMAR-SALIDA                                       *
      ******************************************************************
       2250-INFORMAR-SALIDA.
      *
      *-- INFORMAR EL OCCURS DE SALIDA CON LOS DATOS DEL FETCH

            MOVE TB-BANK            TO BANK(CONT-LEIDOS)
            MOVE TB-OFFICE          TO OFFICE(CONT-LEIDOS)
            MOVE TB-CD              TO CD-S(CONT-LEIDOS)
            MOVE TB-COUNT-NUMBER    TO COUNT-NUMBER(CONT-LEIDOS)
            MOVE TB-CUSTOMER        TO CUSTOMER(CONT-LEIDOS)
            MOVE TB-TYPE            TO TYPE-S(CONT-LEIDOS)
            MOVE TB-BALANCE         TO BALANCE(CONT-LEIDOS)
            MOVE TB-CURRENCY        TO CURRENCY-S(CONT-LEIDOS)
            MOVE TB-NAME            TO NAME(CONT-LEIDOS)
            MOVE TB-SURNAME         TO SURNAME(CONT-LEIDOS)
            MOVE TB-CITY            TO CITY(CONT-LEIDOS)
            MOVE TB-COUNTRY         TO COUNTRY(CONT-LEIDOS)
            MOVE TB-BIRTHDAY        TO BIRTHDAY(CONT-LEIDOS)
      *
      *     MOVE TB-BANK            TO BANK-REP
      *     MOVE TB-OFFICE          TO OFFICE-REP
      *     MOVE TB-CD              TO CD-REP
      *     MOVE TB-COUNT-NUMBER    TO COUNT-NUMBER-REP
      *     MOVE TB-CUSTOMER        TO CUSTOMER-REP
      *
            ADD 1                   TO CONT-LEIDOS
            ADD 1                   TO NUM-ELEM-S
      *
           .
       2250-INFORMAR-SALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2400-ABRIR-CURSOR-REPAG1                                   *
      ******************************************************************
       2400-ABRIR-CURSOR-REPAG1.
      *
           SET NO-FIN-CURSOR TO TRUE
      *
           EXEC SQL
               OPEN REPAG1
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO
                   MOVE CA-99                         TO SUBRETORNO
                   MOVE 'OPEN'                        TO ACCION
                   MOVE 'BREXIT_BANK'                 TO TABLA
                   MOVE '2400-ABRIR-CURSOR-REPAG1'    TO PARRAFO
                   MOVE CA-PGM                        TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2400-ABRIR-CURSOR-REPAG1-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2500-LEER-CURSOR-REPAG1                                    *
      ******************************************************************
       2500-LEER-CURSOR-REPAG1.
      *
           EXEC SQL
              FETCH REPAG1
               INTO :TB-BANK
                   ,:TB-OFFICE
                   ,:TB-CD
                   ,:TB-COUNT-NUMBER
                   ,:TB-CUSTOMER
                   ,:TB-TYPE
                   ,:TB-BALANCE
                   ,:TB-CURRENCY
                   ,:TB-NAME
                   ,:TB-SURNAME
                   ,:TB-CITY
                   ,:TB-COUNTRY
                   ,:TB-BIRTHDAY
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   IF CONT-LEIDOS <= NUM-ELEM-E
                      PERFORM 2250-INFORMAR-SALIDA
                         THRU 2250-INFORMAR-SALIDA-EXIT
                   END-IF
              WHEN 100
                   SET SI-FIN-CURSOR           TO TRUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO
                   MOVE CA-99                         TO SUBRETORNO
                   MOVE 'FETCH'                       TO ACCION
                   MOVE 'BREXIT_BANK'                 TO TABLA
                   MOVE '2500-LEER-CURSOR-REPAG1'     TO PARRAFO
                   MOVE CA-PGM                        TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2500-LEER-CURSOR-REPAG1-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2600-CERRAR-CURSOR-REPAG1                                  *
      ******************************************************************
       2600-CERRAR-CURSOR-REPAG1.
      *
           EXEC SQL
              CLOSE REPAG1
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO
                   MOVE CA-99                         TO SUBRETORNO
                   MOVE 'CLOSE'                       TO ACCION
                   MOVE 'BREXIT_BANK'                 TO TABLA
                   MOVE '2600-CERRAR-CURSOR-REPAG1'   TO PARRAFO
                   MOVE CA-PGM                        TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2600-CERRAR-CURSOR-REPAG1-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2700-ABRIR-CURSOR-REPAG2                                   *
      ******************************************************************
       2700-ABRIR-CURSOR-REPAG2.
      *
           SET NO-FIN-CURSOR TO TRUE
      *
           EXEC SQL
               OPEN REPAG2
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO
                   MOVE CA-99                         TO SUBRETORNO
                   MOVE 'OPEN'                        TO ACCION
                   MOVE 'BREXIT_BANK'                 TO TABLA
                   MOVE '2700-ABRIR-CURSOR-REPAG2'    TO PARRAFO
                   MOVE CA-PGM                        TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2700-ABRIR-CURSOR-REPAG2-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2800-LEER-CURSOR-REPAG2                                    *
      ******************************************************************
       2800-LEER-CURSOR-REPAG2.
      *
           EXEC SQL
              FETCH REPAG2
               INTO :TB-BANK
                   ,:TB-OFFICE
                   ,:TB-CD
                   ,:TB-COUNT-NUMBER
                   ,:TB-CUSTOMER
                   ,:TB-TYPE
                   ,:TB-BALANCE
                   ,:TB-CURRENCY
                   ,:TB-NAME
                   ,:TB-SURNAME
                   ,:TB-CITY
                   ,:TB-COUNTRY
                   ,:TB-BIRTHDAY
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   IF CONT-LEIDOS <= NUM-ELEM-E
                      PERFORM 2250-INFORMAR-SALIDA
                         THRU 2250-INFORMAR-SALIDA-EXIT
                   END-IF
              WHEN 100
                   SET SI-FIN-CURSOR           TO TRUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO
                   MOVE CA-99                         TO SUBRETORNO
                   MOVE 'FETCH'                       TO ACCION
                   MOVE 'BREXIT_BANK'                 TO TABLA
                   MOVE '2800-LEER-CURSOR-REPAG2'     TO PARRAFO
                   MOVE CA-PGM                        TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2800-LEER-CURSOR-REPAG2-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2900-CERRAR-CURSOR-REPAG2                                  *
      ******************************************************************
       2900-CERRAR-CURSOR-REPAG2.
      *
           EXEC SQL
              CLOSE REPAG2
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO
                   MOVE CA-99                         TO SUBRETORNO
                   MOVE 'CLOSE'                       TO ACCION
                   MOVE 'BREXIT_BANK'                 TO TABLA
                   MOVE '2900-CERRAR-CURSOR-REPAG2'   TO PARRAFO
                   MOVE CA-PGM                        TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2900-CERRAR-CURSOR-REPAG2-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2A00-ABRIR-CURSOR-REPAG3                                   *
      ******************************************************************
       2A00-ABRIR-CURSOR-REPAG3.
      *
           SET NO-FIN-CURSOR TO TRUE
      *
           EXEC SQL
               OPEN REPAG3
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO
                   MOVE CA-99                         TO SUBRETORNO
                   MOVE 'OPEN'                        TO ACCION
                   MOVE 'BREXIT_BANK'                 TO TABLA
                   MOVE '2A00-ABRIR-CURSOR-REPAG3'    TO PARRAFO
                   MOVE CA-PGM                        TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2A00-ABRIR-CURSOR-REPAG3-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2A00-LEER-CURSOR-REPAG3                                    *
      ******************************************************************
       2A00-LEER-CURSOR-REPAG3.
      *
           EXEC SQL
              FETCH REPAG3
               INTO :TB-BANK
                   ,:TB-OFFICE
                   ,:TB-CD
                   ,:TB-COUNT-NUMBER
                   ,:TB-CUSTOMER
                   ,:TB-TYPE
                   ,:TB-BALANCE
                   ,:TB-CURRENCY
                   ,:TB-NAME
                   ,:TB-SURNAME
                   ,:TB-CITY
                   ,:TB-COUNTRY
                   ,:TB-BIRTHDAY
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   IF CONT-LEIDOS <= NUM-ELEM-E
                      PERFORM 2250-INFORMAR-SALIDA
                         THRU 2250-INFORMAR-SALIDA-EXIT
                   END-IF
              WHEN 100
                   SET SI-FIN-CURSOR           TO TRUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO
                   MOVE CA-99                         TO SUBRETORNO
                   MOVE 'FETCH'                       TO ACCION
                   MOVE 'BREXIT_BANK'                 TO TABLA
                   MOVE '2A00-LEER-CURSOR-REPAG3'     TO PARRAFO
                   MOVE CA-PGM                        TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2A00-LEER-CURSOR-REPAG3-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2A00-CERRAR-CURSOR-REPAG3                                  *
      ******************************************************************
       2A00-CERRAR-CURSOR-REPAG3.
      *
           EXEC SQL
              CLOSE REPAG3
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                         TO RETORNO
                   MOVE CA-99                         TO SUBRETORNO
                   MOVE 'CLOSE'                       TO ACCION
                   MOVE 'BREXIT_BANK'                 TO TABLA
                   MOVE '2A00-CERRAR-CURSOR-REPAG3'   TO PARRAFO
                   MOVE CA-PGM                        TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2A00-CERRAR-CURSOR-REPAG3-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2B00-ABRIR-REPAG4                                          *
      ******************************************************************
       2B00-ABRIR-REPAG4.
      *
           SET NO-FIN-CURSOR TO TRUE
      *
           EXEC SQL
               OPEN REPAG4
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                  TO RETORNO
                   MOVE CA-99                  TO SUBRETORNO
                   MOVE 'OPEN'                 TO ACCION
                   MOVE 'BREXIT_BANK'          TO TABLA
                   MOVE '2B00-ABRIR-REPAG4'    TO PARRAFO
                   MOVE CA-PGM                 TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2B00-ABRIR-REPAG4-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2B00-LEER-REPAG4                                           *
      ******************************************************************
       2B00-LEER-REPAG4.
      *
           EXEC SQL
              FETCH REPAG4
               INTO :TB-BANK
                   ,:TB-OFFICE
                   ,:TB-CD
                   ,:TB-COUNT-NUMBER
                   ,:TB-CUSTOMER
                   ,:TB-TYPE
                   ,:TB-BALANCE
                   ,:TB-CURRENCY
                   ,:TB-NAME
                   ,:TB-SURNAME
                   ,:TB-CITY
                   ,:TB-COUNTRY
                   ,:TB-BIRTHDAY
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   IF CONT-LEIDOS <= NUM-ELEM-E
                      PERFORM 2250-INFORMAR-SALIDA
                         THRU 2250-INFORMAR-SALIDA-EXIT
                   END-IF
              WHEN 100
                   SET SI-FIN-CURSOR           TO TRUE
              WHEN OTHER
                   MOVE CA-99                  TO RETORNO
                   MOVE CA-99                  TO SUBRETORNO
                   MOVE 'FETCH'                TO ACCION
                   MOVE 'BREXIT_BANK'          TO TABLA
                   MOVE '2B00-LEER-REPAG4'     TO PARRAFO
                   MOVE CA-PGM                 TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2B00-LEER-REPAG4-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2B00-CERRAR-REPAG4                                         *
      ******************************************************************
       2B00-CERRAR-REPAG4.
      *
           EXEC SQL
              CLOSE REPAG4
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                  TO RETORNO
                   MOVE CA-99                  TO SUBRETORNO
                   MOVE 'CLOSE'                TO ACCION
                   MOVE 'BREXIT_BANK'          TO TABLA
                   MOVE '2B00-CERRAR-REPAG4'   TO PARRAFO
                   MOVE CA-PGM                 TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2B00-CERRAR-REPAG4-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2C00-ABRIR-REPAG5                                          *
      ******************************************************************
       2C00-ABRIR-REPAG5.
      *
           SET NO-FIN-CURSOR TO TRUE
      *
           EXEC SQL
               OPEN REPAG5
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                  TO RETORNO
                   MOVE CA-99                  TO SUBRETORNO
                   MOVE 'OPEN'                 TO ACCION
                   MOVE 'BREXIT_BANK'          TO TABLA
                   MOVE '2B00-ABRIR-REPAG4'    TO PARRAFO
                   MOVE CA-PGM                 TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2C00-ABRIR-REPAG5-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2C00-LEER-REPAG5                                           *
      ******************************************************************
       2C00-LEER-REPAG5.
      *
           EXEC SQL
              FETCH REPAG5
               INTO :TB-BANK
                   ,:TB-OFFICE
                   ,:TB-CD
                   ,:TB-COUNT-NUMBER
                   ,:TB-CUSTOMER
                   ,:TB-TYPE
                   ,:TB-BALANCE
                   ,:TB-CURRENCY
                   ,:TB-NAME
                   ,:TB-SURNAME
                   ,:TB-CITY
                   ,:TB-COUNTRY
                   ,:TB-BIRTHDAY
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   IF CONT-LEIDOS <= NUM-ELEM-E
                      PERFORM 2250-INFORMAR-SALIDA
                         THRU 2250-INFORMAR-SALIDA-EXIT
                   END-IF
              WHEN 100
                   SET SI-FIN-CURSOR           TO TRUE
                   MOVE 'N'                    TO MAS-DATOS
              WHEN OTHER
                   MOVE CA-99                  TO RETORNO
                   MOVE CA-99                  TO SUBRETORNO
                   MOVE 'FETCH'                TO ACCION
                   MOVE 'BREXIT_BANK'          TO TABLA
                   MOVE '2C00-LEER-REPAG5'     TO PARRAFO
                   MOVE CA-PGM                 TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2C00-LEER-REPAG5-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2C00-CERRAR-REPAG5                                         *
      ******************************************************************
       2C00-CERRAR-REPAG5.
      *
           EXEC SQL
              CLOSE REPAG5
           END-EXEC.
      *
           EVALUATE SQLCODE
              WHEN 0
                   CONTINUE
              WHEN OTHER
                   MOVE CA-99                  TO RETORNO
                   MOVE CA-99                  TO SUBRETORNO
                   MOVE 'CLOSE'                TO ACCION
                   MOVE 'BREXIT_BANK'          TO TABLA
                   MOVE '2C00-CERRAR-REPAG5'   TO PARRAFO
                   MOVE CA-PGM                 TO NOMRUTINA

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2C00-CERRAR-REPAG5-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2D00-INFORMAR-REPAG                                        *
      ******************************************************************
       2D00-INFORMAR-REPAG.
      *
            MOVE TB-BANK            TO BANK-REP
            MOVE TB-OFFICE          TO OFFICE-REP
            MOVE TB-CD              TO CD-REP
            MOVE TB-COUNT-NUMBER    TO COUNT-NUMBER-REP
            MOVE TB-CUSTOMER        TO CUSTOMER-REP
      *
           .
       2D00-INFORMAR-REPAG-EXIT.
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
