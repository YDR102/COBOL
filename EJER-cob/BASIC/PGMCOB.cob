      *****************************
      *  IDENTIFICATION DIVISION  *
      *****************************                                     *******
       IDENTIFICATION DIVISION.
      ******************************************************************
      *          PRIMER PROGRAMA COBOL DONDE APRENDERMOS               *
      *             LA ESTRUCTURA DE UN PROGRAMA                       *
      ******************************************************************
       PROGRAM-ID. PGMCOB.
       AUTHOR DAVID.
      *
      **************************
      *  ENVIRONMENT DIVISION  *
      ******************************************************************
      *   DEFINIR FICHEROS QUE VAMOS A UTILIZAR                        *
      *   RELACIONAR NOMBRES LOGICOS DE LOS ARCHIVOS                   *
      *   DEL PROGRAMA CON LOS NOMBRES DEL JCL                         *
      *   2 SECIONES:                                                  *
      *     -CONFIGURATION DE SECTION                                  *
      *     -INPUT-OUTPUT SECTION                                      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
      * SE DECLARAN LOS FICHEROS DE ENTRADA Y SALIDA
      * OBLIGATORIO (CUANDO HAY)
       FILE-CONTROL.
           SELECT FENTRADA
              ASSIGN TO ENTRADA
              FILE STATUS IS FS-FENTRADA.
           SELECT FSALIDA
              ASSIGN TO SALIDA
              FILE STATUS IS FS-FSALIDA.
      *
      *******************
      *  DATA DIVISION  *
      ******************************************************************
      * IDENTIFICA TODOS LOS NOMBRES DE DATOS   *
      * SECCIONES:  *
      *   - FILE SECTION  *
      *     DESCRIBE FICHEROS DE ENTRADA Y SALIDA QUE USAMOS  *
      *   - WORKING STOREGE SECTION*
      *     DEFINIMOS LOS CAMPOS DE TRABAJO      *
      *   - LINKAGE SECTION  *
      *     SE USA CUANDO EL PROGRAMA SEA UNA RUTINA YA QUE ES LA ZONA *
      *     QUE SIRVE PARA COMUNICARSE CON EL PROGRAMA  *
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD FENTRADA
            BLOCK CONTAINS 0 RECORDS
            LABEL RECORD ARE STANDARD
            RECORDING MODE IS F
            DATA RECORD IS REG-ENT.
       01 REG-ENT        PIC X(01).
      *
       FD FSALIDA
            BLOCK CONTAINS 0 RECORDS
            LABEL RECORD ARE STANDARD
            RECORDING MODE IS F
            DATA RECORD IS REG-SAL.
       01 REG-SAL        PIC X(01).
      *
      *********************
      *  WORKING STORAGE  *
      ******************************************************************
      *  LUGAR DONDE DECLARAREMOS CONTANTES, VARIAVLES, COPYS, ...     *
      ******************************************************************
      *
       WORKING-STORAGE SECTION.
      *
       01 ALF-VAR        PIC X(01).
       01 NUM-VAR        PIC 9(01).
       01 VAR-SYSIN      PIC X(50).
       01 VAR-SYSIN2     PIC X(50).
       01 VAR-SYSIN3     PIC X(50).
       01 VAR-SYSIN4     PIC X(50).
       01 VAR-SYSIN5     PIC X(50).
       01 VAR-SYSIN6     PIC X(50) VALUE 'HOLA'.
       01 CONTADOR       PIC 9(09).
      *
       01 CA-CONTANTES-NUM.
          05 CA-2        PIC 9(01) VALUE 2.
          05 CA-50       PIC 9(02) VALUE 50.
          05 CA-100      PIC 9(03) VALUE 100.
      *
       01 CA-CONTANTES-ALF.
          05 CA-ALF1     PIC X(04) VALUE 'HOLA'.
          05 CA-ALF2     PIC X(05) VALUE 'DAVID'.
          05 CA-ALF3     PIC X(05) VALUE 'ADIOS'.
      *
       01 FS-FILE-STATUS.
          05 FS-FENTRADA PIC X(02).
          05 FS-FSALIDA  PIC X(02).
      *
       01 SW-SWITCHES.
          05 SW-LAMPARA  PIC  X(01) VALUE 'N'.
              88 ENCENDIDA          VALUE 'S'.
              88 APAGADA            VALUE 'N'.
      *
      *****************
      *  EJERCICIO 1  *
      *****************
      *
       01 VAR-EJER1-1    PIC 9(02) VALUE 25.
       01 VAR-EJER1-2    PIC 9(02).
      *
      *****************
      *  EJERCICIO 2  *
      *****************
      *
       01 VAR-EJER2-1    PIC 9(02) VALUE 88.
       01 VAR-EJER2-2    PIC X(04) VALUE 'HOLA'.
      *
      *****************
      *  EJERCICIO 3  *
      *****************
      *
       01 VAR-EJER3-1    PIC X(11) VALUE 'DAVID RUANO'.
       01 VAR-EJER3-2    PIC 9(02) VALUE 21.
      *
      *****************
      *  EJERCICIO 4  *
      *****************
      *
       01 VAR-EJER4      PIC X(20).
      *
      *****************
      *  EJERCICIO 5  *
      *****************
      *
       01 VAR-EJER5      PIC X(50) VALUE 'BALA PALA RARA'.
      *
      *****************
      *  EJERCICIO 6  *
      *****************
      *
       01 VAR-EJER6      PIC X(50) VALUE 'BOLO POLO'.
      *
      *****************
      *  EJERCICIO 7  *
      *****************
      *
       01 VAR-EJER7-1    PIC 9(03) VALUE 100.
       01 VAR-EJER7-2    PIC 9(03).
      *
      *****************
      *  EJERCICIO 8  *
      *****************
      *
       01 VAR-EJER8-1    PIC X(50).
       01 VAR-EJER8-2    PIC X(50).
      *
      *****************
      *  EJERCICIO 9  *
      *****************
      *
       01 VAR-EJER9-1    PIC X(40).
       01 VAR-EJER9-2    PIC X(40).
      *
      ******************
      *  EJERCICIO 10  *
      ******************
      *
       01 VAR-EJER10     PIC X(99).
      *
      ******************
      *  EJERCICIO 11  *
      ******************
      *
       01 VAR-EJER11     PIC X(20).
      *
      ************************
      *  PROCEDURE DIVISION  *
      ************************
      *
       PROCEDURE DIVISION.
      *SENTENCIA SET
      *    SET ENCENDIDA TO TRUE
      *SENTENCIA INITIALIZE
      *    INITIALIZE OTRA-VARIAVLE
      *SENTENCIA DISPLAY
      *    DISPLAY 'HOLA MUNDO'
      *SENTENCIA ACCEPT
      *    ACCEPT VAR-SYSIN FROM SYSIN
      *    DISPLAY 'VAR-SYSIN: 'VAR-SYSIN
      *
      *    ACCEPT VAR-SYSIN2 FROM SYSIN
      *    DISPLAY 'VAR-SYSIN2: 'VAR-SYSIN2
      *SENTENCIA MOVE
      *    MOVE VAR-SYSIN TO VAR-SYSIN2
      *    DISPLAY 'DESPUES DE MOVER EL VALOR DE SYSIN AL SYSIN2'
      *    DISPLAY 'VAR-SYSIN: 'VAR-SYSIN
      *    DISPLAY 'VAR-SYSIN2: 'VAR-SYSIN2
      *
      *    ACCEPT VAR-SYSIN3 FROM SYSIN
      *    DISPLAY 'VAR-SYSIN3: 'VAR-SYSIN3
      *SENTENCIA INSPECT
      *    INSPECT VAR-SYSIN3 TALLYING CONTADOR FOR ALL 'O'
      *    DISPLAY 'CONTADOR DE "O" EN SYSIN3: 'CONTADOR
      *SENTENCIA IF/ELSE
      *    IF VAR-SYSIN4 IS NUMBER
      *       DISPLAY 'NUMERICO'
      *    ELSE
      *       DISPLAY 'NO NUMERICO'
      *    END-IF
      *
      *SENTENCIA EVALUATE
      *    EVALUATE VAR-SYSIN5
      *        WHEN 'HOLA'
      *           DISPLAY 'EL VALOR ES HOLA'
      *        WHEN 'ADIOS'
      *           DISPLAY 'EL VALOR ES ADIOS'
      *        WHEN OTHER
      *           DISPLAY 'EL VALOR ES'VAR-SYSIN5
      *    END-EVALUATE
      *
      *****************
      *  EJERCICIO 1  *
      *****************
      *
           MOVE VAR-EJER1-1 TO VAR-EJER1-2
           DISPLAY '***EJERCICIO 1***'
           DISPLAY VAR-EJER1-1
           DISPLAY VAR-EJER1-2
      *
      *****************
      *  EJERCICIO 2  *
      *****************
      *
           INITIALIZE VAR-EJER2-1
           INITIALIZE VAR-EJER2-2
           DISPLAY '***EJERCICIO 2***'
           DISPLAY VAR-EJER2-1
           DISPLAY VAR-EJER2-2
      *
      *****************
      *  EJERCICIO 3  *
      *****************
      *
           DISPLAY '***EJERCICIO 3***'
           DISPLAY 'HOLA SOY 'VAR-EJER3-1' Y TENGO 'VAR-EJER3-2' ANNOS'
      *
      *****************
      *  EJERCICIO 4  *
      *****************
      *
           ACCEPT VAR-EJER4 FROM SYSIN
           DISPLAY '***EJERCICIO 4***'
           DISPLAY VAR-EJER4
      *
      *****************
      *  EJERCICIO 5  *
      *****************
      *
           INSPECT VAR-EJER5 TALLYING CONTADOR FOR ALL 'A'
           DISPLAY 'CONTADOR DE "A" EN EJER5: 'CONTADOR
      *
      *****************
      *  EJERCICIO 6  *
      *****************
      *
           INSPECT VAR-EJER6 REPLACING ALL 'O' BY '*'
           DISPLAY '***EJERCICIO 6***'
           DISPLAY VAR-EJER6
      *
      *****************
      *  EJERCICIO 7  *
      *****************
      *
           MOVE VAR-EJER7-1 TO VAR-EJER7-2
           DISPLAY '***EJERCICIO 7***'
           DISPLAY VAR-EJER7-2
      *
      *****************
      *  EJERCICIO 8  *
      *****************
      *
           ACCEPT VAR-EJER8-1 FROM SYSIN
           MOVE VAR-EJER8-1 TO VAR-EJER8-2
           DISPLAY '***EJERCICIO 8***'
           DISPLAY VAR-EJER8-2
      *
      *****************
      *  EJERCICIO 9  *
      *****************
      *
           ACCEPT VAR-EJER9-1 FROM SYSIN
           MOVE VAR-EJER9-1 TO VAR-EJER9-2
           DISPLAY '***EJERCICIO 9***'
           DISPLAY VAR-EJER9-2
      *
      ******************
      *  EJERCICIO 10  *
      ******************
      *
           ACCEPT VAR-EJER10 FROM SYSIN
           INSPECT VAR-EJER10 TALLYING CONTADOR FOR ALL 'E'
           DISPLAY 'CONTADOR DE "E" EN EJER10: 'CONTADOR
      *
      ******************
      *  EJERCICIO 11  *
      ******************
      *
           DISPLAY '***EJERCICIO 11***'
           DISPLAY VAR-EJER11
           INSPECT VAR-EJER11 TALLYING CONTADOR FOR ALL 'O'
           DISPLAY 'CONTADOR DE "O" EN EJER11: 'CONTADOR
           INSPECT VAR-EJER11 REPLACING ALL 'O' BY '$'
           DISPLAY VAR-EJER11
      *
      ********************
      *  EJERCICIO IF 1  *
      ********************
      *
           DISPLAY '***EJERCICIO IF 1***'
           IF CA-2 IS POSITIVE
               DISPLAY 'POSITIVO'
           ELSE
               DISPLAY 'NEGATIVO'
           END-IF
      *
      *
      ********************
      *  EJERCICIO IF 2  *
      ********************
      *
           DISPLAY '***EJERCICIO IF 2***'
           IF CA-2 IS NEGATIVE
               DISPLAY 'NEGATIVO'
           ELSE
               DISPLAY 'NO NEGATIVO'
           END-IF
      *
      ********************
      *  EJERCICIO IF 3  *
      ********************
      *
           DISPLAY '***EJERCICIO IF 3***'
           IF CA-2 IS NUMERIC
               DISPLAY 'ENTRADA VALIDA'
           ELSE
               DISPLAY 'NO ES UN NúMERO'
           END-IF
      *
      ********************
      *  EJERCICIO IF 4  *
      ********************
      *
           DISPLAY '***EJERCICIO IF 4***'
           IF CA-ALF1 IS ALPHABETIC
               DISPLAY 'NOMBRE CORECTO'
           ELSE
                DISPLAY 'NOMBRE INVALIDO'
           END-IF
      *
      ********************
      *  EJERCICIO IF 5  *
      ********************
      *
           DISPLAY '***EJERCICIO IF 5***'
           IF CA-2 >= 18 AND CA-2 <= 30
               DISPLAY 'EDAD ENTRE 18 Y 30'
           ELSE
                DISPLAY 'FUERA DE RANGO'
           END-IF
      *
      ********************
      *  EJERCICIO IF 6  *
      ********************
      *
           DISPLAY '***EJERCICIO IF 6***'
           IF CA-2 >= 0 AND CA-2 <= 100
               DISPLAY 'RANGO ENTRE 0 Y 100'
           ELSE
                DISPLAY 'FUERA DEL RANGO PERMITIDO'
           END-IF
      *
      ********************
      *  EJERCICIO IF 7  *
      ********************
      *
           DISPLAY '***EJERCICIO IF 7***'
           EVALUATE TRUE
               WHEN CA-50 < 18
                  DISPLAY 'MENOR'
               WHEN CA-50 >= 18 AND CA-50 <= 30
                  DISPLAY 'ADULTO JOVEN'
               WHEN CA-50 > 30 AND CA-50 <= 65
                  DISPLAY 'ADULTO MAYOR'
               WHEN OTHER
                  DISPLAY 'MAYOR DE 65'
           END-EVALUATE
      *
      ********************
      *  EJERCICIO IF 8  *
      ********************
      *
           DISPLAY '***EJERCICIO IF 8***'
           ACCEPT NUM-VAR
           DISPLAY NUM-VAR
           EVALUATE TRUE
               WHEN NUM-VAR = 1
                  DISPLAY '1-SUMAR'
               WHEN NUM-VAR = 2
                  DISPLAY '2-RESTAR'
               WHEN NUM-VAR = 3
                  DISPLAY '3-SALIR'
               WHEN OTHER
                  DISPLAY 'NO VALIDO TIENE QUE SER 1, 2 O 3'
           END-EVALUATE
           INITIALIZE NUM-VAR
      *
      ********************
      *  EJERCICIO IF 9  *
      ********************
      *
           DISPLAY '***EJERCICIO IF 9***'
           ACCEPT ALF-VAR
           DISPLAY ALF-VAR
           EVALUATE TRUE
               WHEN ALF-VAR = 'A'
                  DISPLAY 'ESTE TEXTO APARECE SI EWSCOJES LA A'
               WHEN ALF-VAR = 'B'
                  DISPLAY 'ESTE TEXTO APARECE SI EWSCOJES LA B'
               WHEN ALF-VAR = 'C'
                  DISPLAY 'ESTE TEXTO APARECE SI EWSCOJES LA C'
               WHEN OTHER
                  DISPLAY 'NO VALIDO TIENE QUE SER A, B & C'
           END-EVALUATE
           INITIALIZE ALF-VAR
      *
      *********************
      *  EJERCICIO IF 10  *
      *********************
      *
           DISPLAY '***EJERCICIO IF 10***'
           ACCEPT NUM-VAR
           DISPLAY NUM-VAR
           EVALUATE TRUE
               WHEN NUM-VAR < 5
                  DISPLAY 'SUSPENSO'
               WHEN NUM-VAR >= 5 AND NUM-VAR <= 7
                  DISPLAY 'APROBADO'
               WHEN NUM-VAR >= 7 AND NUM-VAR <= 8
                  DISPLAY 'NOTABLE'
               WHEN NUM-VAR >= 9 AND NUM-VAR <= 10
                  DISPLAY 'SOBRESALIENTE'
               WHEN OTHER
                  DISPLAY 'NO ES UNA NOTA VALIDA'
           END-EVALUATE
           INITIALIZE NUM-VAR
      *
           STOP RUN.
