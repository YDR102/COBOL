      ******************************************************************
      *                     E  X  T  C  L  I                           *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   PGMCURS.
       AUTHOR.       DAVID.
       DATE-WRITTEN. 19/05/2025.
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
      * SE DECLARAN LOS FICHEROS DE ENTRADA Y SALIDA
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
      *     W O R K I N G   S T O R A G E                              *
      ******************************************************************
       WORKING-STORAGE SECTION.
      * LUGAR DONDE DECLARAMOS CONSTANTES, VARIABLES, COPYS,SWITCHES,...
      * CONSTANTES ALFANUMERICAS
       01 CA-CONSTANTES-ALF.
          05 CA-RUT                    PIC X(08) VALUE 'RUTCURS'.
          05 CA-00                     PIC X(02) VALUE '00'.
          05 CA-10                     PIC X(02) VALUE '10'.
          05 CA-L                      PIC X(01) VALUE 'L'.
          05 CA-R                      PIC X(01) VALUE 'R'.
      *
       01 FS-FILE-STATUS.
          05 FS-FSALIDA                PIC X(02).
      *
       01 WK-VARIABLES.
          05 WK-REG-RECUPERADOS        PIC 9(03).
          05 WK-REG-LLAMADAS           PIC 9(03).
          05 WK-ESCRITOS-1             PIC 9(03).
      *
       01 WK-INDICES.
          05 IND                 PIC 9(03).

      * SWITCHES PARA EL PROCESO
       01 SWITCHES.
          05 SW-ERROR                  PIC X(01) VALUE 'N'.
             88 SI-ERROR                         VALUE 'S'.
             88 NO-ERROR                         VALUE 'N'.
      *
      *COPY DE COMUNICACION CON LA RUTINA
       COPY CPYCURS.
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
           INITIALIZE CPYCURS
                      WK-VARIABLES
      *
           SET NO-ERROR          TO TRUE
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
           PERFORM 2400-LLAMAR-RUTINA
              THRU 2400-LLAMAR-RUTINA-EXIT
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2400-LLAMAR-RUTINA                                         *
      * ****************************************************************
       2400-LLAMAR-RUTINA.
      *
           DISPLAY 'CALL A LA RUTINA'
      *
           CALL CA-RUT USING CPYCURS
      *
           EVALUATE COD-RETORNO
              WHEN CA-00
                   ADD 1               TO WK-REG-LLAMADAS
              WHEN OTHER
                   DISPLAY COD-RETORNO
                   DISPLAY COD-SUBRETORNO
                   DISPLAY PARRAFO
                   DISPLAY DESCRIPCION
                   DISPLAY TABLA
                   DISPLAY SQLCODE-E

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2400-LLAMAR-RUTINA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3000-FIN                                                   *
      ******************************************************************
       3000-FIN.
      *
           IF NO-ERROR
              PERFORM 3200-GRABAR-ESTADIS
                 THRU 3200-GRABAR-ESTADIS-EXIT
           ELSE
              PERFORM 3300-GRABA-ERROR
                 THRU 3300-GRABA-ERROR-EXIT
           END-IF
      *
           STOP RUN.
       3000-FIN-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3200-GRABAR-ESTADIS                                        *
      ******************************************************************
       3200-GRABAR-ESTADIS.
      *
           DISPLAY '*******************************'
           DISPLAY '*  ESTADISTICAS SALIDA        *'
           DISPLAY '* LLAMADAS A LA RUTINA :      *' WK-REG-LLAMADAS
           DISPLAY '* REGISTROS RECUPERADOS:      *' SAL-LEIDOS
           DISPLAY '*******************************'
      *
           .
       3200-GRABAR-ESTADIS-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3300-GRABA-ERROR                                           *
      ******************************************************************
       3300-GRABA-ERROR.
      *
           DISPLAY '****************************'
           DISPLAY '* SE HA PRODUCIDO UN ERROR *'
           DISPLAY '* DESCRIPCION DEL ERROR:   *' COD-RETORNO
           DISPLAY '* DESCRIPCION DEL ERROR:   *' COD-SUBRETORNO
           DISPLAY '* DESCRIPCION DEL ERROR:   *' PARRAFO
           DISPLAY '* DESCRIPCION DEL ERROR:   *' DESCRIPCION
           DISPLAY '* DESCRIPCION DEL ERROR:   *' TABLA
           DISPLAY '* DESCRIPCION DEL ERROR:   *' SQLCODE-E
           DISPLAY '****************************'
      *
           .
       3300-GRABA-ERROR-EXIT.
           EXIT.
      *
