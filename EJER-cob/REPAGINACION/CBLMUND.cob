      *****************************************************************
      *       PGM COBOL DE EJEMPLO - ESQUELETO                         *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      *                                                                *
      * SE DEFINE EL NOMBRE DEL PROGRAMA FUENTE                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   CBLBRLR.
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
           SELECT FSALIDA
              ASSIGN TO FSALIDA
              FILE STATUS IS FS-FSALIDA.
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
       FD FSALIDA
            BLOCK CONTAINS 0 RECORDS
            LABEL RECORD ARE STANDARD
            RECORDING MODE IS F
            DATA RECORD IS REG-SAL.

       01 REG-SAL             PIC X(150).
      *
      ******************************************************************
      *     W O R K I N G   S T O R A G E                              *
      ******************************************************************
       WORKING-STORAGE SECTION.
      * LUGAR DONDE DECLARAMOS CONSTANTES, VARIABLES, COPYS,SWITCHES,...
      * CONSTANTES ALFANUMERICAS
       01 CA-CONSTANTES-ALF.
          05 CA-PGM                    PIC X(08) VALUE 'CBLMUND'.
          05 CA-RUT                    PIC X(08) VALUE 'RUTMUND'.
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
       COPY CPYMUND.
      *
       01 WK-SALIDA.
          05 SELECCION               PIC X(30).
          05 ENTRENADOR              PIC X(30).
          05 NUM-JUGADORES-CONV      PIC S9(03)V USAGE COMP-3.
          05 FECHA-PRIMER-PARTIDO    PIC X(10).
          05 GRUPO                   PIC X(01).
          05 FASE-ELIMINACION        PIC X(20).
          05 TIMESTAMP-MODIF         PIC X(26).
          05 USUARIO-MODIF           PIC X(30).
      *
       01 WK-INDICES.
          05 IND                 PIC 9(03).

      * SWITCHES PARA EL PROCESO
       01 SWITCHES.
          05 SW-ERROR                  PIC X(01) VALUE 'N'.
             88 SI-ERROR                         VALUE 'S'.
             88 NO-ERROR                         VALUE 'N'.
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
             UNTIL MAS-DATOS = 'N'
      *
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT.
      *
      ******************************************************************
      *     1000-INICIO                                                *
      ******************************************************************
       1000-INICIO.
      *
           INITIALIZE CPYBRLR
                      FS-FILE-STATUS
                      WK-VARIABLES
                      WK-SALIDA
      *
           SET NO-ERROR          TO TRUE
      *
           PERFORM 1100-ABRIR-SALIDA
              THRU 1100-ABRIR-SALIDA-EXIT
      *
           MOVE 3                TO NUM-ELEM-E
           MOVE CA-L             TO OPCION
      *
           .
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     1100-ABRIR-SALIDA                                          *
      ******************************************************************
       1100-ABRIR-SALIDA.
      *
           OPEN OUTPUT FSALIDA

           IF FS-FSALIDA NOT = CA-00
                DISPLAY 'ERROR AL ABRIR FSALIDA'
                DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
                DISPLAY 'FILE STATUS: ' FS-FSALIDA

                PERFORM 3000-FIN
                   THRU 3000-FIN-EXIT
           END-IF
      *
           .
       1100-ABRIR-SALIDA-EXIT.
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
           PERFORM 2450-INFORMAR-SALIDA
              THRU 2450-INFORMAR-SALIDA-EXIT
      *
           MOVE CA-R          TO OPCION
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
           DISPLAY ENTRADA
      *
           CALL CA-RUT USING CPYBRLR
      *
           EVALUATE RETORNO-ERR
              WHEN CA-00
                   ADD 1               TO WK-REG-LLAMADAS
                   ADD NUM-ELEM-S      TO WK-REG-RECUPERADOS
              WHEN OTHER
                   PERFORM 3300-GRABA-ERROR
                      THRU 3300-GRABA-ERROR-EXIT
      *
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2400-LLAMAR-RUTINA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2450-INFORMAR-SALIDA                                       *
      ******************************************************************
       2450-INFORMAR-SALIDA.
      *
           INITIALIZE WK-SALIDA
      *
           PERFORM VARYING IND FROM 1 BY 1
                UNTIL IND > NUM-ELEM-S
                 PERFORM 2500-ESCRIBIR-SALIDA
                     THRU 2500-ESCRIBIR-SALIDA-EXIT

                IF FUNCTION MOD(IND, NUM-ELEM-E) = 0
                     PERFORM 2600-ESCRIBIR-INTERMEDIO
                          THRU 2600-ESCRIBIR-INTERMEDIO-EXIT
                END-IF
           END-PERFORM
      *
           .
       2450-INFORMAR-SALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2500-ESCRIBIR-SALIDA                                       *
      ******************************************************************
       2500-ESCRIBIR-SALIDA.
      *
           MOVE SALIDA-TB(IND)    TO WK-SALIDA
      *
           WRITE REG-SAL     FROM WK-SALIDA

           IF FS-FSALIDA NOT = CA-00
                DISPLAY 'ERROR AL ESCRIBIR FSALIDA'
                DISPLAY 'PARRAFO: 2300-ESCRIBIR-FSALIDA'
                DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
                SET SI-ERROR      TO TRUE
      *
                PERFORM 3000-FIN
                   THRU 3000-FIN-EXIT
           ELSE
                INITIALIZE WK-SALIDA
                ADD 1             TO WK-ESCRITOS-1
           END-IF
      *
           .
       2500-ESCRIBIR-SALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2500-ESCRIBIR-SALIDA                                       *
      ******************************************************************
       2600-ESCRIBIR-INTERMEDIO.
      *
           MOVE SPACES                                      TO REG-SAL
           MOVE '************** REPAGINANDO **************' TO REG-SAL
           WRITE REG-SAL
      *
           IF FS-FSALIDA NOT = CA-00
                DISPLAY 'ERROR AL ESCRIBIR FSALIDA'
                DISPLAY 'PARRAFO: 2600-ESCRIBIR-INTERMEDIO'
                DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
                SET SI-ERROR      TO TRUE
      *
                PERFORM 3000-FIN
                   THRU 3000-FIN-EXIT
           ELSE
                CONTINUE
           END-IF
      *
           .
       2600-ESCRIBIR-INTERMEDIO-EXIT.
           EXIT.
      ******************************************************************
      *     3000-FIN                                                   *
      ******************************************************************
       3000-FIN.
      *
           PERFORM 3100-CERRAR-FICHEROS
              THRU 3100-CERRAR-FICHEROS-EXIT
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
      *     3100-CERRAR-FICHEROS                                       *
      ******************************************************************
       3100-CERRAR-FICHEROS.
      *
           CLOSE FSALIDA

      *
           IF FS-FSALIDA  NOT = CA-00
              DISPLAY 'ERROR AL CERRAR FSALIDA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
              SET SI-ERROR    TO TRUE
           END-IF
           .
       3100-CERRAR-FICHEROS-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3200-GRABAR-ESTADIS                                        *
      ******************************************************************
       3200-GRABAR-ESTADIS.
      *
           DISPLAY '*******************************'
           DISPLAY '*  ESTADISTICAS SALIDA        *'
           DISPLAY '* REGISTROS ESCRITOS S1:      *' WK-ESCRITOS-1
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
           DISPLAY '* RETORNO     DEL ERROR:   *' RETORNO-ERR
           DISPLAY '* SUBRETORNO  DEL ERROR:   *' SUBRETORNO-ERR
           DISPLAY '* ACCION      DEL ERROR:   *' ACCION-ERR
           DISPLAY '* SQLCODE PGM DEL ERROR:   *' SQLCODE-ERR
           DISPLAY '* TABLA       DEL ERROR:   *' TABLA-ERR
           DISPLAY '* PARRAFO     DEL ERROR:   *' PARRAFO-ERR
           DISPLAY '* NOMBRE RUT  DEL ERROR:   *' NOMRUTINA-ERR
           DISPLAY '****************************'
      *
           .
       3300-GRABA-ERROR-EXIT.
           EXIT.
      *
