      ******************************************************************
      *              PGM COBOL DE EJEMPLO  - ESQUELETO
      ******************************************************************
      *       I D E N T I FI C A T I O N   D I V I S I O N
      *
      * SE DEFINE EL NOMBRE DEL PROGRAMA FUENTE
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.    FTEFICH.
       AUTHOR.        BECA.
       DATE-WRITTEN.  02/08/2023.
       DATE-COMPILED. 16/04/2025.
      *
      ******************************************************************
      *       E N V I R O N M E N T     D I V I S I O N
      *
      * DEFINIR FICHEROS QUE VAN A SER USADOS DURANTE EL PROGRAMA
      * RELACIONAR NOMBRES LOGICOS DE LOS ARCHIVOS POR EL PROGRAMA
      * CON LOS NOMBRES REALES DEL JCL
      * DOS DIVISIONES:
      *     - CONFIGURATION SECTION
      *       SE DECLARA EL NOMBRE DE LOS PROCESADORES DONDE SE COMPILA
      *       Y EJECUTA EL PROGRAMA (NO ES OBLIGATORIO)
      *     - INPUT-OUTPUT SECTION
      *     SE DECLARAN DISPOSITIVOS PERIFERICOS (IMPRESORAS...)
      *     TIENE FILE CONTROL QUE ES OBLIGATORIO CUANDO SE USAN
      *     FICHEROS EN EL PROGRAMA, YA QUE LOS DESCRIBE.
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
       FILE-CONTROL.
      * OBLIGATORIO, PARA DECLARAR LOS FICHEROS DE ENTRADA Y SALIDA
      * SELECT -> NOMBRE LOGICO DEL FICHERO EN EL PGM
      * ASSIGN TO-> NOMBRE ETIQUETA EN EL JCL DE UN FICHERO
      * FILE STATUS -> VARIABLE DE ESTADO ASIGNADA A ESE FICHERO
           SELECT FENTRADA
               ASSIGN TO ENTRADA
               FILE STATUS IS FS-FENTRADA.
      *
           SELECT FSALIDA
               ASSIGN TO SALIDA
               FILE STATUS IS FS-FSALIDA.
      *
      ******************************************************************
      *                D A T A      D I V I S I O N
      *
      * IDENTIFICA TODOS LOS NOMBRES DE DATOS UTILIZADOS EN EL PROGRAMA
      * DIVISIONES:
      *     - FILE SECTION
      *       DESCRIBE FICHEROS DE ENTRADA Y SALIDA QUE SE USAN EN EL
      *       PROGRAMA
      *     - WORKING STORAGE SECTION
      *       DEFINE CAMPOS DE TRABAJO NECESARIOS.
      *     - LINKAGE SECTION
      *       SE USA CUANDO EL PROGRAMA SEA UNA RUTINA, YA QUE ES LA
      *       ZONA QUE SIRVE PARA COMUNICARSE CON EL PROGRAMA QUE LA
      *       INVOCA
      ******************************************************************
       DATA DIVISION.
      *
       FILE SECTION.
      *
      ******************************************************************
      *        F I L E     S E C T I O N
      ******************************************************************
      *
       FD  FENTRADA
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD ARE STANDARD
           RECORDING MODE IS F
           DATA RECORD IS REG-ENT.

       01  REG-ENT            PIC X(57).
      *
       FD  FSALIDA
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD ARE STANDARD
           RECORDING MODE IS F
           DATA RECORD IS REG-SAL.

       01  REG-SAL            PIC X(40).
      *
      ******************************************************************
      *        W O R K I N G    S T O R A G E
      ******************************************************************
       WORKING-STORAGE SECTION.
      * LUGAR DONDE DECLARAMOS CONSTANTES, VARIABLES, COPYS, SWITCHES...
      *
       01 CA-CONSTANTES-ALF.
          05 CA-PGM                       PIC X(08) VALUE 'CBLEOI09'.
          05 CA-00                        PIC X(02) VALUE '00'.
          05 CA-10                        PIC X(02) VALUE '10'.
      *
       01 CN-CONSTANTES-NUM.
          05 CN-1                         PIC 9(01) VALUE 1.
      *
       01 WK-VARIABLES.
          05 WK-LEIDOS                    PIC 9(03) VALUE 000.
          05 WK-ESCRITOS                  PIC 9(03) VALUE 000.
      *
       01 FS-FILE-STATUS.
          05 FS-FENTRADA                  PIC X(02).
          05 FS-FSALIDA                   PIC X(02).
      *
       01 SWITCHES.
          05 SW-ERROR                     PIC X(01) VALUE 'N'.
             88 SI-ERROR                            VALUE 'S'.
             88 NO-ERROR                            VALUE 'N'.
      *
          05 SW-FIN-FICHERO               PIC X(01) VALUE 'N'.
             88 SI-FIN-FICHERO                      VALUE 'S'.
             88 NO-FIN-FICHERO                      VALUE 'N'.
      *-- FORMATO REG FICHERO DE SALIDA
       01 CPYSALIDA.
          05 COD-SOCIO-S                 PIC 9(10).
          05 PELICULA-S                  PIC X(20).
          05 FEC-ALQ-S                   PIC X(10).
      *
       01 ALQUILERES.
          05 COD-SOCIO-A                 PIC 9(10).
          05 PELICULA                    PIC X(20).
          05 FEC-ALQ                     PIC X(10).
      *
      *COPY ALQUILER.
      *
      ******************************************************************
      *        P R O C E D U R E     D I V I S I O N
      ******************************************************************
       PROCEDURE DIVISION.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
             UNTIL SI-FIN-FICHERO
      *
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT.
      *
      ******************************************************************
      *   1000-INICIO
      ******************************************************************
       1000-INICIO.
      *
           INITIALIZE WK-VARIABLES
                      FS-FILE-STATUS
                      ALQUILERES
                      CPYSALIDA
      *
           SET NO-ERROR              TO TRUE
           SET NO-FIN-FICHERO        TO TRUE
      *
           PERFORM 1050-ABRIR-ENTRADA
              THRU 1050-ABRIR-ENTRADA-EXIT
      *
           PERFORM 1100-ABRIR-SALIDA
              THRU 1100-ABRIR-SALIDA-EXIT
      *
           PERFORM 9000-LEER-ENTRADA
              THRU 9000-LEER-ENTRADA-EXIT
      *
           .
       1000-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *   1050-ABRIR-ENTRADA
      ******************************************************************
       1050-ABRIR-ENTRADA.
      *
           OPEN INPUT FENTRADA
      *
           IF FS-FENTRADA = CA-00
              CONTINUE
           ELSE
              SET SI-ERROR          TO TRUE
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
      *
           END-IF
      *
           .
       1050-ABRIR-ENTRADA-EXIT.
           EXIT.
      ******************************************************************
      *   1100-ABRIR-SALIDA
      ******************************************************************
       1100-ABRIR-SALIDA.
      *
           OPEN OUTPUT FSALIDA
      *
           IF FS-FSALIDA = CA-00
              CONTINUE
           ELSE
              SET SI-ERROR          TO TRUE
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
      *
           END-IF
      *
           .
       1100-ABRIR-SALIDA-EXIT.
           EXIT.
      ******************************************************************
      *   2000-PROCESO
      ******************************************************************
       2000-PROCESO.
      *
           PERFORM 2200-INFORMAR-SALIDA
              THRU 2200-INFORMAR-SALIDA-EXIT
      *
           PERFORM 2300-ESCRIBIR-SALIDA
              THRU 2300-ESCRIBIR-SALIDA-EXIT
      *
           PERFORM 9000-LEER-ENTRADA
              THRU 9000-LEER-ENTRADA-EXIT
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      ******************************************************************
      *   2200-INFORMAR-SALIDA
      ******************************************************************
       2200-INFORMAR-SALIDA.
      *
           INITIALIZE CPYSALIDA
      *
           MOVE COD-SOCIO-A      TO COD-SOCIO-S
           MOVE PELICULA         TO PELICULA-S
           MOVE FEC-ALQ          TO FEC-ALQ-S
      *
           .
      *
       2200-INFORMAR-SALIDA-EXIT.
           EXIT.
      ******************************************************************
      *   2300-ESCRIBIR-SALIDA
      ******************************************************************
       2300-ESCRIBIR-SALIDA.
      *
           WRITE REG-SAL FROM CPYSALIDA
      *
           IF FS-FSALIDA = CA-00
              INITIALIZE CPYSALIDA
              ADD CN-1           TO WK-ESCRITOS
           ELSE
              SET SI-ERROR       TO TRUE

              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           .
      *
       2300-ESCRIBIR-SALIDA-EXIT.
           EXIT.
      ******************************************************************
      *   3000-FIN
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
              PERFORM 3300-GRABAR-ERROR
                 THRU 3300-GRABAR-ERROR-EXIT
           END-IF
      *
           STOP RUN
      *
           .
      *
       3000-FIN-EXIT.
           EXIT.
      ******************************************************************
      *   3100-CERRAR-FICHEROS
      ******************************************************************
       3100-CERRAR-FICHEROS.
      *
           CLOSE FENTRADA
                 FSALIDA
      *
           IF FS-FENTRADA NOT = CA-00
              SET SI-ERROR         TO TRUE
           END-IF
      *
           IF FS-FSALIDA NOT = CA-00
              SET SI-ERROR         TO TRUE
           END-IF
      *
           .
      *
       3100-CERRAR-FICHEROS-EXIT.
           EXIT.
      ******************************************************************
      *   3200-GRABAR-ESTADIS
      ******************************************************************
       3200-GRABAR-ESTADIS.
      *
           DISPLAY '**************************************'
           DISPLAY '*     ESTADISTICAS SALIDA            *'
           DISPLAY '* REGISTROS LEIDOS   FE:     *' WK-LEIDOS
           DISPLAY '* REGISTROS ESCRITOS FS:     *' WK-ESCRITOS
           DISPLAY '**************************************'
      *
           .
      *
       3200-GRABAR-ESTADIS-EXIT.
           EXIT.
      ******************************************************************
      *   3300-GRABAR-ERROR
      ******************************************************************
       3300-GRABAR-ERROR.
      *
           DISPLAY '**************************************'
           DISPLAY '*    SE HA PRODUCIDO UN ERROR        *'
           DISPLAY '* FILE STATUS DEL    FE:     *' FS-FENTRADA
           DISPLAY '* FILE STATUS DEL    FS:     *' FS-FSALIDA
           DISPLAY '**************************************'
      *
           .
      *
       3300-GRABAR-ERROR-EXIT.
           EXIT.
      ******************************************************************
      *   9000-LEER-ENTRADA
      ******************************************************************
       9000-LEER-ENTRADA.
      *
           INITIALIZE ALQUILERES
      *
           READ FENTRADA INTO ALQUILERES
              AT END
                 SET SI-FIN-FICHERO  TO TRUE
           END-READ
      *
           IF FS-FENTRADA = CA-00
              ADD CN-1               TO WK-LEIDOS
           ELSE
              IF FS-FENTRADA = CA-10
                 DISPLAY 'HE LLEGADO AL FINAL DEL FICHERO'
              ELSE
                 SET SI-ERROR        TO TRUE

                 PERFORM 3000-FIN
                    THRU 3000-FIN-EXIT
              END-IF
           END-IF
           .
      *
       9000-LEER-ENTRADA-EXIT.
           EXIT.
