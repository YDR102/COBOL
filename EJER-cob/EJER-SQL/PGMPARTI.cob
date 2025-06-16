      ******************************************************************
      *                     E  X  T  C  L  I                           *
      ******************************************************************
      *     I D E N T I F I C A T I O N  D I V I S I O N               *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   PGMPARTI.
       AUTHOR.       DAVID.
       DATE-WRITTEN. 22/05/2025.
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
           SELECT PARTICU  ASSIGN TO PARTICU
           FILE STATUS FS-PARTICU.
      *
           SELECT EMPRESA ASSIGN TO EMPRESA
           FILE STATUS FS-EMPRESA.
      *
      ******************************************************************
      *     DATA DIVISION                                              *
      ******************************************************************
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD PARTICU
            BLOCK CONTAINS 0 RECORDS
            LABEL RECORD ARE STANDARD
            RECORDING MODE IS F
            DATA RECORD IS REG-PARTICU.
       01  REG-PARTICU                                       PIC X(125).
      *
       FD EMPRESA
            BLOCK CONTAINS 0 RECORDS
            LABEL RECORD ARE STANDARD
            RECORDING MODE IS F
            DATA RECORD IS REG-EMPRESA.
       01  REG-EMPRESA                                       PIC X(125).
      *
      ******************************************************************
      *     W O R K I N G   S T O R A G E                              *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-EMPRESA                          PIC X(02).
           05  FS-PARTICU                          PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-REG-ESCRIT-EMPRESA               PIC 9(03).
           05  CN-REG-ESCRIT-PARTICU               PIC 9(03).
           05 TB-CONT                              PIC 9(05).
      *
       01  SW-SWITCHES.
           05  SW-FIN-OCCURS                       PIC X(01).
               88  SW-SI-FIN-OCCURS                VALUE 'S'.
               88  SW-NO-FIN-OCCURS                VALUE 'N'.
      *
       01 CT-CONSTANTES.
          05 CT-RUT                    PIC X(08) VALUE 'RUTPARTI'.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-10                     PIC X(02) VALUE '10'.
          05 CT-01                     PIC 9(01) VALUE 1.
          05 CT-E                      PIC X(01) VALUE 'E'.
          05 CT-P                      PIC X(01) VALUE 'P'.
      *
       01 WK-INDICES.
          05 WK-SQLCODE                   PIC -999.

      * SWITCHES PARA EL PROCESO
       01 SWITCHES.
          05 SW-ERROR                  PIC X(01) VALUE 'N'.
             88 SI-ERROR                         VALUE 'S'.
             88 NO-ERROR                         VALUE 'N'.
      *
      *COPY DE COMUNICACION CON LA RUTINA
       COPY CPYPARTI.

       01 CPY-PARTICU.
           05 ID-CLIENTE-PAR     PIC X(10).
           05 NOMBRE-PAR         PIC X(50).
           05 DNI-CIF-PAR        PIC X(15).
           05 EMAIL-PAR          PIC X(50).

       01 CPY-EMPRESA.
           05 ID-CLIENTE-EMP     PIC X(10).
           05 NOMBRE-EMP         PIC X(50).
           05 DNI-CIF-EMP        PIC X(15).
           05 EMAIL-EMP          PIC X(50).
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
           INITIALIZE CPYPARTI
                      TB-CONT
                      WK-SQLCODE
      *
           SET NO-ERROR          TO TRUE
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT
      *
           .
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 1100-ABRIR-FICHEROS                                            *
      ******************************************************************
      *
       1100-ABRIR-FICHEROS.
      *
           OPEN OUTPUT PARTICU
           OPEN OUTPUT EMPRESA
      *
           IF FS-PARTICU NOT = CT-00
              DISPLAY 'ERROR AL ABRIR PARTICU'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-PARTICU
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-EMPRESA NOT = CT-00
              DISPLAY 'ERROR AL ABRIR EMPRESA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-EMPRESA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           .
      *
       1100-ABRIR-FICHEROS-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2000-PROCESO                                               *
      ******************************************************************
       2000-PROCESO.
      *
           PERFORM 2100-LLAMAR-RUTINA
              THRU 2100-LLAMAR-RUTINA-EXIT
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2100-LLAMAR-RUTINA                                         *
      ******************************************************************
       2100-LLAMAR-RUTINA.
      *
           DISPLAY 'CALL A LA RUTINA'
      *
           CALL CT-RUT USING CPYPARTI
      *
           EVALUATE RETORNO-ERR
              WHEN CT-00
                   PERFORM VARYING TB-CONT FROM 1 BY 1
                   UNTIL TB-CONT > REG-RECUPERADOS
                          PERFORM 2400-RESOLUCIO-RUTINA
                             THRU 2400-RESOLUCIO-RUTINA-EXIT
                   END-PERFORM
              WHEN OTHER

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2100-LLAMAR-RUTINA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-PARTICU                                          *
      ******************************************************************
      *
       2200-ESCRIBIR-PARTICU.
      *
           WRITE REG-PARTICU  FROM CPY-PARTICU

           IF FS-PARTICU  NOT = CT-00
                DISPLAY 'ERROR AL ESCRIBIR PARTICU '
                DISPLAY 'PARRAFO: 2300-ESCRIBIR-PARTICU '
                DISPLAY 'FILE STATUS: ' FS-PARTICU
      *
                PERFORM 3000-FIN
                   THRU 3000-FIN-EXIT
           ELSE
                INITIALIZE CPY-PARTICU
                ADD CT-01 TO CN-REG-ESCRIT-PARTICU
           END-IF

      *
           .
      *
       2200-ESCRIBIR-PARTICU-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2300-ESCRIBIR-EMPRESA                                          *
      ******************************************************************
      *
       2300-ESCRIBIR-EMPRESA.
      *
           MOVE SALIDA-OCC(TB-CONT) TO CPY-EMPRESA
                     WRITE REG-EMPRESA FROM CPY-EMPRESA

           IF FS-EMPRESA NOT = CT-00
                DISPLAY 'ERROR AL ESCRIBIR EMPRESA'
                DISPLAY 'PARRAFO: 2300-ESCRIBIR-EMPRESA'
                DISPLAY 'FILE STATUS: ' FS-EMPRESA
      *
                PERFORM 3000-FIN
                   THRU 3000-FIN-EXIT
           ELSE
                INITIALIZE CPY-PARTICU
                ADD CT-01 TO CN-REG-ESCRIT-EMPRESA
           END-IF
      *
           .
      *
       2300-ESCRIBIR-EMPRESA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     2400-RESOLUCIO-RUTINA                                      *
      ******************************************************************
       2400-RESOLUCIO-RUTINA.
      *
               DISPLAY 'RESOLUCIO DE LA RUTINA'
           EVALUATE TIPO-CLIENTE(TB-CONT)
               WHEN CT-P
                   MOVE ID-CLIENTE(TB-CONT)   TO ID-CLIENTE-PAR
                   MOVE NOMBRE(TB-CONT)       TO NOMBRE-PAR
                   MOVE DNI-CIF(TB-CONT)      TO DNI-CIF-PAR
                   MOVE EMAIL(TB-CONT)        TO EMAIL-PAR
      *
                   PERFORM 2200-ESCRIBIR-PARTICU
                      THRU 2200-ESCRIBIR-PARTICU-EXIT
               WHEN CT-E
                   MOVE ID-CLIENTE(TB-CONT)   TO ID-CLIENTE-EMP
                   MOVE NOMBRE(TB-CONT)       TO NOMBRE-EMP
                   MOVE DNI-CIF(TB-CONT)      TO DNI-CIF-EMP
                   MOVE EMAIL(TB-CONT)        TO EMAIL-EMP
      *
                   PERFORM 2300-ESCRIBIR-EMPRESA
                      THRU 2300-ESCRIBIR-EMPRESA-EXIT
               WHEN OTHER
                       DISPLAY 'TIPO DE CLIENTE NO VALIDO'
                       DISPLAY 'PARRAFO: 2400-RESOLUCIO-RUTINA'
                       DISPLAY 'TIPO DE CLIENTE: 'TIPO-CLIENTE(TB-CONT)
           END-EVALUATE
      *
           .
      *
       2400-RESOLUCIO-RUTINA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3000-FIN                                                   *
      ******************************************************************
       3000-FIN.
      *
           IF NO-ERROR
                PERFORM 3200-CERRAR-FICHEROS
                   THRU 3200-CERRAR-FICHEROS-EXIT
                PERFORM 3300-MOSTRAR-ESTADISTICAS
                   THRU 3300-MOSTRAR-ESTADISTICAS-EXIT
           ELSE
                PERFORM 3100-MOSTRAR-ERROR
                   THRU 3100-MOSTRAR-ERROR-EXIT
           END-IF
      *

           STOP RUN.
      *
       3000-FIN-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3100-MOSTRAR-ERROR                                           *
      ******************************************************************
       3100-MOSTRAR-ERROR.
      *
           DISPLAY '****************************'
           DISPLAY '* SE HA PRODUCIDO UN ERROR *'
           DISPLAY '* RETORNO     DEL ERROR:   *' RETORNO-ERR
           DISPLAY '* SUBRETORNO  DEL ERROR:   *' SUBRETORNO-ERR
           DISPLAY '* DESCRIPCION DEL ERROR:   *' DESCRIPCION-ERR
           DISPLAY '* CAMPO       DEL ERROR:   *' CAMPO-ERR
           DISPLAY '* PARRAFO     DEL ERROR:   *' PARRAFO-ERR
           DISPLAY '****************************'
      *
           .
       3100-MOSTRAR-ERROR-EXIT.
           EXIT.
      *
      *
      ******************************************************************
      * 3200-CERRAR-FICHEROS                                           *
      ******************************************************************
      *
       3200-CERRAR-FICHEROS.
      *
           CLOSE PARTICU
           CLOSE EMPRESA
      *
           IF FS-PARTICU NOT = CT-00
              DISPLAY 'ERROR AL CERRAR PARTICU'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-PARTICU
           END-IF
      *
           IF FS-EMPRESA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR EMPRESA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-EMPRESA
           END-IF
      *
           .
      *
       3200-CERRAR-FICHEROS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3300-MOSTRAR-ESTADISTICAS                                      *
      ******************************************************************
      *
       3300-MOSTRAR-ESTADISTICAS.
      *
           DISPLAY '***************************************************'
           DISPLAY '*       ESTADISTICAS DEL PGM PGMFICH              *'
           DISPLAY '***************************************************'
           DISPLAY '*REG.ESCRITOS PARTICU: ' CN-REG-ESCRIT-PARTICU  '  '
                   '                      *'
           DISPLAY '*REG.ESCRITOS EMPRESA: ' CN-REG-ESCRIT-EMPRESA  '  '
                   '                      *'
           DISPLAY '***************************************************'
      *
           .
      *
       3300-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.
