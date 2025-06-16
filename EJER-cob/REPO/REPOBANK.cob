      ******************************************************************
      ** P G M R E P O S.-PGM CON RESPOSICIONAMIENTO QUE LEE LOS      **
      **                  REGISTROS DEL FICHERO DE ENTRADA CON INFO   **
      **                  DE EMPLEADOS Y LOS INSERTA POSTERIORMENTE   **
      **                  EN BBDD.                                    **
      ******************************************************************
      *
      ******************************************************************
      ** IDENTIFICATION DIVISION                                      **
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. REPOBANK.
       AUTHOR. DAVID.
       DATE-WRITTEN. 09/06/2025.
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
           SELECT FENTRADA ASSIGN TO FENTRADA
           FILE STATUS FS-FENTRADA.
      *
           SELECT FINCIDE ASSIGN TO FINCIDE
           FILE STATUS FS-FINCIDE.
      *
      ******************************************************************
      ** DATA DIVISION                                                **
      ******************************************************************
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD FENTRADA
           RECORDING MODE IS F.
       01  REG-FENTRADA               PIC X(191).
      *
       FD FINCIDE
           RECORDING MODE IS F.
       01  REG-FINCIDE                PIC X(191).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA            PIC X(02).
           05  FS-FINCIDE             PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA PIC 9(03).
           05  CN-REG-ESCRIT-FINCIDE  PIC 9(03).
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA        PIC X(01).
               88  SW-SI-FIN-FENTRADA VALUE 'S'.
               88  SW-NO-FIN-FENTRADA VALUE 'N'.
      *
      *COPY DEL FICHERO DE ENTRADA FENTRADA
       COPY CPYBANCO.
       COPY INCBANCO.
      *
      *---------------SQLCA---------------*
      *
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
      *
      *--------DCLGEN EMPLEADOS-----------*
      *
           EXEC SQL
              INCLUDE TBBANCO
           END-EXEC.
      *
      *--------DCLGEN DAREPOS-------------*
      *
           EXEC SQL
              INCLUDE TBDAREPO
           END-EXEC.
      *
      ******************************************************************
      ** PROCEDURE DIVISION                                           **
      ******************************************************************
      *
       PROCEDURE DIVISION.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
             UNTIL SW-SI-FIN-FENTRADA
      *
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT
      *
           .
      *
      ******************************************************************
      ** 1000-INICIO                                                  **
      ******************************************************************
      *
       1000-INICIO.
      *
           INITIALIZE FS-FILE-STATUS
                      CN-CONTADORES
                      CPYBANCO
                      CPYBANCO-ERR
                      DCLBANCO-PICHINCHA
                      DCLDAREPOS
      *
           SET SW-NO-FIN-FENTRADA        TO TRUE
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT
      *
           PERFORM 1200-CONSULTAR-DAREPOS
              THRU 1200-CONSULTAR-DAREPOS-EXIT
      *
           .
      *
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 1100-ABRIR-FICHEROS                                          **
      ******************************************************************
      *
       1100-ABRIR-FICHEROS.
      *
           OPEN INPUT  FENTRADA
           OPEN OUTPUT FINCIDE

      *
           IF FS-FENTRADA NOT = '00'
              DISPLAY 'ERROR AL ABRIR EL FICHERO FENTRADA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
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
      ** 1200-CONSULTAR-DAREPOS                                       **
      ******************************************************************
      *
       1200-CONSULTAR-DAREPOS.
      *
           MOVE 'REPOBANK'                  TO TB-NOMBRE-PGM
      *
           EXEC SQL
               SELECT ESTADO
                     ,VALOR_CLAVE
                 INTO :TB-ESTADO
                     ,:TB-VALOR-CLAVE
                 FROM DAREPOS
                WHERE NOMBRE_PGM = :TB-NOMBRE-PGM
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                    EVALUATE TB-ESTADO
                        WHEN 'KO'
                             PERFORM 9000-LEER-FENTRADA
                                THRU 9000-LEER-FENTRADA-EXIT
                               UNTIL ID-CLIENTE > TB-VALOR-CLAVE-TEXT
                        WHEN 'OK'
                             PERFORM 9000-LEER-FENTRADA
                                THRU 9000-LEER-FENTRADA-EXIT
                        WHEN OTHER
                             DISPLAY 'ERROR: ESTADO INCORRECTO EN DAREP'
                             DISPLAY 'PARRAFO: 1200-CONSULTAR-DAREPOS'
                             DISPLAY 'TABLA: DAREPOS'
      *
                             PERFORM 3000-FIN
                                THRU 3000-FIN-EXIT
                    END-EVALUATE
               WHEN 100
                    PERFORM 1210-INSERTAR-DAREPOS
                       THRU 1210-INSERTAR-DAREPOS-EXIT
      *
                    PERFORM 9000-LEER-FENTRADA
                       THRU 9000-LEER-FENTRADA-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR TECNICO EN BBDD'
                    DISPLAY 'PARRAFO: 1200-CONSULTAR-DAREPOS'
                    DISPLAY 'TABLA: DAREPOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       1200-CONSULTAR-DAREPOS-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 1210-INSERTAR-DAREPOS                                        **
      ******************************************************************
      *
       1210-INSERTAR-DAREPOS.
      *
           MOVE 'KO'                        TO TB-ESTADO
           INITIALIZE TB-VALOR-CLAVE
      *
           EXEC SQL
               INSERT INTO DAREPOS
                      (NOMBRE_PGM
                      ,ESTADO
                      ,VALOR_CLAVE)
                      VALUES(
                       :TB-NOMBRE-PGM
                      ,:TB-ESTADO
                      ,:TB-VALOR-CLAVE)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE
               WHEN -803
                    DISPLAY 'ERROR: REG. DUPLICADO EN BBDD'
                    DISPLAY 'PARRAFO: 1210-INSERTAR-DAREPOS'
                    DISPLAY 'TABLA: DAREPOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR TECNICO EN BBDD'
                    DISPLAY 'PARRAFO: 1210-INSERTAR-DAREPOS'
                    DISPLAY 'TABLA: DAREPOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       1210-INSERTAR-DAREPOS-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 2000-PROCESO                                                 **
      ******************************************************************
      *
       2000-PROCESO.
      *
           PERFORM 2100-INSERT-TABLA
              THRU 2100-INSERT-TABLA-EXIT
      *
           PERFORM 2200-UPDATE-DAREPOS
              THRU 2200-UPDATE-DAREPOS-EXIT
      *
           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 2100-INSERT-TABLA                                            **
      ******************************************************************
      *
       2100-INSERT-TABLA.
      *
           MOVE NUM-CUENTA           TO TB-NUM-CUENTA
           MOVE ID-CLIENTE           TO TB-ID-CLIENTE
           MOVE NOMBRE               TO TB-NOMBRE
           MOVE APELLIDO1            TO TB-APELLIDO1
           MOVE APELLIDO2            TO TB-APELLIDO2
           MOVE IMPORTE              TO TB-IMPORTE
           MOVE DIVISA               TO TB-DIVISA
           MOVE CIUDAD               TO TB-CIUDAD
           MOVE PAIS                 TO TB-PAIS
           MOVE EDAD                 TO TB-EDAD
      *
           EXEC SQL
               INSERT INTO BANCO_PICHINCHA
                      VALUES(
                       :TB-NUM-CUENTA
                      ,:TB-ID-CLIENTE
                      ,:TB-NOMBRE
                      ,:TB-APELLIDO1
                      ,:TB-APELLIDO2
                      ,:TB-IMPORTE
                      ,:TB-DIVISA
                      ,:TB-CIUDAD
                      ,:TB-PAIS
                      ,:TB-EDAD)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE
               WHEN -803
                    DISPLAY 'ERROR: REG. DUPLICADO EN BBDD'
                    DISPLAY 'PARRAFO: 2100-INSERT-EMPLEADOS'
                    DISPLAY 'TABLA: EMPLEADOS'
                    DISPLAY 'SQLCODE: ' SQLCODE

                    PERFORM 2300-ESCRIBIR-FINCIDE
                       THRU 2300-ESCRIBIR-FINCIDE-EXIT

                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR TECNICO EN BBDD'
                    DISPLAY 'PARRAFO: 2100-INSERT-EMPLEADOS'
                    DISPLAY 'TABLA: EMPLEADOS'
                    DISPLAY 'SQLCODE: ' SQLCODE

                    PERFORM 2300-ESCRIBIR-FINCIDE
                       THRU 2300-ESCRIBIR-FINCIDE-EXIT

                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2100-INSERT-TABLA-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 2200-UPDATE-DAREPOS                                          **
      ******************************************************************
      *
       2200-UPDATE-DAREPOS.
      *
           MOVE 'KO'                        TO TB-ESTADO
           MOVE ID-CLIENTE                  TO TB-VALOR-CLAVE-TEXT
           COMPUTE TB-VALOR-CLAVE-LEN =
                   FUNCTION LENGTH(TB-VALOR-CLAVE-TEXT)
      *
           EXEC SQL
               UPDATE DAREPOS
                  SET ESTADO = :TB-ESTADO
                     ,VALOR_CLAVE = :TB-VALOR-CLAVE
                WHERE NOMBRE_PGM = :TB-NOMBRE-PGM
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                    EXEC SQL
                        COMMIT
                    END-EXEC
               WHEN 100
                    DISPLAY 'ERROR: REG. NO ENCONTRADO EN BBDD'
                    DISPLAY 'PARRAFO: 2200-UPDATE-DAREPOS'
                    DISPLAY 'TABLA: EMPLEADOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR TECNICO EN BBDD'
                    DISPLAY 'PARRAFO: 2200-UPDATE-DAREPOS'
                    DISPLAY 'TABLA: EMPLEADOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2200-UPDATE-DAREPOS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2300-ESCRIBIR-FINCIDE                                          *
      ******************************************************************
      *
       2300-ESCRIBIR-FINCIDE.
      *
           MOVE SQLCODE                        TO SQLCODE-ERR
           MOVE '2100-INSERT-EMPLEADOS'        TO DESCRIPCION-ERR

           WRITE REG-FINCIDE       FROM CPYBANCO-ERR
      *
           IF FS-FINCIDE NOT = '00'
              DISPLAY 'ERROR AL ESCRIBIR FINCIDE'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FINCIDE'
              DISPLAY 'FILE STATUS: ' FS-FINCIDE
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CPYBANCO-ERR
              ADD 1                  TO CN-REG-ESCRIT-FINCIDE
           END-IF
      *
           .
      *
       2300-ESCRIBIR-FINCIDE-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 3000-FIN                                                     **
      ******************************************************************
      *
       3000-FIN.
      *
           PERFORM 3100-CERRAR-FICHEROS
              THRU 3100-CERRAR-FICHEROS-EXIT
      *
           PERFORM 3200-MOSTRAR-ESTADISTICAS
              THRU 3200-MOSTRAR-ESTADISTICAS-EXIT
      *
           STOP RUN
      *
           .
      *
       3000-FIN-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 3100-CERRAR-FICHEROS                                         **
      ******************************************************************
      *
       3100-CERRAR-FICHEROS.
      *
           CLOSE FENTRADA
           CLOSE FINCIDE
      *
           IF FS-FENTRADA NOT = '00'
              DISPLAY 'ERROR AL CERRAR EL FICHERO FENTRADA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'NOMBRE FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
           END-IF
      *
           .
      *
       3100-CERRAR-FICHEROS-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 3200-MOSTRAR-ESTADISTICAS                                    **
      ******************************************************************
      *
       3200-MOSTRAR-ESTADISTICAS.
      *
           DISPLAY '************************'
           DISPLAY '*DATOS REPOBANCK       *'
           DISPLAY '************************'
           DISPLAY '*REGEGISTRO FENTRADA:  *' CN-REG-LEIDOS-FENTRADA
           DISPLAY '*REGEGISTRO FINCIDE:   *' CN-REG-ESCRIT-FINCIDE
           DISPLAY '************************'
      *
           .
      *
       3200-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 9000-LEER-FENTRADA                                           **
      ******************************************************************
      *
       9000-LEER-FENTRADA.
      *
           READ FENTRADA INTO CPYBANCO
      *
           EVALUATE FS-FENTRADA
               WHEN '00'
                    ADD 1               TO CN-REG-LEIDOS-FENTRADA
               WHEN '10'
                    SET SW-SI-FIN-FENTRADA     TO TRUE
      *
                    PERFORM 9100-UPDATE-DAREPOS-OK
                       THRU 9100-UPDATE-DAREPOS-OK-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL FICHERO FENTRADA'
                    DISPLAY 'PARRAFO: 9000-LEER-FENTRADA'
                    DISPLAY 'NOMBRE FICHERO: FENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-FENTRADA
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       9000-LEER-FENTRADA-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 9100-UPDATE-DAREPOS-OK                                       **
      ******************************************************************
      *
       9100-UPDATE-DAREPOS-OK.
      *
           MOVE 'OK'                        TO TB-ESTADO
           INITIALIZE TB-VALOR-CLAVE
      *
           EXEC SQL
               UPDATE DAREPOS
                  SET ESTADO = :TB-ESTADO
                     ,VALOR_CLAVE = :TB-VALOR-CLAVE
                WHERE NOMBRE_PGM = :TB-NOMBRE-PGM
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                    CONTINUE
               WHEN 100
                    DISPLAY 'ERROR: REG. NO ENCONTRADO EN BBDD'
                    DISPLAY 'PARRAFO: 9100-UPDATE-DAREPOS-OK'
                    DISPLAY 'TABLA: EMPLEADOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR TECNICO EN BBDD'
                    DISPLAY 'PARRAFO: 9100-UPDATE-DAREPOS-OK'
                    DISPLAY 'TABLA: EMPLEADOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       9100-UPDATE-DAREPOS-OK-EXIT.
           EXIT.
      *
