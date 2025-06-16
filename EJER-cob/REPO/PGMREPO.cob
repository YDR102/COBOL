       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  PGMREPO.
       AUTHOR.      DAVID.
       DATE-WRITTEN 05/06/2025.
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
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD FENTRADA
           RECORDING MODE IS F.
       01  REG-FENTRADA                                      PIC X(192).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA                         PIC X(02).

      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA              PIC 9(05).
           05  CN-REG-ESCRIT-BASEDATO              PIC 9(05).
      *
       01  CT-CONTANTES.
           05  CT-00                               PIC X(02) VALUE '00'.
           05  CT-10                               PIC X(02) VALUE '10'.
           05  CT-99                               PIC X(02) VALUE '99'.
           05  CT-1                                PIC 9(02) VALUE 1.
           05  CT-100                              PIC 9(03) VALUE 100.
      *
       01  PGM                                PIC X(20) VALUE 'PGMREPO'.
       01  WK-SQLCODE                         PIC -999.
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA                     PIC X(01).
               88  SW-SI-FIN-FENTRADA                         VALUE 'S'.
               88  SW-NO-FIN-FENTRADA                         VALUE 'N'.
          05 SW-ERROR                            PIC X(01).
               88  SW-SI-ERROR                                VALUE 'S'.
               88  SW-NO-ERROR                                VALUE 'N'.
      *
      *CPY DE ENTRADA
       COPY CPYREPO.
      *---------------- SQLCA ------------------------
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *---------------- DCLGEN -----------------------
           EXEC SQL
               INCLUDE TBEMPLE
           END-EXEC.
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
      * 1000-INICIO                                                    *
      ******************************************************************
      *
       1000-INICIO.
      *
           INITIALIZE FS-FILE-STATUS
                      CN-CONTADORES
      *
           SET SW-NO-FIN-FENTRADA               TO TRUE
           SET SW-NO-ERROR                      TO TRUE
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT

           PERFORM 1200-CONSULTAR-DAREPOS
              THRU 1200-CONSULTAR-DAREPOS-EXIT

           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
      *
           .
      *
       1000-INICIO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 1100-ABRIR-FICHEROS                                            *
      ******************************************************************
      *
       1100-ABRIR-FICHEROS.
      *
           OPEN INPUT FENTRADA
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FENTRADA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
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
      * 1200-CONSULTAR-DAREPOS                                         *
      ******************************************************************
      *
       1200-CONSULTAR-DAREPOS.
      *
           MOVE PGM TO TB-NOMBRE-PGM
           DISPLAY 'PGMREPO: CONSULTANDO DAREPOS'
           EXEC SQL
               SELECT ESTADO, VALOR_CLAVE
               INTO :TB-ESTADO, :TB-VALOR-CLAVE
               FROM DAREPOS
               WHERE NOMBRE_PGM = :TB-NOMBRE-PGM
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                    DISPLAY 'CONSULTA DAREPOS'
                    EVALUATE TB-ESTADO
                        WHEN 'KO'
                             DISPLAY 'ESTADO: KO'
                             PERFORM 9000-LEER-FENTRADA
                             THRU 9000-LEER-FENTRADA-EXIT
                             UNTIL TB-VALOR-CLAVE-TEXT < MATRICULA
                        WHEN 'OK'
                             DISPLAY 'ESTADO: OK'
                             PERFORM 9000-LEER-FENTRADA
                                THRU 9000-LEER-FENTRADA-EXIT
                    END-EVALUATE
               WHEN 100
                    DISPLAY 'PGMREPO: INCRIBIENDO ESTADO'
                    PERFORM 1300-INSERTAR-DAREPOS
                       THRU 1300-INSERTAR-DAREPOS-EXIT

                    PERFORM 9000-LEER-FENTRADA
                       THRU 9000-LEER-FENTRADA-EXIT
               WHEN OTHER
                    MOVE SQLCODE TO WK-SQLCODE
                    DISPLAY 'ERROR AL CONSULTAR DAREPOS'
                    DISPLAY 'PARRAFO: 1200-CONSULTAR-DAREPOS'
                    DISPLAY 'SQLCODE: ' WK-SQLCODE

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
      * 1300-INSERTAR-DAREPOS                                          *
      ******************************************************************
      *
       1300-INSERTAR-DAREPOS.
      *
           MOVE 'KO'             TO TB-ESTADO
           INITIALIZE TB-VALOR-CLAVE

           EXEC SQL
               INSERT INTO DAREPOS
               VALUES (
                    :TB-NOMBRE-PGM
                   ,:TB-ESTADO
                   ,:TB-VALOR-CLAVE)
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                  CONTINUE
                  DISPLAY 'INSERTAR DAREPOS FUNCIONO'
               WHEN -803
                  DISPLAY 'BBDD: ERROR REG DUPLICADO'
                  DISPLAY 'PARRAFO: 1300-INSERTAR-DAREPOS'

                  PERFORM 3000-FIN
                     THRU 3000-FIN-EXIT
               WHEN OTHER
                  MOVE SQLCODE TO WK-SQLCODE
                  DISPLAY
                  'PGMREPO: ERROR DESCONOCIDO EN INSERTAR-DAREPOS'
                  DISPLAY 'SQLCODE: ' WK-SQLCODE

                  PERFORM 3000-FIN
                     THRU 3000-FIN-EXIT
           END-EVALUATE

      *
           .
      *
       1300-INSERTAR-DAREPOS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2000-PROCESO                                                   *
      ******************************************************************
      *
       2000-PROCESO.
      *
           PERFORM 2100-INSERTAR-TABLA
              THRU 2100-INSERTAR-TABLA-EXIT

           PERFORM 2200-UPDATE-DAREPO-KO
              THRU 2200-UPDATE-DAREPO-KO-EXIT

           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INSERTAR-TABLA                                            *
      ******************************************************************
      *
       2100-INSERTAR-TABLA.
      *
           MOVE MATRICULA        TO TB-MATRICULA
           MOVE APELLIDO         TO TB-APELLIDO
           MOVE NOMBRE           TO TB-NOMBRE
           MOVE CATEGORIA        TO TB-CATEGORIA
           MOVE DEPARTAMENTO     TO TB-DEPARTAMENTO
           MOVE SECCION          TO TB-SECCION
           MOVE SALARIO          TO TB-SALARIO
           MOVE FECHA-INGRESO    TO TB-FECHA-INGRESO
           MOVE FECHA-NACIMIENTO TO TB-FECHA-NACIMIENTO
           EXEC SQL
               INSERT INTO EMPLEADOS
               VALUES (
                       :TB-MATRICULA
                       ,:TB-APELLIDO
                       ,:TB-NOMBRE
                       ,:TB-CATEGORIA
                       ,:TB-DEPARTAMENTO
                       ,:TB-SECCION
                       ,:TB-SALARIO
                       ,:TB-FECHA-INGRESO
                       ,:TB-FECHA-NACIMIENTO
               )
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
                   DISPLAY 'PGMREPO: INSERT EXITOSO'
                   ADD CT-1    TO CN-REG-ESCRIT-BASEDATO
               WHEN -803
                  DISPLAY 'BBDD: ERROR REG DUPLICADO'
                  DISPLAY '2100-INSERTAR-TABLA'

                  PERFORM 3000-FIN
                     THRU 3000-FIN-EXIT
               WHEN OTHER
                  MOVE SQLCODE TO WK-SQLCODE
                  DISPLAY
                  'PGMREPO: ERROR DESCONOCIDO EN INSERTAR-TABLA'
                  DISPLAY 'SQLCODE: ' WK-SQLCODE

                  PERFORM 3000-FIN
                     THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2100-INSERTAR-TABLA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-UPDATE-DAREPO-KO                                          *
      ******************************************************************
      *
       2200-UPDATE-DAREPO-KO.
      *
           MOVE 'KO'             TO TB-ESTADO
           MOVE MATRICULA        TO TB-VALOR-CLAVE-TEXT
           COMPUTE TB-VALOR-CLAVE-LEN =
           FUNCTION LENGTH(TB-VALOR-CLAVE-TEXT)

           EXEC SQL
               UPDATE DAREPOS
               SET ESTADO = :TB-ESTADO,
                   VALOR_CLAVE = :TB-VALOR-CLAVE
               WHERE NOMBRE_PGM = :TB-NOMBRE-PGM
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                  CONTINUE
                  DISPLAY 'PGMREPO: UPDATE EXITOSO'
                  EXEC SQL
                    COMMIT
                  END-EXEC
               WHEN 100
                  DISPLAY 'BBDD: ERROR REG NO ENCONTRADO'
                  DISPLAY '2200-UPDATE-DAREPO-KO'

                  PERFORM 3000-FIN
                     THRU 3000-FIN-EXIT
               WHEN OTHER
                  MOVE SQLCODE TO WK-SQLCODE
                  DISPLAY
                  'PGMREPO: ERROR DESCONOCIDO EN UPDATE-DAREPOS-OK'
                  DISPLAY 'SQLCODE: ' WK-SQLCODE

                  PERFORM 3000-FIN
                     THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2200-UPDATE-DAREPO-KO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2300-ESCRIBIR-BASEDATOS                                          *
      ******************************************************************
      *
       2300-ESCRIBIR-BASEDATOS.
      *
           MOVE MATRICULA        TO TB-MATRICULA
           MOVE APELLIDO         TO TB-APELLIDO
           MOVE NOMBRE           TO TB-NOMBRE
           MOVE CATEGORIA        TO TB-CATEGORIA
           MOVE DEPARTAMENTO     TO TB-DEPARTAMENTO
           MOVE SECCION          TO TB-SECCION
           MOVE SALARIO          TO TB-SALARIO
           MOVE FECHA-INGRESO    TO TB-FECHA-INGRESO
           MOVE FECHA-NACIMIENTO TO TB-FECHA-NACIMIENTO

           EXEC SQL
               INSERT INTO EMPLEADOS
               VALUES (
                   :TB-MATRICULA,
                   :TB-APELLIDO,
                   :TB-NOMBRE,
                   :TB-CATEGORIA,
                   :TB-DEPARTAMENTO,
                   :TB-SECCION,
                   :TB-SALARIO,
                   :TB-FECHA-INGRESO,
                   :TB-FECHA-NACIMIENTO)

           END-EXEC
           DISPLAY 'CRUDPGM: ESCRIBIENDO BASE DE DATOS'
           DISPLAY '**************************************************'
      *
           IF SQLCODE NOT = 000
              MOVE SQLCODE TO WK-SQLCODE
              DISPLAY 'ERROR AL ESCRIBIR EN LA BASE DE DATOS'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-BASEDATOS'
              DISPLAY 'SQLCODE: ' WK-SQLCODE
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              ADD CT-1                  TO CN-REG-ESCRIT-BASEDATO
           END-IF
      *
           .
      *
       2300-ESCRIBIR-BASEDATOS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3000-FIN                                                       *
      ******************************************************************
      *
       3000-FIN.
      *

           PERFORM 3100-CERRAR-FICHEROS
              THRU 3100-CERRAR-FICHEROS-EXIT

           PERFORM 3200-MOSTRAR-ESTADISTICAS
              THRU 3200-MOSTRAR-ESTADISTICAS-EXIT

           STOP RUN
      *
           .
      *
       3000-FIN-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3100-CERRAR-FICHEROS                                           *
      ******************************************************************
      *
       3100-CERRAR-FICHEROS.
      *
           CLOSE FENTRADA
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FENTRADA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
           END-IF
      *
           .
      *
       3100-CERRAR-FICHEROS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3200-MOSTRAR-ESTADISTICAS                                      *
      ******************************************************************
      *
       3200-MOSTRAR-ESTADISTICAS.
      *
           DISPLAY '***************************'
           DISPLAY '*  ESTADISTICAS DEL  PGM  *'
           DISPLAY '***************************'
           DISPLAY '*REG.LEIDOS     FENTRADA: *' CN-REG-LEIDOS-FENTRADA
           DISPLAY '*REG.ESCRITOS BASE DATOS: *' CN-REG-ESCRIT-BASEDATO
           DISPLAY '***************************'
      *
           .
      *
       3200-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 9000-LEER-FENTRADA                                             *
      ******************************************************************
      *
       9000-LEER-FENTRADA.
      *
           READ FENTRADA INTO CPYREPO
           DISPLAY 'PGMREPO: LEYENDO REGISTRO DE FENTRADA'
           DISPLAY
           '**********************************************************'
      *
           EVALUATE FS-FENTRADA
               WHEN CT-00
                    ADD CT-1               TO CN-REG-LEIDOS-FENTRADA
               WHEN CT-10
                    SET SW-SI-FIN-FENTRADA TO TRUE
                    PERFORM 9100-UPDATE-DAREPOS-OK
                       THRU 9100-UPDATE-DAREPOS-OK-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL FICHERO FENTRADA'
                    DISPLAY 'PARRAFO: 9000-LEER-FENTRADA'
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
      * 9100-UPDATE-DAREPOS-OK                                         *
      ******************************************************************
      *
       9100-UPDATE-DAREPOS-OK.
      *
           MOVE 'OK'             TO TB-ESTADO
           INITIALIZE TB-VALOR-CLAVE

           EXEC SQL
               UPDATE DAREPOS
               SET ESTADO = :TB-ESTADO,
                   VALOR_CLAVE = :TB-VALOR-CLAVE
               WHERE NOMBRE_PGM = :TB-NOMBRE-PGM
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                  CONTINUE
                  DISPLAY 'PGMREPO: UPDATE EXITOSO PROGRAMA FIANLIZADO'
                  EXEC SQL
                    COMMIT
                  END-EXEC
               WHEN 100
                  DISPLAY 'BBDD: ERROR REG NO ENCONTRADO'
                  DISPLAY '9100-UPDATE-DAREPOS-OK'
                  PERFORM 3000-FIN
                     THRU 3000-FIN-EXIT
               WHEN OTHER
                  MOVE SQLCODE TO WK-SQLCODE
                  DISPLAY
                  'PGMREPO: ERROR DESCONOCIDO EN PDATE-DAREPOS-OK'
                  DISPLAY 'SQLCODE: ' WK-SQLCODE

                  PERFORM 3000-FIN
                     THRU 3000-FIN-EXIT
           END-EVALUATE
           EXEC SQL
               COMMIT
           END-EXEC
      *
           .
      *
       9100-UPDATE-DAREPOS-OK-EXIT.
           EXIT.