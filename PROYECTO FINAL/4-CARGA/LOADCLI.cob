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
       PROGRAM-ID. LOADCLI.
       AUTHOR. DAVID.
       DATE-WRITTEN. 06/06/2025.
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
       01  REG-FENTRADA               PIC X(0694).
      * CLIENTES
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA            PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA PIC 9(03).
      *
       01  WK-SQLCODE                   PIC -999.
      *
       01  SW-SWITCHES.
           05  SW-FIN-FENTRADA        PIC X(01).
               88  SW-SI-FIN-FENTRADA VALUE 'S'.
               88  SW-NO-FIN-FENTRADA VALUE 'N'.
      *
      *COPY DEL FICHERO DE ENTRADA FENTRADA
       COPY CPYEREPO.
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
              INCLUDE TBEMPLE
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
      ** INICIALIZAR VARIABLES                                        **
      ** APERTURA DE FICHEROS                                         **
      ******************************************************************
      *
       1000-INICIO.
      *
           INITIALIZE FS-FILE-STATUS
                      CN-CONTADORES
                      CPY-CPYEREPO
                      DCLEMPLEADOS
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
      ** ABRIMOS LOS FICHEROS DE ENTRADA Y SALIDA COMPROBANDO SU      **
      ** FILE STATUS.                                                 **
      ******************************************************************
      *
       1100-ABRIR-FICHEROS.
      *
           OPEN INPUT  FENTRADA
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
      ** CONSULTAMOS A LA TABLA DAREPOS CON EL NOMBRE DE NUESTRO PGM  **
      ** PARA RECUPERAR EL ESTADO DE LA ULTIMA EJECUCION. SI ES OK    **
      ** EMPEZAREMOS A PROCESAR EL FICHERO DESDE EL REGISTRO 1, SI ES **
      ** KO, REELEREMOS EL FICHERO DE ENTRADA HASTA POSICIONARME EN EL**
      ** REGISTRO SIGUIENTE AL ULTIMO QUE SE PROCESO CORRECTAMENTE. SI**
      ** LA CONSULTA NOS DEVUELVE UN NOT FOUND, INSERTAREMOS EL PGM   **
      ** EN LA DAREPOS (SIMBOLIZA QUE ES LA PRIMERA VEZ QUE SE EJECUTA**
      ** ESTE PROCESO).                                               **
      ******************************************************************
      *
       1200-CONSULTAR-DAREPOS.
      *
           MOVE 'PGMREPOS'                  TO TB-NOMBRE-PGM
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
                               UNTIL MATRICULA-E > TB-VALOR-CLAVE-TEXT
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
      ** INSERTAMOS EN LA TABLA DAREPOS EL REGISTRO ASOCIADO A NUESTRO**
      ** PGM, DE MANERA QUE YA CONSTARIA COMO QUE EL PGM HA SIDO      **
      ** EJECUTADO EN ALGUN MOMENTO.                                  **
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
      ** INSERTAMOS EL ULTIMO REGISTRO EN LA TABLA EMPLEADOS. A       **
      ** CONTINUACION ACTUALIZAMOS LA DAREPOS CON EL VALOR CLAVE DEL  **
      ** REGISTRO QUE ACABAMOS DE INSERTAR EN LA TABLA EMPLEADOS.     **
      ******************************************************************
      *
       2000-PROCESO.
      *
           PERFORM 2100-INSERT-EMPLEADOS
              THRU 2100-INSERT-EMPLEADOS-EXIT
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
      ** 2100-INSERT-EMPLEADOS                                        **
      ** INSERTAMOS EL REGISTRO LEIDO DEL FICHERO EN LA TABLA EMPLEADOS*
      ** CONTROLANDO POSTERIORMENTE SU SQLCODE.                       **
      ******************************************************************
      *
       2100-INSERT-EMPLEADOS.
      *
           MOVE MATRICULA-E                 TO TB-MATRICULA
           MOVE APELLIDO-E                  TO TB-APELLIDO
           MOVE NOMBRE-E                    TO TB-NOMBRE
           MOVE CATEGORIA-E                 TO TB-CATEGORIA
           MOVE DEPARTAMENTO-E              TO TB-DEPARTAMENTO
           MOVE SECCION-E                   TO TB-SECCION
           MOVE SALARIO-E                   TO TB-SALARIO
           MOVE FECHA-ING-E                 TO TB-FECHA-INGRESO
           MOVE FECHA-NAC-E                 TO TB-FECHA-NACIMIENTO
      *
           EXEC SQL
               INSERT INTO EMPLEADOS
                      (MATRICULA
                      ,APELLIDO
                      ,NOMBRE
                      ,CATEGORIA
                      ,DEPARTAMENTO
                      ,SECCION
                      ,SALARIO
                      ,FECHA_ING
                      ,FECHA_NAC)
                      VALUES(
                       :TB-MATRICULA
                      ,:TB-APELLIDO
                      ,:TB-NOMBRE
                      ,:TB-CATEGORIA
                      ,:TB-DEPARTAMENTO
                      ,:TB-SECCION
                      ,:TB-SALARIO
                      ,:TB-FECHA-INGRESO
                      ,:TB-FECHA-NACIMIENTO)
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
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR: ERROR TECNICO EN BBDD'
                    DISPLAY 'PARRAFO: 2100-INSERT-EMPLEADOS'
                    DISPLAY 'TABLA: EMPLEADOS'
                    DISPLAY 'SQLCODE: ' SQLCODE
      *
                    PERFORM 3000-FIN
                       THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
      *
       2100-INSERT-EMPLEADOS-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 2200-UPDATE-DAREPOS                                          **
      ** ACTUALIZAMOS EL VALOR DE LA CLAVE EN LA DAREPOS CON LA        *
      ** MATRICULA QUE ACABAMOS DE INSSERTAR EN LA TABLA EMPLEADOS.   **
      ******************************************************************
      *
       2200-UPDATE-DAREPOS.
      *
           MOVE 'KO'                        TO TB-ESTADO
           MOVE MATRICULA-E                 TO TB-VALOR-CLAVE-TEXT
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
      ** 3000-FIN                                                     **
      ** MOSTRAMOS LOS DATOS DEL PRESTAMO.                            **
      ** FINALIZAMOS EL PGM.                                          **
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
      ** CERRAMOS LOS FICHEROS DE ENTRADA CONTROLANDO SU              **
      ** FILE STATUS.                                                 **
      ******************************************************************
      *
       3100-CERRAR-FICHEROS.
      *
           CLOSE FENTRADA
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
      ** MOSTRAMOS ESTADISTICAS DEL PGM IMPRIMIENTO LOS REG. LEIDOS   **
      ** DEL FICHERO DE ENTRADA.                                      **
      ******************************************************************
      *
       3200-MOSTRAR-ESTADISTICAS.
      *
           DISPLAY '*******************************'
           DISPLAY '**D A T O S   P G M R E P O S**'
           DISPLAY '*******************************'
           DISPLAY '*REG.LEIDOS FENTRADA: 'CN-REG-LEIDOS-FENTRADA '   '
                   '  *'
           DISPLAY '*******************************'
      *
           .
      *
       3200-MOSTRAR-ESTADISTICAS-EXIT.
           EXIT.
      *
      ******************************************************************
      ** 9000-LEER-FENTRADA                                           **
      ** LEEMOS REGISTRO DEL FICHERO FENTRADA CONTROLANDO SU FILE     **
      ** STATUS.                                                      **
      ******************************************************************
      *
       9000-LEER-FENTRADA.
      *
           READ FENTRADA INTO CPY-CPYEREPO
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
      ** ACTUALIZAMOS EL ESTADO A OK PARA INDICAR QUE EL PROGRAMA A    *
      ** FINALIZADO POR COMPLETO PROCESANDO TODOS LOS REGISTROS DEL   **
      ** FICHERO.                                                     **
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
