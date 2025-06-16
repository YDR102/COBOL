       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  CRUDPGM.
       AUTHOR.      DAVID.
       DATE-WRITTEN 03/06/2025.
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
           SELECT FINCIDE  ASSIGN TO FINCIDE
           FILE STATUS FS-FINCIDE.
      *
           SELECT FSALIDA ASSIGN TO FSALIDA
           FILE STATUS FS-FSALIDA.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD FENTRADA
           RECORDING MODE IS F.
       01  REG-FENTRADA                                      PIC X(192).
      *
       FD FINCIDE
           RECORDING MODE IS F.
       01  REG-FINCIDE                                       PIC X(086).
      *
       FD FSALIDA
           RECORDING MODE IS F.
       01  REG-FSALIDA                                       PIC X(017).
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-FILE-STATUS.
           05  FS-FENTRADA                         PIC X(02).
           05  FS-FSALIDA                          PIC X(02).
           05  FS-FINCIDE                          PIC X(02).
      *
       01  CN-CONTADORES.
           05  CN-REG-LEIDOS-FENTRADA              PIC 9(03).
           05  CN-REG-ESCRIT-FSALIDA               PIC 9(03).
           05  CN-REG-ESCRIT-FINCIDE               PIC 9(03).
      *
       01  CT-CONTANTES.
           05  CT-00                               PIC X(02) VALUE '00'.
           05  CT-10                               PIC X(02) VALUE '10'.
           05  CT-99                               PIC X(02) VALUE '99'.
           05  CT-1                                PIC 9(02) VALUE 1.
           05  CT-100                              PIC 9(03) VALUE 100.
      *
       01 TOTAL-CONTADORES                         PIC 9(03).
      *
         01  CT-RUT.
              05  CT-RUT-1                   PIC X(08) VALUE 'RUTCRUD1'.
              05  CT-RUT-2                   PIC X(08) VALUE 'RUTCRUD2'.
              05  CT-RUT-3                   PIC X(08) VALUE 'RUTCRUD3'.
              05  CT-RUT-4                   PIC X(08) VALUE 'RUTCRUD4'.
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
       COPY CRUDCPYE.
      *
      *CPY DE SALIDA
       COPY CRUDCPYS.
      *
      *CPY DE ERRORES
       COPY CRUDCPYR.
      *
      *CPY DE RUT 1
       COPY CRUDCPY1.
      *
      *CPY DE RUT 2
       COPY CRUDCPY2.
      *
      *CPY DE RUT 3
       COPY CRUDCPY3.
    *
      *CPY DE RUT 4
       COPY CRUDCPY4.
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
                      CRUDCPYS
                      CRUDCPYR
                      CRUDCPYE
                      CRUDCPY1
                      CRUDCPY2
                      CRUDCPY3
                      CRUDCPY4
      *
           SET SW-NO-FIN-FENTRADA               TO TRUE
           SET SW-NO-ERROR                      TO TRUE
      *
           PERFORM 1100-ABRIR-FICHEROS
              THRU 1100-ABRIR-FICHEROS-EXIT
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
           OPEN OUTPUT FSALIDA
           OPEN OUTPUT FINCIDE
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
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FSALIDA'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           IF FS-FINCIDE NOT = CT-00
              DISPLAY 'ERROR AL ABRIR FINCIDE'
              DISPLAY 'PARRAFO: 1100-ABRIR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FINCIDE
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
      * 2000-PROCESO                                                   *
      ******************************************************************
      *
       2000-PROCESO.
      *
           PERFORM 2500-INFORMAR
              THRU 2500-INFORMAR-EXIT

           PERFORM 2600-LLAMADA-RUTINAS
              THRU 2600-LLAMADA-RUTINAS-EXIT

           PERFORM 9000-LEER-FENTRADA
              THRU 9000-LEER-FENTRADA-EXIT
      *
           .
      *
       2000-PROCESO-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2100-INFORMAR-SALIDA                                           *
      ******************************************************************
      *
       2100-INFORMAR-SALIDA.
      *
           DISPLAY 'MOVER ' SALARIO-S-4 ' A SALARIO-S'
           DISPLAY 'MOVER ' FECHA-NACIMIENTO-S-4 ' A FECHA-NACIMIENTO-S'

           MOVE SALARIO-S-4                       TO SALARIO-S
           MOVE FECHA-NACIMIENTO-S-4              TO FECHA-NACIMIENTO-S
      *
           .
      *
       2100-INFORMAR-SALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2200-ESCRIBIR-FSALIDA                                          *
      ******************************************************************
      *
       2200-ESCRIBIR-FSALIDA.
      *
           WRITE REG-FSALIDA        FROM CRUDCPYS
           DISPLAY 'CRUDPGM: ESCRIBIENDO REGISTRO EN FSALIDA'
           DISPLAY '**************************************************'
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR FENTRADA'
              DISPLAY 'PARRAFO: 2200-ESCRIBIR-FSALIDA'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CRUDCPYS
              ADD CT-1                  TO CN-REG-ESCRIT-FSALIDA
           END-IF
      *
           .
      *
       2200-ESCRIBIR-FSALIDA-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2300-ESCRIBIR-FINCIDE                                          *
      ******************************************************************
      *
       2300-ESCRIBIR-FINCIDE.
      *
           WRITE REG-FINCIDE        FROM CRUDCPYR
           DISPLAY 'CRUDPGM: ESCRIBIENDO REGISTRO EN FINCIDE'
           DISPLAY '**************************************************'
      *
           IF FS-FINCIDE NOT = CT-00
              DISPLAY 'ERROR AL ESCRIBIR 2300-ESCRIBIR-FINCIDE'
              DISPLAY 'PARRAFO: 2300-ESCRIBIR-FINCIDE'
              DISPLAY 'FILE STATUS: ' FS-FINCIDE
      *
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           ELSE
              INITIALIZE CRUDCPYR
              ADD CT-1                  TO CN-REG-ESCRIT-FINCIDE
           END-IF
      *
           .
      *
       2300-ESCRIBIR-FINCIDE-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2400-INFORMAR-INCIDE                                           *
      ******************************************************************
      *
       2400-INFORMAR-INCIDE.
      *
           EVALUATE ACCION-E
           WHEN 'A'
               MOVE RETORNO-ERR-1              TO RETORNO
               MOVE PARRAFO-ERR-1              TO PARRAFO
               MOVE DESCRIPCION-ERR-1          TO DESCRIPCION
               MOVE SQLCODE-ERR-1              TO SQLCODE-ERR
           WHEN 'M'
               MOVE RETORNO-ERR-2              TO RETORNO
               MOVE PARRAFO-ERR-2              TO PARRAFO
               MOVE DESCRIPCION-ERR-2          TO DESCRIPCION
               MOVE SQLCODE-ERR-2              TO SQLCODE-ERR
           WHEN 'B'
               MOVE RETORNO-ERR-3              TO RETORNO
               MOVE PARRAFO-ERR-3              TO PARRAFO
               MOVE DESCRIPCION-ERR-3          TO DESCRIPCION
               MOVE SQLCODE-ERR-3              TO SQLCODE-ERR
           WHEN 'C'
               MOVE RETORNO-ERR-4              TO RETORNO
               MOVE PARRAFO-ERR-4              TO PARRAFO
               MOVE DESCRIPCION-ERR-4          TO DESCRIPCION
               MOVE SQLCODE-ERR-4              TO SQLCODE-ERR
           WHEN OTHER
               DISPLAY 'ERROR: ACCION NO VALIDA'
           END-EVALUATE
      *
           .
      *
       2400-INFORMAR-INCIDE-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2500-INFORMAR                                                  *
      ******************************************************************
      *
       2500-INFORMAR.
      *
           EVALUATE ACCION-E
           WHEN 'A'
               DISPLAY 'CRUDPGM: ACCION A REALIZAR: ALTA'
               INITIALIZE CRUDCPY1
      *
               MOVE MATRICULA-E             TO MATRICULA-1
               MOVE NOMBRE-E                TO NOMBRE-1
               MOVE APELLIDO-E              TO APELLIDO-1
               MOVE CATEGORIA-E             TO CATEGORIA-1
               MOVE DEPARTAMENTO-E          TO DEPARTAMENTO-1
               MOVE SECCION-E               TO SECCION-1
               MOVE SALARIO-E               TO SALARIO-1
               MOVE FECHA-INGRESO-E         TO FECHA-INGRESO-1
               MOVE FECHA-NACIMIENTO-E      TO FECHA-NACIMIENTO-1
      *
               CALL CT-RUT-1 USING CRUDCPY1
      *
           WHEN 'M'
               DISPLAY 'CRUDPGM: ACCION A REALIZAR: MODIFICACION'
               INITIALIZE CRUDCPY2
      *
               MOVE MATRICULA-E             TO MATRICULA-2
               MOVE NOMBRE-E                TO NOMBRE-2
               MOVE APELLIDO-E              TO APELLIDO-2
               MOVE CATEGORIA-E             TO CATEGORIA-2
               MOVE DEPARTAMENTO-E          TO DEPARTAMENTO-2
               MOVE SECCION-E               TO SECCION-2
               MOVE SALARIO-E               TO SALARIO-2
               MOVE FECHA-INGRESO-E         TO FECHA-INGRESO-2
               MOVE FECHA-NACIMIENTO-E      TO FECHA-NACIMIENTO-2
      *
               CALL CT-RUT-2 USING CRUDCPY2
      *
           WHEN 'B'
               DISPLAY 'CRUDPGM: ACCION A REALIZAR: BAJA'
               INITIALIZE CRUDCPY3
      *
               MOVE MATRICULA-E             TO MATRICULA-3
      *
               CALL CT-RUT-3 USING CRUDCPY3
      *
           WHEN 'C'
               DISPLAY 'CRUDPGM: ACCION A REALIZAR: CONSULTA'
               INITIALIZE CRUDCPY4
      *
               MOVE MATRICULA-E             TO MATRICULA-4
      *
               CALL CT-RUT-4 USING CRUDCPY4
      *
           WHEN OTHER
               DISPLAY 'ERROR: ACCION NO VALIDA'
           END-EVALUATE
      *
           .
      *
       2500-INFORMAR-EXIT.
           EXIT.
      *
      ******************************************************************
      * 2600-LLAMADA-RUTINAS                                           *
      ******************************************************************
      *
       2600-LLAMADA-RUTINAS.
           DISPLAY 'CRUDPGM: ACCION A REALIZAR: ' ACCION-E
           EVALUATE ACCION-E
               WHEN 'A'
      *****    EVALUAR EL RETORNO DE LA RUTINA CRUD 1
               EVALUATE RETORNO-ERR-1
                   WHEN CT-00
                        DISPLAY 'CRUDPGM: RUTINA CRUD 1 OK'

                        PERFORM 2100-INFORMAR-SALIDA
                           THRU 2100-INFORMAR-SALIDA-EXIT

                        PERFORM 2200-ESCRIBIR-FSALIDA
                           THRU 2200-ESCRIBIR-FSALIDA-EXIT
                   WHEN OTHER
                        DISPLAY 'ERROR: MIRAR FINCIDE'

                        PERFORM 2400-INFORMAR-INCIDE
                           THRU 2400-INFORMAR-INCIDE-EXIT

                        PERFORM 2300-ESCRIBIR-FINCIDE
                           THRU 2300-ESCRIBIR-FINCIDE-EXIT
               END-EVALUATE
               WHEN 'M'
      *****    EVALUAR EL RETORNO DE LA RUTINA CRUD 2
               EVALUATE RETORNO-ERR-2
                   WHEN CT-00
                        DISPLAY 'CRUDPGM: RUTINA CRUD 2 OK'

                        PERFORM 2100-INFORMAR-SALIDA
                           THRU 2100-INFORMAR-SALIDA-EXIT

                        PERFORM 2200-ESCRIBIR-FSALIDA
                           THRU 2200-ESCRIBIR-FSALIDA-EXIT
                   WHEN OTHER
                        DISPLAY 'ERROR: MIRAR FINCIDE'

                        PERFORM 2400-INFORMAR-INCIDE
                           THRU 2400-INFORMAR-INCIDE-EXIT

                        PERFORM 2300-ESCRIBIR-FINCIDE
                           THRU 2300-ESCRIBIR-FINCIDE-EXIT
               END-EVALUATE
               WHEN 'B'
      *****    EVALUAR EL RETORNO DE LA RUTINA CRUD 3
               EVALUATE RETORNO-ERR-3
                   WHEN CT-00
                        DISPLAY 'CRUDPGM: RUTINA CRUD 3 OK'

                        PERFORM 2100-INFORMAR-SALIDA
                           THRU 2100-INFORMAR-SALIDA-EXIT

                        PERFORM 2200-ESCRIBIR-FSALIDA
                           THRU 2200-ESCRIBIR-FSALIDA-EXIT
                   WHEN OTHER
                        DISPLAY 'ERROR: MIRAR FINCIDE'

                        PERFORM 2400-INFORMAR-INCIDE
                           THRU 2400-INFORMAR-INCIDE-EXIT

                        PERFORM 2300-ESCRIBIR-FINCIDE
                           THRU 2300-ESCRIBIR-FINCIDE-EXIT
               END-EVALUATE
               WHEN 'C'
      *****    EVALUAR EL RETORNO DE LA RUTINA CRUD 4
               EVALUATE RETORNO-ERR-4
                   WHEN CT-00
                        DISPLAY 'CRUDPGM: RUTINA CRUD 4 OK'

                        PERFORM 2100-INFORMAR-SALIDA
                           THRU 2100-INFORMAR-SALIDA-EXIT

                        PERFORM 2200-ESCRIBIR-FSALIDA
                           THRU 2200-ESCRIBIR-FSALIDA-EXIT
                   WHEN OTHER
                        DISPLAY 'ERROR: MIRAR FINCIDE'

                        PERFORM 2400-INFORMAR-INCIDE
                           THRU 2400-INFORMAR-INCIDE-EXIT

                        PERFORM 2300-ESCRIBIR-FINCIDE
                           THRU 2300-ESCRIBIR-FINCIDE-EXIT
               END-EVALUATE
               WHEN OTHER
                    DISPLAY 'ERROR: ACCION NO VALIDA'
           END-EVALUATE
      *
           .
      *
       2600-LLAMADA-RUTINAS-EXIT.
           EXIT.
      *
      ******************************************************************
      * 3000-FIN                                                       *
      ******************************************************************
      *
       3000-FIN.
      *
           PERFORM 3200-MOSTRAR-ESTADISTICAS
              THRU 3200-MOSTRAR-ESTADISTICAS-EXIT

           PERFORM 3100-CERRAR-FICHEROS
              THRU 3100-CERRAR-FICHEROS-EXIT

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
           CLOSE FSALIDA
           CLOSE FINCIDE
      *
           IF FS-FENTRADA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FENTRADA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
           END-IF
      *
           IF FS-FSALIDA NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FSALIDA'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FSALIDA
           END-IF
      *
           IF FS-FINCIDE NOT = CT-00
              DISPLAY 'ERROR AL CERRAR FINCIDE'
              DISPLAY 'PARRAFO: 3100-CERRAR-FICHEROS'
              DISPLAY 'FILE STATUS: ' FS-FINCIDE
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
           ADD CN-REG-ESCRIT-FSALIDA TO CN-REG-LEIDOS-FENTRADA
           GIVING TOTAL-CONTADORES

           DISPLAY '***************************'
           DISPLAY '*  ESTADISTICAS DEL  PGM  *'
           DISPLAY '***************************'
           DISPLAY '*REG.LEIDOS     FENTRADA: *' CN-REG-LEIDOS-FENTRADA
           DISPLAY '*REG.ESCRITOS    FSALIDA: *' CN-REG-ESCRIT-FSALIDA
           DISPLAY '*REG.ESCRITOS    FINCIDE: *' CN-REG-ESCRIT-FINCIDE
           DISPLAY '*SALIDA TOTAL CONTADORES: *' TOTAL-CONTADORES
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
           READ FENTRADA INTO CRUDCPYE
           DISPLAY 'CRUDPGM: LEYENDO REGISTRO DE FENTRADA'
      *
           EVALUATE FS-FENTRADA
               WHEN CT-00
                    ADD CT-1                  TO CN-REG-LEIDOS-FENTRADA
               WHEN CT-10
                    SET SW-SI-FIN-FENTRADA TO TRUE
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
