      ******************************************************************
      **      I D E N T I F I C A T I O N   D I V I S I O N           **
      ******************************************************************
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. REPOS.
       AUTHOR. FRAN.
      ******************************************************************
      **      E N V I R O N M E N T   D I V I S I O N                 **
      ******************************************************************
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *--------------------- FICHEROS DE ENTRADA ----------------------*
           SELECT FENTRADA
           ASSIGN TO FENTRADA
           FILE STATUS IS FS-FENTRADA.
      ******************************************************************
      **      D A T A   D I V I S I O N                               **
      ******************************************************************
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.
       FD FENTRADA RECORDING MODE F.
       01 REG-FENTRADA            PIC X(47).
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *--------------------- CONSTANTES ALFANUMERICAS -----------------*
       01 CA-CONSTANTES-ALFANUMERICAS.
          05 CA-00                PIC X(02)         VALUE '00'.
          05 CA-10                PIC X(02)         VALUE '10'.
          05 CA-RUTREPOS          PIC X(08)         VALUE 'RUTREPOS'.
          05 CA-REPOS             PIC X(05)         VALUE 'REPOS'.
          05 CA-KO                PIC X(02)         VALUE 'KO'.
          05 CA-OK                PIC X(02)         VALUE 'OK'.
      *--------------------- CONSTANTES NUMERICAS ---------------------*
       01 CN-CONSTANTES-NUMERICAS.
          05 CN-100               PIC 9(03)         VALUE 100.
      *--------------------- FILE STATUS ------------------------------*
       01 FS-FILE-STATUS.
          05 FS-FENTRADA          PIC X(02).
      *--------------------- SWITCHES ---------------------------------*
       01 SW-FIN-FENTRADA         PIC X(01).
          88 SI-FIN-FENTRADA      VALUE 'S'.
          88 NO-FIN-FENTRADA      VALUE 'N'.
      *--------------------- CONTADORES -------------------------------*
       01 CNT-CONTADORES.
          05 CNT-REG-FENTRADA     PIC 9(09).
          05 CNT-REG-INSERT       PIC 9(09).
          05 CNT-REG-UPDATE       PIC 9(09).
      *--------------------- VARIABLES --------------------------------*
       01 WK-VARIABLES.
          05 WK-CLAVE-ENT.
             10 BANCO-CLAVE       PIC X(04).
             10 OFICINA-CLAVE     PIC X(04).
             10 DC-CLAVE          PIC X(02).
             10 NUM-CUENTA-CLAVE  PIC X(10).
      *--------------------- COPY DE RUTINA ---------------------------*
       COPY CPYREPOS.
      *--------------------- DECLARACIONES SQL ------------------------*
       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE TBDAREPO END-EXEC.
      ******************************************************************
      **      P R O C E D U R E   D I V I S I O N                     **
      ******************************************************************
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
           PERFORM INICIO
              THRU INICIO-EXIT
      *
           PERFORM PROCESO
              THRU PROCESO-EXIT
             UNTIL SI-FIN-FENTRADA
      *
           PERFORM FIN
              THRU FIN-EXIT
           .
      *--------------------- INICIO -----------------------------------*
       INICIO.
           INITIALIZE CNT-CONTADORES
                      FS-FILE-STATUS
                      CPYREPOS
                      DCLDAREPOS
      *
           SET NO-FIN-FENTRADA TO TRUE
      *
           PERFORM ABRIR-FICHEROS
              THRU ABRIR-FICHEROS-EXIT
      *
           MOVE CA-REPOS TO TB-NOMBRE-PGM
      *
           PERFORM CONSULTAR-DAREPOS
              THRU CONSULTAR-DAREPOS-EXIT
           .
      *
       INICIO-EXIT.
           EXIT.
      *--------------------- ABRIR-FICHEROS ---------------------------*
       ABRIR-FICHEROS.
           OPEN INPUT FENTRADA
      *
           IF FS-FENTRADA NOT = CA-00
              DISPLAY 'ERROR EN LA APERTURA DEL FICHERO DE FENTRADA'
              DISPLAY 'NOMBRE DEL FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
              PERFORM FIN
                 THRU FIN-EXIT
           END-IF
      *
           .
      *
       ABRIR-FICHEROS-EXIT.
           EXIT.
      *--------------------- CONSULTAR DAREPOS -------------------------*
       CONSULTAR-DAREPOS.
      *
           EXEC SQL
              SELECT ESTADO,
                     VALOR_CLAVE
              INTO :TB-ESTADO,
                   :TB-VALOR-CLAVE-TEXT
              FROM DAREPOS
              WHERE NOMBRE_PGM = :TB-NOMBRE-PGM
           END-EXEC
      *
           EVALUATE SQLCODE
      *
              WHEN 0
      *
                 EVALUATE TB-ESTADO
                    WHEN CA-KO
                       DISPLAY 'VOY A LEER ENTRADA'
                       PERFORM LEER-ENTRADA
                          THRU LEER-ENTRADA-EXIT
                         UNTIL WK-CLAVE-ENT > TB-VALOR-CLAVE-TEXT
      *
                    WHEN CA-OK
                       PERFORM LEER-ENTRADA
                          THRU LEER-ENTRADA-EXIT
      *
                    WHEN OTHER
                       DISPLAY 'ERROR: CAMPO TB-ESTADO NO VALIDO'
                       PERFORM FIN
                          THRU FIN-EXIT
                 END-EVALUATE
      *
              WHEN 100
      *
                 PERFORM INSERTAR-DAREPOS
                    THRU INSERTAR-DAREPOS-EXIT
                 PERFORM LEER-ENTRADA
                    THRU LEER-ENTRADA-EXIT
      *
              WHEN OTHER
      *
                 DISPLAY 'ERROR SELECT DE DAREPOS'
                 DISPLAY 'SQLCODE: ' SQLCODE
                 DISPLAY 'PARRAFO: CONSULTAR-DAREPOS'
                 DISPLAY 'TABLA: DAREPOS'
                 PERFORM FIN
                    THRU FIN-EXIT
           END-EVALUATE
           .
       CONSULTAR-DAREPOS-EXIT.
           EXIT
           .
      *--------------------- INSERTAR DAREPOS --------------------------*
       INSERTAR-DAREPOS.
           MOVE CA-KO    TO TB-ESTADO
           INITIALIZE TB-VALOR-CLAVE-TEXT
      *
           EXEC SQL
              INSERT INTO DAREPOS VALUES (:TB-NOMBRE-PGM,
                                          :TB-ESTADO,
                                          :TB-VALOR-CLAVE-TEXT)
           END-EXEC
      *
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 DISPLAY 'ERROR AL INSERTAR EN DAREPOS POR REG DUPLI'
                 DISPLAY 'RETORNO: 99'
                 DISPLAY 'SQLCODE: ' SQLCODE
                 DISPLAY 'PARRAFO: INSERTAR-DAREPOS'
                 DISPLAY 'TABLA  : DAREPOS'
                 DISPLAY 'ERROR: NO ES PUSIBLA'
                 PERFORM FIN
                    THRU FIN-EXIT
              WHEN OTHER
                 DISPLAY 'ERROR AL INSERTAR EN DAREPOS0'
                 DISPLAY 'RETORNO: 99'
                 DISPLAY 'SQLCODE: ' SQLCODE
                 DISPLAY 'PARRAFO: INSERTAR-DAREPOS'
                 DISPLAY 'TABLA  : DAREPOS'
                 PERFORM FIN
                    THRU FIN-EXIT
           END-EVALUATE
           .
       INSERTAR-DAREPOS-EXIT.
           EXIT
           .
      *--------------------- LEER ENTRADA ------------------------------*
       LEER-ENTRADA.
           READ FENTRADA INTO ENTRADA
      *
           EVALUATE FS-FENTRADA
      *
             WHEN CA-00
                ADD 1 TO CNT-REG-FENTRADA
                MOVE BANCO           TO BANCO-CLAVE
                MOVE OFICINA         TO OFICINA-CLAVE
                MOVE DC              TO DC-CLAVE
                MOVE NUM-CUENTA      TO NUM-CUENTA-CLAVE
      *
             WHEN CA-10
                DISPLAY 'HE LEIDO Y SE HA ACABADO'
                SET SI-FIN-FENTRADA TO TRUE
                 PERFORM DAREPOS-OK
                    THRU DAREPOS-OK-EXIT
      *
             WHEN OTHER
                DISPLAY 'HE LEIDO Y ALGO HA IDO MUY MUY MAL'
                DISPLAY 'ERROR AL LEER FICHERO DE FENTRADA'
                DISPLAY 'NOMBRE DEL FICHERO: FENTRADA'
                DISPLAY 'FILE STATUS: ' FS-FENTRADA
                PERFORM FIN
                   THRU FIN-EXIT
      *
           END-EVALUATE
           .
      *
       LEER-ENTRADA-EXIT.
           EXIT
           .
      *--------------------- PROCESO ----------------------------------*
       PROCESO.
           PERFORM LLAMAR-RUTINA
              THRU LLAMAR-RUTINA-EXIT
      *
           PERFORM UPDATE-DAREPOS
              THRU UPDATE-DAREPOS-EXIT
      *
           PERFORM LEER-ENTRADA
              THRU LEER-ENTRADA-EXIT
           .
       PROCESO-EXIT.
           EXIT
           .
      *--------------------- LLAMAR RUTINA ----------------------------*
       LLAMAR-RUTINA.
           CALL CA-RUTREPOS        USING CPYREPOS
      *
           EVALUATE RETORNO
               WHEN CA-00
                  ADD 1 TO CNT-REG-INSERT
               WHEN OTHER
                  DISPLAY 'HA HABIDO UN ERROR EN LA RUTINA'
                  DISPLAY 'RETORNO         :' RETORNO
                  DISPLAY 'SUBRETORNO      :' SUBRETORNO
                  DISPLAY 'ACCION          :' ACCION
                  DISPLAY 'TABLA           :' TABLA
                  DISPLAY 'PARRAFO         :' PARRAFO
                  DISPLAY 'SQLCODE         :' SQLCODE-E
                  PERFORM FIN
                     THRU FIN-EXIT
           END-EVALUATE
           .
       LLAMAR-RUTINA-EXIT.
           EXIT
           .
      *--------------------- UPDATE DAREPOS ---------------------------*
       UPDATE-DAREPOS.
           MOVE WK-CLAVE-ENT TO TB-VALOR-CLAVE-TEXT
           MOVE CA-REPOS  TO TB-NOMBRE-PGM
      *
           EXEC SQL
              UPDATE DAREPOS
              SET VALOR_CLAVE = :TB-VALOR-CLAVE-TEXT
              WHERE NOMBRE_PGM = :TB-NOMBRE-PGM
           END-EXEC
      *
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN OTHER
                 DISPLAY 'ERROR EN EL UPDATE DE DAREPOS'
                 DISPLAY 'PARRAFO: UPDATE-DAREPOS'
                 DISPLAY 'TABLA: DAREPOS'
                 DISPLAY 'SQLCODE: ' SQLCODE
                 PERFORM FIN
                    THRU FIN-EXIT
           END-EVALUATE
      *
           EXEC SQL
              COMMIT
           END-EXEC
           .
       UPDATE-DAREPOS-EXIT.
           EXIT
           .
      *--------------------- FIN --------------------------------------*
       FIN.
           PERFORM CERRAR-FICHEROS
              THRU CERRAR-FICHEROS-EXIT
           PERFORM MOSTRAR-ESTADISTICAS
              THRU MOSTRAR-ESTADISTICAS-EXIT
           STOP RUN
           .
      *
       FIN-EXIT.
           EXIT
           .
      *--------------------- DAREPOS OK -------------------------------*
       DAREPOS-OK.
           MOVE CA-OK TO TB-ESTADO
           INITIALIZE TB-VALOR-CLAVE-TEXT
      *
           EXEC SQL
              UPDATE DAREPOS SET ESTADO = :TB-ESTADO,
                                 VALOR_CLAVE = :TB-VALOR-CLAVE-TEXT
              WHERE NOMBRE_PGM = :TB-NOMBRE-PGM
           END-EXEC
      *
           EVALUATE SQLCODE
              WHEN 0
                 ADD 1 TO CNT-REG-UPDATE
              WHEN OTHER
                 DISPLAY 'ERROR AL PONER OK EN DAREPOS'
                 DISPLAY 'PARRAFO: DAREPOS-OK'
                 DISPLAY 'TABLA: DAREPOS'
                 DISPLAY 'SQLCODE: ' SQLCODE
                 PERFORM FIN
                    THRU FIN-EXIT
           END-EVALUATE

           .
      *
       DAREPOS-OK-EXIT.
           EXIT
           .
      *--------------------- CERRAR-FICHEROS --------------------------*
       CERRAR-FICHEROS.
           CLOSE FENTRADA
      *
           IF FS-FENTRADA NOT = CA-00
              DISPLAY 'ERROR AL CERRAR EL FICHERO DE FENTRADA'
              DISPLAY 'NOMBRE DEL FICHERO: FENTRADA'
              DISPLAY 'FILE STATUS: ' FS-FENTRADA
           END-IF
           .
      *
       CERRAR-FICHEROS-EXIT.
           EXIT
           .
      *--------------------- MOSTRAR-ESTADISTICAS ---------------------*
       MOSTRAR-ESTADISTICAS.
           DISPLAY '**********************************************'
           DISPLAY '* E S T A D I S T I C A S                    *'
           DISPLAY '*--------------------------------------------*'
           DISPLAY '* REGISTROS LEIDOS   DE FENTRADA : '
                                              CNT-REG-FENTRADA ' *'
           DISPLAY '* REGISTROS INSERTADOS EN CUENTAS: '
                                              CNT-REG-INSERT   ' *'
           DISPLAY '* REGISTROS UPDATEADOS EN DAREPOS: '
                                              CNT-REG-UPDATE   ' *'
           DISPLAY '**********************************************'
           .
      *
       MOSTRAR-ESTADISTICAS-EXIT.
           EXIT
           .
      *
