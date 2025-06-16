      ******************************************************************
      **      I D E N T I F I C A T I O N   D I V I S I O N           **
      ******************************************************************
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. RUTREPOS.
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
      ******************************************************************
      **      D A T A   D I V I S I O N                               **
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       EXEC SQL INCLUDE SQLCA    END-EXEC.
       EXEC SQL INCLUDE TBCUENTA END-EXEC.
      *----------------------------------------------------------------*
       LINKAGE SECTION.
      *----------------------------------------------------------------*
       COPY CPYREPOS.
      ******************************************************************
      **      P R O C E D U R E   D I V I S I O N                     **
      ******************************************************************
       PROCEDURE DIVISION USING CPYREPOS.
      *----------------------------------------------------------------*
           PERFORM INICIO
              THRU INICIO-EXIT
      *
           PERFORM PROCESO
              THRU PROCESO-EXIT
      *
           PERFORM FIN
              THRU FIN-EXIT
           .
      *--------------------- INICIO -----------------------------------*
       INICIO.
           INITIALIZE ERRORES
                      DCLCUENTAS
      *
           MOVE ZEROES TO RETORNO
      *
           PERFORM VALIDAR-CAMPOS
              THRU VALIDAR-CAMPOS-EXIT
           .
      *
       INICIO-EXIT.
           EXIT.
      *--------------------- VALIDAR CAMPOS ---------------------------*
       VALIDAR-CAMPOS.
           IF BANCO  = SPACES OR LOW-VALUES
              MOVE '88'                            TO RETORNO
              MOVE '01'                            TO SUBRETORNO
              MOVE 'CAMPO BANCO  VACIO'            TO ACCION
              MOVE 'VALIDAR-CAMPOS'                TO PARRAFO
              PERFORM FIN
                 THRU FIN-EXIT
           END-IF
      *
           IF OFICINA = SPACES OR LOW-VALUES
              MOVE '88'                            TO RETORNO
              MOVE '01'                            TO SUBRETORNO
              MOVE 'CAMPO OFICINA VACIO'           TO ACCION
              MOVE 'VALIDAR-CAMPOS'                TO PARRAFO
              PERFORM FIN
                 THRU FIN-EXIT
           END-IF
      *
           IF DC = SPACES OR LOW-VALUES
              MOVE '88'                            TO RETORNO
              MOVE '01'                            TO SUBRETORNO
              MOVE 'CAMPO DC VACIO'                TO ACCION
              MOVE 'VALIDAR-CAMPOS'                TO PARRAFO
              PERFORM FIN
                 THRU FIN-EXIT
           END-IF
      *
           IF NUM-CUENTA = SPACES OR LOW-VALUES
              MOVE '88'                            TO RETORNO
              MOVE '01'                            TO SUBRETORNO
              MOVE 'CAMPO NUM-CUENTA VACIO'        TO ACCION
              MOVE 'VALIDAR-CAMPOS'                TO PARRAFO
              PERFORM FIN
                 THRU FIN-EXIT
           END-IF
      *
           IF TITULAR = SPACES OR LOW-VALUES
              MOVE '88'                            TO RETORNO
              MOVE '01'                            TO SUBRETORNO
              MOVE 'CAMPO TITULAR VACIO'           TO ACCION
              MOVE 'VALIDAR-CAMPOS'                TO PARRAFO
              PERFORM FIN
                 THRU FIN-EXIT
           END-IF
      *
           IF SALDO = ZEROES
              MOVE '88'                            TO RETORNO
              MOVE '02'                            TO SUBRETORNO
              MOVE 'CAMPO SALDO VACIO'             TO ACCION
              MOVE 'VALIDAR-CAMPOS'                TO PARRAFO
              PERFORM FIN
                 THRU FIN-EXIT
           END-IF
           .
       VALIDAR-CAMPOS-EXIT.
           EXIT
           .
      *--------------------- PROCESO ----------------------------------*
       PROCESO.
           PERFORM INFORMAR-DCLGEN
              THRU INFORMAR-DCLGEN-EXIT
      *
           PERFORM INSERT-CUENTAS
              THRU INSERT-CUENTAS-EXIT
           .
       PROCESO-EXIT.
           EXIT
           .
      *--------------------- INFORMAR DCLGEN --------------------------*
       INFORMAR-DCLGEN.
           MOVE BANCO                   TO TB-BANCO
           MOVE OFICINA                 TO TB-OFICINA
           MOVE DC                      TO TB-DC
           MOVE NUM-CUENTA              TO TB-NUM-CUENTA
           MOVE TITULAR                 TO TB-TITULAR
           MOVE SALDO                   TO TB-SALDO
           .
       INFORMAR-DCLGEN-EXIT.
           EXIT
           .
      *--------------------- INSERT CUENTAS ---------------------------*
       INSERT-CUENTAS.
           EXEC SQL
              INSERT INTO CUENTAS VALUES( :TB-BANCO,
                                          :TB-OFICINA,
                                          :TB-DC,
                                          :TB-NUM-CUENTA,
                                          :TB-TITULAR,
                                          :TB-SALDO )
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                  CONTINUE
               WHEN -803
                  MOVE '88'                     TO RETORNO
                  MOVE '88'                     TO SUBRETORNO
                  MOVE 'REGISTRO DUPLICADO'     TO ACCION
                  MOVE 'CUENTAS'                TO TABLA
                  MOVE 'INSERT-CUENTAS'         TO PARRAFO
                  MOVE 'RUTREPOS'               TO NOMRUTINA
                  MOVE SQLCODE                  TO SQLCODE-E
                  PERFORM FIN
                     THRU FIN-EXIT
               WHEN OTHER
                  MOVE '99'                     TO RETORNO
                  MOVE '10'                     TO SUBRETORNO
                  MOVE 'INSERT'                 TO ACCION
                  MOVE 'CUENTAS'                TO TABLA
                  MOVE 'INSERT-CUENTAS'         TO PARRAFO
                  MOVE 'RUTREPOS'               TO NOMRUTINA
                  MOVE SQLCODE                  TO SQLCODE-E
                  PERFORM FIN
                     THRU FIN-EXIT
           END-EVALUATE
           .
       INSERT-CUENTAS-EXIT.
           EXIT
           .
      *--------------------- FIN --------------------------------------*
       FIN.
           GOBACK
           .
      *
       FIN-EXIT.
           EXIT
           .
      *
