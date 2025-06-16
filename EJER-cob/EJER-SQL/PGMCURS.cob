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
      * CONSTANTES
       01 CT-CONSTANTES.
          05 CT-RUT                    PIC X(08) VALUE 'RUTCURS'.
          05 CT-00                     PIC X(02) VALUE '00'.
          05 CT-10                     PIC X(02) VALUE '10'.
          05 CT-01                     PIC 9(01) VALUE 1.
          05 CT-L                      PIC X(01) VALUE 'L'.
          05 CT-R                      PIC X(01) VALUE 'R'.
      *
       01 CONTADORES.
          05 TB-CONT                   PIC 9(03).
      *
       01 FS-FILE-STATUS.
          05 FS-FSALIDA                PIC X(02).
      *
       01 WK-VARIABLES.
          05 WK-REG-LLAMADAS           PIC 9(03).
          05 WK-ESCRITOS-1             PIC 9(03).
      *
       01 WK-INDICES.
          05 IND                       PIC 9(03).

      * SWITCHES PARA EL PROCESO
       01 SWITCHES.
          05 SW-ERROR                  PIC X(01) VALUE 'N'.
             88 SI-ERROR                         VALUE 'S'.
             88 NO-ERROR                         VALUE 'N'.
      *
      *COPY DE COMUNICACION CON LA RUTINA
       COPY 'CURSORES'.
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
           INITIALIZE CPY-CPYCURS
                      WK-VARIABLES
                      TB-CONT
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
           CALL CT-RUT USING CPY-CPYCURS
      *
           EVALUATE RETORNO-ERR
              WHEN CT-00
                   PERFORM VARYING TB-CONT FROM 1 BY 1
                   UNTIL TB-CONT >= REG-RECUPERADOS
                       DISPLAY '**************************************'
                       DISPLAY 'RESGRISTRO:    ' TB-CONT
                       DISPLAY '**************************************'
                       DISPLAY 'ID-PEDIDO:     ' ID-PEDIDO    (TB-CONT)
                       DISPLAY 'ID-CLIENTE:    ' ID-CLIENTE   (TB-CONT)
                       DISPLAY 'FECHA-PEDIDO:  ' FECHA-PEDIDO (TB-CONT)
                       DISPLAY 'IMPORTE-TOTAL: ' IMPORTE-TOTAL(TB-CONT)
                       DISPLAY 'ESTADO:        ' ESTADO       (TB-CONT)
                       DISPLAY 'TIPO-ENVIO:    ' TIPO-ENVIO   (TB-CONT)
                       DISPLAY 'COMENTARIOS:   ' COMENTARIOS  (TB-CONT)
                   END-PERFORM
              WHEN OTHER

                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
           END-EVALUATE
      *
           .
       2100-LLAMAR-RUTINA-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3000-FIN                                                   *
      ******************************************************************
       3000-FIN.
      *
           IF NO-ERROR
              PERFORM 3100-GRABAR-ESTADIS
                 THRU 3100-GRABAR-ESTADIS-EXIT
           ELSE
              PERFORM 3200-GRABA-ERROR
                 THRU 3200-GRABA-ERROR-EXIT
           END-IF
           STOP RUN.
      *
       3000-FIN-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3100-GRABAR-ESTADIS                                        *
      ******************************************************************
       3100-GRABAR-ESTADIS.
      *
           DISPLAY '*******************************'
           DISPLAY '*  ESTADISTICAS SALIDA        *'
           DISPLAY '* LLAMADAS A LA RUTINA :      *' WK-REG-LLAMADAS
           DISPLAY '* REGISTROS RECUPERADOS:      *' REG-RECUPERADOS
           DISPLAY '*******************************'
      *
           .
       3100-GRABAR-ESTADIS-EXIT.
           EXIT.
      *
      ******************************************************************
      *     3200-GRABA-ERROR                                           *
      ******************************************************************
       3200-GRABA-ERROR.
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
       3200-GRABA-ERROR-EXIT.
           EXIT.
      *
