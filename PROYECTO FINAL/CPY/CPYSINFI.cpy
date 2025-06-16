       01 CPYSINFI.
          05 SALIDA-SIN.
                  10 ID-SINIETRO-S         PIC S9(9).
                  10 FECHA-SINIESTRO-S     PIC X(10).
                  10 CAUSAS-S              PIC X(55).
                  10 ACPTADO-S             PIC X(01).
                  10 INDEMNIZACION-S       PIC S9(13)V9(2).
                  10 NUMERO-POLIZA-S       PIC X(9).
                  10 DNI-PERITO-S          PIC X(9).
          05 ERRORES.
                  10 COD-RETORNO            PIC X(02).
                  10 COD-SUBRETORNO         PIC S9(09).
                  10 PARRAFO                PIC X(30).
                  10 TABLA                  PIC X(25).
                  10 DESCRIPCION            PIC X(30).
                  10 SQLCODE-E              PIC -999.