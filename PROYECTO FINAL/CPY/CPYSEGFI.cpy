       01 CPYSEGFI.
          05 SALIDA-SEG.
                  10 NUMERO-POLIZA-S         PIC X(9).
                  10 TIPO-S                  PIC X(2).
                  10 FECHA-INICIO-S          PIC X(10).
                  10 FECHA-VENCIMIENTO-S     PIC X(10).
                  10 COND-PART-S             PIC X(2000).
                  10 OBSERVACIONES-S         PIC X(500).
                  10 DNI-CL-S                PIC X(9).
          05 ERRORES.
                  10 COD-RETORNO            PIC X(02).
                  10 COD-SUBRETORNO         PIC S9(09).
                  10 PARRAFO                PIC X(30).
                  10 TABLA                  PIC X(25).
                  10 DESCRIPCION            PIC X(30).
                  10 SQLCODE-E              PIC -999.
