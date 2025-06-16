       01 CRUDCPY4.
          05 ENTRADA-4.
               10 MATRICULA-4                      PIC X(04).
          05 SALIDA.
               10 SALARIO-S-4                      PIC 9(04)V9(03).
               10 FECHA-NACIMIENTO-S-4             PIC X(10).
          05 ERRORES-4.
               10 RETORNO-ERR-4                    PIC X(02).
               10 DESCRIPCION-ERR-4                PIC X(50).
               10 PARRAFO-ERR-4                    PIC X(30).
               10 SQLCODE-ERR-4                    PIC -999.