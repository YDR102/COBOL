       01 CRUDCPY1.
          05 ENTRADA-1.
               10 MATRICULA-1                     PIC X(04).
               10 APELLIDO-1                      PIC X(30).
               10 NOMBRE-1                        PIC X(20).
               10 CATEGORIA-1                     PIC X(40).
               10 DEPARTAMENTO-1                  PIC X(40).
               10 SECCION-1                       PIC X(30).
               10 SALARIO-1                       PIC 9(04)V9(03).
               10 FECHA-INGRESO-1                 PIC X(10).
               10 FECHA-NACIMIENTO-1              PIC X(10).
          05 ERRORES-1.
               10 RETORNO-ERR-1                   PIC X(02).
               10 DESCRIPCION-ERR-1               PIC X(50).
               10 PARRAFO-ERR-1                   PIC X(30).
               10 SQLCODE-ERR-1                   PIC -999.