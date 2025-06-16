       01 CRUDCPY2.
          05 ENTRADA-2.
               10 MATRICULA-2                     PIC X(04).
               10 APELLIDO-2                      PIC X(30).
               10 NOMBRE-2                        PIC X(20).
               10 CATEGORIA-2                     PIC X(40).
               10 DEPARTAMENTO-2                  PIC X(40).
               10 SECCION-2                       PIC X(30).
               10 SALARIO-2                       PIC 9(04)V9(03).
               10 FECHA-INGRESO-2                 PIC X(10).
               10 FECHA-NACIMIENTO-2              PIC X(10).
          05 ERRORES-2.
               10 RETORNO-ERR-2                   PIC X(02).
               10 DESCRIPCION-ERR-2               PIC X(50).
               10 PARRAFO-ERR-2                   PIC X(30).
               10 SQLCODE-ERR-2                   PIC -999.