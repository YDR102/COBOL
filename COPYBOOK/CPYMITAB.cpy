       01 CPYBRLR.
          05 ENTRADA.
             10 OPCION                      PIC X(01).
             10 NUM-ELEM-E                  PIC 9(03).
          05 REPAGINACION.
             10 DNI-REP                     PIC X(04).
          05 SALIDA.
             10 SALIDA-TB OCCURS 3.
                15 DNI                      PIC X(4).
                15 NOMBRE                   PIC X(4).
                15 APELLIDOS                PIC X(2).
                15 FECNAC                   PIC X(7).
                15 SEXO                     PIC S9(10).
          05 SALIDA-CONTROL.
             10 MAS-DATOS                   PIC X(01).
             10 NUM-ELEM-S                  PIC 9(03).
          05 ERRORES.
             10 RETORNO-ERR                 PIC X(02).
             10 SUBRETORNO-ERR              PIC X(02).
             10 ACCION-ERR                  PIC X(20).
             10 TABLA-ERR                   PIC X(20).
             10 PARRAFO-ERR                 PIC X(20).
             10 NOMRUTINA-ERR               PIC X(08).
             10 SQLCODE-ERR                 PIC 9(04).
