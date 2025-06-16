       01 CPYBRLR.
           05 ENTRADA.
               10 OPCION                      PIC X(01).
               10 NUM-ELEM-E                  PIC 9(03).
           05 REPAGINACION.
               10 SELECCION-REP               PIC X(30).
           05 SALIDA.
               10 SALIDA-TB         OCCURS 3.
                   15 SELECCION               PIC X(30).
                   15 ENTRENADOR              PIC X(30).
                   15 NUM-JUGADORES-CONV      PIC S9(03)V USAGE COMP-3.
                   15 FECHA-PRIMER-PARTIDO    PIC X(10).
                   15 GRUPO                   PIC X(01).
                   15 FASE-ELIMINACION        PIC X(20).
                   15 TIMESTAMP-MODIF         PIC X(26).
                   15 USUARIO-MODIF           PIC X(30).
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
               10 SQLCODE-ERR                 PIC -999.