       01 CPYRUTCL.
          05 ENTRADA.
             10 OPCION                            PIC X(01).
             10 NUM-ELEM-E                        PIC 9(03).
          05 REPAGINACION.
             10 DNI-REP                           PIC X(09).
          05 SALIDA.
             10 SALIDA-TB OCCURS 3.
                15  DNI-CL                        PIC X(9).
                15  NOMBRE-CL                     PIC X(25).
                15  APELLIDO-1                    PIC X(25).
                15  APELLIDO-2                    PIC X(25).
                15  CLASE-VIA                     PIC X(25).
                15  NOMBRE-VIA                    PIC X(55).
                15  NUMERO-VIA                    PIC S9(9).
                15  COD-POSTAL                    PIC X(5).
                15  CIUDAD                        PIC X(25).
                15  TELEFONO                      PIC X(10).
                15  OBSERVACIONES                 PIC X(500).
          05 SALIDA-CONTROL.
             10 MAS-DATOS                         PIC X(01).
             10 NUM-ELEM-S                        PIC 9(03).
          05 ERRORES.
             10 RETORNO-ERR                       PIC X(02).
             10 SUBRETORNO-ERR                    PIC X(02).
             10 ACCION-ERR                        PIC X(20).
             10 TABLA-ERR                         PIC X(20).
             10 PARRAFO-ERR                       PIC X(20).
             10 NOMRUTINA-ERR                     PIC X(08).
             10 SQLCODE-ERR                       PIC 9(04).