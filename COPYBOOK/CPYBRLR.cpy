       01 CPYBRLR.
          05 ENTRADA.
             10 OPCION                      PIC X(01).
             10 NUM-ELEM-E                  PIC 9(03).
          05 REPAGINACION.
             10 BANK-REP                    PIC X(04).
             10 OFFICE-REP                  PIC X(04).
             10 CD-REP                      PIC X(02).
             10 COUNT-NUMBER-REP            PIC X(07).
             10 CUSTOMER-REP                PIC S9(10).
          05 SALIDA.
             10 SALIDA-TB OCCURS 3.
                15 BANK                     PIC X(4).
                15 OFFICE                   PIC X(4).
                15 CD-S                     PIC X(2).
                15 COUNT-NUMBER             PIC X(7).
                15 CUSTOMER                 PIC S9(10).
                15 TYPE-S                   PIC X(15).
                15 BALANCE                  PIC S9(15)V9(2).
                15 CURRENCY-S               PIC X(3).
                15 NAME                     PIC X(20).
                15 SURNAME                  PIC X(50).
                15 CITY                     PIC X(20).
                15 COUNTRY                  PIC X(20).
                15 BIRTHDAY                 PIC X(10).
          05 SALIDA-CONTROL.
             10 MAS-DATOS                   PIC X(01).
             10 NUM-ELEM-S                  PIC 9(03).
          05 ERRORES.
             10 RETORNO                     PIC X(02).
             10 SUBRETORNO                  PIC X(02).
             10 ACCION                      PIC X(20).
             10 TABLA                       PIC X(20).
             10 PARRAFO                     PIC X(20).
             10 NOMRUTINA                   PIC X(08).
             10 SQLCODE-E                   PIC 9(04).
      *
