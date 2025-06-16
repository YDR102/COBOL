      ******************************************************************
      ** C O P Y   E N T R A D A                                      **
      **--------------------------------------------------------------**
      ** LONGITUD: 47                                                 **
      **--------------------------------------------------------------**
      ** BANCO              --> ALFANUMERICO DE 04 POSICIONES         **
      ** OFICINA            --> ALFANUMERICO DE 04 POSICIONES         **
      ** DC                 --> ALFANUMERICO DE 02 POSICIONES         **
      ** NUM-CUENTA         --> ALFANUMERICO DE 10 POSICIONES         **
      ** TITULAR            --> ALFANUMERICO DE 10 POSICIONES         **
      ** SALARIO            --> DECIMAL  SIG DE 17 POSICIONES (15,02) **
      ******************************************************************
       01 CPYREPOS.
          05 ENTRADA.
             10 BANCO                PIC X(04).
             10 OFICINA              PIC X(04).
             10 DC                   PIC X(02).
             10 NUM-CUENTA           PIC X(10).
             10 TITULAR              PIC X(10).
             10 SALDO                PIC S9(15)V9(02).
          05 ERRORES.
             10 RETORNO                     PIC X(02).
             10 SUBRETORNO                  PIC X(02).
             10 ACCION                      PIC X(20).
             10 TABLA                       PIC X(20).
             10 PARRAFO                     PIC X(20).
             10 NOMRUTINA                   PIC X(08).
             10 SQLCODE-E                   PIC S9(04).
      *
