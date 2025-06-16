       01 CPY-RUT.
           05 ENTRADA-RUT.
                10 BANCO-RUT               PIC X(4).
                10 OFICINA-RUT             PIC X(4).
                10 DC-RUT                  PIC X(2).
                10 NUM-CUENTA-RUT          PIC X(10).
                10 TITULAR-RUT             PIC X(10).
                10 SALDO-RUT               PIC S9(15)V9(2) USAGE COMP-3.
           05 REPO-RUT.
                10 ESTADO-RUT              PIC X(02).
                10 VALOR-CLAVE             PIC X(10).
           05 ERRORES-RUT.
                10  RETORNO-ERR            PIC X(02).
                10  SUBRETORNO-ERR         PIC X(02).
                10  DESCRIPCION-ERR        PIC X(40).
                10  CAMPO-ERR              PIC X(20).
                10  PARRAFO-ERR            PIC X(40).
                10  SQLCODE-ERR            PIC S9(04).