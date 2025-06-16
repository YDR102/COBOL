       01 EJCPYC01.
           05 ENTRADA-RUT.
                10 PREFIJO-E-RUT           PIC X(02).
           05 SALIDA-RUT.
                10 PROVINCIA-S-RUT         PIC X(20).
           05 ERRORES-RUT.
               10  RETORNO-ERR         PIC X(02).
               10  SUBRETORNO-ERR      PIC X(02).
               10  DESCRIPCION-ERR     PIC X(40).
               10  CAMPO-ERR           PIC X(20).
               10  PARRAFO-ERR         PIC X(40).
               10  SQLCODE-ERR         PIC S9(04).