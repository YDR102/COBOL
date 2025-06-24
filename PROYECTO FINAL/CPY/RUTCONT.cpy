       01 RUTCONT.
           05 ENTRADA-RUT.
                10 IMPORT-ORIG                PIC S9(15)V9(02).
                10 DIV-ORIG                   PIC X(03).
           05 SALIDA-RUT.
                10 IMPORT-DEST                PIC S9(15)V9(02).
                10 DIV-DEST                   PIC X(03).
           05 ERRORES-RUT.
                10 COD-RETORNO                PIC X(02).
                10 COD-SUBRETORNO             PIC X(02).
                10 PARRAFO                    PIC X(30).
                10 TABLA                      PIC X(30).
                10 DESCRIPCION                PIC X(50).
