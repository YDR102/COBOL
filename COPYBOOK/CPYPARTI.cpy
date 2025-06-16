       01 CPYPARTI.
           05 SALIDA-OCC                    OCCURS 20 TIMES.
                10  ID-CLIENTE              PIC X(10).
                10  NOMBRE                  PIC X(50).
                10  TIPO-CLIENTE            PIC X(01).
                10  DNI-CIF                 PIC X(15).
                10  TELEFONO                PIC X(12).
                10  EMAIL                   PIC X(50).
                10  DIRECCION               PIC X(100).
           05 ERRORES-RUT.
                10  RETORNO-ERR             PIC X(02).
                10  SUBRETORNO-ERR          PIC X(02).
                10  DESCRIPCION-ERR         PIC X(40).
                10  CAMPO-ERR               PIC X(20).
                10  PARRAFO-ERR             PIC X(40).
           05 SALIDA-RUT.
                10  REG-RECUPERADOS         PIC 9(03).