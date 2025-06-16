       01 CPY-CPYCURS.
           05 SALIDA-OCC                   OCCURS 10 TIMES.
                10  ID-PEDIDO              PIC S9(9) USAGE COMP.
                10  ID-CLIENTE             PIC S9(9) USAGE COMP.
                10  FECHA-PEDIDO           PIC X(10).
                10  IMPORTE-TOTAL          PIC S9(8)V9(2) USAGE COMP-3.
                10  ESTADO                 PIC X(9).
                10  TIPO-ENVIO             PIC X(8).
                10  COMENTARIOS            PIC X(50).
           05 ERRORES-RUT.
                10 RETORNO-ERR             PIC X(02).
                10 SUBRETORNO-ERR          PIC X(02).
                10 DESCRIPCION-ERR         PIC X(40).
                10 CAMPO-ERR               PIC X(20).
                10 PARRAFO-ERR             PIC X(40).
           05 SALIDA-RUT.
                10 REG-RECUPERADOS         PIC 9(03).