//VALJCNMA JOB NOTIFY=&SYSUID
//*LIBREBIA DE PGM
//JOBLIB   DD DSN=IBMUSER.COBOL.LOAD,DISP=SHR
//*PASO DE BORRADO
//BORRADO  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE DES.VAL.SEG.DESCART.F280519.OR
 DELETE DES.UNLOAD.ENT.CLIENTES.F220519.OR
 DELETE DES.VAL.CLI.DESCART.F280519
 DELETE DES.VAL.CLI.NMAPFRE.F280519
 SET MAXCC=0
/*
//* ORDENAR FICHERO 1
//ORDENAR1 EXEC PGM=SORT
//SORTIN   DD DSN=DES.VAL.SEG.DESCART.F280519,DISP=SHR
//SORTOUT  DD DSN=DES.VAL.SEG.DESCART.F280519.OR,
//            DISP=(,CATLG,DELETE),
//            SPACE=(TRK,(5,5),RLSE)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(2532,9,CH,A)
/*
//* ORDENAR FICHERO 2
//ORDENAR2 EXEC PGM=SORT
//SORTIN   DD DSN=DES.UNLOAD.ENT.CLIENTES.F220519,DISP=SHR
//SORTOUT  DD DSN=DES.UNLOAD.ENT.CLIENTES.F220519.OR,
//            DISP=(,CATLG,DELETE),
//            SPACE=(TRK,(5,5),RLSE)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=(1,9,CH,A)
/*
//* EJECUCION DEL PGM CON FICHEROS ORDENADOS
//VALJCNMA EXEC PGM=VALCNMAP
//ENTRADA1 DD DSN=DES.VAL.SEG.DESCART.F280519.OR,DISP=SHR
//ENTRADA2 DD DSN=DES.UNLOAD.ENT.CLIENTES.F220519.OR,DISP=SHR
//FSALIDA  DD DSN=DES.VAL.CLI.NMAPFRE.F280519,
//            DISP=(,CATLG,DELETE),
//            SPACE=(TRK,(5,5),RLSE)
//DESCARTE DD DSN=DES.VAL.CLI.DESCART.F280519,
//            DISP=(,CATLG,DELETE),
//            SPACE=(TRK,(5,5),RLSE)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
