//LOADJAGE JOB NOTIFY=&SYSUID
//JOBLIB   DD DSN=DSN810.SDSNLOAD,DISP=SHR
//         DD DSN=IBMUSER.COBOL.LOAD,DISP=SHR
//*---------------------------------------------------------------------
//SUPRIME  EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *
     DELETE DES.REPOS.ORDENA
     SET MAXCC=0
/*
//*---------------------------------------------------------------------
//SORT01   EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=DES.REPOS.ENTRADA,DISP=SHR
//SORTOUT  DD DSN=DES.REPOS.ORDENA,
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(1,1))
//SYSIN    DD *
     SORT FIELDS=(01,20,CH,A)
/*
//*---------------------------------------------------------------------
//BIND      EXEC PGM=IKJEFT01
//DBRMLIB   DD DSN=IBMUSER.COBOL.DBRMLIB,DISP=SHR
//SYSTSPRT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSTSIN   DD *
DSN SYSTEM(DB8G)
BIND PLAN(REPOS) MEMBER(REPOS,RUTREPOS)
END
/*
//*---------------------------------------------------------------------
//EJECUTA  EXEC PGM=IKJEFT01
//FENTRADA DD DSN=DES.REPOS.ORDENA,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
  DSN SYSTEM(DB8G)
  RUN PLAN(REPOS) PROGRAM(REPOS)
  END
/*
