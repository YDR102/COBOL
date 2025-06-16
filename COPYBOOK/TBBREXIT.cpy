      ******************************************************************
      * DCLGEN TABLE(BREXIT_BANK)                                      *
      *        LIBRARY(IBMUSER.COBOL.COPYS(TBBREXIT))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TB-)                                              *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE BREXIT_BANK TABLE
           ( BANK                           CHAR(4) NOT NULL,
             OFFICE                         CHAR(4) NOT NULL,
             CD                             CHAR(2) NOT NULL,
             COUNT_NUMBER                   CHAR(7) NOT NULL,
             CUSTOMER                       DECIMAL(10, 0) NOT NULL,
             TYPE                           CHAR(15) NOT NULL,
             BALANCE                        DECIMAL(17, 2) NOT NULL,
             CURRENCY                       CHAR(3) NOT NULL,
             NAME                           CHAR(20) NOT NULL,
             SURNAME                        CHAR(50) NOT NULL,
             CITY                           CHAR(20) NOT NULL,
             COUNTRY                        CHAR(20) NOT NULL,
             BIRTHDAY                       DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BREXIT_BANK                        *
      ******************************************************************
       01  DCLBREXIT-BANK.
      *    *************************************************************
      *                       BANK
           10 TB-BANK              PIC X(4).
      *    *************************************************************
      *                       OFFICE
           10 TB-OFFICE            PIC X(4).
      *    *************************************************************
      *                       CD
           10 TB-CD                PIC X(2).
      *    *************************************************************
      *                       COUNT_NUMBER
           10 TB-COUNT-NUMBER      PIC X(7).
      *    *************************************************************
      *                       CUSTOMER
           10 TB-CUSTOMER          PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
      *                       TYPE
           10 TB-TYPE              PIC X(15).
      *    *************************************************************
      *                       BALANCE
           10 TB-BALANCE           PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
      *                       CURRENCY
           10 TB-CURRENCY          PIC X(3).
      *    *************************************************************
      *                       NAME
           10 TB-NAME              PIC X(20).
      *    *************************************************************
      *                       SURNAME
           10 TB-SURNAME           PIC X(50).
      *    *************************************************************
      *                       CITY
           10 TB-CITY              PIC X(20).
      *    *************************************************************
      *                       COUNTRY
           10 TB-COUNTRY           PIC X(20).
      *    *************************************************************
      *                       BIRTHDAY
           10 TB-BIRTHDAY          PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
