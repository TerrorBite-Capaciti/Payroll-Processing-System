      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATA-STORAGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-FILE ASSIGN TO "DATA-FILE.TXT".


       DATA DIVISION.
       FILE SECTION.
       FD DATA-FILE.
       01 DATA-RECORD.
           05 EMPLOYEE-ID PIC 9(5).
           05 EMPLOYEE-NAME PIC X(20).



       PROCEDURE DIVISION.
            OPEN OUTPUT DATA-FILE.
            MOVE 12345 TO EMPLOYEE-ID
            MOVE "JOHN DOE" TO EMPLOYEE-NAME
            WRITE DATA-RECORD
            CLOSE DATA-FILE
            STOP RUN.
       END PROGRAM DATA-STORAGE.
