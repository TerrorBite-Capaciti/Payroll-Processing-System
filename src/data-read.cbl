      ******************************************************************
      * Author:
      * Date:
      * Purpose:Reads and displays employee records from a file
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATA-READ.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-FILE ASSIGN TO "DATA-FILE.TXT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD DATA-FILE.
       01 DATA-RECORD.
           05 EMPLOYEE-ID   PIC 9(5).
           05 EMPLOYEE-NAME PIC X(20).


       WORKING-STORAGE SECTION.
       01 FILE-STATUS      PIC XX.
       01 END-OF-FILE      PIC X VALUE "N".


       PROCEDURE DIVISION.
           OPEN INPUT DATA-FILE

           IF FILE-STATUS NOT = "00"
               DISPLAY "File Open Error: " FILE-STATUS
               STOP RUN
           END-IF

           PERFORM UNTIL END-OF-FILE = "Y"
               READ DATA-FILE
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                   DISPLAY "ID: " EMPLOYEE-ID " | Name: " EMPLOYEE-NAME
               END-READ
           END-PERFORM

           CLOSE DATA-FILE
           DISPLAY "End of file reached."

           STOP RUN.
       END PROGRAM DATA-READ.
