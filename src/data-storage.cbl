      ******************************************************************
      * Author:
      * Date:
      * Purpose: Writes sample employee records to a sequential file
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATA-STORAGE.

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
           05 EMPLOYEE-ID PIC 9(5).
           05 EMPLOYEE-NAME PIC X(20).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS PIC XX.



       PROCEDURE DIVISION.
            OPEN OUTPUT  DATA-FILE.

               IF FILE-STATUS IS NOT = "00"
                   DISPLAY "File Open Error:" FILE-STATUS
                   STOP RUN
                  END-IF

            MOVE 12345 TO EMPLOYEE-ID
            MOVE "Nizaam Hafejjee" TO EMPLOYEE-NAME
            WRITE DATA-RECORD

            MOVE 89896 TO EMPLOYEE-ID
            MOVE "Ayanda Radebe" TO EMPLOYEE-NAME
            WRITE DATA-RECORD

            MOVE 78978 TO EMPLOYEE-ID
            MOVE "Lefa Jele" TO EMPLOYEE-NAME
            WRITE DATA-RECORD

            MOVE 45645 TO EMPLOYEE-ID
            MOVE "Silindile Shabangu" TO EMPLOYEE-NAME
            WRITE DATA-RECORD

            MOVE 12312 TO EMPLOYEE-ID
            MOVE "Sisamkele Vava" TO EMPLOYEE-NAME
            WRITE DATA-RECORD

            CLOSE DATA-FILE
            DISPLAY "Records written successfully."

            STOP RUN.
       END PROGRAM DATA-STORAGE.
