      ******************************************************************
      * Author: 
      * Date:
      * Purpose: Read & display employee records in a formatted layout
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-MASTER-READ.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "employee_master_storage.cbl"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID         PIC 9(5).
           05 EMP-NAME       PIC X(30).
           05 EMP-POSITION   PIC X(20).
           05 BASE-SALARY    PIC 9(7)V99.
           05 TAX-RATE       PIC 9(3)V99.
           05 MEDICAL-AID    PIC X.

       WORKING-STORAGE SECTION.
       01 FILE-STATUS        PIC XX.
       01 END-OF-FILE        PIC X VALUE "N".
       01 WS-DISPLAY-SALARY  PIC Z,ZZZ,ZZ9.99.
       01 WS-DISPLAY-TAX     PIC Z99.99.
       01 WS-MEDICAL-AID     PIC X(10).
       01 ERROR-MESSAGE      PIC X(50).

           PROCEDURE DIVISION.
           PERFORM MAIN-PROCEDURE
           STOP RUN.

       MAIN-PROCEDURE.
           PERFORM OPEN-FILE
           IF FILE-STATUS = "00"
               PERFORM PROCESS-RECORDS
               PERFORM CLOSE-FILE
           ELSE
               PERFORM DISPLAY-ERROR
           END-IF
           .

       OPEN-FILE.
           OPEN INPUT EMPLOYEE-FILE
           EVALUATE FILE-STATUS
               WHEN "00" CONTINUE
               WHEN "35" MOVE "File not found" TO ERROR-MESSAGE
               WHEN OTHER MOVE "Unknown file error" TO ERROR-MESSAGE
           END-EVALUATE.

       PROCESS-RECORDS.
           PERFORM UNTIL END-OF-FILE = "Y"
               READ EMPLOYEE-FILE
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       PERFORM DISPLAY-EMPLOYEE-DETAILS
               END-READ
           END-PERFORM.

       DISPLAY-EMPLOYEE-DETAILS.
           MOVE BASE-SALARY TO WS-DISPLAY-SALARY
           MOVE TAX-RATE TO WS-DISPLAY-TAX
           MOVE SPACES TO WS-MEDICAL-AID
           IF MEDICAL-AID = "Y" OR "y"
               MOVE "YES" TO WS-MEDICAL-AID
           ELSE
               MOVE "NO" TO WS-MEDICAL-AID
           END-IF

           DISPLAY " "
           DISPLAY "=============================================="
           DISPLAY " EMPLOYEE DETAILS"
           DISPLAY "=============================================="
           DISPLAY " ID:        " EMP-ID
           DISPLAY " NAME:      " FUNCTION TRIM(EMP-NAME)
           DISPLAY " POSITION:  " FUNCTION TRIM(EMP-POSITION)
           DISPLAY " SALARY:    R" WS-DISPLAY-SALARY
           DISPLAY " TAX RATE:  " WS-DISPLAY-TAX "%"
           DISPLAY " MEDICAL AID: " WS-MEDICAL-AID
           DISPLAY "==============================================".

       CLOSE-FILE.
           CLOSE EMPLOYEE-FILE
           DISPLAY " "
           DISPLAY "END OF EMPLOYEE RECORDS".

       DISPLAY-ERROR.
           DISPLAY " "
           DISPLAY "ERROR: " ERROR-MESSAGE
           DISPLAY "FILE STATUS: " FILE-STATUS
           DISPLAY "Please verify the file exists in the current direc".
       


