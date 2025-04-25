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
 
           SELECT REPORT-FILE ASSIGN TO "employee_report.txt"
                ORGANIZATION IS LINE SEQUENTIAL.
 
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
 
       FD REPORT-FILE.
       01 REPORT-RECORD      PIC X(80).
 
       WORKING-STORAGE SECTION.
       01 FILE-STATUS            PIC XX.
       01 END-OF-FILE            PIC X VALUE "N".
       01 ERROR-MESSAGE          PIC X(50).
       01 WS-DISPLAY-SALARY      PIC Z,ZZZ,ZZ9.99.
       01 WS-DISPLAY-TAX         PIC Z99.99.
       01 WS-MEDICAL-AID         PIC X(10).
       01 WS-EMP-ID-FMT          PIC X(6).
       01 WS-EMP-NAME-FMT        PIC X(30).
       01 WS-EMP-POSITION-FMT    PIC X(20).
       01 WS-SALARY-FMT          PIC X(12).
       01 WS-TAX-FMT             PIC X(7).
       01 WS-MEDICAL-FMT         PIC X(10).
 
       PROCEDURE DIVISION.
           PERFORM MAIN-PROCEDURE
           STOP RUN.
 
       MAIN-PROCEDURE.
           PERFORM OPEN-FILES
           IF FILE-STATUS = "00"
               PERFORM WRITE-REPORT-HEADER
               PERFORM PROCESS-RECORDS
               PERFORM CLOSE-FILES
           ELSE
               PERFORM DISPLAY-ERROR
           END-IF.
 
       OPEN-FILES.
           OPEN INPUT EMPLOYEE-FILE
                OUTPUT REPORT-FILE.
           EVALUATE FILE-STATUS
               WHEN "00" CONTINUE
               WHEN "35" MOVE "File not found" TO ERROR-MESSAGE
               WHEN OTHER MOVE "Unknown file error" TO ERROR-MESSAGE
           END-EVALUATE.
 
       WRITE-REPORT-HEADER.
           MOVE "ID  NAME POSITION SALARY  TAX MEDICAL" TO REPORT-RECORD
           WRITE REPORT-RECORD
           MOVE "-----  ------------- --------  ------" TO REPORT-RECORD
           WRITE REPORT-RECORD.
 
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
           IF MEDICAL-AID = "Y" OR MEDICAL-AID = "y"
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
           DISPLAY "=============================================="
 
           MOVE EMP-ID TO WS-EMP-ID-FMT
           MOVE FUNCTION TRIM(EMP-NAME) TO WS-EMP-NAME-FMT
           MOVE FUNCTION TRIM(EMP-POSITION) TO WS-EMP-POSITION-FMT
           MOVE "R" TO WS-SALARY-FMT (1:1)
           MOVE WS-DISPLAY-SALARY TO WS-SALARY-FMT (2:11)
           MOVE WS-DISPLAY-TAX TO WS-TAX-FMT
           MOVE WS-MEDICAL-AID TO WS-MEDICAL-FMT
 
           STRING
               WS-EMP-ID-FMT DELIMITED BY SIZE
               WS-EMP-NAME-FMT DELIMITED BY SIZE
               WS-EMP-POSITION-FMT DELIMITED BY SIZE
               WS-SALARY-FMT DELIMITED BY SIZE
               WS-TAX-FMT DELIMITED BY SIZE
               WS-MEDICAL-FMT DELIMITED BY SIZE
               INTO REPORT-RECORD
           END-STRING
           WRITE REPORT-RECORD.
 
       CLOSE-FILES.
           CLOSE EMPLOYEE-FILE
                 REPORT-FILE
           DISPLAY " "
           DISPLAY "END OF EMPLOYEE RECORDS".
 
       DISPLAY-ERROR.
           DISPLAY " "
           DISPLAY "ERROR: " ERROR-MESSAGE
           DISPLAY "FILE STATUS: " FILE-STATUS
           DISPLAY "Please verify the file exists.".