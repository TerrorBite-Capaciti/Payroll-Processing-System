       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-MASTER-STORAGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "employee_master_storage.DAT"
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
       01 WS-ERROR-MESSAGE   PIC X(50).

       PROCEDURE DIVISION.
       BEGIN.
           OPEN OUTPUT EMPLOYEE-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "File Open Error: " FILE-STATUS
               STOP RUN
           END-IF

           PERFORM WRITE-RECORDS

           CLOSE EMPLOYEE-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "File Close Error: " FILE-STATUS
           ELSE
               DISPLAY "EMPLOYEE-MASTER.DAT created successfully."
           END-IF

           STOP RUN.

       WRITE-RECORDS.
           MOVE 12345 TO EMP-ID
           MOVE "Nizaam Hafejjee" TO EMP-NAME
           MOVE "Developer" TO EMP-POSITION
           MOVE 35000.00 TO BASE-SALARY
           MOVE 250.00 TO TAX-RATE
           MOVE "Y" TO MEDICAL-AID
           WRITE EMPLOYEE-RECORD
           IF FILE-STATUS NOT = "00"
               DISPLAY "Write Error for EMP-ID: " EMP-ID " Status: " 
               FILE-STATUS
           END-IF.
           
           MOVE 89896 TO EMP-ID
           MOVE "Ayanda Radebe" TO EMP-NAME
           MOVE "Software Dev" TO EMP-POSITION
           MOVE 40000.00 TO BASE-SALARY
           MOVE 300.00 TO TAX-RATE
           MOVE "Y" TO MEDICAL-AID
           WRITE EMPLOYEE-RECORD
           IF FILE-STATUS NOT = "00"
               DISPLAY "Write Error for EMP-ID: " EMP-ID " Status: " 
               FILE-STATUS
           END-IF.
           

           MOVE 78978 TO EMP-ID
           MOVE "Lefa Jele" TO EMP-NAME
           MOVE "QA Tester" TO EMP-POSITION
           MOVE 30000.00 TO BASE-SALARY
           MOVE 200.00 TO TAX-RATE
           MOVE "N" TO MEDICAL-AID
           WRITE EMPLOYEE-RECORD
           IF FILE-STATUS NOT = "00"
               DISPLAY "Write Error for EMP-ID: " EMP-ID " Status: " 
               FILE-STATUS
           END-IF.
           

           MOVE 45645 TO EMP-ID
           MOVE "Silindile Shabangu" TO EMP-NAME
           MOVE "Analyst" TO EMP-POSITION
           MOVE 32000.00 TO BASE-SALARY
           MOVE 250.00 TO TAX-RATE
           MOVE "Y" TO MEDICAL-AID
           WRITE EMPLOYEE-RECORD
           IF FILE-STATUS NOT = "00"
               DISPLAY "Write Error for EMP-ID: " EMP-ID " Status: " 
               FILE-STATUS
           END-IF.
           

           MOVE 12312 TO EMP-ID
           MOVE "Sisamkele Vava" TO EMP-NAME
           MOVE "Admin Assistant" TO EMP-POSITION
           MOVE 28000.00 TO BASE-SALARY
           MOVE 180.00 TO TAX-RATE
           MOVE "N" TO MEDICAL-AID
           WRITE EMPLOYEE-RECORD
           IF FILE-STATUS NOT = "00"
               DISPLAY "Write Error for EMP-ID: " EMP-ID " Status: " 
               FILE-STATUS
           END-IF.

