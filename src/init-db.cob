       IDENTIFICATION DIVISION.
       PROGRAM-ID. INIT-DB.
       AUTHOR. BYTEBANK-DEV.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "../data/employees.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EMP-ID
               FILE STATUS IS FS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID      PIC X(10).
           05 EMP-NAME    PIC X(50).
           05 EMP-DEPT    PIC X(20).
           05 EMP-POSITION PIC X(30).
           05 EMP-PAY-RATE PIC 9(5)V99.
           05 EMP-STATUS  PIC X(1).

       WORKING-STORAGE SECTION.
       01 FS-STATUS      PIC XX.
       01 ADMIN-EMPLOYEE.
           05 FILLER      PIC X(10) VALUE "ADMIN0001".
           05 FILLER      PIC X(50) VALUE "System Administrator".
           05 FILLER      PIC X(20) VALUE "IT".
           05 FILLER      PIC X(30) VALUE "Head of IT".
           05 FILLER      PIC 9(5)V99 VALUE 1000000.
           05 FILLER      PIC X(1) VALUE "A".

       PROCEDURE DIVISION.
           OPEN OUTPUT EMPLOYEE-FILE
           IF FS-STATUS NOT = "00"
               DISPLAY "Error creating file: " FS-STATUS
               STOP RUN
           END-IF

           MOVE ADMIN-EMPLOYEE TO EMPLOYEE-RECORD
           WRITE EMPLOYEE-RECORD
           IF FS-STATUS NOT = "00"
               DISPLAY "Error writing record: " FS-STATUS
           ELSE
               DISPLAY "Database initialized with admin user"
           END-IF

           CLOSE EMPLOYEE-FILE
           STOP RUN.
       END PROGRAM INIT-DB.