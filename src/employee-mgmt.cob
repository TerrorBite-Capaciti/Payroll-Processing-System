       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-MGMT.
       AUTHOR.     BYTEBANK-DEV.
      *****************************************************************
      *****************************************************************
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT EMPLOYEE-FILE ASSIGN TO "data/employees.dat"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EMPLOYEE-ID
              FILE STATUS IS FILE-STATUS-CODE
              .
      *****************************************************************
      *****************************************************************
       DATA DIVISION.
       FILE SECTION. 
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05  EMP-ID        PIC X(10).
           05  EMP-NAME      PIC X(50).
           05  EMP-SURNAME    PIC X(50).
           05  EMP-POSITION-TYPE PIC X(20).
           05 EMP-BIRTH-DATE PIC X(8).
           05  EMP-AGE       PIC 9(2).
      *****   THIS IS AN OPTIONAL DATA-ITEM    ****
           05 EMP-UNION-FEE PIC 9(5).

       WORKING-STORAGE SECTION.
      *****DATA ITEMS FOR INPUTTING AND WRITING TO FILE
       01 WS-EMPLOYEE.
           05  WS-EMP-ID        PIC X(10).
           05  WS-EMP-NAME      PIC X(50).
           05  WS-EMP-SURNAME    PIC X(50).
           05  WS-EMP-POSITION-TYPE PIC X(20).
           05  WS-EMP-BIRTH-DATE PIC X(8).
           05  WS-EMP-AGE       PIC 9(2).
      *****   THIS IS AN OPTIONAL DATA-ITEM    ****
           05  WS-EMP-UNION-FEE PIC 9(5).

       77 WS-EOF             PIC X(1) VALUE 'N'.
       77 USER-INPUT         PIC X(1).
       77 FILE-STATUS-CODE   PIC X(2).

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "Enter Employee ID: "
           ACCEPT EMP-ID
           DISPLAY "Enter Employee Name: "
           ACCEPT EMP-NAME
           DISPLAY "Enter Employee Age: "
           ACCEPT EMP-AGE

           DISPLAY "Employee ID: " EMP-ID
           DISPLAY "Employee Name: " EMP-NAME
           DISPLAY "Employee Age: " EMP-AGE
           GOBACK.

       END PROGRAM EMPLOYEE-MGMT.
