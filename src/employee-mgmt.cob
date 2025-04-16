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
       WORKING-STORAGE SECTION.
       01  EMP-ID        PIC X(10).
       01  EMP-NAME      PIC X(50).
       01  EMP-AGE       PIC 99.

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
