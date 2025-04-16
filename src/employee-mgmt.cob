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
              RECORD KEY IS EMP-ID
              FILE STATUS IS FILE-STATUS-CODE
              .
      *****************************************************************
      *****************************************************************
       DATA DIVISION.
       FILE SECTION. 
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05  EMP-ID        PIC 9(10).
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
       77 FILE-EXISTS        PIC X(1) VALUE 'N'.

       PROCEDURE DIVISION.
       BEGIN.
           PERFORM CHECK-FILE-EXISTS
           IF FILE-EXISTS = 'N'
              PERFORM CREATE-EMPLOYEE-LIST
           END-IF.

           PERFORM UNTIL USER-INPUT = "4"
           DISPLAY "========== EMPLOYEE MANAGEMENT MODULE ============"
           DISPLAY " "
           DISPLAY "1. LIST ALL EMPLOYEES"
           DISPLAY "2. ADD A NEW EMPLOYEE"
           DISPLAY "3. SEARCH EMPLOYEE BY ID"
           DISPLAY " "
           DISPLAY "CHOICE: " WITH NO ADVANCING 
           ACCEPT USER-INPUT

           EVALUATE USER-INPUT
             WHEN "1"
              PERFORM LIST-ALL-EMPLOYEES
             
             WHEN "2"
              PERFORM ADD-NEW-EMPLOYEE

             WHEN "3"
              PERFORM SEARCH-BY-ID

             WHEN OTHER 
              DISPLAY "GOING BACK TO MAIN MENU"
              GOBACK
           END-EVALUATE

           END-PERFORM
           .
      *****************************************************************
      *****************************************************************
       CHECK-FILE-EXISTS.
           OPEN INPUT EMPLOYEE-FILE

           IF FILE-STATUS-CODE = "00"
              CLOSE EMPLOYEE-FILE 
              MOVE 'Y' TO FILE-EXISTS
           ELSE
              MOVE 'N' TO FILE-EXISTS
           END-IF.
           CLOSE EMPLOYEE-FILE
           .
      *****************************************************************
      *****************************************************************
       CREATE-EMPLOYEE-LIST.
           OPEN INPUT EMPLOYEE-FILE
           DISPLAY "CREATING FILE"

           IF FILE-STATUS-CODE = "00"
              DISPLAY "FILE CREATED. code: " FILE-STATUS-CODE
           ELSE
              DISPLAY "ERROR CREATING FILE. code: " FILE-STATUS-CODE
           END-IF
           CLOSE EMPLOYEE-FILE
           .
      *****************************************************************
      *****************************************************************
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
