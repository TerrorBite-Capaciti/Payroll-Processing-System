       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-MGMT.
       AUTHOR. YOUR-NAME.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO 'EMPLOYEE-DATA.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS-CODE.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID                PIC 9(5).
           05 EMP-FIRST-NAME        PIC X(20).
           05 EMP-LAST-NAME         PIC X(20).
           05 EMP-POSITION-TYPE     PIC X(15).
           05 EMP-BIRTH-DATE        PIC 9(8).  *> YYYYMMDD
           05 EMP-UNION-FEE         PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 FILE-STATUS-CODE            PIC XX.
       01 FILE-EXISTS                 PIC X VALUE 'N'.
       01 USER-INPUT                  PIC X(1).
       01 WS-EMP-ID                   PIC 9(5).
       01 WS-EMP-FIRST-NAME           PIC X(20).
       01 WS-EMP-LAST-NAME            PIC X(20).
       01 WS-EMP-POSITION-TYPE        PIC X(15).
       01 WS-EMP-BIRTH-DATE           PIC 9(8).
       01 WS-EMP-UNION-FEE            PIC 9(5)V99.
       01 WS-VALID-TYPE               PIC X VALUE 'N'.
       01 WS-EMP-SALARY               PIC 9(7)V99.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM CHECK-FILE-EXISTS
           IF FILE-EXISTS = 'N'
               PERFORM CREATE-EMPLOYEE-LIST
           END-IF

           PERFORM UNTIL USER-INPUT = "4"
               DISPLAY "========== EMPLOYEE MANAGEMENT MODULE ============"
               DISPLAY " "
               DISPLAY "1. LIST ALL EMPLOYEES"
               DISPLAY "2. ADD A NEW EMPLOYEE"
               DISPLAY "3. SEARCH EMPLOYEE BY ID"
               DISPLAY "4. RETURN TO MAIN MENU"
               DISPLAY " "
               DISPLAY "CHOICE: " WITH NO ADVANCING
               ACCEPT USER-INPUT

               EVALUATE USER-INPUT
                   WHEN "1" PERFORM LIST-ALL-EMPLOYEES
                   WHEN "2" PERFORM ADD-NEW-EMPLOYEE
                   WHEN "3" PERFORM SEARCH-BY-ID
                   WHEN "4" CONTINUE
                   WHEN OTHER
                       DISPLAY "INVALID INPUT. Please select a valid option (1-4)."
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       CHECK-FILE-EXISTS.
           OPEN INPUT EMPLOYEE-FILE
           EVALUATE FILE-STATUS-CODE
               WHEN "00"
                   CLOSE EMPLOYEE-FILE
                   MOVE 'Y' TO FILE-EXISTS
               WHEN "02"
                   DISPLAY "File not found. Creating new employee list..."
                   MOVE 'N' TO FILE-EXISTS
               WHEN OTHER
                   DISPLAY "ERROR OPENING FILE. File Status: " FILE-STATUS-CODE
                   MOVE 'N' TO FILE-EXISTS
           END-EVALUATE.

       CREATE-EMPLOYEE-LIST.
           OPEN OUTPUT EMPLOYEE-FILE
           CLOSE EMPLOYEE-FILE.

       LIST-ALL-EMPLOYEES.
           OPEN INPUT EMPLOYEE-FILE
           PERFORM UNTIL FILE-STATUS-CODE = "10"
               READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD
                   AT END
                       DISPLAY "END OF FILE REACHED."
                   NOT AT END
                       DISPLAY "ID: " EMP-ID
                       DISPLAY "Name: " EMP-FIRST-NAME " " EMP-LAST-NAME
                       DISPLAY "Position: " EMP-POSITION-TYPE
                       DISPLAY "Birth Date: " EMP-BIRTH-DATE
                       DISPLAY "Union Fee: " EMP-UNION-FEE
               END-READ
           END-PERFORM
           CLOSE EMPLOYEE-FILE.

       ADD-NEW-EMPLOYEE.
           PERFORM VALIDATE-EMP-ID
           IF WS-VALID-TYPE = 'Y'
               PERFORM INPUT-EMPLOYEE-DATA
               PERFORM CALCULATE-SALARY
               DISPLAY "EMPLOYEE ADDED SUCCESSFULLY"
               OPEN OUTPUT EMPLOYEE-FILE
               WRITE EMPLOYEE-RECORD
               IF FILE-STATUS-CODE NOT = "00"
                   DISPLAY "ERROR WRITING EMPLOYEE RECORD. File Status: " FILE-STATUS-CODE
               END-IF
               CLOSE EMPLOYEE-FILE
           END-IF.


       SEARCH-BY-ID.
           DISPLAY "Enter employee ID to search: "
           ACCEPT WS-EMP-ID
           OPEN INPUT EMPLOYEE-FILE
           PERFORM UNTIL FILE-STATUS-CODE = "10"
               READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD
                   AT END
                       DISPLAY "EMPLOYEE NOT FOUND."
                   NOT AT END
                       IF EMP-ID = WS-EMP-ID
                           DISPLAY "Employee ID: " EMP-ID
                           DISPLAY "Name: " EMP-FIRST-NAME " " EMP-LAST-NAME
                           DISPLAY "Position: " EMP-POSITION-TYPE
                           DISPLAY "Birth Date: " EMP-BIRTH-DATE
                           DISPLAY "Union Fee: " EMP-UNION-FEE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE EMPLOYEE-FILE.

       VALIDATE-EMP-ID.
           MOVE 'N' TO WS-VALID-TYPE
           PERFORM UNTIL WS-VALID-TYPE = 'Y'
               DISPLAY "ENTER EMPLOYEE'S ID (5 DIGITS): " WITH NO ADVANCING
               ACCEPT WS-EMP-ID
               IF FUNCTION NUMVAL (WS-EMP-ID) > 0 AND LENGTH OF WS-EMP-ID = 5
                   MOVE 'Y' TO WS-VALID-TYPE
               ELSE
                   DISPLAY "INVALID EMPLOYEE ID. Please enter a 5-digit numeric ID."
               END-IF
           END-PERFORM.

       INPUT-EMPLOYEE-DATA.
           DISPLAY "Enter Employee First Name: "
           ACCEPT WS-EMP-FIRST-NAME
           DISPLAY "Enter Employee Last Name: "
           ACCEPT WS-EMP-LAST-NAME
           DISPLAY "Enter Employee Position (INTERN, INTERMEDIATE, SENIOR): "
           ACCEPT WS-EMP-POSITION-TYPE
           DISPLAY "Enter Employee Birth Date (YYYYMMDD): "
           ACCEPT WS-EMP-BIRTH-DATE
           DISPLAY "Enter Employee Union Fee: "
           ACCEPT WS-EMP-UNION-FEE.

       CALCULATE-SALARY.
           EVALUATE WS-EMP-POSITION-TYPE
               WHEN "INTERN"
                   MOVE 10000 TO WS-EMP-SALARY
               WHEN "INTERMEDIATE"
                   MOVE 15000 TO WS-EMP-SALARY
               WHEN "SENIOR"
                   MOVE 20000 TO WS-EMP-SALARY
               WHEN OTHER
                   MOVE 0 TO WS-EMP-SALARY
           END-EVALUATE
           DISPLAY "EMPLOYEE SALARY: " WS-EMP-SALARY.
