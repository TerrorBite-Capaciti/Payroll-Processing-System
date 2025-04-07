       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       AUTHOR. YOUR-NAME.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "data/employees.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SALARY-FILE ASSIGN TO "data/salary_records.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMPLOYEE-ID           PIC X(5).
           05 EMPLOYEE-NAME         PIC X(30).
           05 BASIC-SALARY          PIC 9(7)V99.
           05 HOURS-WORKED          PIC 9(3)V99.
           05 OVERTIME-HOURS        PIC 9(3)V99.
           05 LEAVE-DEDUCTIONS      PIC 9(5)V99.
           05 TAX-RATE              PIC 9(2)V99.
           05 BENEFITS             PIC 9(5)V99.

       FD SALARY-FILE.
       01 SALARY-RECORD.
           05 SALARY-EMPLOYEE-ID    PIC X(5).
           05 GROSS-SALARY          PIC 9(7)V99.
           05 TAX-DEDUCTED          PIC 9(6)V99.
           05 NET-SALARY            PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01 EOF-FLAG                 PIC X VALUE 'N'.
          88 EOF                   VALUE 'Y'.
       01 TEMP-GROSS-SALARY        PIC 9(7)V99.
       01 TEMP-TAX-DEDUCTED        PIC 9(6)V99.
       01 TEMP-NET-SALARY          PIC 9(7)V99.
       01 LOGGED-IN-ROLE           PIC X(10).

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           * CALL CLI MENU FOR USER LOGIN AND ROLE SELECTION
           CALL 'cli_menu.cbl' USING LOGGED-IN-ROLE.

           * Check the user role (Admin or Employee)
           IF LOGGED-IN-ROLE = 'ADMIN'
               DISPLAY "Welcome Admin! You have access to all payroll functionalities."
               * Admin can perform all tasks related to payroll processing
               OPEN INPUT EMPLOYEE-FILE.
               OPEN OUTPUT SALARY-FILE.

               PERFORM PROCESS-EMPLOYEES UNTIL EOF.

               CLOSE EMPLOYEE-FILE.
               CLOSE SALARY-FILE.
           ELSE IF LOGGED-IN-ROLE = 'EMPLOYEE'
               DISPLAY "Welcome Employee! You have access to your payroll information."
               * Employee can only view their payslip and salary history
               DISPLAY "Viewing your payslip and salary history functionality."
               * You can add additional employee-related functionality here.

           ELSE
               DISPLAY "Invalid role, access denied."
           END-IF.

           STOP RUN.
       
       STORE-SALARY.
           MOVE EMPLOYEE-ID TO SALARY-EMPLOYEE-ID.
           MOVE TEMP-GROSS-SALARY TO GROSS-SALARY.
           MOVE TEMP-TAX-DEDUCTED TO TAX-DEDUCTED.
           MOVE TEMP-NET-SALARY TO NET-SALARY.
           WRITE SALARY-RECORD.
       
       CALCULATE-SALARY.
           COMPUTE TEMP-GROSS-SALARY = BASIC-SALARY + 
               (OVERTIME-HOURS * 100).
           COMPUTE TEMP-TAX-DEDUCTED = (TEMP-GROSS-SALARY * 
               TAX-RATE) / 100.
           COMPUTE TEMP-NET-SALARY = TEMP-GROSS-SALARY - 
               TEMP-TAX-DEDUCTED - 
               LEAVE-DEDUCTIONS + 
               BENEFITS.
       
       PROCESS-EMPLOYEES.
           READ EMPLOYEE-FILE
               AT END SET EOF TO TRUE
               NOT AT END PERFORM CALCULATE-SALARY
                        PERFORM STORE-SALARY.
       
       END PROGRAM PAYROLL.
