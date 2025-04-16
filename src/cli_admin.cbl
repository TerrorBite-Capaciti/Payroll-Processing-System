      ******************************************************************
      * Author: TerrorBite CAPACITI (BYTE BANK)
      * Date: 04 APRIL 2025
      * Purpose: PAYROLL SYSTEM
      * Compiler: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BYTEBANK-PAYROLL.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 USER-CHOICE    PIC X.
       01 EMPLOYEE-NAME       PIC X(30).
       01 EMPLOYEE-SURNAME    PIC X(30).
       01 EMPLOYEE-ID         PIC 9(5).
       01 EMPLOYEE-BIRTHDAY   PIC X(10).
       01 EMPLOYEE-POSITION   PIC X(15).
       01 HOURS-WORKED         PIC 9(3).
       01 BASE-SALARY          PIC 9(6).
       01 FINAL-SALARY         PIC 9(7).
       01 BIRTHDAY-BONUS      PIC 9(5) VALUE 0.
       01 UNION-FEE            PIC 9 VALUE 0.
       01  UNION-FEE-ANSWER      PIC X.
       01 CURRENT-MONTH        PIC 9(2).
       01 BIRTHDAY-MONTH       PIC 9(2).
       01 BIRTHDAY-DAY         PIC 9(2).

       PROCEDURE DIVISION.
       DISPLAY-INTRO.

           DISPLAY "--------------------------------------------------".
           DISPLAY "            WELCOME TO BYTE-BANK PAYROLL SYSTEM   ".
           DISPLAY "--------------------------------------------------".
           DISPLAY " ".
           DISPLAY "Please select your role:".
           DISPLAY "1. Admin Login".
           DISPLAY "2. Employee Login".
           DISPLAY " ".
           DISPLAY "Enter your choice: ".
           ACCEPT USER-CHOICE.

           EVALUATE USER-CHOICE
            WHEN "1"
                PERFORM ADMIN-MENU
            WHEN "2"
                PERFORM EMPLOYEE-MENU
            WHEN OTHER
              DISPLAY "Invalid selection. Please run the progarm again."
           END-EVALUATE.
           STOP RUN.

       ADMIN-MENU.
            PERFORM UNTIL USER-CHOICE ="5"
           DISPLAY"___________________________________________________".
           DISPLAY"            BYTE-BANK ADMIN MENU                   ".
           DISPLAY"___________________________________________________".
           DISPLAY"1. Add New Employee".
           DISPLAY"2. Process Payroll".
           DISPLAY"3. View Employee Payslip".
           DISPLAY"4. View All Employees Report".
           DISPLAY"5. Exit".
           DISPLAY"___________________________________________________".
            DISPLAY " ".
           DISPLAY"Enter your choice : "
           ACCEPT USER-CHOICE

           EVALUATE USER-CHOICE
            WHEN "1"
                PERFORM ADD-EMPLOYEE
      * Code to add new employee goes here
            WHEN "2"
               PERFORM PROCESS-PAYROLL
      * Code to process payroll goes here
            WHEN "3"
                PERFORM VIEW-PAYROLL
      * Code to view payslip goes here
            WHEN "4"
                PERFORM VIEW-REPORT
      * Code to view employee reports goes here
            WHEN "5"
                DISPLAY "Exiting program."
            WHEN OTHER
                DISPLAY "Invalid selection. Please select between 1-5."
             END-EVALUATE
        END-PERFORM.

           STOP RUN.

    ADD-EMPLOYEE.
           DISPLAY "Add Employee".
         DISPLAY "Enter Employee Name: ".
           ACCEPT EMPLOYEE-NAME.
           DISPLAY "Enter Employee Surname: ".
           ACCEPT EMPLOYEE-SURNAME.
           DISPLAY "Enter Employee ID: ".
           ACCEPT EMPLOYEE-ID.
           DISPLAY "Enter Employee Birthday (YYYY-MM-DD): ".
           ACCEPT EMPLOYEE-BIRTHDAY.
           DISPLAY "Enter Employee Position (Low/Intermediate/Senior):".
           ACCEPT EMPLOYEE-POSITION.
           DISPLAY "Enter hours worked: ".
           ACCEPT HOURS-WORKED.

      * Extract month and day from birthday for bonus check
             UNSTRING EMPLOYEE-BIRTHDAY DELIMITED BY "-"
            INTO BIRTHDAY-MONTH BIRTHDAY-DAY
        END-UNSTRING

      * Check if birthday is in the current month
           IF BIRTHDAY-MONTH = CURRENT-MONTH THEN
               MOVE 500 TO BIRTHDAY-BONUS
           END-IF.

      * Assign salary based on position
           IF EMPLOYEE-POSITION = "Intern" THEN
               MOVE 10000 TO BASE-SALARY
           ELSE IF EMPLOYEE-POSITION = "Intermediate" THEN
               MOVE 50000 TO BASE-SALARY
           ELSE IF EMPLOYEE-POSITION = "Senior" THEN
               MOVE 100000 TO BASE-SALARY
           ELSE
               DISPLAY "Invalid Position"
           END-IF.

      * Calculate final salary with birthday bonus
           COMPUTE FINAL-SALARY = BASE-SALARY * HOURS-WORKED / 8 +
          BIRTHDAY-BONUS.

           DISPLAY "Employee added successfully with salary: R"
           FINAL-SALARY.


       PROCESS-PAYROLL.
           DISPLAY "Process Payroll..".
            DISPLAY "Enter Employee Name: ".
           ACCEPT EMPLOYEE-NAME.
           DISPLAY "Enter Employee ID: ".
           ACCEPT EMPLOYEE-ID.

      * Salary calculation logic
           DISPLAY"Enter Employee Position (Low/Intermediate/Senior):".

           ACCEPT EMPLOYEE-POSITION.
           DISPLAY "Enter hours worked: ".
           ACCEPT HOURS-WORKED.

           IF EMPLOYEE-POSITION = "Intern" THEN
               MOVE 10000 TO BASE-SALARY
           ELSE IF EMPLOYEE-POSITION = "Intermediate" THEN
               MOVE 50000 TO BASE-SALARY
           ELSE IF EMPLOYEE-POSITION = "Senior" THEN
               MOVE 100000 TO BASE-SALARY
           ELSE
               DISPLAY "Invalid Position"
           END-IF.

      * Calculate final salary with birthday bonus
           COMPUTE FINAL-SALARY = BASE-SALARY * HOURS-WORKED / 8 +
           -BIRTHDAY-BONUS.
           DISPLAY "Employee Salary: R" FINAL-SALARY.

      * Prompt for union fee
           DISPLAY "Do you want to pay the union fee (Y/N)? ".
           ACCEPT UNION-FEE-ANSWER

           IF UNION-FEE = "Y" THEN
               DISPLAY "Union fee deducted from salary."
               COMPUTE FINAL-SALARY = FINAL-SALARY - 100.
           END-IF.

       VIEW-PAYSLIP.
           DISPLAY "View Payslip functionality coming soon...".

       VIEW-REPORT.
           DISPLAY "View All Employees Report  coming soon.".

       EMPLOYEE-MENU.
           DISPLAY"___________________________________________________".
           DISPLAY"            BYTE-BANK EMPLOYEE MENU                ".
           DISPLAY"___________________________________________________".
           DISPLAY"1. View Employee Payslip".
           DISPLAY"2. View All Employees Report".
           DISPLAY"3. Exit".
           DISPLAY"___________________________________________________".
           DISPLAY "Please select an option (1-3): ".
           ACCEPT USER-CHOICE.
           EVALUATE USER-CHOICE
            WHEN 1
                DISPLAY "View Employee Payslip Functionality."
      * Code to view payslip goes here
            WHEN 2
                DISPLAY "View All Employees Report Functionality."
      * Code to view reports goes here
            WHEN 3
                DISPLAY "Exiting program."
            WHEN OTHER
                DISPLAY "Invalid selection. Please select between 3-5."
           END-EVALUATE.
